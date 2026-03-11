# ──────────────────────────────────────────────────────────────────────────────
# paginate.R — R-side row height calculation and pagination
#
# Used for deterministic page assignment when group_by is set.
# Without group_by, RTF-native page breaks are preferred.
#
# Architecture: Two-pass columnar scanner
#   Pass 1 (vectorized): Fast scan to identify multi-line candidates
#   Pass 2 (targeted): Expensive stri_wrap() only on candidates
# ──────────────────────────────────────────────────────────────────────────────


#' Compute available character width per column
#'
#' @param columns Named list of fr_col objects (visible only).
#' @param page fr_page object.
#' @return Named integer vector — character capacity per column.
#' @noRd
compute_column_char_widths <- function(columns, page) {
  # Approximate: column width in inches * characters-per-inch for the font
  # Use AFM space width as the average character width
  space_twips <- measure_text_width_twips(" ", page$font_family, page$font_size)
  vapply(columns, function(col) {
    col_twips <- inches_to_twips(col$width)
    max(1L, as.integer(col_twips / space_twips))
  }, integer(1))
}


#' Measure the height (in lines) of a single cell
#'
#' @param text Character scalar. Cell text content.
#' @param char_width Integer. Column character width capacity.
#' @return Integer. Number of lines this cell needs.
#' @noRd
measure_cell_height <- function(text, char_width) {
  if (!nzchar(text)) return(1L)

  # Split by explicit newlines first
  parts <- strsplit(text, "\n", fixed = TRUE)[[1L]]

  total <- 0L
  for (part in parts) {
    if (!nzchar(part)) {
      total <- total + 1L
      next
    }
    # Estimate line wrapping
    wrapped <- stringi::stri_wrap(part, width = char_width,
                                   simplify = TRUE)
    total <- total + length(wrapped)
  }

  max(1L, total)
}


#' Calculate row heights for a data frame (columnar two-pass scanner)
#'
#' Pass 1: Vectorized detection of multi-line candidates (newlines or wide text).
#' Pass 2: Expensive `stri_wrap()` only on candidate cells.
#'
#' @param data Data frame (body data for one page group).
#' @param columns Named list of fr_col objects (visible only).
#' @param page fr_page object.
#' @return Integer vector of length `nrow(data)` — height in lines per row.
#' @noRd
calculate_row_heights <- function(data, columns, page) {
  nr <- nrow(data)
  if (nr == 0L) return(integer(0))

  heights <- rep(1L, nr)
  col_chars <- compute_column_char_widths(columns, page)

  for (j in seq_along(columns)) {
    nm <- names(columns)[j]
    content <- data[[nm]]
    if (!is.character(content)) next

    # Strip sentinel tokens so width estimation uses visible text only
    content <- sentinel_to_plain_vec(content)

    # Pass 1: Vectorized candidate detection
    nl_count <- stringi::stri_count_fixed(content, "\n")
    char_count <- nchar(content)
    candidates <- which(nl_count > 0L | char_count > col_chars[j])
    if (length(candidates) == 0L) next

    # Pass 2: Expensive wrap only for candidates
    for (i in candidates) {
      h <- measure_cell_height(content[i], col_chars[j])
      heights[i] <- max(heights[i], h)
    }
  }

  heights
}


#' Calculate page budget in line-equivalents
#'
#' Estimates how many single-height body rows fit on one page, accounting
#' for titles, column headers, spanning headers, footnotes, page chrome.
#'
#' @param spec A finalized fr_spec object.
#' @return Integer. Number of single-line rows per page.
#' @noRd
calculate_page_budget <- function(spec) {
  # Page height in twips
  page_dims <- paper_dims_twips(spec$page$paper, spec$page$orientation)
  page_height <- page_dims[["height"]]

  # Margins (top + bottom)
  margins <- spec$page$margins
  margin_top    <- inches_to_twips(margins$top)
  margin_bottom <- inches_to_twips(margins$bottom)

  # Row height for one line at current font size
  one_row_twips <- row_height_twips(spec$page$font_size)

  # Chrome rows: titles + column header + spanners + spacing

  n_titles <- length(spec$meta$titles %||% list())
  n_spanners <- n_spanner_levels(spec$header$spans)
  n_header_rows <- 1L + n_spanners
  titles_after <- spec$spacing$titles_after %||% 1L
  footnotes_before <- spec$spacing$footnotes_before %||% 1L

  # Footnotes (repeat on every page in RTF)
  n_footnotes <- sum(vapply(
    spec$meta$footnotes %||% list(),
    function(fn) fn$placement == "every",
    logical(1)
  ))

  # Page header/footer chrome (approximate as 2 rows each if present)
  n_pagehead <- if (!is.null(spec$pagehead)) 2L else 0L
  n_pagefoot <- if (!is.null(spec$pagefoot)) 2L else 0L

  # Available height for body
  chrome_rows <- n_titles + titles_after + n_header_rows +
    footnotes_before + n_footnotes + n_pagehead + n_pagefoot
  available_twips <- page_height - margin_top - margin_bottom -
    chrome_rows * one_row_twips

  # Budget in line-equivalents
  budget <- as.integer(available_twips / one_row_twips)
  max(5L, budget)  # minimum 5 rows per page
}


#' Assign rows to pages with group-aware orphan/widow control
#'
#' Greedy page-filling algorithm that respects group boundaries.
#' Groups smaller than `orphan_min + widow_min` are kept together.
#' Larger groups may split, but enforce minimum rows at page boundaries.
#'
#' @param row_heights Integer vector — height in lines per body row.
#' @param budget Integer — lines available per page.
#' @param data Data frame (for group key computation).
#' @param group_by Character vector of grouping column names.
#' @param orphan_min Integer. Min rows to keep at bottom of page.
#' @param widow_min Integer. Min rows to carry to next page.
#' @return Integer vector — page number (1-based) for each row, or
#'   0 for excluded trailing blank rows (not rendered).
#' @noRd
paginate_rows <- function(row_heights, budget, data, group_by,
                           orphan_min = 3L, widow_min = 3L) {
  nr <- length(row_heights)
  if (nr == 0L) return(integer(0))

  pages <- rep(0L, nr)
  current_page <- 1L
  used <- 0L

  # Deferred blank accounting: trailing blanks between groups are not counted
  # toward `used` immediately. At the next group, we check whether the pending
  # blank + group fit. If only the group fits, the blank is dropped (invisible
  # at a page boundary). This avoids wasting 1 row of budget per group.
  pending_blank_idx <- NULL
  pending_blank_height <- 0L

  # Identify blank rows and group boundaries
  is_blank <- rowSums(data != "") == 0L
  group_by <- intersect(group_by, names(data))

  if (length(group_by) > 0L) {
    keys <- inject(paste(!!!data[group_by], sep = fr_env$group_sep))
  } else {
    keys <- rep("__all__", nr)
  }

  # Find group start positions
  is_header <- !is_blank & c(TRUE, keys[-1L] != keys[-length(keys)])
  header_positions <- which(is_header)

  for (g_idx in seq_along(header_positions)) {
    g_start <- header_positions[g_idx]
    g_end <- if (g_idx < length(header_positions)) {
      header_positions[g_idx + 1L] - 1L
    } else {
      nr
    }

    # Skip trailing blanks from group end
    while (g_end > g_start && is_blank[g_end]) g_end <- g_end - 1L

    group_rows <- g_start:g_end
    group_height <- sum(row_heights[group_rows])

    # Assign pending blank to current page (belongs to previous group)
    if (!is.null(pending_blank_idx)) {
      pages[pending_blank_idx] <- current_page
    }

    # Deferred blank logic: 4 branches
    if (used + pending_blank_height + group_height <= budget) {
      # Branch 1: pending blank + group both fit on current page
      pages[group_rows] <- current_page
      used <- used + pending_blank_height + group_height
    } else if (used + group_height <= budget) {
      # Branch 2: group fits without blank — blank already assigned above
      pages[group_rows] <- current_page
      used <- used + group_height
    } else if (group_height <= budget) {
      # Branch 3: new page needed, group starts fresh
      current_page <- current_page + 1L
      pages[group_rows] <- current_page
      used <- group_height
    } else {
      # Branch 4: group larger than one page — must split row-by-row
      if (used > 0L) {
        current_page <- current_page + 1L
        used <- 0L
      }

      for (r in group_rows) {
        if (used + row_heights[r] > budget && used > 0L) {
          current_page <- current_page + 1L
          used <- 0L
        }
        pages[r] <- current_page
        used <- used + row_heights[r]
      }
    }

    # Defer trailing blank (if it exists between groups)
    if (g_end < nr && is_blank[g_end + 1L]) {
      pending_blank_idx <- g_end + 1L
      pending_blank_height <- row_heights[g_end + 1L]
    } else {
      pending_blank_idx <- NULL
      pending_blank_height <- 0L
    }
  }

  # Final pending blank stays at page 0 — excluded from rendering
  pages
}
