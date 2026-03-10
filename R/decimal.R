# ──────────────────────────────────────────────────────────────────────────────
# decimal.R — Centered anchor-point decimal alignment (backend-agnostic)
#
# Architecture:
#   1. detect_separator() — finds the separator between value pairs
#   2. split_at_separator() — splits each cell into two halves
#   3. compute_decimal_geometry() — computes sub-cell widths for centered layout
#
# Both halves are right-aligned (\qr in RTF), which naturally produces
# decimal-point alignment within each half. The content block is centered
# in the column via equal left/right margins.
# ──────────────────────────────────────────────────────────────────────────────


#' Detect the separator between value pairs in a column
#'
#' Examines a character vector of cell values and identifies which separator
#' appears in most non-empty cells. Used to split values like `"11.1 - 18.2"`
#' or `"55.0, 88.0"` into two independently-aligned halves.
#'
#' Priority: `" - "` > `", "` > `" "` (multi-char separators first, to avoid
#' splitting `"12.3 (45.6%)"` at the wrong space).
#'
#' @param content_vec Character vector of cell values.
#' @return Character scalar — the detected separator, or `NULL` if no
#'   separator found in a majority of cells.
#' @noRd
detect_separator <- function(content_vec) {
  content_vec <- content_vec[nzchar(trimws(content_vec))]
  if (length(content_vec) == 0L) return(NULL)

  separators <- c(" - ", ", ", " ")
  counts <- vapply(separators, function(sep) {
    sum(stringi::stri_detect_fixed(content_vec, sep))
  }, integer(1))

  # Pick the separator present in the most cells, requiring > 50%
  best <- which.max(counts)
  if (counts[best] > length(content_vec) / 2) {
    separators[best]
  } else {
    NULL
  }
}


#' Split cell values at a separator into two halves
#'
#' @param content_vec Character vector of cell values.
#' @param separator Character scalar — the separator string.
#' @return A list with `left` and `right` character vectors, each the same
#'   length as `content_vec`. Cells without the separator have `right = ""`.
#' @noRd
split_at_separator <- function(content_vec, separator) {
  if (is.null(separator)) {
    return(list(left = content_vec, right = rep("", length(content_vec))))
  }

  pos <- stringi::stri_locate_first_fixed(content_vec, separator)
  has_sep <- !is.na(pos[, 1L])

  left  <- character(length(content_vec))
  right <- character(length(content_vec))

  if (any(has_sep)) {
    left[has_sep]  <- stringi::stri_sub(content_vec[has_sep], 1L,
                                         pos[has_sep, 1L] - 1L)
    right[has_sep] <- stringi::stri_sub(content_vec[has_sep],
                                         pos[has_sep, 2L] + 1L)
  }
  left[!has_sep]  <- content_vec[!has_sep]
  right[!has_sep] <- ""

  list(left = left, right = right)
}


#' Compute decimal alignment geometry for a single column
#'
#' For a column with `align = "decimal"`, computes the sub-cell widths
#' needed for centered decimal alignment. Both halves are right-aligned
#' in the output, which naturally aligns decimal points.
#'
#' @param content_vec Character vector of trimmed cell values.
#' @param column_width_twips Integer. Total column width in twips.
#' @param font_family Character. Font family name.
#' @param font_size Numeric. Font size in points.
#' @param bold Logical. Whether cells are bold.
#' @param padding Integer. Padding in twips around each sub-cell.
#' @return A list with:
#'   - `sub1_width`: width of left sub-cell in twips
#'   - `max_left`: max width of left halves
#'   - `max_right`: max width of right halves
#'   - `separator`: the detected separator
#'   - `left_parts`: character vector of left halves
#'   - `right_parts`: character vector of right halves
#' @noRd
compute_decimal_geometry <- function(content_vec, column_width_twips,
                                      font_family, font_size,
                                      bold = FALSE, padding = 36L) {
  content_vec <- trimws(content_vec)

  separator <- detect_separator(content_vec)
  halves <- split_at_separator(content_vec, separator)

  # Measure widths of each half (vectorized)
  non_empty_left  <- nzchar(halves$left)
  non_empty_right <- nzchar(halves$right)

  left_widths  <- integer(length(content_vec))
  right_widths <- integer(length(content_vec))

  if (any(non_empty_left)) {
    left_widths[non_empty_left] <- measure_text_width_twips(
      halves$left[non_empty_left], font_family, font_size, bold
    )
  }
  if (any(non_empty_right)) {
    right_widths[non_empty_right] <- measure_text_width_twips(
      halves$right[non_empty_right], font_family, font_size, bold
    )
  }

  max_left  <- if (any(non_empty_left))  max(left_widths)  else 0L
  max_right <- if (any(non_empty_right)) max(right_widths) else 0L

  # Compute centered geometry
  content_width <- max_left + max_right + padding * 2L
  slack <- column_width_twips - content_width
  left_margin <- max(0L, as.integer(slack / 2))

  sub1_width <- left_margin + padding + max_left + padding

  list(
    sub1_width  = sub1_width,
    max_left    = max_left,
    max_right   = max_right,
    separator   = separator,
    left_parts  = halves$left,
    right_parts = halves$right
  )
}


#' Pre-compute decimal geometry for all decimal-aligned columns
#'
#' Called during `finalize_spec()` to pre-compute geometry for all columns
#' with `align = "decimal"`. The result is stored on `spec$decimal_geometry`
#' and used by both RTF and LaTeX backends.
#'
#' @param spec An fr_spec object (post-finalize_columns).
#' @return Named list of geometry objects (one per decimal column), or NULL.
#' @noRd
compute_all_decimal_geometry <- function(spec) {
  columns <- spec$columns
  col_names <- names(columns)

  # Early exit: skip loop if no decimal columns
  has_decimal <- vapply(columns, function(col) identical(col$align, "decimal"),
                        logical(1))
  if (!any(has_decimal)) return(NULL)

  result <- list()

  for (nm in col_names) {
    col <- columns[[nm]]
    if (!identical(col$align, "decimal")) next
    if (!nm %in% names(spec$data)) next

    vals <- as.character(spec$data[[nm]])
    vals[is.na(vals)] <- ""

    col_width_twips <- inches_to_twips(col$width)

    geom <- compute_decimal_geometry(
      vals, col_width_twips,
      spec$page$font_family, spec$page$font_size,
      bold = FALSE,
      padding = fr_env$rtf_decimal_pad
    )

    result[[nm]] <- geom
  }

  if (length(result) == 0L) NULL else result
}
