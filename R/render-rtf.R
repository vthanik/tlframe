# ──────────────────────────────────────────────────────────────────────────────
# render-rtf.R — RTF backend for fr_render()
#
# Emits a complete RTF document with:
#   - Font table and color table
#   - Section definitions (page size, margins, orientation)
#   - Page header (pagehead) via RTF {\header} group
#   - Footnotes + page footer (pagefoot) combined in RTF {\footer} group
#     (repeats on every page automatically)
#   - Titles as \trhdr rows (repeat on every page)
#   - Column headers with \trhdr (repeat on every page)
#   - Spanning headers with \clmgf / \clmrg
#   - Body rows with per-cell styling and borders
#   - Section breaks (\sect) for page_by groups and col_split panels
# ──────────────────────────────────────────────────────────────────────────────


#' Render an fr_spec to RTF
#'
#' @param spec Finalized fr_spec object.
#' @param page_groups List of page group lists (data + group_label).
#' @param col_panels List of column name vectors (one per panel).
#' @param path Output file path.
#' @noRd
render_rtf <- function(spec, page_groups, col_panels, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))

  # Separate "last" footnotes — they render as body rows, not in {\footer}
  footnotes <- spec$meta$footnotes %||% list()
  last_footnotes <- Filter(function(fn) fn$placement == "last", footnotes)
  every_footnotes <- Filter(function(fn) fn$placement != "last", footnotes)

  colors <- collect_colors(spec)
  color_info <- build_rtf_colortbl(colors)

  rtf_write(con, rtf_preamble(spec, color_info))

  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]

    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      # Filter columns to this panel's visible columns
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L

      if (section_idx > 1L) {
        # End previous table with \pard before \sect to exit table context;
        # without this, Word treats consecutive \trowd rows across \sect as
        # one continuous table and ignores the page break.
        rtf_write(con, "\\pard\\par\n\\sect\n")
      }

      # Determine is_last before section def (affects footer height)
      is_last <- (group_idx == length(page_groups) &&
                    panel_idx == length(col_panels))

      # Single-page estimation: when content fits on one page, render
      # footnotes as body rows instead of in {\footer} group
      is_single_page <- estimate_single_page(spec, group$data)

      rtf_write(con, rtf_section_def(spec, is_last,
                                      body_footnotes = is_single_page))

      # Page header/footer (RTF header/footer groups)
      # Use RTF field codes for dynamic page numbering.
      # Placeholders survive rtf_escape_and_resolve(), then get replaced
      # with actual field codes by rtf_resolve_page_fields().
      token_map <- build_token_map(
        page_num = "\x01RTFPAGE\x02",
        total_pages = "\x01RTFNUMPAGES\x02",
        spec = spec
      )
      rtf_write(con, rtf_resolve_page_fields(
        rtf_page_header(spec, token_map)))
      rtf_write(con, rtf_resolve_page_fields(
        rtf_footer_group(spec, token_map, is_last, vis_columns,
                         skip_footnotes = is_single_page)))

      # Blank lines after page header (spacing$pagehead_after)
      if (!is.null(spec$pagehead)) {
        n_ph <- spec$spacing$pagehead_after %||% 1L
        if (n_ph > 0L) {
          ph_fs <- pt_to_half_pt(spec$page$font_size)
          rtf_write(con, strrep(
            paste0("\\pard\\plain\\fs", ph_fs, "\\par\n"), n_ph
          ))
        }
      }

      # Resolve borders for this section
      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(spec$rules, nrow(group$data),
                                 length(vis_columns), nrow_header)

      # Suppress body bottom border when another element will render it:
      # - Multi-page: {\footer} group renders the bottom rule
      # - Single-page: body footnote rows render it as cell top border
      bottom_rule <- find_bottom_rule(spec)
      if (!is.null(bottom_rule)) {
        suppress_bottom <- if (is_single_page) {
          length(spec$meta$footnotes %||% list()) > 0L
        } else {
          has_every_fn <- any(vapply(spec$meta$footnotes %||% list(),
                       function(fn) fn$placement == "every", logical(1)))
          has_last_body_fn <- is_last && length(last_footnotes) > 0L
          has_every_fn || has_last_body_fn || !is.null(spec$pagefoot)
        }
        if (suppress_bottom) {
          nr <- nrow(group$data)
          if (nr > 0L) {
            for (j in seq_along(vis_columns)) {
              borders$body$bottom[nr, j] <- list(NULL)
            }
          }
        }
      }

      # Title rows as \trhdr (always repeat on every page)
      rtf_write(con, rtf_title_rows(spec, vis_columns, color_info))

      # Group label (page_by value) — \trhdr row to stay within the table
      if (!is.null(group$group_label) && nzchar(group$group_label)) {
        rtf_write(con, rtf_page_by_rows(spec, vis_columns, group$group_label))
      }

      # Per-group header label overrides (per-group / function / auto N-counts)
      label_overrides <- resolve_group_labels(
        spec, group$data, group$group_label
      )

      # Spanning header rows
      rtf_write(con, rtf_spanner_rows(spec, vis_columns, borders, color_info))

      # Column header row
      rtf_write(con, rtf_col_header_row(spec, vis_columns, borders, color_info,
                                         label_overrides = label_overrides))

      # Body rows
      cell_grid <- build_cell_grid(group$data, vis_columns,
                                   spec$cell_styles, spec$page)
      rtf_write(con, rtf_body_rows(spec, group$data, vis_columns,
                                   cell_grid, borders, color_info))

      # Body footnotes: single-page mode OR "last" placement footnotes
      if (is_single_page) {
        rtf_write(con, rtf_body_footnotes(spec, vis_columns, is_last,
                                           borders, color_info))
      } else if (is_last && length(last_footnotes) > 0L) {
        # "last" footnotes as body rows — appear once at end of data
        rtf_write(con, rtf_body_footnotes_last(
          spec, vis_columns, last_footnotes, borders, color_info
        ))
      }

    }
  }

  rtf_write(con, "}")
}


#' Replace page number placeholders with RTF field codes
#'
#' Token map uses sentinel placeholders for {thepage} and {total_pages}
#' to survive rtf_escape_and_resolve(). This function replaces them with
#' actual RTF field codes that Word evaluates dynamically.
#' @noRd
rtf_resolve_page_fields <- function(text) {
  text <- gsub("\x01RTFPAGE\x02",
               "{\\field{\\*\\fldinst PAGE}}", text, fixed = TRUE)
  text <- gsub("\x01RTFNUMPAGES\x02",
               "{\\field{\\*\\fldinst NUMPAGES}}", text, fixed = TRUE)
  text
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Document Components
# ══════════════════════════════════════════════════════════════════════════════

#' RTF preamble: header, font table, color table
#' @noRd
rtf_preamble <- function(spec, color_info) {
  font_name <- spec$page$font_family
  rtf_fam <- get_rtf_font_family(font_name)
  prq <- get_rtf_font_prq(font_name)

  fonttbl <- paste0("{\\fonttbl{\\f0\\", rtf_fam, "\\fprq", prq,
                    " ", font_name, ";}}")
  paste0("{\\rtf1\\ansi\\ansicpg1252\\deff0\n",
         fonttbl, "\n",
         color_info$rtf, "\n")
}


#' RTF section definition: page size, margins, orientation, header/footer position
#' @param spec Finalized fr_spec object.
#' @param is_last Logical. TRUE for the final section (affects footer space).
#' @param body_footnotes Logical. When TRUE, footnotes are rendered as body
#'   rows instead of in the footer, so footer height does not account for them.
#' @noRd
rtf_section_def <- function(spec, is_last = FALSE, body_footnotes = FALSE) {
  page <- spec$page
  dims <- paper_dims_twips(page$paper, page$orientation)
  ml <- inches_to_twips(page$margins$left)
  mr <- inches_to_twips(page$margins$right)
  mt <- inches_to_twips(page$margins$top)
  mb <- inches_to_twips(page$margins$bottom)

  orient <- if (page$orientation == "landscape") "\\lndscpsxn" else ""

  # \headery: only emit when pagehead exists, to avoid reserving dead space
  headery_str <- ""
  if (!is.null(spec$pagehead)) {
    chrome_fs <- spec$pagehead$font_size %||% (page$font_size - 1)
    line_twips <- as.integer(round(chrome_fs * 20 * fr_env$rtf_leading_factor))
    headery_str <- paste0("\\headery", max(as.integer(mt - line_twips), fr_env$rtf_min_headery))
  }

  # \footery = bottom margin. Word auto-expands the footer area upward
  # if content exceeds it, shrinking body area to fit.
  # Only "every" footnotes go in {\footer}; "last" are body rows.
  # When body_footnotes = TRUE, all footnotes are in body rows, not footer.
  has_footer_fn <- if (body_footnotes) {
    FALSE
  } else {
    any(vapply(spec$meta$footnotes %||% list(),
               function(fn) fn$placement == "every", logical(1)))
  }
  footery_str <- if (!is.null(spec$pagefoot) || has_footer_fn) {
    paste0("\\footery", mb)
  } else ""

  paste0("\\sectd\\sbkpage", orient,
         "\\pgwsxn", dims[["width"]],
         "\\pghsxn", dims[["height"]],
         "\\margl", ml,
         "\\margr", mr,
         "\\margt", mt,
         "\\margb", mb,
         headery_str,
         footery_str,
         "\n")
}


#' RTF page header (running header)
#'
#' Uses a single paragraph with tab stops for left/center/right layout:
#' - Left text at position 0 (default paragraph alignment \ql)
#' - Center text at a \tqc tab stop at the page midpoint
#' - Right text at a \tqr tab stop at the right margin
#' @noRd
rtf_page_header <- function(spec, token_map) {
  if (is.null(spec$pagehead)) return("")
  rtf_pagechrome_paragraph(spec$pagehead, spec, token_map, "header", "page header")
}

#' Find the bottom rule for the table body
#'
#' Searches spec$rules for a horizontal rule with region="body", side="below",
#' rows=NULL (i.e., targets the last row = table bottom border).
#' Returns the rule object or NULL if none found.
#' @noRd
find_bottom_rule <- function(spec) {
  rules <- spec$rules %||% list()
  for (rule in rules) {
    if (inherits(rule, "fr_rule_hline") &&
        rule$region == "body" &&
        rule$side == "below" &&
        is.null(rule$rows)) {
      return(rule)
    }
  }
  NULL
}


#' RTF footer group — footnotes + pagefoot combined in {\footer}
#'
#' Builds a single `{\footer ...}` group containing:
#'   1. Bottom rule (table bottom border as paragraph top border, repeats every page)
#'   2. Blank spacing lines (footnotes_before — gap after bottom rule)
#'   3. Footnote separator line + footnote paragraphs (table content width)
#'   4. 1/4 baselineskip gap
#'   5. Pagefoot paragraph (L/C/R tab-stop layout, full printable width)
#'
#' Only `placement = "every"` footnotes are included (they repeat on every
#' page). `placement = "last"` footnotes are rendered as body rows via
#' [rtf_body_footnotes_last()] so they appear only once at the end.
#'
#' @param spec Finalized fr_spec object.
#' @param token_map Token map for page chrome resolution.
#' @param is_last Logical. TRUE for the final section.
#' @param vis_columns Named list of fr_col objects for the current panel.
#'   Used to calculate table content width for footnote paragraph width.
#' @noRd
rtf_footer_group <- function(spec, token_map, is_last, vis_columns,
                             skip_footnotes = FALSE) {
  footnotes <- spec$meta$footnotes %||% list()
  has_pagefoot <- !is.null(spec$pagefoot)

  # When skip_footnotes = TRUE (single-page mode), footnotes are rendered
  # as body rows — the footer only contains pagefoot (if any).
  # "last" placement footnotes are NEVER in {\footer} — they're body rows.
  entries <- if (skip_footnotes) {
    list()
  } else {
    Filter(function(fn) fn$placement == "every", footnotes)
  }

  has_footnotes <- length(entries) > 0L

  if (!has_footnotes && !has_pagefoot) return("")

  parts <- character(0)
  fn_fs <- pt_to_half_pt(spec$page$font_size)

  # Calculate left/right indent so footnotes are centered under the table
  # Table width = sum of visible column widths
  # Split the gap equally into \li and \ri for horizontal centering
  printable_twips <- printable_area_twips(spec$page)[["width"]]
  table_twips <- sum(vapply(vis_columns, function(c) inches_to_twips(c$width),
                            integer(1), USE.NAMES = FALSE))
  fn_gap_twips <- max(0L, as.integer(printable_twips - table_twips))
  fn_indent_each <- as.integer(fn_gap_twips / 2L)
  fn_indent_str <- if (fn_indent_each > 0L) {
    paste0("\\li", fn_indent_each, "\\ri", fn_indent_each)
  } else ""

  # Bottom rule — render table bottom border as paragraph top border in footer
  # so it repeats on every page (mirrors body-below hline from spec$rules).
  # Skip when footnotes are in body rows (they handle the bottom rule).
  if (!skip_footnotes) {
    bottom_rule <- find_bottom_rule(spec)
    if (!is.null(bottom_rule)) {
      bw <- as.integer(round(bottom_rule$width * 20))  # pt to twips
      parts <- c(parts, paste0(
        "\\pard\\plain\\sb0", fn_indent_str,
        "\\brdrt\\brdrs\\brdrw", bw,
        "\\fs2\\par\n"
      ))
    }
  }

  # Footnote paragraphs (width constrained to table content width)
  if (has_footnotes) {
    # Blank lines before footnotes (spacing$footnotes_before, default 1)
    # — gap between table bottom border and footnote separator
    n_before <- spec$spacing$footnotes_before %||% 1L
    if (n_before > 0L) {
      parts <- c(parts, strrep(
        paste0("\\pard\\plain\\sb0", fn_indent_str, "\\fs", fn_fs, "\\par\n"),
        n_before
      ))
    }

    for (idx in seq_along(entries)) {
      fn <- entries[[idx]]
      fs <- pt_to_half_pt(fn$font_size %||% spec$page$font_size)
      align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
      content <- rtf_escape_and_resolve(fn$content)

      # Separator as paragraph top border on first footnote
      sep_str <- ""
      if (idx == 1L && isTRUE(spec$meta$footnote_separator)) {
        sep_str <- "\\brdrt\\brdrs\\brdrw5 "
      }

      parts <- c(parts, paste0(
        "\\pard\\plain\\sb0", fn_indent_str, sep_str, align_rtf,
        "\\fs", fs, " ", content, "\\par\n"
      ))
    }
  }

  # Pagefoot (L/C/R tab-stop layout, full printable width)
  if (has_pagefoot) {
    # 1/4 baselineskip gap between footnotes and pagefoot
    gap_twips <- if (has_footnotes) {
      chrome_fs <- spec$pagefoot$font_size %||% (spec$page$font_size - 1)
      as.integer(round(chrome_fs * 20 * fr_env$rtf_leading_factor / 4))
    } else {
      0L
    }
    pf_content <- rtf_chrome_content(spec$pagefoot, spec, token_map,
                                     "page footer", sb_twips = gap_twips)
    if (nzchar(pf_content)) {
      parts <- c(parts, pf_content)
    }
  }

  paste0("{\\footer\n", paste0(parts, collapse = ""), "}\n")
}



#' Build RTF chrome paragraph content (no group wrapper)
#'
#' Returns the inner paragraph string for L/C/R tab-stop layout.
#' Used by rtf_pagechrome_paragraph() and rtf_footer_group().
#' @param chrome Chrome spec (left/center/right/bold/font_size).
#' @param spec Finalized fr_spec object.
#' @param token_map Token map for token resolution.
#' @param context Context string for error messages.
#' @param sb_twips Space before in twips (\\sb). Default 0.
#' @noRd
rtf_chrome_content <- function(chrome, spec, token_map, context,
                               sb_twips = 0L) {
  fs <- pt_to_half_pt(chrome$font_size %||% (spec$page$font_size - 1))

  # Calculate tab stop positions in twips (full printable width)
  printable_twips <- printable_area_twips(spec$page)[["width"]]
  center_pos <- as.integer(round(printable_twips / 2))
  right_pos  <- as.integer(printable_twips)

  # Tab stop definitions
  tab_defs <- paste0("\\tqc\\tx", center_pos, "\\tqr\\tx", right_pos)

  # Build content: left text, then \tab + center, then \tab + right
  content_parts <- character(0)

  if (!is.null(chrome$left)) {
    txt <- resolve_tokens(chrome$left, token_map, context)
    content_parts <- c(content_parts, rtf_escape_and_resolve(txt))
  }

  if (!is.null(chrome$center)) {
    txt <- resolve_tokens(chrome$center, token_map, context)
    content_parts <- c(content_parts, paste0("\\tab ", rtf_escape_and_resolve(txt)))
  } else if (!is.null(chrome$right)) {
    content_parts <- c(content_parts, "\\tab ")
  }

  if (!is.null(chrome$right)) {
    txt <- resolve_tokens(chrome$right, token_map, context)
    content_parts <- c(content_parts, paste0("\\tab ", rtf_escape_and_resolve(txt)))
  }

  if (length(content_parts) == 0L) return("")

  bold_on  <- if (isTRUE(chrome$bold)) "\\b " else ""
  bold_off <- if (isTRUE(chrome$bold)) "\\b0" else ""

  sb_str <- paste0("\\sb", as.integer(sb_twips))

  paste0("\\pard\\plain", sb_str, "\\ql", tab_defs,
         "\\fs", fs, " ",
         bold_on,
         paste0(content_parts, collapse = ""),
         bold_off,
         "\\par\n")
}


#' Build RTF header or footer paragraph with group wrapper
#'
#' Wraps rtf_chrome_content() in a {\header ...} or {\footer ...} group.
#' Used for pagehead ({\header}). Footer path uses rtf_footer_group().
#' @noRd
rtf_pagechrome_paragraph <- function(chrome, spec, token_map, group, context) {
  content <- rtf_chrome_content(chrome, spec, token_map, context)
  if (!nzchar(content)) return("")
  paste0("{\\", group, "\n", content, "}\n")
}


#' RTF title rows (repeating \trhdr rows — always used)
#'
#' Renders titles as full-width merged table rows with \trhdr so they repeat
#' on every page. The first title gets an IF PAGE > 1 field code to append
#' the continuation text (e.g., "(continued)") on pages 2+.
#'
#' @noRd
rtf_title_rows <- function(spec, columns, color_info) {
  titles <- spec$meta$titles
  if (length(titles) == 0L) return("")

  continuation <- spec$page$continuation
  col_names <- names(columns)
  ncol <- length(col_names)

  # Total table width in twips (single merged cell spanning all columns)
  total_twips <- sum(vapply(columns, function(c) inches_to_twips(c$width),
                            integer(1), USE.NAMES = FALSE))

  lines <- character(length(titles))
  for (idx in seq_along(titles)) {
    entry <- titles[[idx]]
    fs <- pt_to_half_pt(entry$font_size %||% spec$page$font_size)
    align_rtf <- fr_env$align_to_rtf[[entry$align %||% "center"]]
    bold_on  <- if (isTRUE(entry$bold)) "\\b " else ""
    bold_off <- if (isTRUE(entry$bold)) "\\b0" else ""
    content <- rtf_escape_and_resolve(entry$content)

    # Append continuation text to first title (always visible — RTF field
    # codes don't work in Word table cells, so we show it unconditionally;
    # the first page naturally starts with a group header without it looking
    # out of place since titles repeat via \trhdr on every page)
    cont_field <- ""
    if (idx == 1L && !is.null(continuation)) {
      cont_field <- paste0(" ", rtf_escape(continuation))
    }

    # Row: \trhdr with a single merged cell spanning full width
    row_def <- "\\trowd\\trhdr\\trqc"

    # Merge cells: first cell = \clmgf, rest = \clmrg
    cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                                integer(1), USE.NAMES = FALSE))
    cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
    if (ncol > 1L) {
      cell_defs <- paste0(cell_defs,
                          paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
    }

    # Cell content (only first cell has content, rest are empty merge targets)
    cell_content <- paste0(
      "\\pard\\intbl", align_rtf, "\\fs", fs, " ",
      bold_on, content, cont_field, bold_off, "\\cell"
    )
    if (ncol > 1L) {
      cell_content <- paste0(cell_content,
                             paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                    collapse = ""))
    }

    lines[idx] <- paste0(row_def, cell_defs, "\n", cell_content, "\\row\n")
  }

  # Blank \trhdr rows for titles_after spacing
  n_after <- spec$spacing$titles_after %||% 1L
  if (n_after > 0L) {
    blank_fs <- pt_to_half_pt(spec$page$font_size)
    blank_cell <- paste0("\\pard\\intbl\\fs", blank_fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else {
      ""
    }
    blank_row <- paste0("\\trowd\\trhdr\\trqc", cell_defs, "\n",
                         blank_cell, blank_empty, "\\row\n")
    lines <- c(lines, rep(blank_row, n_after))
  }

  paste0(lines, collapse = "")
}


#' RTF page-by group label as \trhdr rows
#'
#' Emits the page_by group label as a merged \trhdr row (same pattern as
#' title rows) so it stays within the table, has table content width,
#' and repeats on continuation pages.
#' @noRd
rtf_page_by_rows <- function(spec, columns, group_label) {
  col_names <- names(columns)
  ncol <- length(col_names)
  fs <- pt_to_half_pt(spec$page$font_size)
  pb_align <- fr_env$align_to_rtf[[spec$body$page_by_align %||% "left"]]
  pb_bold_on  <- if (isTRUE(spec$body$page_by_bold)) "\\b " else ""
  pb_bold_off <- if (isTRUE(spec$body$page_by_bold)) "\\b0" else ""
  content <- rtf_escape(group_label)

  # Merged cell defs spanning all columns
  cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                              integer(1), USE.NAMES = FALSE))
  cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
  if (ncol > 1L) {
    cell_defs <- paste0(cell_defs,
                        paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
  }

  row_def <- "\\trowd\\trhdr\\trqc"
  cell_content <- paste0(
    "\\pard\\intbl", pb_align, "\\fs", fs, " ",
    pb_bold_on, content, pb_bold_off, "\\cell"
  )
  if (ncol > 1L) {
    cell_content <- paste0(cell_content,
                           paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                  collapse = ""))
  }

  lines <- paste0(row_def, cell_defs, "\n", cell_content, "\\row\n")

  # Blank \trhdr rows for page_by_after spacing
  n_after <- spec$spacing$page_by_after %||% 1L
  if (n_after > 0L) {
    blank_cell <- paste0("\\pard\\intbl\\fs", fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    blank_row <- paste0(row_def, cell_defs, "\n",
                        blank_cell, blank_empty, "\\row\n")
    lines <- c(lines, rep(blank_row, n_after))
  }

  paste0(lines, collapse = "")
}


#' RTF spanner (spanning header) rows
#' @noRd
rtf_spanner_rows <- function(spec, columns, borders, color_info) {
  spans <- spec$header$spans
  if (length(spans) == 0L) return("")

  col_names <- names(columns)
  ncol <- length(col_names)
  fs <- pt_to_half_pt(spec$page$font_size)

  # Build cumulative column positions in twips
  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Group spans by level
  levels <- sort(unique(vapply(spans, function(s) s$level, integer(1))))

  lines <- character(0)
  for (lvl in rev(levels)) {
    lvl_spans <- Filter(function(s) s$level == lvl, spans)

    # Row definition: \trowd\trhdr
    row_def <- paste0("\\trowd\\trhdr\\trqc")

    # Build cell definitions
    cell_defs <- character(0)
    cell_contents <- character(0)
    j <- 1L

    while (j <= ncol) {
      # Check if this column starts a span
      matching_span <- NULL
      for (sp in lvl_spans) {
        sp_cols <- intersect(sp$columns, col_names)
        if (length(sp_cols) > 0L && col_names[j] == sp_cols[1L]) {
          matching_span <- sp
          break
        }
      }

      if (!is.null(matching_span)) {
        sp_cols <- intersect(matching_span$columns, col_names)
        sp_end <- match(sp_cols[length(sp_cols)], col_names)

        # First cell: \clmgf, rest: \clmrg
        # Bottom border on spanned cells when span$hline is TRUE
        span_border <- ""
        if (isTRUE(matching_span$hline)) {
          span_border <- paste0("\\clbrdrb\\brdrs\\brdrw", fr_env$rtf_spanner_brdrw, "\\brdrcf1")
        }
        border_str <- rtf_cell_border_string(borders$header, 1L, j, color_info)
        cell_defs <- c(cell_defs, paste0(border_str, span_border, "\\clmgf\\cellx", cum_widths[j]))
        for (k in (j + 1L):min(sp_end, ncol)) {
          border_str <- rtf_cell_border_string(borders$header, 1L, k, color_info)
          cell_defs <- c(cell_defs, paste0(border_str, span_border, "\\clmrg\\cellx", cum_widths[k]))
        }
        content <- rtf_escape_and_resolve(matching_span$label)
        cell_contents <- c(cell_contents,
                           paste0("\\pard\\intbl\\qc\\fs", fs, "\\b ",
                                  content, "\\b0\\cell"))
        for (k in (j + 1L):min(sp_end, ncol)) {
          cell_contents <- c(cell_contents, "\\pard\\intbl\\cell")
        }
        j <- sp_end + 1L
      } else {
        # Empty cell (no span covers this column)
        border_str <- rtf_cell_border_string(borders$header, 1L, j, color_info)
        cell_defs <- c(cell_defs, paste0(border_str, "\\cellx", cum_widths[j]))
        cell_contents <- c(cell_contents, "\\pard\\intbl\\cell")
        j <- j + 1L
      }
    }

    lines <- c(lines, paste0(row_def,
                              paste0(cell_defs, collapse = ""),
                              "\n",
                              paste0(cell_contents, collapse = ""),
                              "\\row\n"))
  }

  paste0(lines, collapse = "")
}


#' RTF column header row
#' @noRd
rtf_col_header_row <- function(spec, columns, borders, color_info,
                                label_overrides = NULL) {
  col_names <- names(columns)
  ncol <- length(col_names)

  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Header row index (after spanners)
  h_row <- 1L + n_spanner_levels(spec$header$spans)

  # Build header cell grid with style overrides
  header_valign <- spec$header$valign %||% "bottom"
  hgrid <- build_header_cell_grid(columns, spec$cell_styles, spec$page, h_row,
                                   default_valign = header_valign,
                                   header_cfg = spec$header)

  # \trowd\trhdr — marks row as header (repeats on each page)
  row_def <- "\\trowd\\trhdr\\trqc"

  cell_defs <- character(ncol)
  cell_contents <- character(ncol)

  for (j in seq_len(ncol)) {
    g <- hgrid[j, ]

    # Background color
    bg_str <- ""
    if (!is.na(g$bg) && nzchar(g$bg)) {
      ci <- color_info$index[[g$bg]]
      if (!is.null(ci)) bg_str <- paste0("\\clcbpat", ci)
    }

    # Vertical alignment (per-cell, from grid — respects style overrides)
    va_str <- fr_env$valign_to_rtf[g$valign]

    border_str <- rtf_cell_border_string(borders$header, h_row, j, color_info)
    cell_defs[j] <- paste0(border_str, bg_str, va_str, "\\cellx", cum_widths[j])

    # Use per-group override if available, else fall back to column label
    label <- label_overrides[col_names[j]]
    if (is.na(label) || is.null(label)) {
      label <- columns[[j]]$label
      if ((is.null(label) || !nzchar(label)) && !isTRUE(columns[[j]]$is_gap)) {
        label <- col_names[j]
      }
    }
    # Escape/resolve first, THEN replace \n with \line
    # (rtf_escape would double-escape the backslash in \line)
    content <- rtf_escape_and_resolve(label %||% "")
    content <- gsub("\n", "\\\\line ", content)

    fs <- pt_to_half_pt(g$font_size)
    align_rtf <- fr_env$align_to_rtf[[g$align]]

    # Inline formatting from style overrides
    fmt_on <- ""
    fmt_off <- ""
    if (isTRUE(g$bold)) { fmt_on <- paste0(fmt_on, "\\b "); fmt_off <- paste0("\\b0", fmt_off) }
    if (isTRUE(g$italic)) { fmt_on <- paste0(fmt_on, "\\i "); fmt_off <- paste0("\\i0", fmt_off) }
    if (isTRUE(g$underline)) { fmt_on <- paste0(fmt_on, "\\ul "); fmt_off <- paste0("\\ulnone", fmt_off) }

    # Foreground color
    fg_str <- ""
    if (!is.na(g$fg) && g$fg != "#000000") {
      ci <- color_info$index[[g$fg]]
      if (!is.null(ci)) fg_str <- paste0("\\cf", ci, " ")
    }

    cell_contents[j] <- paste0("\\pard\\plain\\intbl", align_rtf,
                                "\\fs", fs, " ",
                                fg_str, fmt_on, content, fmt_off, "\\cell")
  }

  paste0(row_def,
         paste0(cell_defs, collapse = ""),
         "\n",
         paste0(cell_contents, collapse = ""),
         "\\row\n")
}


#' RTF body rows
#' @noRd
rtf_body_rows <- function(spec, data, columns, cell_grid, borders, color_info) {
  if (nrow(data) == 0L) return("")

  col_names <- names(columns)
  ncol <- length(col_names)
  nr <- nrow(data)

  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Precompute decimal before-widths from actual content
  decimal_before <- compute_decimal_before_twips(data, columns, cell_grid,
                                                  spec$page$font_family,
                                                  spec$page$font_size)
  is_decimal_col <- !is.na(decimal_before)

  # Precompute decimal sub-cell geometry (column-dependent, row-independent)
  dec_sub1_end <- integer(ncol)
  dec_sub2_end <- integer(ncol)
  decimal_pad <- fr_env$rtf_decimal_pad
  decimal_pad_str <- paste0("\\clpadl", decimal_pad,
                            "\\clpadr", decimal_pad,
                            "\\clpadfl3\\clpadfr3")
  for (j in which(is_decimal_col)) {
    sub1_width <- decimal_before[j] + decimal_pad * 2L
    left_edge <- if (j == 1L) 0L else cum_widths[j - 1L]
    dec_sub1_end[j] <- left_edge + sub1_width
    dec_sub2_end[j] <- cum_widths[j]
  }

  empty_cell <- "\\pard\\intbl\\cell"

  # Precompute keep-together mask from group_by: TRUE if row should stay with next
  keep_mask <- build_keep_mask(data, spec$body$group_by)

  # Precompute row heights from fr_row_style objects
  row_heights <- build_row_heights(nr, spec$cell_styles)

  lines <- vector("list", nr)
  for (i in seq_len(nr)) {
    # Row properties
    keep_str <- if (isTRUE(keep_mask[i])) "\\trkeep" else ""
    height_str <- ""
    if (!is.na(row_heights[i])) {
      height_str <- paste0("\\trrh", inches_to_twips(row_heights[i]))
    }
    row_def <- paste0("\\trowd\\trqc", keep_str, height_str)

    # Cell definitions with borders
    # Decimal columns emit TWO sub-cells, so collect into a list
    cell_defs <- vector("list", ncol)
    for (j in seq_len(ncol)) {
      # Background color
      grid_row <- cell_grid$row_idx == i & cell_grid$col_idx == j
      bg <- cell_grid$bg[grid_row]
      bg_str <- ""
      if (!is.na(bg) && nzchar(bg)) {
        ci <- color_info$index[[bg]]
        if (!is.null(ci)) bg_str <- paste0("\\clcbpat", ci)
      }

      # Vertical alignment
      va <- cell_grid$valign[grid_row]
      va_str <- fr_env$valign_to_rtf[va]

      if (is_decimal_col[j]) {
        # Sub-cell split: two adjacent cells for right-before / left-after
        border_str1 <- rtf_cell_border_string(borders$body, i, j, color_info,
                                               sides = c("top", "bottom", "left"))
        border_str2 <- rtf_cell_border_string(borders$body, i, j, color_info,
                                               sides = c("top", "bottom", "right"))
        cell_defs[[j]] <- paste0(
          border_str1, bg_str, va_str, decimal_pad_str, "\\cellx", dec_sub1_end[j],
          border_str2, bg_str, va_str, decimal_pad_str, "\\cellx", dec_sub2_end[j]
        )
      } else {
        border_str <- rtf_cell_border_string(borders$body, i, j, color_info)
        cell_defs[[j]] <- paste0(border_str, bg_str, va_str, "\\cellx", cum_widths[j])
      }
    }

    # Cell contents (decimal columns emit two \cell entries)
    cell_contents <- vector("list", ncol)
    for (j in seq_len(ncol)) {
      grid_row <- which(cell_grid$row_idx == i & cell_grid$col_idx == j)
      if (length(grid_row) == 0L) {
        cell_contents[[j]] <- if (is_decimal_col[j]) {
          paste0(empty_cell, empty_cell)
        } else {
          empty_cell
        }
        next
      }
      g <- cell_grid[grid_row, ]

      fs <- pt_to_half_pt(g$font_size)

      # Inline formatting
      fmt_on <- ""
      fmt_off <- ""
      if (isTRUE(g$bold)) { fmt_on <- paste0(fmt_on, "\\b "); fmt_off <- paste0("\\b0", fmt_off) }
      if (isTRUE(g$italic)) { fmt_on <- paste0(fmt_on, "\\i "); fmt_off <- paste0("\\i0", fmt_off) }
      if (isTRUE(g$underline)) { fmt_on <- paste0(fmt_on, "\\ul "); fmt_off <- paste0("\\ulnone", fmt_off) }

      # Foreground color
      fg_str <- ""
      if (!is.na(g$fg) && g$fg != "#000000") {
        ci <- color_info$index[[g$fg]]
        if (!is.null(ci)) fg_str <- paste0("\\cf", ci, " ")
      }

      # Indentation
      indent_str <- ""
      if (g$indent > 0) {
        indent_twips <- inches_to_twips(g$indent)
        indent_str <- paste0("\\li", indent_twips)
      }

      # Decimal alignment: sub-cell split (right-aligned before, left-aligned after)
      if (identical(g$align, "decimal") && is_decimal_col[j]) {
        trimmed <- trimws(g$content)
        if (nzchar(trimmed)) {
          parts <- split_at_decimal(trimmed)
          before_esc <- rtf_escape_and_resolve(parts$before)
          after_esc  <- rtf_escape_and_resolve(trimws(parts$after, which = "left"))
          cell_contents[[j]] <- paste0(
            "\\pard\\plain\\intbl\\qr", indent_str,
            "\\fs", fs, " ",
            fg_str, fmt_on, before_esc, fmt_off, "\\cell",
            "\\pard\\plain\\intbl\\ql",
            "\\fs", fs, " ",
            fg_str, fmt_on, after_esc, fmt_off, "\\cell"
          )
        } else {
          cell_contents[[j]] <- paste0(empty_cell, empty_cell)
        }
      } else if (is_decimal_col[j]) {
        # Non-decimal cell in a decimal column (style override)
        align_rtf <- fr_env$align_to_rtf[[g$align]]
        content <- rtf_escape_and_resolve(g$content)
        cell_contents[[j]] <- paste0(
          "\\pard\\plain\\intbl", align_rtf, indent_str,
          "\\fs", fs, " ",
          fg_str, fmt_on, content, fmt_off, "\\cell",
          empty_cell
        )
      } else {
        align_rtf <- fr_env$align_to_rtf[[g$align]]
        content <- rtf_escape_and_resolve(g$content)
        cell_contents[[j]] <- paste0(
          "\\pard\\plain\\intbl", align_rtf, indent_str,
          "\\fs", fs, " ",
          fg_str, fmt_on, content, fmt_off, "\\cell"
        )
      }
    }

    lines[[i]] <- paste0(row_def,
                        paste0(unlist(cell_defs), collapse = ""),
                        "\n",
                        paste0(unlist(cell_contents), collapse = ""),
                        "\\row\n")
  }

  paste0(lines, collapse = "")
}





# ══════════════════════════════════════════════════════════════════════════════
# Single-Page Estimation + Body Footnotes
# ══════════════════════════════════════════════════════════════════════════════

#' Estimate whether a section fits on one page
#'
#' Counts row equivalents for all table components (pagehead, titles, spacing,
#' page_by, spanners, header, body, footnotes) and compares to the printable
#' page height. Used to decide whether footnotes should be rendered as body
#' rows (single-page) or in the `{\footer}` group (multi-page).
#'
#' @param spec Finalized fr_spec object.
#' @param group_data Data frame for the current page_by group.
#' @return Logical scalar. TRUE if content fits on one page.
#' @noRd
estimate_single_page <- function(spec, group_data) {
  row_twips <- row_height_twips(spec$page$font_size)
  printable_h <- printable_area_twips(spec$page)[["height"]]

  # Count rows: pagehead + titles + spacing + page_by + spanners + header + body
  n_rows <- 0L
  if (!is.null(spec$pagehead)) {
    n_rows <- n_rows + 1L + (spec$spacing$pagehead_after %||% 1L)
  }
  n_rows <- n_rows + length(spec$meta$titles %||% list())
  n_rows <- n_rows + (spec$spacing$titles_after %||% 1L)
  # page_by label + spacing (if present, estimated at 2 rows)
  if (!is.null(spec$body$page_by)) n_rows <- n_rows + 2L
  n_rows <- n_rows + n_spanner_levels(spec$header$spans) + 1L  # spanners + col header
  n_rows <- n_rows + nrow(group_data)

  # Also count footnote rows if they'd go in body
  n_fn <- length(spec$meta$footnotes %||% list())
  if (n_fn > 0L) {
    n_rows <- n_rows + n_fn + (spec$spacing$footnotes_before %||% 1L)
  }

  n_rows <= floor(printable_h / row_twips)
}


#' Render footnotes as body table rows (non-repeating)
#'
#' For single-page output, footnotes are rendered as merged table rows in the
#' body (without `\trhdr`) so they appear at the bottom of the table content
#' rather than in the page footer margin. Follows the same merged-cell pattern
#' as [rtf_title_rows()].
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param is_last Logical. TRUE for the final section.
#' @param borders Border structures from resolve_borders().
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_body_footnotes <- function(spec, columns, is_last, borders, color_info) {
  footnotes <- spec$meta$footnotes %||% list()

  entries <- if (is_last) {
    footnotes
  } else {
    Filter(function(fn) fn$placement == "every", footnotes)
  }

  rtf_footnote_rows(spec, columns, entries, color_info)
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Cell Border Helpers
#' Render "last" placement footnotes as body table rows
#'
#' For multi-page output, "last" footnotes are rendered as non-repeating
#' body rows at the end of the final section's data. They appear only once.
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param last_entries List of footnote entries with placement = "last".
#' @param borders Border structures from resolve_borders().
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_body_footnotes_last <- function(spec, columns, last_entries,
                                     borders, color_info) {
  rtf_footnote_rows(spec, columns, last_entries, color_info)
}

#' Shared helper: render footnote entries as RTF body table rows
#'
#' Generates merged-cell table rows for footnote content, with an optional
#' bottom rule row and spacing rows above the footnotes.
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param entries List of footnote entries to render.
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_footnote_rows <- function(spec, columns, entries, color_info) {
  if (length(entries) == 0L) return("")

  ncol <- length(columns)

  # Cumulative column widths for merged cell defs
  cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                              integer(1), USE.NAMES = FALSE))
  cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
  if (ncol > 1L) {
    cell_defs <- paste0(cell_defs,
                        paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
  }

  # Bottom rule and footnote separator
  bottom_rule <- find_bottom_rule(spec)
  br_str <- ""
  if (!is.null(bottom_rule)) {
    bw <- pt_to_twips(bottom_rule$width)
    ci <- color_info$index[[bottom_rule$fg]] %||% 1L
    br_str <- paste0("\\clbrdrt\\brdrs\\brdrw", bw, "\\brdrcf", ci)
  }
  sep_str <- ""
  if (isTRUE(spec$meta$footnote_separator)) {
    sep_str <- "\\clbrdrt\\brdrs\\brdrw5\\brdrcf1"
  }
  rule_border <- paste0(br_str, sep_str)

  # Pre-allocate lines list: rule row + blank spacing rows + footnote entry rows
  n_before <- spec$spacing$footnotes_before %||% 1L
  has_rule <- nzchar(rule_border)
  lines <- vector("list", as.integer(has_rule) + n_before + length(entries))
  li <- 0L

  # 1) Bottom rule row — immediately after last body row
  if (has_rule) {
    rule_cell_defs <- paste0(rule_border, "\\clmgf\\cellx", cum_widths[1L])
    if (ncol > 1L) {
      rule_cell_defs <- paste0(rule_cell_defs,
                               paste0(rule_border, "\\clmrg\\cellx",
                                      cum_widths[-1L], collapse = ""))
    }
    rule_fs <- pt_to_half_pt(2L)
    rule_cell <- paste0("\\pard\\intbl\\fs", rule_fs, " \\cell")
    rule_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    li <- li + 1L
    lines[[li]] <- paste0("\\trowd\\trqc", rule_cell_defs, "\n",
                           rule_cell, rule_empty, "\\row\n")
  }

  # 2) Spacing blank rows (footnotes_before)
  if (n_before > 0L) {
    blank_fs <- pt_to_half_pt(spec$page$font_size)
    blank_cell <- paste0("\\pard\\intbl\\fs", blank_fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    blank_row <- paste0("\\trowd\\trqc", cell_defs, "\n",
                        blank_cell, blank_empty, "\\row\n")
    for (k in seq_len(n_before)) {
      li <- li + 1L
      lines[[li]] <- blank_row
    }
  }

  # 3) Footnote content rows
  for (idx in seq_along(entries)) {
    fn <- entries[[idx]]
    fs <- pt_to_half_pt(fn$font_size %||% spec$page$font_size)
    align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
    content <- rtf_escape_and_resolve(fn$content)

    cell_content <- paste0(
      "\\pard\\intbl", align_rtf, "\\fs", fs, " ", content, "\\cell"
    )
    if (ncol > 1L) {
      cell_content <- paste0(cell_content,
                             paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                    collapse = ""))
    }

    li <- li + 1L
    lines[[li]] <- paste0("\\trowd\\trqc", cell_defs, "\n",
                           cell_content, "\\row\n")
  }

  paste0(lines, collapse = "")
}


# ══════════════════════════════════════════════════════════════════════════════

#' Build RTF border control words for a single cell
#'
#' @param border_matrices List with top, bottom, left, right matrices.
#' @param i Row index.
#' @param j Column index.
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF border control words.
#' @noRd
rtf_cell_border_string <- function(border_matrices, i, j, color_info,
                                   sides = c("top", "bottom", "left", "right")) {
  rtf_side_keywords <- c(
    top    = "\\clbrdrt",
    bottom = "\\clbrdrb",
    left   = "\\clbrdrl",
    right  = "\\clbrdrr"
  )

  parts <- character(0)
  for (side in sides) {
    mat <- border_matrices[[side]]
    if (i > nrow(mat) || j > ncol(mat)) next
    bs <- mat[i, j][[1L]]
    if (is.null(bs)) next

    # RTF border: \clbrdr{side}\brdrs\brdrwN\brdrcfN
    linestyle_rtf <- fr_env$linestyle_rtf[[bs$linestyle]] %||% "\\brdrs"
    width_twips <- pt_to_twips(bs$width)
    color_idx <- color_info$index[[bs$fg]] %||% 1L

    parts <- c(parts, paste0(
      rtf_side_keywords[[side]],
      linestyle_rtf,
      "\\brdrw", width_twips,
      "\\brdrcf", color_idx
    ))
  }

  paste0(parts, collapse = "")
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Write Helper
# ══════════════════════════════════════════════════════════════════════════════

#' Write text to an RTF connection as raw bytes
#' @noRd
rtf_write <- function(con, text) {
  if (!nzchar(text)) return(invisible(NULL))
  writeBin(charToRaw(text), con)
  invisible(NULL)
}
