# ─────────────────────────────────────────────────────────────────────────────
# api.R — Pipeline entry point: fr_table
#
# Pipeline contract (all verbs follow this):
#   - First argument is always an fr_spec (validated with check_fr_spec)
#   - Returns a modified fr_spec — the pipeline is immutable
#   - Side effects happen only at fr_render()
#
# See also: api-content.R (fr_titles, fr_footnotes),
#           api-theme.R (fr_theme, fr_theme_get, fr_theme_reset),
#           api-cols.R (fr_cols, fr_select), api-header.R (fr_header),
#           api-spans.R (fr_spans), api-rows.R (fr_rows),
#           api-page.R (fr_page, fr_pagehead, fr_pagefoot),
#           api-rules.R (fr_hlines, fr_vlines, fr_grid),
#           api-style.R (fr_style, fr_row_style, fr_col_style, fr_styles).
# ─────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_table — Pipeline entry point
# ══════════════════════════════════════════════════════════════════════════════

#' Start a tlframe Table Pipeline
#'
#' @description
#'
#' `fr_table()` is the entry point for every **tlframe** pipeline. It wraps a
#' presentation-ready data frame in an `fr_spec` object, which you then
#' configure with the `fr_*()` verbs and finally render with `fr_render()`.
#'
#' The data frame should already contain the rows and columns you want to
#' display — tlframe does not summarise or reshape data. Use packages such as
#' **gt**, **Tplyr**, **rtables**, **tidytlg**, or **tfrmt** to summarise your
#' data, then hand the summary data frame off to `fr_table()`.
#'
#' @param data A data frame (or tibble). Must already be presentation-ready:
#'   each row maps to one table row, each column to one table column. No
#'   summarisation is performed by tlframe.
#'
#' @return An `fr_spec` object. Pass it to any `fr_*()` verb via `|>`.
#'
#' @section Pipeline overview:
#' ```
#' data |>
#'   fr_table()                        # start
#'   |> fr_titles(...)                 # titles above the table
#'   |> fr_footnotes(...)              # footnotes below
#'   |> fr_cols(col = fr_col(...))     # column widths / labels
#'   |> fr_header(n = ..., format = .) # N-count labels / header styling
#'   |> fr_spans(...)                  # spanning headers
#'   |> fr_rows(page_by = ...)         # pagination / grouping
#'   |> fr_page(orientation = ...)     # page layout
#'   |> fr_pagehead(left = ...)        # running header
#'   |> fr_pagefoot(right = ...)       # running footer
#'   |> fr_hlines("header")         # horizontal rules
#'   |> fr_vlines("box")               # vertical rules
#'   |> fr_styles(fr_row_style(...))   # cell / row / column styling
#'   |> fr_render("output.rtf")        # render to file
#' ```
#'
#' @section Tips:
#' * Any `fr_*()` verb can be called multiple times; most **replace** (not
#'   append) the previous setting. Exceptions: `fr_spans()` and `fr_styles()`
#'   **append** on repeated calls.
#' * Column order in the rendered table matches the column order of `data`.
#'   Reorder columns in `data` before calling `fr_table()` if needed.
#' * Invisible columns (e.g. grouping keys) can be hidden via
#'   `fr_cols(col = fr_col(visible = FALSE))` rather than dropping them
#'   from `data`.
#'
#' @examples
#' ## ── Minimal: one-step pipeline ───────────────────────────────────────────
#'
#' tbl_demog |> fr_table()
#'
#' ## ── Typical regulatory pipeline ─────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
#'     "Full Analysis Set"
#'   ) |>
#'   fr_footnotes("[a] Percentages based on the number of subjects in each arm.") |>
#'   fr_hlines("header") |>
#'   fr_page(orientation = "landscape", font_size = 9)
#'
#' ## ── AE by SOC/PT with pagination ─────────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.2 Adverse Events by System Organ Class and Preferred Term",
#'             "Safety Analysis Set") |>
#'   fr_rows(page_by = "soc", group_by = "soc") |>
#'   fr_hlines("header")
#'
#' ## ── Store spec, reuse, branch ────────────────────────────────────────────
#'
#' base_spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_page(orientation = "landscape", font_size = 9)
#'
#' # Branch A: portrait with smaller font
#' spec_a <- base_spec |> fr_page(orientation = "portrait", font_size = 8)
#'
#' # Branch B: add titles and render
#' spec_b <- base_spec |>
#'   fr_titles("Table 14.1.1 Demographics") |>
#'   fr_hlines("booktabs")
#'
#' @seealso [fr_titles()], [fr_footnotes()], [fr_cols()], [fr_header()],
#'   [fr_spans()], [fr_rows()], [fr_page()], [fr_hlines()], [fr_vlines()],
#'   [fr_styles()], [fr_config()], [fr_render()]
#'
#' @export
fr_table <- function(data) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.", call = caller_env())
  }
  spec <- new_fr_spec(data)
  spec <- apply_config(spec)
  spec <- apply_fr_theme(spec)
  spec
}
