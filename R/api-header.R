# ──────────────────────────────────────────────────────────────────────────────
# api-header.R — Column header presentation verb: fr_header
# ──────────────────────────────────────────────────────────────────────────────


#' Configure Column Header Presentation
#'
#' @description
#'
#' The single owner of all header presentation settings: alignment, vertical
#' alignment, N-count label formatting, bold, background/foreground colour,
#' and font size. These defaults apply to **all** column header cells unless
#' overridden by per-column `fr_col(header_align = ...)` or
#' `fr_style(region = "header", ...)`.
#'
#' [fr_cols()] owns column **structure** (labels, widths, body alignment,
#' visibility); `fr_header()` owns header **presentation**. There is no
#' overlap — N-count formatting, header alignment, and header valign live
#' exclusively here.
#'
#' N-count label formatting is **deferred** — the `n` and `format` values
#' are stored on the spec and resolved during rendering. This makes
#' `fr_header()` and [fr_cols()] order-independent (ggplot2 pattern).
#'
#' Calling `fr_header()` again **replaces** the previous header config
#' (except spans, which are managed by [fr_spans()]).
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param align Default horizontal alignment for all header cells. One of
#'   `"left"`, `"center"`, `"right"`, or `NULL` (inherit from column).
#'   Overridden per-column by `fr_col(header_align = ...)`.
#' @param valign Default vertical alignment. One of `"top"`, `"middle"`,
#'   `"bottom"` (default). Relevant when header rows have unequal height.
#' @param n Subject counts for N-count column header labels. Three forms:
#'
#'   * **Named numeric vector** — global N, same for all page groups.
#'     Names must match data column names.
#'     Example: `c(placebo = 45, zom_50mg = 44)`.
#'
#'   * **Named list of named numeric vectors** — per-group N. List names
#'     must match `page_by` group values. Each element is a named numeric
#'     vector keyed by column name.
#'     Example: `list("Blood Pressure" = c(placebo = 42, zom_50mg = 40),
#'                     "Heart Rate" = c(placebo = 45, zom_50mg = 44))`.
#'
#'   * **Function** — called **once** with the full `spec$data` during
#'     `finalize_spec()`. Must accept one argument (the data frame) and
#'     return either:
#'
#'     \describe{
#'       \item{**Named numeric vector**}{Treatment names → counts.
#'         Names are matched to column display labels (case-insensitive).
#'         Same N on every page (global).
#'         Example return: `c("Placebo" = 45, "Zomerane 50mg" = 44)`.}
#'       \item{**2-column data frame**}{Column 1 = treatment labels,
#'         column 2 = counts (numeric). Same N on every page (global).
#'         Equivalent to a named vector.}
#'       \item{**3-column data frame**}{Column 1 = page\_by group values,
#'         column 2 = treatment labels, column 3 = counts (numeric).
#'         Different N per page\_by group — tlframe slices per group at
#'         render time.}
#'     }
#'
#'     For external data (e.g. ADSL), capture via closure:
#'     `\(d) adsl |> ...` (the `d` argument receives `spec$data` but
#'     can be ignored in favour of the captured external data).
#'
#' @param format A [glue][glue::glue]-style format string for N-count
#'   labels. Available tokens: `{label}` (column display label) and
#'   `{n}` (count).
#'   Default `NULL` (no N-count formatting).
#'   Example: `"{label}\n(N={n})"`.
#'
#' @param bold Logical or `NULL`. Whether header cells are bold. Default
#'   `NULL` inherits the built-in default (FALSE). Set `TRUE` explicitly
#'   if you want bold headers.
#' @param bg Background colour for header cells: hex string (`"#003366"`) or
#'   CSS named colour (`"steelblue"`, `"lightgray"`, etc.). `NULL` inherits.
#' @param fg Foreground (text) colour for header cells: hex string or CSS
#'   named colour. `NULL` inherits.
#' @param font_size Font size in points for header cells. `NULL` inherits
#'   from page font size.
#' @param repeat_on_page Logical. Whether to repeat the column header on
#'   every page. Default `TRUE` (standard for regulatory tables).
#' @return A modified `fr_spec`. Header config stored in `spec$header`.
#'
#' @section Priority chain for header alignment:
#' ```
#' config header.align < fr_header(align) < fr_col(header_align) < fr_style(region="header", align)
#' ```
#'
#' @section N-count label resolution:
#' N-count labels are resolved at different stages depending on the form of
#' `n`:
#'
#' * **Named numeric vector** (global) — resolved once during
#'   `finalize_spec()`. All page groups share the same labels.
#' * **Named list** — resolved per page group in the render loop.
#'   Each group gets its own header labels via a label override map.
#' * **Function** — called once during `finalize_spec()`. The return
#'   type determines global (named vector or 2-col df) vs per-group
#'   (3-col df) resolution.
#'
#' The `{label}` token in `format` expands to the column's resolved label
#' (from `fr_col(label = ...)` or auto-generated). This means
#' `fr_header()` and `fr_cols()` can be called in any order.
#'
#' @section Regulatory conventions:
#' Many pharma TFL outputs use **bold, centered** column headers with
#' `valign = "bottom"` so that short labels (e.g. "Characteristic") align
#' at the bottom when adjacent columns have multi-line labels (e.g.
#' "Placebo\\n(N=45)"). The `{label}\\n(N={n})` format is the standard
#' ICH/CDISC convention for treatment-arm headers. By default, headers are
#' **not bold** and inherit alignment from the column's `align` setting —
#' use `fr_header(bold = TRUE, align = "center")` to opt in.
#'
#' @examples
#' ## ── Center all headers with bottom valign ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(align = "center", valign = "bottom", bold = TRUE)
#'
#' ## ── Global N-count (same for all groups) ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg", align = "right"),
#'     placebo        = fr_col("Placebo", align = "right")
#'   ) |>
#'   fr_header(
#'     n = c(zom_50mg = 45, placebo = 45),
#'     format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Per-group N-count (different N per page_by group) ────────────────────
#'
#' # Hardcode N per parameter — each page_by group shows its own N
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_header(
#'     n = list(
#'       "Systolic BP (mmHg)"  = c(placebo = 45, zom_50mg = 45, zom_100mg = 39, total = 129),
#'       "Heart Rate (bpm)"    = c(placebo = 45, zom_50mg = 38, zom_100mg = 40, total = 123),
#'       "Temperature (C)"     = c(placebo = 42, zom_50mg = 39, zom_100mg = 37, total = 118)
#'     ),
#'     format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Function N-count — named vector return (global) ───────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(
#'     n = \(d) c(placebo = nrow(d[d$characteristic == "n", ]),
#'                zom_50mg = nrow(d[d$characteristic == "n", ])),
#'     format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Function N-count — 2-col data frame return (global) ──────────────────
#'
#' # Function receives spec$data, returns data frame with treatment + count.
#' # Treatment labels are matched to column labels (case-insensitive).
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     zom_50mg = fr_col("Zomerane 50 mg"),
#'     placebo  = fr_col("Placebo")
#'   ) |>
#'   fr_header(
#'     n = \(d) data.frame(
#'       trt = c("Zomerane 50 mg", "Placebo"),
#'       n   = c(45L, 45L)
#'     ),
#'     format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Function N-count using closure for external data ─────────────────────
#'
#' # Capture external dataset via closure — d argument is ignored
#' \donttest{
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   tbl_vs |>
#'     fr_table() |>
#'     fr_rows(page_by = "param") |>
#'     fr_header(
#'       n = \(d) advs |>
#'         dplyr::filter(SAFFL == "Y") |>
#'         dplyr::group_by(PARAM, TRTA) |>
#'         dplyr::summarise(n = dplyr::n_distinct(USUBJID), .groups = "drop"),
#'       format = "{label}\n(N={n})"
#'     )
#' }
#' }
#'
#' ## ── Header background colour (hex or CSS named colour) ──────────────────
#'
#' # Hex colour
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bg = "#E0E0E0", bold = TRUE)
#'
#' # CSS named colour (148 available: lavender, aliceblue, gainsboro, etc.)
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bg = "lavender", fg = "midnightblue", bold = TRUE)
#'
#' ## ── Order-independent: fr_header first, then fr_cols ─────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(n = c(zom_50mg = 45), format = "{label}\n(N={n})") |>
#'   fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right"))
#'
#' @seealso [fr_cols()] for column structure, [fr_spans()] for spanning
#'   headers, [fr_col()] for per-column `header_align` overrides,
#'   [fr_style()] with `region = "header"` for cell-level overrides,
#'   [fr_config()] for setting header defaults via `_tlframe.yml`.
#'
#' @export
fr_header <- function(spec, align = NULL, valign = NULL,
                      bold = NULL, bg = NULL, fg = NULL,
                      font_size = NULL, repeat_on_page = NULL,
                      n = NULL, format = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  if (!is.null(align))  align  <- match_arg_fr(align,  fr_env$valid_aligns,  call = call)
  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(bold))   check_scalar_lgl(bold, arg = "bold", call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)
  if (!is.null(bg)) bg <- resolve_color(bg, call = call)
  if (!is.null(fg)) fg <- resolve_color(fg, call = call)
  if (!is.null(repeat_on_page)) check_scalar_lgl(repeat_on_page, arg = "repeat_on_page", call = call)

  # Validate N-count parameters (3 forms: named numeric, named list, function)
  validate_n_param(n = n, format = format, call = call)

  # Preserve existing spans — fr_header replaces everything else
  old_spans <- spec$header$spans

  spec$header <- new_fr_header(
    spans          = old_spans,
    repeat_on_page = repeat_on_page %||% spec$header$repeat_on_page,
    valign         = valign    %||% spec$header$valign,
    align          = align     %||% spec$header$align,
    bold           = bold      %||% spec$header$bold,
    bg             = bg        %||% spec$header$bg,
    fg             = fg        %||% spec$header$fg,
    font_size      = font_size %||% spec$header$font_size,
    n              = n         %||% spec$header$n,
    format         = format    %||% spec$header$format,
    span_gap       = spec$header$span_gap
  )

  spec
}
