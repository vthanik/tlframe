# ──────────────────────────────────────────────────────────────────────────────
# api-style.R — Cell styling verbs: fr_style, fr_row_style, fr_col_style,
#               fr_styles, fr_rows_matches, fr_style_explain
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows_matches — Row selector helper for fr_row_style / fr_style
# ══════════════════════════════════════════════════════════════════════════════

#' Select Rows by Column Value or Pattern
#'
#' @description
#'
#' Creates a row selector object for use in the `rows` argument of
#' [fr_row_style()] or [fr_style()]. At style-application time (inside
#' [fr_styles()]), the selector is evaluated against the table data frame to
#' produce the matching row positions.
#'
#' This avoids hard-coding integer row numbers when styling rows by their
#' content — for example, bolding every "Total" row or colouring every
#' p-value row red. The selector is data-driven: if rows are reordered or
#' filtered, the styles automatically track the correct rows.
#'
#' @param col Character scalar. Name of the data column to match against.
#'   Must exist in the data frame passed to [fr_table()].
#' @param value Scalar. Exact value to match (using `==`). Mutually exclusive
#'   with `pattern`. Supports any atomic type (character, numeric, logical).
#' @param pattern Character scalar. A regular expression passed to [grep()]
#'   (Perl-compatible). Mutually exclusive with `value`. The regex is
#'   matched against the character representation of the column values.
#' @param ignore.case Logical. Whether `pattern` matching is case-insensitive.
#'   Default `FALSE`. Ignored when `value` is used.
#'
#' @return An `fr_rows_selector` object for use in [fr_row_style()] or
#'   [fr_style()].
#'
#' @section Common patterns:
#' | Pattern | Matches |
#' |---------|---------|
#' | `value = "Total"` | Exact string "Total" |
#' | `pattern = "^Total"` | Starts with "Total" |
#' | `pattern = "p[- .]?value"` | "p-value", "p value", "p.value" |
#' | `pattern = "^\\\\s"` | Rows starting with whitespace (indented) |
#' | `pattern = "^[A-Z]"` | Rows starting with uppercase (group headers) |
#'
#' @examples
#' ## ── Bold every "Total" row ────────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_styles(
#'     fr_row_style(rows = fr_rows_matches("characteristic", "Total"), bold = TRUE)
#'   )
#'
#' ## ── Red text for p-value rows (regex) ────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_styles(
#'     fr_row_style(
#'       rows = fr_rows_matches("characteristic", pattern = "^p[- .]?value",
#'                               ignore.case = TRUE),
#'       fg = "#CC0000", italic = TRUE
#'     )
#'   )
#'
#' ## ── Combine with cell styles ──────────────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_styles(
#'     # Shade every SOC header row
#'     fr_row_style(rows = fr_rows_matches("row_type", "soc"), bg = "#F0F0F0", bold = TRUE),
#'     # Red text for all PT rows with a p-value pattern
#'     fr_row_style(rows = fr_rows_matches("row_type", "pt"), fg = "#333333")
#'   )
#'
#' @seealso [fr_row_style()] for row styling, [fr_style()] for cell styling,
#'   [fr_styles()] to apply styles to a spec.
#'
#' @export
fr_rows_matches <- function(col, value = NULL, pattern = NULL,
                             ignore.case = FALSE) {
  call <- caller_env()

  if (!is.character(col) || length(col) != 1L) {
    cli_abort("{.arg col} must be a single character string (column name).", call = call)
  }
  if (is.null(value) && is.null(pattern)) {
    cli_abort("Provide either {.arg value} (exact match) or {.arg pattern} (regex).",
              call = call)
  }
  if (!is.null(value) && !is.null(pattern)) {
    cli_abort("Provide {.arg value} or {.arg pattern}, not both.", call = call)
  }
  if (!is.null(pattern) && (!is.character(pattern) || length(pattern) != 1L)) {
    cli_abort("{.arg pattern} must be a single character regex string.", call = call)
  }
  check_scalar_lgl(ignore.case, arg = "ignore.case", call = call)

  structure(
    list(col         = col,
         value       = value,
         pattern     = pattern,
         ignore.case = ignore.case),
    class = "fr_rows_selector"
  )
}



# ══════════════════════════════════════════════════════════════════════════════
# fr_style / fr_row_style / fr_col_style — Style constructors
# ══════════════════════════════════════════════════════════════════════════════

#' Define a Cell Style Override
#'
#' @description
#'
#' Creates a cell-level style object for use in [fr_styles()]. Targets
#' individual cells by row and column position (or by region keyword). Only
#' properties you explicitly set override the base style; `NULL` means
#' "inherit from the base style".
#'
#' For row-level or column-level styling, prefer [fr_row_style()] and
#' [fr_col_style()] which have a simpler interface for uniform row/column
#' formatting.
#'
#' @param region Region to target. One of:
#'   * `"body"` (default) — data body rows.
#'   * `"header"` — column header row(s) and spanner row(s).
#'   * `"stub"` — the first (row-label) column across all body rows.
#'     For **multiple stub columns**, use `region = "body"` with
#'     `cols = c("lab_name", "visit")` instead.
#' @param rows Integer vector of row positions, `"all"` (all rows in the
#'   region), or `NULL` (all rows). Row indices are 1-based relative to the
#'   region. Multiple indices are supported: `rows = c(1L, 3L, 5L)`.
#' @param cols Character vector of column names, or `NULL` (all columns).
#' @param bold,italic,underline Logical or `NULL` to inherit.
#' @param fg Foreground (text) colour: hex string or named colour, or `NULL`.
#' @param bg Background (fill) colour: hex string or named colour, or `NULL`.
#' @param font_size Font size in points, or `NULL` to inherit.
#' @param align Horizontal alignment override: `"left"`, `"center"`, `"right"`,
#'   `"decimal"`, or `NULL`. Controls how text flows within the cell
#'   (left-to-right positioning). In RTF this maps to `\ql`, `\qc`, `\qr`;
#'   in LaTeX to `l`, `c`, `r` column types. See **Alignment model** below.
#' @param valign Vertical alignment override: `"top"`, `"middle"`, `"bottom"`,
#'   or `NULL`. Controls where content sits within a cell when the row is
#'   taller than the content (e.g., due to multi-line labels in adjacent
#'   cells). In RTF this maps to `\clvertalt`, `\clvertalc`, `\clvertalb`;
#'   in LaTeX to `p` (top), `m` (middle), `b` (bottom). See **Alignment
#'   model** below.
#' @param indent Indentation in inches, or `NULL`.
#' @param colspan Integer. Number of columns the cell spans (horizontal merge).
#'   `NULL` = no span.
#' @param rowspan Integer. Number of rows the cell spans (vertical merge).
#'   `NULL` = no span.
#'
#' @return An `fr_cell_style` object for use in [fr_styles()].
#'
#' @section Alignment model:
#' tlframe separates alignment into two orthogonal axes, following the
#' same model as RTF and LaTeX/tabularray:
#'
#' | Parameter | Axis       | Controls                          | Values                               |
#' |-----------|------------|-----------------------------------|--------------------------------------|
#' | `align`   | Horizontal | Left-to-right text positioning    | `"left"`, `"center"`, `"right"`, `"decimal"` |
#' | `valign`  | Vertical   | Top-to-bottom content positioning | `"top"`, `"middle"`, `"bottom"`      |
#'
#' **`align`** (horizontal) is a **paragraph-level** property — it controls how
#' each line of text is positioned within the cell width. Most cells need this.
#'
#' **`valign`** (vertical) is a **cell-level** property — it controls where the
#' content block sits when the cell is taller than needed. This matters only
#' when rows have unequal content height (e.g., a single-line label next to a
#' multi-line label). For single-line body rows, `valign` has no visible
#' effect.
#'
#' They combine freely: a cell can be `align = "right", valign = "bottom"`
#' (text right-aligned, sitting at the bottom of a tall row).
#'
#' @examples
#' ## ── Bold the entire column header ────────────────────────────────────────
#'
#' fr_style(region = "header", bold = TRUE)
#'
#' ## ── Red text for specific body rows ──────────────────────────────────────
#'
#' fr_style(region = "body", rows = c(1L, 3L), fg = "#CC0000")
#'
#' ## ── Blue background on a specific column ──────────────────────────────────
#'
#' fr_style(cols = "total", bg = "#E8F4FD")
#'
#' ## ── Highlight the Total column header ────────────────────────────────────
#'
#' fr_style(region = "header", cols = "total", bg = "#D0E4FF", bold = TRUE)
#'
#' ## ── Stub column: bold row labels ─────────────────────────────────────────
#'
#' fr_style(region = "stub", bold = TRUE)
#'
#' ## ── Multi-stub layout (e.g. multiple row-label columns) ──────────────────
#'
#' # The "stub" region targets exactly one column (the first dataset column).
#' # When your table has multiple row-label columns (like System Organ Class
#' # followed by Preferred Term), target them by name using `region = "body"`:
#' fr_style(region = "body", cols = c("soc", "pt"), align = "left", bold = TRUE)
#'
#' ## ── Merged spanning title cell ────────────────────────────────────────────
#'
#' fr_style(region = "header", rows = 1L, cols = "characteristic",
#'          colspan = 5L, bold = TRUE, bg = "#F0F0F0")
#'
#' @seealso [fr_row_style()] for row-level styling, [fr_col_style()] for
#'   column-level styling, [fr_styles()] to apply styles to a spec.
#'
#' @export
fr_style <- function(region = "body", rows = NULL, cols = NULL,
                     bold = NULL, italic = NULL, underline = NULL,
                     fg = NULL, bg = NULL, font_size = NULL,
                     align = NULL, valign = NULL, indent = NULL,
                     colspan = NULL, rowspan = NULL) {
  call <- caller_env()
  region <- match_arg_fr(region, c("body", "header", "stub"), call = call)

  if (!is.null(align))  align  <- match_arg_fr(align,  fr_env$valid_aligns,  call = call)
  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)

  new_fr_cell_style(
    type      = "cell",
    region    = region,
    rows      = rows,
    cols      = cols,
    bold      = bold,
    italic    = italic,
    underline = underline,
    fg        = fg,
    bg        = bg,
    font_size = font_size,
    align     = align,
    valign    = valign,
    indent    = indent,
    colspan   = colspan,
    rowspan   = rowspan
  )
}


#' Define a Row Style Override
#'
#' @description
#'
#' Creates a row-level style object for use in [fr_styles()]. Row styles apply
#' uniformly across all cells in the targeted rows.
#'
#' @param rows Integer vector of body row positions, `"all"`, or `NULL` (all
#'   body rows). Row indices are 1-based. Multiple indices are supported:
#'   `rows = c(1L, 3L, 5L)`.
#' @param bold,italic,underline Logical or `NULL` to inherit.
#' @param fg Foreground (text) colour, or `NULL`.
#' @param bg Background (fill) colour, or `NULL`.
#' @param font_size Font size in points, or `NULL`.
#' @param align Horizontal alignment: `"left"`, `"center"`, `"right"`,
#'   `"decimal"`, or `NULL`. See [fr_style()] **Alignment model**.
#' @param valign Vertical alignment: `"top"`, `"middle"`, `"bottom"`, or
#'   `NULL`. See [fr_style()] **Alignment model**.
#' @param height Row height in inches, or `NULL` (auto).
#'
#' @return An `fr_cell_style` object with `type = "row"` for use in
#'   [fr_styles()].
#'
#' @section Style precedence:
#' When multiple styles target the same cell, narrower scopes win:
#' ```
#' fr_col_style  <  fr_row_style  <  fr_style (cell)
#' ```
#' Within the same scope level, later styles (later in the `fr_styles()`
#' call or in later `fr_styles()` calls) override earlier ones.
#' `fr_row_style()` overrides `fr_col_style()` for the same property
#' because row styles are narrower than column styles.
#'
#' @section Tips:
#' * Use `fr_row_style(rows = "all", bg = "#F5F5F5")` for a subtle
#'   background on all body rows (zebra striping requires alternating calls:
#'   rows of odd/even index).
#' * Row height in regulatory tables is normally controlled by `fr_page(font_size = ...)`
#'   and the layout engine. Set `height` only when you need a specific row
#'   to be taller (e.g. a summary row).
#' * Use [fr_rows_matches()] instead of hard-coded row indices for
#'   content-based row targeting (e.g. bold every "Total" row).
#'
#' @examples
#' ## ── Bold the first body row (e.g. total row) ─────────────────────────────
#'
#' fr_row_style(rows = 1L, bold = TRUE)
#'
#' ## ── Light grey background on all body rows ────────────────────────────────
#'
#' fr_row_style(rows = "all", bg = "#F5F5F5")
#'
#' ## ── Highlight multiple specific rows ──────────────────────────────────────
#'
#' # Pass a vector of row indices to style multiple disparate rows at once:
#' fr_row_style(rows = c(2L, 4L, 6L), bg = "#E8F4FD")
#'
#' ## ── Highlight last row (totals) in a different colour ─────────────────────
#'
#' fr_row_style(rows = nrow(tbl_demog), bg = "#FFF3CD", bold = TRUE)
#'
#' ## ── Increase height of header-adjacent row ────────────────────────────────
#'
#' fr_row_style(rows = 1L, height = 0.3)
#'
#' ## ── Full pipeline with row styles ─────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_styles(
#'     fr_row_style(rows = "all", bg = "#FAFAFA"),
#'     fr_style(region = "header", bold = TRUE, bg = "#E0E0E0")
#'   )
#'
#' @seealso [fr_col_style()] for column-level styling, [fr_style()] for
#'   cell-level styling, [fr_styles()] to apply to a spec.
#'
#' @export
fr_row_style <- function(rows = NULL, bold = NULL, italic = NULL,
                          underline = NULL, fg = NULL, bg = NULL,
                          font_size = NULL, align = NULL, valign = NULL,
                          height = NULL) {
  call <- caller_env()
  if (!is.null(align))  align  <- match_arg_fr(align,  fr_env$valid_aligns,  call = call)
  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)
  if (!is.null(height))    check_positive_num(height, arg = "height", call = call)

  new_fr_row_style(
    rows      = rows,
    bold      = bold,
    italic    = italic,
    underline = underline,
    fg        = fg,
    bg        = bg,
    font_size = font_size,
    align     = align,
    valign    = valign,
    height    = height
  )
}


#' Define a Column Style Override
#'
#' @description
#'
#' Creates a column-level style object for use in [fr_styles()]. Column styles
#' apply uniformly to all body cells in the targeted columns.
#'
#' @param cols Character vector of column names, or `NULL` (all columns).
#' @param bold,italic,underline Logical or `NULL` to inherit.
#' @param fg Foreground (text) colour, or `NULL`.
#' @param bg Background (fill) colour, or `NULL`.
#' @param font_size Font size in points, or `NULL`.
#' @param align Horizontal alignment: `"left"`, `"center"`, `"right"`,
#'   `"decimal"`, or `NULL`. See [fr_style()] **Alignment model**.
#' @param valign Vertical alignment: `"top"`, `"middle"`, `"bottom"`, or
#'   `NULL`. See [fr_style()] **Alignment model**.
#'
#' @return An `fr_cell_style` object with `type = "col"` for use in
#'   [fr_styles()].
#'
#' @section Style precedence:
#' Column styles are the **broadest** scope and are overridden by both
#' row styles and cell styles:
#' ```
#' fr_col_style  <  fr_row_style  <  fr_style (cell)
#' ```
#' Column **alignment** set via `fr_col(align = ...)` in [fr_cols()]
#' is the base default. `fr_col_style(align = ...)` overrides it for
#' body cells. `fr_row_style(align = ...)` overrides both. Finally,
#' `fr_style(region = "body", align = ...)` targeting specific cells wins.
#'
#' @section Tips:
#' * Column alignment is usually set in [fr_cols()] via `fr_col(align = ...)`.
#'   Use `fr_col_style()` only when you need to override alignment for
#'   a subset of columns without reconfiguring the full column spec.
#' * Use `fr_col_style(cols = "total", bg = "#EBF5FB")` to give the Total
#'   column a distinct background — a common regulatory convention.
#' * Column styles apply only to **body** cells. To style header cells,
#'   use `fr_style(region = "header", cols = ...)` or [fr_header()].
#'
#' @examples
#' ## ── Total column with blue tint ──────────────────────────────────────────
#'
#' fr_col_style(cols = "total", bg = "#EBF5FB")
#'
#' ## ── Bold the row-label stub column ───────────────────────────────────────
#'
#' fr_col_style(cols = "characteristic", bold = TRUE)
#'
#' ## ── Right-align all numeric data columns ─────────────────────────────────
#'
#' fr_col_style(cols = c("zom_50mg", "zom_100mg", "placebo", "total"),
#'              align = "right")
#'
#' ## ── Full pipeline with column styles ─────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_vlines("box") |>
#'   fr_styles(
#'     fr_style(region = "header", bold = TRUE),
#'     fr_col_style(cols = "total", bg = "#EBF5FB")
#'   )
#'
#' @seealso [fr_row_style()] for row-level styling, [fr_style()] for
#'   cell-level styling, [fr_styles()] to apply to a spec.
#'
#' @export
fr_col_style <- function(cols = NULL, bold = NULL, italic = NULL,
                          underline = NULL, fg = NULL, bg = NULL,
                          font_size = NULL, align = NULL, valign = NULL) {
  call <- caller_env()
  if (!is.null(align))  align  <- match_arg_fr(align,  fr_env$valid_aligns,  call = call)
  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)

  new_fr_col_style(
    cols      = cols,
    bold      = bold,
    italic    = italic,
    underline = underline,
    fg        = fg,
    bg        = bg,
    font_size = font_size,
    align     = align,
    valign    = valign
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_styles — Apply style overrides to a spec
# ══════════════════════════════════════════════════════════════════════════════

#' Apply Style Overrides to a Table
#'
#' @description
#'
#' Appends one or more style override objects — created by [fr_style()],
#' [fr_row_style()], or [fr_col_style()] — to the table specification.
#' Multiple calls to `fr_styles()` **accumulate**: styles are applied in
#' order, with later styles overriding earlier ones where they target the
#' same cells.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param ... One or more style objects created by [fr_style()],
#'   [fr_row_style()], or [fr_col_style()].
#'
#' @return A modified `fr_spec`. Styles appended to `spec$cell_styles`.
#'
#' @section Style application order:
#' Styles are applied in the order they are passed. The precedence (from
#' lowest to highest) is:
#' 1. **Column styles** (`fr_col_style`) — widest scope, applied first.
#' 2. **Row styles** (`fr_row_style`) — override column styles.
#' 3. **Cell styles** (`fr_style`) — narrowest scope, applied last.
#'
#' Within the same type, later styles override earlier ones for the same
#' cell properties. This means you can set a broad default and then
#' selectively override specific cells.
#'
#' @section Tips:
#' * It is idiomatic to pass all styles in a single `fr_styles()` call,
#'   ordering them from broad to specific.
#' * `fr_styles()` **appends** — call `fr_table()` again to start fresh
#'   and discard accumulated styles.
#' * The number of styles has negligible performance impact — the render
#'   engine applies them in a single pass over the cell grid.
#'
#' @examples
#' ## ── Bold header + highlighted Total column ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_styles(
#'     fr_style(region = "header", bold = TRUE),
#'     fr_col_style(cols = "total", bg = "#EBF5FB")
#'   )
#'
#' ## ── Zebra striping (alternating row shading) ─────────────────────────────
#'
#' n <- nrow(tbl_demog)
#' odd_rows  <- seq(1, n, by = 2)
#' even_rows <- seq(2, n, by = 2)
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_styles(
#'     fr_row_style(rows = odd_rows,  bg = "#FFFFFF"),
#'     fr_row_style(rows = even_rows, bg = "#F5F5F5")
#'   )
#'
#' ## ── Multi-layer: col → row → cell ────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_styles(
#'     # Broad: light background for all data columns
#'     fr_col_style(cols = c("zom_50mg", "zom_100mg", "placebo"), bg = "#FAFAFA"),
#'     # Mid: bold entire header
#'     fr_style(region = "header", bold = TRUE),
#'     # Narrow: red text for high-risk row
#'     fr_style(region = "body", rows = 3L, fg = "#CC0000", bold = TRUE)
#'   )
#'
#' ## ── Stub column bold + header background ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_vlines("box") |>
#'   fr_styles(
#'     fr_style(region = "header", bold = TRUE, bg = "#E8E8E8"),
#'     fr_style(region = "stub",   bold = TRUE)
#'   )
#'
#' ## ── Accumulating styles across multiple calls ─────────────────────────────
#'
#' spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_styles(fr_style(region = "header", bold = TRUE))
#'
#' # Later: add Total column highlight (accumulates, does not replace)
#' spec <- spec |>
#'   fr_styles(fr_col_style(cols = "total", bg = "#EBF5FB"))
#'
#' @seealso [fr_style()], [fr_row_style()], [fr_col_style()] for the style
#'   constructors.
#'
#' @export
fr_styles <- function(spec, ...) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  dots <- list(...)
  if (length(dots) == 0L) return(spec)

  for (i in seq_along(dots)) {
    if (!inherits(dots[[i]], "fr_cell_style")) {
      cli_abort(
        c("Argument {i} passed to {.fn fr_styles} is not a style object.",
          "i" = "Use {.fn fr_style}, {.fn fr_row_style}, or {.fn fr_col_style} to create styles."),
        call = call
      )
    }
  }

  # Eagerly resolve any fr_rows_matches() selectors using the current data
  dots <- lapply(dots, function(style) {
    if (inherits(style$rows, "fr_rows_selector")) {
      style$rows <- resolve_rows_selector(style$rows, spec$data, call = call)
    }
    style
  })

  spec$cell_styles <- c(spec$cell_styles, dots)
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_style_explain — Diagnostic: show styles applied to a cell
# ══════════════════════════════════════════════════════════════════════════════

#' Explain Style Resolution for a Cell
#'
#' @description
#'
#' Diagnostic tool that reports which cell styles affect a specific cell in the
#' table body, the order they are applied, and the final resolved properties.
#' Use this when a cell does not look as expected in the rendered output —
#' it shows exactly which style layers are active and how they combine.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param row Integer scalar. Body row index (1-based).
#' @param col Character scalar or integer. Column name or column index.
#'
#' @return Invisibly returns a list with:
#'   * `$final` — named list of resolved properties (`bold`, `italic`,
#'     `fg`, `bg`, `align`, `valign`, `indent`, `font_size`).
#'   * `$layers` — ordered list of matching styles with their index
#'     in `spec$cell_styles`, type, and overridden properties.
#'
#'   Prints a human-readable summary to the console showing:
#'   1. The cell content value.
#'   2. Each matching style layer with its index, type (col/row/cell),
#'      and properties it sets.
#'   3. The final resolved properties after all layers are applied.
#'
#' @section Interpreting the output:
#' ```
#' -- Style explain: row 1, col "total" --
#' Content: "135"
#' 2 matching styles:
#'   [1] col: bg="#EBF5FB"
#'   [2] row: bold=TRUE
#'
#' Final: bold=TRUE, italic=FALSE, fg=#000000, bg=#EBF5FB, ...
#' ```
#' The `[1]`, `[2]` indices refer to the position in `spec$cell_styles`.
#' Properties from later layers override earlier ones. In this example,
#' the column style sets the background; the row style adds bold.
#'
#' @examples
#' spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_styles(
#'     fr_col_style(cols = "total", bg = "#EBF5FB"),
#'     fr_row_style(rows = 1L, bold = TRUE)
#'   )
#'
#' # See which styles affect row 1, column "total"
#' fr_style_explain(spec, row = 1L, col = "total")
#'
#' # Programmatic access to the resolved properties
#' result <- fr_style_explain(spec, row = 1L, col = "total")
#' result$final$bold   # TRUE (from row style)
#' result$final$bg     # "#EBF5FB" (from col style)
#'
#' @seealso [fr_styles()] to apply styles, [fr_style()] for cell-level
#'   overrides, [fr_row_style()] and [fr_col_style()] for broader styles.
#'
#' @export
fr_style_explain <- function(spec, row, col) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  col_names <- names(spec$data)
  if (is.character(col)) {
    if (!col %in% col_names) {
      cli_abort("Column {.val {col}} not found in data.", call = call)
    }
    col_idx <- match(col, col_names)
    col_name <- col
  } else {
    col_idx <- as.integer(col)
    if (col_idx < 1L || col_idx > length(col_names)) {
      cli_abort("Column index {.val {col_idx}} out of range.", call = call)
    }
    col_name <- col_names[col_idx]
  }

  nr <- nrow(spec$data)
  row <- as.integer(row)
  if (row < 1L || row > nr) {
    cli_abort("Row {.val {row}} out of range (1..{nr}).", call = call)
  }

  # Collect matching styles
  layers <- list()
  final <- list(bold = FALSE, italic = FALSE, underline = FALSE,
                fg = "#000000", bg = NA_character_, indent = 0,
                font_size = spec$page$font_size, align = "left",
                valign = "top")

  # Detect column default alignment
  if (!is.null(spec$columns[[col_name]])) {
    final$align <- spec$columns[[col_name]]$align %||% "left"
  }

  for (i in seq_along(spec$cell_styles)) {
    style <- spec$cell_styles[[i]]
    if (style$region != "body" && style$region != "stub") next

    # Check row match
    row_match <- is.null(style$rows) || identical(style$rows, "all") ||
      (row %in% style$rows)
    if (!row_match) next

    # Check col match
    col_match <- if (style$type == "row" || is.null(style$cols) ||
                     identical(style$cols, "all")) {
      TRUE
    } else if (is.character(style$cols)) {
      col_name %in% style$cols
    } else if (is.numeric(style$cols)) {
      col_idx %in% style$cols
    } else {
      TRUE
    }
    if (!col_match) next

    # This style matches
    layer <- list(index = i, type = style$type, region = style$region)
    props <- character(0)
    for (prop in c("bold", "italic", "underline", "fg", "bg",
                   "font_size", "align", "valign", "indent")) {
      val <- style[[prop]]
      if (!is.null(val)) {
        final[[prop]] <- val
        layer[[prop]] <- val
        props <- c(props, paste0(prop, "=", deparse(val, width.cutoff = 40L)))
      }
    }
    layer$summary <- paste0(props, collapse = ", ")
    layers <- c(layers, list(layer))
  }

  # Print summary
  cli::cli_h3("Style explain: row {row}, col {.val {col_name}}")
  content <- spec$data[[col_name]][row]
  cli::cli_text("Content: {.val {content}}")

  if (length(layers) == 0L) {
    cli::cli_text("No cell_styles match this cell.")
  } else {
    cli::cli_text("{length(layers)} matching style{?s}:")
    for (l in layers) {
      cli::cli_text("  [{l$index}] {l$type}: {l$summary}")
    }
  }

  cli::cli_text("")
  cli::cli_text("Final: bold={final$bold}, italic={final$italic}, fg={final$fg}, bg={final$bg}, align={final$align}, valign={final$valign}, indent={final$indent}")

  invisible(list(final = final, layers = layers))
}
