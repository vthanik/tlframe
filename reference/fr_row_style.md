# Define a Row Style Override

Creates a row-level style object for use in
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md).
Row styles apply uniformly across all cells in the targeted rows.

## Usage

``` r
fr_row_style(
  rows = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  fg = NULL,
  bg = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL,
  height = NULL
)
```

## Arguments

- rows:

  Integer vector of body row positions, `"all"`, or `NULL` (all body
  rows). Row indices are 1-based. Multiple indices are supported:
  `rows = c(1L, 3L, 5L)`.

- bold, italic, underline:

  Logical or `NULL` to inherit.

- fg:

  Foreground (text) colour, or `NULL`.

- bg:

  Background (fill) colour, or `NULL`.

- font_size:

  Font size in points, or `NULL`.

- align:

  Horizontal alignment: `"left"`, `"center"`, `"right"`, `"decimal"`, or
  `NULL`. See
  [`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md)
  **Alignment model**.

- valign:

  Vertical alignment: `"top"`, `"middle"`, `"bottom"`, or `NULL`. See
  [`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md)
  **Alignment model**.

- height:

  Row height in inches, or `NULL` (auto).

## Value

An `fr_cell_style` object with `type = "row"` for use in
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md).

## Style precedence

When multiple styles target the same cell, narrower scopes win:

    fr_col_style  <  fr_row_style  <  fr_style (cell)

Within the same scope level, later styles (later in the
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
call or in later
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
calls) override earlier ones. `fr_row_style()` overrides
[`fr_col_style()`](https://vthanik.github.io/tlframe/reference/fr_col_style.md)
for the same property because row styles are narrower than column
styles.

## Tips

- Use `fr_row_style(rows = "all", bg = "#F5F5F5")` for a subtle
  background on all body rows (zebra striping requires alternating
  calls: rows of odd/even index).

- Row height in regulatory tables is normally controlled by
  `fr_page(font_size = ...)` and the layout engine. Set `height` only
  when you need a specific row to be taller (e.g. a summary row).

- Use
  [`fr_rows_matches()`](https://vthanik.github.io/tlframe/reference/fr_rows_matches.md)
  instead of hard-coded row indices for content-based row targeting
  (e.g. bold every "Total" row).

## See also

[`fr_col_style()`](https://vthanik.github.io/tlframe/reference/fr_col_style.md)
for column-level styling,
[`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md)
for cell-level styling,
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
to apply to a spec.

## Examples

``` r
## ── Standalone: store and reuse ─────────────────────────────────────────
bold_first <- fr_row_style(rows = 1L, bold = TRUE)
bold_first  # inspect
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Bold the first body row (e.g. total row) ─────────────────────────────

fr_row_style(rows = 1L, bold = TRUE)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Light grey background on all body rows ────────────────────────────────

fr_row_style(rows = "all", bg = "#F5F5F5")
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] "all"
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> [1] "#F5F5F5"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Highlight multiple specific rows ──────────────────────────────────────

# Pass a vector of row indices to style multiple disparate rows at once:
fr_row_style(rows = c(2L, 4L, 6L), bg = "#E8F4FD")
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 2 4 6
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> [1] "#E8F4FD"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Highlight last row (totals) in a different colour ─────────────────────

fr_row_style(rows = nrow(tbl_demog), bg = "#FFF3CD", bold = TRUE)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 28
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> [1] "#FFF3CD"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Increase height of header-adjacent row ────────────────────────────────

fr_row_style(rows = 1L, height = 0.3)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $fg
#> NULL
#> 
#> $bg
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> [1] 0.3
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Full pipeline with row styles ─────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(rows = "all", bg = "#FAFAFA"),
    fr_style(region = "header", bold = TRUE, bg = "#E0E0E0")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Styles: 2 overrides
```
