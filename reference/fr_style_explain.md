# Explain Style Resolution for a Cell

Diagnostic tool that reports which cell styles affect a specific cell in
the table body, the order they are applied, and the final resolved
properties. Use this when a cell does not look as expected in the
rendered output — it shows exactly which style layers are active and how
they combine.

## Usage

``` r
fr_style_explain(spec, row, col)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md).

- row:

  Integer scalar. Body row index (1-based).

- col:

  Character scalar or integer. Column name or column index.

## Value

Invisibly returns a list with:

- `$final` — named list of resolved properties (`bold`, `italic`, `fg`,
  `bg`, `align`, `valign`, `indent`, `font_size`).

- `$layers` — ordered list of matching styles with their index in
  `spec$cell_styles`, type, and overridden properties.

Prints a human-readable summary to the console showing:

1.  The cell content value.

2.  Each matching style layer with its index, type (col/row/cell), and
    properties it sets.

3.  The final resolved properties after all layers are applied.

## Interpreting the output

    -- Style explain: row 1, col "total" --
    Content: "135"
    2 matching styles:
      [1] col: bg="#EBF5FB"
      [2] row: bold=TRUE

    Final: bold=TRUE, italic=FALSE, fg=#000000, bg=#EBF5FB, ...

The `[1]`, `[2]` indices refer to the position in `spec$cell_styles`.
Properties from later layers override earlier ones. In this example, the
column style sets the background; the row style adds bold.

## See also

[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
to apply styles,
[`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md)
for cell-level overrides,
[`fr_row_style()`](https://vthanik.github.io/tlframe/reference/fr_row_style.md)
and
[`fr_col_style()`](https://vthanik.github.io/tlframe/reference/fr_col_style.md)
for broader styles.

## Examples

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_col_style(cols = "total", bg = "#EBF5FB"),
    fr_row_style(rows = 1L, bold = TRUE)
  )

# See which styles affect row 1, column "total"
fr_style_explain(spec, row = 1L, col = "total")
#> 
#> ── Style explain: row 1, col "total" 
#> Content: "135"
#> 2 matching styles:
#> [1] col: bg="#EBF5FB"
#> [2] row: bold=TRUE
#> 
#> Final: bold=TRUE, italic=FALSE, fg=#000000, bg=#EBF5FB, align=left, valign=top,
#> indent=0

# Programmatic access to the resolved properties
result <- fr_style_explain(spec, row = 1L, col = "total")
#> 
#> ── Style explain: row 1, col "total" 
#> Content: "135"
#> 2 matching styles:
#> [1] col: bg="#EBF5FB"
#> [2] row: bold=TRUE
#> 
#> Final: bold=TRUE, italic=FALSE, fg=#000000, bg=#EBF5FB, align=left, valign=top,
#> indent=0
result$final$bold   # TRUE (from row style)
#> [1] TRUE
result$final$bg     # "#EBF5FB" (from col style)
#> [1] "#EBF5FB"

## ── Multiple overlapping styles: see precedence in action ─────────────────

spec2 <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_col_style(cols = "total", bg = "#EBF5FB"),
    fr_row_style(rows = 1L, bg = "#FFF3CD", bold = TRUE),
    fr_style(region = "body", rows = 1L, cols = "total",
             fg = "#CC0000", italic = TRUE)
  )

# Cell (1, "total") has three overlapping layers — cell wins for fg/italic,
# row wins for bg/bold (narrower scope), col style is overridden:
fr_style_explain(spec2, row = 1L, col = "total")
#> 
#> ── Style explain: row 1, col "total" 
#> Content: "135"
#> 3 matching styles:
#> [1] col: bg="#EBF5FB"
#> [2] row: bold=TRUE, bg="#FFF3CD"
#> [3] cell: italic=TRUE, fg="#CC0000"
#> 
#> Final: bold=TRUE, italic=TRUE, fg=#CC0000, bg=#FFF3CD, align=left, valign=top,
#> indent=0

## ── Inspect a header cell ─────────────────────────────────────────────────

spec3 <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style(region = "header", bold = TRUE, bg = "#E0E0E0"),
    fr_style(region = "header", cols = "total", bg = "#D0E4FF")
  )

# Note: fr_style_explain inspects body cells. For header-region styles,
# review spec3$cell_styles directly:
str(spec3$cell_styles)
#> List of 2
#>  $ :List of 17
#>   ..$ type     : chr "cell"
#>   ..$ region   : chr "header"
#>   ..$ rows     : NULL
#>   ..$ cols     : NULL
#>   ..$ bold     : logi TRUE
#>   ..$ italic   : NULL
#>   ..$ underline: NULL
#>   ..$ fg       : NULL
#>   ..$ bg       : chr "#E0E0E0"
#>   ..$ font     : NULL
#>   ..$ font_size: NULL
#>   ..$ align    : NULL
#>   ..$ valign   : NULL
#>   ..$ indent   : NULL
#>   ..$ colspan  : NULL
#>   ..$ rowspan  : NULL
#>   ..$ height   : NULL
#>   ..- attr(*, "class")= chr "fr_cell_style"
#>  $ :List of 17
#>   ..$ type     : chr "cell"
#>   ..$ region   : chr "header"
#>   ..$ rows     : NULL
#>   ..$ cols     : chr "total"
#>   ..$ bold     : NULL
#>   ..$ italic   : NULL
#>   ..$ underline: NULL
#>   ..$ fg       : NULL
#>   ..$ bg       : chr "#D0E4FF"
#>   ..$ font     : NULL
#>   ..$ font_size: NULL
#>   ..$ align    : NULL
#>   ..$ valign   : NULL
#>   ..$ indent   : NULL
#>   ..$ colspan  : NULL
#>   ..$ rowspan  : NULL
#>   ..$ height   : NULL
#>   ..- attr(*, "class")= chr "fr_cell_style"
```
