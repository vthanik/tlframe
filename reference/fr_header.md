# Configure Column Header Presentation

The single owner of all header **presentation** settings: alignment,
vertical alignment, bold, background/foreground colour, and font size.
These defaults apply to **all** column header cells unless overridden by
per-column `fr_col(header_align = ...)` or
`fr_style(region = "header", ...)`.

[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
owns column **structure** (labels, widths, body alignment, visibility,
N-counts, spanning groups); `fr_header()` owns header **presentation**
only. N-count labels and format are set exclusively via
[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
`.n` / `.n_format` and
[`fr_col()`](https://vthanik.github.io/tlframe/reference/fr_col.md) `n`
parameters.

Calling `fr_header()` again **replaces** the previous header config
(except spans, which are managed by
[`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md)).

## Usage

``` r
fr_header(
  spec,
  align = NULL,
  valign = NULL,
  bold = NULL,
  bg = NULL,
  fg = NULL,
  font_size = NULL,
  repeat_on_page = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md).

- align:

  Horizontal alignment for header cells. Accepts two forms:

  - **Scalar string** — `"left"`, `"center"`, `"right"`, or `NULL`
    (inherit from column). Applied to all header cells uniformly.

  - **Named list + tidyselect** — per-column alignment using column
    names and tidyselect helpers. Names are alignment values, values are
    column selections:

        fr_header(spec, align = list(
          center = c(starts_with("zom"), "placebo"),
          left   = "characteristic",
          right  = "total"
        ))

    Unmatched columns keep `fr_col(header_align = ...)` or fall back to
    the body `align` setting.

  **Precedence** (highest wins):

  1.  `fr_col(header_align = "right")` — per-column override

  2.  `fr_header(align = list(...))` — tidyselect targeting

  3.  `fr_header(align = "center")` — blanket scalar

  4.  Column body `align` — inherited default

- valign:

  Default vertical alignment. One of `"top"`, `"middle"`, `"bottom"`
  (default). Relevant when header rows have unequal height.

- bold:

  Logical or `NULL`. Whether header cells are bold. Default `NULL`
  inherits the built-in default (FALSE). Set `TRUE` explicitly if you
  want bold headers.

- bg:

  Background colour for header cells: hex string (`"#003366"`) or CSS
  named colour (`"steelblue"`, `"lightgray"`, etc.). `NULL` inherits.

- fg:

  Foreground (text) colour for header cells: hex string or CSS named
  colour. `NULL` inherits.

- font_size:

  Font size in points for header cells. `NULL` inherits from page font
  size.

- repeat_on_page:

  Logical. Whether to repeat the column header on every page. Default
  `TRUE` (standard for regulatory tables).

## Value

A modified `fr_spec`. Header config stored in `spec$header`.

## Priority chain for header alignment

    config header.align < fr_header(align) < fr_col(header_align) < fr_style(region="header", align)

## Regulatory conventions

Many pharma TFL outputs use **bold, centered** column headers with
`valign = "bottom"` so that short labels (e.g. "Characteristic") align
at the bottom when adjacent columns have multi-line labels (e.g.
`"Placebo\\n(N=45)"`). By default, headers are **not bold** and inherit
alignment from the column's `align` setting — use
`fr_header(bold = TRUE, align = "center")` to opt in.

## See also

[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
for column structure and N-counts,
[`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md)
for spanning headers,
[`fr_col()`](https://vthanik.github.io/tlframe/reference/fr_col.md) for
per-column `header_align` overrides,
[`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md)
with `region = "header"` for cell-level overrides,
[`fr_config()`](https://vthanik.github.io/tlframe/reference/fr_config.md)
for setting header defaults via `_tlframe.yml`.

## Examples

``` r
## ── Center all headers with bottom valign ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_header(align = "center", valign = "bottom", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: bold, valign=bottom, align=center

## ── Tidyselect alignment (per-column targeting) ──────────────────────────

tbl_demog |>
  fr_table() |>
  fr_header(bold = TRUE, align = list(
    left   = "characteristic",
    center = c(starts_with("zom"), "placebo", "total")
  ))
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: bold, valign=bottom, align_map

## ── Header background colour (hex or CSS named colour) ──────────────────

# Hex colour
tbl_demog |>
  fr_table() |>
  fr_header(bg = "#E0E0E0", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: bold, valign=bottom

# CSS named colour (148 available: lavender, aliceblue, gainsboro, etc.)
tbl_demog |>
  fr_table() |>
  fr_header(bg = "lavender", fg = "midnightblue", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: bold, valign=bottom

## ── N-counts now on fr_cols(), not fr_header() ────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("Zomerane 50 mg", n = 45),
    placebo        = fr_col("Placebo",         n = 45),
    .n_format = "{label}\n(N={n})"
  ) |>
  fr_header(bold = TRUE, align = "center")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo" 0.97in left
#> zom_50mg "Zomerane 50 mg" 1.20in left
#> zom_100mg "zom_100mg" 0.97in left
#> total "total" 0.97in left
#> group "group" 0.90in left
#> Header: bold, valign=bottom, align=center
```
