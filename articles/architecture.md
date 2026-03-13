# Architecture

This article covers tlframe’s internal architecture for developers who
want to understand the render pipeline, write custom backends, or
contribute.

## Three-layer architecture

    +----------------------------------------------------------------------+
    | API Layer (13 files)                                                 |
    | fr_table -> fr_cols -> fr_header -> fr_titles -> ... -> fr_render    |
    | Each verb: validate -> modify fr_spec -> return fr_spec              |
    +----------------------------------------------------------------------+
    | Infrastructure Layer (9 files)                                       |
    | classes.R     S3 constructors (fr_spec, fr_col, fr_rule, ...)        |
    | columns.R     Width estimation, AFM metrics, auto-distribution       |
    | config.R      YAML discovery, loading, merging                       |
    | constants.R   fr_env: fonts, paper sizes, presets, colors            |
    | helpers.R     Error conditions, text normalisation                   |
    | markup.R      Inline rich text: fr_super() -> sentinel tokens        |
    | tokens.R      {thepage}, {program}, {datetime} resolution            |
    | units.R       Twips, half-points, row height calculation             |
    | validate.R    Type-safe validation helpers                           |
    +----------------------------------------------------------------------+
    | Render Layer (8 files)                                               |
    | render.R          Dispatch: finalize -> prepare_pages -> backend     |
    | render-common.R   Shared: cell grid, borders, escaping, colors       |
    | render-rtf.R      RTF 1.9.1 backend                                  |
    | render-latex.R    tabularray/XeLaTeX backend                         |
    | render-pdf.R      PDF wrapper (generates .tex, compiles twice)       |
    | render-figure.R   Figure rendering (single + multi-page)             |
    | decimal.R         Stat-display decimal alignment engine              |
    | paginate.R        R-side pagination (group-aware page breaks)        |
    +----------------------------------------------------------------------+

## The `fr_spec` object

Every verb reads and writes a single S3 object:

| Field | Type | Set by |
|----|----|----|
| `$data` | data.frame | [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md) |
| `$meta$titles` | list | [`fr_titles()`](https://vthanik.github.io/tlframe/reference/fr_titles.md) |
| `$meta$footnotes` | list | [`fr_footnotes()`](https://vthanik.github.io/tlframe/reference/fr_footnotes.md) |
| `$columns` | named list of `fr_col` | [`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md) |
| `$columns_meta` | list | [`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md) |
| `$header` | list | [`fr_header()`](https://vthanik.github.io/tlframe/reference/fr_header.md) |
| `$body` | list | [`fr_rows()`](https://vthanik.github.io/tlframe/reference/fr_rows.md) |
| `$rules` | list of `fr_rule` | [`fr_hlines()`](https://vthanik.github.io/tlframe/reference/fr_hlines.md), [`fr_vlines()`](https://vthanik.github.io/tlframe/reference/fr_vlines.md) |
| `$cell_styles` | list | [`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md) |
| `$page` | list | [`fr_page()`](https://vthanik.github.io/tlframe/reference/fr_page.md) |
| `$pagehead` | list | [`fr_pagehead()`](https://vthanik.github.io/tlframe/reference/fr_pagehead.md) |
| `$pagefoot` | list | [`fr_pagefoot()`](https://vthanik.github.io/tlframe/reference/fr_pagefoot.md) |
| `$spacing` | list | [`fr_spacing()`](https://vthanik.github.io/tlframe/reference/fr_spacing.md) |
| `$decimal_geometry` | list or NULL | `finalize_spec()` |
| `$plots` | list or NULL | [`fr_figure()`](https://vthanik.github.io/tlframe/reference/fr_figure.md) (multi-page) |
| `$figure_meta` | data.frame or NULL | [`fr_figure()`](https://vthanik.github.io/tlframe/reference/fr_figure.md) (per-page tokens) |

## The render pipeline

    fr_spec
      |
      v
    finalize_spec()            <- Resolve all deferred computations
      |  - Build default columns for unmentioned columns
      |  - Distribute widths (auto/fit/equal/percent)
      |  - Resolve N-count header labels
      |  - Inject group_label header rows (if set)
      |  - Insert blank_after rows
      |  - Apply indent_by offsets (single or multi-level)
      |  - Compute decimal geometry
      v
    prepare_pages()            <- Split data into page groups
      |  - Split by page_by values (preserving data order)
      |  - Each group: {label, data, n_counts}
      v
    calculate_col_panels()     <- Handle wide tables
      |  - If .split=TRUE and width > page: split into panels
      |  - Stub columns repeat in each panel
      v
    render_<format>()          <- Backend-specific rendering
         RTF:   render_rtf(spec, page_groups, col_panels, path)
         LaTeX: render_latex(spec, page_groups, col_panels, path)
         PDF:   render_pdf() -> render_latex() + xelatex x2

### `finalize_spec()`

Key operations:

- **Width distribution**: `"auto"` measures content with AFM font
  metrics. If total exceeds printable area, columns scale
  proportionally.
- **N-count labels**: `.n` + `.n_format` on
  [`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
  resolve into final column labels.
- **Group label injection**: `group_label` inserts header rows at group
  boundaries, placing the group value in the target column. Style
  indices are remapped to account for injected rows.
- **Blank rows**: `blank_after` inserts empty rows at group boundaries.
- **Indent**: `indent_by` applies indentation — either single-level
  (character vector) or multi-level (named list with key/col/levels).
- **Decimal geometry**: 15-type stat display engine analyses cell
  values, computes space-padded alignment offsets.

## Writing a custom backend

Any backend must implement one function:

``` r
render_<format>(spec, page_groups, col_panels, path)
```

| Argument      | Type                 | Description                          |
|---------------|----------------------|--------------------------------------|
| `spec`        | Finalised `fr_spec`  | All deferred computations resolved   |
| `page_groups` | List of lists        | Each: `$label`, `$data`, `$n_counts` |
| `col_panels`  | List of char vectors | Column names per panel               |
| `path`        | Character            | Output file path                     |

Register it:

``` r
render_html <- function(spec, page_groups, col_panels, path) {
  # ... generate HTML ...
  writeLines(html, path)
  path
}
fr_register_backend("html", extensions = "html", render_fn = render_html)

# Now this works:
spec |> fr_render("output.html")
```

### Shared utilities

`render-common.R` provides backend-agnostic helpers:

- **`build_cell_grid()`** — data frame to rendered cell matrix
- **`build_header_cell_grid()`** — header row with resolved N-counts
- **`resolve_borders()`** — rules list to per-cell border spec
- **`collect_colors()`** — scan styles for color table
- **`rtf_escape_and_resolve()` / `latex_escape_and_resolve()`** — escape
  special chars and resolve markup sentinels

## The sentinel system

Inline markup survives string operations via sentinel tokens:

    User writes:       "P{fr_super('a')}-value < 0.05"
                              |
    glue evaluation:   "P\x01SUPER:a\x02-value < 0.05"
                              |
    RTF backend:       "P{\super a}-value < 0.05"
    LaTeX backend:     "P\textsuperscript{a}-value < 0.05"

Sentinel types: `SUPER`, `SUB`, `BOLD`, `ITALIC`, `UNDERLINE`,
`NEWLINE`, `UNICODE`.

## AFM font metrics

Pre-parsed Adobe Font Metrics for 12 font variants in `R/sysdata.rda`:

| Family    | Variants                            |
|-----------|-------------------------------------|
| Helvetica | Regular, Bold, Oblique, BoldOblique |
| Times     | Roman, Bold, Italic, BoldItalic     |
| Courier   | Regular, Bold, Oblique, BoldOblique |

Helvetica ‘M’ = 833 units vs ‘i’ = 222 units (3.75x). Courier is fixed
at 600 units. `estimate_col_width()` uses these for accurate
auto-widths.

## Available backends

``` r
fr_backends()
#>   format extensions               description
#> 1    rtf   rtf, doc          Rich Text Format
#> 2  latex        tex LaTeX source (tabularray)
#> 3    pdf        pdf           PDF via XeLaTeX
```
