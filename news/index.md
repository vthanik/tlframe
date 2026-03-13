# Changelog

## tlframe 0.1.0.9000 (development version)

### New features

#### Multi-page figures

- [`fr_figure()`](https://vthanik.github.io/tlframe/reference/fr_figure.md)
  now accepts a list of plot objects and an optional `meta` data frame.
  Each column of `meta` becomes a `{token}` resolved in titles and
  footnotes dynamically per page, enabling figure programmes that loop
  over parameters (e.g. one page per biomarker or visit).

#### Group label injection

- `fr_rows(group_label = )` auto-injects a header row at each group
  boundary using values from a specified column. This is the standard
  demographics pattern where the group variable label appears as a bold
  row above indented summary statistics, without requiring pre-formatted
  data.

#### Multi-level indent_by

- `fr_rows(indent_by = )` now accepts a named list specifying `key`,
  `col`, and `levels` for SOC/HLT/PT three-level hierarchies. Each row
  is indented to its natural depth, replacing the previous single-column
  form that only handled two levels.

#### Pagination controls

- `fr_rows(group_keep = FALSE)` disables RTF `\keepn` for visual-only
  grouping (blank rows and indentation without page-keep constraints).
  Useful when groups are short and the default keep behaviour wastes
  space.
- `fr_rows(page_by_visible = FALSE)` inserts page breaks at `page_by`
  boundaries while hiding the label row itself.
- `fr_page(orphan_min = , widow_min = )` sets minimum body-row counts at
  the bottom and top of a page respectively, preventing isolated rows at
  page breaks.

#### Wrap parameter

- `fr_rows(wrap = )` controls whether long cell content wraps within its
  column or is truncated, complementing the existing `repeat_cols`
  behaviour.

### Bug fixes

- `measure_text_width_twips()`: multi-byte UTF-8 characters (em dashes,
  arrows, etc.) now measured correctly via
  [`utf8ToInt()`](https://rdrr.io/r/base/utf8Conversion.html) instead of
  misindexing the raw-byte lookup table.
- `resolve_borders()`: early return when the table body is empty
  prevents index-out-of-bounds crashes on zero-row tables.
- `escape_and_resolve()`: guard against malformed sentinel tokens with
  fewer than three capture groups (was a subscript-out-of-bounds crash).
- [`fr_rows()`](https://vthanik.github.io/tlframe/reference/fr_rows.md):
  error when `group_label` is set without `group_by`.
- [`fr_rows()`](https://vthanik.github.io/tlframe/reference/fr_rows.md):
  warning when `page_by` and `group_by` share the same column.
- LaTeX hline dedup regex anchored with `^` to prevent row 10 matching
  row 100.
- Decimal alignment: [`round()`](https://rdrr.io/r/base/Round.html)
  replaces [`as.integer()`](https://rdrr.io/r/base/integer.html) for
  center-offset calculation, removing a systematic 1-twip leftward bias.
- `paginate_rows()`: all consecutive trailing blank rows captured, not
  just the first.

### Pipeline API

- [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md),
  [`fr_listing()`](https://vthanik.github.io/tlframe/reference/fr_listing.md),
  [`fr_figure()`](https://vthanik.github.io/tlframe/reference/fr_figure.md)
  entry points for tables, patient listings, and embedded figures.
- 10 pipeline verbs:
  [`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md),
  [`fr_titles()`](https://vthanik.github.io/tlframe/reference/fr_titles.md),
  [`fr_footnotes()`](https://vthanik.github.io/tlframe/reference/fr_footnotes.md),
  [`fr_header()`](https://vthanik.github.io/tlframe/reference/fr_header.md),
  [`fr_rows()`](https://vthanik.github.io/tlframe/reference/fr_rows.md),
  [`fr_hlines()`](https://vthanik.github.io/tlframe/reference/fr_hlines.md),
  [`fr_vlines()`](https://vthanik.github.io/tlframe/reference/fr_vlines.md),
  [`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md),
  [`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md),
  [`fr_page()`](https://vthanik.github.io/tlframe/reference/fr_page.md).
- [`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md)
  produces RTF and PDF from the same spec.
- Verb order is irrelevant – all resolution deferred to render time.

### Column system

- [`fr_col()`](https://vthanik.github.io/tlframe/reference/fr_col.md)
  constructor with width, alignment, visibility, and grouping.
- Width modes: fixed inches, percentages, `"auto"` (AFM font metrics),
  `"fit"`, and `"equal"`.
- Decimal alignment engine (15 stat-display types) for pharma summary
  tables.
- N-count injection via `.n` and `.n_format` in
  [`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md).
- Column splitting (`.split = TRUE`) for wide tables exceeding page
  width.

### Rendering

- Native RTF 1.9.1 backend with field codes for page numbering.
- LaTeX/tabularray backend compiled via XeLaTeX for PDF output.
- R-side pagination with group-aware page breaks.
- Token system (`{thepage}`, `{total_pages}`, `{program}`, `{datetime}`)
  for running headers and footers.
- Latin Modern font fallback for PDF on Linux/Docker without Microsoft
  fonts (built into tinytex/texlive, no bundled fonts needed).
- `TLFRAME_FONT_DIR` environment variable: point to a directory of
  `.ttf`/`.otf` files for project-local fonts without system-wide
  installation. Ideal for Docker/CI pipelines.

### Styling

- [`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md),
  [`fr_row_style()`](https://vthanik.github.io/tlframe/reference/fr_row_style.md),
  [`fr_col_style()`](https://vthanik.github.io/tlframe/reference/fr_col_style.md)
  for cell-level formatting.
- [`fr_style_if()`](https://vthanik.github.io/tlframe/reference/fr_style_if.md)
  for data-driven conditional styles.
- [`fr_rows_matches()`](https://vthanik.github.io/tlframe/reference/fr_rows_matches.md)
  for pattern-based row selectors.
- [`fr_style_explain()`](https://vthanik.github.io/tlframe/reference/fr_style_explain.md)
  for debugging style cascade resolution.
- Inline markup:
  [`fr_super()`](https://vthanik.github.io/tlframe/reference/fr_super.md),
  [`fr_sub()`](https://vthanik.github.io/tlframe/reference/fr_sub.md),
  [`fr_bold()`](https://vthanik.github.io/tlframe/reference/fr_bold.md),
  [`fr_italic()`](https://vthanik.github.io/tlframe/reference/fr_italic.md),
  [`fr_dagger()`](https://vthanik.github.io/tlframe/reference/fr_dagger.md),
  [`fr_emdash()`](https://vthanik.github.io/tlframe/reference/fr_emdash.md),
  and more.

### Configuration

- Four-tier defaults: package \< `_tlframe.yml` \<
  [`fr_theme()`](https://vthanik.github.io/tlframe/reference/fr_theme.md)
  \< per-table verbs.
- [`fr_recipe()`](https://vthanik.github.io/tlframe/reference/fr_recipe.md)
  and
  [`fr_apply()`](https://vthanik.github.io/tlframe/reference/fr_apply.md)
  for reusable pipeline fragments.
- [`c.fr_recipe()`](https://vthanik.github.io/tlframe/reference/c.fr_recipe.md)
  for composing recipes (company + study + table-type layers).

### Validation

- [`fr_validate()`](https://vthanik.github.io/tlframe/reference/fr_validate.md)
  pre-render checks for columns, widths, styles, spans, and fonts.
- `fr_get_*()` accessors for programmatic QC of spec internals.

### Datasets

- Synthetic CDISC ADaM datasets (study TFRM-2024-001, 135 subjects):
  `adsl`, `adae`, `adtte`, `adcm`, `advs`.
- Pre-summarized TFL-ready tables: `tbl_demog`, `tbl_ae_soc`,
  `tbl_ae_summary`, `tbl_disp`, `tbl_tte`, `tbl_cm`, `tbl_vs`.
