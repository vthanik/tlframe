# Configure Row Grouping and Pagination

Controls how data rows are grouped, paginated, indented, and spaced.
Calling `fr_rows()` again **merges** with the previous row
configuration: only the arguments you explicitly supply are changed.

tlframe uses a two-level greedy pagination algorithm (identical for RTF
and PDF output):

- **`page_by`** forces a page break whenever the named column(s) change
  value.

- **`group_by`** keeps rows with the same value in the named column(s)
  on the same page via `\trkeep` (RTF). If a group is too large for a
  single page, it breaks normally and a "(continued)" header row repeats
  at the top of the next page.

## Usage

``` r
fr_rows(
  spec,
  page_by = NULL,
  group_by = NULL,
  indent_by = NULL,
  blank_after = NULL,
  page_by_bold = FALSE,
  page_by_align = "left",
  page_by_visible = TRUE,
  group_label = NULL,
  group_keep = TRUE,
  sort_by = NULL,
  repeat_cols = NULL,
  wrap = FALSE
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md).

- page_by:

  Character vector of column name(s). A new page begins whenever any of
  these columns change value. The column value is rendered as a group
  label above the column headers, and the column is **automatically
  hidden** from the table body at render time. Use for multi-SOC tables
  that break by System Organ Class.

- group_by:

  Character vector of column name(s). Rows sharing the same value are
  visually grouped (indented detail rows, blank-after spacing) **and**
  kept together on the same page when possible. When a group spans
  pages, a "(continued)" header row repeats at the top of the next page.

- indent_by:

  Row indentation specification. Accepts two forms:

  **Simple (single level)**: A character vector of column name(s). All
  detail rows (non-header, non-blank) in these columns receive one
  indent level. Typically used with `group_by` for SOC/PT tables:
  `indent_by = "pt"`.

  **Multi-level (SAS-style)**: A named list with three elements:

  - `key` — column name containing row type markers (e.g., `"row_type"`)

  - `col` — column name(s) to apply indent to (e.g., `"term"`)

  - `levels` — named numeric vector mapping key values to indent
    multipliers (e.g., `c(soc = 0, hlt = 1, pt = 2)`)

  Each indent level = 2 space-character widths (~0.17 in at 9pt). Rows
  whose key value is not in `levels` receive no indent.

- blank_after:

  Character vector of column name(s). A blank row is inserted after each
  group boundary in these columns.

- page_by_bold:

  Logical. Whether the page-by label is rendered in bold. Default
  `FALSE`. Set `TRUE` to make the label stand out above the column
  headers.

- page_by_align:

  Horizontal alignment for the page-by label. One of `"left"` (default),
  `"center"`, `"right"`, or `"decimal"`.

- page_by_visible:

  Logical. Whether the page-by group label is displayed above the column
  headers. Default `TRUE`. Set `FALSE` to get page breaks at group
  boundaries without a visible label — useful when the grouping column
  already appears in the table body.

- group_label:

  Character scalar. Column name into which group header values are
  injected. When `group_by` and `group_label` are both set, a header row
  is inserted at the start of each group: the group value appears in the
  `group_label` column, all other columns are empty, and detail rows are
  indented underneath. Style the header rows via
  [`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
  (e.g., bold). Requires `group_by`.

- group_keep:

  Logical. Whether `group_by` groups are kept together on the same page
  via RTF `\keepn` / LaTeX keep-with-next. Default `TRUE`. Set `FALSE`
  for visual-only grouping (blank_after, indent) without page-keeping —
  useful for long groups where you want the renderer to break freely.

- sort_by:

  Character vector of column name(s). Sorts the data by these columns
  before rendering. For listings, this controls the display order (e.g.,
  `sort_by = c("USUBJID", "ASTDT")`). Sorting is applied in
  `finalize_spec()`.

- repeat_cols:

  Character vector of column name(s). Suppresses repeated consecutive
  values in these columns — only the first occurrence in each run is
  displayed. Standard for listings where subject ID appears once per
  block. Suppression is applied in `finalize_spec()`.

- wrap:

  Logical. When `TRUE`, enables text wrapping in body cells. Default
  `FALSE`. For listings with long text fields (e.g., verbatim terms,
  medical history), wrapping prevents cell content from overflowing.

## Value

A modified `fr_spec`. Row config stored in `spec$body`.

## Regulatory conventions — AE tables

Standard pharma AE tables (MedDRA SOC/PT hierarchy) follow these row
conventions:

- **Sort order**: SOC rows sorted by **descending incidence** across all
  arms; within each SOC, PT rows sorted by descending incidence
  (alphabetical as tiebreaker). This is the standard regulatory sort
  order.

- **"TOTAL SUBJECTS WITH AN EVENT"** is always the first summary row,
  above the first SOC. Pre-compute this in your data frame.

- **Subject counting**: a subject is counted once per SOC and once per
  PT, even with multiple events. Document this in
  [`fr_footnotes()`](https://vthanik.github.io/tlframe/reference/fr_footnotes.md).

- **Page breaks by SOC**: each System Organ Class starts a new page
  (`page_by = "soc"`). PT rows stay with their SOC header via
  `group_by`.

- **Indentation**: PT rows are indented under the SOC header
  (`indent_by = "pt_label"`). Indent: 0.1667 in (2 spaces / 240 twips).

## Regulatory conventions — descriptive statistics tables

For demographics and continuous endpoint tables, the standard summary
statistic row order is: `n, Mean, SD (or SE), Median, Q1/Q3, Min/Max`
Pre-sort your long-form data to match this order. Use `blank_after` to
insert visual separation between characteristic blocks (e.g. between Age
and Sex sections).

## Orphan and widow control

The pagination engine respects the `orphan_min` and `widow_min` settings
in
[`fr_page()`](https://vthanik.github.io/tlframe/reference/fr_page.md).
Orphan control prevents a lone row being stranded at the bottom of a
page; widow control prevents a lone row starting a new page. These apply
within `group_by` groups: if placing a group would leave fewer than
`orphan_min` rows before the page break, the group is moved to the next
page instead.

## Tips

- `page_by` columns are **automatically hidden** at render time — their
  values appear as group labels in the section header, so they don't
  need to appear in the table body. Override with
  `fr_cols(col = fr_col(visible = TRUE))` if needed (set *after*
  `fr_rows()`).

- `group_by`, `indent_by`, etc. columns are structural — hide them via
  `fr_cols(col = fr_col(visible = FALSE))` if they shouldn't appear.

- Use `group_by` (not `page_by`) when you want rows to travel together
  but not necessarily start on a new page.

- `blank_after` is the simplest way to add visual group separation
  without bold headers.

## See also

[`fr_page()`](https://vthanik.github.io/tlframe/reference/fr_page.md) to
set `orphan_min` / `widow_min`,
[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md) to
hide structural columns from display.

## Examples

``` r
## ── AE table paginated by System Organ Class ─────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_rows(page_by = "soc")
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: page_by=soc

## ── Demographics table: blank row after each group ────────────────────────

tbl_demog |>
  fr_table() |>
  fr_rows(blank_after = "group")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom

## ── Indented hierarchy (SOC bold header + indented PT rows) ──────────────

tbl_ae_soc |>
  fr_table() |>
  fr_rows(
    group_by  = "soc",
    indent_by = "pt"
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: group_by=soc, indent_by=pt

## ── Combined: page_by + group_by + indent ──────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_cols(soc = fr_col(visible = FALSE),
          row_type = fr_col(visible = FALSE)) |>
  fr_rows(
    page_by   = "soc",
    group_by  = "soc",
    indent_by = "pt"
  )
#> Warning: `page_by` and `group_by` share column: "soc".
#> ℹ `group_by` grouping is applied within each `page_by` page.
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Courier New
#> Columns (5 visible of 7):
#> pt "pt" 4.05in left
#> placebo "placebo" 0.82in left
#> zom_50mg "zom_50mg" 0.82in left
#> zom_100mg "zom_100mg" 0.90in left
#> total "total" 0.90in left
#> Header: valign=bottom
#> Rows: page_by=soc, group_by=soc, indent_by=pt

## ── Multi-level indent (SOC / HLT / PT hierarchy) ─────────────────────
## Uses a named list: key column determines indent level per row

# Given data with columns: soc, term, row_type, placebo, ...
# where row_type is "soc", "hlt", or "pt"
spec <- data.frame(
  soc = c("GI disorders", "GI disorders", "GI disorders"),
  term = c("GI disorders", "GI signs", "Nausea"),
  row_type = c("soc", "hlt", "pt"),
  result = c("72 (53.3)", "54 (40.0)", "24 (17.8)"),
  stringsAsFactors = FALSE
) |>
  fr_table() |>
  fr_cols(soc = fr_col(visible = FALSE),
          row_type = fr_col(visible = FALSE)) |>
  fr_rows(
    group_by  = "soc",
    indent_by = list(
      key    = "row_type",
      col    = "term",
      levels = c(soc = 0, hlt = 1, pt = 2)
    )
  )

## ── group_label: auto-inject group headers into display column ──────
## When group and display data are in separate columns

data.frame(
  group = c("Sex", "Sex", "Age", "Age", "Age"),
  stat  = c("Female", "Male", "Mean (SD)", "Median", "Min, Max"),
  value = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0", "65, 88"),
  stringsAsFactors = FALSE
) |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_rows(
    group_by    = "group",
    group_label = "stat",
    indent_by   = "stat"
  )
#> 
#> ── fr_spec: Table 
#> Data: 5 rows x 3 columns
#> Page: landscape letter, 9pt Courier New
#> Columns (2 visible of 3):
#> stat "stat" 0.82in left
#> value "value" 0.90in left
#> Header: valign=bottom
#> Rows: group_by=group, indent_by=stat

## ── sort_by: order a listing by subject and start date ────────────────

adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDT", "AESEV")] |>
  fr_table() |>
  fr_rows(sort_by = c("USUBJID", "ASTDT"))
#> 
#> ── fr_spec: Table 
#> Data: 20 rows x 5 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: sort_by=USUBJID,ASTDT

## ── repeat_cols: suppress repeated subject IDs in a listing ───────────

adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV")] |>
  fr_table() |>
  fr_rows(
    sort_by     = c("USUBJID", "AEBODSYS"),
    repeat_cols = "USUBJID"
  )
#> 
#> ── fr_spec: Table 
#> Data: 20 rows x 4 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: sort_by=USUBJID,AEBODSYS

## ── wrap = TRUE: enable text wrapping for long verbatim terms ─────────

adae[1:10, c("USUBJID", "AEBODSYS", "AEDECOD", "AEOUT")] |>
  fr_table() |>
  fr_rows(wrap = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 10 rows x 4 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: wrap

## ── Combined: sort_by + repeat_cols on adverse event listing ──────────

adae[1:30, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV", "ASTDT")] |>
  fr_table() |>
  fr_rows(
    sort_by     = c("USUBJID", "AEBODSYS", "AEDECOD"),
    repeat_cols = c("USUBJID", "AEBODSYS"),
    wrap        = TRUE
  )
#> 
#> ── fr_spec: Table 
#> Data: 30 rows x 5 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
#> Rows: sort_by=USUBJID,AEBODSYS,AEDECOD, wrap
```
