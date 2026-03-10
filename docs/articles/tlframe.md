# The Pharma Programmer's Handbook

## What is tlframe?

**tlframe** produces regulatory-grade clinical trial tables, listings,
and figures (TLFs) in RTF and PDF from a single specification. You
describe *what* the output should look like; tlframe handles the
rendering.

The core idea is a composable pipeline:

``` r
data |> fr_table() |> fr_titles(...) |> fr_cols(...) |> fr_render("out.rtf")
```

Every function takes a spec, returns a spec. Side effects happen only at
[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md).
Verb order doesn’t matter — tlframe resolves everything (column widths,
N-count labels, page breaks) at render time.

**Key design choices:**

- **No dplyr dependency.** Only lightweight infrastructure: rlang, cli,
  vctrs, stringi.
- **RTF and PDF from one spec.** Change the file extension; the output
  adapts.
- **Pharma-native defaults.** Landscape letter, 9pt Courier, ICH
  E3–compliant page headers/footers out of the box.
- **Built-in datasets.** Every example in this guide runs with data
  shipped in the package — no external files needed.

> **SAS parallel:** tlframe replaces `PROC REPORT` + ODS RTF/PDF. Each
> verb maps to a `DEFINE`, `COMPUTE`, or ODS statement. The pipeline
> replaces the procedural SAS workflow with a declarative specification.

## Table Anatomy

Every pharma table has the same structural components. Here’s how they
map to tlframe verbs:

    +------------------------------------------------------------------------+
    | Protocol TFRM-2024-001                                    CONFIDENTIAL |<- fr_pagehead()
    +------------------------------------------------------------------------+
    |                                                                        |
    |                           Table 14.1.1                                 |<- fr_titles()
    |               Demographics and Baseline Characteristics                |
    |                     Intent-to-Treat Population                         |
    |                                                                        |
    | ---------------------------------------------------------------------- |
    | [Spanning Header: "Active Treatment"]                                  |<- fr_spans()
    | ---------------------------------------------------------------------- |
    |                  Placebo   Zomerane 50mg   Zomerane 100mg    Total     |<- fr_cols() +
    |                   (N=45)      (N=45)          (N=45)        (N=135)    |   fr_header(n=)
    | ====================================================================== |<- fr_hlines()
    | Age (years)                                                            |
    |   Mean (SD)    68.2 (7.1)   67.8 (6.9)     68.0 (7.3)    68.0 (7.1)    |<- data rows
    |   Median       68.0         68.0           68.0           68.0         |
    |                                                                        |
    | Sex, n (%)                                                             |<- fr_rows(blank_after=)
    |   Male         26 (57.8)    24 (53.3)      25 (55.6)     75 (55.6)     |
    |   Female       19 (42.2)    21 (46.7)      20 (44.4)     60 (44.4)     |
    | ---------------------------------------------------------------------- |
    | Percentages based on number of subjects per treatment group.           |<- fr_footnotes()
    | MMSE = Mini-Mental State Examination.                                  |
    |                                                                        |
    +------------------------------------------------------------------------+
    | t_demog_14_1_5.R                                           Page 1 of 1 |<- fr_pagefoot()
    +------------------------------------------------------------------------+

Every labelled region has exactly one verb. Learn 10 verbs, build any
table.

## Your First Table in 3 Lines

``` r
spec <- tbl_demog |> fr_table()
spec
#> 
#> ── fr_spec: Table
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Courier New
#> Header: valign=bottom
```

[`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md)
wraps a data frame into an `fr_spec` object. Printing it shows what the
spec contains. To produce a file:

``` r
spec |> fr_render(tempfile(fileext = ".rtf"))
```

That’s a complete, valid RTF table — landscape letter, 9pt Courier, all
columns auto-labelled from column names. No configuration required.

> **SAS parallel:** `PROC REPORT DATA=tbl_demog; RUN;` with ODS RTF
> destination open. tlframe’s zero-config output is the same idea.

For PDF output, change the extension:

``` r
spec |> fr_render(tempfile(fileext = ".pdf"))
```

## Titles and Footnotes

### Basic Titles

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Intent-to-Treat Population"
  )
fr_get_titles(spec)[[1]]$content
#> [1] "Table 14.1.1"
```

Each argument to
[`fr_titles()`](https://vthanik.github.io/tlframe/reference/fr_titles.md)
produces one centered title line. Calling
[`fr_titles()`](https://vthanik.github.io/tlframe/reference/fr_titles.md)
again replaces all previous titles.

> **SAS parallel:** `TITLE1 "Table 14.1.1"; TITLE2 "Demographics...";`

### Styled Titles

Individual title lines can be styled by passing a named list instead of
a plain string:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    list("Sponsor: Acme Pharma", align = "left"),
    list("Protocol: TFRM-2024-001", align = "left"),
    "Table 14.1.1",
    list("Demographics and Baseline Characteristics", bold = TRUE),
    "Intent-to-Treat Population"
  )
length(fr_get_titles(spec))
#> [1] 5
```

Available list fields: `content`, `align` (`"left"`, `"center"`,
`"right"`), `bold` (logical), `font_size` (numeric).

The `.align` and `.bold` arguments set defaults for all lines:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    .bold = TRUE, .align = "center"
  )
fr_get_titles(spec)[[1]]$bold
#> [1] TRUE
```

### Footnotes

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "Percentages based on number of subjects per treatment group.",
    "MMSE = Mini-Mental State Examination.",
    .separator = FALSE
  )
length(fr_get_footnotes(spec))
#> [1] 2
```

Key options:

- **`.separator`**: `TRUE` (default) draws a thin rule above footnotes.
  Pharma convention often sets this to `FALSE`.
- **`.placement`**: `"bottom"` (default) or `"last"`. Last-page-only
  footnotes work in PDF; RTF repeats all footnotes on every page.
- **`.align`**: Default `"left"`.

> **SAS parallel:** `FOOTNOTE1 "Percentages..."; FOOTNOTE2 "MMSE...";`

### Inline Markup in Titles and Footnotes

Use `{fr_*()}` expressions inside any text string for rich formatting:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "{fr_super('a')} Fisher's exact test.",
    "{fr_super('b')} Cochran-Mantel-Haenszel test.",
    "BMI = Body Mass Index (kg/m{fr_super(2)})."
  )
fr_get_footnotes(spec)[[1]]$content
#> [1] "\001SUPER:a\002 Fisher's exact test."
```

Available markup functions:

| Function | Output | Use case |
|----|----|----|
| `fr_super(x)` | Superscript | Footnote markers, units |
| `fr_sub(x)` | Subscript | Chemical formulas |
| `fr_bold(x)` | Bold text | Emphasis |
| `fr_italic(x)` | Italic text | Population labels |
| `fr_underline(x)` | Underline | Highlighting |
| [`fr_dagger()`](https://vthanik.github.io/tlframe/reference/fr_dagger.md) | Dagger symbol | Footnote markers |
| [`fr_ddagger()`](https://vthanik.github.io/tlframe/reference/fr_ddagger.md) | Double dagger | Footnote markers |
| `fr_unicode(code)` | Unicode char | Plus-minus sign (177), etc. |
| [`fr_newline()`](https://vthanik.github.io/tlframe/reference/fr_newline.md) | Line break | Multi-line cells |

## Column Configuration

### Defining Columns

[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
configures columns. Each named argument is a column defined with
[`fr_col()`](https://vthanik.github.io/tlframe/reference/fr_col.md):

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    placebo        = fr_col("Placebo", align = "right"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "right"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "right"),
    total          = fr_col("Total", align = "right"),
    group          = fr_col(visible = FALSE)
  )
names(fr_get_columns(spec))
#> [1] "characteristic" "placebo"        "zom_50mg"       "zom_100mg"     
#> [5] "total"          "group"
```

[`fr_col()`](https://vthanik.github.io/tlframe/reference/fr_col.md)
parameters:

| Parameter | Default | Description |
|----|----|----|
| `label` | Column name | Display header text |
| `width` | `1.5` | Width in inches (or `"auto"`, `"35%"`) |
| `align` | Auto-detect | `"left"`, `"right"`, `"center"`, `"decimal"` |
| `header_align` | Same as `align` | Header cell alignment |
| `visible` | `TRUE` | `FALSE` hides from output (keeps for logic) |
| `stub` | `FALSE` | `TRUE` marks as stub (repeats in col_split panels) |

> **SAS parallel:**
> `DEFINE col / DISPLAY "Label" STYLE(column)=[width=2.5in just=right];`

### Width Modes

The `.width` argument to
[`fr_cols()`](https://vthanik.github.io/tlframe/reference/fr_cols.md)
sets the global width strategy:

``` r
# Fixed (default): each column uses its fr_col(width=) or 1.5 inches
fr_cols(.width = NULL, ...)

# Auto: estimate from content, scale down if wider than page
fr_cols(.width = "auto", ...)

# Fit: like auto, but always scales to fill the full page width
fr_cols(.width = "fit", ...)

# Equal: divide page equally among columns without explicit widths
fr_cols(.width = "equal", ...)

# Percentage: specify each column as a share of printable width
fr_cols(col_a = fr_col("A", width = "35%"), col_b = fr_col("B", width = "65%"))
```

For most pharma tables, `"fit"` or explicit inch widths work best.

### Decimal Alignment

Clinical tables frequently display mixed statistics — counts,
percentages, means, p-values — in the same column. `align = "decimal"`
automatically detects the stat type and aligns decimal points:

``` r
decimal_data <- data.frame(
  statistic = c("n", "Mean (SD)", "Median", "Min, Max", "p-value"),
  value     = c("45", "68.2 (7.1)", "68.0", "52, 84", "<0.001"),
  stringsAsFactors = FALSE
)
spec <- decimal_data |>
  fr_table() |>
  fr_cols(
    statistic = fr_col("Statistic", width = 1.5),
    value     = fr_col("Value", width = 2.0, align = "decimal")
  )
```

The engine recognises 10 stat display types: counts, floats, n (%), n/N
(%), estimate (spread), estimate (CI), range pairs, integer ranges,
p-values, and missing values. Cross-type alignment is handled
automatically.

### Hidden Columns and Label Functions

Columns used for grouping logic but not shown in output:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE))
# group column drives blank_after but doesn't appear in rendered output
```

The `.label_fn` argument auto-generates labels from column names:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    .label_fn = function(x) gsub("_", " ", tools::toTitleCase(x)),
    group = fr_col(visible = FALSE)
  )
fr_get_columns(spec)$characteristic$label
#> [1] "Characteristic"
```

## Header Presentation and N-Counts

### Basic Header Styling

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_header(bold = TRUE, align = "center")
```

[`fr_header()`](https://vthanik.github.io/tlframe/reference/fr_header.md)
controls the header row’s appearance. Parameters include `bold`,
`align`, `valign`, `bg`, `fg`, and `font_size`.

> **SAS parallel:**
> `PROC REPORT ... STYLE(header)=[just=center font_weight=bold];`

### N-Counts: The Four Forms

Clinical trial tables almost always show treatment arm sample sizes in
the column headers. tlframe supports four progressively powerful forms.

**Form 1: Named numeric vector** (most common)

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_header(
    n      = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    format = "{name}\n(N={n})"
  )
```

The `format` template has two tokens:
[name](https://github.com/christopherkenny/name) (the column label) and
`{n}` (the count). The default format is `"{name}\n(N={n})"`.

**Form 2: Named list** (for per-group counts with `page_by`)

When `page_by` creates separate pages per parameter, N-counts may differ
across pages. Pass a named list keyed by the page_by value:

``` r
fr_header(
  n = list(
    "Systolic BP (mmHg)"  = c(placebo = 42, zom_50mg = 44, zom_100mg = 43),
    "Diastolic BP (mmHg)" = c(placebo = 42, zom_50mg = 44, zom_100mg = 43)
  ),
  format = "{name}\n(N={n})"
)
```

**Form 3: Function** (computed per page group)

For maximum flexibility, pass a function that receives the page group’s
data and label:

``` r
fr_header(
  n = function(group_data, group_label) {
    c(placebo = sum(group_data$arm == "Placebo"),
      active  = sum(group_data$arm == "Active"))
  }
)
```

**Form 4: `"auto"`** (computed from a source dataset)

The simplest option for per-group counts. Provide the source dataset and
subject ID column:

``` r
fr_header(
  n = "auto",
  n_subject = "USUBJID",
  n_data = advs
)
```

tlframe counts unique subjects per treatment arm per page_by group
automatically. This is the recommended approach for vital signs and lab
tables.

## Row Organization

[`fr_rows()`](https://vthanik.github.io/tlframe/reference/fr_rows.md)
controls how rows are grouped, indented, separated, and paginated.

### page_by: Separate Pages per Parameter

``` r
vs_wk24 <- tbl_vs[tbl_vs$timepoint == "Week 24",
                   c("param", "statistic", "placebo_value",
                     "zom_50mg_value", "zom_100mg_value")]
spec <- vs_wk24 |>
  fr_table() |>
  fr_cols(param = fr_col(visible = FALSE)) |>
  fr_rows(page_by = "param", page_by_bold = TRUE)
```

Each unique value of `param` gets its own page (or section). The page_by
label appears as a bold header above each section.

> **SAS parallel:** `BY param;` in PROC REPORT with `#BYVAL` in titles.

### group_by and blank_after: Visual Sections

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_rows(group_by = "group", blank_after = "group")
```

`group_by` keeps groups together during pagination (no group is split
across pages). `blank_after` inserts a blank row after each group for
visual separation.

### indent_by: SOC/PT Hierarchies

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("SOC / Preferred Term", width = 3.0),
    row_type = fr_col(visible = FALSE)
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt")
```

`indent_by` indents the named column’s values by 2 characters whenever
the value differs from the group header. SOC terms appear flush-left;
PTs are indented beneath them.

> **SAS parallel:** `DEFINE pt / DISPLAY INDENT=2;` with
> `ORDER=INTERNAL` on the SOC variable.

### Combining Row Features

Real tables use multiple row features together:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("SOC / PT", width = 3.0),
    row_type = fr_col(visible = FALSE),
    placebo  = fr_col("Placebo"),
    zom_50mg = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total    = fr_col("Total")
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_header(
    n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    bold = TRUE, align = "center"
  ) |>
  fr_hlines("header")
```

### Listings: sort_by, repeat_cols, and wrap

For record-level listings,
[`fr_listing()`](https://vthanik.github.io/tlframe/reference/fr_listing.md)
provides listing-optimised defaults (8pt font, left-aligned, text
wrapping). The `sort_by` and `repeat_cols` arguments control sort order
and repeated column suppression:

``` r
ae_list <- adae[1:30, c("USUBJID", "ARM", "AEBODSYS",
                          "AEDECOD", "AESEV", "AESER")]
spec <- ae_list |>
  fr_listing() |>
  fr_titles("Listing 16.2.7.1", "Adverse Events") |>
  fr_cols(
    USUBJID  = fr_col("Subject ID", width = 1.3),
    ARM      = fr_col("Treatment", width = 1.2),
    AEBODSYS = fr_col("System Organ Class", width = 2.0),
    AEDECOD  = fr_col("Preferred Term", width = 1.5),
    AESEV    = fr_col("Severity", width = 0.8),
    AESER    = fr_col("Serious", width = 0.7)
  ) |>
  fr_rows(sort_by = c("ARM", "USUBJID"),
          repeat_cols = "ARM") |>
  fr_hlines("header")
```

**Listing defaults vs table defaults:**

| Feature | [`fr_table()`](https://vthanik.github.io/tlframe/reference/fr_table.md) | [`fr_listing()`](https://vthanik.github.io/tlframe/reference/fr_listing.md) |
|----|----|----|
| Font size | 9pt | 8pt |
| Alignment | Auto-detect | Left |
| Text wrapping | Off | On |
| Column splitting | Off | On |

`repeat_cols` suppresses repeated values so the treatment arm only
prints when it changes — the same as `NOREPEAT` in SAS PROC REPORT.

The `wrap = TRUE` option enables automatic text wrapping for long cell
values, using the column’s width to determine break points.

## Page Layout and Running Headers/Footers

### Page Configuration

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_page(
    orientation = "landscape",
    paper       = "letter",
    font_family = "Courier New",
    font_size   = 9,
    margins     = c(top = 1.0, right = 0.75, bottom = 1.0, left = 0.75)
  )
pg <- fr_get_page(spec)
pg$orientation
#> [1] "landscape"
pg$font_size
#> [1] 9
```

These are the package defaults — you only need
[`fr_page()`](https://vthanik.github.io/tlframe/reference/fr_page.md)
when overriding them. Available paper sizes: `"letter"`, `"a4"`,
`"legal"`.

Margin formats:

| Format                              | Meaning                           |
|-------------------------------------|-----------------------------------|
| `margins = 1.0`                     | All four margins = 1 inch         |
| `margins = c(1.0, 0.75)`            | Top/bottom = 1, left/right = 0.75 |
| `margins = c(1.0, 0.75, 1.0, 0.75)` | Top, right, bottom, left          |
| `margins = list(top = 1, ...)`      | Named list                        |

### Running Page Headers and Footers

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_pagehead(
    left  = "Protocol TFRM-2024-001",
    right = "CONFIDENTIAL"
  ) |>
  fr_pagefoot(
    left   = "{program}",
    center = "{datetime}",
    right  = "Page {thepage} of {total_pages}"
  )
```

**Built-in tokens** (resolved at render time):

| Token           | Value               |
|-----------------|---------------------|
| `{thepage}`     | Current page number |
| `{total_pages}` | Total page count    |
| `{program}`     | Source file name    |
| `{datetime}`    | Render timestamp    |

**Custom tokens** via `fr_page(tokens = ...)`:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "15MAR2025")) |>
  fr_pagefoot(
    left  = "Study: {study}",
    right = "Cutoff: {cutoff}"
  )
```

> **SAS parallel:** Page headers map to
> `ODS ESCAPECHAR='~'; TITLE j=l "Protocol..." j=r "CONFIDENTIAL";`.
> Pagefoot maps to
> `FOOTNOTE j=l "&_SASPROGRAMFILE" j=r "Page ^{thispage} of ^{lastpage}";`

### ICH E3 Compliance Mapping

ICH E3 requires specific elements on each TLF page. Here’s how tlframe
maps:

| ICH E3 Requirement | tlframe Feature |
|----|----|
| Study ID on every page | `fr_pagehead(left = "Study TFRM-2024-001")` |
| Table number and title | `fr_titles("Table 14.1.1", "Demographics...")` |
| Population label | Third title line: `"Intent-to-Treat Population"` |
| Treatment arm N-counts | `fr_header(n = ..., format = "{name}\n(N={n})")` |
| Page numbering | `fr_pagefoot(right = "Page {thepage} of {total_pages}")` |
| Program name | `fr_pagefoot(left = "{program}")` |
| Continuation label | `fr_page(continuation = "(continued)")` |
| Footnotes/abbreviations | `fr_footnotes(...)` |

### Continuation Text

For multi-page tables, `continuation` appends text to the title on pages
after the first:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1", "TEAEs by SOC and Preferred Term") |>
  fr_page(continuation = "(continued)")
# Page 2+ will show: "Table 14.3.1 (continued)"
```

### Section Spacing

[`fr_spacing()`](https://vthanik.github.io/tlframe/reference/fr_spacing.md)
controls the blank lines between structural sections:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_spacing(
    titles_after     = 1,
    footnotes_before = 1,
    pagehead_after   = 0,
    pagefoot_before  = 0,
    page_by_after    = 1
  )
```

## Rules, Spans, Styles, and Markup

### Horizontal Rules (fr_hlines)

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header")
```

**Presets** (most common first):

| Preset       | Description                                            |
|--------------|--------------------------------------------------------|
| `"header"`   | Lines above and below column headers (pharma standard) |
| `"open"`     | Top + bottom table border only                         |
| `"booktabs"` | Top (thick) + header-bottom (thin) + bottom (thick)    |
| `"box"`      | Full border around the table                           |
| `"hsides"`   | Top + bottom border                                    |
| `"void"`     | No horizontal rules                                    |

Custom styling:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("open", width = "thick", color = "#003366", linestyle = "dashed")
```

Width options: `"hairline"` (0.25pt), `"thin"` (0.5pt), `"medium"`
(1pt), `"thick"` (1.5pt), or a numeric value in points.

> **SAS parallel:**
> `STYLE(header)=[borderbottomstyle=solid borderbottomwidth=1pt];`

### Vertical Rules (fr_vlines) and Grid (fr_grid)

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_vlines("inner")
```

Vertical presets: `"box"`, `"inner"`, `"outer"`, `"all"`, `"void"`.

[`fr_grid()`](https://vthanik.github.io/tlframe/reference/fr_grid.md) is
shorthand for `fr_hlines("box") + fr_vlines("box")`:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_grid("box/all", width = "thin", color = "#999999")
```

### Spanning Headers (fr_spans)

Spanning headers group columns under a shared label:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_spans(
    "Active Treatment" = c("zom_50mg", "zom_100mg")
  ) |>
  fr_header(n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)) |>
  fr_hlines("header")
```

For multi-level spans, call
[`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md)
multiple times with different `.level` values:

``` r
spec |>
  fr_spans("Placebo" = c("pbo_base", "pbo_value"), .level = 1) |>
  fr_spans("Treatment Group" = c("pbo_base", "pbo_value",
                                  "act_base", "act_value"), .level = 2)
```

Unlike most verbs,
[`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md)
**appends** on repeated calls (it doesn’t replace).

### Cell Styling (fr_styles)

Apply visual overrides to specific rows, columns, or cells:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(rows = 1, bold = TRUE),                    # bold first row
    fr_col_style(cols = "total", bg = "#EBF5FB"),           # tint total column
    fr_style(rows = 3, cols = "placebo", fg = "#CC0000")    # red single cell
  )
```

**Style constructors:**

| Constructor | Scope | Key arguments |
|----|----|----|
| [`fr_row_style()`](https://vthanik.github.io/tlframe/reference/fr_row_style.md) | Entire row(s) | `rows`, `bold`, `bg`, `fg`, … |
| [`fr_col_style()`](https://vthanik.github.io/tlframe/reference/fr_col_style.md) | Entire column(s) | `cols`, `bold`, `bg`, `fg`, … |
| [`fr_style()`](https://vthanik.github.io/tlframe/reference/fr_style.md) | Row-column intersection | `rows`, `cols`, `region`, all style props |

Like
[`fr_spans()`](https://vthanik.github.io/tlframe/reference/fr_spans.md),
[`fr_styles()`](https://vthanik.github.io/tlframe/reference/fr_styles.md)
**appends** on repeated calls.

**Content-based row selection** with
[`fr_rows_matches()`](https://vthanik.github.io/tlframe/reference/fr_rows_matches.md):

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("SOC / PT", width = 3.0),
    row_type = fr_col(visible = FALSE),
    placebo  = fr_col("Placebo"),
    zom_50mg = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total    = fr_col("Total")
  ) |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "soc"),
      bold = TRUE
    ),
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "total"),
      bold = TRUE, bg = "#D5E8D4"
    )
  )
```

[`fr_rows_matches()`](https://vthanik.github.io/tlframe/reference/fr_rows_matches.md)
also supports regex patterns:

``` r
# Bold all rows where statistic starts with an uppercase letter
spec <- tbl_tte |>
  fr_table() |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("statistic", pattern = "^[A-Z]"),
      bold = TRUE
    )
  )
```

**Header region styling:**

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style(region = "header", bold = TRUE, bg = "#003366",
             fg = "#FFFFFF", align = "center")
  )
```

**Zebra striping:**

``` r
spec <- tbl_disp |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = seq(1, nrow(tbl_disp), 2), bg = "#F5F5F5")
  )
```

## Rendering

``` r
# RTF output
spec |> fr_render("output/table_14_1_5.rtf")

# PDF output (requires XeLaTeX via tinytex)
spec |> fr_render("output/table_14_1_5.pdf")

# LaTeX source (for manual compilation)
spec |> fr_render("output/table_14_1_5.tex")
```

The format is auto-detected from the file extension. Override with
`format =`:

``` r
spec |> fr_render("output.txt", format = "rtf")
```

[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md)
returns the output path invisibly, making it pipeline-friendly:

``` r
tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_hlines("header") |>
  fr_render(tempfile(fileext = ".rtf"))
```

**PDF requirements:** PDF rendering compiles LaTeX via XeLaTeX. Install
with
[`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html).
On Linux without Microsoft fonts, tlframe automatically uses bundled
Liberation fonts as fallback.

## Validation and Inspection

### Pre-Render Validation

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    group     = fr_col(visible = FALSE)
  )
result <- fr_validate(spec)
result$valid
#> NULL
```

[`fr_validate()`](https://vthanik.github.io/tlframe/reference/fr_validate.md)
checks that column names match the data, row configurations reference
valid columns, style indices are in range, and fonts are available. Use
`strict = TRUE` for stricter checks.

### Accessor Functions

The `fr_get_*()` family extracts spec components for programmatic
inspection or QC:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_footnotes("Source: ADSL") |>
  fr_cols(group = fr_col(visible = FALSE))

# Data
nrow(fr_get_data(spec))
#> [1] 28

# Titles
length(fr_get_titles(spec))
#> [1] 2

# Footnotes
fr_get_footnotes(spec)[[1]]$content
#> [1] "Source: ADSL"

# Page config
fr_get_page(spec)$font_size
#> [1] 9

# All columns
names(fr_get_columns(spec))
#> [1] "characteristic" "placebo"        "zom_50mg"       "zom_100mg"     
#> [5] "total"          "group"

# Single column
fr_get_col(spec, "group")$visible
#> [1] FALSE
```

### Style Debugging

[`fr_style_explain()`](https://vthanik.github.io/tlframe/reference/fr_style_explain.md)
shows the resolution chain for a specific cell:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = 1, bold = TRUE),
    fr_col_style(cols = "total", bg = "#EBF5FB")
  )
fr_style_explain(spec, row = 1, col = "total")
#> 
#> ── Style explain: row 1, col "total"
#> Content: "135"
#> 2 matching styles:
#> [1] row: bold=TRUE
#> [2] col: bg="#EBF5FB"
#> 
#> Final: bold=TRUE, italic=FALSE, fg=#000000, bg=#EBF5FB, align=left, valign=top,
#> indent=0
```

## Complete Production Table

Here’s a complete demographics table with every component — the kind
you’d submit in a regulatory filing:

``` r
spec <- tbl_demog |>
  fr_table() |>

  # ── Titles ──
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Intent-to-Treat Population"
  ) |>

  # ── Page chrome ──
  fr_pagehead(
    left  = "Protocol TFRM-2024-001",
    right = c("Page {thepage} of {total_pages}",
              "Database Lock: 15MAR2025")
  ) |>
  fr_pagefoot(
    left  = "{program}",
    right = "{datetime}"
  ) |>

  # ── Columns ──
  fr_cols(
    .width = "fit",
    characteristic = fr_col("", width = 2.5),
    placebo        = fr_col("Placebo", align = "decimal"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "decimal"),
    total          = fr_col("Total", align = "decimal"),
    group          = fr_col(visible = FALSE)
  ) |>

  # ── Header ──
  fr_header(
    bold   = TRUE,
    align  = "center",
    n      = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    format = "{name}\n(N={n})"
  ) |>

  # ── Rows ──
  fr_rows(group_by = "group", blank_after = "group") |>

  # ── Rules ──
  fr_hlines("header") |>

  # ── Footnotes ──
  fr_footnotes(
    "Percentages based on number of subjects per treatment group.",
    "MMSE = Mini-Mental State Examination.",
    .separator = FALSE
  )

# Inspect the result
fr_validate(spec)$valid
#> NULL
```

``` r
# Render to both formats
spec |> fr_render("output/Table_14_1_5.rtf")
spec |> fr_render("output/Table_14_1_5.pdf")
```

This is the same pattern every cookbook recipe below follows — just with
different data and configuration.

## Cookbook: Pharma Table Patterns

Each recipe uses built-in datasets and follows ICH E3 table numbering.
All examples are copy-paste ready.

### 14.1.1 Demographics Summary

``` r
demog_spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Intent-to-Treat Population"
  ) |>
  fr_cols(
    .width = "fit",
    characteristic = fr_col("", width = 2.5),
    placebo        = fr_col("Placebo", align = "decimal"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "decimal"),
    total          = fr_col("Total", align = "decimal"),
    group          = fr_col(visible = FALSE)
  ) |>
  fr_header(
    n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    bold = TRUE, align = "center",
    format = "{name}\n(N={n})"
  ) |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_hlines("header") |>
  fr_footnotes(
    "Percentages based on number of subjects per treatment group.",
    "MMSE = Mini-Mental State Examination.",
    .separator = FALSE
  )
```

### 14.1.4 Subject Disposition

``` r
disp_spec <- tbl_disp |>
  fr_table() |>
  fr_titles(
    "Table 14.1.4",
    list("Subject Disposition", bold = TRUE),
    "All Randomized Subjects"
  ) |>
  fr_cols(
    category  = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo", align = "decimal"),
    zom_50mg  = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
    total     = fr_col("Total", align = "decimal")
  ) |>
  fr_header(
    n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    bold = TRUE, align = "center",
    format = "{name}\n(N={n})"
  ) |>
  fr_hlines("header") |>
  fr_footnotes(
    "Percentages based on number of subjects randomized per arm.",
    .separator = FALSE
  )
```

### 14.3.1 AE by SOC/PT

``` r
ae_soc_spec <- tbl_ae_soc |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("Treatment-Emergent Adverse Events by SOC and Preferred Term",
         bold = TRUE),
    "Safety Population"
  ) |>
  fr_pagehead(left = "TFRM-2024-001", right = "CONFIDENTIAL") |>
  fr_pagefoot(left = "{program}",
              right = "Page {thepage} of {total_pages}") |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc       = fr_col(visible = FALSE),
    pt        = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type  = fr_col(visible = FALSE),
    placebo   = fr_col("Placebo", align = "decimal"),
    zom_50mg  = fr_col("Zomerane\n50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    total     = fr_col("Total", align = "decimal")
  ) |>
  fr_header(
    n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    bold = TRUE, align = "center",
    format = "{name}\n(N={n})"
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "total"), bold = TRUE),
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA version 26.0.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence.",
    .separator = FALSE
  )
```

### 14.2.1 Time-to-Event

``` r
tte_spec <- tbl_tte |>
  fr_table() |>
  fr_titles(
    "Table 14.2.1",
    list("Time to Study Withdrawal", bold = TRUE),
    "Intent-to-Treat Population"
  ) |>
  fr_cols(
    section   = fr_col(visible = FALSE),
    statistic = fr_col("", width = 3.5),
    zom_50mg  = fr_col("Zomerane\n50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    placebo   = fr_col("Placebo", align = "decimal")
  ) |>
  fr_header(
    n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45),
    bold = TRUE, align = "center",
    format = "{name}\n(N={n})"
  ) |>
  fr_rows(group_by = "section", blank_after = "section") |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("statistic", pattern = "^[A-Z]"),
      bold = TRUE
    )
  ) |>
  fr_footnotes(
    "[a] Kaplan-Meier estimate with Greenwood 95% CI.",
    "[b] Two-sided log-rank test stratified by age group.",
    "[c] Cox proportional hazards model.",
    "NE = Not Estimable.",
    .separator = FALSE
  )
```

### 14.4.1 Concomitant Medications

``` r
cm_spec <- tbl_cm |>
  fr_table() |>
  fr_titles(
    "Table 14.4.1",
    list("Concomitant Medications by Category and Agent", bold = TRUE),
    "Safety Population"
  ) |>
  fr_cols(
    category   = fr_col(visible = FALSE),
    medication = fr_col("Medication Category / Agent", width = 3.0),
    row_type   = fr_col(visible = FALSE),
    placebo    = fr_col("Placebo", align = "decimal"),
    zom_50mg   = fr_col("Zomerane\n50mg", align = "decimal"),
    zom_100mg  = fr_col("Zomerane\n100mg", align = "decimal"),
    total      = fr_col("Total", align = "decimal")
  ) |>
  fr_header(
    n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
    bold = TRUE, align = "center",
    format = "{name}\n(N={n})"
  ) |>
  fr_rows(group_by = "category", indent_by = "medication") |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "total"), bold = TRUE),
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "category"), bold = TRUE)
  ) |>
  fr_footnotes(
    "Subjects counted once per category and medication.",
    .separator = FALSE
  )
```

### 14.3.6 Vital Signs with page_by

``` r
vs_spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6",
    "Vital Signs --- Week 24 Summary",
    "Safety Population"
  ) |>
  fr_cols(
    param     = fr_col(visible = FALSE),
    timepoint = fr_col(visible = FALSE),
    statistic         = fr_col("Statistic", width = 1.2),
    placebo_base      = fr_col("Baseline"),
    placebo_value     = fr_col("Value"),
    placebo_chg       = fr_col("CFB"),
    zom_50mg_base     = fr_col("Baseline"),
    zom_50mg_value    = fr_col("Value"),
    zom_50mg_chg      = fr_col("CFB"),
    zom_100mg_base    = fr_col("Baseline"),
    zom_100mg_value   = fr_col("Value"),
    zom_100mg_chg     = fr_col("CFB")
  ) |>
  fr_rows(page_by = "param") |>
  fr_header(
    bold = TRUE, align = "center",
    n = "auto", n_subject = "USUBJID", n_data = advs,
    format = "{name}\n(N={n})"
  ) |>
  fr_spans(
    "Placebo"        = c("placebo_base", "placebo_value", "placebo_chg"),
    "Zomerane 50mg"  = c("zom_50mg_base", "zom_50mg_value", "zom_50mg_chg"),
    "Zomerane 100mg" = c("zom_100mg_base", "zom_100mg_value", "zom_100mg_chg")
  ) |>
  fr_hlines("header") |>
  fr_footnotes("CFB = Change from Baseline.", .separator = FALSE)
```

### 16.2.7 Adverse Event Listing

``` r
ae_list <- adae[1:40, c("USUBJID", "AEBODSYS", "AEDECOD",
                         "AESEV", "AESER", "AEREL", "AEACN")]
ae_listing_spec <- ae_list |>
  fr_listing() |>
  fr_titles(
    "Listing 16.2.7.1",
    list("Adverse Events", bold = TRUE),
    "Safety Population"
  ) |>
  fr_cols(
    USUBJID  = fr_col("Subject ID", width = 1.3),
    AEBODSYS = fr_col("System Organ Class", width = 1.8),
    AEDECOD  = fr_col("Preferred Term", width = 1.5),
    AESEV    = fr_col("Severity", width = 0.8),
    AESER    = fr_col("Serious", width = 0.7),
    AEREL    = fr_col("Related", width = 0.8),
    AEACN    = fr_col("Action", width = 1.2)
  ) |>
  fr_rows(sort_by = c("USUBJID", "AEBODSYS", "AEDECOD"),
          repeat_cols = "USUBJID") |>
  fr_hlines("header") |>
  fr_footnotes(
    "Related = Probable or Possible.",
    "Action = Action taken with study drug.",
    .separator = FALSE
  )
```

### Wide Table with Column Split

When a table has too many columns for one page, `fr_cols(.split = TRUE)`
splits the table into panels. Stub columns repeat in each panel:

``` r
wide_spec <- tbl_vs[tbl_vs$timepoint == "Week 24" &
                     tbl_vs$param == "Systolic BP (mmHg)", ] |>
  fr_table() |>
  fr_titles("Table 14.3.6", "Systolic BP --- Column Split") |>
  fr_cols(
    param     = fr_col(visible = FALSE),
    timepoint = fr_col(visible = FALSE),
    statistic = fr_col("Statistic", width = 1.2, stub = TRUE),
    .split = TRUE, .width = "fit",
    placebo_base      = fr_col("Placebo\nBaseline", width = 1.0),
    placebo_value     = fr_col("Placebo\nValue", width = 1.0),
    placebo_chg       = fr_col("Placebo\nCFB", width = 1.0),
    zom_50mg_base     = fr_col("Zom 50mg\nBaseline", width = 1.0),
    zom_50mg_value    = fr_col("Zom 50mg\nValue", width = 1.0),
    zom_50mg_chg      = fr_col("Zom 50mg\nCFB", width = 1.0),
    zom_100mg_base    = fr_col("Zom 100mg\nBaseline", width = 1.0),
    zom_100mg_value   = fr_col("Zom 100mg\nValue", width = 1.0),
    zom_100mg_chg     = fr_col("Zom 100mg\nCFB", width = 1.0)
  ) |>
  fr_hlines("header")
```

## What’s Next

This guide covered the core workflow: data in, spec out, render to file.
For more advanced topics:

- **Study-level automation**:
  [`fr_theme()`](https://vthanik.github.io/tlframe/reference/fr_theme.md),
  [`fr_config()`](https://vthanik.github.io/tlframe/reference/fr_config.md),
  YAML configuration, and batch rendering workflows — see
  [`vignette("advanced-features")`](https://vthanik.github.io/tlframe/articles/advanced-features.md).
- **Architecture and extending**: The render pipeline, custom backends,
  sentinel system — see
  [`vignette("extending-tlframe")`](https://vthanik.github.io/tlframe/articles/extending-tlframe.md).
