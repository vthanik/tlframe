#' tlframe: Clinical Tables, Figures, and Listings for Regulatory Submission
#'
#' @description
#' `tlframe` produces regulatory-grade clinical trial tables, listings, and
#' figures (TLFs) in RTF and PDF from a single specification.
#'
#' Built on disciplined S3 classes with `rlang` error handling.
#'
#' ## Core Pipeline
#'
#' ```r
#' data |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1", .align = "left") |>
#'   fr_cols(param = fr_col("Parameter", width = 2.5)) |>
#'   fr_hline("booktabs") |>
#'   fr_footnotes("Source: ADSL") |>
#'   fr_page(font_size = 9, orientation = "landscape") |>
#'   fr_pagehead(left = "ABC-001", right = "Page {thepage} of {total_pages}") |>
#'   fr_render(output = c("rtf", "pdf"), path = "output/t_14_1_1")
#' ```
#'
#' ## Design Principles
#'
#' - **`fr_` prefix** avoids all base R clashes (col, text, sub, page)
#' - **Glue-string markup** in titles, footnotes, column labels, and cell data:
#'   `"{fr_super(1)} chi-square test"`
#' - **Titles/footnotes always in body**: page header/footer via `fr_pagehead()`
#'   / `fr_pagefoot()`
#' - **`{token}` placeholders** in page headers/footers: `{thepage}`,
#'   `{total_pages}`, `{program}`, `{datetime}`, plus custom tokens.
#'   Literal brace: `{{` produces `{` in output.
#' - **No trimming** of leading whitespace — pharma indentation preserved
#' - **Immutable pipeline**: every verb returns a modified spec, side effects
#'   only at `fr_render()`
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import cli
#' @importFrom glue glue
#' @importFrom stringi stri_replace_all_fixed stri_detect_regex stri_count_fixed
#' @importFrom tools file_ext
#' @importFrom vctrs vec_cast
#' @importFrom yaml read_yaml
## usethis namespace: end
NULL
