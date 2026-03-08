# ──────────────────────────────────────────────────────────────────────────────
# test-classes.R — Tests for classes.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_col (user-facing, exported)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col creates fr_col object with defaults", {
  col <- fr_col()
  expect_s3_class(col, "fr_col")
  expect_equal(col$label, "")
  expect_null(col$width)
  expect_null(col$align)
  expect_null(col$visible)
  expect_equal(col$id, "")
})

test_that("fr_col accepts all valid parameters", {
  col <- fr_col(label = "Parameter", width = 2.5, align = "decimal",
                visible = FALSE)
  expect_equal(col$label, "Parameter")
  expect_equal(col$width, 2.5)
  expect_equal(col$align, "decimal")
  expect_false(col$visible)
})

test_that("fr_col accepts all valid alignment values", {
  for (a in c("left", "center", "right", "decimal")) {
    col <- fr_col(align = a)
    expect_equal(col$align, a)
  }
})

test_that("fr_col errors on invalid alignment", {
  expect_error(fr_col(align = "justify"), class = "rlang_error")
})

test_that("fr_col errors on invalid width", {
  expect_error(fr_col(width = -1), class = "rlang_error")
  expect_error(fr_col(width = 0), class = "rlang_error")
  expect_error(fr_col(width = "wide"), class = "rlang_error")
})

test_that("fr_col errors on non-string label", {
  expect_error(fr_col(label = 123), class = "rlang_error")
  expect_error(fr_col(label = c("a", "b")), class = "rlang_error")
})

test_that("fr_col errors on non-logical visible", {
  expect_error(fr_col(visible = "yes"), class = "rlang_error")
  expect_error(fr_col(visible = 1), class = "rlang_error")
})

test_that("fr_col stores header_align", {
  col <- fr_col(header_align = "center")
  expect_equal(col$header_align, "center")
})

test_that("fr_col header_align defaults to NULL", {
  col <- fr_col()
  expect_null(col$header_align)
})

test_that("fr_col validates header_align", {
  for (a in c("left", "center", "right", "decimal")) {
    col <- fr_col(header_align = a)
    expect_equal(col$header_align, a)
  }
  expect_error(fr_col(header_align = "justify"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# normalise_margins
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalise_margins handles scalar (all sides equal)", {
  m <- normalise_margins(1)
  expect_equal(m, list(top = 1, bottom = 1, left = 1, right = 1))
})

test_that("normalise_margins handles scalar zero", {
  m <- normalise_margins(0)
  expect_equal(m, list(top = 0, bottom = 0, left = 0, right = 0))
})

test_that("normalise_margins handles length-2 (vertical, horizontal)", {
  m <- normalise_margins(c(1, 0.75))
  expect_equal(m, list(top = 1, bottom = 1, left = 0.75, right = 0.75))
})

test_that("normalise_margins handles length-4 (CSS t, r, b, l)", {
  m <- normalise_margins(c(1, 0.5, 1.5, 0.75))
  expect_equal(m, list(top = 1, right = 0.5, bottom = 1.5, left = 0.75))
})

test_that("normalise_margins handles named list", {
  m <- normalise_margins(list(top = 1, bottom = 1, left = 0.75, right = 0.75))
  expect_equal(m$top, 1)
  expect_equal(m$left, 0.75)
})

test_that("normalise_margins errors on length 3", {
  expect_error(normalise_margins(c(1, 2, 3)), class = "rlang_error")
})

test_that("normalise_margins errors on negative values", {
  expect_error(normalise_margins(-1), class = "rlang_error")
  expect_error(normalise_margins(c(1, -0.5)), class = "rlang_error")
})

test_that("normalise_margins errors on non-numeric non-list", {
  expect_error(normalise_margins("1inch"), class = "rlang_error")
  expect_error(normalise_margins(TRUE), class = "rlang_error")
})

test_that("normalise_margins errors on incomplete named list", {
  expect_error(normalise_margins(list(top = 1, bottom = 1)),
               class = "rlang_error")
})

test_that("normalise_margins errors on negative values in list", {
  expect_error(
    normalise_margins(list(top = 1, bottom = -1, left = 0.75, right = 0.75)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_user_tokens
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_user_tokens accepts empty list", {
  expect_silent(validate_user_tokens(list()))
})

test_that("validate_user_tokens accepts custom tokens", {
  expect_silent(validate_user_tokens(list(study = "ABC-001", pop = "ITT")))
})

test_that("validate_user_tokens allows overriding program and datetime", {
  expect_silent(validate_user_tokens(list(program = "my_script.R")))
  expect_silent(validate_user_tokens(list(datetime = "01MAR2025")))
})

test_that("validate_user_tokens errors on overriding thepage", {
  expect_error(validate_user_tokens(list(thepage = "99")),
               "thepage")
})

test_that("validate_user_tokens errors on overriding total_pages", {
  expect_error(validate_user_tokens(list(total_pages = "100")),
               "total_pages")
})

test_that("validate_user_tokens errors on unnamed list", {
  expect_error(validate_user_tokens(list("ABC")), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_page
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_page creates fr_page with defaults", {
  page <- new_fr_page()
  expect_s3_class(page, "fr_page")
  expect_equal(page$orientation, "landscape")
  expect_equal(page$paper, "letter")
  expect_equal(page$font_size, 9)
  expect_null(page$continuation)
  expect_false(page$col_split)
  expect_equal(page$orphan_min, 3L)
  expect_equal(page$widow_min, 3L)
})

test_that("new_fr_page accepts scalar margin", {
  page <- new_fr_page(margins = 1)
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$left, 1)
})

test_that("new_fr_page accepts length-2 margin", {
  page <- new_fr_page(margins = c(1, 0.75))
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$bottom, 1)
  expect_equal(page$margins$left, 0.75)
  expect_equal(page$margins$right, 0.75)
})

test_that("new_fr_page accepts length-4 margin", {
  page <- new_fr_page(margins = c(1, 0.5, 1.5, 0.75))
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$right, 0.5)
  expect_equal(page$margins$bottom, 1.5)
  expect_equal(page$margins$left, 0.75)
})

test_that("new_fr_page continuation defaults to NULL", {
  page <- new_fr_page()
  expect_null(page$continuation)
})

test_that("new_fr_page accepts explicit continuation", {
  page <- new_fr_page(continuation = "(continued)")
  expect_equal(page$continuation, "(continued)")
})

test_that("new_fr_page errors on invalid orientation", {
  expect_error(new_fr_page(orientation = "diagonal"), class = "rlang_error")
})

test_that("new_fr_page errors on invalid paper", {
  expect_error(new_fr_page(paper = "tabloid"), class = "rlang_error")
})

test_that("new_fr_page errors on font_size out of range", {
  expect_error(new_fr_page(font_size = 2), class = "rlang_error")
  expect_error(new_fr_page(font_size = 100), class = "rlang_error")
})

test_that("new_fr_page errors on overriding readonly tokens", {
  expect_error(new_fr_page(tokens = list(thepage = "1")))
})

test_that("new_fr_page accepts custom tokens", {
  page <- new_fr_page(tokens = list(study = "ABC-001"))
  expect_equal(page$tokens$study, "ABC-001")
})

test_that("new_fr_page col_split and stub_cols work", {
  page <- new_fr_page(col_split = TRUE, stub_cols = c("subjid", "param"))
  expect_true(page$col_split)
  expect_equal(page$stub_cols, c("subjid", "param"))
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_span
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_span creates fr_span object", {
  span <- new_fr_span("Treatment", c("drug", "placebo"), level = 1L)
  expect_s3_class(span, "fr_span")
  expect_equal(span$label, "Treatment")
  expect_equal(span$columns, c("drug", "placebo"))
  expect_equal(span$level, 1L)
})

test_that("new_fr_span errors on non-string label", {
  expect_error(new_fr_span(123, "col1"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_header
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_header creates fr_header with defaults", {
  h <- new_fr_header()
  expect_s3_class(h, "fr_header")
  expect_equal(h$spans, list())
  expect_true(h$repeat_on_page)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_body
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_body creates fr_body with defaults", {
  b <- new_fr_body()
  expect_s3_class(b, "fr_body")
  expect_equal(b$page_by, character(0))
  expect_equal(b$group_by, character(0))
})

test_that("new_fr_body stores column names", {
  b <- new_fr_body(page_by = "category", group_by = "param_group")
  expect_equal(b$page_by, "category")
  expect_equal(b$group_by, "param_group")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_rule — region/side/linestyle validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_rule creates fr_rule with defaults", {
  r <- new_fr_rule()
  expect_s3_class(r, "fr_rule")
  expect_s3_class(r, "fr_rule_hline")
  expect_equal(r$direction, "horizontal")
  expect_equal(r$region, "header")
  expect_equal(r$side, "below")
  expect_equal(r$linestyle, "solid")
  expect_equal(r$width, 0.5)
  expect_equal(r$fg, "#000000")
  expect_null(r$rows)
  expect_null(r$cols)
})

test_that("new_fr_rule uses fr_rule_vline subclass for vertical", {
  r <- new_fr_rule(direction = "vertical")
  expect_s3_class(r, "fr_rule_vline")
  expect_s3_class(r, "fr_rule")
})

test_that("new_fr_rule validates region", {
  expect_silent(new_fr_rule(region = "header"))
  expect_silent(new_fr_rule(region = "body"))
  expect_silent(new_fr_rule(region = "spanners"))
  expect_error(new_fr_rule(region = "footer"), class = "rlang_error")
  expect_error(new_fr_rule(region = "titles"), class = "rlang_error")
})

test_that("new_fr_rule validates side", {
  expect_silent(new_fr_rule(side = "above"))
  expect_silent(new_fr_rule(side = "below"))
  expect_error(new_fr_rule(side = "left"), class = "rlang_error")
  expect_error(new_fr_rule(side = "between"), class = "rlang_error")
})

test_that("new_fr_rule validates linestyle including dashdot", {
  for (ls in c("solid", "dashed", "dotted", "double", "dashdot")) {
    r <- new_fr_rule(linestyle = ls)
    expect_equal(r$linestyle, ls)
  }
  expect_error(new_fr_rule(linestyle = "wavy"),  class = "rlang_error")
  expect_error(new_fr_rule(linestyle = "dash"),  class = "rlang_error")
})

test_that("new_fr_rule validates direction", {
  expect_silent(new_fr_rule(direction = "horizontal"))
  expect_silent(new_fr_rule(direction = "vertical"))
  expect_error(new_fr_rule(direction = "diagonal"), class = "rlang_error")
})

test_that("new_fr_rule validates rows parameter", {
  # NULL is valid
  r <- new_fr_rule(rows = NULL)
  expect_null(r$rows)

  # "all" is valid
  r <- new_fr_rule(rows = "all")
  expect_equal(r$rows, "all")

  # Integer vector is valid
  r <- new_fr_rule(rows = c(5, 10))
  expect_equal(r$rows, c(5L, 10L))

  # Errors
  expect_error(new_fr_rule(rows = "some"), class = "rlang_error")
  expect_error(new_fr_rule(rows = c(-1, 5)), class = "rlang_error")
  expect_error(new_fr_rule(rows = TRUE), class = "rlang_error")
})

test_that("new_fr_rule validates cols parameter", {
  r <- new_fr_rule(cols = NULL)
  expect_null(r$cols)

  r <- new_fr_rule(cols = "all")
  expect_equal(r$cols, "all")

  r <- new_fr_rule(cols = c(2, 4))
  expect_equal(r$cols, c(2L, 4L))

  expect_error(new_fr_rule(cols = "some"), class = "rlang_error")
  expect_error(new_fr_rule(cols = c(0, 2)), class = "rlang_error")
})

test_that("new_fr_rule accepts named width shorthands", {
  expect_equal(new_fr_rule(width = "thin")$width,     0.50)
  expect_equal(new_fr_rule(width = "hairline")$width, 0.25)
  expect_equal(new_fr_rule(width = "medium")$width,   1.00)
  expect_equal(new_fr_rule(width = "thick")$width,    1.50)
})

test_that("new_fr_rule accepts numeric width in points", {
  expect_equal(new_fr_rule(width = 0.75)$width, 0.75)
})

test_that("new_fr_rule errors on invalid width", {
  expect_error(new_fr_rule(width = 0),       class = "rlang_error")
  expect_error(new_fr_rule(width = -1),      class = "rlang_error")
  expect_error(new_fr_rule(width = "huge"),  class = "rlang_error")
})

test_that("new_fr_rule stores partial line fields", {
  r <- new_fr_rule(leftpos = 0.1, rightpos = 0.9)
  expect_equal(r$leftpos,  0.1)
  expect_equal(r$rightpos, 0.9)
  expect_null(r$abovepos)
  expect_null(r$belowpos)
})

test_that("new_fr_rule validates partial line fractions", {
  expect_error(new_fr_rule(leftpos = -0.1), class = "rlang_error")
  expect_error(new_fr_rule(leftpos = 1.1),  class = "rlang_error")
  expect_error(new_fr_rule(rightpos = 2),   class = "rlang_error")
})

test_that("new_fr_rule validates and resolves color", {
  r <- new_fr_rule(fg = "#003366")
  expect_equal(r$fg, "#003366")

  r2 <- new_fr_rule(fg = "black")
  expect_match(r2$fg, "^#[0-9A-F]{6}$")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_cell_style
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_cell_style creates object with NULLs and type=cell", {
  cs <- new_fr_cell_style()
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type,   "cell")
  expect_equal(cs$region, "body")
  expect_null(cs$bold)
  expect_null(cs$fg)
  expect_null(cs$bg)
  expect_null(cs$colspan)
  expect_null(cs$rowspan)
})

test_that("new_fr_cell_style accepts style properties", {
  cs <- new_fr_cell_style(
    region = "header", bold = TRUE, italic = FALSE,
    fg = "#FF0000", bg = "#EEEEEE", font_size = 10
  )
  expect_equal(cs$region, "header")
  expect_true(cs$bold)
  expect_false(cs$italic)
  expect_equal(cs$fg, "#FF0000")
  expect_equal(cs$font_size, 10)
})

test_that("new_fr_cell_style stores colspan and rowspan", {
  cs <- new_fr_cell_style(colspan = 3L, rowspan = 2L)
  expect_equal(cs$colspan, 3L)
  expect_equal(cs$rowspan, 2L)
})

test_that("new_fr_row_style creates row-type cell_style", {
  rs <- new_fr_row_style(rows = c(1L, 2L), bg = "#EEEEEE", bold = TRUE)
  expect_s3_class(rs, "fr_cell_style")
  expect_equal(rs$type,   "row")
  expect_equal(rs$region, "body")
  expect_equal(rs$rows,   c(1L, 2L))
  expect_equal(rs$bg,     "#EEEEEE")
  expect_true(rs$bold)
  expect_null(rs$cols)
})

test_that("new_fr_col_style creates col-type cell_style", {
  cs <- new_fr_col_style(cols = c("a", "b"), fg = "#003366")
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type,   "col")
  expect_equal(cs$region, "body")
  expect_equal(cs$cols,   c("a", "b"))
  expect_equal(cs$fg,     "#003366")
  expect_null(cs$rows)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_meta, new_title_entry, new_footnote_entry
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_meta creates fr_meta with defaults", {
  m <- new_fr_meta()
  expect_s3_class(m, "fr_meta")
  expect_equal(m$titles, list())
  expect_equal(m$footnotes, list())
  expect_false(m$footnote_separator)
})

test_that("new_title_entry stores content and alignment", {
  te <- new_title_entry("Table 14.1.1", align = "left", bold = TRUE)
  expect_s3_class(te, "fr_title_entry")
  expect_equal(te$content, "Table 14.1.1")
  expect_equal(te$align, "left")
  expect_true(te$bold)
  expect_null(te$font_size)
})

test_that("fr_title_entry has a print method", {
  te <- new_title_entry("Table 14.1.1", align = "center", bold = TRUE)
  out <- capture.output(print(te))
  expect_match(out, "fr_title_entry")
  expect_match(out, "Table 14.1.1")
})

test_that("new_title_entry validates alignment", {
  expect_error(new_title_entry("x", align = "justify"), class = "rlang_error")
})

test_that("new_footnote_entry stores placement", {
  fe <- new_footnote_entry("Source: ADSL", placement = "every")
  expect_s3_class(fe, "fr_footnote_entry")
  expect_equal(fe$placement, "every")

  fe2 <- new_footnote_entry("End of table", placement = "last")
  expect_equal(fe2$placement, "last")
})

test_that("fr_footnote_entry has a print method", {
  fe <- new_footnote_entry("Source: ADSL", placement = "last")
  out <- capture.output(print(fe))
  expect_match(out, "fr_footnote_entry")
  expect_match(out, "Source: ADSL")
})

test_that("new_footnote_entry validates placement", {
  expect_error(new_footnote_entry("x", placement = "first"),
               class = "rlang_error")
})

test_that("new_footnote_entry defaults to left align and every placement", {
  fe <- new_footnote_entry("Source")
  expect_equal(fe$align, "left")
  expect_equal(fe$placement, "every")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_pagechrome
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_pagechrome creates fr_pagechrome", {
  pc <- new_fr_pagechrome(left = "Study ABC", right = "Page 1")
  expect_s3_class(pc, "fr_pagechrome")
  expect_equal(pc$left, "Study ABC")
  expect_equal(pc$right, "Page 1")
  expect_null(pc$center)
  expect_false(pc$bold)
})

test_that("new_fr_pagechrome accepts multi-line character vectors", {
  pc <- new_fr_pagechrome(left = c("Line 1", "Line 2"))
  expect_length(pc$left, 2)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_spec
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_spec creates fr_spec from data frame", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  spec <- new_fr_spec(df)
  expect_s3_class(spec, "fr_spec")
  expect_identical(spec$data, df)
  expect_s3_class(spec$meta, "fr_meta")
  expect_s3_class(spec$page, "fr_page")
  expect_equal(spec$rules, list())
  expect_equal(spec$cell_styles, list())
  expect_null(spec$pagehead)
  expect_null(spec$pagefoot)
})

test_that("new_fr_spec errors on non-data.frame", {
  expect_error(new_fr_spec(list(a = 1)), class = "rlang_error")
  expect_error(new_fr_spec("not a df"), class = "rlang_error")
  expect_error(new_fr_spec(1:5), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# Validators
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_fr_spec passes for valid fr_spec", {
  spec <- new_fr_spec(data.frame(x = 1))
  expect_invisible(check_fr_spec(spec))
})

test_that("check_fr_spec errors on non-fr_spec", {
  expect_error(check_fr_spec(list()), class = "rlang_error")
  expect_error(check_fr_spec(data.frame()), class = "rlang_error")
  expect_error(check_fr_spec("spec"), class = "rlang_error")
})

test_that("check_fr_col passes for valid fr_col", {
  col <- fr_col("test")
  expect_invisible(check_fr_col(col))
})

test_that("check_fr_col errors on non-fr_col", {
  expect_error(check_fr_col(list()), class = "rlang_error")
  expect_error(check_fr_col("col"), class = "rlang_error")
})


test_that("print.fr_spec shows dimensions and orientation", {
  spec <- data.frame(a = 1:3, b = letters[1:3]) |>
    fr_table() |>
    fr_titles("Test table") |>
    fr_cols(a = fr_col("A"), b = fr_col("B"))
  out <- capture.output(print(spec))
  expect_match(out, "fr_spec.*3 rows.*2 columns.*landscape")
  expect_match(out, "1 title")
})

test_that("print.fr_col shows label, width, and alignment", {
  col <- fr_col("Age", width = 1.5, align = "right")
  out <- capture.output(print(col))
  expect_match(out, "fr_col.*Age.*1\\.50in.*right")
})


# ══════════════════════════════════════════════════════════════════════════════
# Validation helpers
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_non_negative_int accepts valid inputs", {
  expect_equal(check_non_negative_int(0L), 0L)
  expect_equal(check_non_negative_int(3), 3L)
  expect_equal(check_non_negative_int(5L), 5L)
})

test_that("check_non_negative_int rejects invalid inputs", {
  expect_error(check_non_negative_int(-1), class = "rlang_error")
  expect_error(check_non_negative_int(1.5), class = "rlang_error")
  expect_error(check_non_negative_int(NA_real_), class = "rlang_error")
})

test_that("validate_n_param accepts valid n forms", {
  expect_silent(validate_n_param(n = c(a = 10, b = 20)))
  expect_silent(validate_n_param(n = "auto", n_subject = "USUBJID"))
  expect_silent(validate_n_param(n = function(d, g) c(a = 1)))
  expect_silent(validate_n_param(n = list(grp1 = c(a = 10))))
  expect_silent(validate_n_param(n = NULL))
})

test_that("validate_n_param rejects invalid n forms", {
  # unnamed numeric
  expect_error(validate_n_param(n = c(1, 2)), class = "rlang_error")
  # auto without n_subject
  expect_error(validate_n_param(n = "auto"), class = "rlang_error")
  # wrong type
  expect_error(validate_n_param(n = TRUE), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_pct — Percentage width
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col(width = '20%') stores fr_pct(0.20)", {
  col <- fr_col(width = "20%")
  expect_true(is_fr_pct(col$width))
  expect_equal(unclass(col$width), 0.20)
})

test_that("fr_col(width = '0%') errors (not positive)", {
  expect_error(fr_col(width = "0%"), class = "rlang_error")
})

test_that("fr_col(width = '101%') errors (over 100%)", {
  expect_error(fr_col(width = "101%"), class = "rlang_error")
})

test_that("fr_col(width = 1.5) still works (regression)", {
  col <- fr_col(width = 1.5)
  expect_equal(col$width, 1.5)
  expect_false(is_fr_pct(col$width))
})

test_that("fr_col(width = 'auto') still works (regression)", {
  col <- fr_col(width = "auto")
  expect_equal(col$width, "auto")
})

test_that("fr_col(width = '50.5%') parses decimal percentages", {
  col <- fr_col(width = "50.5%")
  expect_true(is_fr_pct(col$width))
  expect_equal(unclass(col$width), 0.505)
})

test_that("print.fr_col shows percentage for pct widths", {
  col <- fr_col("Test", width = "20%")
  out <- capture.output(print(col))
  expect_match(out, "20%", fixed = TRUE)
})

test_that("finalize_spec resolves fr_pct to absolute inches", {
  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = "20%")) |>
    fr_page(orientation = "landscape", paper = "letter", margins = 0.75)
  spec <- finalize_spec(spec)

  # Landscape letter 11in - 2*0.75 = 9.5in; 20% of 9.5 = 1.9
  expect_equal(spec$columns[["a"]]$width, 0.20 * 9.5, tolerance = 0.01)
})

test_that("fr_cols .width = '25%' applies to all columns", {
  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(.width = "25%")

  for (nm in c("a", "b", "c")) {
    expect_true(is_fr_pct(spec$columns[[nm]]$width))
    expect_equal(unclass(spec$columns[[nm]]$width), 0.25)
  }
})

test_that("mixed absolute and percentage widths work", {
  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 2.0),
      b = fr_col("B", width = "30%")
    ) |>
    fr_page(orientation = "landscape", paper = "letter", margins = 1)
  spec <- finalize_spec(spec)

  # a stays 2.0 inches; b = 30% of 9.0 = 2.7
  expect_equal(spec$columns[["a"]]$width, 2.0, tolerance = 0.01)
  expect_equal(spec$columns[["b"]]$width, 0.30 * 9.0, tolerance = 0.01)
})
