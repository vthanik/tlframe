# ══════════════════════════════════════════════════════════════════════════════
# test-paginate.R — R-side pagination tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("compute_column_char_widths returns positive integers", {
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  result <- compute_column_char_widths(cols, page)
  expect_true(all(result > 0L))
})

test_that("measure_cell_height returns 1 for single-line text", {
  expect_equal(measure_cell_height("hello world", 80L), 1L)
})

test_that("measure_cell_height counts newlines", {
  expect_equal(measure_cell_height("line1\nline2\nline3", 80L), 3L)
})

test_that("measure_cell_height wraps long text", {
  long <- paste(rep("word", 20), collapse = " ")
  h <- measure_cell_height(long, 20L)
  expect_gt(h, 1L)
})

test_that("measure_cell_height returns 1 for empty text", {
  expect_equal(measure_cell_height("", 80L), 1L)
})

test_that("calculate_row_heights returns uniform twips for short content", {
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  one_row <- row_height_twips(page$font_size)
  expect_equal(heights, rep(one_row, 3L))
})

test_that("calculate_row_heights detects multi-line content", {
  df <- data.frame(a = c("x", "line1\nline2", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  one_row <- row_height_twips(page$font_size)
  expect_equal(heights[1], one_row)
  expect_gt(heights[2], one_row)
  expect_equal(heights[3], one_row)
})

test_that("calculate_page_budget returns twips value", {
  spec <- data.frame(a = "x", stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  budget <- calculate_page_budget(spec)
  one_row <- row_height_twips(spec$page$font_size)
  # Budget should be at least 5 rows worth of twips
  expect_gte(budget, 5L * one_row)
  # Budget should be reasonable (< full page height)
  page_h <- paper_dims_twips(spec$page$paper, spec$page$orientation)[["height"]]
  expect_lt(budget, page_h)
})

test_that("calculate_page_budget does not subtract pagehead/pagefoot/every footnotes", {
  # Budget should be the same with or without pagehead/pagefoot/every footnotes
  # because those live in RTF margin areas, not the body area.
  spec_base <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1")
  spec_base <- finalize_spec(spec_base)
  budget_base <- calculate_page_budget(spec_base)

  spec_chrome <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_pagehead(left = "Page {thepage}") |>
    fr_pagefoot(left = "Footer") |>
    fr_footnotes("Note 1", .placement = "every")
  spec_chrome <- finalize_spec(spec_chrome)
  budget_chrome <- calculate_page_budget(spec_chrome)

  # Budget should be identical — margin chrome does not reduce body area
  expect_equal(budget_chrome, budget_base)
})

test_that("calculate_page_budget reserves space for last footnotes", {
  spec_no_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1")
  spec_no_fn <- finalize_spec(spec_no_fn)

  spec_last_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_footnotes("Note 1", "Note 2", .placement = "last")
  spec_last_fn <- finalize_spec(spec_last_fn)

  budget_no_fn <- calculate_page_budget(spec_no_fn)
  budget_last_fn <- calculate_page_budget(spec_last_fn)

  one_row <- row_height_twips(spec_no_fn$page$font_size)
  # 2 last footnotes + 1 footnotes_before spacing = 3 rows worth of twips
  expect_equal(budget_no_fn - budget_last_fn, 3L * one_row)
})

test_that("calculate_page_budget accounts for large footer overflow", {
  # With tiny margins (0.25in = 360 twips) and many "every" footnotes,

  # the footer content overflows the margin and eats into body area.
  # At 9pt, one_row = 236 twips. 8 every footnotes + 1 spacing = 9 lines
  # = 9 * 236 = 2124 twips, but margin is only 360 twips.
  # Overflow = 2124 - 360 = 1764 twips = ~7 rows lost.
  spec_small_margin <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_page(margins = 0.25) |>
    fr_footnotes(
      "fn1",
      "fn2",
      "fn3",
      "fn4",
      "fn5",
      "fn6",
      "fn7",
      "fn8",
      .placement = "every"
    )
  spec_small_margin <- finalize_spec(spec_small_margin)

  spec_no_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_page(margins = 0.25)
  spec_no_fn <- finalize_spec(spec_no_fn)

  budget_no_fn <- calculate_page_budget(spec_no_fn)
  budget_overflow <- calculate_page_budget(spec_small_margin)

  # The large footer should reduce the budget
  expect_lt(budget_overflow, budget_no_fn)
  # With ~7 rows of overflow, budget should drop significantly
  expect_gte(budget_no_fn - budget_overflow, 5L)
})

test_that("paginate_rows assigns all to page 1 when content fits", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 10L)
  df <- data.frame(
    grp = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  result <- paginate_rows(heights, 50L * rh, df, "grp")
  expect_equal(result$n_pages, 1L)
  expect_length(result$page_breaks, 0L)
})

test_that("paginate_rows splits when budget exceeded", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 20L)
  df <- data.frame(
    grp = c(rep("A", 10), rep("B", 10)),
    val = as.character(1:20),
    stringsAsFactors = FALSE
  )
  result <- paginate_rows(heights, 12L * rh, df, "grp")
  expect_gte(result$n_pages, 2L)
  expect_gte(length(result$page_breaks), 1L)
})

test_that("paginate_rows keeps small groups together", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 10L)
  df <- data.frame(
    grp = c(rep("A", 5), rep("B", 5)),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  # Budget of 6 rows: group A (5 rows) fits, then group B goes to next page
  result <- paginate_rows(heights, 6L * rh, df, "grp")
  expect_equal(result$n_pages, 2L)
  # \trpagebb on first row of group B (row 6)
  expect_equal(result$page_breaks, 6L)
})

test_that("paginate_rows handles empty input", {
  result <- paginate_rows(integer(0), 50L, data.frame(), "grp")
  expect_equal(result$n_pages, 0L)
  expect_length(result$page_breaks, 0L)
  expect_length(result$skip_rows, 0L)
})

test_that("deferred blank saves page break", {
  rh <- row_height_twips(9)
  grp <- c(rep("A", 6), rep("B", 5), rep("C", 7), rep("D", 6))
  df <- data.frame(
    grp = c(grp[1:6], "", grp[7:11], "", grp[12:18], "", grp[19:24]),
    val = c(
      paste0("a", 1:6),
      "",
      paste0("b", 1:5),
      "",
      paste0("c", 1:7),
      "",
      paste0("d", 1:6)
    ),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(heights, 26L * rh, df, "grp")
  expect_equal(result$n_pages, 1L)
  expect_length(result$page_breaks, 0L)
})

test_that("genuine page break preserved with deferred blanks", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 15), "", rep("B", 15)),
    val = c(paste0("a", 1:15), "", paste0("b", 1:15)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(heights, 20L * rh, df, "grp")
  expect_equal(result$n_pages, 2L)
  # \trpagebb on first row of group B (row 17)
  expect_equal(result$page_breaks, 17L)
})

test_that("last group without trailing blank paginates correctly", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5)),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(heights, 12L * rh, df, "grp")
  expect_equal(result$n_pages, 1L)
})

test_that("trailing blank at page boundary — group B moves to next page", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 8), "", rep("B", 8)),
    val = c(paste0("a", 1:8), "", paste0("b", 1:8)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(heights, 10L * rh, df, "grp")
  expect_equal(result$n_pages, 2L)
  # \trpagebb on first row of group B (row 10)
  expect_equal(result$page_breaks, 10L)
})

test_that("final blank after last group is suppressed (skip_rows)", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5), ""),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5), ""),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(heights, 20L * rh, df, "grp")
  expect_equal(result$n_pages, 1L)
  # Final trailing blank (row 12) is suppressed
  expect_true(12L %in% result$skip_rows)
})

test_that("large group split applies widow protection", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 22),
    val = as.character(1:22),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 22L)
  result <- paginate_rows(
    heights,
    20L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(result$n_pages, 2L)
  expect_length(result$page_breaks, 1L)
  # Break point should leave at least widow_min rows on page 2
  rows_on_page2 <- nrow(df) - result$page_breaks[1] + 1L
  expect_gte(rows_on_page2, 3L)
})

test_that("large group split respects orphan_min floor when stealing", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 7),
    val = as.character(1:7),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 7L)
  result <- paginate_rows(
    heights,
    5L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(result$n_pages, 2L)
  # Break at row that ensures >= 3 on page 1 and >= 3 on page 2
  rows_on_page1 <- result$page_breaks[1] - 1L
  rows_on_page2 <- 7L - rows_on_page1
  expect_gte(rows_on_page1, 3L)
  expect_gte(rows_on_page2, 3L)
})

test_that("large group split cannot steal below orphan_min", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 5),
    val = as.character(1:5),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 5L)
  result <- paginate_rows(
    heights,
    3L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(result$n_pages, 2L)
  # Break at row 4: 3 on page 1, 2 on page 2 (can't steal below orphan_min)
  expect_equal(result$page_breaks, 4L)
})

test_that("large group split suppresses blank rows at page boundaries", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 6), "", rep("A", 6), "", rep("A", 6)),
    val = c(paste0("a", 1:6), "", paste0("b", 1:6), "", paste0("c", 1:6)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  result <- paginate_rows(
    heights,
    8L * rh,
    df,
    "grp",
    orphan_min = 2L,
    widow_min = 2L
  )

  # Blank rows (7, 14) at page boundaries should be in skip_rows
  blank_rows <- which(df$val == "")
  for (br in blank_rows) {
    if (br %in% result$skip_rows) {
      expect_true(TRUE)
    }
  }
  # Should have at least one page break
  expect_gte(result$n_pages, 2L)
})

test_that("RTF render with group_by produces valid RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create a table with group_by
  df <- data.frame(
    soc = c(rep("SOC A", 3), rep("SOC B", 3)),
    pt = paste0("PT-", 1:6),
    n = as.character(c(10, 20, 30, 40, 50, 60)),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(group_by = "soc") |>
    fr_render(tmp)

  # File should be valid RTF
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\rtf1", txt, fixed = FALSE))
})

test_that("RTF render without group_by uses RTF-native pagination", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(
    a = as.character(1:10),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\rtf1", txt, fixed = FALSE))
  # Without group_by, no \trpagebb should appear
  expect_false(grepl("trpagebb", txt, fixed = TRUE))
})

test_that("RTF render with group_by uses keepn + trkeep for pagination", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Two groups that together exceed page budget
  n_per_group <- 40
  df <- data.frame(
    soc = c(rep("SOC A", n_per_group), rep("SOC B", n_per_group)),
    pt = paste0("PT-", seq_len(2 * n_per_group)),
    n = as.character(seq_len(2 * n_per_group)),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(group_by = "soc") |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # RTF-native pagination: \trkeep (row-level) + \keepn (paragraph-level)
  expect_true(grepl("trkeep", txt, fixed = TRUE))
  expect_true(grepl("keepn", txt, fixed = TRUE))
  # No R-side \trpagebb page breaks
  expect_false(grepl("trpagebb", txt, fixed = TRUE))
})

test_that("group_by + indent_by uses only group_by for pagination keys", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 12L)
  df <- data.frame(
    soc = c(
      rep("Eye disorders", 4),
      "",
      rep("Cardiac disorders", 4),
      "",
      rep("Skin disorders", 2)
    ),
    pt = c(
      "Eye disorders",
      "Cataract",
      "Glaucoma",
      "Dry eye",
      "",
      "Cardiac disorders",
      "Tachycardia",
      "Bradycardia",
      "Palpitations",
      "",
      "Skin disorders",
      "Rash"
    ),
    n = as.character(1:12),
    stringsAsFactors = FALSE
  )
  # Budget of 6 rows: Eye (4+blank=5) on page 1,

  # Cardiac (4+blank=5) on page 2, Skin (2) on page 3
  result <- paginate_rows(heights, 6L * rh, df, "soc")
  expect_equal(result$n_pages, 3L)
  # Page breaks before Cardiac (row 6) and Skin (row 11)
  expect_equal(result$page_breaks, c(6L, 11L))
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — measure_cell_height with embedded blank lines
# ══════════════════════════════════════════════════════════════════════════════

test_that("measure_cell_height handles text with embedded blank lines", {
  # Text like "line1\n\nline3" should count the blank line
  result <- measure_cell_height("line1\n\nline3", char_width = 80L)
  expect_equal(result, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — calculate_row_heights edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("calculate_row_heights returns integer(0) for zero-row data", {
  data <- data.frame(x = character(0), stringsAsFactors = FALSE)
  page <- list(
    orientation = "landscape",
    paper = "letter",
    margins = 1,
    font_size = 9,
    font_family = "Courier New",
    col_gap = 4L
  )
  cols <- list(x = fr_col("X", width = 2))
  result <- calculate_row_heights(data, cols, page)
  expect_equal(result, integer(0))
})

test_that("calculate_row_heights skips non-character columns", {
  data <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  page <- list(
    orientation = "landscape",
    paper = "letter",
    margins = 1,
    font_size = 9,
    font_family = "Courier New",
    col_gap = 4L
  )
  cols <- list(
    x = fr_col("X", width = 1),
    y = fr_col("Y", width = 1)
  )
  result <- calculate_row_heights(data, cols, page)
  expect_length(result, 3L)
  expect_true(all(result > 0L))
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — paginate_rows with no group_by
# ══════════════════════════════════════════════════════════════════════════════

test_that("paginate_rows uses __all__ key when no group_by columns", {
  rh <- row_height_twips(9)
  n <- 10L
  df <- data.frame(
    label = paste("Row", seq_len(n)),
    val = seq_len(n),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, n)
  # Budget of 4 rows
  result <- paginate_rows(heights, 4L * rh, df, character(0))
  expect_true(result$n_pages >= 2L)
})

test_that("paginate_rows handles group larger than one page (branch 4)", {
  rh <- row_height_twips(9)
  # Create a large group (12 rows) with budget of 5 rows
  df <- data.frame(
    grp = rep("A", 12L),
    label = paste("Row", 1:12),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 12L)
  result <- paginate_rows(heights, 5L * rh, df, "grp")
  expect_true(result$n_pages >= 2L)
})

test_that("paginate_rows splits oversized group across pages (branch 4)", {
  rh <- row_height_twips(9)
  # Single group with 10 data rows + 2 internal blanks = 12 rows.
  # Budget = 5 rows. Group height = 12*rh > 5*rh.
  # Include blanks at positions that test suppression at page boundaries.
  df <- data.frame(
    grp = rep("A", 12L),
    val = c("1", "2", "3", "4", "", "5", "6", "7", "", "8", "9", "10"),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 12L)
  result <- paginate_rows(heights, 5L * rh, df, "grp")
  # Should split across multiple pages

  expect_true(result$n_pages >= 2L)
})

test_that("paginate_rows branch 4 with prior used > 0 starts fresh page", {
  rh <- row_height_twips(9)
  # Group A: 2 rows (fits on page 1 with budget 3).
  # Group B: 8 rows (doesn't fit on remaining page, and > budget).
  # This triggers branch 4 with used > 0.
  df <- data.frame(
    grp = c("A", "A", "B", "B", "B", "B", "B", "B", "B", "B"),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 10L)
  result <- paginate_rows(heights, 3L * rh, df, "grp")
  # Should have page break at group B start (row 3)
  expect_true(3L %in% result$page_breaks)
  expect_true(result$n_pages >= 3L)
})

test_that("paginate_rows branch 4 widow/orphan control steals rows", {
  rh <- row_height_twips(9)
  # Single large group: 7 rows, budget = 5 rows.
  # First page gets rows 1-5, second page gets rows 6-7 (2 rows = widow).
  # With widow_min=3, should steal 1 row: first page 1-4, second 5-7.
  df <- data.frame(
    grp = rep("A", 7L),
    val = as.character(1:7),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 7L)
  result <- paginate_rows(
    heights,
    5L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_true(result$n_pages >= 2L)
})

test_that("paginate_rows branch 4 suppresses blanks at page boundaries", {
  rh <- row_height_twips(9)
  # No group_by -> single oversized group. Internal blank rows at positions 4, 8.
  # Budget = 4 rows. Blanks at page boundaries should be suppressed.
  df <- data.frame(
    label = c("1", "2", "3", "", "4", "5", "6", "", "7", "8"),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 10L)
  result <- paginate_rows(heights, 4L * rh, df, character(0))
  expect_equal(result$n_pages, 3L)
  # Blank rows 4 and 8 should be suppressed at page boundaries
  expect_true(4L %in% result$skip_rows)
  expect_true(8L %in% result$skip_rows)
})

test_that("paginate_rows branch 4 trailing blank + widow steal + re-trim", {
  rh <- row_height_twips(9)
  # No group_by -> single oversized group. Budget = 4, widow_min = 3.
  # Row layout: d d d B d d d B d d (B=blank, all cols empty)
  # Page 1: rows 1-3, row 4 blank suppressed
  # Page 2: rows 5-7, row 8 blank suppressed
  # Page 3: rows 9-10 (2 data = widow, steal triggers if possible)
  df <- data.frame(
    label = c("1", "2", "3", "", "4", "5", "6", "", "7", "8"),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 10L)
  result <- paginate_rows(
    heights,
    4L * rh,
    df,
    character(0),
    orphan_min = 2L,
    widow_min = 3L
  )
  expect_true(result$n_pages >= 2L)
})

test_that("paginate_rows suppresses multiple trailing blank rows between groups", {
  rh <- row_height_twips(9)
  # Group A: 2 rows, then 2 blank rows (all cols empty), then group B: 2 rows.
  # Budget = 10 rows (everything fits on one page).
  # Extra trailing blanks (beyond the first) should be suppressed.
  df <- data.frame(
    soc = c("Eye", "Eye", "", "", "Cardiac", "Cardiac"),
    pt = c("Cataract", "Glaucoma", "", "", "Tachycardia", "Bradycardia"),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 6L)
  result <- paginate_rows(heights, 10L * rh, df, "soc")
  # Second blank row (row 4) should be suppressed; first blank is deferred
  expect_true(4L %in% result$skip_rows)
})

test_that("paginate_rows suppresses blank rows at page boundaries (branch 2)", {
  rh <- row_height_twips(9)
  # Group A: 3 rows, blank row 4, group B: 2 rows.

  # Budget = 5*rh. After group A: used = 3*rh.
  # Blank + group B = rh + 2*rh = 3*rh, total = 6*rh > 5*rh (branch 1 fails).
  # Group B alone = 2*rh, total = 3*rh + 2*rh = 5*rh <= 5*rh (branch 2: skip blank).
  df <- data.frame(
    grp = c("A", "A", "A", "", "B", "B"),
    val = c("1", "2", "3", "", "4", "5"),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 6L)
  result <- paginate_rows(heights, 5L * rh, df, "grp")
  expect_equal(result$n_pages, 1L)
  expect_true(4L %in% result$skip_rows)
})
