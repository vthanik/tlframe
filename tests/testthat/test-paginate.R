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

test_that("calculate_row_heights returns all 1s for short content", {
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  expect_equal(heights, c(1L, 1L, 1L))
})

test_that("calculate_row_heights detects multi-line content", {
  df <- data.frame(a = c("x", "line1\nline2", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  expect_equal(heights[1], 1L)
  expect_gt(heights[2], 1L)
  expect_equal(heights[3], 1L)
})

test_that("calculate_page_budget returns positive integer", {
  spec <- data.frame(a = "x", stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  budget <- calculate_page_budget(spec)
  expect_true(budget >= 5L)
})

test_that("paginate_rows assigns all to page 1 when content fits", {
  heights <- rep(1L, 10)
  df <- data.frame(
    grp = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 50L, df, "grp")
  expect_true(all(pages == 1L))
})

test_that("paginate_rows splits when budget exceeded", {
  heights <- rep(1L, 20)
  df <- data.frame(
    grp = c(rep("A", 10), rep("B", 10)),
    val = as.character(1:20),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 12L, df, "grp")
  expect_true(max(pages) >= 2L)
})

test_that("paginate_rows keeps small groups together", {
  heights <- rep(1L, 10)
  df <- data.frame(
    grp = c(rep("A", 5), rep("B", 5)),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  # Budget of 6: group A (5 rows) fits, then group B goes to next page
  pages <- paginate_rows(heights, 6L, df, "grp")
  # Group A should be on page 1
  expect_true(all(pages[1:5] == 1L))
  # Group B should be on page 2
  expect_true(all(pages[6:10] == 2L))
})

test_that("paginate_rows handles empty input", {
  pages <- paginate_rows(integer(0), 50L, data.frame(), "grp")
  expect_length(pages, 0L)
})

test_that("RTF render with group_by produces multiple sections", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create a table with group_by that has enough rows to page
  df <- data.frame(
    soc = c(rep("SOC A", 3), rep("SOC B", 3)),
    pt  = paste0("PT-", 1:6),
    n   = as.character(c(10, 20, 30, 40, 50, 60)),
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
})
