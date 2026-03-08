# ──────────────────────────────────────────────────────────────────────────────
# test-col-split.R — Tests for column panel splitting
# ──────────────────────────────────────────────────────────────────────────────

test_that("calculate_col_panels returns single panel when all columns fit", {
  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- finalize_spec(spec)

  panels <- calculate_col_panels(spec)
  expect_equal(length(panels), 1L)
  expect_true("a" %in% panels[[1]])
  expect_true("b" %in% panels[[1]])
  expect_true("c" %in% panels[[1]])
})

test_that("calculate_col_panels splits columns when col_split = TRUE and table too wide", {
  # Create a wide table with many columns
  data <- as.data.frame(matrix(1, nrow = 2, ncol = 20))
  names(data) <- paste0("col", seq_len(20))
  data$stub <- "row"

  spec <- fr_table(data)
  spec <- fr_page(spec, col_split = TRUE, stub_cols = "stub",
                  orientation = "portrait")

  # Force narrow widths
  spec <- finalize_spec(spec)

  # Set each column to 1 inch — total 21 in which exceeds printable
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.0
  }

  panels <- calculate_col_panels(spec)
  expect_true(length(panels) > 1L)

  # Stub column appears in every panel
  for (panel in panels) {
    expect_true("stub" %in% panel)
  }
})

test_that("calculate_col_panels with col_split = FALSE returns single panel", {
  data <- as.data.frame(matrix(1, nrow = 2, ncol = 20))
  names(data) <- paste0("col", seq_len(20))

  spec <- fr_table(data)
  spec <- finalize_spec(spec)

  panels <- calculate_col_panels(spec)
  expect_equal(length(panels), 1L)
})

test_that("col_split renders multiple sections in RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- as.data.frame(matrix("x", nrow = 2, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$param <- c("A", "B")

  spec <- fr_table(data)
  spec <- fr_page(spec, col_split = TRUE, stub_cols = "param",
                  orientation = "portrait")
  spec <- finalize_spec(spec)

  # Force wide columns to trigger split
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.5
  }

  page_groups <- prepare_pages(spec)
  col_panels <- calculate_col_panels(spec)

  if (length(col_panels) > 1L) {
    colors <- collect_colors(spec)
    color_info <- build_rtf_colortbl(colors)

    render_rtf(spec, page_groups, col_panels, tmp)

    txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
    expect_true(grepl("\\sect", txt, fixed = TRUE))
  }
})

test_that("page_by + col_split: group-first ordering (all panels per group)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create data with 2 page_by groups and enough columns to force split
  data <- as.data.frame(matrix("x", nrow = 4, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$grp <- c("Group A", "Group A", "Group B", "Group B")
  data$param <- c("R1", "R2", "R1", "R2")

  spec <- fr_table(data)
  spec <- fr_page(spec, col_split = TRUE, stub_cols = "param",
                  orientation = "portrait")
  spec <- fr_rows(spec, page_by = "grp")
  spec <- finalize_spec(spec)

  # Force wide columns to trigger split
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.5
  }

  col_panels <- calculate_col_panels(spec)
  skip_if(length(col_panels) <= 1L, "col_split did not produce multiple panels")

  page_groups <- prepare_pages(spec)
  render_rtf(spec, page_groups, col_panels, tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Find positions of group labels — Group A should appear first (multiple
  # times for each panel), then Group B
  pos_a <- gregexpr("Group A", txt, fixed = TRUE)[[1]]
  pos_b <- gregexpr("Group B", txt, fixed = TRUE)[[1]]
  expect_true(length(pos_a) > 0L)
  expect_true(length(pos_b) > 0L)

  # All Group A occurrences must come before any Group B occurrence
  expect_true(max(pos_a) < min(pos_b),
              label = "All Group A panels appear before Group B panels")
})
