# ──────────────────────────────────────────────────────────────────────────────
# test-render-pdf.R — Tests for render-pdf.R
# ──────────────────────────────────────────────────────────────────────────────

test_that("find_xelatex returns character or NULL", {
  result <- tlframe:::find_xelatex()
  expect_true(is.null(result) || is.character(result))
})

test_that("find_xelatex returns a path when xelatex is on PATH", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")
  result <- tlframe:::find_xelatex()
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("render_pdf runs without error", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- tbl_demog |> fr_table() |> fr_titles("Test Table")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  # Compilation may produce empty PDF depending on system LaTeX config;

  # we just test the pipeline doesn't error
  expect_no_error(fr_render(spec, tmp))
})

test_that("render_pdf with pagehead and pagefoot", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- tbl_demog |>
    fr_table() |>
    fr_titles("PDF Test") |>
    fr_pagehead(left = "Study X", right = "{datetime}") |>
    fr_pagefoot(center = "Page {thepage} of {total_pages}")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_pdf cleans up temp files", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- data.frame(a = 1:3) |> fr_table()
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  # .aux and .log should be cleaned up in tempdir
  base_name <- tools::file_path_sans_ext(basename(tmp))
  aux <- file.path(tempdir(), paste0(base_name, ".aux"))
  expect_false(file.exists(aux))
})

test_that("render_pdf errors when XeLaTeX not found (mocked)", {
  local_mocked_bindings(find_xelatex = function() NULL, .package = "tlframe")
  spec <- data.frame(a = 1:3) |> fr_table()
  finalized <- tlframe:::finalize_spec(spec)
  page_groups <- tlframe:::prepare_pages(finalized)
  col_panels <- tlframe:::calculate_col_panels(finalized)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  expect_error(
    tlframe:::render_pdf(finalized, page_groups, col_panels, tmp),
    "XeLaTeX not found"
  )
})
