# ──────────────────────────────────────────────────────────────────────────────
# test-recipe.R — Tests for fr_recipe, fr_apply, c.fr_recipe, print.fr_recipe
# ──────────────────────────────────────────────────────────────────────────────

# ── fr_recipe() ──────────────────────────────────────────────────────────────

test_that("fr_recipe creates recipe from a single verb call", {
  r <- fr_recipe(fr_hlines("header"))
  expect_s3_class(r, "fr_recipe")
  expect_length(r, 1L)
  expect_true(is.call(r[[1L]]))
})

test_that("fr_recipe creates recipe from multiple verb calls", {
  r <- fr_recipe(
    fr_hlines("header"),
    fr_page(font_size = 8),
    fr_header(bold = TRUE)
  )
  expect_s3_class(r, "fr_recipe")
  expect_length(r, 3L)
  for (i in seq_along(r)) {
    expect_true(is.call(r[[i]]))
  }
})

test_that("fr_recipe captures calls without evaluating them", {
  # Calls are captured as expressions, not evaluated
  r <- fr_recipe(fr_page(orientation = "landscape", font_size = 9))
  fn_name <- as.character(r[[1L]][[1L]])
  expect_equal(fn_name, "fr_page")
})

test_that("fr_recipe errors on empty input", {
  expect_error(fr_recipe(), "At least one verb call is required")
})

test_that("fr_recipe errors on non-call argument", {
  expect_error(
    fr_recipe("not_a_call"),
    "must be a function call"
  )
})

test_that("fr_recipe errors on numeric argument", {
  expect_error(
    fr_recipe(42),
    "must be a function call"
  )
})

test_that("fr_recipe errors when a non-call is mixed with valid calls", {
  expect_error(
    fr_recipe(fr_hlines("header"), "invalid"),
    "Argument 2"
  )
})

test_that("fr_recipe errors on symbol (bare name) argument", {
  expect_error(
    fr_recipe(x),
    "must be a function call"
  )
})


# ── fr_apply() ───────────────────────────────────────────────────────────────

test_that("fr_apply applies a single verb recipe", {
  r <- fr_recipe(fr_page(font_size = 8))
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$page$font_size, 8)
})

test_that("fr_apply applies multiple verbs in order", {
  r <- fr_recipe(
    fr_hlines("header"),
    fr_page(font_size = 7, orientation = "landscape"),
    fr_header(bold = TRUE)
  )
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_equal(spec$page$font_size, 7)
  expect_equal(spec$page$orientation, "landscape")
  expect_true(spec$header$bold)
  expect_true(length(spec$rules) > 0L)
})

test_that("fr_apply applies titles and footnotes", {
  r <- fr_recipe(
    fr_titles("Table 1", "Demographics"),
    fr_footnotes("Note: * p < 0.05")
  )
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_equal(spec$meta$titles[[1L]]$content, "Table 1")
  expect_equal(spec$meta$titles[[2L]]$content, "Demographics")
  expect_equal(spec$meta$footnotes[[1L]]$content, "Note: * p < 0.05")
})

test_that("fr_apply works in a pipeline", {
  r <- fr_recipe(fr_header(bold = TRUE))
  spec <- tbl_demog |>
    fr_table() |>
    fr_apply(r) |>
    fr_page(font_size = 10)
  expect_true(spec$header$bold)
  expect_equal(spec$page$font_size, 10)
})

test_that("fr_apply verbs can override recipe settings downstream", {
  r <- fr_recipe(fr_header(bold = TRUE))
  spec <- tbl_demog |>
    fr_table() |>
    fr_apply(r) |>
    fr_header(bold = FALSE)
  expect_false(spec$header$bold)
})

test_that("fr_apply errors on non-spec first argument", {
  r <- fr_recipe(fr_hlines("header"))
  expect_error(fr_apply("not_a_spec", r))
})

test_that("fr_apply errors on non-recipe second argument", {
  spec <- tbl_demog |> fr_table()
  expect_error(
    fr_apply(spec, "not_a_recipe"),
    "must be an.*fr_recipe"
  )
})

test_that("fr_apply errors on list instead of recipe", {
  spec <- tbl_demog |> fr_table()
  expect_error(
    fr_apply(spec, list(quote(fr_hlines("header")))),
    "must be an.*fr_recipe"
  )
})

test_that("fr_apply applies recipe to different datasets", {
  r <- fr_recipe(
    fr_page(font_size = 8),
    fr_hlines("header")
  )
  spec1 <- tbl_demog |> fr_table() |> fr_apply(r)
  spec2 <- tbl_ae_soc |> fr_table() |> fr_apply(r)
  expect_equal(spec1$page$font_size, 8)
  expect_equal(spec2$page$font_size, 8)
})

test_that("fr_apply applies pagehead and pagefoot", {
  r <- fr_recipe(
    fr_pagehead(left = "{program}", right = "{datetime}"),
    fr_pagefoot(center = "Page {thepage} of {total_pages}")
  )
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_equal(spec$pagehead$left, "{program}")
  expect_equal(spec$pagehead$right, "{datetime}")
  expect_equal(spec$pagefoot$center, "Page {thepage} of {total_pages}")
})

test_that("fr_apply applies spacing", {
  r <- fr_recipe(fr_spacing(titles_after = 2))
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_equal(spec$spacing$titles_after, 2L)
})

test_that("fr_apply later verbs override earlier for replace-semantics", {
  r <- fr_recipe(
    fr_page(font_size = 8),
    fr_page(font_size = 12)
  )
  spec <- tbl_demog |> fr_table() |> fr_apply(r)
  expect_equal(spec$page$font_size, 12)
})


# ── c.fr_recipe() ───────────────────────────────────────────────────────────

test_that("c combines two recipes", {
  r1 <- fr_recipe(fr_hlines("header"))
  r2 <- fr_recipe(fr_page(font_size = 8))
  combined <- c(r1, r2)
  expect_s3_class(combined, "fr_recipe")
  expect_length(combined, 2L)
})

test_that("c combines three recipes", {
  r1 <- fr_recipe(fr_hlines("header"))
  r2 <- fr_recipe(fr_page(font_size = 8))
  r3 <- fr_recipe(fr_header(bold = TRUE))
  combined <- c(r1, r2, r3)
  expect_s3_class(combined, "fr_recipe")
  expect_length(combined, 3L)
})

test_that("c preserves order of calls", {
  r1 <- fr_recipe(fr_hlines("header"))
  r2 <- fr_recipe(fr_page(font_size = 8))
  combined <- c(r1, r2)
  fn1 <- as.character(combined[[1L]][[1L]])
  fn2 <- as.character(combined[[2L]][[1L]])
  expect_equal(fn1, "fr_hlines")
  expect_equal(fn2, "fr_page")
})

test_that("c combines recipes with multiple verbs each", {
  r1 <- fr_recipe(fr_hlines("header"), fr_page(font_size = 8))
  r2 <- fr_recipe(fr_header(bold = TRUE), fr_titles("T1"))
  combined <- c(r1, r2)
  expect_length(combined, 4L)
})

test_that("c.fr_recipe errors on non-recipe argument", {
  r1 <- fr_recipe(fr_hlines("header"))
  expect_error(
    c(r1, "not_a_recipe"),
    "must be.*fr_recipe"
  )
})

test_that("c.fr_recipe errors when list mixed with recipe", {
  r1 <- fr_recipe(fr_hlines("header"))
  expect_error(
    c(r1, list(1, 2, 3)),
    "must be.*fr_recipe"
  )
})

test_that("combined recipe applies correctly via fr_apply", {
  r1 <- fr_recipe(fr_hlines("header"))
  r2 <- fr_recipe(fr_page(font_size = 8))
  combined <- c(r1, r2)
  spec <- tbl_demog |> fr_table() |> fr_apply(combined)
  expect_equal(spec$page$font_size, 8)
  expect_true(length(spec$rules) > 0L)
})

test_that("combined recipe override works: later recipe wins for replace verbs", {
  r1 <- fr_recipe(fr_page(font_size = 8))
  r2 <- fr_recipe(fr_page(font_size = 12))
  combined <- c(r1, r2)
  spec <- tbl_demog |> fr_table() |> fr_apply(combined)
  expect_equal(spec$page$font_size, 12)
})


# ── print.fr_recipe() ───────────────────────────────────────────────────────

test_that("print.fr_recipe prints without error", {
  r <- fr_recipe(fr_hlines("header"), fr_page(font_size = 8))
  expect_no_error(print(r))
})

test_that("print.fr_recipe returns invisible", {
  r <- fr_recipe(fr_hlines("header"))
  out <- withVisible(print(r))
  expect_false(out$visible)
  expect_s3_class(out$value, "fr_recipe")
})

test_that("print.fr_recipe shows verb count", {
  r <- fr_recipe(fr_hlines("header"))
  out <- capture.output(print(r), type = "message")
  combined <- paste(out, collapse = " ")
  expect_match(combined, "1 verb")
  r2 <- fr_recipe(fr_hlines("header"), fr_page(font_size = 8))
  out2 <- capture.output(print(r2), type = "message")
  combined2 <- paste(out2, collapse = " ")
  expect_match(combined2, "2 verb")
})

test_that("print.fr_recipe shows function names", {
  r <- fr_recipe(fr_hlines("header"), fr_page(font_size = 8))
  out <- capture.output(print(r), type = "message")
  combined <- paste(out, collapse = " ")
  expect_match(combined, "fr_hlines")
  expect_match(combined, "fr_page")
})

test_that("print.fr_recipe shows named arguments", {
  r <- fr_recipe(fr_page(font_size = 8, orientation = "landscape"))
  out1 <- capture.output(print(r))
  out2 <- capture.output(print(r), type = "message")
  combined <- paste(c(out1, out2), collapse = " ")
  expect_match(combined, "font_size")
  expect_match(combined, "orientation")
})

test_that("print.fr_recipe handles verb with no arguments", {
  r <- fr_recipe(fr_hlines("header"))
  expect_no_error(print(r))
})

test_that("print.fr_recipe truncates long argument strings", {
  # Create a recipe with very long argument values
  r <- fr_recipe(
    fr_pagehead(
      left = "This is a very long string that should get truncated in the print output definitely",
      right = "Another long value here for testing"
    )
  )
  output <- capture.output(print(r), type = "message")
  combined <- paste(output, collapse = " ")
  # The truncation kicks in at 50 characters, adding "..."
  expect_match(combined, "\\.\\.\\.")
})


# ── Serialisation roundtrip ──────────────────────────────────────────────────

test_that("recipe survives saveRDS/readRDS roundtrip", {
  r <- fr_recipe(
    fr_page(font_size = 9, orientation = "landscape"),
    fr_hlines("header"),
    fr_header(bold = TRUE)
  )
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(r, tmp)
  r2 <- readRDS(tmp)

  expect_s3_class(r2, "fr_recipe")
  expect_length(r2, 3L)

  # Apply roundtripped recipe and verify it works

  spec <- tbl_demog |> fr_table() |> fr_apply(r2)
  expect_equal(spec$page$font_size, 9)
  expect_true(spec$header$bold)
})
