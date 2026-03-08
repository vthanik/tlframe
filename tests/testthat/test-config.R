# ──────────────────────────────────────────────────────────────────────────────
# test-config.R — Tests for YAML config system
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_config loads a YAML file and stores in fr_env", {
  # Create a temp config
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "page:",
    "  font_size: 10",
    "  orientation: portrait",
    "rules:",
    "  hlines: booktabs"
  ), tmp)

  fr_config_reset()
  cfg <- fr_config(tmp)

  expect_type(cfg, "list")
  expect_equal(cfg$page$font_size, 10)
  expect_equal(cfg$page$orientation, "portrait")
  expect_equal(cfg$rules$hlines, "booktabs")

  fr_config_reset()
})

test_that("fr_config_get returns loaded config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("page:\n  font_size: 11", tmp)

  fr_config(tmp)
  cfg <- fr_config_get()
  expect_equal(cfg$page$font_size, 11)

  fr_config_reset()
})

test_that("fr_config_reset clears config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("page:\n  font_size: 11", tmp)

  fr_config(tmp)
  fr_config_reset()

  # After reset, apply_config should be a no-op
  spec <- tbl_demog |> fr_table()
  # Default font size is 9, not 11

  expect_equal(spec$page$font_size, 9)
})

test_that("apply_config applies page settings to fr_table", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "page:",
    "  font_size: 10",
    "  orientation: portrait",
    "  paper: a4"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 10)
  expect_equal(spec$page$orientation, "portrait")
  expect_equal(spec$page$paper, "a4")
})

test_that("config < fr_theme < per-table verbs (precedence)", {
  fr_config_reset()
  fr_theme_reset()

  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); fr_theme_reset(); unlink(tmp) }, add = TRUE)

  # Config: font_size = 10
  writeLines("page:\n  font_size: 10", tmp)
  fr_config(tmp)

  # Theme: font_size = 11 (overrides config)
  fr_theme(font_size = 11)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 11)

  # Per-table verb: font_size = 12 (overrides theme)
  spec <- spec |> fr_page(font_size = 12)
  expect_equal(spec$page$font_size, 12)
})

test_that("config applies header defaults", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "header:",
    "  align: right",
    "  valign: top",
    "  bold: false"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$header$align, "right")
  expect_equal(spec$header$valign, "top")
  expect_false(spec$header$bold)
})

test_that("config applies custom tokens", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "tokens:",
    "  company: Pharma Corp",
    "  study_id: TFRM-001"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$tokens$company, "Pharma Corp")
  expect_equal(spec$page$tokens$study_id, "TFRM-001")
})

test_that("config applies rules", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "rules:",
    "  hlines: booktabs"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  # Should have booktabs rules (3 rules: top, mid, bottom)
  hline_rules <- Filter(function(r) inherits(r, "fr_rule_hline"), spec$rules)
  expect_length(hline_rules, 3L)
})

test_that("merge_config deep merges lists", {
  base <- list(a = 1, b = list(x = 10, y = 20), c = 3)
  over <- list(b = list(x = 99, z = 30), d = 4)
  result <- tlframe:::merge_config(base, over)

  expect_equal(result$a, 1)
  expect_equal(result$b$x, 99)
  expect_equal(result$b$y, 20)
  expect_equal(result$b$z, 30)
  expect_equal(result$c, 3)
  expect_equal(result$d, 4)
})

test_that("find_config finds _tlframe.yml in parent directories", {
  tmp_dir <- tempfile("tlframe_test_")
  dir.create(tmp_dir)
  sub_dir <- file.path(tmp_dir, "sub", "sub2")
  dir.create(sub_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Place config in parent
  writeLines("page:\n  font_size: 7", file.path(tmp_dir, "_tlframe.yml"))

  found <- tlframe:::find_config(sub_dir)
  expect_true(file.exists(found))
  cfg <- yaml::read_yaml(found)
  expect_equal(cfg$page$font_size, 7)
})

test_that("fr_config errors on missing file", {
  expect_error(fr_config("nonexistent_path.yml"), "not found")
})


# ── Spacing ──────────────────────────────────────────────────────────────────

test_that("fr_spacing sets spacing values on spec", {
  spec <- data.frame(a = 1) |> fr_table()

  # Defaults

  expect_equal(spec$spacing$titles_after, 1L)
  expect_equal(spec$spacing$footnotes_before, 1L)
  expect_equal(spec$spacing$pagehead_after, 0L)

  # Override
  spec2 <- spec |> fr_spacing(titles_after = 0L, footnotes_before = 2L)
  expect_equal(spec2$spacing$titles_after, 0L)
  expect_equal(spec2$spacing$footnotes_before, 2L)
  expect_equal(spec2$spacing$pagehead_after, 0L)  # unchanged
})

test_that("fr_spacing rejects negative values", {
  spec <- data.frame(a = 1) |> fr_table()
  expect_error(fr_spacing(spec, titles_after = -1L), "non-negative")
})

test_that("fr_spacing rejects non-integer values", {
  spec <- data.frame(a = 1) |> fr_table()
  expect_error(fr_spacing(spec, titles_after = 1.5), "non-negative integer")
})

test_that("config applies spacing from YAML", {
  tmp <- tempfile(fileext = ".yml")
  on.exit({ unlink(tmp); fr_config_reset() }, add = TRUE)
  writeLines(c(
    "spacing:",
    "  titles_after: 2",
    "  footnotes_before: 0"
  ), tmp)

  fr_config_reset()
  fr_config(tmp)
  spec <- data.frame(a = 1) |> fr_table()

  expect_equal(spec$spacing$titles_after, 2L)
  expect_equal(spec$spacing$footnotes_before, 0L)
  expect_equal(spec$spacing$pagehead_after, 0L)  # default kept
})

test_that("fr_theme applies spacing", {
  on.exit(fr_theme_reset(), add = TRUE)
  fr_theme_reset()
  fr_theme(spacing = list(titles_after = 3L))

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$spacing$titles_after, 3L)
  expect_equal(spec$spacing$footnotes_before, 1L)
})


test_that("apply_config warns on invalid hlines preset", {
  withr::local_options(list(cli.default_handler = identity))
  fr_config_reset()
  fr_env$config <- list(rules = list(hlines = "nonexistent_preset"))
  expect_warning(
    data.frame(a = 1) |> fr_table(),
    "rules\\.hlines"
  )
  fr_config_reset()
})
