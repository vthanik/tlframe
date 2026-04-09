# ──────────────────────────────────────────────────────────────────────────────
# test-helpers.R — Tests for helpers.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Error conditions
# ══════════════════════════════════════════════════════════════════════════════

test_that("arframe_error raises error with arframe_error class", {
  expect_error(
    arframe_error("test error"),
    class = "arframe_error"
  )
})


test_that("arframe_error supports cli interpolation via ...", {
  # cli_abort evaluates glue expressions in caller_env(), which is
  # arframe_error's frame -- not the test's frame. So {col_name} from
  # a local variable here is not visible. Use a literal .val instead.
  expect_error(
    arframe_error("Column {.val age} not found"),
    "age"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# normalise_text
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalise_text passes through plain strings", {
  expect_equal(normalise_text("hello"), "hello")
  expect_equal(normalise_text(""), "")
})

test_that("normalise_text evaluates markup", {
  result <- normalise_text("{fr_super(1)} test")
  expect_true(grepl("\x01SUPER:1\x02", result, fixed = TRUE))
})

test_that("normalise_text errors on non-character", {
  expect_error(normalise_text(123), class = "rlang_error")
  expect_error(normalise_text(NULL), class = "rlang_error")
})

test_that("normalise_text errors on vector input", {
  expect_error(normalise_text(c("a", "b")), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_timestamp
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_timestamp returns pharma format string", {
  ts <- get_timestamp()
  expect_true(is.character(ts))
  expect_true(nzchar(ts))
  # Format: ddMONyyyy HH:MM:SS (e.g., "03MAR2026 14:30:00")
  expect_match(ts, "^\\d{2}[A-Za-z]{3}\\d{4} \\d{2}:\\d{2}:\\d{2}$")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_source_path
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_source_path returns character scalar", {
  result <- get_source_path()
  expect_true(is.character(result))
  expect_length(result, 1)
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map includes all built-in tokens", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 5, spec = spec)

  expect_true("thepage" %in% names(tm))
  expect_true("total_pages" %in% names(tm))
  expect_true("program" %in% names(tm))
  expect_true("datetime" %in% names(tm))

  expect_equal(tm$thepage, "1")
  expect_equal(tm$total_pages, "5")
})

test_that("build_token_map uses pagination engine values for readonly tokens", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 3, total_pages = 10, spec = spec)
  expect_equal(tm$thepage, "3")
  expect_equal(tm$total_pages, "10")
})

test_that("build_token_map merges custom tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(study = "ABC-001", pop = "ITT"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$study, "ABC-001")
  expect_equal(tm$pop, "ITT")
})

test_that("build_token_map allows program override via tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "my_script.R"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$program, "my_script.R")
})

test_that("build_token_map allows datetime override via tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(datetime = "01MAR2025 10:00:00"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$datetime, "01MAR2025 10:00:00")
})

test_that("build_token_map does not duplicate overridable tokens in custom", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "test.R", study = "XYZ"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(sum(names(tm) == "program"), 1)
  expect_equal(tm$study, "XYZ")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_default_columns
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_default_columns creates fr_col for each column", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), c = c(TRUE, FALSE, TRUE))
  cols <- build_default_columns(df)

  expect_length(cols, 3)
  expect_named(cols, c("a", "b", "c"))
  for (col in cols) {
    expect_s3_class(col, "fr_col")
  }
})

test_that("build_default_columns sets id from column name", {
  df <- data.frame(param = "x", value = 1)
  cols <- build_default_columns(df)
  expect_equal(cols$param$id, "param")
  expect_equal(cols$value$id, "value")
})

test_that("build_default_columns auto-detects alignment", {
  df <- data.frame(name = "Alice", score = 95.5, flag = TRUE)
  cols <- build_default_columns(df)
  expect_equal(cols$name$align, "left")
  expect_equal(cols$score$align, "right")
  expect_equal(cols$flag$align, "left")
})

test_that("build_default_columns uses column name as label", {
  df <- data.frame(param = "x")
  cols <- build_default_columns(df)
  expect_equal(cols$param$label, "param")
})

test_that("build_default_columns preserves user-configured columns", {
  df <- data.frame(param = "x", value = 1)
  configured <- list(param = fr_col("Parameter", width = 2.5, align = "left"))
  cols <- build_default_columns(df, configured = configured)

  expect_equal(cols$param$label, "Parameter")
  expect_equal(cols$param$width, 2.5)
  expect_equal(cols$param$id, "param")
  expect_equal(cols$value$label, "value")
  expect_equal(cols$value$align, "right")
})

test_that("build_default_columns returns fr_col objects", {
  df <- data.frame(x = 1)
  cols <- build_default_columns(df)
  expect_s3_class(cols$x, "fr_col")
})


# ══════════════════════════════════════════════════════════════════════════════
# printable_area_inches / printable_area_twips
# ══════════════════════════════════════════════════════════════════════════════

test_that("printable_area_inches returns positive dimensions", {
  page <- new_fr_page()
  area <- printable_area_inches(page)
  expect_named(area, c("width", "height"))
  expect_gt(area[["width"]], 0)
  expect_gt(area[["height"]], 0)
})

test_that("printable_area_inches accounts for margins", {
  page_small <- new_fr_page(margins = 0.5)
  page_large <- new_fr_page(margins = 2)
  area_small <- printable_area_inches(page_small)
  area_large <- printable_area_inches(page_large)
  expect_gt(area_small[["width"]], area_large[["width"]])
  expect_gt(area_small[["height"]], area_large[["height"]])
})

test_that("printable_area_twips is consistent with inches version", {
  page <- new_fr_page()
  area_in <- printable_area_inches(page)
  area_tw <- printable_area_twips(page)
  expect_equal(area_tw[["width"]], inches_to_twips(area_in[["width"]]))
  expect_equal(area_tw[["height"]], inches_to_twips(area_in[["height"]]))
})

test_that("landscape is wider than portrait", {
  landscape <- new_fr_page(orientation = "landscape", margins = 1)
  portrait <- new_fr_page(orientation = "portrait", margins = 1)
  area_l <- printable_area_inches(landscape)
  area_p <- printable_area_inches(portrait)
  expect_gt(area_l[["width"]], area_p[["width"]])
  expect_lt(area_l[["height"]], area_p[["height"]])
})


# ══════════════════════════════════════════════════════════════════════════════
# label_to_plain
# ══════════════════════════════════════════════════════════════════════════════

test_that("label_to_plain passes through plain text", {
  expect_equal(label_to_plain("hello"), "hello")
  expect_equal(label_to_plain(""), "")
})

test_that("label_to_plain strips sentinels", {
  s <- paste0("kg/m", markup_sentinel("SUPER", "2"))
  expect_equal(label_to_plain(s), "kg/m2")
})

test_that("label_to_plain handles non-character input", {
  expect_equal(label_to_plain(123), "123")
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_rows_selector
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_rows_selector finds exact value matches", {
  data <- data.frame(
    group = c("A", "B", "A", "C"),
    val = 1:4,
    stringsAsFactors = FALSE
  )

  selector <- structure(
    list(col = "group", value = "A", pattern = NULL, ignore.case = FALSE),
    class = "fr_rows_selector"
  )
  result <- resolve_rows_selector(selector, data)
  expect_equal(result, c(1L, 3L))
})

test_that("resolve_rows_selector finds regex pattern matches", {
  data <- data.frame(
    label = c("Total", "Subtotal", "Mean", "Total N"),
    stringsAsFactors = FALSE
  )
  selector <- structure(
    list(col = "label", value = NULL, pattern = "^Total", ignore.case = FALSE),
    class = "fr_rows_selector"
  )
  result <- resolve_rows_selector(selector, data)
  expect_equal(result, c(1L, 4L))
})

test_that("resolve_rows_selector respects ignore.case for patterns", {
  data <- data.frame(
    label = c("total", "TOTAL", "Mean"),
    stringsAsFactors = FALSE
  )
  selector <- structure(
    list(col = "label", value = NULL, pattern = "^total$", ignore.case = TRUE),
    class = "fr_rows_selector"
  )
  result <- resolve_rows_selector(selector, data)
  expect_equal(result, c(1L, 2L))
})

test_that("resolve_rows_selector errors when column not found", {
  data <- data.frame(a = 1:3)
  selector <- structure(
    list(col = "missing_col", value = "x", pattern = NULL, ignore.case = FALSE),
    class = "fr_rows_selector"
  )
  expect_error(
    resolve_rows_selector(selector, data),
    class = "rlang_error"
  )
})

test_that("resolve_rows_selector warns when no rows match exact value", {
  data <- data.frame(
    group = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  selector <- structure(
    list(col = "group", value = "Z", pattern = NULL, ignore.case = FALSE),
    class = "fr_rows_selector"
  )
  expect_warning(
    result <- resolve_rows_selector(selector, data),
    "no rows matched"
  )
  expect_equal(result, integer(0))
})

test_that("resolve_rows_selector warns when no rows match pattern", {
  data <- data.frame(
    label = c("Mean", "Median"),
    stringsAsFactors = FALSE
  )
  selector <- structure(
    list(col = "label", value = NULL, pattern = "^Total", ignore.case = FALSE),
    class = "fr_rows_selector"
  )
  expect_warning(
    result <- resolve_rows_selector(selector, data),
    "no rows matched"
  )
  expect_equal(result, integer(0))
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_tidyselect
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_tidyselect resolves column names", {
  data <- data.frame(a = 1, b = 2, c = 3)
  expr <- rlang::quo(a)
  result <- resolve_tidyselect(expr, data)
  expect_equal(result, c(a = 1L))
})

test_that("resolve_tidyselect errors with context on bad expression", {
  data <- data.frame(a = 1)
  expr <- rlang::quo(nonexistent)
  expect_error(
    resolve_tidyselect(expr, data, context = "test selector"),
    "test selector"
  )
})

test_that("resolve_tidyselect errors without context on bad expression", {
  data <- data.frame(a = 1)
  expr <- rlang::quo(nonexistent)
  expect_error(
    resolve_tidyselect(expr, data, context = NULL),
    "tidyselect"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_cols_expr
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_cols_expr returns NULL for null quosure", {
  quo <- rlang::quo(NULL)
  result <- resolve_cols_expr(quo)
  expect_null(result)
})

test_that("resolve_cols_expr returns character vector as-is", {
  quo <- rlang::quo(c("a", "b"))
  result <- resolve_cols_expr(quo)
  expect_equal(result, c("a", "b"))
})

test_that("resolve_cols_expr returns numeric vector as-is", {
  quo <- rlang::quo(c(1, 2, 3))
  result <- resolve_cols_expr(quo)
  expect_equal(result, c(1, 2, 3))
})

test_that("resolve_cols_expr defers tidyselect expressions", {
  # starts_with() needs data context so it errors — should be deferred
  quo <- rlang::quo(starts_with("x"))
  result <- resolve_cols_expr(quo)
  expect_true(rlang::is_quosure(result))
})

test_that("resolve_cols_expr errors on invalid type (logical)", {
  quo <- rlang::quo(TRUE)
  expect_error(
    resolve_cols_expr(quo),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_style_cols
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_style_cols resolves quosure cols against data", {
  data <- data.frame(x_a = 1, x_b = 2, y = 3)
  style <- new_fr_cell_style(cols = rlang::quo(starts_with("x")))
  result <- resolve_style_cols(style, data)
  expect_equal(result$cols, c("x_a", "x_b"))
})

test_that("resolve_style_cols passes through character cols unchanged", {
  data <- data.frame(a = 1, b = 2)
  style <- new_fr_cell_style(cols = c("a", "b"))
  result <- resolve_style_cols(style, data)
  expect_equal(result$cols, c("a", "b"))
})

test_that("resolve_style_cols passes through NULL cols unchanged", {
  data <- data.frame(a = 1)
  style <- new_fr_cell_style(cols = NULL)
  result <- resolve_style_cols(style, data)
  expect_null(result$cols)
})


# ══════════════════════════════════════════════════════════════════════════════
# apply_settings_section
# ══════════════════════════════════════════════════════════════════════════════

test_that("apply_settings_section returns spec unchanged for non-list section", {
  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_settings_section(spec, "not a list", fr_page, "orientation")
  expect_identical(result, spec)
})

test_that("apply_settings_section returns spec unchanged for NULL section", {
  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_settings_section(spec, NULL, fr_page, "orientation")
  expect_identical(result, spec)
})

test_that("apply_settings_section returns spec unchanged when no params match", {
  spec <- new_fr_spec(data.frame(x = 1))
  cfg_section <- list(unrelated_key = "value")
  result <- apply_settings_section(spec, cfg_section, fr_page, "orientation")
  expect_identical(result, spec)
})

test_that("apply_settings_section applies matching parameters via verb", {
  spec <- new_fr_spec(data.frame(x = 1))
  cfg_section <- list(orientation = "portrait")
  result <- apply_settings_section(
    spec,
    cfg_section,
    fr_page,
    c("orientation", "paper")
  )
  expect_equal(result$page$orientation, "portrait")
})


# ══════════════════════════════════════════════════════════════════════════════
# apply_fr_theme
# ══════════════════════════════════════════════════════════════════════════════

test_that("apply_fr_theme returns spec unchanged when no theme is set", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- NULL

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_identical(result, spec)
})

test_that("apply_fr_theme returns spec unchanged when theme is empty list", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list()

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_identical(result, spec)
})

test_that("apply_fr_theme applies page orientation from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(orientation = "portrait")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$page$orientation, "portrait")
})

test_that("apply_fr_theme applies hlines from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(hlines = "header")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_true(length(result$rules) > 0L)
})

test_that("apply_fr_theme applies vlines from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(vlines = "box")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_true(length(result$rules) > 0L)
})

test_that("apply_fr_theme applies space_mode from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(space_mode = "fixed")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$columns_meta$space_mode, "fixed")
})

test_that("apply_fr_theme applies n_format from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(n_format = "(N={n})")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$columns_meta$n_format, "(N={n})")
})

test_that("apply_fr_theme applies split from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(split = TRUE)

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_true(result$columns_meta$split)
})

test_that("apply_fr_theme applies stub from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(stub = "param")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$columns_meta$stub, "param")
})

test_that("apply_fr_theme applies group_keep from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(group_keep = 3L)

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$body$group_keep, 3L)
})

test_that("apply_fr_theme applies footnote_separator from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(footnote_separator = "---")

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$meta$footnote_separator, "---")
})

test_that("apply_fr_theme applies header defaults from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(header = list(bold = TRUE, align = "center"))

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_true(result$header$bold)
  expect_equal(result$header$align, "center")
})

test_that("apply_fr_theme applies header span_gap from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(header = list(span_gap = 2L))

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$header$span_gap, 2L)
})

test_that("apply_fr_theme applies spacing from theme", {
  old_theme <- .arframe_state$theme
  on.exit(.arframe_state$theme <- old_theme, add = TRUE)
  .arframe_state$theme <- list(
    spacing = list(titles_after = 2L, footnotes_before = 3L)
  )

  spec <- new_fr_spec(data.frame(x = 1))
  result <- apply_fr_theme(spec)
  expect_equal(result$spacing$titles_after, 2L)
  expect_equal(result$spacing$footnotes_before, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# visible_columns
# ══════════════════════════════════════════════════════════════════════════════

test_that("visible_columns returns only visible columns", {
  cols <- list(
    a = list(id = "a", visible = TRUE),
    b = list(id = "b", visible = FALSE),
    c = list(id = "c", visible = TRUE)
  )
  result <- visible_columns(cols)
  expect_length(result, 2)
  expect_named(result, c("a", "c"))
})

test_that("visible_columns treats missing visible as TRUE", {
  cols <- list(
    a = list(id = "a"),
    b = list(id = "b", visible = FALSE)
  )
  result <- visible_columns(cols)
  expect_length(result, 1)
  expect_named(result, "a")
})

test_that("visible_columns returns empty list when all hidden", {
  cols <- list(
    a = list(id = "a", visible = FALSE),
    b = list(id = "b", visible = FALSE)
  )
  result <- visible_columns(cols)
  expect_length(result, 0)
})


# ══════════════════════════════════════════════════════════════════════════════
# stub_column_names
# ══════════════════════════════════════════════════════════════════════════════

test_that("stub_column_names returns names of stub columns", {
  cols <- list(
    param = list(id = "param", stub = TRUE),
    value = list(id = "value", stub = FALSE),
    label = list(id = "label", stub = TRUE)
  )
  result <- stub_column_names(cols)
  expect_equal(result, c("param", "label"))
})

test_that("stub_column_names returns empty when no stubs", {
  cols <- list(
    a = list(id = "a", stub = FALSE),
    b = list(id = "b")
  )
  result <- stub_column_names(cols)
  expect_length(result, 0)
})


# ══════════════════════════════════════════════════════════════════════════════
# split_footnotes
# ══════════════════════════════════════════════════════════════════════════════

test_that("split_footnotes separates every and last placements", {
  fn1 <- new_footnote_entry("Note 1", placement = "every")
  fn2 <- new_footnote_entry("Note 2", placement = "last")
  fn3 <- new_footnote_entry("Note 3", placement = "every")

  result <- split_footnotes(list(fn1, fn2, fn3))
  expect_length(result$every, 2)
  expect_length(result$last, 1)
  expect_equal(result$last[[1]]$content, "Note 2")
})

test_that("split_footnotes returns empty lists when no footnotes", {
  result <- split_footnotes(list())
  expect_length(result$every, 0)
  expect_length(result$last, 0)
})

test_that("split_footnotes handles all-every footnotes", {
  fn1 <- new_footnote_entry("A", placement = "every")
  fn2 <- new_footnote_entry("B", placement = "every")
  result <- split_footnotes(list(fn1, fn2))
  expect_length(result$every, 2)
  expect_length(result$last, 0)
})

test_that("split_footnotes handles all-last footnotes", {
  fn1 <- new_footnote_entry("A", placement = "last")
  fn2 <- new_footnote_entry("B", placement = "last")
  result <- split_footnotes(list(fn1, fn2))
  expect_length(result$every, 0)
  expect_length(result$last, 2)
})
