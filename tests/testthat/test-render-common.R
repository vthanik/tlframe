# ──────────────────────────────────────────────────────────────────────────────
# test-render-common.R — Tests for shared rendering utilities
# ──────────────────────────────────────────────────────────────────────────────

test_that("rtf_escape handles special characters", {
  expect_equal(rtf_escape("hello"), "hello")
  expect_equal(rtf_escape("a\\b"), "a\\\\b")
  expect_equal(rtf_escape("a{b}c"), "a\\{b\\}c")
  expect_equal(rtf_escape("\\{\\}"), "\\\\\\{\\\\\\}")
})

test_that("rtf_escape handles empty and NA input", {
  expect_equal(rtf_escape(""), "")
  expect_equal(rtf_escape(character(0)), character(0))
})

test_that("rtf_escape converts non-ASCII to Unicode escapes", {
  result <- rtf_escape("\u00b1")
  expect_true(grepl("\\\\u", result))
})

test_that("rtf_sentinel_resolver handles all types", {
  expect_equal(rtf_sentinel_resolver("SUPER", "a"), "{\\super a}")
  expect_equal(rtf_sentinel_resolver("SUB", "x"), "{\\sub x}")
  expect_equal(rtf_sentinel_resolver("BOLD", "hi"), "{\\b hi}")
  expect_equal(rtf_sentinel_resolver("ITALIC", "em"), "{\\i em}")
  expect_equal(rtf_sentinel_resolver("UNDERLINE", "u"), "{\\ul u}")
  expect_equal(rtf_sentinel_resolver("NEWLINE", ""), "\\line ")
})

test_that("rtf_escape_and_resolve handles mixed text and sentinels", {
  # Plain text — no sentinels

  expect_equal(rtf_escape_and_resolve("hello"), "hello")

  # Text with special chars
  expect_equal(rtf_escape_and_resolve("a\\b"), "a\\\\b")

  # Text with sentinel
  sentinel <- paste0("\x01SUPER:1\x02")
  result <- rtf_escape_and_resolve(sentinel)
  expect_equal(result, "{\\super 1}")

  # Mixed: text + sentinel + text
  mixed <- paste0("hello \x01BOLD:world\x02 end")
  result <- rtf_escape_and_resolve(mixed)
  expect_true(grepl("hello", result))
  expect_true(grepl("\\{\\\\b world\\}", result))
  expect_true(grepl("end", result))
})

test_that("build_cell_grid produces correct dimensions", {
  data <- data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1, align = "left"),
    b = fr_col("B", width = 1, align = "right")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  expect_equal(nrow(grid), 4L)  # 2 rows x 2 cols
  expect_equal(ncol(grid), 13L) # row_idx, col_idx, col_name, content, align, valign, ...
  expect_equal(grid$content, c("x", "y", "1", "2"))
})

test_that("build_cell_grid handles empty data", {
  data <- data.frame(a = character(0), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 1, align = "left"))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  expect_equal(nrow(grid), 0L)
})

test_that("resolve_borders maps hline presets correctly", {
  # Header preset: bottom border on header
  rules <- list(
    new_fr_rule(direction = "horizontal", region = "header",
                side = "below", width = 0.5, linestyle = "solid",
                fg = "#000000")
  )

  borders <- resolve_borders(rules, nrow_body = 3L, ncol = 2L, nrow_header = 1L)

  # Header bottom border should be set
  expect_false(is.null(borders$header$bottom[1, 1][[1]]))
  expect_equal(borders$header$bottom[1, 1][[1]]$width, 0.5)

  # Body top border should NOT be set (just header bottom)
  expect_true(is.null(borders$body$top[1, 1][[1]]))
})

test_that("resolve_borders handles box preset", {
  rules <- list(structure(list(preset = "box"), class = "fr_rule_box"))

  borders <- resolve_borders(rules, nrow_body = 2L, ncol = 3L, nrow_header = 1L)

  # Top of header: should be set

  expect_false(is.null(borders$header$top[1, 1][[1]]))

  # Bottom of last body row: should be set
  expect_false(is.null(borders$body$bottom[2, 1][[1]]))

  # Left edge of header: should be set
  expect_false(is.null(borders$header$left[1, 1][[1]]))

  # Right edge of header: should be set
  expect_false(is.null(borders$header$right[1, 3][[1]]))
})

test_that("build_rtf_colortbl produces valid RTF", {
  colors <- c("#000000", "#FF0000")
  result <- build_rtf_colortbl(colors)

  expect_true(grepl("^\\{\\\\colortbl;", result$rtf))
  expect_true(grepl("\\}$", result$rtf))
  expect_true(grepl("\\\\red0\\\\green0\\\\blue0", result$rtf))
  expect_true(grepl("\\\\red255\\\\green0\\\\blue0", result$rtf))

  expect_equal(result$index[["#000000"]], 1L)
  expect_equal(result$index[["#FF0000"]], 2L)
})

test_that("collect_colors finds colors from rules and styles", {
  spec <- new_fr_spec(data.frame(x = 1))
  spec$rules <- list(
    new_fr_rule(fg = "#FF0000")
  )
  spec$cell_styles <- list(
    new_fr_cell_style(fg = "#00FF00", bg = "#0000FF")
  )

  colors <- collect_colors(spec)
  expect_true("#000000" %in% colors)
  expect_true("#FF0000" %in% colors)
  expect_true("#00FF00" %in% colors)
  expect_true("#0000FF" %in% colors)
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by auto-implies blank_after (no double blanks)
# ══════════════════════════════════════════════════════════════════════════════

test_that("group_by + blank_after on same column produces single blank rows", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )
  spec <- df |> fr_table() |>
    fr_rows(group_by = "grp", blank_after = "grp")
  spec <- finalize_spec(spec)
  # Should have 4 data rows + 1 blank row (between A and B)
  expect_equal(nrow(spec$data), 5L)
  # Row 3 should be the blank row (all empty)
  expect_true(all(spec$data[3L, ] == ""))
})

test_that("group_by alone does NOT auto-insert blank rows", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )
  spec <- df |> fr_table() |>
    fr_rows(group_by = "grp")
  spec <- finalize_spec(spec)
  # group_by auto-implies blank_after — a blank row is inserted between groups
  # 4 original rows + 1 blank row between groups A and B = 5
  expect_equal(nrow(spec$data), 5L)
})



test_that("split_at_decimal splits at dot", {
  res <- split_at_decimal("168.0")
  expect_equal(res$before, "168")
  expect_equal(res$after, ".0")
})

test_that("split_at_decimal splits at space when no dot", {
  res <- split_at_decimal("28 (62%)")
  expect_equal(res$before, "28")
  expect_equal(res$after, " (62%)")
})

test_that("split_at_decimal splits at dash (not leading)", {
  res <- split_at_decimal("41-82")
  expect_equal(res$before, "41")
  expect_equal(res$after, "-82")
})

test_that("split_at_decimal preserves leading negative", {
  # Leading dash should NOT split — whole string stays as "before"
  res <- split_at_decimal("-3")
  expect_equal(res$before, "-3")
  expect_equal(res$after, "")
})

test_that("split_at_decimal prefers dot over dash", {
  res <- split_at_decimal("-3.2")
  expect_equal(res$before, "-3")
  expect_equal(res$after, ".2")
})

test_that("split_at_decimal prefers space when dot is after space (pct pattern)", {
  # "28 (62.2%)" — dot at pos 7, space at pos 3 → dot is inside parens
  res <- split_at_decimal("28 (62.2%)")
  expect_equal(res$before, "28")
  expect_equal(res$after, " (62.2%)")
})

test_that("split_at_decimal prefers dot when dot is before space", {
  # "62.3 (10.14)" — dot at pos 3, space at pos 5 → dot is the decimal
  res <- split_at_decimal("62.3 (10.14)")
  expect_equal(res$before, "62")
  expect_equal(res$after, ".3 (10.14)")
})

test_that("split_at_decimal handles CI pattern (dot before space)", {
  # "0.8 (-1.2, 2.8)" — dot at pos 2, space at pos 4 → standard decimal
  res <- split_at_decimal("0.8 (-1.2, 2.8)")
  expect_equal(res$before, "0")
  expect_equal(res$after, ".8 (-1.2, 2.8)")
})

test_that("compute_decimal_before_twips returns positive width from content", {
  data <- data.frame(
    a = c("168.0", "45", "1"),
    stringsAsFactors = FALSE
  )
  cols <- list(a = fr_col("A", width = 1.0, align = "decimal"))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  result <- compute_decimal_before_twips(data, cols, grid, page$font_family, page$font_size)

  # Should be positive and equal to the width of the widest "before" part ("168")
  expect_true(result[["a"]] > 0L)
  before_w <- measure_text_width_twips("168", page$font_family, page$font_size)
  expect_equal(result[["a"]], as.integer(before_w))
})


test_that("compute_decimal_before_twips returns NA for non-decimal columns", {
  data <- data.frame(a = "hello", b = "1.5", stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1.0, align = "left"),
    b = fr_col("B", width = 1.0, align = "decimal")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  result <- compute_decimal_before_twips(data, cols, grid, page$font_family, page$font_size)

  expect_true(is.na(result[["a"]]))
  expect_false(is.na(result[["b"]]))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 3: Inline markup in data cell content
# ──────────────────────────────────────────────────────────────────────────────

test_that("build_cell_grid evaluates inline markup in cell content", {
  data <- data.frame(
    a = c("Age {fr_super('a')}", "Weight"),
    b = c("65.2", "80.1"),
    stringsAsFactors = FALSE
  )
  cols <- list(
    a = fr_col("A", width = 1.5),
    b = fr_col("B", width = 1.0)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)

  # The {fr_super('a')} should be resolved to a sentinel token
  age_content <- grid$content[grid$row_idx == 1L & grid$col_name == "a"]
  expect_true(grepl("\x01SUPER:a\x02", age_content))
  # Plain text stays plain
  weight_content <- grid$content[grid$row_idx == 2L & grid$col_name == "a"]
  expect_equal(weight_content, "Weight")
})


# ══════════════════════════════════════════════════════════════════════════════
# inject_align_gaps
# ══════════════════════════════════════════════════════════════════════════════

test_that("inject_align_gaps inserts gap between right→left adjacent columns", {
  df <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  spec <- df |> fr_table() |>
    fr_cols(a = fr_col("A", align = "right"),
            b = fr_col("B", align = "left"))
  spec <- finalize_spec(spec)
  col_names <- names(spec$columns)
  gap_cols <- col_names[grepl("__align_gap_", col_names)]
  expect_length(gap_cols, 1L)
  expect_true(spec$columns[[gap_cols]]$is_gap)
})

test_that("inject_align_gaps does NOT insert gap for left→left columns", {
  df <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  spec <- df |> fr_table() |>
    fr_cols(a = fr_col("A", align = "left"),
            b = fr_col("B", align = "left"))
  spec <- finalize_spec(spec)
  col_names <- names(spec$columns)
  gap_cols <- col_names[grepl("__align_gap_", col_names)]
  expect_length(gap_cols, 0L)
})

test_that("inject_align_gaps respects align_gap = FALSE", {
  df <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  spec <- df |> fr_table() |>
    fr_cols(a = fr_col("A", align = "right"),
            b = fr_col("B", align = "left")) |>
    fr_header(align_gap = FALSE)
  spec <- finalize_spec(spec)
  col_names <- names(spec$columns)
  gap_cols <- col_names[grepl("__align_gap_", col_names)]
  expect_length(gap_cols, 0L)
})
