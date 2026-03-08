# ──────────────────────────────────────────────────────────────────────────────
# test-render-rtf.R — Tests for RTF rendering
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_render creates a valid RTF file from minimal pipeline", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2 (10.1)", "50 (55.6%)"),
    stringsAsFactors = FALSE
  )

  data |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  content <- readBin(tmp, "raw", file.info(tmp)$size)
  txt <- rawToChar(content)

  # Valid RTF structure
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(endsWith(trimws(txt), "}"))
})

test_that("fr_render output contains title text", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_titles("Table 14.1.1 Demographics") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Table 14.1.1 Demographics", txt, fixed = TRUE))
})

test_that("fr_render output contains footnote text", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_footnotes("Source: ADSL") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

test_that("fr_render includes \\trhdr in column header row", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\trhdr", txt, fixed = TRUE))
})

test_that("fr_render includes font table", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\fonttbl", txt, fixed = TRUE))
})

test_that("fr_render includes color table", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\colortbl", txt, fixed = TRUE))
})

test_that("fr_render generates \\sect markers for page_by groups", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(page_by = "group") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Two groups = at least one \sect marker
  expect_true(grepl("\\sect", txt, fixed = TRUE))
})

test_that("fr_render applies hline preset borders", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Should contain border control words
  expect_true(grepl("\\clbrdrb", txt, fixed = TRUE))
})

test_that("fr_render handles landscape orientation", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "landscape") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\lndscpsxn", txt, fixed = TRUE))
})

test_that("fr_render handles portrait orientation", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_false(grepl("\\lndscpsxn", txt, fixed = TRUE))
})

test_that("fr_render returns path invisibly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  result <- data |> fr_table() |> fr_render(tmp)
  expect_equal(result, tmp)
})

test_that("detect_format works for rtf extension", {
  expect_equal(detect_format("test.rtf"), "rtf")
  expect_equal(detect_format("output/test.RTF"), "rtf")
})

test_that("detect_format errors on unknown extension", {
  expect_error(detect_format("test.xyz"), "Unsupported")
})

test_that("detect_format errors on missing extension", {
  expect_error(detect_format("noext"), "Cannot detect")
})

test_that("fr_render errors on non-fr_spec input", {
  expect_error(fr_render(list(), "test.rtf"), "fr_spec")
})

test_that("fr_render handles markup in titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("{fr_bold('Table 14.1.1')} Demographics") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Bold sentinel should be resolved to RTF
  expect_true(grepl("\\{\\\\b Table 14.1.1\\}", txt))
  expect_true(grepl("Demographics", txt, fixed = TRUE))
})

test_that("fr_render handles multi-column data correctly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex", "BMI"),
    placebo = c("65.2", "50%", "25.1"),
    active = c("64.8", "48%", "25.3"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_titles("Table 14.1.1") |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Age", txt, fixed = TRUE))
  expect_true(grepl("placebo", txt, fixed = TRUE))
  expect_true(grepl("65.2", txt, fixed = TRUE))
})


test_that("column header newlines render as \\line in RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(
      a = fr_col(label = "Col A\n(N=10)"),
      b = fr_col(label = "Col B"),
      .width = "equal"
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Col A\\line (N=10)", txt, fixed = TRUE))
  expect_false(grepl("Col A\\\\line", txt, fixed = TRUE))
})


test_that("fr_spans adds bottom border to spanned cells by default", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_spans("AB" = c("a", "b")) |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Spanned cells should have bottom border (\clbrdrb)
  expect_true(grepl("clbrdrb.*clmgf", txt))
})


test_that("fr_spans(.hline = FALSE) suppresses spanner bottom border", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_spans("AB" = c("a", "b"), .hline = FALSE) |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Find the spanner row — it has \clmgf (merge first)
  spanner_line <- grep("clmgf", strsplit(txt, "\n")[[1]], value = TRUE)
  expect_true(length(spanner_line) > 0)
  # The spanner row should NOT have \clbrdrb (no bottom border)
  expect_false(grepl("clbrdrb", spanner_line[1]))
})


test_that("column header cells default to bottom vertical alignment", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(
      a = fr_col(label = "Short"),
      b = fr_col(label = "Multi\nLine"),
      .width = "equal"
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Header cells should have \clvertalb (bottom alignment)
  expect_true(grepl("clvertalb", txt, fixed = TRUE))
})


test_that("fr_header(valign = 'top') uses \\clvertalt (RTF default, no keyword needed)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(valign = "top") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Top is RTF default — no \clvertalb or \clvertalc should appear
  expect_false(grepl("clvertalb", txt, fixed = TRUE))
  expect_false(grepl("clvertalc", txt, fixed = TRUE))
})


test_that("fr_col_style(valign = 'middle') emits \\clvertalc in body cells", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", valign = "middle")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Body should have \clvertalc for column "a"
  expect_true(grepl("clvertalc", txt, fixed = TRUE))
})


test_that("fr_row_style(valign = 'bottom') emits \\clvertalb in body cells", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, valign = "bottom")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Body should have \clvertalb for row 1
  expect_true(grepl("clvertalb", txt, fixed = TRUE))
})


test_that("body cells default to valign = 'top' (no \\clvertal keyword)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(valign = "top") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # No vertical alignment keywords — everything is top (RTF default)
  expect_false(grepl("clvertalb", txt, fixed = TRUE))
  expect_false(grepl("clvertalc", txt, fixed = TRUE))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 1+2: Continuation rows use plain text (no field codes)
# ──────────────────────────────────────────────────────────────────────────────

test_that("group_by does not emit continuation rows (no field codes, no trhdr group rows)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(group_by = "grp") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # No field codes
  expect_false(grepl("fldinst", txt, fixed = TRUE))
  # No continuation text — groups are separated by blank rows instead
  expect_false(grepl("continued", txt, fixed = TRUE))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 4: page_by preserves original data order
# ──────────────────────────────────────────────────────────────────────────────

test_that("page_by preserves original data order (not alphabetical)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    visit = c("Week 4", "Week 4", "Baseline", "Baseline"),
    param = c("SBP", "DBP", "SBP", "DBP"),
    val = c("130", "80", "120", "78"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(page_by = "visit") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # "Week 4" should appear BEFORE "Baseline" (original order, not alphabetical)
  pos_week4 <- regexpr("Week 4", txt, fixed = TRUE)
  pos_baseline <- regexpr("Baseline", txt, fixed = TRUE)
  expect_true(pos_week4 < pos_baseline)
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 5: Decimal-aligned cells get reduced cell padding
# ──────────────────────────────────────────────────────────────────────────────

test_that("decimal-aligned cells use sub-cell split with right-before / left-after", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Weight"),
    val = c("65.2", "80.1"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Sub-cell padding
  expect_true(grepl("clpadl36", txt, fixed = TRUE))
  expect_true(grepl("clpadr36", txt, fixed = TRUE))
  # Right-aligned sub-cell for before-part
  expect_true(grepl("\\qr", txt, fixed = TRUE))
  # Left-aligned sub-cell for after-part
  expect_true(grepl("\\ql", txt, fixed = TRUE))
  # No tab-stop patterns
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
  expect_false(grepl("\\tqr", txt, fixed = TRUE))
  # Before/after parts appear in content
  expect_true(grepl("65", txt, fixed = TRUE))
  expect_true(grepl(".2", txt, fixed = TRUE))
})

test_that("decimal sub-cell split works with space-separated content", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    val = c("28 (62%)", "5 (11%)"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Sub-cell split: \qr before, \ql after, no tab stops
  expect_true(grepl("\\qr", txt, fixed = TRUE))
  expect_true(grepl("\\ql", txt, fixed = TRUE))
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
})

test_that("decimal sub-cell split works with dash-separated content", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    val = c("41-82", "55-99"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Sub-cell split with no tab stops
  expect_true(grepl("\\qr", txt, fixed = TRUE))
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
})


# ── Spacing tests ────────────────────────────────────────────────────────────

test_that("fr_spacing(titles_after = 0) suppresses blank line after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_spacing(titles_after = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Titles are now \trhdr rows. Count \trhdr rows containing title text only.
  # With titles_after = 0: 1 title row, no blank spacer rows.
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  # 1 title row + 1 column header row = 2 \trhdr total (no blank spacer rows)
  expect_equal(n_trhdr, 2L)
})

test_that("fr_spacing(titles_after = 2) inserts two blank lines after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_spacing(titles_after = 2L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # 1 title row + 2 blank spacer rows + 1 column header row = 4 \trhdr
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  expect_equal(n_trhdr, 4L)
})

test_that("fr_spacing(footnotes_before = 0) suppresses blank row before footnotes", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_footnotes("[a] Test footnote.", .separator = FALSE) |>
    fr_spacing(footnotes_before = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Footnote text should be present (as body rows for single-page tables)
  expect_true(grepl("\\[a\\] Test footnote\\.", txt, fixed = FALSE))
})

test_that("'last' footnotes appear in final section only", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = 1:4,
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(page_by = "grp") |>
    fr_footnotes(
      list("Every page note", placement = "every"),
      list("Last only note", placement = "last")
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Split on \pard\par\n\sect\n (the actual page break marker between sections)
  sections <- strsplit(txt, "\\pard\\par\n\\sect\n", fixed = TRUE)[[1L]]
  # Should have 2 sections (preamble+section1 before \sect, section2 after)
  expect_true(length(sections) >= 2L)

  # "Last only note" should NOT appear in the first section
  expect_false(grepl("Last only note", sections[1L], fixed = TRUE))
  # "Last only note" SHOULD appear in the final section
  # (as body rows for single-page tables, or in {\footer} for multi-page)
  last_section <- sections[length(sections)]
  expect_true(grepl("Last only note", last_section, fixed = TRUE))

  # "Every page note" should appear in both sections
  expect_true(grepl("Every page note", sections[1L], fixed = TRUE))
  expect_true(grepl("Every page note", last_section, fixed = TRUE))
})

# ── Single-page footnotes in body ────────────────────────────────────────────

test_that("single-page table renders footnotes in body (not footer)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2", "50"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_footnotes("Source: ADSL") |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Footnote text should appear as body table rows (with \trowd, not \trhdr)
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))

  # Footer group should NOT contain footnote text (single-page mode)
  footer_match <- regmatches(txt, gregexpr("\\{\\\\footer[^}]*\\}", txt))[[1L]]
  if (length(footer_match) > 0L) {
    for (fm in footer_match) {
      expect_false(grepl("Source: ADSL", fm, fixed = TRUE))
    }
  }
})

test_that("default spacing inserts 1 blank line after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # 1 title row + 1 blank spacer row + 1 column header row = 3 \trhdr
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  expect_equal(n_trhdr, 3L)
})
