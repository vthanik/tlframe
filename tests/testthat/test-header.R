# ──────────────────────────────────────────────────────────────────────────────
# test-header.R — Tests for fr_header() and fr_cols()
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_header sets align, valign, bold", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(align = "center", valign = "top", bold = FALSE)

  expect_equal(spec$header$align, "center")
  expect_equal(spec$header$valign, "top")
  expect_false(spec$header$bold)
})

test_that("fr_header preserves existing spans", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Treatment" = c("zom_50mg", "zom_100mg")) |>
    fr_header(align = "center")

  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$align, "center")
})

test_that("fr_header stores n and format", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{label}\n(N={n})"
    )

  expect_equal(spec$header$n, c(zom_50mg = 45, placebo = 45))
  expect_equal(spec$header$format, "{label}\n(N={n})")
})

test_that("fr_header n-count labels resolve during finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo = fr_col("Placebo", align = "right")
    ) |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{label}\n(N={n})"
    )

  # finalize_spec resolves the labels
  fspec <- tlframe:::finalize_spec(spec)

  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg\n(N=45)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("fr_header and fr_cols are order-independent", {
  # fr_header first, then fr_cols
  spec1 <- tbl_demog |>
    fr_table() |>
    fr_header(n = c(zom_50mg = 45), format = "{label}\n(N={n})") |>
    fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right"))

  # fr_cols first, then fr_header
  spec2 <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right")) |>
    fr_header(n = c(zom_50mg = 45), format = "{label}\n(N={n})")

  fspec1 <- tlframe:::finalize_spec(spec1)
  fspec2 <- tlframe:::finalize_spec(spec2)

  expect_equal(fspec1$columns$zom_50mg$label, "Zom 50mg\n(N=45)")
  expect_equal(fspec2$columns$zom_50mg$label, "Zom 50mg\n(N=45)")
})

test_that("fr_header sets bg and fg", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(bg = "#E0E0E0", fg = "navy")

  expect_equal(spec$header$bg, "#E0E0E0")
  expect_equal(spec$header$fg, "#000080")
})

test_that("fr_header sets font_size", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(font_size = 8)

  expect_equal(spec$header$font_size, 8)
})

test_that("fr_header sets repeat_on_page", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(repeat_on_page = FALSE)

  expect_false(spec$header$repeat_on_page)
})

test_that("fr_header validates inputs", {
  spec <- tbl_demog |> fr_table()

  expect_error(fr_header(spec, n = c(1, 2)), "named numeric")
  expect_error(fr_header(spec, n = TRUE), "named numeric|data frame")
  expect_error(fr_header(spec, bold = "yes"), "TRUE.*FALSE")
  expect_error(fr_header(spec, font_size = -1), "positive")

  # character scalar is not a valid n form
  expect_error(fr_header(spec, n = "TRTA"), "named numeric")

  # function is no longer a valid n form
  expect_error(fr_header(spec, n = function(d) d), "named numeric")

  # list n must be named
  expect_error(fr_header(spec, n = list(c(a = 1))), "named by page_by")

  # list n elements must be named numeric
  expect_error(fr_header(spec, n = list(grp = c(1, 2))), "named numeric")
})

test_that("fr_header align propagates to columns via finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(align = "center")

  fspec <- tlframe:::finalize_spec(spec)

  # All columns without per-column header_align should get "center"
  for (nm in names(fspec$columns)) {
    expect_equal(fspec$columns[[nm]]$header_align, "center")
  }
})

test_that("per-column header_align overrides fr_header align", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("Zom", header_align = "right")) |>
    fr_header(align = "center")

  fspec <- tlframe:::finalize_spec(spec)

  # zom_50mg has per-column override

  expect_equal(fspec$columns$zom_50mg$header_align, "right")
  # Others get fr_header default
  expect_equal(fspec$columns$characteristic$header_align, "center")
})

test_that("header_cfg flows to build_header_cell_grid", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(.width = "auto") |>
    fr_header(bold = FALSE, bg = "#E0E0E0", font_size = 8)

  fspec <- tlframe:::finalize_spec(spec)
  vis_cols <- Filter(function(c) !isFALSE(c$visible), fspec$columns)

  h_row <- 1L + length(fspec$header$spans)
  hgrid <- tlframe:::build_header_cell_grid(
    vis_cols, fspec$cell_styles, fspec$page, h_row,
    default_valign = "bottom",
    header_cfg = fspec$header
  )

  expect_false(any(hgrid$bold))
  expect_true(all(hgrid$bg == "#E0E0E0"))
  expect_true(all(hgrid$font_size == 8))
})


# ──────────────────────────────────────────────────────────────────────────────
# Per-group N-count tests
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_header stores per-group list n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list(
        "Group A" = c(zom_50mg = 42, placebo = 40),
        "Group B" = c(zom_50mg = 45, placebo = 44)
      ),
      format = "{label}\n(N={n})"
    )

  expect_true(is.list(spec$header$n))
  expect_length(spec$header$n, 2L)
  expect_equal(spec$header$n[["Group A"]], c(zom_50mg = 42, placebo = 40))
})

test_that("fr_header stores data frame n", {
  df <- data.frame(trt = c("Zom", "Placebo"), n = c(42L, 40L))
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(n = df, format = "{label}\n(N={n})")

  expect_true(is.data.frame(spec$header$n))
})

test_that("per-group list n skips global resolution in finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo  = fr_col("Placebo", align = "right")
    ) |>
    fr_header(
      n = list("Group A" = c(zom_50mg = 42, placebo = 40)),
      format = "{label}\n(N={n})"
    )

  # finalize_spec should NOT resolve labels for per-group n
  suppressWarnings(fspec <- tlframe:::finalize_spec(spec))

  # Labels should remain unmodified (base labels, no N appended)
  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg")
  expect_equal(fspec$columns$placebo$label, "Placebo")
})

test_that("resolve_group_labels returns overrides for per-group list", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo  = fr_col("Placebo")
    ) |>
    fr_header(
      n = list(
        "Group A" = c(zom_50mg = 42, placebo = 40),
        "Group B" = c(zom_50mg = 45, placebo = 44)
      ),
      format = "{label}\n(N={n})"
    )

  fspec <- suppressWarnings(tlframe:::finalize_spec(spec))

  res_a <- tlframe:::resolve_group_labels(fspec, fspec$data, "Group A")
  ov_a <- res_a$columns
  expect_equal(ov_a[["zom_50mg"]], "Zomerane 50 mg\n(N=42)")
  expect_equal(ov_a[["placebo"]], "Placebo\n(N=40)")

  res_b <- tlframe:::resolve_group_labels(fspec, fspec$data, "Group B")
  ov_b <- res_b$columns
  expect_equal(ov_b[["zom_50mg"]], "Zomerane 50 mg\n(N=45)")
  expect_equal(ov_b[["placebo"]], "Placebo\n(N=44)")
})

test_that("resolve_group_labels returns NULL for missing group key", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list("Group A" = c(zom_50mg = 42)),
      format = "{label}\n(N={n})"
    )

  fspec <- suppressWarnings(tlframe:::finalize_spec(spec))
  ov <- tlframe:::resolve_group_labels(fspec, fspec$data, "Missing Group")
  expect_null(ov)
})

test_that("resolve_group_labels returns NULL for global numeric n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{label}\n(N={n})"
    )

  fspec <- tlframe:::finalize_spec(spec)
  ov <- tlframe:::resolve_group_labels(fspec, fspec$data, NULL)
  expect_null(ov)
})


test_that("per-group list n without page_by emits warning", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list("A" = c(zom_50mg = 42)),
      format = "{label}\n(N={n})"
    )

  expect_warning(tlframe:::finalize_spec(spec), "page_by")
})

test_that("{label} token works in format string", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("Zom")) |>
    fr_header(
      n = c(zom_50mg = 45),
      format = "{label}\n(N={n})"
    )

  fspec <- tlframe:::finalize_spec(spec)
  expect_equal(fspec$columns$zom_50mg$label, "Zom\n(N=45)")
})


# ══════════════════════════════════════════════════════════════════════════════
# parse_df_n_counts() tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("parse_df_n_counts handles 2-col data frame", {
  spec <- tbl_demog |> fr_table()
  df <- data.frame(trt = c("A", "B"), n = c(10L, 20L))
  result <- tlframe:::parse_df_n_counts(df, spec)
  expect_equal(result$type, "global")
  expect_equal(result$counts, c(A = 10L, B = 20L))
})

test_that("parse_df_n_counts handles 3-col data frame", {
  spec <- tbl_demog |> fr_table()
  df <- data.frame(
    grp = c("G1", "G1", "G2"),
    trt = c("A", "B", "A"),
    n   = c(10L, 20L, 30L)
  )
  result <- tlframe:::parse_df_n_counts(df, spec)
  expect_equal(result$type, "per_group")
  expect_equal(result$page_col, 1L)
  expect_equal(result$trt_col, 2L)
  expect_equal(result$count_col, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# match_trt_to_columns() tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_trt_to_columns matches case-insensitively to labels", {
  columns <- list(
    col_a = fr_col("Placebo"),
    col_b = fr_col("Zomerane 50mg")
  )
  counts <- c("placebo" = 45L, "zomerane 50mg" = 44L)
  result <- tlframe:::match_trt_to_columns(counts, columns)
  expect_equal(result[["col_a"]], 45L)
  expect_equal(result[["col_b"]], 44L)
})

test_that("match_trt_to_columns skips unmatched treatments", {
  columns <- list(col_a = fr_col("Placebo"))
  counts <- c("Placebo" = 45L, "Unknown" = 99L)
  result <- tlframe:::match_trt_to_columns(counts, columns)
  expect_equal(length(result), 1L)
  expect_equal(result[["col_a"]], 45L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans — .gap parameter
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spans .gap sets span_gap on header", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg"), .gap = FALSE)

  expect_false(spec$header$span_gap)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans — tidyselect integration
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spans with starts_with() resolves columns", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = starts_with("zom_"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, c("zom_50mg", "zom_100mg"))
})

test_that("fr_spans with contains() resolves columns", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("50 mg Group" = contains("50"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, "zom_50mg")
})

test_that("fr_spans with character vector still works (no regression)", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, c("zom_50mg", "zom_100mg"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Direct data frame N-count tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_n_param accepts 2-col data frame", {
  df <- data.frame(trt = c("A", "B"), n = c(10L, 20L))
  expect_no_error(tlframe:::validate_n_param(df))
})

test_that("validate_n_param accepts 3-col data frame", {
  df <- data.frame(grp = "G1", trt = "A", n = 10L)
  expect_no_error(tlframe:::validate_n_param(df))
})

test_that("validate_n_param rejects 1-col data frame", {
  df <- data.frame(n = c(10L, 20L))
  expect_error(tlframe:::validate_n_param(df), "2 or 3 columns")
})

test_that("validate_n_param rejects 4-col data frame", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)
  expect_error(tlframe:::validate_n_param(df), "2 or 3 columns")
})

test_that("validate_n_param rejects non-numeric last column", {
  df <- data.frame(trt = "A", n = "ten")
  expect_error(tlframe:::validate_n_param(df), "numeric")
})

test_that("2-col data frame resolves globally via finalize_spec", {
  df <- data.frame(
    trt = c("Zom", "Placebo"),
    n   = c(99L, 88L)
  )

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom"),
      placebo  = fr_col("Placebo")
    ) |>
    fr_header(n = df, format = "{label}\n(N={n})")

  fspec <- tlframe:::finalize_spec(spec)
  expect_equal(fspec$columns$zom_50mg$label, "Zom\n(N=99)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=88)")
})

test_that("3-col data frame resolves per-group via resolve_group_labels", {
  df <- data.frame(
    param = c("G1", "G1", "G2", "G2"),
    trt   = c("Col A", "Col B", "Col A", "Col B"),
    n     = c(10L, 20L, 30L, 40L)
  )

  display <- data.frame(
    grp   = c("G1", "G2"),
    col_a = c("x", "y"),
    col_b = c("x", "y"),
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_cols(
      col_a = fr_col("Col A"),
      col_b = fr_col("Col B")
    ) |>
    fr_header(n = df, format = "{label}\n(N={n})")

  fspec <- tlframe:::finalize_spec(spec)

  # Labels NOT resolved globally (3-col → per-group)
  expect_equal(fspec$columns$col_a$label, "Col A")
  expect_equal(fspec$columns$col_b$label, "Col B")

  # Per-group resolution
  res_g1 <- tlframe:::resolve_group_labels(fspec, display[1, ], "G1")
  expect_equal(res_g1$columns[["col_a"]], "Col A\n(N=10)")
  expect_equal(res_g1$columns[["col_b"]], "Col B\n(N=20)")

  res_g2 <- tlframe:::resolve_group_labels(fspec, display[2, ], "G2")
  expect_equal(res_g2$columns[["col_a"]], "Col A\n(N=30)")
  expect_equal(res_g2$columns[["col_b"]], "Col B\n(N=40)")
})

test_that("3-col data frame without page_by emits warning", {
  df <- data.frame(grp = "G1", trt = "A", n = 10L)

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("A")) |>
    fr_header(n = df, format = "{label}\n(N={n})")

  expect_warning(tlframe:::finalize_spec(spec), "page_by")
})

test_that("2-col data frame matches spanner labels", {
  df <- data.frame(
    trt = c("Zomerane", "Placebo"),
    n   = c(89L, 45L)
  )

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg  = fr_col("Zom 50mg"),
      zom_100mg = fr_col("Zom 100mg"),
      placebo   = fr_col("Placebo")
    ) |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg")) |>
    fr_header(n = df, format = "{label}\n(N={n})")

  fspec <- tlframe:::finalize_spec(spec)

  # Spanner label should be overridden
  expect_equal(fspec$header$spans[[1]]$label, "Zomerane\n(N=89)")
  # Column matched
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("unmatched page_by group warns in resolve_group_labels", {
  df <- data.frame(
    param = c("G1", "G1"),
    trt   = c("Col A", "Col B"),
    n     = c(10L, 20L)
  )

  display <- data.frame(
    grp   = c("G1", "G2"),
    col_a = c("x", "y"),
    col_b = c("x", "y"),
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_cols(col_a = fr_col("Col A"), col_b = fr_col("Col B")) |>
    fr_header(n = df, format = "{label}\n(N={n})")

  fspec <- tlframe:::finalize_spec(spec)

  # G2 not in df → should warn
  expect_warning(
    tlframe:::resolve_group_labels(fspec, display[2, ], "G2"),
    "No N-counts found"
  )
})

