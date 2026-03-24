# ─────────────────────────────────────────────────────────────────────────────
# test-api-rows.R — Tests for fr_rows() validation and error-handling paths
# ─────────────────────────────────────────────────────────────────────────────

# Shared fixtures
df_ae <- data.frame(
  AEBODSYS = c("Nervous system disorders", "Gastrointestinal disorders"),
  AEDECOD = c("Headache", "Nausea"),
  n = c(10L, 15L),
  stringsAsFactors = FALSE
)

df_hier <- data.frame(
  soc = c("GI disorders", "GI disorders", "GI disorders"),
  term = c("GI disorders", "GI signs", "Nausea"),
  row_type = c("soc", "hlt", "pt"),
  result = c("72 (53.3)", "54 (40.0)", "24 (17.8)"),
  stringsAsFactors = FALSE
)

df_group <- data.frame(
  group = c("Sex", "Sex", "Age", "Age"),
  stat = c("Female", "Male", "Mean", "Median"),
  value = c("27", "18", "75.0", "74.0"),
  stringsAsFactors = FALSE
)


# ══════════════════════════════════════════════════════════════════════════════
# validate_cols errors — non-character input
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when sort_by is non-character", {
  expect_error(
    fr_table(df_ae) |> fr_rows(sort_by = 123),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when page_by is non-character (numeric)", {
  expect_error(
    fr_table(df_ae) |> fr_rows(page_by = 42L),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when blank_after is non-character", {
  expect_error(
    fr_table(df_ae) |> fr_rows(blank_after = TRUE),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when suppress is non-character", {
  expect_error(
    fr_table(df_ae) |> fr_rows(suppress = list(1, 2)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_indent_by — list form (valid)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows accepts indent_by as named list with key/col/levels", {
  spec <- fr_table(df_hier) |>
    fr_rows(
      group_by = "soc",
      indent_by = list(
        key = "row_type",
        col = "term",
        levels = c(soc = 0, hlt = 1, pt = 2)
      )
    )
  expect_s3_class(spec$body, "fr_body")
  expect_true(is.list(spec$body$indent_by))
  expect_equal(spec$body$indent_by$key, "row_type")
  expect_equal(spec$body$indent_by$col, "term")
  expect_equal(spec$body$indent_by$levels, c(soc = 0, hlt = 1, pt = 2))
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_indent_by — list form errors (missing required elements)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when indent_by list is missing 'key'", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(
        group_by = "soc",
        indent_by = list(col = "term", levels = c(soc = 0))
      ),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when indent_by list is missing 'col'", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(
        group_by = "soc",
        indent_by = list(key = "row_type", levels = c(soc = 0))
      ),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when indent_by list is missing 'levels'", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(
        group_by = "soc",
        indent_by = list(key = "row_type", col = "term")
      ),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when indent_by list is missing all required elements", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(group_by = "soc", indent_by = list(foo = "bar")),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_indent_by$levels — non-numeric or unnamed
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when indent_by$levels is non-numeric", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(
        group_by = "soc",
        indent_by = list(
          key = "row_type",
          col = "term",
          levels = c(soc = "zero", hlt = "one")
        )
      ),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when indent_by$levels is unnamed numeric", {
  expect_error(
    fr_table(df_hier) |>
      fr_rows(
        group_by = "soc",
        indent_by = list(
          key = "row_type",
          col = "term",
          levels = c(0, 1, 2)
        )
      ),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_indent_by — wrong type (not character or list)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when indent_by is numeric", {
  expect_error(
    fr_table(df_ae) |> fr_rows(indent_by = 42),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when indent_by is logical", {
  expect_error(
    fr_table(df_ae) |> fr_rows(indent_by = TRUE),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by list form — with label
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows accepts group_by list form with label", {
  spec <- fr_table(df_group) |>
    fr_rows(group_by = list(cols = "group", label = "stat"))
  expect_s3_class(spec$body, "fr_body")
  expect_equal(spec$body$group_by, "group")
  expect_equal(spec$body$group_label, "stat")
})

test_that("group_by list with label auto-infers indent_by", {
  spec <- fr_table(df_group) |>
    fr_rows(group_by = list(cols = "group", label = "stat"))
  # indent_by is auto-inferred from group_label when not set

  expect_equal(spec$body$indent_by, "stat")
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by list form — with leaf
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows accepts group_by list form with leaf", {
  df <- data.frame(
    soc = c("GI", "GI"),
    pt = c("Nausea", "Vomiting"),
    n = c(10L, 5L),
    stringsAsFactors = FALSE
  )
  spec <- fr_table(df) |>
    fr_rows(group_by = list(cols = c("soc", "pt"), leaf = "pt"))
  expect_equal(spec$body$group_leaf, "pt")
  # collapse_hierarchy transforms multi-col group_by into single-col
  # with __display__ and __row_level__ columns
  expect_equal(spec$body$group_by, "soc")
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by leaf not in cols — error
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when group_by$leaf is not in group_by$cols", {
  df <- data.frame(
    soc = c("GI", "GI"),
    pt = c("Nausea", "Vomiting"),
    n = c(10L, 5L),
    stringsAsFactors = FALSE
  )
  expect_error(
    fr_table(df) |>
      fr_rows(group_by = list(cols = "soc", leaf = "pt")),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by invalid type — error
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when group_by is numeric", {
  expect_error(
    fr_table(df_ae) |> fr_rows(group_by = 99),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when group_by is logical", {
  expect_error(
    fr_table(df_ae) |> fr_rows(group_by = TRUE),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when group_by is list without 'cols' element", {
  expect_error(
    fr_table(df_ae) |> fr_rows(group_by = list(label = "AEDECOD")),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Multi-level indent_by without group_by — warning
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows warns when multi-level indent_by is used without group_by", {
  expect_warning(
    fr_table(df_hier) |>
      fr_rows(
        indent_by = list(
          key = "row_type",
          col = "term",
          levels = c(soc = 0, hlt = 1, pt = 2)
        )
      ),
    "group_by"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# group_keep and wrap validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows errors when group_keep is non-logical", {
  expect_error(
    fr_table(df_ae) |> fr_rows(group_keep = "yes"),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when wrap is non-logical", {
  expect_error(
    fr_table(df_ae) |> fr_rows(wrap = "yes"),
    class = "rlang_error"
  )
})

test_that("fr_rows accepts group_keep = FALSE", {
  spec <- fr_table(df_ae) |>
    fr_rows(group_by = "AEBODSYS", group_keep = FALSE)
  expect_false(spec$body$group_keep)
})

test_that("fr_rows accepts wrap = TRUE", {
  spec <- fr_table(df_ae) |> fr_rows(wrap = TRUE)
  expect_true(spec$body$wrap)
})


# ══════════════════════════════════════════════════════════════════════════════
# blank_after parameter
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows sets blank_after from character column name", {
  spec <- fr_table(df_ae) |> fr_rows(blank_after = "AEBODSYS")
  expect_equal(spec$body$blank_after, "AEBODSYS")
})


# ══════════════════════════════════════════════════════════════════════════════
# page_by parameter — list form
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows sets page_by from character", {
  spec <- fr_table(df_ae) |> fr_rows(page_by = "AEBODSYS")
  expect_equal(spec$body$page_by, "AEBODSYS")
  expect_true(spec$body$page_by_visible)
})

test_that("fr_rows accepts page_by list form with visible = FALSE", {
  spec <- fr_table(df_ae) |>
    fr_rows(page_by = list(cols = "AEBODSYS", visible = FALSE))
  expect_equal(spec$body$page_by, "AEBODSYS")
  expect_false(spec$body$page_by_visible)
})

test_that("fr_rows errors when page_by is invalid list (no cols element)", {
  expect_error(
    fr_table(df_ae) |> fr_rows(page_by = list(visible = TRUE)),
    class = "rlang_error"
  )
})

test_that("fr_rows errors when page_by is invalid type (logical)", {
  expect_error(
    fr_table(df_ae) |> fr_rows(page_by = TRUE),
    class = "rlang_error"
  )
})
