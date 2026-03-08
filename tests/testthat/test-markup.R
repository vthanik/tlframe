# ──────────────────────────────────────────────────────────────────────────────
# test-markup.R — Tests for markup.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Markup helpers — construction
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_super creates fr_markup object", {
  m <- fr_super(1)
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "SUPER")
  expect_equal(m$content, "1")
})

test_that("fr_sub creates fr_markup object", {
  m <- fr_sub("2")
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "SUB")
  expect_equal(m$content, "2")
})

test_that("fr_bold creates fr_markup object", {
  m <- fr_bold("Table 14.1.1")
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "BOLD")
  expect_equal(m$content, "Table 14.1.1")
})

test_that("fr_italic creates fr_markup object", {
  m <- fr_italic("p-value")
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "ITALIC")
})

test_that("fr_underline creates fr_markup object", {
  m <- fr_underline("important")
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "UNDERLINE")
})

test_that("fr_unicode creates fr_markup with UTF-8 content", {
  m <- fr_unicode(0x00B1)
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "UNICODE")
  expect_equal(m$content, "\u00b1")
})

test_that("fr_unicode errors on invalid input", {
  expect_error(fr_unicode("not_a_number"))
  expect_error(fr_unicode(c(1, 2)))
})

test_that("fr_dagger is shorthand for U+2020", {
  m <- fr_dagger()
  expect_s3_class(m, "fr_markup")
  expect_equal(m$content, "\u2020")
})

test_that("fr_ddagger is shorthand for U+2021", {
  m <- fr_ddagger()
  expect_s3_class(m, "fr_markup")
  expect_equal(m$content, "\u2021")
})

test_that("fr_newline creates NEWLINE markup", {
  m <- fr_newline()
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "NEWLINE")
})

test_that("fr_super accepts numeric input and coerces to character", {
  m <- fr_super(2)
  expect_equal(m$content, "2")
})


# ══════════════════════════════════════════════════════════════════════════════
# format / as.character — sentinel emission
# ══════════════════════════════════════════════════════════════════════════════

test_that("format.fr_markup produces sentinel token", {
  m <- fr_super(1)
  s <- format(m)
  expect_true(grepl("\x01", s, fixed = TRUE))
  expect_true(grepl("\x02", s, fixed = TRUE))
  expect_true(grepl("SUPER", s, fixed = TRUE))
  expect_true(grepl("1", s, fixed = TRUE))
})

test_that("as.character.fr_markup matches format output", {
  m <- fr_bold("test")
  expect_equal(as.character(m), format(m))
})

test_that("sentinels are unique per type", {
  s_super <- format(fr_super("x"))
  s_sub <- format(fr_sub("x"))
  s_bold <- format(fr_bold("x"))
  expect_false(s_super == s_sub)
  expect_false(s_super == s_bold)
  expect_false(s_sub == s_bold)
})


# ══════════════════════════════════════════════════════════════════════════════
# print — human-readable preview
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.fr_markup produces readable output", {
  expect_output(print(fr_super(1)), "\\^\\{1\\}")
  expect_output(print(fr_sub(2)), "_\\{2\\}")
  expect_output(print(fr_bold("x")), "\\*\\*x\\*\\*")
  expect_output(print(fr_italic("y")), "_y_")
  expect_output(print(fr_underline("z")), "__z__")
  expect_output(print(fr_newline()), "newline")
})

test_that("print.fr_markup returns invisibly", {
  m <- fr_super(1)
  expect_invisible(print(m))
})


# ══════════════════════════════════════════════════════════════════════════════
# eval_markup — glue string evaluation
# ══════════════════════════════════════════════════════════════════════════════

test_that("eval_markup passes plain strings through unchanged", {
  expect_equal(eval_markup("hello world"), "hello world")
  expect_equal(eval_markup(""), "")
  expect_equal(eval_markup("Page 1 of 5"), "Page 1 of 5")
})

test_that("eval_markup evaluates {fr_super()} expressions", {
  result <- eval_markup("{fr_super(1)} test")
  expect_true(grepl("\x01SUPER:1\x02", result, fixed = TRUE))
  expect_true(grepl("test", result, fixed = TRUE))
})

test_that("eval_markup evaluates {fr_bold()} expressions", {
  result <- eval_markup("{fr_bold('Table')}")
  expect_true(grepl("\x01BOLD:Table\x02", result, fixed = TRUE))
})

test_that("eval_markup handles multiple markup in one string", {
  result <- eval_markup("{fr_super(1)} and {fr_sub(2)}")
  expect_true(grepl("SUPER:1", result, fixed = TRUE))
  expect_true(grepl("SUB:2", result, fixed = TRUE))
})

test_that("eval_markup resolves variables from environment", {
  num <- 42
  result <- eval_markup("{fr_super(num)}")
  expect_true(grepl("SUPER:42", result, fixed = TRUE))
})

test_that("eval_markup handles non-character input gracefully", {
  expect_equal(eval_markup(123), 123)
  expect_null(eval_markup(NULL))
})

test_that("eval_markup_vec processes character vectors", {
  texts <- c("plain", "{fr_super(1)} note", "also plain")
  results <- eval_markup_vec(texts)
  expect_length(results, 3)
  expect_equal(results[1], "plain")
  expect_true(grepl("SUPER", results[2]))
  expect_equal(results[3], "also plain")
})


# ══════════════════════════════════════════════════════════════════════════════
# Sentinel resolution
# ══════════════════════════════════════════════════════════════════════════════

test_that("has_sentinel detects sentinels", {
  s <- markup_sentinel("SUPER", "1")
  expect_true(has_sentinel(paste0("text ", s, " more")))
  expect_false(has_sentinel("plain text"))
  expect_false(has_sentinel(""))
})

test_that("resolve_sentinels replaces tokens with resolver output", {
  s <- paste0("BMI kg/m", markup_sentinel("SUPER", "2"))
  result <- resolve_sentinels(s, function(type, content) {
    paste0("[", type, ":", content, "]")
  })
  expect_equal(result, "BMI kg/m[SUPER:2]")
})

test_that("resolve_sentinels handles multiple sentinels", {
  s <- paste0(
    markup_sentinel("SUPER", "1"),
    " and ",
    markup_sentinel("SUB", "x")
  )
  result <- resolve_sentinels(s, function(type, content) {
    paste0("<", tolower(type), ">", content, "</", tolower(type), ">")
  })
  expect_equal(result, "<super>1</super> and <sub>x</sub>")
})

test_that("resolve_sentinels passes through text without sentinels", {
  result <- resolve_sentinels("plain text", function(type, content) "FAIL")
  expect_equal(result, "plain text")
})

test_that("resolve_sentinels_vec works on vectors", {
  texts <- c(
    markup_sentinel("BOLD", "hello"),
    "plain",
    markup_sentinel("ITALIC", "world")
  )
  results <- resolve_sentinels_vec(texts, function(type, content) {
    paste0("[", content, "]")
  })
  expect_equal(results, c("[hello]", "plain", "[world]"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Plain text extraction
# ══════════════════════════════════════════════════════════════════════════════

test_that("sentinel_to_plain strips markup but keeps content", {
  s <- paste0("BMI kg/m", markup_sentinel("SUPER", "2"))
  expect_equal(sentinel_to_plain(s), "BMI kg/m2")
})

test_that("sentinel_to_plain converts NEWLINE to actual newline", {
  s <- paste0("line1", markup_sentinel("NEWLINE", ""), "line2")
  expect_equal(sentinel_to_plain(s), "line1\nline2")
})

test_that("sentinel_to_plain passes through plain text", {
  expect_equal(sentinel_to_plain("hello"), "hello")
})

test_that("sentinel_to_plain_vec works on vectors", {
  texts <- c(
    paste0("kg/m", markup_sentinel("SUPER", "2")),
    "plain",
    paste0("H", markup_sentinel("SUB", "2"), "O")
  )
  results <- sentinel_to_plain_vec(texts)
  expect_equal(results, c("kg/m2", "plain", "H2O"))
})


# ══════════════════════════════════════════════════════════════════════════════
# End-to-end: glue string → sentinel → plain text
# ══════════════════════════════════════════════════════════════════════════════

test_that("full pipeline: eval_markup → sentinel_to_plain", {
  input <- "{fr_super(1)} Pearson chi-square test"
  with_sentinels <- eval_markup(input)
  plain <- sentinel_to_plain(with_sentinels)
  expect_equal(plain, "1 Pearson chi-square test")
})

test_that("full pipeline: BMI kg/m² example", {
  input <- "BMI kg/m{fr_super(2)}"
  with_sentinels <- eval_markup(input)
  plain <- sentinel_to_plain(with_sentinels)
  expect_equal(plain, "BMI kg/m2")
})

test_that("full pipeline: H₂O example", {
  input <- "H{fr_sub(2)}O"
  with_sentinels <- eval_markup(input)
  plain <- sentinel_to_plain(with_sentinels)
  expect_equal(plain, "H2O")
})

test_that("full pipeline: multiple markup functions", {
  input <- "{fr_bold('Table')} {fr_super(1)}"
  with_sentinels <- eval_markup(input)
  plain <- sentinel_to_plain(with_sentinels)
  expect_equal(plain, "Table 1")
})

test_that("full pipeline: dagger symbol", {
  input <- "{fr_dagger()} Fisher exact test"
  with_sentinels <- eval_markup(input)
  plain <- sentinel_to_plain(with_sentinels)
  expect_equal(plain, "\u2020 Fisher exact test")
})
