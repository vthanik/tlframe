# ──────────────────────────────────────────────────────────────────────────────
# decimal.R — Full stat display alignment engine (18-type taxonomy)
#
# Architecture (first-principles vectorized redesign):
#   1. detect_stat_types()         — vectorized type classification (one regex pass per type)
#   2. parse_stat_components_vec() — column-oriented component frame (list of char vectors)
#   3. compute_stat_widths_vec()   — vectorized max(nchar()) per component field
#   4. rebuild_column_aligned_vec()— type-grouped vectorized stri_pad_* reconstruction
#   5. align_decimal_column()      — master pipeline: detect → parse → widths → rebuild
#   6. compute_all_decimal_geometry() — integration with finalize_spec()
#
# All formatted strings have the same nchar (padded). In monospace fonts
# (the submission font), space padding produces pixel-perfect
# alignment. Rendered as a single cell with left indent for centering.
# ──────────────────────────────────────────────────────────────────────────────

# Constants for decimal alignment live in fr_env (see constants.R, section 11)

#' Detect the stat display type of a single value
#'
#' Thin scalar wrapper around `detect_stat_types()`.
#'
#' @param value Character scalar (trimmed).
#' @return Character scalar — one of the type names, or "unknown".
#' @noRd
detect_stat_type <- function(value) {
  detect_stat_types(value)[[1L]]
}


#' Vectorized type detection across a column
#'
#' Tests all values against each pattern in priority order using
#' `stringi::stri_detect_regex()` (vectorized). Each value is assigned
#' the first matching type.
#'
#' @param content_vec Character vector of trimmed cell values.
#' @return Character vector of type names (same length as input).
#' @noRd
detect_stat_types <- function(content_vec) {
  n <- length(content_vec)
  result <- rep("unknown", n)
  unassigned <- rep(TRUE, n)

  for (i in seq_along(.arframe_registry$stat_type_patterns)) {
    if (!any(unassigned)) {
      break
    }
    matches <- stringi::stri_detect_regex(
      content_vec,
      .arframe_registry$stat_type_patterns[[i]]
    )
    hits <- unassigned & matches
    if (any(hits)) {
      result[hits] <- names(.arframe_registry$stat_type_patterns)[[i]]
      unassigned[hits] <- FALSE
    }
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Parse Regex Constants (precomputed at load time)
# ══════════════════════════════════════════════════════════════════════════════

#' Token-aware est_ci parse regex (12 groups: 4 per position)
#' @noRd
est_ci_parse_re <- paste0(
  "^",
  .arframe_const$num_or_tok_cap,
  "\\s*\\(\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*,\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*\\)$"
)

#' Token-aware est_ci_bracket parse regex (12 groups)
#' @noRd
est_ci_bracket_parse_re <- paste0(
  "^",
  .arframe_const$num_or_tok_cap,
  "\\s*\\[\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*,\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*\\]$"
)

#' est_ci_pval parse regex (16 groups: 4 × 3 CI + 4 pval)
#' @noRd
est_ci_pval_parse_re <- paste0(
  "^",
  .arframe_const$num_or_tok_cap,
  "\\s*\\(\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*,\\s*",
  .arframe_const$num_or_tok_cap,
  "\\s*\\)\\s+",
  .arframe_const$pval_or_tok_cap,
  "$"
)

#' est_spread_pct parse regex (6 groups)
#' @noRd
est_spread_pct_parse_re <- paste0(
  "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*%\\s*\\)$"
)

#' est_spread_pct_ci parse regex (12 groups)
#' @noRd
est_spread_pct_ci_parse_re <- paste0(
  "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*%\\s*\\)",
  "\\s+",
  "\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
)

#' n_pct_rate parse regex (8 groups)
#' @noRd
n_pct_rate_parse_re <- paste0(
  "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)",
  "\\s+(-?)(\\d+)\\.?(\\d*)$"
)

#' n_over_N_pct_ci parse regex (12 groups)
#' @noRd
n_over_N_pct_ci_parse_re <- paste0(
  "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)",
  "\\s+\\[\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\]$"
)


# ══════════════════════════════════════════════════════════════════════════════
# 3. Scalar Compatibility Wrappers (thin wrappers for tests / external callers)
# ══════════════════════════════════════════════════════════════════════════════

#' Parse a stat value into typed components
#'
#' Thin scalar wrapper around `parse_stat_components_vec()`. Returns a named
#' list of scalars (one per component field) for a single value.
#'
#' @param value Character scalar.
#' @param type_name Character scalar — detected type.
#' @return Named list with type-specific fields.
#' @noRd
parse_stat_value <- function(value, type_name) {
  lapply(parse_stat_components_vec(value, type_name), `[[`, 1L)
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. Vectorized Engine — Component Frame + Vectorized Reconstruction
#
# First-principles redesign of the alignment pipeline:
#
#   parse_stat_components_vec()    — "column-oriented" parser
#     Returns a named list of character vectors (one vector per component
#     field, all length n). Replaces the "row-oriented" list-of-named-lists
#     from parse_stat_values_batch(), eliminating the O(n) list overhead that
#     prevented vectorized padding operations.
#
#   compute_stat_widths_vec()      — vectorized width computation
#     Same logic as compute_stat_widths() but operates directly on character
#     vectors via max(nchar(vec)) instead of vapply(list, ..., nchar).
#     No more per-element list dereference overhead.
#
#   rebuild_column_aligned_vec()   — fully vectorized reconstruction
#     Groups values by type, then applies stri_pad_left/stri_pad_right/paste0
#     on each TYPE GROUP as a vector, not as n individual function calls.
#     A 1000-row column of est_ci values: 1 rebuild call → 10 vectorized ops.
#     Previously: 1000 calls to rebuild_stat_aligned() with a 25-branch switch.
#
#   align_decimal_column()         — same public interface, new internal path
#     Dispatches through the new engine. Output is byte-for-byte identical to
#     the old path.
# ══════════════════════════════════════════════════════════════════════════════

# ── Width helpers ─────────────────────────────────────────────────────────────
.maxw <- function(comps, f, idx) max(0L, nchar(comps[[f]][idx]))
.maxw_si <- function(comps, intf, signf, idx) {
  max(0L, nchar(paste0(comps[[signf]][idx], comps[[intf]][idx])))
}
.maxw_tok_si <- function(comps, tokf, signf, intf, idx) {
  tok <- comps[[tokf]][idx]
  has_tok <- nzchar(tok)
  w <- integer(length(idx))
  if (any(has_tok)) {
    w[has_tok] <- nchar(tok[has_tok])
  }
  if (any(!has_tok)) {
    w[!has_tok] <- nchar(paste0(
      comps[[signf]][idx[!has_tok]],
      comps[[intf]][idx[!has_tok]]
    ))
  }
  max(0L, w)
}
.has_any <- function(comps, f, idx) any(nzchar(comps[[f]][idx]))

# ── Rebuild helpers (lifted from rebuild_column_aligned_vec) ──────────────────
.vfloat <- function(j, comps, sf, intf, decf, w_si, w_dec, has_dec) {
  si <- stringi::stri_pad_left(paste0(comps[[sf]][j], comps[[intf]][j]), w_si)
  if (has_dec) {
    paste0(si, ".", stringi::stri_pad_right(comps[[decf]][j], w_dec))
  } else {
    si
  }
}
.vtokfloat <- function(j, comps, tokf, sf, intf, decf, w_si, w_dec, has_dec) {
  tok <- comps[[tokf]][j]
  has_tok <- nzchar(tok)
  out <- character(length(j))
  if (any(has_tok)) {
    p <- stringi::stri_pad_left(tok[has_tok], w_si)
    out[has_tok] <- if (has_dec) paste0(p, strrep(" ", 1L + w_dec)) else p
  }
  if (any(!has_tok)) {
    out[!has_tok] <- .vfloat(
      j[!has_tok],
      comps,
      sf,
      intf,
      decf,
      w_si,
      w_dec,
      has_dec
    )
  }
  out
}
.vpct <- function(j, comps, widths) {
  pct <- stringi::stri_pad_left(
    paste0(comps$pct_prefix[j], comps$pct_int[j]),
    widths$w_pct_prefix + widths$w_pct_int
  )
  if (isTRUE(widths$has_dec)) {
    pct <- paste0(
      pct,
      ".",
      stringi::stri_pad_right(comps$pct_dec[j], widths$w_pct_dec)
    )
  }
  paste0(pct, stringi::stri_pad_right(comps$pct_sign[j], widths$w_pct_sign))
}
.vci <- function(j, comps, widths, open_ch, close_ch) {
  est <- .vtokfloat(
    j,
    comps,
    "est_token",
    "est_sign",
    "est_int",
    "est_dec",
    widths$w_est_si,
    widths$w_est_dec,
    widths$has_est_dec
  )
  lo <- .vtokfloat(
    j,
    comps,
    "lo_token",
    "lo_sign",
    "lo_int",
    "lo_dec",
    widths$w_lo_si,
    widths$w_lo_dec,
    widths$has_lo_dec
  )
  hi <- .vtokfloat(
    j,
    comps,
    "hi_token",
    "hi_sign",
    "hi_int",
    "hi_dec",
    widths$w_hi_si,
    widths$w_hi_dec,
    widths$has_hi_dec
  )
  paste0(est, " ", open_ch, lo, ", ", hi, close_ch)
}

# ── Handler registry ──────────────────────────────────────────────────────────
# Each entry: parse(vals), width(comps,idx), rebuild_dominant(j,comps,widths,tp),
#   rebuild_cross(j,comps,widths,fw,dominant_type), int_slot,
#   optionally: primary (char), width_siblings (char vec), width_subsiblings (char vec)
.decimal_handlers <- local({
  e <- new.env(parent = emptyenv())

  e$n_only <- list(
    parse = function(vals) list(n = vals),
    width = function(comps, idx) {
      w <- .maxw(comps, "n", idx)
      list(w_n = w, full_width = w)
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      stringi::stri_pad_left(comps$n[j], widths$w_n)
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      h_dom <- .decimal_handlers[[dominant_type]]
      if (!is.null(h_dom$primary)) {
        h_dom <- .decimal_handlers[[h_dom$primary]]
      }
      int_slot_w <- if (!is.null(h_dom$int_slot)) {
        widths[[h_dom$int_slot]]
      } else {
        NULL
      }
      if (is.null(int_slot_w)) {
        return(stringi::stri_pad_right(comps$n[j], fw))
      }
      pn <- stringi::stri_pad_left(comps$n[j], int_slot_w)
      dec_w <- if (
        dominant_type %in%
          c(
            "est_spread",
            "est_spread_pct",
            "est_ci",
            "est_ci_bracket",
            "est_ci_pval",
            "est_spread_pct_ci"
          )
      ) {
        if (isTRUE(widths$has_est_dec)) 1L + widths$w_est_dec else 0L
      } else if (dominant_type == "scalar_float") {
        if (isTRUE(widths$has_dec)) 1L + widths$w_dec else 0L
      } else if (dominant_type == "pvalue") {
        1L + widths$w_dec
      } else {
        0L
      }
      if (dec_w > 0L) {
        pn <- stringi::stri_pad_right(pn, int_slot_w + dec_w)
      }
      stringi::stri_pad_right(pn, fw)
    },
    int_slot = NULL
  )

  e$scalar_float <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, "^(-?)(\\d+)\\.(\\d+)$")
      list(sign = m[, 2L], int = m[, 3L], dec = m[, 4L])
    },
    width = function(comps, idx) {
      w_si <- .maxw_si(comps, "int", "sign", idx)
      w_dec <- .maxw(comps, "dec", idx)
      list(
        w_sign_int = w_si,
        w_dec = w_dec,
        has_dec = TRUE,
        full_width = w_si + 1L + w_dec
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      .vfloat(
        j,
        comps,
        "sign",
        "int",
        "dec",
        widths$w_sign_int,
        widths$w_dec,
        TRUE
      )
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (
        dominant_type %in%
          c(
            "est_spread",
            "est_spread_pct",
            "est_ci",
            "est_ci_bracket",
            "est_ci_pval",
            "est_spread_pct_ci"
          )
      ) {
        stringi::stri_pad_right(
          .vfloat(
            j,
            comps,
            "sign",
            "int",
            "dec",
            widths$w_est_si,
            widths$w_est_dec,
            widths$has_est_dec
          ),
          fw
        )
      } else if (dominant_type == "n_pct") {
        w_n <- widths$w_npct_n %||% widths$w_n
        stringi::stri_pad_right(
          .vfloat(
            j,
            comps,
            "sign",
            "int",
            "dec",
            w_n,
            max(nchar(comps$dec[j])),
            any(nzchar(comps$dec[j]))
          ),
          fw
        )
      } else if (dominant_type == "pvalue") {
        w_si <- (widths$w_prefix %||% 0L) + (widths$w_int %||% 0L)
        .vfloat(j, comps, "sign", "int", "dec", w_si, widths$w_dec, TRUE)
      } else {
        stringi::stri_pad_right(comps$raw[j], fw)
      }
    },
    int_slot = "w_sign_int"
  )

  e$pvalue <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, "^([<>=])(\\d+)\\.(\\d+)$")
      list(prefix = m[, 2L], int = m[, 3L], dec = m[, 4L])
    },
    width = function(comps, idx) {
      w_pfx <- .maxw(comps, "prefix", idx)
      w_int <- .maxw(comps, "int", idx)
      w_dec <- .maxw(comps, "dec", idx)
      list(
        w_prefix = w_pfx,
        w_int = w_int,
        w_dec = w_dec,
        full_width = w_pfx + w_int + 1L + w_dec
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        stringi::stri_pad_left(comps$prefix[j], widths$w_prefix),
        stringi::stri_pad_left(comps$int[j], widths$w_int),
        ".",
        stringi::stri_pad_right(comps$dec[j], widths$w_dec)
      )
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (
        dominant_type %in%
          c(
            "scalar_float",
            "est_spread",
            "est_spread_pct",
            "est_ci",
            "est_ci_bracket",
            "est_ci_pval",
            "est_spread_pct_ci"
          )
      ) {
        w_si <- widths$w_sign_int %||% widths$w_est_si
        stringi::stri_pad_right(
          .vfloat(j, comps, "prefix", "int", "dec", w_si, widths$w_dec, TRUE),
          fw
        )
      } else {
        stringi::stri_pad_right(comps$raw[j], fw)
      }
    },
    int_slot = "w_int"
  )

  e$n_pct <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
      )
      list(
        n = m[, 2L],
        pct_prefix = m[, 3L],
        pct_int = m[, 4L],
        pct_dec = m[, 5L],
        pct_sign = m[, 6L]
      )
    },
    width = function(comps, idx) {
      w_n <- .maxw(comps, "n", idx)
      w_pp <- .maxw(comps, "pct_prefix", idx)
      w_pi <- .maxw(comps, "pct_int", idx)
      w_pd <- .maxw(comps, "pct_dec", idx)
      w_ps <- .maxw(comps, "pct_sign", idx)
      has_dec <- .has_any(comps, "pct_dec", idx)
      fw <- w_n + 2L + w_pp + w_pi
      if (has_dec) {
        fw <- fw + 1L + w_pd
      }
      fw <- fw + w_ps + 1L
      list(
        w_n = w_n,
        w_pct_prefix = w_pp,
        w_pct_int = w_pi,
        w_pct_dec = w_pd,
        w_pct_sign = w_ps,
        has_dec = has_dec,
        full_width = fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      pn <- stringi::stri_pad_left(comps$n[j], widths$w_n)
      n_val <- suppressWarnings(as.integer(comps$n[j]))
      is_zero <- !is.na(n_val) & n_val == 0L
      r <- paste0(pn, " (", .vpct(j, comps, widths), ")")
      r[is_zero] <- stringi::stri_pad_right(pn[is_zero], widths$full_width)
      r
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (is.null(widths$w_npct_n)) {
        return(stringi::stri_pad_right(comps$raw[j], fw))
      }
      pn <- if (dominant_type %in% c("n_over_N_pct", "n_over_N_pct_ci")) {
        stringi::stri_pad_right(
          comps$n[j],
          (widths$w_num %||% 0L) + 1L + (widths$w_den %||% 0L)
        )
      } else {
        stringi::stri_pad_left(comps$n[j], widths$w_npct_n)
      }
      pct <- stringi::stri_pad_left(
        paste0(comps$pct_prefix[j], comps$pct_int[j]),
        widths$w_npct_pct_prefix + widths$w_npct_pct_int
      )
      if (isTRUE(widths$has_npct_dec)) {
        pct <- paste0(
          pct,
          ".",
          stringi::stri_pad_right(comps$pct_dec[j], widths$w_npct_pct_dec)
        )
      }
      pct <- paste0(
        pct,
        stringi::stri_pad_right(comps$pct_sign[j], widths$w_npct_pct_sign)
      )
      stringi::stri_pad_right(paste0(pn, " (", pct, ")"), fw)
    },
    int_slot = "w_n"
  )

  e$n_over_N_pct <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
      )
      list(
        num = m[, 2L],
        den = m[, 3L],
        pct_prefix = m[, 4L],
        pct_int = m[, 5L],
        pct_dec = m[, 6L],
        pct_sign = m[, 7L]
      )
    },
    width_subsiblings = "n_over_N",
    width = function(comps, idx) {
      w_num <- .maxw(comps, "num", idx)
      w_den <- .maxw(comps, "den", idx)
      w_pp <- .maxw(comps, "pct_prefix", idx)
      w_pi <- .maxw(comps, "pct_int", idx)
      w_pd <- .maxw(comps, "pct_dec", idx)
      w_ps <- .maxw(comps, "pct_sign", idx)
      has_dec <- .has_any(comps, "pct_dec", idx)
      fw <- w_num + 1L + w_den + 2L + w_pp + w_pi
      if (has_dec) {
        fw <- fw + 1L + w_pd
      }
      fw <- fw + w_ps + 1L
      list(
        w_num = w_num,
        w_den = w_den,
        w_pct_prefix = w_pp,
        w_pct_int = w_pi,
        w_pct_dec = w_pd,
        w_pct_sign = w_ps,
        has_dec = has_dec,
        full_width = fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        stringi::stri_pad_left(comps$num[j], widths$w_num),
        "/",
        stringi::stri_pad_right(comps$den[j], widths$w_den),
        " (",
        .vpct(j, comps, widths),
        ")"
      )
    },
    rebuild_cross = NULL,
    int_slot = "w_num"
  )

  e$est_spread <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
      )
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        sprd_sign = m[, 5L],
        sprd_int = m[, 6L],
        sprd_dec = m[, 7L]
      )
    },
    width_siblings = "est_spread_pct",
    width = function(comps, idx) {
      w_est_si <- .maxw_si(comps, "est_int", "est_sign", idx)
      w_est_dec <- .maxw(comps, "est_dec", idx)
      w_sprd_si <- .maxw_si(comps, "sprd_int", "sprd_sign", idx)
      has_est_dec <- .has_any(comps, "est_dec", idx)
      has_sprd_dec <- .has_any(comps, "sprd_dec", idx)
      is_pct <- comps$type[idx] == "est_spread_pct"
      dec_w <- nchar(comps$sprd_dec[idx])
      dec_w[is_pct] <- dec_w[is_pct] + 1L
      w_sprd_dec_slot <- max(0L, dec_w)
      fw <- w_est_si
      if (has_est_dec) {
        fw <- fw + 1L + w_est_dec
      }
      fw <- fw + 2L + w_sprd_si
      if (has_sprd_dec) {
        fw <- fw + 1L + w_sprd_dec_slot
      }
      fw <- fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_sprd_si = w_sprd_si,
        w_sprd_dec_slot = w_sprd_dec_slot,
        has_est_dec = has_est_dec,
        has_sprd_dec = has_sprd_dec,
        full_width = fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      est <- .vfloat(
        j,
        comps,
        "est_sign",
        "est_int",
        "est_dec",
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      si_str <- stringi::stri_pad_left(
        paste0(comps$sprd_sign[j], comps$sprd_int[j]),
        widths$w_sprd_si
      )
      if (widths$has_sprd_dec) {
        dec_str <- if (tp == "est_spread_pct") {
          paste0(comps$sprd_dec[j], "%")
        } else {
          comps$sprd_dec[j]
        }
        sprd <- paste0(
          si_str,
          ".",
          stringi::stri_pad_right(dec_str, widths$w_sprd_dec_slot)
        )
      } else {
        sprd <- if (tp == "est_spread_pct") paste0(si_str, "%") else si_str
      }
      paste0(est, " (", sprd, ")")
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (!dominant_type %in% c("est_ci", "est_ci_bracket")) {
        return(stringi::stri_pad_right(comps$raw[j], fw))
      }
      est <- .vfloat(
        j,
        comps,
        "est_sign",
        "est_int",
        "est_dec",
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      sprd <- .vfloat(
        j,
        comps,
        "sprd_sign",
        "sprd_int",
        "sprd_dec",
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      open_ch <- if (dominant_type == "est_ci") "(" else "["
      close_ch <- if (dominant_type == "est_ci") ")" else "]"
      stringi::stri_pad_right(paste0(est, " ", open_ch, sprd, close_ch), fw)
    },
    int_slot = "w_est_si"
  )

  e$est_spread_pct <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, est_spread_pct_parse_re)
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        sprd_sign = m[, 5L],
        sprd_int = m[, 6L],
        sprd_dec = m[, 7L]
      )
    },
    primary = "est_spread",
    int_slot = "w_est_si"
  )

  e$est_ci <- list(
    parse = function(vals) {
      m <- na_to_empty(stringi::stri_match_first_regex(vals, est_ci_parse_re))
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        est_token = m[, 5L],
        lo_sign = m[, 6L],
        lo_int = m[, 7L],
        lo_dec = m[, 8L],
        lo_token = m[, 9L],
        hi_sign = m[, 10L],
        hi_int = m[, 11L],
        hi_dec = m[, 12L],
        hi_token = m[, 13L]
      )
    },
    width_siblings = "est_ci_bracket",
    width = function(comps, idx) {
      w_est_si <- .maxw_tok_si(comps, "est_token", "est_sign", "est_int", idx)
      w_est_dec <- .maxw(comps, "est_dec", idx)
      w_lo_si <- .maxw_tok_si(comps, "lo_token", "lo_sign", "lo_int", idx)
      w_lo_dec <- .maxw(comps, "lo_dec", idx)
      w_hi_si <- .maxw_tok_si(comps, "hi_token", "hi_sign", "hi_int", idx)
      w_hi_dec <- .maxw(comps, "hi_dec", idx)
      has_est_dec <- .has_any(comps, "est_dec", idx)
      has_lo_dec <- .has_any(comps, "lo_dec", idx)
      has_hi_dec <- .has_any(comps, "hi_dec", idx)
      fw <- w_est_si
      if (has_est_dec) {
        fw <- fw + 1L + w_est_dec
      }
      fw <- fw + 2L + w_lo_si
      if (has_lo_dec) {
        fw <- fw + 1L + w_lo_dec
      }
      fw <- fw + 2L + w_hi_si
      if (has_hi_dec) {
        fw <- fw + 1L + w_hi_dec
      }
      fw <- fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_lo_si = w_lo_si,
        w_lo_dec = w_lo_dec,
        w_hi_si = w_hi_si,
        w_hi_dec = w_hi_dec,
        has_est_dec = has_est_dec,
        has_lo_dec = has_lo_dec,
        has_hi_dec = has_hi_dec,
        full_width = fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      if (tp == "est_ci_bracket") {
        .vci(j, comps, widths, "[", "]")
      } else {
        .vci(j, comps, widths, "(", ")")
      }
    },
    rebuild_cross = NULL,
    int_slot = "w_est_si"
  )

  e$est_ci_bracket <- list(
    parse = function(vals) {
      m <- na_to_empty(stringi::stri_match_first_regex(
        vals,
        est_ci_bracket_parse_re
      ))
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        est_token = m[, 5L],
        lo_sign = m[, 6L],
        lo_int = m[, 7L],
        lo_dec = m[, 8L],
        lo_token = m[, 9L],
        hi_sign = m[, 10L],
        hi_int = m[, 11L],
        hi_dec = m[, 12L],
        hi_token = m[, 13L]
      )
    },
    primary = "est_ci",
    int_slot = "w_est_si"
  )

  e$est_ci_pval <- list(
    parse = function(vals) {
      m <- na_to_empty(stringi::stri_match_first_regex(
        vals,
        est_ci_pval_parse_re
      ))
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        est_token = m[, 5L],
        lo_sign = m[, 6L],
        lo_int = m[, 7L],
        lo_dec = m[, 8L],
        lo_token = m[, 9L],
        hi_sign = m[, 10L],
        hi_int = m[, 11L],
        hi_dec = m[, 12L],
        hi_token = m[, 13L],
        pv_prefix = m[, 14L],
        pv_int = m[, 15L],
        pv_dec = m[, 16L],
        pv_token = m[, 17L]
      )
    },
    width = function(comps, idx) {
      w_est_si <- .maxw_tok_si(comps, "est_token", "est_sign", "est_int", idx)
      w_est_dec <- .maxw(comps, "est_dec", idx)
      w_lo_si <- .maxw_tok_si(comps, "lo_token", "lo_sign", "lo_int", idx)
      w_lo_dec <- .maxw(comps, "lo_dec", idx)
      w_hi_si <- .maxw_tok_si(comps, "hi_token", "hi_sign", "hi_int", idx)
      w_hi_dec <- .maxw(comps, "hi_dec", idx)
      has_est_dec <- .has_any(comps, "est_dec", idx)
      has_lo_dec <- .has_any(comps, "lo_dec", idx)
      has_hi_dec <- .has_any(comps, "hi_dec", idx)
      ci_fw <- w_est_si
      if (has_est_dec) {
        ci_fw <- ci_fw + 1L + w_est_dec
      }
      ci_fw <- ci_fw + 2L + w_lo_si
      if (has_lo_dec) {
        ci_fw <- ci_fw + 1L + w_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_hi_si
      if (has_hi_dec) {
        ci_fw <- ci_fw + 1L + w_hi_dec
      }
      ci_fw <- ci_fw + 1L
      w_pv_pi <- .maxw_tok_si(comps, "pv_token", "pv_prefix", "pv_int", idx)
      w_pv_dec <- .maxw(comps, "pv_dec", idx)
      has_pv_dec <- .has_any(comps, "pv_dec", idx)
      pv_fw <- w_pv_pi
      if (has_pv_dec) {
        pv_fw <- pv_fw + 1L + w_pv_dec
      }
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_lo_si = w_lo_si,
        w_lo_dec = w_lo_dec,
        w_hi_si = w_hi_si,
        w_hi_dec = w_hi_dec,
        has_est_dec = has_est_dec,
        has_lo_dec = has_lo_dec,
        has_hi_dec = has_hi_dec,
        w_pv_pi = w_pv_pi,
        w_pv_dec = w_pv_dec,
        has_pv_dec = has_pv_dec,
        gap = .arframe_const$compound_gap,
        full_width = ci_fw + .arframe_const$compound_gap + pv_fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      pv <- .vtokfloat(
        j,
        comps,
        "pv_token",
        "pv_prefix",
        "pv_int",
        "pv_dec",
        widths$w_pv_pi,
        widths$w_pv_dec,
        widths$has_pv_dec
      )
      paste0(.vci(j, comps, widths, "(", ")"), strrep(" ", widths$gap), pv)
    },
    rebuild_cross = NULL,
    int_slot = "w_est_si"
  )

  e$est_spread_pct_ci <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, est_spread_pct_ci_parse_re)
      list(
        est_sign = m[, 2L],
        est_int = m[, 3L],
        est_dec = m[, 4L],
        sprd_sign = m[, 5L],
        sprd_int = m[, 6L],
        sprd_dec = m[, 7L],
        ci_lo_sign = m[, 8L],
        ci_lo_int = m[, 9L],
        ci_lo_dec = m[, 10L],
        ci_hi_sign = m[, 11L],
        ci_hi_int = m[, 12L],
        ci_hi_dec = m[, 13L]
      )
    },
    width = function(comps, idx) {
      w_est_si <- .maxw_si(comps, "est_int", "est_sign", idx)
      w_est_dec <- .maxw(comps, "est_dec", idx)
      w_sprd_si <- .maxw_si(comps, "sprd_int", "sprd_sign", idx)
      w_sprd_dec_slot <- .maxw(comps, "sprd_dec", idx) + 1L
      has_est_dec <- .has_any(comps, "est_dec", idx)
      has_sprd_dec <- .has_any(comps, "sprd_dec", idx)
      espct_fw <- w_est_si
      if (has_est_dec) {
        espct_fw <- espct_fw + 1L + w_est_dec
      }
      espct_fw <- espct_fw + 2L + w_sprd_si
      if (has_sprd_dec) {
        espct_fw <- espct_fw + 1L + w_sprd_dec_slot
      }
      espct_fw <- espct_fw + 1L
      w_ci_lo_si <- .maxw_si(comps, "ci_lo_int", "ci_lo_sign", idx)
      w_ci_lo_dec <- .maxw(comps, "ci_lo_dec", idx)
      w_ci_hi_si <- .maxw_si(comps, "ci_hi_int", "ci_hi_sign", idx)
      w_ci_hi_dec <- .maxw(comps, "ci_hi_dec", idx)
      has_ci_lo_dec <- .has_any(comps, "ci_lo_dec", idx)
      has_ci_hi_dec <- .has_any(comps, "ci_hi_dec", idx)
      ci_fw <- 1L + w_ci_lo_si
      if (has_ci_lo_dec) {
        ci_fw <- ci_fw + 1L + w_ci_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_ci_hi_si
      if (has_ci_hi_dec) {
        ci_fw <- ci_fw + 1L + w_ci_hi_dec
      }
      ci_fw <- ci_fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_sprd_si = w_sprd_si,
        w_sprd_dec_slot = w_sprd_dec_slot,
        has_est_dec = has_est_dec,
        has_sprd_dec = has_sprd_dec,
        w_ci_lo_si = w_ci_lo_si,
        w_ci_lo_dec = w_ci_lo_dec,
        w_ci_hi_si = w_ci_hi_si,
        w_ci_hi_dec = w_ci_hi_dec,
        has_ci_lo_dec = has_ci_lo_dec,
        has_ci_hi_dec = has_ci_hi_dec,
        gap = .arframe_const$compound_gap,
        full_width = espct_fw + .arframe_const$compound_gap + ci_fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      est <- .vfloat(
        j,
        comps,
        "est_sign",
        "est_int",
        "est_dec",
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      si_str <- stringi::stri_pad_left(
        paste0(comps$sprd_sign[j], comps$sprd_int[j]),
        widths$w_sprd_si
      )
      sprd <- if (widths$has_sprd_dec) {
        paste0(
          si_str,
          ".",
          stringi::stri_pad_right(
            paste0(comps$sprd_dec[j], "%"),
            widths$w_sprd_dec_slot
          )
        )
      } else {
        paste0(si_str, "%")
      }
      ci_lo <- .vfloat(
        j,
        comps,
        "ci_lo_sign",
        "ci_lo_int",
        "ci_lo_dec",
        widths$w_ci_lo_si,
        widths$w_ci_lo_dec,
        widths$has_ci_lo_dec
      )
      ci_hi <- .vfloat(
        j,
        comps,
        "ci_hi_sign",
        "ci_hi_int",
        "ci_hi_dec",
        widths$w_ci_hi_si,
        widths$w_ci_hi_dec,
        widths$has_ci_hi_dec
      )
      paste0(
        est,
        " (",
        sprd,
        ")",
        strrep(" ", widths$gap),
        "(",
        ci_lo,
        ", ",
        ci_hi,
        ")"
      )
    },
    rebuild_cross = NULL,
    int_slot = "w_est_si"
  )

  e$n_pct_rate <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, n_pct_rate_parse_re)
      list(
        n = m[, 2L],
        pct_prefix = m[, 3L],
        pct_int = m[, 4L],
        pct_dec = m[, 5L],
        pct_sign = m[, 6L],
        rate_sign = m[, 7L],
        rate_int = m[, 8L],
        rate_dec = m[, 9L]
      )
    },
    width = function(comps, idx) {
      w_n <- .maxw(comps, "n", idx)
      w_pp <- .maxw(comps, "pct_prefix", idx)
      w_pi <- .maxw(comps, "pct_int", idx)
      w_pd <- .maxw(comps, "pct_dec", idx)
      w_ps <- .maxw(comps, "pct_sign", idx)
      has_dec <- .has_any(comps, "pct_dec", idx)
      npct_fw <- w_n + 2L + w_pp + w_pi
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pd
      }
      npct_fw <- npct_fw + w_ps + 1L
      w_rate_si <- .maxw_si(comps, "rate_int", "rate_sign", idx)
      w_rate_dec <- .maxw(comps, "rate_dec", idx)
      has_rate_dec <- .has_any(comps, "rate_dec", idx)
      rate_fw <- w_rate_si
      if (has_rate_dec) {
        rate_fw <- rate_fw + 1L + w_rate_dec
      }
      list(
        w_n = w_n,
        w_pct_prefix = w_pp,
        w_pct_int = w_pi,
        w_pct_dec = w_pd,
        w_pct_sign = w_ps,
        has_dec = has_dec,
        w_rate_si = w_rate_si,
        w_rate_dec = w_rate_dec,
        has_rate_dec = has_rate_dec,
        gap = .arframe_const$compound_gap,
        full_width = npct_fw + .arframe_const$compound_gap + rate_fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      pn <- stringi::stri_pad_left(comps$n[j], widths$w_n)
      n_val <- suppressWarnings(as.integer(comps$n[j]))
      is_zero <- !is.na(n_val) & n_val == 0L
      r <- paste0(
        pn,
        " (",
        .vpct(j, comps, widths),
        ")",
        strrep(" ", widths$gap),
        .vfloat(
          j,
          comps,
          "rate_sign",
          "rate_int",
          "rate_dec",
          widths$w_rate_si,
          widths$w_rate_dec,
          widths$has_rate_dec
        )
      )
      r[is_zero] <- stringi::stri_pad_right(pn[is_zero], widths$full_width)
      r
    },
    rebuild_cross = NULL,
    int_slot = "w_n"
  )

  e$n_over_N_pct_ci <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, n_over_N_pct_ci_parse_re)
      list(
        num = m[, 2L],
        den = m[, 3L],
        pct_prefix = m[, 4L],
        pct_int = m[, 5L],
        pct_dec = m[, 6L],
        pct_sign = m[, 7L],
        ci_lo_sign = m[, 8L],
        ci_lo_int = m[, 9L],
        ci_lo_dec = m[, 10L],
        ci_hi_sign = m[, 11L],
        ci_hi_int = m[, 12L],
        ci_hi_dec = m[, 13L]
      )
    },
    width = function(comps, idx) {
      w_num <- .maxw(comps, "num", idx)
      w_den <- .maxw(comps, "den", idx)
      w_pp <- .maxw(comps, "pct_prefix", idx)
      w_pi <- .maxw(comps, "pct_int", idx)
      w_pd <- .maxw(comps, "pct_dec", idx)
      w_ps <- .maxw(comps, "pct_sign", idx)
      has_dec <- .has_any(comps, "pct_dec", idx)
      npct_fw <- w_num + 1L + w_den + 2L + w_pp + w_pi
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pd
      }
      npct_fw <- npct_fw + w_ps + 1L
      w_ci_lo_si <- .maxw_si(comps, "ci_lo_int", "ci_lo_sign", idx)
      w_ci_lo_dec <- .maxw(comps, "ci_lo_dec", idx)
      w_ci_hi_si <- .maxw_si(comps, "ci_hi_int", "ci_hi_sign", idx)
      w_ci_hi_dec <- .maxw(comps, "ci_hi_dec", idx)
      has_ci_lo_dec <- .has_any(comps, "ci_lo_dec", idx)
      has_ci_hi_dec <- .has_any(comps, "ci_hi_dec", idx)
      ci_fw <- 1L + w_ci_lo_si
      if (has_ci_lo_dec) {
        ci_fw <- ci_fw + 1L + w_ci_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_ci_hi_si
      if (has_ci_hi_dec) {
        ci_fw <- ci_fw + 1L + w_ci_hi_dec
      }
      ci_fw <- ci_fw + 1L
      list(
        w_num = w_num,
        w_den = w_den,
        w_pct_prefix = w_pp,
        w_pct_int = w_pi,
        w_pct_dec = w_pd,
        w_pct_sign = w_ps,
        has_dec = has_dec,
        w_ci_lo_si = w_ci_lo_si,
        w_ci_lo_dec = w_ci_lo_dec,
        w_ci_hi_si = w_ci_hi_si,
        w_ci_hi_dec = w_ci_hi_dec,
        has_ci_lo_dec = has_ci_lo_dec,
        has_ci_hi_dec = has_ci_hi_dec,
        gap = 1L,
        full_width = npct_fw + 1L + ci_fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      ci_lo <- .vfloat(
        j,
        comps,
        "ci_lo_sign",
        "ci_lo_int",
        "ci_lo_dec",
        widths$w_ci_lo_si,
        widths$w_ci_lo_dec,
        widths$has_ci_lo_dec
      )
      ci_hi <- .vfloat(
        j,
        comps,
        "ci_hi_sign",
        "ci_hi_int",
        "ci_hi_dec",
        widths$w_ci_hi_si,
        widths$w_ci_hi_dec,
        widths$has_ci_hi_dec
      )
      paste0(
        stringi::stri_pad_left(comps$num[j], widths$w_num),
        "/",
        stringi::stri_pad_right(comps$den[j], widths$w_den),
        " (",
        .vpct(j, comps, widths),
        ")",
        strrep(" ", widths$gap),
        "[",
        ci_lo,
        ", ",
        ci_hi,
        "]"
      )
    },
    rebuild_cross = NULL,
    int_slot = "w_num"
  )

  e$n_over_N <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(vals, "^(\\d+)\\s*/\\s*(\\d+)$")
      list(num = m[, 2L], den = m[, 3L])
    },
    width = function(comps, idx) {
      w_num <- .maxw(comps, "num", idx)
      w_den <- .maxw(comps, "den", idx)
      list(w_num = w_num, w_den = w_den, full_width = w_num + 1L + w_den)
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        stringi::stri_pad_left(comps$num[j], widths$w_num),
        "/",
        stringi::stri_pad_right(comps$den[j], widths$w_den)
      )
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (dominant_type == "n_over_N_pct") {
        stringi::stri_pad_right(
          stringi::stri_pad_left(comps$num[j], widths$w_num),
          fw
        )
      } else {
        stringi::stri_pad_right(comps$raw[j], fw)
      }
    },
    int_slot = "w_num"
  )

  e$n_over_float <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        "^(\\d+)\\s*/\\s*(\\d+)\\.(\\d+)$"
      )
      list(num = m[, 2L], den_int = m[, 3L], den_dec = m[, 4L])
    },
    width = function(comps, idx) {
      w_num <- .maxw(comps, "num", idx)
      w_di <- .maxw(comps, "den_int", idx)
      w_dd <- .maxw(comps, "den_dec", idx)
      list(
        w_num = w_num,
        w_den_int = w_di,
        w_den_dec = w_dd,
        full_width = w_num + 1L + w_di + 1L + w_dd
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        stringi::stri_pad_left(comps$num[j], widths$w_num),
        "/",
        stringi::stri_pad_left(comps$den_int[j], widths$w_den_int),
        ".",
        stringi::stri_pad_right(comps$den_dec[j], widths$w_den_dec)
      )
    },
    rebuild_cross = NULL,
    int_slot = "w_num"
  )

  e$range_pair <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        "^[\\(\\[]?\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*[\\)\\]]?$"
      )
      list(
        l_sign = m[, 2L],
        l_int = m[, 3L],
        l_dec = m[, 4L],
        r_sign = m[, 5L],
        r_int = m[, 6L],
        r_dec = m[, 7L],
        open_delim = ifelse(
          grepl("^\\s*\\(", vals),
          "(",
          ifelse(grepl("^\\s*\\[", vals), "[", "")
        ),
        close_delim = ifelse(
          grepl("\\)\\s*$", vals),
          ")",
          ifelse(grepl("\\]\\s*$", vals), "]", "")
        )
      )
    },
    width = function(comps, idx) {
      w_l_si <- .maxw_si(comps, "l_int", "l_sign", idx)
      w_l_dec <- .maxw(comps, "l_dec", idx)
      w_r_si <- .maxw_si(comps, "r_int", "r_sign", idx)
      w_r_dec <- .maxw(comps, "r_dec", idx)
      has_l_dec <- .has_any(comps, "l_dec", idx)
      has_r_dec <- .has_any(comps, "r_dec", idx)
      has_delim <- any(nzchar(comps$open_delim[idx]))
      fw <- w_l_si
      if (has_l_dec) {
        fw <- fw + 1L + w_l_dec
      }
      fw <- fw + 2L + w_r_si
      if (has_r_dec) {
        fw <- fw + 1L + w_r_dec
      }
      if (has_delim) {
        fw <- fw + 2L
      }
      list(
        w_l_si = w_l_si,
        w_l_dec = w_l_dec,
        w_r_si = w_r_si,
        w_r_dec = w_r_dec,
        has_l_dec = has_l_dec,
        has_r_dec = has_r_dec,
        has_delim = has_delim,
        full_width = fw
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        comps$open_delim[j],
        .vfloat(
          j,
          comps,
          "l_sign",
          "l_int",
          "l_dec",
          widths$w_l_si,
          widths$w_l_dec,
          widths$has_l_dec
        ),
        ", ",
        .vfloat(
          j,
          comps,
          "r_sign",
          "r_int",
          "r_dec",
          widths$w_r_si,
          widths$w_r_dec,
          widths$has_r_dec
        ),
        comps$close_delim[j]
      )
    },
    rebuild_cross = function(j, comps, widths, fw, dominant_type) {
      if (
        !dominant_type %in%
          c("est_spread", "est_spread_pct", "est_ci", "est_ci_bracket")
      ) {
        return(stringi::stri_pad_right(comps$raw[j], fw))
      }
      l_has_dec <- any(nzchar(comps$l_dec[j]))
      l <- .vfloat(
        j,
        comps,
        "l_sign",
        "l_int",
        "l_dec",
        widths$w_est_si,
        widths$w_est_dec,
        l_has_dec && isTRUE(widths$has_est_dec)
      )
      if (isTRUE(widths$has_est_dec) && !l_has_dec) {
        l <- stringi::stri_pad_right(l, widths$w_est_si + 1L + widths$w_est_dec)
      }
      r <- paste0(comps$r_sign[j], comps$r_int[j])
      has_r_dec <- nzchar(comps$r_dec[j])
      r[has_r_dec] <- paste0(r[has_r_dec], ".", comps$r_dec[j][has_r_dec])
      stringi::stri_pad_right(
        paste0(comps$open_delim[j], l, ", ", r, comps$close_delim[j]),
        fw
      )
    },
    int_slot = "w_l_si"
  )

  e$int_range <- list(
    parse = function(vals) {
      m <- stringi::stri_match_first_regex(
        vals,
        paste0("^(\\d+)\\s+([-\u2013\u2014])\\s+(\\d+)$")
      )
      list(left = m[, 2L], sep = m[, 3L], right = m[, 4L])
    },
    width = function(comps, idx) {
      w_left <- .maxw(comps, "left", idx)
      w_right <- .maxw(comps, "right", idx)
      list(
        w_left = w_left,
        w_right = w_right,
        full_width = w_left + 3L + w_right
      )
    },
    rebuild_dominant = function(j, comps, widths, tp) {
      paste0(
        stringi::stri_pad_left(comps$left[j], widths$w_left),
        " ",
        comps$sep[j],
        " ",
        stringi::stri_pad_left(comps$right[j], widths$w_right)
      )
    },
    rebuild_cross = NULL,
    int_slot = "w_left"
  )

  e
})

#' Parse stat values into a column-oriented component frame (vectorized)
#'
#' Returns a named list of character vectors — one vector per component field,
#' all of length n. Fields irrelevant to a given type default to "".
#' This layout enables vectorized padding: `stri_pad_left(comps$est_int, w)`
#' processes all n values in one C-level call instead of n R function calls.
#'
#' @param values Character vector of trimmed cell values.
#' @param types  Character vector of detected types (same length as values).
#' @return Named list of character vectors, all length n.
#' @noRd
parse_stat_components_vec <- function(values, types) {
  n <- length(values)

  # All component field names. Pre-allocated to "" (zero-cost for unused fields).
  comp_fields <- c(
    "n",
    "sign",
    "int",
    "dec",
    "prefix",
    "est_sign",
    "est_int",
    "est_dec",
    "est_token",
    "lo_sign",
    "lo_int",
    "lo_dec",
    "lo_token",
    "hi_sign",
    "hi_int",
    "hi_dec",
    "hi_token",
    "pv_prefix",
    "pv_int",
    "pv_dec",
    "pv_token",
    "sprd_sign",
    "sprd_int",
    "sprd_dec",
    "pct_prefix",
    "pct_int",
    "pct_dec",
    "pct_sign",
    "num",
    "den",
    "den_int",
    "den_dec",
    "rate_sign",
    "rate_int",
    "rate_dec",
    "ci_lo_sign",
    "ci_lo_int",
    "ci_lo_dec",
    "ci_hi_sign",
    "ci_hi_int",
    "ci_hi_dec",
    "l_sign",
    "l_int",
    "l_dec",
    "r_sign",
    "r_int",
    "r_dec",
    "open_delim",
    "close_delim",
    "left",
    "sep",
    "right"
  )

  empty <- rep("", n)
  out <- c(
    list(type = types, raw = values),
    stats::setNames(lapply(comp_fields, function(.) empty), comp_fields)
  )

  # Fill component vectors by type — one batch regex per type group.
  for (tp in unique(types[!types %in% c("missing", "unknown")])) {
    h <- .decimal_handlers[[tp]]
    idx <- which(types == tp)
    parsed <- h$parse(values[idx])
    for (f in names(parsed)) {
      out[[f]][idx] <- parsed[[f]]
    }
  }

  out
}


#' Compute column-wide max widths per component (vectorized, component frame)
#'
#' Replaces compute_stat_widths() with direct max(nchar(vec)) on character
#' vectors. Preserves all sibling expansion and grid logic.
#'
#' @param comps Component frame from parse_stat_components_vec().
#' @param dominant_type Character.
#' @param types Character vector of all types.
#' @param custom_idx Integer vector or NULL. When provided, use this as the
#'   index set instead of auto-detecting from dominant_type (for subsidiary
#'   width computation).
#' @return Named list with max widths and full_width.
#' @noRd
compute_stat_widths_vec <- function(
  comps,
  dominant_type,
  types,
  custom_idx = NULL
) {
  h <- .decimal_handlers[[dominant_type]]
  if (!is.null(h$primary)) {
    h <- .decimal_handlers[[h$primary]]
  }

  if (!is.null(custom_idx)) {
    idx <- custom_idx
  } else {
    sibs <- h$width_siblings %||% character(0)
    extra <- h$width_subsiblings %||% character(0)
    idx <- which(types %in% c(dominant_type, sibs))
    if (length(extra) > 0L) idx <- c(idx, which(types %in% extra))
  }

  if (length(idx) == 0L) {
    return(list(full_width = max(0L, nchar(comps$raw))))
  }
  h$width(comps, idx)
}


#' Fully vectorized decimal column reconstruction
#'
#' Groups values by type, then applies vectorized stri_pad_left/stri_pad_right/
#' paste0 on each type group. A column of n est_ci values: 1 call to this
#' function = ~12 vectorized ops. Previously: n calls to rebuild_stat_aligned().
#'
#' @param comps  Component frame from parse_stat_components_vec().
#' @param types  Character vector of detected types (length n).
#' @param widths Named list from compute_stat_widths_vec().
#' @param dominant_type Character.
#' @return Character vector of length n. All strings padded to full_width.
#' @noRd
rebuild_column_aligned_vec <- function(comps, types, widths, dominant_type) {
  n <- length(types)
  fw <- widths$full_width
  result <- character(n)

  h_dom <- .decimal_handlers[[dominant_type]]
  if (!is.null(h_dom$primary)) {
    h_dom <- .decimal_handlers[[h_dom$primary]]
  }
  dom_sibs <- h_dom$width_siblings %||% character(0)
  dom_set <- c(dominant_type, dom_sibs)

  for (tp in unique(types[!types %in% c("missing", "unknown")])) {
    j <- which(types == tp)
    h <- .decimal_handlers[[tp]]
    if (!is.null(h$primary)) {
      h <- .decimal_handlers[[h$primary]]
    }
    if (tp %in% dom_set) {
      result[j] <- h$rebuild_dominant(j, comps, widths, tp)
    } else if (!is.null(h$rebuild_cross)) {
      result[j] <- h$rebuild_cross(j, comps, widths, fw, dominant_type)
    } else {
      result[j] <- stringi::stri_pad_right(comps$raw[j], fw)
    }
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Master Column Aligner
# ══════════════════════════════════════════════════════════════════════════════

#' Align all values in a decimal column via pre-formatted padding
#'
#' Pipeline: detect types → dominant type selection → parse into component
#' frame (vectorized) → compute widths (vectorized) → rebuild all (vectorized).
#' Returns a character vector where all strings have the same `nchar`.
#'
#' The rebuild step is fully vectorized: no R function call per value.
#' A column of n est_ci values: ~12 vectorized stri_pad_*/paste0 calls on
#' vectors of length n. Previously: n calls to rebuild_stat_aligned().
#'
#' @param content_vec Character vector of cell values.
#' @return Character vector of aligned strings (all same `nchar`).
#' @noRd
align_decimal_column <- function(content_vec) {
  content_vec <- trimws(content_vec)
  n <- length(content_vec)
  if (n == 0L) {
    return(character(0))
  }

  # STEP 1: Vectorized type detection (one stri_detect_regex per pattern)
  types <- detect_stat_types(content_vec)

  non_skip <- types[!types %in% c("missing", "unknown")]
  if (length(non_skip) == 0L) {
    return(rep("", n))
  }

  # STEP 2: Find dominant type via family-aware priority (unchanged logic)
  families <- .arframe_registry$stat_type_family[non_skip]
  fam_counts <- table(families)

  decimal_fams <- c("estimate", "float", "compound")
  has_decimal <- any(names(fam_counts) %in% decimal_fams)
  if (has_decimal) {
    fam_counts <- fam_counts[names(fam_counts) %in% decimal_fams]
  }

  max_count <- max(fam_counts)
  tied_fams <- names(fam_counts)[fam_counts == max_count]
  dominant_family <- tied_fams[which.max(.arframe_registry$stat_family_priority[
    tied_fams
  ])]

  family_types <- unique(non_skip[families == dominant_family])
  dominant_type <- family_types[which.max(.arframe_registry$stat_type_richness[
    family_types
  ])]

  # STEP 3: Parse into column-oriented component frame (vectorized)
  comps <- parse_stat_components_vec(content_vec, types)

  # STEP 4: Compute column-wide max widths (vectorized)
  widths <- compute_stat_widths_vec(comps, dominant_type, types)

  # STEP 5: Expand integer slot when n_only values exceed dominant int width
  nonly_idx <- which(types == "n_only")
  if (length(nonly_idx) > 0L && dominant_type != "n_only") {
    max_n_w <- max(nchar(comps$n[nonly_idx]))
    int_slot <- .decimal_handlers[[dominant_type]]$int_slot
    if (!is.null(int_slot) && !is.null(widths[[int_slot]])) {
      expansion <- max_n_w - widths[[int_slot]]
      if (expansion > 0L) {
        widths[[int_slot]] <- max_n_w
        widths$full_width <- widths$full_width + expansion
      }
    }
  }

  # STEP 6: Compute subsidiary n_pct widths (for n_pct in non-n_pct columns)
  # Computed directly from component frame — no secondary parse call needed.
  npct_idx <- which(types == "n_pct")
  if (length(npct_idx) > 0L && dominant_type != "n_pct") {
    widths$w_npct_n <- max(0L, nchar(comps$n[npct_idx]))
    widths$w_npct_pct_prefix <- max(0L, nchar(comps$pct_prefix[npct_idx]))
    widths$w_npct_pct_int <- max(0L, nchar(comps$pct_int[npct_idx]))
    widths$w_npct_pct_dec <- max(0L, nchar(comps$pct_dec[npct_idx]))
    widths$w_npct_pct_sign <- max(0L, nchar(comps$pct_sign[npct_idx]))
    widths$has_npct_dec <- any(nzchar(comps$pct_dec[npct_idx]))
  }

  # STEP 7: Vectorized rebuild — no R function call per value
  result <- rebuild_column_aligned_vec(comps, types, widths, dominant_type)

  # STEP 8: Ensure uniform nchar (pad any shorter strings on the right)
  max_nc <- max(nchar(result))
  if (max_nc > 0L) {
    result <- stringi::stri_pad_right(result, max_nc)
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Integration with finalize_spec()
# ══════════════════════════════════════════════════════════════════════════════

#' Compute global decimal geometry (single alignment for all values)
#'
#' @param vals Character vector of cell values.
#' @param col_width_twips Column width in twips.
#' @param space_twips Width of a space character in twips.
#' @return List with `formatted`, `center_offset`, and `max_width`.
#' @noRd
global_decimal_geom <- function(vals, col_width_twips, space_twips) {
  formatted <- align_decimal_column(vals)
  if (length(formatted) > 0L && any(nzchar(formatted))) {
    max_width <- round(max(nchar(formatted)) * space_twips)
    scalar_offset <- max(0L, round((col_width_twips - max_width) / 2))
  } else {
    max_width <- 0L
    scalar_offset <- 0L
  }
  list(
    formatted = formatted,
    center_offset = rep(scalar_offset, length(formatted)),
    max_width = max_width
  )
}


#' Pre-compute decimal geometry for all decimal-aligned columns
#'
#' Called during `finalize_spec()` to pre-compute geometry for all columns
#' with `align = "decimal"`. The result is stored on `spec$decimal_geometry`
#' and used by both RTF and LaTeX backends.
#'
#' @param spec An fr_spec object (post-finalize_columns).
#' @return Named list of geometry objects (one per decimal column), or NULL.
#' @noRd
compute_all_decimal_geometry <- function(spec) {
  columns <- spec$columns
  col_names <- names(columns)

  # Early exit: skip loop if no decimal columns
  has_decimal <- vapply(
    columns,
    function(col) identical(col$align, "decimal"),
    logical(1)
  )
  if (!any(has_decimal)) {
    return(NULL)
  }

  result <- list()

  # Hoist invariants outside the column loop.
  # Only page_by creates alignment boundaries (separate physical pages).
  # group_by groups are visible together on the same page and must align
  # consistently across the entire column.
  pb <- spec$body$page_by
  align_key <- if (length(pb) > 0L) pb else character(0)
  space_twips <- measure_text_width_twips(
    " ",
    spec$page$font_family,
    spec$page$font_size
  )

  # Pre-compute page labels once (shared across all decimal columns)
  has_align_key <- length(align_key) > 0L &&
    all(align_key %in% names(spec$data))
  if (has_align_key) {
    page_labels <- build_group_keys(spec$data, align_key)
    unique_pages <- unique(page_labels)
  }

  for (nm in col_names) {
    col <- columns[[nm]]
    if (!identical(col$align, "decimal")) {
      next
    }
    if (!nm %in% names(spec$data)) {
      next
    }

    vals <- na_to_empty(as.character(spec$data[[nm]]))

    col_width_twips <- inches_to_twips(col$width)

    # Decide: per-page or global alignment
    use_per_page <- FALSE
    if (has_align_key && length(unique_pages) > 1L) {
      # Smart check: compare stat type signatures across pages.
      # Exclude filler types (n_only, missing, unknown) that appear in every page.
      type_sigs <- vapply(
        unique_pages,
        function(pg) {
          pg_types <- detect_stat_types(vals[page_labels == pg])
          sig_types <- setdiff(pg_types, .arframe_const$stat_sig_skip)
          paste(sort(unique(sig_types)), collapse = ",")
        },
        character(1)
      )
      use_per_page <- length(unique(type_sigs)) > 1L
    }

    if (use_per_page) {
      formatted <- character(length(vals))
      page_max_nc <- integer(length(vals))
      for (pg in unique_pages) {
        mask <- page_labels == pg
        pg_formatted <- align_decimal_column(vals[mask])
        formatted[mask] <- pg_formatted
        pg_nc <- if (any(nzchar(pg_formatted))) {
          max(nchar(pg_formatted))
        } else {
          0L
        }
        page_max_nc[mask] <- pg_nc
      }

      # Per-row center_offset (vectorized: one offset per page width)
      widths_twips <- round(page_max_nc * space_twips)
      center_offset <- pmax(
        0L,
        round((col_width_twips - widths_twips) / 2)
      )
      max_width_twips <- round(max(page_max_nc) * space_twips)
    } else {
      geom <- global_decimal_geom(vals, col_width_twips, space_twips)
      formatted <- geom$formatted
      center_offset <- geom$center_offset
      max_width_twips <- geom$max_width
    }

    result[[nm]] <- list(
      formatted = formatted,
      center_offset = center_offset, # Integer VECTOR (one per row)
      max_width = max_width_twips
    )
  }

  if (length(result) == 0L) NULL else result
}
