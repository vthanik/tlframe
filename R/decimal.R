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

  for (i in seq_along(fr_env$stat_type_patterns)) {
    if (!any(unassigned)) {
      break
    }
    matches <- stringi::stri_detect_regex(
      content_vec,
      fr_env$stat_type_patterns[[i]]
    )
    hits <- unassigned & matches
    if (any(hits)) {
      result[hits] <- names(fr_env$stat_type_patterns)[[i]]
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
  fr_env$num_or_tok_cap,
  "\\s*\\(\\s*",
  fr_env$num_or_tok_cap,
  "\\s*,\\s*",
  fr_env$num_or_tok_cap,
  "\\s*\\)$"
)

#' Token-aware est_ci_bracket parse regex (12 groups)
#' @noRd
est_ci_bracket_parse_re <- paste0(
  "^",
  fr_env$num_or_tok_cap,
  "\\s*\\[\\s*",
  fr_env$num_or_tok_cap,
  "\\s*,\\s*",
  fr_env$num_or_tok_cap,
  "\\s*\\]$"
)

#' est_ci_pval parse regex (16 groups: 4 × 3 CI + 4 pval)
#' @noRd
est_ci_pval_parse_re <- paste0(
  "^",
  fr_env$num_or_tok_cap,
  "\\s*\\(\\s*",
  fr_env$num_or_tok_cap,
  "\\s*,\\s*",
  fr_env$num_or_tok_cap,
  "\\s*\\)\\s+",
  fr_env$pval_or_tok_cap,
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
    idx <- which(types == tp)
    vals <- values[idx]

    switch(
      tp,

      n_only = {
        out$n[idx] <- vals
      },

      scalar_float = {
        m <- stringi::stri_match_first_regex(vals, "^(-?)(\\d+)\\.(\\d+)$")
        out$sign[idx] <- m[, 2L]
        out$int[idx] <- m[, 3L]
        out$dec[idx] <- m[, 4L]
      },

      pvalue = {
        m <- stringi::stri_match_first_regex(vals, "^([<>=])(\\d+)\\.(\\d+)$")
        out$prefix[idx] <- m[, 2L]
        out$int[idx] <- m[, 3L]
        out$dec[idx] <- m[, 4L]
      },

      n_pct = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
        )
        out$n[idx] <- m[, 2L]
        out$pct_prefix[idx] <- m[, 3L]
        out$pct_int[idx] <- m[, 4L]
        out$pct_dec[idx] <- m[, 5L]
        out$pct_sign[idx] <- m[, 6L]
      },

      n_over_N_pct = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
        )
        out$num[idx] <- m[, 2L]
        out$den[idx] <- m[, 3L]
        out$pct_prefix[idx] <- m[, 4L]
        out$pct_int[idx] <- m[, 5L]
        out$pct_dec[idx] <- m[, 6L]
        out$pct_sign[idx] <- m[, 7L]
      },

      est_spread = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
        )
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$sprd_sign[idx] <- m[, 5L]
        out$sprd_int[idx] <- m[, 6L]
        out$sprd_dec[idx] <- m[, 7L]
      },

      est_spread_pct = {
        m <- stringi::stri_match_first_regex(vals, est_spread_pct_parse_re)
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$sprd_sign[idx] <- m[, 5L]
        out$sprd_int[idx] <- m[, 6L]
        out$sprd_dec[idx] <- m[, 7L]
      },

      est_ci = {
        m <- na_to_empty(stringi::stri_match_first_regex(vals, est_ci_parse_re))
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$est_token[idx] <- m[, 5L]
        out$lo_sign[idx] <- m[, 6L]
        out$lo_int[idx] <- m[, 7L]
        out$lo_dec[idx] <- m[, 8L]
        out$lo_token[idx] <- m[, 9L]
        out$hi_sign[idx] <- m[, 10L]
        out$hi_int[idx] <- m[, 11L]
        out$hi_dec[idx] <- m[, 12L]
        out$hi_token[idx] <- m[, 13L]
      },

      est_ci_bracket = {
        m <- na_to_empty(stringi::stri_match_first_regex(
          vals,
          est_ci_bracket_parse_re
        ))
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$est_token[idx] <- m[, 5L]
        out$lo_sign[idx] <- m[, 6L]
        out$lo_int[idx] <- m[, 7L]
        out$lo_dec[idx] <- m[, 8L]
        out$lo_token[idx] <- m[, 9L]
        out$hi_sign[idx] <- m[, 10L]
        out$hi_int[idx] <- m[, 11L]
        out$hi_dec[idx] <- m[, 12L]
        out$hi_token[idx] <- m[, 13L]
      },

      est_ci_pval = {
        m <- na_to_empty(stringi::stri_match_first_regex(
          vals,
          est_ci_pval_parse_re
        ))
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$est_token[idx] <- m[, 5L]
        out$lo_sign[idx] <- m[, 6L]
        out$lo_int[idx] <- m[, 7L]
        out$lo_dec[idx] <- m[, 8L]
        out$lo_token[idx] <- m[, 9L]
        out$hi_sign[idx] <- m[, 10L]
        out$hi_int[idx] <- m[, 11L]
        out$hi_dec[idx] <- m[, 12L]
        out$hi_token[idx] <- m[, 13L]
        out$pv_prefix[idx] <- m[, 14L]
        out$pv_int[idx] <- m[, 15L]
        out$pv_dec[idx] <- m[, 16L]
        out$pv_token[idx] <- m[, 17L]
      },

      est_spread_pct_ci = {
        m <- stringi::stri_match_first_regex(vals, est_spread_pct_ci_parse_re)
        out$est_sign[idx] <- m[, 2L]
        out$est_int[idx] <- m[, 3L]
        out$est_dec[idx] <- m[, 4L]
        out$sprd_sign[idx] <- m[, 5L]
        out$sprd_int[idx] <- m[, 6L]
        out$sprd_dec[idx] <- m[, 7L]
        out$ci_lo_sign[idx] <- m[, 8L]
        out$ci_lo_int[idx] <- m[, 9L]
        out$ci_lo_dec[idx] <- m[, 10L]
        out$ci_hi_sign[idx] <- m[, 11L]
        out$ci_hi_int[idx] <- m[, 12L]
        out$ci_hi_dec[idx] <- m[, 13L]
      },

      n_pct_rate = {
        m <- stringi::stri_match_first_regex(vals, n_pct_rate_parse_re)
        out$n[idx] <- m[, 2L]
        out$pct_prefix[idx] <- m[, 3L]
        out$pct_int[idx] <- m[, 4L]
        out$pct_dec[idx] <- m[, 5L]
        out$pct_sign[idx] <- m[, 6L]
        out$rate_sign[idx] <- m[, 7L]
        out$rate_int[idx] <- m[, 8L]
        out$rate_dec[idx] <- m[, 9L]
      },

      n_over_N_pct_ci = {
        m <- stringi::stri_match_first_regex(vals, n_over_N_pct_ci_parse_re)
        out$num[idx] <- m[, 2L]
        out$den[idx] <- m[, 3L]
        out$pct_prefix[idx] <- m[, 4L]
        out$pct_int[idx] <- m[, 5L]
        out$pct_dec[idx] <- m[, 6L]
        out$pct_sign[idx] <- m[, 7L]
        out$ci_lo_sign[idx] <- m[, 8L]
        out$ci_lo_int[idx] <- m[, 9L]
        out$ci_lo_dec[idx] <- m[, 10L]
        out$ci_hi_sign[idx] <- m[, 11L]
        out$ci_hi_int[idx] <- m[, 12L]
        out$ci_hi_dec[idx] <- m[, 13L]
      },

      n_over_N = {
        m <- stringi::stri_match_first_regex(vals, "^(\\d+)\\s*/\\s*(\\d+)$")
        out$num[idx] <- m[, 2L]
        out$den[idx] <- m[, 3L]
      },

      n_over_float = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*/\\s*(\\d+)\\.(\\d+)$"
        )
        out$num[idx] <- m[, 2L]
        out$den_int[idx] <- m[, 3L]
        out$den_dec[idx] <- m[, 4L]
      },

      range_pair = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^[\\(\\[]?\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*[\\)\\]]?$"
        )
        out$l_sign[idx] <- m[, 2L]
        out$l_int[idx] <- m[, 3L]
        out$l_dec[idx] <- m[, 4L]
        out$r_sign[idx] <- m[, 5L]
        out$r_int[idx] <- m[, 6L]
        out$r_dec[idx] <- m[, 7L]
        out$open_delim[idx] <- ifelse(
          grepl("^\\s*\\(", vals),
          "(",
          ifelse(grepl("^\\s*\\[", vals), "[", "")
        )
        out$close_delim[idx] <- ifelse(
          grepl("\\)\\s*$", vals),
          ")",
          ifelse(grepl("\\]\\s*$", vals), "]", "")
        )
      },

      int_range = {
        m <- stringi::stri_match_first_regex(
          vals,
          paste0("^(\\d+)\\s+([-\u2013\u2014])\\s+(\\d+)$")
        )
        out$left[idx] <- m[, 2L]
        out$sep[idx] <- m[, 3L]
        out$right[idx] <- m[, 4L]
      }
    )
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
  if (!is.null(custom_idx)) {
    idx <- custom_idx
  } else {
    # Sibling expansion: types sharing the same field structure
    same_sibling <- switch(
      dominant_type,
      est_spread_pct = "est_spread",
      est_spread = "est_spread_pct",
      est_ci = "est_ci_bracket",
      est_ci_bracket = "est_ci",
      NULL
    )
    type_set <- c(dominant_type, same_sibling)
    idx <- which(types %in% type_set)

    # Subset sibling: n_over_N contributes to n_over_N_pct grid
    if (dominant_type == "n_over_N_pct") {
      idx <- c(idx, which(types == "n_over_N"))
    }
  }

  if (length(idx) == 0L) {
    raw_w <- nchar(comps$raw)
    return(list(full_width = max(0L, raw_w)))
  }

  # Vectorized helpers closed over comps and idx
  maxw <- function(f) max(0L, nchar(comps[[f]][idx]))
  maxw_si <- function(intf, signf) {
    max(0L, nchar(paste0(comps[[signf]][idx], comps[[intf]][idx])))
  }
  maxw_tok_si <- function(tokf, signf, intf) {
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
  has_any <- function(f) any(nzchar(comps[[f]][idx]))

  switch(
    dominant_type,

    n_only = {
      w_n <- maxw("n")
      list(w_n = w_n, full_width = w_n)
    },

    scalar_float = {
      w_si <- maxw_si("int", "sign")
      w_dec <- maxw("dec")
      list(
        w_sign_int = w_si,
        w_dec = w_dec,
        has_dec = TRUE,
        full_width = w_si + 1L + w_dec
      )
    },

    pvalue = {
      w_pfx <- maxw("prefix")
      w_int <- maxw("int")
      w_dec <- maxw("dec")
      list(
        w_prefix = w_pfx,
        w_int = w_int,
        w_dec = w_dec,
        full_width = w_pfx + w_int + 1L + w_dec
      )
    },

    n_pct = {
      w_n <- maxw("n")
      w_pp <- maxw("pct_prefix")
      w_pi <- maxw("pct_int")
      w_pd <- maxw("pct_dec")
      w_ps <- maxw("pct_sign")
      has_dec <- has_any("pct_dec")
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

    n_over_N_pct = {
      w_num <- maxw("num")
      w_den <- maxw("den")
      w_pp <- maxw("pct_prefix")
      w_pi <- maxw("pct_int")
      w_pd <- maxw("pct_dec")
      w_ps <- maxw("pct_sign")
      has_dec <- has_any("pct_dec")
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

    est_spread = ,
    est_spread_pct = {
      w_est_si <- maxw_si("est_int", "est_sign")
      w_est_dec <- maxw("est_dec")
      w_sprd_si <- maxw_si("sprd_int", "sprd_sign")
      has_est_dec <- has_any("est_dec")
      has_sprd_dec <- has_any("sprd_dec")
      # Dec slot: nchar(dec) + 1 for pct suffix, nchar(dec) otherwise
      is_pct_type <- comps$type[idx] == "est_spread_pct"
      dec_w <- nchar(comps$sprd_dec[idx])
      dec_w[is_pct_type] <- dec_w[is_pct_type] + 1L
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

    est_ci = ,
    est_ci_bracket = {
      w_est_si <- maxw_tok_si("est_token", "est_sign", "est_int")
      w_est_dec <- maxw("est_dec")
      w_lo_si <- maxw_tok_si("lo_token", "lo_sign", "lo_int")
      w_lo_dec <- maxw("lo_dec")
      w_hi_si <- maxw_tok_si("hi_token", "hi_sign", "hi_int")
      w_hi_dec <- maxw("hi_dec")
      has_est_dec <- has_any("est_dec")
      has_lo_dec <- has_any("lo_dec")
      has_hi_dec <- has_any("hi_dec")
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

    n_over_N = {
      w_num <- maxw("num")
      w_den <- maxw("den")
      list(w_num = w_num, w_den = w_den, full_width = w_num + 1L + w_den)
    },

    n_over_float = {
      w_num <- maxw("num")
      w_di <- maxw("den_int")
      w_dd <- maxw("den_dec")
      list(
        w_num = w_num,
        w_den_int = w_di,
        w_den_dec = w_dd,
        full_width = w_num + 1L + w_di + 1L + w_dd
      )
    },

    est_ci_pval = {
      w_est_si <- maxw_tok_si("est_token", "est_sign", "est_int")
      w_est_dec <- maxw("est_dec")
      w_lo_si <- maxw_tok_si("lo_token", "lo_sign", "lo_int")
      w_lo_dec <- maxw("lo_dec")
      w_hi_si <- maxw_tok_si("hi_token", "hi_sign", "hi_int")
      w_hi_dec <- maxw("hi_dec")
      has_est_dec <- has_any("est_dec")
      has_lo_dec <- has_any("lo_dec")
      has_hi_dec <- has_any("hi_dec")
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
      w_pv_pi <- maxw_tok_si("pv_token", "pv_prefix", "pv_int")
      w_pv_dec <- maxw("pv_dec")
      has_pv_dec <- has_any("pv_dec")
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
        gap = fr_env$compound_gap,
        full_width = ci_fw + fr_env$compound_gap + pv_fw
      )
    },

    n_pct_rate = {
      w_n <- maxw("n")
      w_pp <- maxw("pct_prefix")
      w_pi <- maxw("pct_int")
      w_pd <- maxw("pct_dec")
      w_ps <- maxw("pct_sign")
      has_dec <- has_any("pct_dec")
      npct_fw <- w_n + 2L + w_pp + w_pi
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pd
      }
      npct_fw <- npct_fw + w_ps + 1L
      w_rate_si <- maxw_si("rate_int", "rate_sign")
      w_rate_dec <- maxw("rate_dec")
      has_rate_dec <- has_any("rate_dec")
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
        gap = fr_env$compound_gap,
        full_width = npct_fw + fr_env$compound_gap + rate_fw
      )
    },

    n_over_N_pct_ci = {
      w_num <- maxw("num")
      w_den <- maxw("den")
      w_pp <- maxw("pct_prefix")
      w_pi <- maxw("pct_int")
      w_pd <- maxw("pct_dec")
      w_ps <- maxw("pct_sign")
      has_dec <- has_any("pct_dec")
      npct_fw <- w_num + 1L + w_den + 2L + w_pp + w_pi
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pd
      }
      npct_fw <- npct_fw + w_ps + 1L
      w_ci_lo_si <- maxw_si("ci_lo_int", "ci_lo_sign")
      w_ci_lo_dec <- maxw("ci_lo_dec")
      w_ci_hi_si <- maxw_si("ci_hi_int", "ci_hi_sign")
      w_ci_hi_dec <- maxw("ci_hi_dec")
      has_ci_lo_dec <- has_any("ci_lo_dec")
      has_ci_hi_dec <- has_any("ci_hi_dec")
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

    est_spread_pct_ci = {
      w_est_si <- maxw_si("est_int", "est_sign")
      w_est_dec <- maxw("est_dec")
      w_sprd_si <- maxw_si("sprd_int", "sprd_sign")
      w_sprd_dec_slot <- maxw("sprd_dec") + 1L
      has_est_dec <- has_any("est_dec")
      has_sprd_dec <- has_any("sprd_dec")
      espct_fw <- w_est_si
      if (has_est_dec) {
        espct_fw <- espct_fw + 1L + w_est_dec
      }
      espct_fw <- espct_fw + 2L + w_sprd_si
      if (has_sprd_dec) {
        espct_fw <- espct_fw + 1L + w_sprd_dec_slot
      }
      espct_fw <- espct_fw + 1L
      w_ci_lo_si <- maxw_si("ci_lo_int", "ci_lo_sign")
      w_ci_lo_dec <- maxw("ci_lo_dec")
      w_ci_hi_si <- maxw_si("ci_hi_int", "ci_hi_sign")
      w_ci_hi_dec <- maxw("ci_hi_dec")
      has_ci_lo_dec <- has_any("ci_lo_dec")
      has_ci_hi_dec <- has_any("ci_hi_dec")
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
        gap = fr_env$compound_gap,
        full_width = espct_fw + fr_env$compound_gap + ci_fw
      )
    },

    range_pair = {
      w_l_si <- maxw_si("l_int", "l_sign")
      w_l_dec <- maxw("l_dec")
      w_r_si <- maxw_si("r_int", "r_sign")
      w_r_dec <- maxw("r_dec")
      has_l_dec <- has_any("l_dec")
      has_r_dec <- has_any("r_dec")
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

    int_range = {
      w_left <- maxw("left")
      w_right <- maxw("right")
      list(
        w_left = w_left,
        w_right = w_right,
        full_width = w_left + 3L + w_right
      )
    },

    list(full_width = 0L)
  )
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

  # ── Vectorized inner helpers (close over comps) ─────────────────────────

  # Pad sign+int right-justified, optional ".dec" left-justified
  vfloat <- function(j, sf, intf, decf, w_si, w_dec, has_dec) {
    si <- stringi::stri_pad_left(paste0(comps[[sf]][j], comps[[intf]][j]), w_si)
    if (has_dec) {
      paste0(si, ".", stringi::stri_pad_right(comps[[decf]][j], w_dec))
    } else {
      si
    }
  }

  # Pad token-or-float position (token takes precedence if non-empty)
  vtokfloat <- function(j, tokf, sf, intf, decf, w_si, w_dec, has_dec) {
    tok <- comps[[tokf]][j]
    has_tok <- nzchar(tok)
    out <- character(length(j))
    if (any(has_tok)) {
      p <- stringi::stri_pad_left(tok[has_tok], w_si)
      out[has_tok] <- if (has_dec) paste0(p, strrep(" ", 1L + w_dec)) else p
    }
    if (any(!has_tok)) {
      out[!has_tok] <- vfloat(
        j[!has_tok],
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

  # Pad a percentage part (prefix+int right, optional .dec, sign right)
  vpct <- function(j, w) {
    pct <- stringi::stri_pad_left(
      paste0(comps$pct_prefix[j], comps$pct_int[j]),
      w$w_pct_prefix + w$w_pct_int
    )
    if (isTRUE(w$has_dec)) {
      pct <- paste0(
        pct,
        ".",
        stringi::stri_pad_right(comps$pct_dec[j], w$w_pct_dec)
      )
    }
    paste0(pct, stringi::stri_pad_right(comps$pct_sign[j], w$w_pct_sign))
  }

  # Build CI string with chosen delimiters
  vci <- function(j, open_ch, close_ch) {
    est <- vtokfloat(
      j,
      "est_token",
      "est_sign",
      "est_int",
      "est_dec",
      widths$w_est_si,
      widths$w_est_dec,
      widths$has_est_dec
    )
    lo <- vtokfloat(
      j,
      "lo_token",
      "lo_sign",
      "lo_int",
      "lo_dec",
      widths$w_lo_si,
      widths$w_lo_dec,
      widths$has_lo_dec
    )
    hi <- vtokfloat(
      j,
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

  # ── Process each type group ─────────────────────────────────────────────

  # Dominant type set (includes same-structure siblings)
  dom_set <- c(
    dominant_type,
    switch(
      dominant_type,
      est_spread_pct = "est_spread",
      est_spread = "est_spread_pct",
      est_ci = "est_ci_bracket",
      est_ci_bracket = "est_ci",
      character(0)
    )
  )

  for (tp in unique(types[!types %in% c("missing", "unknown")])) {
    j <- which(types == tp)

    if (tp %in% dom_set) {
      # ── Dominant / sibling type: vectorized rebuild ──────────────────
      result[j] <- switch(
        tp,

        n_only = stringi::stri_pad_left(comps$n[j], widths$w_n),

        scalar_float = vfloat(
          j,
          "sign",
          "int",
          "dec",
          widths$w_sign_int,
          widths$w_dec,
          TRUE
        ),

        pvalue = {
          pfx <- stringi::stri_pad_left(comps$prefix[j], widths$w_prefix)
          int <- stringi::stri_pad_left(comps$int[j], widths$w_int)
          dec <- stringi::stri_pad_right(comps$dec[j], widths$w_dec)
          paste0(pfx, int, ".", dec)
        },

        n_pct = {
          pn <- stringi::stri_pad_left(comps$n[j], widths$w_n)
          n_val <- suppressWarnings(as.integer(comps$n[j]))
          is_zero <- !is.na(n_val) & n_val == 0L
          pct <- vpct(j, widths)
          r <- paste0(pn, " (", pct, ")")
          r[is_zero] <- stringi::stri_pad_right(pn[is_zero], fw)
          r
        },

        n_over_N_pct = {
          num <- stringi::stri_pad_left(comps$num[j], widths$w_num)
          den <- stringi::stri_pad_right(comps$den[j], widths$w_den)
          paste0(num, "/", den, " (", vpct(j, widths), ")")
        },

        est_spread = ,
        est_spread_pct = {
          est <- vfloat(
            j,
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
            sprd <- if (tp == "est_spread_pct") {
              paste0(si_str, "%")
            } else {
              si_str
            }
          }
          paste0(est, " (", sprd, ")")
        },

        est_ci = vci(j, "(", ")"),
        est_ci_bracket = vci(j, "[", "]"),

        est_ci_pval = {
          ci_str <- vci(j, "(", ")")
          pv <- vtokfloat(
            j,
            "pv_token",
            "pv_prefix",
            "pv_int",
            "pv_dec",
            widths$w_pv_pi,
            widths$w_pv_dec,
            widths$has_pv_dec
          )
          paste0(ci_str, strrep(" ", widths$gap), pv)
        },

        n_pct_rate = {
          pn <- stringi::stri_pad_left(comps$n[j], widths$w_n)
          n_val <- suppressWarnings(as.integer(comps$n[j]))
          is_zero <- !is.na(n_val) & n_val == 0L
          pct <- vpct(j, widths)
          npct <- paste0(pn, " (", pct, ")")
          rate <- vfloat(
            j,
            "rate_sign",
            "rate_int",
            "rate_dec",
            widths$w_rate_si,
            widths$w_rate_dec,
            widths$has_rate_dec
          )
          r <- paste0(npct, strrep(" ", widths$gap), rate)
          r[is_zero] <- stringi::stri_pad_right(pn[is_zero], fw)
          r
        },

        n_over_N = {
          num <- stringi::stri_pad_left(comps$num[j], widths$w_num)
          den <- stringi::stri_pad_right(comps$den[j], widths$w_den)
          paste0(num, "/", den)
        },

        n_over_float = {
          num <- stringi::stri_pad_left(comps$num[j], widths$w_num)
          di <- stringi::stri_pad_left(comps$den_int[j], widths$w_den_int)
          dd <- stringi::stri_pad_right(comps$den_dec[j], widths$w_den_dec)
          paste0(num, "/", di, ".", dd)
        },

        n_over_N_pct_ci = {
          num <- stringi::stri_pad_left(comps$num[j], widths$w_num)
          den <- stringi::stri_pad_right(comps$den[j], widths$w_den)
          pct <- vpct(j, widths)
          npct_str <- paste0(num, "/", den, " (", pct, ")")
          ci_lo <- vfloat(
            j,
            "ci_lo_sign",
            "ci_lo_int",
            "ci_lo_dec",
            widths$w_ci_lo_si,
            widths$w_ci_lo_dec,
            widths$has_ci_lo_dec
          )
          ci_hi <- vfloat(
            j,
            "ci_hi_sign",
            "ci_hi_int",
            "ci_hi_dec",
            widths$w_ci_hi_si,
            widths$w_ci_hi_dec,
            widths$has_ci_hi_dec
          )
          paste0(
            npct_str,
            strrep(" ", widths$gap),
            "[",
            ci_lo,
            ", ",
            ci_hi,
            "]"
          )
        },

        est_spread_pct_ci = {
          est <- vfloat(
            j,
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
            dec_str <- paste0(comps$sprd_dec[j], "%")
            sprd <- paste0(
              si_str,
              ".",
              stringi::stri_pad_right(dec_str, widths$w_sprd_dec_slot)
            )
          } else {
            sprd <- paste0(si_str, "%")
          }
          espct <- paste0(est, " (", sprd, ")")
          ci_lo <- vfloat(
            j,
            "ci_lo_sign",
            "ci_lo_int",
            "ci_lo_dec",
            widths$w_ci_lo_si,
            widths$w_ci_lo_dec,
            widths$has_ci_lo_dec
          )
          ci_hi <- vfloat(
            j,
            "ci_hi_sign",
            "ci_hi_int",
            "ci_hi_dec",
            widths$w_ci_hi_si,
            widths$w_ci_hi_dec,
            widths$has_ci_hi_dec
          )
          paste0(espct, strrep(" ", widths$gap), "(", ci_lo, ", ", ci_hi, ")")
        },

        range_pair = {
          l <- vfloat(
            j,
            "l_sign",
            "l_int",
            "l_dec",
            widths$w_l_si,
            widths$w_l_dec,
            widths$has_l_dec
          )
          r <- vfloat(
            j,
            "r_sign",
            "r_int",
            "r_dec",
            widths$w_r_si,
            widths$w_r_dec,
            widths$has_r_dec
          )
          paste0(comps$open_delim[j], l, ", ", r, comps$close_delim[j])
        },

        int_range = {
          l <- stringi::stri_pad_left(comps$left[j], widths$w_left)
          r <- stringi::stri_pad_left(comps$right[j], widths$w_right)
          paste0(l, " ", comps$sep[j], " ", r)
        },

        stringi::stri_pad_right(comps$raw[j], fw)
      )
    } else {
      # ── Non-dominant type: cross-type alignment fallback (vectorized) ──

      result[j] <- switch(
        tp,

        n_only = {
          int_slot_w <- switch(
            dominant_type,
            n_pct = ,
            n_pct_rate = widths$w_n,
            n_over_N_pct = ,
            n_over_N_pct_ci = ,
            n_over_N = ,
            n_over_float = widths$w_num %||% widths$w_n,
            est_spread = ,
            est_spread_pct = ,
            est_ci = ,
            est_ci_bracket = ,
            est_ci_pval = ,
            est_spread_pct_ci = widths$w_est_si,
            scalar_float = widths$w_sign_int,
            pvalue = widths$w_int,
            range_pair = widths$w_l_si,
            int_range = widths$w_left,
            NULL
          )
          if (!is.null(int_slot_w)) {
            pn <- stringi::stri_pad_left(comps$n[j], int_slot_w)
            # Space-fill decimal zone for decimal-bearing dominant types
            dec_w <- switch(
              dominant_type,
              est_spread = ,
              est_spread_pct = ,
              est_ci = ,
              est_ci_bracket = ,
              est_ci_pval = ,
              est_spread_pct_ci = if (isTRUE(widths$has_est_dec)) {
                1L + widths$w_est_dec
              } else {
                0L
              },
              scalar_float = if (isTRUE(widths$has_dec)) {
                1L + widths$w_dec
              } else {
                0L
              },
              pvalue = 1L + widths$w_dec,
              0L
            )
            if (dec_w > 0L) {
              pn <- stringi::stri_pad_right(pn, int_slot_w + dec_w)
            }
            stringi::stri_pad_right(pn, fw)
          } else {
            stringi::stri_pad_right(comps$n[j], fw)
          }
        },

        scalar_float = {
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
            est <- vfloat(
              j,
              "sign",
              "int",
              "dec",
              widths$w_est_si,
              widths$w_est_dec,
              widths$has_est_dec
            )
            stringi::stri_pad_right(est, fw)
          } else if (dominant_type == "n_pct") {
            w_n <- widths$w_npct_n %||% widths$w_n
            r <- vfloat(
              j,
              "sign",
              "int",
              "dec",
              w_n,
              max(nchar(comps$dec[j])),
              any(nzchar(comps$dec[j]))
            )
            stringi::stri_pad_right(r, fw)
          } else if (dominant_type == "pvalue") {
            # Align decimal of scalar_float with pvalue's decimal position.
            # pvalue format: [prefix][int].[dec] — so prefix+int occupies
            # w_prefix + w_int characters before the dot.
            w_si <- (widths$w_prefix %||% 0L) + (widths$w_int %||% 0L)
            vfloat(j, "sign", "int", "dec", w_si, widths$w_dec, TRUE)
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        pvalue = {
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
            est <- vfloat(j, "prefix", "int", "dec", w_si, widths$w_dec, TRUE)
            stringi::stri_pad_right(est, fw)
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        n_pct = {
          if (!is.null(widths$w_npct_n)) {
            # When dominant is n_over_N_pct or n_over_N_pct_ci, align "(" by
            # padding n to fill the "num/den" prefix width of the dominant type.
            pn <- if (dominant_type %in% c("n_over_N_pct", "n_over_N_pct_ci")) {
              prefix_w <- (widths$w_num %||% 0L) + 1L + (widths$w_den %||% 0L)
              stringi::stri_pad_right(comps$n[j], prefix_w)
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
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        n_over_N = {
          if (dominant_type == "n_over_N_pct") {
            num <- stringi::stri_pad_left(comps$num[j], widths$w_num)
            stringi::stri_pad_right(num, fw)
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        est_spread = ,
        est_spread_pct = {
          if (dominant_type %in% c("est_ci", "est_ci_bracket")) {
            est <- vfloat(
              j,
              "est_sign",
              "est_int",
              "est_dec",
              widths$w_est_si,
              widths$w_est_dec,
              widths$has_est_dec
            )
            sprd <- vfloat(
              j,
              "sprd_sign",
              "sprd_int",
              "sprd_dec",
              widths$w_lo_si,
              widths$w_lo_dec,
              widths$has_lo_dec
            )
            open_ch <- if (dominant_type == "est_ci") "(" else "["
            close_ch <- if (dominant_type == "est_ci") ")" else "]"
            stringi::stri_pad_right(
              paste0(est, " ", open_ch, sprd, close_ch),
              fw
            )
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        range_pair = {
          if (
            dominant_type %in%
              c(
                "est_spread",
                "est_spread_pct",
                "est_ci",
                "est_ci_bracket"
              )
          ) {
            l_has_dec <- any(nzchar(comps$l_dec[j]))
            l <- vfloat(
              j,
              "l_sign",
              "l_int",
              "l_dec",
              widths$w_est_si,
              widths$w_est_dec,
              l_has_dec && isTRUE(widths$has_est_dec)
            )
            if (isTRUE(widths$has_est_dec) && !l_has_dec) {
              l <- stringi::stri_pad_right(
                l,
                widths$w_est_si + 1L + widths$w_est_dec
              )
            }
            r <- paste0(comps$r_sign[j], comps$r_int[j])
            has_r_dec <- nzchar(comps$r_dec[j])
            r[has_r_dec] <- paste0(r[has_r_dec], ".", comps$r_dec[j][has_r_dec])
            od <- comps$open_delim[j]
            cd <- comps$close_delim[j]
            stringi::stri_pad_right(paste0(od, l, ", ", r, cd), fw)
          } else {
            stringi::stri_pad_right(comps$raw[j], fw)
          }
        },

        # Missing/unknown already handled; everything else raw-padded
        missing = ,
        unknown = strrep(" ", fw),

        stringi::stri_pad_right(comps$raw[j], fw)
      )
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
  families <- fr_env$stat_type_family[non_skip]
  fam_counts <- table(families)

  decimal_fams <- c("estimate", "float", "compound")
  has_decimal <- any(names(fam_counts) %in% decimal_fams)
  if (has_decimal) {
    fam_counts <- fam_counts[names(fam_counts) %in% decimal_fams]
  }

  max_count <- max(fam_counts)
  tied_fams <- names(fam_counts)[fam_counts == max_count]
  dominant_family <- tied_fams[which.max(fr_env$stat_family_priority[
    tied_fams
  ])]

  family_types <- unique(non_skip[families == dominant_family])
  dominant_type <- family_types[which.max(fr_env$stat_type_richness[
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
    int_slot <- switch(
      dominant_type,
      n_pct = ,
      n_pct_rate = "w_n",
      n_over_N_pct = ,
      n_over_N_pct_ci = ,
      n_over_N = ,
      n_over_float = "w_num",
      est_spread = ,
      est_spread_pct = ,
      est_ci = ,
      est_ci_bracket = ,
      est_ci_pval = ,
      est_spread_pct_ci = "w_est_si",
      scalar_float = "w_sign_int",
      pvalue = "w_int",
      range_pair = "w_l_si",
      int_range = "w_left",
      NULL
    )
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
          sig_types <- setdiff(pg_types, fr_env$stat_sig_skip)
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
