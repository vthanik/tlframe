# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_vitals_dataset.R
#
# Generates a synthetic `tbl_vitals` TFL-ready dataset with 8 treatment
# columns (for col_split testing: 1 stub + 5 cols on page 1, 1 stub + 3 cols
# on page 2).
#
# Fictional study: TFRM-2024-001
#   Drug:    Zomerane dose-finding (8-arm)
#   Design:  Randomized, double-blind, dose-ranging
#   Arms:    Placebo | 10mg | 25mg | 50mg | 100mg | 200mg | 400mg | Total
#   N:       45 subjects per arm (315 total)
#
# Prerequisites:
#   None — this is a standalone TFL-ready dataset.
#
# Run once to regenerate:
#   source("data-raw/create_vitals_dataset.R")
# ─────────────────────────────────────────────────────────────────────────────

set.seed(20240201)

# ── Study constants ─────────────────────────────────────────────────────────

arms <- c("Placebo", "Zom 10mg", "Zom 25mg", "Zom 50mg",
          "Zom 100mg", "Zom 200mg", "Zom 400mg")
n_per_arm <- 45L
n_total   <- n_per_arm * length(arms)  # 315

# Column names for the wide TFL layout
arm_colnames <- c("placebo", "zom_10mg", "zom_25mg", "zom_50mg",
                   "zom_100mg", "zom_200mg", "zom_400mg")

# ── Formatting helpers ──────────────────────────────────────────────────────

fmt_mean_sd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_median  <- function(x) sprintf("%.1f", median(x, na.rm = TRUE))
fmt_minmax  <- function(x) sprintf("%.1f, %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
fmt_n_pct   <- function(n, denom) sprintf("%d (%.1f)", as.integer(n), 100 * n / denom)

# ── Vital signs parameters ──────────────────────────────────────────────────

vitals_params <- list(
  "Systolic Blood Pressure (mmHg)" = list(mean = 135, sd = 15,
    effect = c(0, -2, -4, -6, -8, -10, -12)),
  "Diastolic Blood Pressure (mmHg)" = list(mean = 85, sd = 10,
    effect = c(0, -1, -2, -3, -5, -6, -7)),
  "Heart Rate (bpm)" = list(mean = 75, sd = 12,
    effect = c(0, 0, -1, -2, -3, -4, -5)),
  "Weight (kg)" = list(mean = 78, sd = 14,
    effect = c(0, 0, 0, 0.5, 0.8, 1.2, 1.5)),
  "BMI (kg/m2)" = list(mean = 27, sd = 4,
    effect = c(0, 0, 0, 0.2, 0.3, 0.4, 0.5)),
  "Temperature (C)" = list(mean = 36.6, sd = 0.3,
    effect = c(0, 0, 0, 0, 0, 0, 0)),
  "Respiratory Rate (breaths/min)" = list(mean = 16, sd = 2,
    effect = c(0, 0, 0, 0, 0, 0, 0))
)

# ── Simulate subject-level data ─────────────────────────────────────────────

timepoints <- c("Baseline", "Week 4", "Week 12", "Week 24")

sim_data <- vector("list", n_total * length(vitals_params) * length(timepoints))
idx <- 0L

for (arm_i in seq_along(arms)) {
  for (subj_j in seq_len(n_per_arm)) {
    for (param_name in names(vitals_params)) {
      p <- vitals_params[[param_name]]
      base_val <- rnorm(1, p$mean, p$sd)

      for (tp_k in seq_along(timepoints)) {
        tp <- timepoints[tp_k]
        if (tp == "Baseline") {
          val <- base_val
          chg <- NA_real_
        } else {
          time_factor <- tp_k / length(timepoints)
          val <- base_val + p$effect[arm_i] * time_factor + rnorm(1, 0, p$sd * 0.15)
          chg <- val - base_val
        }
        idx <- idx + 1L
        sim_data[[idx]] <- data.frame(
          arm = arms[arm_i], param = param_name,
          timepoint = tp, aval = val, chg = chg,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

sim_df <- do.call(rbind, sim_data[seq_len(idx)])

# ── Build wide TFL layout ──────────────────────────────────────────────────

build_stat_block <- function(data, val_col, stat_prefix = "") {
  rows <- list()

  for (stat_name in c("n", "Mean (SD)", "Median", "Min, Max")) {
    row <- list(statistic = paste0(stat_prefix, stat_name))

    for (arm_i in seq_along(arms)) {
      arm_data <- data[data$arm == arms[arm_i], ][[val_col]]
      arm_data <- arm_data[!is.na(arm_data)]
      val <- switch(stat_name,
        "n"        = as.character(length(arm_data)),
        "Mean (SD)" = fmt_mean_sd(arm_data),
        "Median"   = fmt_median(arm_data),
        "Min, Max" = fmt_minmax(arm_data)
      )
      row[[arm_colnames[arm_i]]] <- val
    }

    # Total column
    all_data <- data[[val_col]]
    all_data <- all_data[!is.na(all_data)]
    row[["total"]] <- switch(stat_name,
      "n"        = as.character(length(all_data)),
      "Mean (SD)" = fmt_mean_sd(all_data),
      "Median"   = fmt_median(all_data),
      "Min, Max" = fmt_minmax(all_data)
    )

    rows[[length(rows) + 1L]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }

  do.call(rbind, rows)
}

tbl_rows <- list()

for (param_name in names(vitals_params)) {
  param_data <- sim_df[sim_df$param == param_name, ]

  for (tp in timepoints) {
    tp_data <- param_data[param_data$timepoint == tp, ]

    # Header row for this timepoint
    header_label <- if (tp == "Baseline") tp else paste(tp, "Value")
    blank_row <- data.frame(
      statistic = header_label,
      stringsAsFactors = FALSE
    )
    for (cn in c(arm_colnames, "total")) blank_row[[cn]] <- ""
    blank_row$param     <- param_name
    blank_row$timepoint <- header_label

    tbl_rows[[length(tbl_rows) + 1L]] <- blank_row

    # Value stats
    val_block <- build_stat_block(tp_data, "aval", stat_prefix = "  ")
    val_block$param     <- param_name
    val_block$timepoint <- header_label
    tbl_rows[[length(tbl_rows) + 1L]] <- val_block

    # Change from baseline (post-baseline only)
    if (tp != "Baseline") {
      chg_label <- paste(tp, "Change from Baseline")
      blank_chg <- data.frame(statistic = chg_label, stringsAsFactors = FALSE)
      for (cn in c(arm_colnames, "total")) blank_chg[[cn]] <- ""
      blank_chg$param     <- param_name
      blank_chg$timepoint <- chg_label
      tbl_rows[[length(tbl_rows) + 1L]] <- blank_chg

      chg_block <- build_stat_block(tp_data, "chg", stat_prefix = "  ")
      chg_block$param     <- param_name
      chg_block$timepoint <- chg_label
      tbl_rows[[length(tbl_rows) + 1L]] <- chg_block
    }
  }
}

tbl_vitals <- do.call(rbind, tbl_rows)
rownames(tbl_vitals) <- NULL

# Reorder columns: stub first, then arms, then total, then metadata
tbl_vitals <- tbl_vitals[, c("param", "timepoint", "statistic",
                               arm_colnames, "total")]

cat("tbl_vitals:", nrow(tbl_vitals), "rows x", ncol(tbl_vitals), "cols\n")
cat("  Columns:", paste(names(tbl_vitals), collapse = ", "), "\n")
cat("  Use with fr_cols() col_split for 1+5 / 1+3 page panels\n")

usethis::use_data(tbl_vitals, overwrite = TRUE)
