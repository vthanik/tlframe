# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_advs_dataset.R
#
# Generates two datasets for per-group N-count examples:
#
#   advs   — Record-level vital signs (ADaM ADVS-like), one row per subject
#            per parameter per visit. Some subjects have missing parameters
#            to create natural N variation across page_by groups.
#
#   tbl_vs — Pre-summarized vital signs table (3-arm, page_by-ready).
#            Columns: param, statistic, placebo, zom_50mg, zom_100mg, total.
#            Use with fr_rows(page_by = "param") for per-parameter pages.
#
# Fictional study: TFRM-2024-001 (same 135 subjects as adsl)
#   Arms: Placebo (45) | Zomerane 50mg (45) | Zomerane 100mg (45)
#
# Prerequisites: adsl must exist (source create_datasets.R first)
#
# Run once to regenerate:
#   source("data-raw/create_advs_dataset.R")
# ─────────────────────────────────────────────────────────────────────────────

set.seed(20240301)

# ── Load adsl for subject identifiers ────────────────────────────────────────

if (!exists("adsl")) load("data/adsl.rda")

# ── Parameters with different availability rates ─────────────────────────────
# Not all subjects have every vital sign measured — creates natural N variation.

params <- list(
  "Systolic BP (mmHg)" = list(
    mean = 135, sd = 15, avail = c(1.00, 0.98, 0.96),  # nearly all
    effect = c(0, -5, -8)
  ),
  "Diastolic BP (mmHg)" = list(
    mean = 85, sd = 10, avail = c(1.00, 0.98, 0.96),
    effect = c(0, -3, -5)
  ),
  "Heart Rate (bpm)" = list(
    mean = 75, sd = 12, avail = c(0.93, 0.91, 0.87),   # some missing
    effect = c(0, -2, -4)
  ),
  "Weight (kg)" = list(
    mean = 78, sd = 14, avail = c(0.96, 0.93, 0.89),
    effect = c(0, 0.5, 1.0)
  ),
  "Temperature (C)" = list(
    mean = 36.6, sd = 0.3, avail = c(0.89, 0.84, 0.80), # most missing
    effect = c(0, 0, 0)
  )
)

arms <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
arm_cols <- c("placebo", "zom_50mg", "zom_100mg")
visits <- c("Baseline", "Week 12", "Week 24")

# ── Build record-level advs ─────────────────────────────────────────────────

records <- vector("list", nrow(adsl) * length(params) * length(visits))
idx <- 0L

for (i in seq_len(nrow(adsl))) {
  subj <- adsl[i, ]
  arm_idx <- match(subj$TRT01A, arms)

  for (pname in names(params)) {
    p <- params[[pname]]
    # Determine if this subject has this parameter (availability rate)
    if (runif(1) > p$avail[arm_idx]) next

    base_val <- rnorm(1, p$mean, p$sd)

    for (v in seq_along(visits)) {
      if (visits[v] == "Baseline") {
        val <- base_val
        chg <- NA_real_
      } else {
        time_frac <- v / length(visits)
        val <- base_val + p$effect[arm_idx] * time_frac + rnorm(1, 0, p$sd * 0.1)
        chg <- val - base_val
      }

      idx <- idx + 1L
      records[[idx]] <- data.frame(
        STUDYID  = subj$STUDYID,
        USUBJID  = subj$USUBJID,
        TRTA     = subj$TRT01A,
        TRTAN    = subj$TRT01AN,
        SAFFL    = subj$SAFFL,
        PARAM    = pname,
        AVISIT   = visits[v],
        AVAL     = round(val, 1),
        CHG      = if (is.na(chg)) NA_real_ else round(chg, 1),
        stringsAsFactors = FALSE
      )
    }
  }
}

advs <- do.call(rbind, records[seq_len(idx)])
rownames(advs) <- NULL

cat("advs:", nrow(advs), "rows x", ncol(advs), "cols\n")
cat("  Subjects per param per arm:\n")
for (pname in names(params)) {
  ns <- tapply(
    advs$USUBJID[advs$PARAM == pname & advs$AVISIT == "Baseline"],
    advs$TRTA[advs$PARAM == pname & advs$AVISIT == "Baseline"],
    function(x) length(unique(x))
  )
  cat("    ", pname, ":", paste(names(ns), ns, sep = "=", collapse = ", "), "\n")
}

# ── Build pre-summarized tbl_vs ─────────────────────────────────────────────

fmt_mean_sd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_median  <- function(x) sprintf("%.1f", median(x, na.rm = TRUE))
fmt_range   <- function(x) sprintf("%.1f, %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))

tbl_rows <- list()

for (pname in names(params)) {
  pdata <- advs[advs$PARAM == pname & advs$AVISIT == "Week 24", ]

  for (stat in c("n", "Mean (SD)", "Median", "Min, Max")) {
    row <- list(param = pname, statistic = stat)

    for (a in seq_along(arms)) {
      arm_data <- pdata$AVAL[pdata$TRTA == arms[a]]
      arm_data <- arm_data[!is.na(arm_data)]
      row[[arm_cols[a]]] <- switch(stat,
        "n"         = as.character(length(arm_data)),
        "Mean (SD)" = fmt_mean_sd(arm_data),
        "Median"    = fmt_median(arm_data),
        "Min, Max"  = fmt_range(arm_data)
      )
    }

    # Total
    all_data <- pdata$AVAL[!is.na(pdata$AVAL)]
    row[["total"]] <- switch(stat,
      "n"         = as.character(length(all_data)),
      "Mean (SD)" = fmt_mean_sd(all_data),
      "Median"    = fmt_median(all_data),
      "Min, Max"  = fmt_range(all_data)
    )

    tbl_rows[[length(tbl_rows) + 1L]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
}

tbl_vs <- do.call(rbind, tbl_rows)
rownames(tbl_vs) <- NULL

cat("\ntbl_vs:", nrow(tbl_vs), "rows x", ncol(tbl_vs), "cols\n")
cat("  Parameters:", paste(unique(tbl_vs$param), collapse = ", "), "\n")

usethis::use_data(advs, tbl_vs, overwrite = TRUE)
