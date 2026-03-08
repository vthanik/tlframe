# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_tbl_datasets.R
#
# Creates presentation-ready (TFL-ready) summary tables from the synthetic
# ADaM datasets produced by create_datasets.R. These tables are used in
# tlframe examples and documentation.
#
# Fictional study: TFRM-2024-001
#   Drug:   Zomerane (50mg | 100mg) vs Placebo
#   N:      135 subjects (45 per arm)
#
# Tables produced:
#   tbl_demog    — Table 14.1.1 Demographics and Baseline Characteristics
#   tbl_ae_soc   — Table 14.3.1 Adverse Events by SOC / Preferred Term
#   tbl_disp     — Table 14.1.4 Subject Disposition
#   tbl_tte      — Table 14.2.1 Time-to-Event Summary
#   tbl_cm       — Table 14.4.1 Concomitant Medications
#
# Prerequisites:
#   Run create_datasets.R first to produce data/adsl.rda, etc.
#
# Run once to regenerate:
#   source("data-raw/create_tbl_datasets.R")
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr, warn.conflicts = FALSE)

# Load raw ADaM datasets
load("data/adsl.rda")
load("data/adae.rda")
load("data/adtte.rda")
load("data/adcm.rda")


# ══════════════════════════════════════════════════════════════════════════════
# Formatting helpers
# ══════════════════════════════════════════════════════════════════════════════

fmt_mean_sd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_median  <- function(x) sprintf("%.1f", median(x, na.rm = TRUE))
fmt_minmax  <- function(x) sprintf("%.1f, %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
fmt_n_pct   <- function(n, denom) sprintf("%d (%.1f)", as.integer(n), 100 * n / denom)


# Arms in column order
arm_levels <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
arm_colnames <- c("placebo", "zom_50mg", "zom_100mg")

N_PER_ARM <- 45L
N_TOTAL   <- 135L


# ══════════════════════════════════════════════════════════════════════════════
# 1. tbl_demog — Demographics and Baseline Characteristics
# ══════════════════════════════════════════════════════════════════════════════

# Helper: summarize a continuous variable by arm
summarize_continuous <- function(data, var, label) {
  var_sym <- rlang::sym(var)

  by_arm <- data %>%
    group_by(ARM) %>%
    summarize(
      mean_sd = fmt_mean_sd(!!var_sym),
      median  = fmt_median(!!var_sym),
      minmax  = fmt_minmax(!!var_sym),
      .groups = "drop"
    )

  total <- data %>%
    summarize(
      mean_sd = fmt_mean_sd(!!var_sym),
      median  = fmt_median(!!var_sym),
      minmax  = fmt_minmax(!!var_sym)
    )

  make_wide <- function(stat_label, stat_col) {
    vals <- setNames(by_arm[[stat_col]], by_arm$ARM)
    tibble(
      characteristic = paste0("  ", stat_label),
      placebo   = vals[["Placebo"]],
      zom_50mg  = vals[["Zomerane 50mg"]],
      zom_100mg = vals[["Zomerane 100mg"]],
      total     = total[[stat_col]]
    )
  }

  bind_rows(
    tibble(characteristic = label, placebo = "", zom_50mg = "", zom_100mg = "", total = ""),
    make_wide("Mean (SD)", "mean_sd"),
    make_wide("Median", "median"),
    make_wide("Min, Max", "minmax")
  )
}

# Helper: summarize a categorical variable by arm
summarize_categorical <- function(data, var, label, levels = NULL) {
  var_sym <- rlang::sym(var)
  if (is.null(levels)) levels <- sort(unique(data[[var]]))

  header <- tibble(characteristic = label, placebo = "", zom_50mg = "", zom_100mg = "", total = "")

  rows <- lapply(levels, function(lv) {
    by_arm <- data %>%
      group_by(ARM) %>%
      summarize(n = sum(!!var_sym == lv), .groups = "drop")
    vals <- setNames(by_arm$n, by_arm$ARM)

    tibble(
      characteristic = paste0("  ", lv),
      placebo   = fmt_n_pct(vals[["Placebo"]],         N_PER_ARM),
      zom_50mg  = fmt_n_pct(vals[["Zomerane 50mg"]],   N_PER_ARM),
      zom_100mg = fmt_n_pct(vals[["Zomerane 100mg"]],  N_PER_ARM),
      total     = fmt_n_pct(sum(data[[var]] == lv),     N_TOTAL)
    )
  })

  bind_rows(header, rows)
}

# Subject counts
n_row <- tibble(
  characteristic = "Subjects, n",
  placebo   = as.character(N_PER_ARM),
  zom_50mg  = as.character(N_PER_ARM),
  zom_100mg = as.character(N_PER_ARM),
  total     = as.character(N_TOTAL)
)

tbl_demog <- bind_rows(
  n_row %>% mutate(group = "n"),
  summarize_continuous(adsl, "AGE", "Age (years)") %>% mutate(group = "age"),
  summarize_categorical(adsl, "AGEGR1", "Age Group, n (%)", c("<65", "65-80", ">80")) %>%
    mutate(group = "age"),
  summarize_categorical(adsl, "SEX", "Sex, n (%)", c("F", "M")) %>%
    mutate(characteristic = case_match(characteristic,
      "  F" ~ "  Female",
      "  M" ~ "  Male",
      .default = characteristic
    )) %>%
    mutate(group = "sex"),
  summarize_categorical(adsl, "RACE", "Race, n (%)",
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "AMERICAN INDIAN OR ALASKA NATIVE")) %>%
    mutate(characteristic = case_match(characteristic,
      "  WHITE" ~ "  White",
      "  BLACK OR AFRICAN AMERICAN" ~ "  Black or African American",
      "  ASIAN" ~ "  Asian",
      "  AMERICAN INDIAN OR ALASKA NATIVE" ~ "  American Indian or Alaska Native",
      .default = characteristic
    )) %>%
    mutate(group = "race"),
  summarize_continuous(adsl, "BMIBL", "BMI (kg/m2)") %>% mutate(group = "bmi"),
  summarize_continuous(adsl, "MMSEBL", "MMSE Score at Baseline") %>% mutate(group = "mmse"),
  summarize_categorical(adsl, "EOSSTT", "Study Completion, n (%)",
    c("COMPLETED", "DISCONTINUED")) %>%
    mutate(characteristic = case_match(characteristic,
      "  COMPLETED"    ~ "  Completed",
      "  DISCONTINUED" ~ "  Discontinued",
      .default = characteristic
    )) %>%
    mutate(group = "completion")
) %>%
  as.data.frame()

cat("tbl_demog:", nrow(tbl_demog), "rows x", ncol(tbl_demog), "cols\n")
print(head(tbl_demog, 5))


# ══════════════════════════════════════════════════════════════════════════════
# 2. tbl_ae_soc — Adverse Events by System Organ Class and Preferred Term
# ══════════════════════════════════════════════════════════════════════════════

saf <- adsl %>% filter(SAFFL == "Y")
ae  <- adae %>% filter(SAFFL == "Y", TRTEMFL == "Y")

n_saf_df <- saf %>% count(ARM, name = "n_saf")
n_saf <- setNames(n_saf_df$n_saf, n_saf_df$ARM)
n_saf_total <- nrow(saf)

# Count unique subjects by arm for a given SOC/PT filter
ae_n_pct <- function(ae_data, arm_val = NULL, soc = NULL, pt = NULL) {
  sub <- ae_data
  if (!is.null(arm_val)) sub <- sub %>% filter(ARM == arm_val)
  if (!is.null(soc))     sub <- sub %>% filter(AEBODSYS == soc)
  if (!is.null(pt))      sub <- sub %>% filter(AEDECOD == pt)
  n <- n_distinct(sub$USUBJID)
  denom <- if (!is.null(arm_val)) n_saf[[arm_val]] else n_saf_total
  fmt_n_pct(n, denom)
}

make_ae_row <- function(soc_val, pt_label, row_type, soc = NULL, pt = NULL) {
  v_placebo   <- ae_n_pct(ae, "Placebo",         soc, pt)
  v_zom_50mg  <- ae_n_pct(ae, "Zomerane 50mg",   soc, pt)
  v_zom_100mg <- ae_n_pct(ae, "Zomerane 100mg",  soc, pt)
  v_total     <- ae_n_pct(ae, NULL,               soc, pt)
  tibble(
    soc       = soc_val,
    pt        = pt_label,
    row_type  = row_type,
    placebo   = v_placebo,
    zom_50mg  = v_zom_50mg,
    zom_100mg = v_zom_100mg,
    total     = v_total
  )
}

# Total row
ae_rows <- list(make_ae_row(
  soc_val  = "SUBJECTS WITH >=1 TEAE",
  pt_label = "SUBJECTS WITH >=1 TEAE",
  row_type = "total"
))

# SOCs sorted by descending total incidence
soc_order <- ae %>%
  summarize(n = n_distinct(USUBJID), .by = AEBODSYS) %>%
  arrange(desc(n))

for (soc_name in soc_order$AEBODSYS) {
  ae_rows[[length(ae_rows) + 1]] <- make_ae_row(soc_name, soc_name, "soc", soc = soc_name)

  # PT rows sorted by descending incidence within SOC
  pt_order <- ae %>%
    filter(AEBODSYS == soc_name) %>%
    summarize(n = n_distinct(USUBJID), .by = AEDECOD) %>%
    arrange(desc(n))

  for (pt_name in pt_order$AEDECOD) {
    ae_rows[[length(ae_rows) + 1]] <- make_ae_row(soc_name, paste0("  ", pt_name), "pt",
                                                    soc = soc_name, pt = pt_name)
  }
}

tbl_ae_soc <- bind_rows(ae_rows) %>% as.data.frame()

cat("tbl_ae_soc:", nrow(tbl_ae_soc), "rows x", ncol(tbl_ae_soc), "cols\n")
print(head(tbl_ae_soc, 8))


# ══════════════════════════════════════════════════════════════════════════════
# 3. tbl_disp — Subject Disposition
# ══════════════════════════════════════════════════════════════════════════════

disp_categories <- c("Randomized", "Completed", "Discontinued")
disp_reasons <- c("Adverse Event", "Withdrew Consent", "Lost to Follow-up",
                   "Lack of Efficacy", "Physician Decision")

disp_count <- function(data, arm_val, category) {
  sub <- data %>% filter(ARM == arm_val)
  n <- switch(category,
    "Randomized"   = nrow(sub),
    "Completed"    = sum(sub$EOSSTT == "COMPLETED"),
    "Discontinued" = sum(sub$EOSSTT == "DISCONTINUED"),
    sum(sub$DCSREAS == category, na.rm = TRUE)
  )
  denom <- nrow(sub)
  if (category == "Randomized") as.character(n) else fmt_n_pct(n, denom)
}

disp_rows <- list()

for (cat in disp_categories) {
  disp_rows[[length(disp_rows) + 1]] <- tibble(
    category  = cat,
    placebo   = disp_count(adsl, "Placebo", cat),
    zom_50mg  = disp_count(adsl, "Zomerane 50mg", cat),
    zom_100mg = disp_count(adsl, "Zomerane 100mg", cat),
    total     = if (cat == "Randomized") as.character(N_TOTAL)
                else fmt_n_pct(sum(adsl$EOSSTT == toupper(cat)), N_TOTAL)
  )
}

# Discontinuation reason sub-rows
for (reason in disp_reasons) {
  disp_rows[[length(disp_rows) + 1]] <- tibble(
    category  = paste0("  ", reason),
    placebo   = disp_count(adsl, "Placebo", reason),
    zom_50mg  = disp_count(adsl, "Zomerane 50mg", reason),
    zom_100mg = disp_count(adsl, "Zomerane 100mg", reason),
    total     = fmt_n_pct(sum(adsl$DCSREAS == reason, na.rm = TRUE), N_TOTAL)
  )
}

tbl_disp <- bind_rows(disp_rows) %>% as.data.frame()

cat("tbl_disp:", nrow(tbl_disp), "rows x", ncol(tbl_disp), "cols\n")
print(tbl_disp)


# ══════════════════════════════════════════════════════════════════════════════
# 4. tbl_tte — Time-to-Event Summary (TTWD parameter)
# ══════════════════════════════════════════════════════════════════════════════

ttwd <- adtte %>% filter(PARAMCD == "TTWD")

tte_summary <- ttwd %>%
  group_by(ARM) %>%
  summarize(
    n          = n(),
    n_event    = sum(CNSR == 0),
    n_censored = sum(CNSR == 1),
    median     = sprintf("%.1f", median(AVAL)),
    min        = sprintf("%.0f", min(AVAL)),
    max        = sprintf("%.0f", max(AVAL)),
    .groups    = "drop"
  )

# Also compute total
tte_total <- ttwd %>%
  summarize(
    n          = n(),
    n_event    = sum(CNSR == 0),
    n_censored = sum(CNSR == 1),
    median     = sprintf("%.1f", median(AVAL)),
    min        = sprintf("%.0f", min(AVAL)),
    max        = sprintf("%.0f", max(AVAL))
  )

make_tte_row <- function(stat_label, stat_col) {
  vals <- setNames(as.character(tte_summary[[stat_col]]), tte_summary$ARM)
  tibble(
    statistic = stat_label,
    placebo   = vals[["Placebo"]],
    zom_50mg  = vals[["Zomerane 50mg"]],
    zom_100mg = vals[["Zomerane 100mg"]],
    total     = as.character(tte_total[[stat_col]])
  )
}

tbl_tte <- bind_rows(
  make_tte_row("Number of subjects",   "n"),
  make_tte_row("Events, n",            "n_event"),
  make_tte_row("Censored, n",          "n_censored"),
  make_tte_row("Median time (days)",   "median"),
  make_tte_row("Minimum (days)",       "min"),
  make_tte_row("Maximum (days)",       "max")
) %>% as.data.frame()

cat("tbl_tte:", nrow(tbl_tte), "rows x", ncol(tbl_tte), "cols\n")
print(tbl_tte)


# ══════════════════════════════════════════════════════════════════════════════
# 5. tbl_cm — Concomitant Medications by Category and Agent
# ══════════════════════════════════════════════════════════════════════════════

cm_saf <- adcm %>% filter(SAFFL == "Y")

cm_n_pct <- function(cm_data, arm_val = NULL, cat = NULL, drug = NULL) {
  sub <- cm_data
  if (!is.null(arm_val)) sub <- sub %>% filter(ARM == arm_val)
  if (!is.null(cat))     sub <- sub %>% filter(CMCAT == cat)
  if (!is.null(drug))    sub <- sub %>% filter(CMDECOD == drug)
  n <- n_distinct(sub$USUBJID)
  denom <- if (!is.null(arm_val)) n_saf[[arm_val]] else n_saf_total
  fmt_n_pct(n, denom)
}

make_cm_row <- function(cat_val, drug_label, row_type, cat = NULL, drug = NULL) {
  tibble(
    category  = cat_val,
    medication = drug_label,
    row_type  = row_type,
    placebo   = cm_n_pct(cm_saf, "Placebo",         cat, drug),
    zom_50mg  = cm_n_pct(cm_saf, "Zomerane 50mg",   cat, drug),
    zom_100mg = cm_n_pct(cm_saf, "Zomerane 100mg",  cat, drug),
    total     = cm_n_pct(cm_saf, NULL,               cat, drug)
  )
}

cm_rows <- list()

# Total row
cm_rows[[1]] <- make_cm_row(
  cat_val    = "ANY CONCOMITANT MEDICATION",
  drug_label = "ANY CONCOMITANT MEDICATION",
  row_type   = "total"
)

# Categories sorted by descending total subject count
cat_order <- cm_saf %>%
  summarize(n = n_distinct(USUBJID), .by = CMCAT) %>%
  arrange(desc(n))

for (cat_name in cat_order$CMCAT) {
  cm_rows[[length(cm_rows) + 1]] <- make_cm_row(cat_name, cat_name, "category", cat = cat_name)

  # Drugs within category sorted by descending count
  drug_order <- cm_saf %>%
    filter(CMCAT == cat_name) %>%
    summarize(n = n_distinct(USUBJID), .by = CMDECOD) %>%
    arrange(desc(n))

  for (drug_name in drug_order$CMDECOD) {
    cm_rows[[length(cm_rows) + 1]] <- make_cm_row(cat_name, paste0("  ", drug_name), "drug",
                                                     cat = cat_name, drug = drug_name)
  }
}

tbl_cm <- bind_rows(cm_rows) %>% as.data.frame()

cat("tbl_cm:", nrow(tbl_cm), "rows x", ncol(tbl_cm), "cols\n")
print(head(tbl_cm, 8))


# ══════════════════════════════════════════════════════════════════════════════
# Save all to data/
# ══════════════════════════════════════════════════════════════════════════════

usethis::use_data(tbl_demog, tbl_ae_soc, tbl_disp, tbl_tte, tbl_cm, overwrite = TRUE)

cat("\nDone. All TBL datasets saved to data/\n")
