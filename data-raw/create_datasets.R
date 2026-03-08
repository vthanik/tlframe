# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_datasets.R
#
# Generates four synthetic CDISC ADaM datasets for use in tlframe examples
# and documentation. All datasets share the same subjects and are internally
# consistent.
#
# Fictional study: TFRM-2024-001
#   Drug:        Zomerane (fictional anti-cholinesterase inhibitor)
#   Indication:  Mild-to-moderate Alzheimer's Disease
#   Design:      Randomized, double-blind, placebo-controlled, 24-week
#   Arms:        Placebo | Zomerane 50mg | Zomerane 100mg
#   N:           135 subjects (45 per arm, 5 sites)
#
# Datasets produced:
#   adsl  — Subject Level Analysis Dataset  (Table 14.1.1 demographics + disposition)
#   adae  — Adverse Events Analysis Dataset (Table 14.3.1 AE by SOC and PT)
#   adtte — Time to Event Analysis Dataset  (Table 14.2.x time to event)
#   adcm  — Concomitant Medications Dataset (Table 14.4.1 conmed by category)
#
# Run once to regenerate:
#   source("data-raw/create_datasets.R")
# ─────────────────────────────────────────────────────────────────────────────

set.seed(20240101)

# ── Study constants ───────────────────────────────────────────────────────────

STUDYID     <- "TFRM-2024-001"
N_PER_ARM   <- 45L
ARMS        <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
ARM_DOSES   <- c(0L, 50L, 100L)
N_TOTAL     <- N_PER_ARM * length(ARMS)    # 135
STUDY_DAYS  <- 168L                         # 24 weeks


# ══════════════════════════════════════════════════════════════════════════════
# 1. ADSL — Subject Level Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# ── Identifiers ──────────────────────────────────────────────────────────────

# Five sites, subjects distributed across sites within arms
site_pool <- sprintf("%03d", 1:5)
siteid <- sample(site_pool, N_TOTAL, replace = TRUE)
subjid <- sprintf("%04d", seq_len(N_TOTAL))
usubjid <- paste0("TFR-", siteid, "-", subjid)

arm_vec  <- rep(ARMS, each = N_PER_ARM)
arm_dose <- rep(ARM_DOSES, each = N_PER_ARM)

# ── Demographics ─────────────────────────────────────────────────────────────

# Age: Alzheimer's population — mostly 65-85
age_raw <- c(
  round(rnorm(N_PER_ARM, mean = 74, sd = 7)),
  round(rnorm(N_PER_ARM, mean = 73, sd = 7)),
  round(rnorm(N_PER_ARM, mean = 74, sd = 7))
)
age <- pmax(55L, pmin(88L, as.integer(age_raw)))

agegr1 <- cut(age, breaks = c(-Inf, 64, 80, Inf),
               labels = c("<65", "65-80", ">80"), right = TRUE)
agegr1 <- as.character(agegr1)
agegr1n <- c("<65" = 1L, "65-80" = 2L, ">80" = 3L)[agegr1]
names(agegr1n) <- NULL

# Sex: 55% F, 45% M (Alzheimer's affects women more)
sex <- sample(c("F", "M"), N_TOTAL, replace = TRUE, prob = c(0.55, 0.45))

# Race: CDISC controlled terminology
race_opts <- c(
  "WHITE",
  "BLACK OR AFRICAN AMERICAN",
  "ASIAN",
  "AMERICAN INDIAN OR ALASKA NATIVE"
)
race <- sample(race_opts, N_TOTAL, replace = TRUE,
               prob = c(0.76, 0.10, 0.12, 0.02))

# Ethnicity
ethnic <- sample(
  c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
  N_TOTAL, replace = TRUE, prob = c(0.12, 0.88)
)

# Country
countries <- c("USA", "GBR", "CAN", "DEU", "FRA")
country <- sample(countries, N_TOTAL, replace = TRUE,
                  prob = c(0.38, 0.20, 0.17, 0.15, 0.10))

# Height (cm): sex-stratified
heightbl <- ifelse(
  sex == "M",
  pmax(158, pmin(193, round(rnorm(N_TOTAL, mean = 174, sd = 7)))),
  pmax(148, pmin(180, round(rnorm(N_TOTAL, mean = 161, sd = 6))))
)

# Weight (kg)
weightbl <- round(pmax(45, pmin(115, rnorm(N_TOTAL, mean = 73, sd = 13))), 1)

# BMI
bmibl <- round(weightbl / (heightbl / 100)^2, 1)

# ── Study dates ───────────────────────────────────────────────────────────────

rand_origin <- as.Date("2020-01-06")
trtsdt <- rand_origin + sample(0:358, N_TOTAL, replace = TRUE)

# ── Disposition ───────────────────────────────────────────────────────────────

# Discontinuation probability: higher in higher-dose arms (more AEs)
disc_prob_map <- c(
  "Placebo"          = 0.09,
  "Zomerane 50mg"    = 0.13,
  "Zomerane 100mg"   = 0.18
)
disc_prob <- disc_prob_map[arm_vec]
discfl    <- rbinom(N_TOTAL, 1L, prob = disc_prob)  # 1 = discontinued

# Discontinuation reasons (more AE-driven in active arms)
disc_reasons <- c(
  "Adverse Event", "Withdrew Consent",
  "Lost to Follow-up", "Lack of Efficacy", "Physician Decision"
)
disc_reason_probs <- list(
  "Placebo"          = c(0.20, 0.40, 0.25, 0.10, 0.05),
  "Zomerane 50mg"    = c(0.45, 0.25, 0.15, 0.10, 0.05),
  "Zomerane 100mg"   = c(0.55, 0.20, 0.10, 0.10, 0.05)
)

dcsreas <- vapply(seq_len(N_TOTAL), function(i) {
  if (discfl[i] == 1L) {
    sample(disc_reasons, 1L, prob = disc_reason_probs[[arm_vec[i]]])
  } else {
    NA_character_
  }
}, character(1))

eosstt <- ifelse(discfl == 0L, "COMPLETED", "DISCONTINUED")

# Actual treatment end date
disc_day <- round(runif(N_TOTAL, min = 0.20, max = 0.95) * (STUDY_DAYS - 1L))
trtedt <- as.Date(ifelse(
  discfl == 0L,
  as.integer(trtsdt) + STUDY_DAYS - 1L,
  as.integer(trtsdt) + disc_day
), origin = "1970-01-01")

trtdurd <- as.integer(trtedt - trtsdt + 1L)

# ── Population flags ─────────────────────────────────────────────────────────

saffl <- rep("Y", N_TOTAL)
ittfl <- rep("Y", N_TOTAL)
efffl <- ifelse(trtdurd >= 14L, "Y", "N")  # at least 2 weeks on treatment

# ── Disease characteristics ───────────────────────────────────────────────────

durdis   <- round(runif(N_TOTAL, min = 6, max = 60))    # months since diagnosis
durdsgr1 <- ifelse(durdis < 12, "<12", ">=12")
mmsebl   <- as.integer(pmax(10L, pmin(26L, round(rnorm(N_TOTAL, mean = 20, sd = 4)))))

# ── Assemble ADSL ─────────────────────────────────────────────────────────────

adsl <- data.frame(
  STUDYID  = STUDYID,
  USUBJID  = usubjid,
  SUBJID   = subjid,
  SITEID   = siteid,
  ARM      = arm_vec,
  TRT01P   = arm_vec,
  TRT01PN  = arm_dose,
  TRT01A   = arm_vec,
  TRT01AN  = arm_dose,
  TRTSDT   = trtsdt,
  TRTEDT   = trtedt,
  TRTDURD  = trtdurd,
  AGE      = age,
  AGEGR1   = agegr1,
  AGEGR1N  = agegr1n,
  AGEU     = "YEARS",
  SEX      = sex,
  RACE     = race,
  ETHNIC   = ethnic,
  COUNTRY  = country,
  HEIGHTBL = as.numeric(heightbl),
  WEIGHTBL = weightbl,
  BMIBL    = bmibl,
  MMSEBL   = mmsebl,
  DURDIS   = as.numeric(durdis),
  DURDSGR1 = durdsgr1,
  SAFFL    = saffl,
  ITTFL    = ittfl,
  EFFFL    = efffl,
  EOSSTT   = eosstt,
  DCSREAS  = dcsreas,
  stringsAsFactors = FALSE
)

cat("adsl: ", nrow(adsl), "rows x", ncol(adsl), "cols\n")
cat("  Arms:", paste(table(adsl$ARM), collapse = " / "), "\n")
cat("  Discontinued:", sum(adsl$EOSSTT == "DISCONTINUED"), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# 2. ADAE — Adverse Events Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# AE term catalog — realistic for cholinesterase inhibitor class in elderly
# Expanded to ~40 PTs across 10 SOCs for multi-page tbl_ae_soc (100+ rows)
ae_catalog <- data.frame(
  AEBODSYS = c(
    # Gastrointestinal disorders (7 PTs — class effect, dose-related)
    rep("Gastrointestinal disorders", 7),
    # Nervous system disorders (6 PTs — class effect, dose-related)
    rep("Nervous system disorders", 6),
    # General disorders and administration site conditions (4 PTs)
    rep("General disorders and administration site conditions", 4),
    # Musculoskeletal and connective tissue disorders (4 PTs)
    rep("Musculoskeletal and connective tissue disorders", 4),
    # Infections and infestations (5 PTs)
    rep("Infections and infestations", 5),
    # Cardiac disorders (5 PTs)
    rep("Cardiac disorders", 5),
    # Skin and subcutaneous tissue disorders (5 PTs)
    rep("Skin and subcutaneous tissue disorders", 5),
    # Psychiatric disorders (5 PTs)
    rep("Psychiatric disorders", 5),
    # Respiratory, thoracic and mediastinal disorders (5 PTs)
    rep("Respiratory, thoracic and mediastinal disorders", 5),
    # Metabolism and nutrition disorders (4 PTs)
    rep("Metabolism and nutrition disorders", 4),
    # Eye disorders (4 PTs)
    rep("Eye disorders", 4),
    # Vascular disorders (4 PTs)
    rep("Vascular disorders", 4),
    # Renal and urinary disorders (4 PTs)
    rep("Renal and urinary disorders", 4),
    # Hepatobiliary disorders (3 PTs)
    rep("Hepatobiliary disorders", 3),
    # Ear and labyrinth disorders (3 PTs)
    rep("Ear and labyrinth disorders", 3),
    # Blood and lymphatic system disorders (3 PTs)
    rep("Blood and lymphatic system disorders", 3),
    # Reproductive system and breast disorders (3 PTs)
    rep("Reproductive system and breast disorders", 3),
    # Investigations (3 PTs)
    rep("Investigations", 3)
  ),
  AEDECOD = c(
    # GI
    "Nausea", "Vomiting", "Diarrhoea", "Abdominal pain upper",
    "Constipation", "Dyspepsia", "Flatulence",
    # Nervous
    "Headache", "Dizziness", "Somnolence", "Tremor", "Insomnia", "Paraesthesia",
    # General
    "Fatigue", "Peripheral oedema", "Asthenia", "Pyrexia",
    # MSK
    "Back pain", "Arthralgia", "Myalgia", "Pain in extremity",
    # Infections
    "Nasopharyngitis", "Urinary tract infection", "Upper respiratory tract infection",
    "Bronchitis", "Influenza",
    # Cardiac
    "Bradycardia", "Palpitations", "Tachycardia", "Atrial fibrillation",
    "Extrasystoles",
    # Skin
    "Hyperhidrosis", "Rash", "Pruritus", "Dermatitis", "Dry skin",
    # Psychiatric
    "Anxiety", "Depression", "Agitation", "Confusional state", "Hallucination",
    # Respiratory
    "Cough", "Dyspnoea", "Epistaxis", "Rhinorrhoea", "Oropharyngeal pain",
    # Metabolism
    "Decreased appetite", "Hypokalaemia", "Dehydration", "Weight decreased",
    # Eye
    "Vision blurred", "Dry eye", "Conjunctivitis", "Lacrimation increased",
    # Vascular
    "Hypertension", "Hypotension", "Flushing", "Hot flush",
    # Renal
    "Pollakiuria", "Incontinence", "Nocturia", "Dysuria",
    # Hepatobiliary
    "Hepatic enzyme increased", "Cholelithiasis", "Hepatic steatosis",
    # Ear
    "Tinnitus", "Vertigo", "Ear pain",
    # Blood
    "Anaemia", "Leukopenia", "Thrombocytopenia",
    # Reproductive
    "Erectile dysfunction", "Gynaecomastia", "Menstrual disorder",
    # Investigations
    "Blood creatinine increased", "Weight increased",
    "Alanine aminotransferase increased"
  ),
  # Per-subject probability of experiencing each AE, by arm
  # GI + nervous AEs are dose-related (cholinergic class effect)
  # Per-subject probability — elevated to ensure all PTs appear (100+ tbl rows)
  freq_pbo = c(
    # GI (7)
    0.10, 0.07, 0.08, 0.06, 0.08, 0.07, 0.05,
    # Nervous (6)
    0.10, 0.08, 0.06, 0.05, 0.08, 0.05,
    # General (4)
    0.11, 0.07, 0.06, 0.08,
    # MSK (4)
    0.10, 0.08, 0.06, 0.05,
    # Infections (5)
    0.13, 0.09, 0.10, 0.07, 0.06,
    # Cardiac (5)
    0.06, 0.06, 0.05, 0.05, 0.05,
    # Skin (5)
    0.07, 0.07, 0.06, 0.05, 0.05,
    # Psychiatric (5)
    0.08, 0.07, 0.09, 0.06, 0.05,
    # Respiratory (5)
    0.08, 0.06, 0.05, 0.05, 0.06,
    # Metabolism (4)
    0.07, 0.05, 0.05, 0.06,
    # Eye (4)
    0.06, 0.05, 0.05, 0.05,
    # Vascular (4)
    0.09, 0.05, 0.05, 0.06,
    # Renal (4)
    0.06, 0.05, 0.05, 0.05,
    # Hepatobiliary (3)
    0.05, 0.05, 0.05,
    # Ear (3)
    0.06, 0.05, 0.05,
    # Blood (3)
    0.05, 0.05, 0.05,
    # Reproductive (3)
    0.05, 0.05, 0.05,
    # Investigations (3)
    0.05, 0.05, 0.05
  ),
  freq_low = c(
    # GI (7)
    0.18, 0.12, 0.12, 0.08, 0.10, 0.09, 0.07,
    # Nervous (6)
    0.14, 0.12, 0.10, 0.08, 0.10, 0.06,
    # General (4)
    0.13, 0.09, 0.08, 0.10,
    # MSK (4)
    0.10, 0.08, 0.08, 0.06,
    # Infections (5)
    0.12, 0.09, 0.10, 0.07, 0.06,
    # Cardiac (5)
    0.07, 0.07, 0.06, 0.05, 0.04,
    # Skin (5)
    0.08, 0.08, 0.07, 0.06, 0.05,
    # Psychiatric (5)
    0.09, 0.08, 0.10, 0.07, 0.06,
    # Respiratory (5)
    0.09, 0.06, 0.05, 0.05, 0.06,
    # Metabolism (4)
    0.09, 0.06, 0.06, 0.07,
    # Eye (4)
    0.06, 0.05, 0.05, 0.04,
    # Vascular (4)
    0.09, 0.06, 0.05, 0.06,
    # Renal (4)
    0.07, 0.05, 0.05, 0.04,
    # Hepatobiliary (3)
    0.06, 0.04, 0.04,
    # Ear (3)
    0.06, 0.05, 0.04,
    # Blood (3)
    0.05, 0.04, 0.04,
    # Reproductive (3)
    0.04, 0.04, 0.04,
    # Investigations (3)
    0.06, 0.05, 0.05
  ),
  freq_hi  = c(
    # GI (7)
    0.25, 0.18, 0.15, 0.10, 0.12, 0.12, 0.08,
    # Nervous (6)
    0.18, 0.15, 0.12, 0.10, 0.12, 0.08,
    # General (4)
    0.15, 0.10, 0.09, 0.12,
    # MSK (4)
    0.09, 0.07, 0.09, 0.08,
    # Infections (5)
    0.10, 0.08, 0.09, 0.07, 0.06,
    # Cardiac (5)
    0.09, 0.08, 0.07, 0.07, 0.06,
    # Skin (5)
    0.10, 0.09, 0.08, 0.07, 0.06,
    # Psychiatric (5)
    0.10, 0.09, 0.11, 0.08, 0.07,
    # Respiratory (5)
    0.10, 0.07, 0.06, 0.06, 0.07,
    # Metabolism (4)
    0.11, 0.07, 0.07, 0.08,
    # Eye (4)
    0.07, 0.06, 0.06, 0.05,
    # Vascular (4)
    0.09, 0.07, 0.06, 0.07,
    # Renal (4)
    0.08, 0.07, 0.06, 0.05,
    # Hepatobiliary (3)
    0.08, 0.06, 0.05,
    # Ear (3)
    0.07, 0.06, 0.05,
    # Blood (3)
    0.07, 0.05, 0.05,
    # Reproductive (3)
    0.05, 0.05, 0.05,
    # Investigations (3)
    0.08, 0.07, 0.07
  ),
  stringsAsFactors = FALSE
)

ae_rows <- vector("list", N_TOTAL * 5L)  # pre-allocate generously
ae_idx  <- 0L

for (i in seq_len(N_TOTAL)) {
  freq_col <- switch(arm_vec[i],
    "Placebo"          = "freq_pbo",
    "Zomerane 50mg"    = "freq_low",
    "Zomerane 100mg"   = "freq_hi"
  )

  for (j in seq_len(nrow(ae_catalog))) {
    if (runif(1L) >= ae_catalog[[freq_col]][j]) next

    ae_start  <- sample.int(trtdurd[i], 1L)
    ae_dur    <- sample(1L:21L, 1L)
    ae_end    <- min(ae_start + ae_dur - 1L, trtdurd[i])
    astdt     <- trtsdt[i] + ae_start - 1L
    aendt     <- trtsdt[i] + ae_end   - 1L

    # Severity: higher dose → more moderate/severe
    sev_prob <- switch(as.character(arm_dose[i]),
      "0"  = c(0.72, 0.23, 0.05),
      "50" = c(0.60, 0.30, 0.10),
      "100"= c(0.50, 0.35, 0.15)
    )
    aesev <- sample(c("MILD", "MODERATE", "SEVERE"), 1L, prob = sev_prob)
    aeser <- if (aesev == "SEVERE" && runif(1L) < 0.30) "Y" else "N"

    # Causality: GI/nervous AEs more likely related in active arms
    gi_ns <- ae_catalog$AEBODSYS[j] %in%
               c("Gastrointestinal disorders", "Nervous system disorders")
    if (arm_dose[i] == 0L) {
      rel_prob <- if (gi_ns) c(0.05, 0.10, 0.15, 0.70) else c(0.15, 0.15, 0.10, 0.60)
    } else {
      rel_prob <- if (gi_ns) c(0.10, 0.35, 0.40, 0.15) else c(0.10, 0.20, 0.25, 0.45)
    }
    aerel <- sample(c("PROBABLE", "POSSIBLE", "REMOTE", "NONE"), 1L, prob = rel_prob)

    aeout <- if (aesev == "SEVERE" && aeser == "Y") {
      sample(c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"), 1L, prob = c(0.70, 0.30))
    } else {
      sample(c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"), 1L, prob = c(0.92, 0.08))
    }

    ae_idx <- ae_idx + 1L
    ae_rows[[ae_idx]] <- data.frame(
      STUDYID  = STUDYID,
      USUBJID  = usubjid[i],
      ARM      = arm_vec[i],
      TRTA     = arm_vec[i],
      TRTAN    = arm_dose[i],
      AGE      = age[i],
      SEX      = sex[i],
      RACE     = race[i],
      SAFFL    = "Y",
      AEBODSYS = ae_catalog$AEBODSYS[j],
      AEDECOD  = ae_catalog$AEDECOD[j],
      AESEV    = aesev,
      AESER    = aeser,
      AEREL    = aerel,
      AEOUT    = aeout,
      ASTDT    = astdt,
      AENDT    = aendt,
      ASTDY    = as.integer(ae_start),
      AENDY    = as.integer(ae_end),
      ADURN    = as.integer(ae_end - ae_start + 1L),
      TRTEMFL  = "Y",
      stringsAsFactors = FALSE
    )
  }
}

adae <- do.call(rbind, ae_rows[seq_len(ae_idx)])
adae <- adae[order(adae$USUBJID, adae$ASTDY, adae$AEBODSYS), ]
rownames(adae) <- NULL

# Sequence number per subject
adae$AESEQ <- as.integer(ave(seq_len(nrow(adae)), adae$USUBJID,
                              FUN = seq_along))

# First-occurrence flags (Y = first, NA = not first)
adae$AOCCFL  <- ifelse(!duplicated(adae$USUBJID),                           "Y", NA_character_)
adae$AOCCSFL <- ifelse(!duplicated(paste(adae$USUBJID, adae$AEBODSYS)),     "Y", NA_character_)
adae$AOCCPFL <- ifelse(!duplicated(paste(adae$USUBJID, adae$AEDECOD)),      "Y", NA_character_)

cat("adae:", nrow(adae), "rows x", ncol(adae), "cols\n")
cat("  Subjects with AE:", length(unique(adae$USUBJID)), "/", N_TOTAL, "\n")
cat("  AEs by arm:\n")
print(table(adae$ARM))


# ══════════════════════════════════════════════════════════════════════════════
# 3. ADTTE — Time to Event Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# Two parameters:
#   TTWD  — Time to Study Withdrawal (event = discontinuation, censored = completed)
#   TTAE  — Time to First Adverse Event (event = first AE, censored = no AE by end)

tte_rows <- vector("list", N_TOTAL * 2L)
tte_idx  <- 0L

for (i in seq_len(N_TOTAL)) {

  # ── TTWD ──────────────────────────────────────────────────────────────────
  tte_idx <- tte_idx + 1L
  tte_rows[[tte_idx]] <- data.frame(
    STUDYID = STUDYID,
    USUBJID = usubjid[i],
    ARM     = arm_vec[i],
    TRTA    = arm_vec[i],
    TRTAN   = arm_dose[i],
    AGE     = age[i],
    SEX     = sex[i],
    SAFFL   = "Y",
    ITTFL   = "Y",
    PARAMCD = "TTWD",
    PARAM   = "Time to Study Withdrawal (Days)",
    AVAL    = as.numeric(trtdurd[i]),
    CNSR    = if (discfl[i] == 1L) 0L else 1L,   # 0 = event, 1 = censored
    STARTDT = trtsdt[i],
    ADT     = trtedt[i],
    ADY     = as.integer(trtdurd[i]),
    stringsAsFactors = FALSE
  )

  # ── TTAE ──────────────────────────────────────────────────────────────────
  subj_aes <- adae[adae$USUBJID == usubjid[i], ]

  if (nrow(subj_aes) > 0L) {
    first_day <- min(subj_aes$ASTDY)
    cnsr_ttae <- 0L
    aval_ttae <- as.numeric(first_day)
    adt_ttae  <- trtsdt[i] + first_day - 1L
  } else {
    cnsr_ttae <- 1L
    aval_ttae <- as.numeric(trtdurd[i])
    adt_ttae  <- trtedt[i]
  }

  tte_idx <- tte_idx + 1L
  tte_rows[[tte_idx]] <- data.frame(
    STUDYID = STUDYID,
    USUBJID = usubjid[i],
    ARM     = arm_vec[i],
    TRTA    = arm_vec[i],
    TRTAN   = arm_dose[i],
    AGE     = age[i],
    SEX     = sex[i],
    SAFFL   = "Y",
    ITTFL   = "Y",
    PARAMCD = "TTAE",
    PARAM   = "Time to First Adverse Event (Days)",
    AVAL    = aval_ttae,
    CNSR    = cnsr_ttae,
    STARTDT = trtsdt[i],
    ADT     = adt_ttae,
    ADY     = as.integer(aval_ttae),
    stringsAsFactors = FALSE
  )
}

adtte <- do.call(rbind, tte_rows[seq_len(tte_idx)])
rownames(adtte) <- NULL

cat("adtte:", nrow(adtte), "rows x", ncol(adtte), "cols\n")
cat("  Events (TTWD):", sum(adtte$CNSR[adtte$PARAMCD == "TTWD"] == 0), "\n")
cat("  Events (TTAE):", sum(adtte$CNSR[adtte$PARAMCD == "TTAE"] == 0), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# 4. ADCM — Concomitant Medications Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# Medication catalog — realistic for elderly Alzheimer's patients
# (hypertension, diabetes, pain, GI, supplements are common)
cm_catalog <- data.frame(
  CMDECOD = c(
    "PARACETAMOL", "IBUPROFEN", "ASPIRIN",
    "AMLODIPINE", "LISINOPRIL", "METOPROLOL", "ATENOLOL",
    "METFORMIN", "GLIPIZIDE",
    "OMEPRAZOLE", "PANTOPRAZOLE",
    "CALCIUM CARBONATE", "VITAMIN D", "MULTIVITAMINS",
    "WARFARIN", "ATORVASTATIN", "SIMVASTATIN",
    "LEVOTHYROXINE", "FUROSEMIDE", "LORAZEPAM"
  ),
  CMCAT = c(
    "ANALGESICS", "ANALGESICS", "ANALGESICS",
    "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES",
    "ANTIDIABETICS", "ANTIDIABETICS",
    "GASTROINTESTINAL AGENTS", "GASTROINTESTINAL AGENTS",
    "SUPPLEMENTS", "SUPPLEMENTS", "SUPPLEMENTS",
    "ANTICOAGULANTS", "LIPID MODIFYING AGENTS", "LIPID MODIFYING AGENTS",
    "THYROID AGENTS", "DIURETICS", "ANXIOLYTICS"
  ),
  # Prevalence in Alzheimer's elderly population (independent of treatment)
  prev = c(
    0.30, 0.10, 0.28,
    0.38, 0.32, 0.22, 0.14,
    0.18, 0.10,
    0.22, 0.14,
    0.30, 0.38, 0.28,
    0.08, 0.36, 0.18,
    0.14, 0.20, 0.12
  ),
  stringsAsFactors = FALSE
)

cm_rows <- vector("list", N_TOTAL * 5L)
cm_idx  <- 0L

for (i in seq_len(N_TOTAL)) {
  for (j in seq_len(nrow(cm_catalog))) {
    if (runif(1L) >= cm_catalog$prev[j]) next

    # Pre-existing (75%) or new conmed (25%)
    pre_existing <- runif(1L) < 0.75
    if (pre_existing) {
      cmstdt <- trtsdt[i] - sample(30L:730L, 1L)
    } else {
      cmstdt <- trtsdt[i] + sample.int(as.integer(trtdurd[i]), 1L)
    }

    # Ongoing (60%) or resolved (40%)
    ongoing <- runif(1L) < 0.60
    cmendt  <- if (ongoing) NA else cmstdt + sample(14L:180L, 1L)

    cm_idx <- cm_idx + 1L
    cm_rows[[cm_idx]] <- data.frame(
      STUDYID  = STUDYID,
      USUBJID  = usubjid[i],
      ARM      = arm_vec[i],
      TRTA     = arm_vec[i],
      TRTAN    = arm_dose[i],
      AGE      = age[i],
      SEX      = sex[i],
      SAFFL    = "Y",
      CMDECOD  = cm_catalog$CMDECOD[j],
      CMCAT    = cm_catalog$CMCAT[j],
      CMSTDT   = as.Date(cmstdt, origin = "1970-01-01"),
      CMENDT   = as.Date(cmendt, origin = "1970-01-01"),
      ONGOING  = ongoing,
      stringsAsFactors = FALSE
    )
  }
}

adcm <- do.call(rbind, cm_rows[seq_len(cm_idx)])
adcm <- adcm[order(adcm$USUBJID, adcm$CMCAT, adcm$CMDECOD), ]
rownames(adcm) <- NULL

cat("adcm:", nrow(adcm), "rows x", ncol(adcm), "cols\n")
cat("  Subjects with conmed:", length(unique(adcm$USUBJID)), "/", N_TOTAL, "\n")
cat("  Conmeds by category:\n")
print(sort(table(adcm$CMCAT), decreasing = TRUE))


# ══════════════════════════════════════════════════════════════════════════════
# Save to data/
# ══════════════════════════════════════════════════════════════════════════════

usethis::use_data(adsl, adae, adtte, adcm, overwrite = TRUE)

cat("\nDone. Datasets saved to data/\n")
