# ─────────────────────────────────────────────────────────────────────────────
# data.R — Documentation for built-in CDISC ADaM example datasets
#
# All four datasets share the same 135 synthetic subjects from fictional
# study TFRM-2024-001 (Zomerane for mild-to-moderate Alzheimer's Disease).
# They follow CDISC ADaM naming conventions and controlled terminology.
#
# Small, realistic CDISC ADaM datasets ready to use in examples.
# ─────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# adsl
# ══════════════════════════════════════════════════════════════════════════════

#' Subject Level Analysis Dataset (ADSL)
#'
#' @description
#' Synthetic CDISC ADaM Subject Level (ADSL) dataset from fictional study
#' **TFRM-2024-001**, a 24-week randomized, double-blind, placebo-controlled
#' trial of Zomerane (a fictional anti-cholinesterase inhibitor) in
#' mild-to-moderate Alzheimer's Disease.
#'
#' Contains one row per subject (N = 135). Used for:
#' - **Demographics table** (Table 14.1.1): age, sex, race, BMI by treatment arm
#' - **Disposition table** (Table 14.1.4): completion, discontinuation reasons
#'
#' Variable names follow the CDISC ADaM Implementation Guide.
#'
#' @format A data frame with 135 rows and 31 variables:
#' \describe{
#'   \item{STUDYID}{Study identifier (`"TFRM-2024-001"`)}
#'   \item{USUBJID}{Unique subject identifier (`"TFR-NNN-NNNN"`)}
#'   \item{SUBJID}{Subject identifier within study}
#'   \item{SITEID}{Site identifier (`"001"` to `"005"`)}
#'   \item{ARM}{Planned treatment arm: `"Placebo"`, `"Zomerane 50mg"`, or `"Zomerane 100mg"`}
#'   \item{TRT01P}{Planned treatment (same as `ARM`)}
#'   \item{TRT01PN}{Planned treatment numeric code: `0`, `50`, or `100`}
#'   \item{TRT01A}{Actual treatment (same as `TRT01P` --- no switches)}
#'   \item{TRT01AN}{Actual treatment numeric code}
#'   \item{TRTSDT}{Treatment start date}
#'   \item{TRTEDT}{Treatment end date}
#'   \item{TRTDURD}{Treatment duration in days}
#'   \item{AGE}{Age in years at randomization (55--88)}
#'   \item{AGEGR1}{Age group: `"<65"`, `"65-80"`, or `">80"`}
#'   \item{AGEGR1N}{Age group numeric: 1, 2, or 3}
#'   \item{AGEU}{Age units (`"YEARS"`)}
#'   \item{SEX}{Sex: `"F"` (Female) or `"M"` (Male)}
#'   \item{RACE}{Race (CDISC controlled terminology): `"WHITE"`,
#'     `"BLACK OR AFRICAN AMERICAN"`, `"ASIAN"`,
#'     `"AMERICAN INDIAN OR ALASKA NATIVE"`}
#'   \item{ETHNIC}{Ethnicity: `"HISPANIC OR LATINO"` or
#'     `"NOT HISPANIC OR LATINO"`}
#'   \item{COUNTRY}{Country of enrolment: `"USA"`, `"GBR"`, `"CAN"`,
#'     `"DEU"`, or `"FRA"`}
#'   \item{HEIGHTBL}{Baseline height (cm)}
#'   \item{WEIGHTBL}{Baseline weight (kg)}
#'   \item{BMIBL}{Baseline BMI (kg/m squared)}
#'   \item{MMSEBL}{Baseline Mini-Mental State Examination score (10--26);
#'     lower scores indicate greater cognitive impairment}
#'   \item{DURDIS}{Disease duration at baseline (months)}
#'   \item{DURDSGR1}{Disease duration group: `"<12"` or `">=12"` months}
#'   \item{SAFFL}{Safety population flag: `"Y"` (all randomized subjects)}
#'   \item{ITTFL}{Intent-to-treat population flag: `"Y"` (all randomized)}
#'   \item{EFFFL}{Efficacy population flag: `"Y"` if treatment duration >= 14 days}
#'   \item{EOSSTT}{End of study status: `"COMPLETED"` or `"DISCONTINUED"`}
#'   \item{DCSREAS}{Discontinuation reason (NA if completed): `"Adverse Event"`,
#'     `"Withdrew Consent"`, `"Lost to Follow-up"`, `"Lack of Efficacy"`, or
#'     `"Physician Decision"`}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_datasets.R`.
#'   No real patient data. Follows CDISC ADaM ADSL specifications.
#'
#' @examples
#' # Quick look
#' head(adsl)
#' table(adsl$ARM)
#' table(adsl$EOSSTT, adsl$ARM)
#'
#' # Demographics table input
#' subset(adsl, SAFFL == "Y", select = c(ARM, AGE, SEX, RACE, BMIBL))
"adsl"


# ══════════════════════════════════════════════════════════════════════════════
# adae
# ══════════════════════════════════════════════════════════════════════════════

#' Adverse Events Analysis Dataset (ADAE)
#'
#' @description
#' Synthetic CDISC ADaM Adverse Events (ADAE) dataset from study TFRM-2024-001.
#' Contains one row per subject per adverse event. Used for:
#' - **Adverse events table** (Table 14.3.1): incidence by body system and
#'   preferred term, within treatment arm
#'
#' All adverse events in this dataset are treatment-emergent (`TRTEMFL = "Y"`).
#' Gastrointestinal and nervous system events are dose-related, reflecting the
#' cholinergic class effect of Zomerane.
#'
#' @format A data frame with approximately 200 rows and 21 variables:
#' \describe{
#'   \item{STUDYID}{Study identifier}
#'   \item{USUBJID}{Unique subject identifier}
#'   \item{ARM}{Planned treatment arm}
#'   \item{TRTA}{Actual treatment arm}
#'   \item{TRTAN}{Actual treatment numeric code}
#'   \item{AGE}{Subject age at time of event}
#'   \item{SEX}{Subject sex}
#'   \item{RACE}{Subject race}
#'   \item{SAFFL}{Safety population flag}
#'   \item{AEBODSYS}{MedDRA System Organ Class (SOC), e.g.
#'     `"Gastrointestinal disorders"`, `"Nervous system disorders"`}
#'   \item{AEDECOD}{MedDRA Preferred Term (PT), e.g. `"Nausea"`, `"Dizziness"`}
#'   \item{AESEV}{Severity: `"MILD"`, `"MODERATE"`, or `"SEVERE"`}
#'   \item{AESER}{Serious adverse event flag: `"Y"` or `"N"`}
#'   \item{AEREL}{Relationship to study drug: `"PROBABLE"`, `"POSSIBLE"`,
#'     `"REMOTE"`, or `"NONE"`}
#'   \item{AEOUT}{Outcome: `"RECOVERED/RESOLVED"` or
#'     `"NOT RECOVERED/NOT RESOLVED"`}
#'   \item{ASTDT}{AE start date}
#'   \item{AENDT}{AE end date}
#'   \item{ASTDY}{AE start day relative to treatment start}
#'   \item{AENDY}{AE end day relative to treatment start}
#'   \item{ADURN}{AE duration in days}
#'   \item{TRTEMFL}{Treatment-emergent flag (`"Y"` for all records)}
#'   \item{AESEQ}{AE sequence number within subject}
#'   \item{AOCCFL}{First-occurrence flag (any AE, per subject): `"Y"` or `NA`}
#'   \item{AOCCSFL}{First SOC occurrence flag (per subject per SOC): `"Y"` or `NA`}
#'   \item{AOCCPFL}{First PT occurrence flag (per subject per PT): `"Y"` or `NA`}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_datasets.R`.
#'
#' @examples
#' # Subjects with at least one treatment-emergent AE
#' te_aes <- subset(adae, TRTEMFL == "Y" & !is.na(AOCCFL))
#' table(te_aes$ARM)
#'
#' # AEs by body system
#' sort(table(adae$AEBODSYS), decreasing = TRUE)
#'
#' # Serious AEs only
#' subset(adae, AESER == "Y", select = c(USUBJID, ARM, AEDECOD, AESEV))
"adae"


# ══════════════════════════════════════════════════════════════════════════════
# adtte
# ══════════════════════════════════════════════════════════════════════════════

#' Time to Event Analysis Dataset (ADTTE)
#'
#' @description
#' Synthetic CDISC ADaM Time-to-Event (ADTTE) dataset from study TFRM-2024-001.
#' Contains one row per subject per time-to-event parameter. Used for:
#' - **Time to event table** (Table 14.2.x): Kaplan-Meier estimates, median
#'   time-to-event, and hazard ratios by treatment arm
#'
#' Two parameters are included:
#' - `"TTWD"` — Time to Study Withdrawal (event = discontinued, censored = completed)
#' - `"TTAE"` — Time to First Adverse Event (event = first AE, censored = no AE)
#'
#' @format A data frame with 270 rows (135 subjects x 2 parameters) and 16 variables:
#' \describe{
#'   \item{STUDYID}{Study identifier}
#'   \item{USUBJID}{Unique subject identifier}
#'   \item{ARM}{Planned treatment arm}
#'   \item{TRTA}{Actual treatment arm}
#'   \item{TRTAN}{Actual treatment numeric code}
#'   \item{AGE}{Subject age}
#'   \item{SEX}{Subject sex}
#'   \item{SAFFL}{Safety population flag}
#'   \item{ITTFL}{Intent-to-treat population flag}
#'   \item{PARAMCD}{Parameter code: `"TTWD"` or `"TTAE"`}
#'   \item{PARAM}{Parameter label:
#'     `"Time to Study Withdrawal (Days)"` or
#'     `"Time to First Adverse Event (Days)"`}
#'   \item{AVAL}{Analysis value --- time in days from treatment start to event or censoring}
#'   \item{CNSR}{Censoring indicator: `0` = event occurred, `1` = censored}
#'   \item{STARTDT}{Treatment start date (used as reference/origin date)}
#'   \item{ADT}{Date of event or censoring}
#'   \item{ADY}{Relative day of event or censoring}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_datasets.R`.
#'
#' @examples
#' # Subset to one parameter
#' ttwd <- subset(adtte, PARAMCD == "TTWD")
#' table(ttwd$ARM, ttwd$CNSR)  # 0 = withdrew, 1 = completed
#'
#' # Median time to first AE by arm (unadjusted)
#' ttae <- subset(adtte, PARAMCD == "TTAE")
#' tapply(ttae$AVAL, ttae$ARM, median)
"adtte"


# ══════════════════════════════════════════════════════════════════════════════
# adcm
# ══════════════════════════════════════════════════════════════════════════════

#' Concomitant Medications Analysis Dataset (ADCM)
#'
#' @description
#' Synthetic CDISC ADaM Concomitant Medications (ADCM) dataset from study
#' TFRM-2024-001. Contains one row per subject per concomitant medication. Used for:
#' - **Concomitant medications table** (Table 14.4.1): subjects using each
#'   medication category and individual agent, by treatment arm
#'
#' Medications reflect realistic co-morbidities in an elderly Alzheimer's
#' population: hypertension, diabetes, pain, GI protection, and supplements
#' are common. Medication use is independent of treatment assignment.
#'
#' @format A data frame with approximately 550 rows and 13 variables:
#' \describe{
#'   \item{STUDYID}{Study identifier}
#'   \item{USUBJID}{Unique subject identifier}
#'   \item{ARM}{Planned treatment arm}
#'   \item{TRTA}{Actual treatment arm}
#'   \item{TRTAN}{Actual treatment numeric code}
#'   \item{AGE}{Subject age}
#'   \item{SEX}{Subject sex}
#'   \item{SAFFL}{Safety population flag}
#'   \item{CMDECOD}{Decoded medication name (WHO Drug Dictionary-style, uppercase),
#'     e.g. `"PARACETAMOL"`, `"ATORVASTATIN"`, `"VITAMIN D"`}
#'   \item{CMCAT}{ATC-inspired medication category, e.g. `"ANALGESICS"`,
#'     `"ANTIHYPERTENSIVES"`, `"LIPID MODIFYING AGENTS"`, `"SUPPLEMENTS"`}
#'   \item{CMSTDT}{Concomitant medication start date}
#'   \item{CMENDT}{Concomitant medication end date (`NA` if ongoing at data cut)}
#'   \item{ONGOING}{Logical; `TRUE` if medication was ongoing at data cut}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_datasets.R`.
#'
#' @examples
#' # Unique subjects per medication category (for conmed table)
#' tapply(adcm$USUBJID, adcm$CMCAT, function(x) length(unique(x)))
#'
#' # Most common individual medications
#' sort(table(adcm$CMDECOD), decreasing = TRUE)
#'
#' # Subjects on lipid-modifying agents, by arm
#' statins <- subset(adcm, CMCAT == "LIPID MODIFYING AGENTS")
#' table(statins$ARM)
"adcm"


# ══════════════════════════════════════════════════════════════════════════════
# TFL-Ready Summary Tables
# ══════════════════════════════════════════════════════════════════════════════


#' Demographics and Baseline Characteristics Table (Table 14.1.1)
#'
#' @description
#' Pre-summarized demographics table in wide format, ready for use with
#' \code{fr_table()}. One row per characteristic line (headers, summary
#' statistics, category counts). Columns are treatment arms.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{characteristic}{Row label (e.g. `"Age (years)"`, `"  Mean (SD)"`)}
#'   \item{placebo}{Placebo arm summary}
#'   \item{zom_50mg}{Zomerane 50mg arm summary}
#'   \item{zom_100mg}{Zomerane 100mg arm summary}
#'   \item{total}{All subjects summary}
#'   \item{group}{Characteristic group key (e.g. `"age"`, `"sex"`). Use with
#'     `fr_rows(blank_after = "group")` to insert visual separation between
#'     blocks. Hide with `fr_cols(group = fr_col(visible = FALSE))`.}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_tbl_datasets.R`
#'   from [adsl].
#'
#' @examples
#' head(tbl_demog, 10)
"tbl_demog"


#' Adverse Events by System Organ Class Table (Table 14.3.1)
#'
#' @description
#' Pre-summarized adverse event table with SOC header rows and indented PT
#' rows. Columns are treatment arms showing n (%) of subjects.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{soc}{System Organ Class name (structural column)}
#'   \item{pt}{Display label: SOC name for header rows, indented PT for detail rows}
#'   \item{row_type}{`"total"`, `"soc"`, or `"pt"`}
#'   \item{placebo}{Placebo arm n (%)}
#'   \item{zom_50mg}{Zomerane 50mg arm n (%)}
#'   \item{zom_100mg}{Zomerane 100mg arm n (%)}
#'   \item{total}{All subjects n (%)}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_tbl_datasets.R`
#'   from [adae].
#'
#' @examples
#' head(tbl_ae_soc, 10)
"tbl_ae_soc"


#' Subject Disposition Table (Table 14.1.4)
#'
#' @description
#' Pre-summarized disposition table showing randomized, completed, and
#' discontinued counts with discontinuation reasons as indented sub-rows.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{category}{Row label (e.g. `"Randomized"`, `"  Adverse Event"`)}
#'   \item{placebo}{Placebo arm n or n (%)}
#'   \item{zom_50mg}{Zomerane 50mg arm n or n (%)}
#'   \item{zom_100mg}{Zomerane 100mg arm n or n (%)}
#'   \item{total}{All subjects n or n (%)}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_tbl_datasets.R`
#'   from [adsl].
#'
#' @examples
#' tbl_disp
"tbl_disp"


#' Time-to-Event Summary Table (Table 14.2.1)
#'
#' @description
#' Pre-summarized time-to-withdrawal table showing number of subjects,
#' events, censored counts, and median/min/max time in days.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{statistic}{Row label (e.g. `"Number of subjects"`, `"Median time (days)"`)}
#'   \item{placebo}{Placebo arm value}
#'   \item{zom_50mg}{Zomerane 50mg arm value}
#'   \item{zom_100mg}{Zomerane 100mg arm value}
#'   \item{total}{All subjects value}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_tbl_datasets.R`
#'   from [adtte].
#'
#' @examples
#' tbl_tte
"tbl_tte"


#' Concomitant Medications Table (Table 14.4.1)
#'
#' @description
#' Pre-summarized concomitant medications table with category header rows
#' and indented medication rows. Structure mirrors [tbl_ae_soc].
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{category}{Medication category (structural column)}
#'   \item{medication}{Display label: category name or indented drug name}
#'   \item{row_type}{`"total"`, `"category"`, or `"drug"`}
#'   \item{placebo}{Placebo arm n (%)}
#'   \item{zom_50mg}{Zomerane 50mg arm n (%)}
#'   \item{zom_100mg}{Zomerane 100mg arm n (%)}
#'   \item{total}{All subjects n (%)}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_tbl_datasets.R`
#'   from [adcm].
#'
#' @examples
#' head(tbl_cm, 10)
"tbl_cm"


#' Vital Signs Analysis Dataset (ADVS)
#'
#' @description
#' Synthetic record-level vital signs dataset from study TFRM-2024-001.
#' Contains one row per subject per parameter per visit. Some subjects have
#' missing parameters to create **natural N variation** across vital sign
#' groups — exactly the scenario that per-group N-count headers address.
#'
#' Designed as the `n_data` source for [fr_header()] with `n = "auto"` or
#' `n = function(...)` when the display table ([tbl_vs]) is pre-summarized.
#'
#' @format A data frame with approximately 1900 rows and 9 variables:
#' \describe{
#'   \item{STUDYID}{Study identifier (`"TFRM-2024-001"`)}
#'   \item{USUBJID}{Unique subject identifier}
#'   \item{TRTA}{Actual treatment arm: `"Placebo"`, `"Zomerane 50mg"`,
#'     or `"Zomerane 100mg"`}
#'   \item{TRTAN}{Actual treatment numeric code: `0`, `50`, or `100`}
#'   \item{SAFFL}{Safety population flag (`"Y"`)}
#'   \item{PARAM}{Vital sign parameter: `"Systolic BP (mmHg)"`,
#'     `"Diastolic BP (mmHg)"`, `"Heart Rate (bpm)"`, `"Weight (kg)"`,
#'     or `"Temperature (C)"`}
#'   \item{AVISIT}{Analysis visit: `"Baseline"`, `"Week 12"`, or `"Week 24"`}
#'   \item{AVAL}{Analysis value (measured vital sign)}
#'   \item{CHG}{Change from baseline (`NA` at baseline)}
#' }
#'
#' @section N variation by design:
#' Not all subjects have every parameter measured. Availability rates decrease
#' with dose and parameter complexity — Temperature has the most missing data,
#' Blood Pressure the least. This produces realistic per-group N differences:
#'
#' | Parameter | Placebo | Zom 50mg | Zom 100mg |
#' |-----------|---------|----------|-----------|
#' | Systolic BP | 45 | 45 | ~39 |
#' | Heart Rate | 45 | ~38 | ~40 |
#' | Temperature | ~42 | ~39 | ~37 |
#'
#' @source Synthetic data generated in `data-raw/create_advs_dataset.R`
#'   using subjects from [adsl].
#'
#' @examples
#' # Subjects per parameter per arm
#' with(
#'   advs[advs$AVISIT == "Baseline", ],
#'   tapply(USUBJID, list(PARAM, TRTA), function(x) length(unique(x)))
#' )
#'
#' # Use as n_data source for per-group N-counts
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_header(
#'     n = "auto",
#'     n_subject = "USUBJID",
#'     n_data = advs,
#'     format = "{name}\n(N={n})"
#'   )
#'
#' @seealso [tbl_vs] for the pre-summarized display table, [fr_header()]
#'   for N-count formatting.
"advs"


#' Vital Signs Summary Table (3-arm, page_by-ready)
#'
#' @description
#' Pre-summarized vital signs table in wide format with a `param` column
#' for [fr_rows()] `page_by` grouping. Each parameter produces a separate
#' page with its own summary statistics.
#'
#' Pair with [advs] as `n_data` in [fr_header()] to get per-parameter
#' N-counts in column headers.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{param}{Vital sign parameter name (page_by key)}
#'   \item{statistic}{Summary statistic: `"n"`, `"Mean (SD)"`, `"Median"`,
#'     or `"Min, Max"`}
#'   \item{placebo}{Placebo arm value}
#'   \item{zom_50mg}{Zomerane 50mg arm value}
#'   \item{zom_100mg}{Zomerane 100mg arm value}
#'   \item{total}{All arms combined}
#' }
#'
#' @source Synthetic data generated in `data-raw/create_advs_dataset.R`
#'   from [advs] (Week 24 values).
#'
#' @examples
#' tbl_vs
#'
#' # Per-parameter pages with per-group N-counts
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_cols(
#'     statistic = fr_col("Statistic", width = 2),
#'     placebo   = fr_col("Placebo"),
#'     zom_50mg  = fr_col("Zomerane 50mg"),
#'     zom_100mg = fr_col("Zomerane 100mg"),
#'     total     = fr_col("Total")
#'   ) |>
#'   fr_header(
#'     n = "auto",
#'     n_subject = "USUBJID",
#'     n_data = advs,
#'     format = "{name}\n(N={n})"
#'   )
#'
#' @seealso [advs] for the record-level source data, [fr_header()] for
#'   N-count label formatting.
"tbl_vs"


