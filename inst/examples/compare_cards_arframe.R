# ============================================================================
# cards ARD -> arframe: The Easy Conversion Path
# ============================================================================
#
# If your team already uses cards::ard_stack() for analysis, arframe
# plugs in directly via fr_wide_ard(). No gt, no tfrmt, no docorator.
#
#   cards::ard_stack() |> fr_wide_ard() |> fr_table() |> fr_render("out.pdf")
#
# This script shows three real tables built from cards ARD output:
#   1. Demographics (continuous + categorical)
#   2. AE by SOC/PT (hierarchical)
#   3. Two-way subgroup table (.by = c(ARM, SEX))
#
# Missing packages are installed automatically.

# ── Package setup ───────────────────────────────────────────────────────────

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, quiet = TRUE)
  }
}

ensure_packages("cards")

library(arframe)
library(cards)

outdir <- file.path(tempdir(), "cards_arframe")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


# ── Study-wide theme (set once, all tables below inherit) ──────────────────

fr_theme_reset()
fr_theme(
  font_size = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines = "header",
  header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "Protocol: TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 1: Demographics — cards -> fr_wide_ard() -> PDF                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("\n")
cat("================================================================\n")
cat("  TABLE 1: Demographics (continuous + categorical)\n")
cat("================================================================\n\n")

adsl_saf <- arframe::adsl[arframe::adsl$SAFFL == "Y", ]

# Step 1: Create ARD with cards (same as any pharmaverse workflow)
demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "ARM",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("SEX", "RACE")),
  .overall = TRUE
)

cat("cards ARD created:", nrow(demog_ard), "rows\n")

# Step 2: Convert to wide with fr_wide_ard() — THIS IS THE BRIDGE
demog_wide <- fr_wide_ard(
  demog_ard,
  statistic = list(
    continuous = c(
      "n"         = "{N}",
      "Mean (SD)" = "{mean} ({sd})",
      "Median"    = "{median}",
      "Min, Max"  = "{min}, {max}"
    ),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, median = 1, p = 1),
  label = c(AGE = "Age (years)", SEX = "Sex, n (%)", RACE = "Race, n (%)")
)

cat("Wide summary:\n")
print(demog_wide)
cat("\n")

# Step 3: Pipe straight into arframe
demog_spec <- demog_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Safety Population"
  ) |>
  fr_cols(
    variable   = fr_col(visible = FALSE),
    stat_label = fr_col("", width = 2.5),
    Placebo    = fr_col("Placebo", align = "decimal"),
    `Zomerane 50mg`  = fr_col("Zomerane 50mg", align = "decimal"),
    `Zomerane 100mg` = fr_col("Zomerane 100mg", align = "decimal"),
    Total      = fr_col("Total", align = "decimal"),
    .n = c(Placebo = 45, `Zomerane 50mg` = 45, `Zomerane 100mg` = 45, Total = 135)
  ) |>
  fr_rows(group_by = "variable", group_label = "stat_label", group_bold = TRUE) |>
  fr_footnotes("Percentages based on N per treatment group.")

demog_pdf <- file.path(outdir, "demog.pdf")
demog_spec |> fr_render(demog_pdf)

cat("PDF written: ", demog_pdf, "\n")
cat("Total pipeline: ard_stack() |> fr_wide_ard() |> fr_table() |> fr_render()\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 2: AE by SOC/PT — hierarchical ARD -> PDF                      ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  TABLE 2: AE by SOC/PT (hierarchical ARD)\n")
cat("================================================================\n\n")

adae_te <- arframe::adae[arframe::adae$TRTEMFL == "Y", ]

# Step 1: Hierarchical ARD with cards
ae_ard <- ard_stack_hierarchical(
  data = adae_te,
  variables = c(AEBODSYS, AEDECOD),
  by = ARM,
  denominator = adsl_saf,
  id = USUBJID
)

cat("Hierarchical ARD created:", nrow(ae_ard), "rows\n")

# Step 2: Convert to wide — fr_wide_ard() handles SOC/PT structure
ae_wide <- fr_wide_ard(
  ae_ard,
  statistic = "{n} ({p}%)",
  decimals = c(p = 1),
  label = c("..ard_hierarchical_overall.." = "Any TEAE")
)

cat("Wide summary:\n")
print(ae_wide, n = 10)
cat("\n")

# Step 3: Pipe into arframe
ae_spec <- ae_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("TEAEs by System Organ Class and Preferred Term", bold = TRUE),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type = fr_col(visible = FALSE),
    Placebo  = fr_col("Placebo", align = "decimal"),
    `Zomerane 50mg`  = fr_col("Zomerane\n50mg", align = "decimal"),
    `Zomerane 100mg` = fr_col("Zomerane\n100mg", align = "decimal"),
    .n = c(Placebo = 45, `Zomerane 50mg` = 45, `Zomerane 100mg` = 45)
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "overall"), bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA version 26.0.",
    "Subjects counted once per SOC and Preferred Term."
  )

ae_pdf <- file.path(outdir, "ae_soc.pdf")
ae_spec |> fr_render(ae_pdf)

cat("PDF written: ", ae_pdf, "\n")
cat("Hierarchical ARD handled automatically — SOC/PT/row_type columns created.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 3: Subgroup table — .by = c(ARM, SEX) with extra group cols     ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  TABLE 3: Subgroup demographics (.by = c(ARM, SEX))\n")
cat("================================================================\n\n")

# Step 1: Multi-group ARD
subgrp_ard <- ard_stack(
  data = adsl_saf,
  .by = c("ARM", "SEX"),
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = "RACE")
)

cat("Multi-group ARD created:", nrow(subgrp_ard), "rows\n")

# Step 2: Convert — extra group (SEX) becomes a column in output
subgrp_wide <- fr_wide_ard(
  subgrp_ard,
  statistic = list(
    continuous = c("Mean (SD)" = "{mean} ({sd})"),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, p = 1),
  label = c(AGE = "Age (years)", RACE = "Race, n (%)")
)

cat("Wide summary:\n")
print(subgrp_wide, n = 10)
cat("\n")

# Step 3: Pipe into arframe — page_by SEX gives each subgroup its own page
subgrp_spec <- subgrp_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.1.2",
    "Demographics by Sex Subgroup",
    "Safety Population"
  ) |>
  fr_cols(
    SEX        = fr_col(visible = FALSE),
    variable   = fr_col(visible = FALSE),
    stat_label = fr_col("", width = 2.5),
    Placebo    = fr_col("Placebo", align = "decimal"),
    `Zomerane 50mg`  = fr_col("Zomerane 50mg", align = "decimal"),
    `Zomerane 100mg` = fr_col("Zomerane 100mg", align = "decimal")
  ) |>
  fr_rows(
    page_by = "SEX",
    group_by = "variable",
    group_label = "stat_label",
    group_bold = TRUE
  ) |>
  fr_footnotes("Each sex subgroup on a separate page.")

subgrp_pdf <- file.path(outdir, "subgroup.pdf")
subgrp_spec |> fr_render(subgrp_pdf)

cat("PDF written: ", subgrp_pdf, "\n")
cat("page_by = 'SEX': each sex gets its own page automatically.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Summary: Why cards + arframe works                                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  WHY cards + arframe\n")
cat("================================================================\n\n")

cat("The pharmaverse path:  cards -> tfrmt -> gt -> docorator -> PDF\n")
cat("The arframe path:      cards -> fr_wide_ard() -> fr_table() -> PDF\n\n")

cat("What fr_wide_ard() handles for you:\n")
cat("  - List columns (raw ard_stack output)     -> unpacked automatically\n")
cat("  - Renamed columns (rename_ard_columns())  -> detected automatically\n")
cat("  - Hierarchical ARD (SOC/PT)               -> soc/pt/row_type columns\n")
cat("  - Multi-group .by (e.g., ARM + SEX)       -> extra group columns\n")
cat("  - Overall/Total rows (.overall = TRUE)     -> 'Total' column\n")
cat("  - Per-variable decimals                    -> decimals = list(...)\n")
cat("  - Custom stat formatting                   -> fmt = list(...)\n")
cat("  - BigN extraction                          -> big_n = 'N'\n\n")

cat("What you DON'T need:\n")
cat("  - tfrmt (no body_plan, frmt_structure, frmt_combine)\n")
cat("  - gt (no tab_style, cols_align, tab_options)\n")
cat("  - docorator (no as_docorator, fancyhead, render_pdf)\n")
cat("  - tidyr reshape (no pivot_longer/wider between steps)\n\n")

cat("Output directory:", outdir, "\n")
cat("  demog.pdf     — Demographics from cards ARD\n")
cat("  ae_soc.pdf    — AE SOC/PT from hierarchical ARD\n")
cat("  subgroup.pdf  — Sex subgroup from multi-group ARD\n")

fr_theme_reset()
