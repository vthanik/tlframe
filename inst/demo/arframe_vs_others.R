# ============================================================================
# arframe vs tfrmt + gt + docorator — Head-to-Head Comparison
# Table 14.3.1: Treatment-Emergent AEs by SOC/PT (the hardest CSR table)
# ============================================================================
#
# This script builds the same production AE table two ways:
#   A) arframe alone (1 package, ~30 lines)
#   B) tfrmt + gt + docorator (3 packages, ~130 lines)
#
# PRIMARY OUTPUT: PDF — the standard at GSK and many sponsors.
#
# WHY this table?
#   - SOC/PT hierarchy with indentation (MedDRA)
#   - Multi-page with continuation headers
#   - Sorted by descending incidence
#   - Conditional row bolding (SOC rows, total row)
#   - N-counts in column headers with format control
#   - Pageheader / pagefooter with program name and page X of Y
#   - Decimal alignment across columns
#   - PDF output with exact font control
#
# If a package can produce this table to submission quality in PDF,
# it can handle anything in a CSR.

library(arframe)

# Population N-counts (reusable across all safety tables)
n_safety <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  A. arframe — 1 package, ~30 lines, PDF + RTF + HTML from one spec    ║
# ╚══════════════════════════════════════════════════════════════════════════╝

fr_theme(
  font_size   = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

ae_arframe <- tbl_ae_soc |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
         bold = TRUE),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc       = fr_col(visible = FALSE),
    pt        = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type  = fr_col(visible = FALSE),
    placebo   = fr_col("Placebo",          align = "decimal"),
    zom_50mg  = fr_col("Zomerane\n50mg",   align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg",  align = "decimal"),
    total     = fr_col("Total",            align = "decimal"),
    .n = n_safety
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "total"), bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"),   bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA version 26.0.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence."
  )

# One spec -> three formats
ae_arframe |> fr_render(file.path(tempdir(), "arframe_14_3_1.pdf"))
ae_arframe |> fr_render(file.path(tempdir(), "arframe_14_3_1.rtf"))
ae_arframe  # HTML in viewer

cat("\n== arframe ==\n")
cat("Packages needed:   1 (arframe)\n")
cat("Lines of code:     ~30\n")
cat("PDF backend:       XeLaTeX + tabularray (native typesetting)\n")
cat("Decimal alignment: YES (tabularray column type)\n")
cat("N-counts:          Automatic from named vector\n")
cat("Page headers:      Built-in (fancyhdr)\n")
cat("Page X of Y:       Built-in (lastpage)\n")
cat("Continuation:      Built-in ('(continued)' on page 2+)\n")
cat("Theme:             Study-wide, set once, all tables inherit\n")
cat("Output:            .pdf + .rtf + .html from SAME spec\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  B. tfrmt + gt + docorator — 3 packages, ~130 lines                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# The pharmaverse TLF stack:
#   1) tfrmt  — defines format spec (number formats, row groups, ordering)
#   2) gt     — renders the table (HTML, limited RTF)
#   3) docorator — wraps gt output with page headers/footers, renders PDF
#
# Each package handles one slice of what arframe does in a single pipeline.
# The data must be reshaped to ARD format before tfrmt can use it.

# ── Step 0: Reshape data to ARD format ──────────────────────────────────
# tfrmt requires Analysis Results Data: one row per statistic.
# arframe takes your summary table as-is. This reshape is extra work.
#
# tbl_ae_soc has columns: soc, pt, row_type, placebo, zom_50mg, zom_100mg, total
# tfrmt needs:  group, label, column, param, value
#
# In practice this reshape happens in the analysis pipeline (e.g. Tplyr/cardinal),
# but the point is: tfrmt cannot consume the wide summary table directly.

cat("== tfrmt + gt + docorator ==\n\n")

cat("── Step 0: Data reshape (tfrmt requires ARD format) ──\n")
cat("tfrmt cannot use wide summary data directly.\n")
cat("You must reshape to: group | label | column | param | value\n")
cat("This adds ~20 lines of tidyr::pivot_longer or reshape().\n\n")

# Simulated ARD reshape (showing the concept):
# ard <- tbl_ae_soc |>
#   tidyr::pivot_longer(
#     cols = c(placebo, zom_50mg, zom_100mg, total),
#     names_to = "column",
#     values_to = "value"
#   ) |>
#   dplyr::mutate(param = "n_pct")  # tfrmt needs a param column


cat("── Step 1: tfrmt format spec (~40 lines) ──\n")
cat(paste0(
  '
# library(tfrmt)
# library(tibble)
#
# ae_tfrmt <- tfrmt(
#   group  = soc,
#   label  = pt,
#   column = column,
#   param  = param,
#   value  = value,
#
#   # Number formatting per statistic type
#   body_plan = body_plan(
#     frmt_structure(
#       group_val = ".default",
#       label_val = ".default",
#       frmt("x (xx.x)")
#     )
#   ),
#
#   # Row grouping and spacing
#   row_grp_plan = row_grp_plan(
#     row_grp_structure(
#       group_val = ".default",
#       element_block(post_space = " ")
#     )
#   ),
#
#   # Column ordering
#   col_plan = col_plan(
#     pt,
#     placebo, zom_50mg, zom_100mg, total
#   ),
#
#   # Column labels with N-counts (manual)
#   col_style_plan = col_style_plan(
#     col_style_structure(
#       col = placebo,   width = 120, align = "center"
#     ),
#     col_style_structure(
#       col = zom_50mg,  width = 120, align = "center"
#     ),
#     col_style_structure(
#       col = zom_100mg, width = 120, align = "center"
#     ),
#     col_style_structure(
#       col = total,     width = 120, align = "center"
#     )
#   )
# )
#
# # Render to gt object
# ae_gt <- print_to_gt(ae_tfrmt, ard_data)
'
))

cat("\nNOTE: tfrmt has no decimal alignment — only left/center/right.\n")
cat("NOTE: N-counts in headers require manual paste0() or col_style_plan.\n")
cat("NOTE: SOC bold requires post-processing the gt object.\n\n")


cat("── Step 2: gt post-processing (~30 lines) ──\n")
cat(paste0(
  '
# # tfrmt produces a gt object, but it lacks:
# # - SOC row bolding (tfrmt has no conditional row styling)
# # - "Any TEAE" total row bolding
# # - PT indentation (tfrmt groups but does not indent)
# # You must manually add these to the gt object:
#
# ae_gt <- ae_gt |>
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     locations = gt::cells_row_groups()
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     locations = gt::cells_body(rows = row_type == "total")
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(indent = gt::px(20)),
#     locations = gt::cells_body(columns = pt, rows = row_type == "pt")
#   ) |>
#   gt::tab_header(
#     title = "Table 14.3.1",
#     subtitle = "TEAEs by SOC and Preferred Term"
#   ) |>
#   gt::tab_footnote("MedDRA version 26.0.") |>
#   gt::tab_footnote("Subjects counted once per SOC and PT.") |>
#   gt::tab_footnote("Sorted by descending total incidence.")
'
))

cat("\nNOTE: gt has no page-level features — no headers, footers, pagination.\n")
cat("NOTE: gt cannot do decimal alignment.\n")
cat("NOTE: gt cannot add continuation text on page 2+.\n\n")


cat("── Step 3: docorator wrapping (~20 lines) ──\n")
cat(paste0(
  '
# library(docorator)
#
# # docorator adds the document shell that gt lacks:
# ae_doc <- ae_gt |>
#   as_docorator(
#     orientation = "landscape",
#     margin = list(top = 1, bottom = 1, left = 1, right = 1)
#   ) |>
#   fancyhead(
#     left  = "TFRM-2024-001",
#     right = "CONFIDENTIAL"
#   ) |>
#   fancyfoot(
#     left  = "t_14_3_1.R",
#     right = "Page \\\\thepage\\\\ of \\\\lastpage"
#   )
#
# # Finally render to PDF
# render_pdf(ae_doc, file.path(tempdir(), "docorator_14_3_1.pdf"))
'
))

cat("\nNOTE: docorator PDF uses RMarkdown/Quarto + LaTeX — not direct XeLaTeX.\n")
cat("NOTE: docorator auto-scales gt tables to fit the page.\n")
cat("NOTE: docorator cannot add continuation text on subsequent pages.\n")
cat("NOTE: docorator has no control over repeating column headers.\n")
cat("NOTE: The gt table inside is still center-aligned, not decimal-aligned.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  C. Side-by-Side Summary                                              ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  SIDE-BY-SIDE: arframe vs tfrmt + gt + docorator\n")
cat("================================================================\n\n")

summary_lines <- c(
  "                              arframe          tfrmt + gt + docorator",
  "                              ───────          ──────────────────────",
  "Packages needed               1                3 (tfrmt + gt + docorator)",
  "Lines of code                 ~30              ~130 (+ data reshape)",
  "Input data format             Wide summary     ARD (one row per stat)",
  "Data reshape needed           NO               YES (pivot_longer)",
  "PDF backend                   XeLaTeX native   RMarkdown + LaTeX (docorator)",
  "RTF output                    Native           gt::gtsave (single-page)",
  "HTML output                   Native           gt (native)",
  "One spec, all formats         YES              NO (different pipelines)",
  "",
  "── Table Features ──",
  "Decimal alignment             YES              NO (center only)",
  "N-counts in headers           Automatic        Manual paste0()",
  "SOC/PT indent_by              1 line           Manual gt::tab_style()",
  "Three-level indent            1 line           NOT POSSIBLE",
  "Conditional row bold          fr_row_style()   Manual gt::tab_style()",
  "group_by keep-together        YES              NO",
  "Spanning headers              fr_spans()       gt::tab_spanner()",
  "",
  "── Page Features ──",
  "Multi-page pagination         YES              Partial (docorator)",
  "Repeating column headers      YES              NO",
  "Continuation text             YES              NO",
  "Page X of Y                   YES              YES (docorator)",
  "Pageheader / pagefooter       Built-in         docorator layer",
  "Landscape orientation         Built-in         docorator layer",
  "",
  "── Workflow ──",
  "Study-wide theme              fr_theme()       NOT POSSIBLE",
  "YAML config (_arframe.yml)    YES              NO",
  "Batch render 20 tables        for loop         Manual per-table docorator wrap",
  "Column splitting (wide)       YES              NOT POSSIBLE",
  "page_by (per-param pages)     YES              NOT POSSIBLE"
)

for (line in summary_lines) cat(line, "\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  D. Feature Comparison as PDF (arframe eating its own dog food)        ║
# ╚══════════════════════════════════════════════════════════════════════════╝

comparison <- data.frame(
  Feature = c(
    "Packages needed",
    "Lines of code (AE SOC/PT)",
    "Data reshape required",
    "Native PDF (typeset)",
    "PDF backend",
    "Native RTF",
    "Native HTML",
    "One spec -> all formats",
    "Decimal alignment",
    "N-counts in headers (auto)",
    "SOC/PT indent_by",
    "Three-level indent (SOC/HLT/PT)",
    "Conditional row bold",
    "group_by keep-together",
    "Spanning headers",
    "Multi-page pagination",
    "Repeating column headers",
    "Continuation text",
    "Page X of Y",
    "Pageheader / pagefooter",
    "Study-wide theme",
    "YAML config",
    "page_by (per-param pages)",
    "Column splitting (wide tables)",
    "Batch render (loop)",
    "No tidyverse dependency"
  ),
  arframe = c(
    "1", "~30", "NO", "YES", "XeLaTeX", "YES", "YES", "YES",
    "YES", "YES", "YES", "YES", "YES", "YES", "YES",
    "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES",
    "YES", "YES"
  ),
  pharmaverse = c(
    "3", "~130", "YES", "Partial", "RMarkdown", "Single-page", "YES", "NO",
    "NO", "Manual", "Manual", "NO", "Manual", "NO", "YES",
    "Partial", "NO", "NO", "YES", "docorator", "NO", "NO", "NO", "NO",
    "Manual", "NO"
  ),
  stringsAsFactors = FALSE
)

comparison_spec <- comparison |>
  fr_table() |>
  fr_titles(
    "arframe vs pharmaverse TLF stack",
    list("Feature Comparison: arframe vs tfrmt + gt + docorator", bold = TRUE),
    "PDF as Primary Output"
  ) |>
  fr_cols(
    Feature    = fr_col("Feature", width = 3.0, align = "left"),
    arframe    = fr_col("arframe\n(1 package)", width = 1.5, align = "center"),
    pharmaverse = fr_col("tfrmt + gt +\ndocorator", width = 1.5, align = "center")
  ) |>
  fr_header(bold = TRUE, align = "center") |>
  fr_hlines("header") |>
  fr_page(orientation = "landscape", font_family = "Courier New", font_size = 9) |>
  fr_styles(
    fr_style_if(
      condition = ~ .x == "YES",
      cols = c("arframe", "pharmaverse"),
      bold = TRUE
    ),
    fr_style_if(
      condition = ~ .x == "NO",
      cols = c("arframe", "pharmaverse"),
      fg = "red"
    )
  )

comparison_spec  # HTML preview
comparison_spec |> fr_render(file.path(tempdir(), "Package_Comparison.pdf"))

cat("\nComparison PDF written to:", file.path(tempdir(), "Package_Comparison.pdf"), "\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  E. Full Study Program — 7 Tables, All as PDF                         ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# A real CSR has 15–30 tables. Here's the core 7, each under 20 lines,
# all sharing one theme. With tfrmt + gt + docorator, each table needs:
#   - ARD data reshape
#   - tfrmt spec
#   - gt post-processing
#   - docorator wrapping
# That's 4 steps x 7 tables = 28 code blocks vs 7 pipelines below.

fr_theme_reset()
fr_theme(
  font_size   = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

n_itt    <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
n_safety <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)

# ── Table 14.1.1: Demographics ──
t_14_1_1 <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics",
            "Intent-to-Treat Population") |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo",          align = "decimal"),
    zom_50mg  = fr_col("Zomerane 50mg",    align = "decimal"),
    zom_100mg = fr_col("Zomerane 100mg",   align = "decimal"),
    total     = fr_col("Total",            align = "decimal"),
    group     = fr_col(visible = FALSE),
    .n = n_itt
  ) |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_footnotes("Percentages based on N per treatment group.",
               "MMSE = Mini-Mental State Examination.")

# ── Table 14.1.4: Disposition ──
t_14_1_4 <- tbl_disp |>
  fr_table() |>
  fr_titles("Table 14.1.4", "Subject Disposition",
            "All Randomized Subjects") |>
  fr_cols(
    category  = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo",        align = "decimal"),
    zom_50mg  = fr_col("Zomerane 50mg",  align = "decimal"),
    zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
    total     = fr_col("Total",          align = "decimal"),
    .n = n_itt
  ) |>
  fr_footnotes("Percentages based on N randomized per arm.")

# ── Table 14.2.1: Time-to-Event ──
t_14_2_1 <- tbl_tte |>
  fr_table() |>
  fr_titles("Table 14.2.1",
            list("Time to Study Withdrawal", bold = TRUE),
            "Intent-to-Treat Population") |>
  fr_cols(
    section   = fr_col(visible = FALSE),
    statistic = fr_col("", width = 3.5),
    zom_50mg  = fr_col("Zomerane\n50mg",  align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    placebo   = fr_col("Placebo",          align = "decimal"),
    .n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45)
  ) |>
  fr_rows(group_by = "section", blank_after = "section") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("statistic", pattern = "^[A-Z]"),
                 bold = TRUE)
  ) |>
  fr_footnotes("[a] Kaplan-Meier estimate with Greenwood 95% CI.",
               "[b] Two-sided log-rank test stratified by age group.",
               "[c] Cox proportional hazards model.",
               "NE = Not Estimable.")

# ── Table 14.3.1: AE by SOC/PT (reuse from above) ──
t_14_3_1 <- ae_arframe

# ── Table 14.3.1.1: Overall AE Summary ──
t_14_3_1_1 <- tbl_ae_summary |>
  fr_table() |>
  fr_titles("Table 14.3.1.1",
            "Overall Summary of Treatment-Emergent Adverse Events",
            "Safety Population") |>
  fr_cols(
    category  = fr_col("", width = 3.5),
    zom_50mg  = fr_col("Zomerane\n50mg",  align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    placebo   = fr_col("Placebo",          align = "decimal"),
    total     = fr_col("Total",            align = "decimal"),
    .n = n_safety
  ) |>
  fr_footnotes("Subjects may be counted in more than one category.")

# ── Table 14.4.1: Concomitant Medications ──
t_14_4_1 <- tbl_cm |>
  fr_table() |>
  fr_titles("Table 14.4.1",
            list("Concomitant Medications by Category and Agent", bold = TRUE),
            "Safety Population") |>
  fr_cols(
    category   = fr_col(visible = FALSE),
    medication = fr_col("Medication Category / Agent", width = 3.0),
    row_type   = fr_col(visible = FALSE),
    placebo    = fr_col("Placebo",          align = "decimal"),
    zom_50mg   = fr_col("Zomerane\n50mg",   align = "decimal"),
    zom_100mg  = fr_col("Zomerane\n100mg",  align = "decimal"),
    total      = fr_col("Total",            align = "decimal"),
    .n = n_safety
  ) |>
  fr_rows(group_by = "category", indent_by = "medication") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "total"),    bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "category"), bold = TRUE)
  ) |>
  fr_footnotes("Subjects counted once per category and medication.")

# ── Table 14.3.6: Vital Signs ──
vs_n <- aggregate(
  USUBJID ~ PARAM + TRTA, data = advs[advs$AVISIT == "Baseline", ],
  FUN = function(x) length(unique(x))
)

t_14_3_6 <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
  fr_table() |>
  fr_titles("Table 14.3.6",
            "Vital Signs --- Week 24 Summary",
            "Safety Population") |>
  fr_cols(
    param     = fr_col(visible = FALSE),
    timepoint = fr_col(visible = FALSE),
    statistic         = fr_col("Statistic", width = 1.2),
    placebo_base      = fr_col("Baseline"),
    placebo_value     = fr_col("Value"),
    placebo_chg       = fr_col("CFB"),
    zom_50mg_base     = fr_col("Baseline"),
    zom_50mg_value    = fr_col("Value"),
    zom_50mg_chg      = fr_col("CFB"),
    zom_100mg_base    = fr_col("Baseline"),
    zom_100mg_value   = fr_col("Value"),
    zom_100mg_chg     = fr_col("CFB"),
    .n = vs_n
  ) |>
  fr_rows(page_by = "param") |>
  fr_spans(
    "Placebo"        = c("placebo_base", "placebo_value", "placebo_chg"),
    "Zomerane 50mg"  = c("zom_50mg_base", "zom_50mg_value", "zom_50mg_chg"),
    "Zomerane 100mg" = c("zom_100mg_base", "zom_100mg_value", "zom_100mg_chg")
  ) |>
  fr_footnotes("CFB = Change from Baseline.")

# ── Batch render all 7 tables ──
tables <- list(
  "Table_14_1_1"   = t_14_1_1,
  "Table_14_1_4"   = t_14_1_4,
  "Table_14_2_1"   = t_14_2_1,
  "Table_14_3_1"   = t_14_3_1,
  "Table_14_3_1_1" = t_14_3_1_1,
  "Table_14_4_1"   = t_14_4_1,
  "Table_14_3_6"   = t_14_3_6
)

outdir <- file.path(tempdir(), "csr_tables")
dir.create(outdir, showWarnings = FALSE)

for (nm in names(tables)) {
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".pdf")))
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".rtf")))
}

cat("\n7 submission-quality tables written to:\n", outdir, "\n")
cat("  -> 7 PDFs (XeLaTeX, typeset quality)\n")
cat("  -> 7 RTFs (for eCTD if needed)\n")
cat("\n── Bottom Line ──\n")
cat("arframe:    ~200 lines total for 7 tables (theme + pipelines + batch render)\n")
cat("pharmaverse: ~900+ lines (ARD reshape + tfrmt + gt fixups + docorator per table)\n")
cat("SAS:        ~2000+ lines of PROC REPORT + ODS PDF macros\n")

fr_theme_reset()
