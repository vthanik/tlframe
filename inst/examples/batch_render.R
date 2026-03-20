# ============================================================================
# Batch Render — Metadata-Driven Table Production
# ============================================================================
#
# Two methods for running an entire CSR table package from a single manifest:
#
#   Method 1: Excel/CSV manifest → loop renders all tables
#   Method 2: R list with inline data + config → functional batch
#
# Both methods share one theme — no per-table formatting boilerplate.

library(arframe)
library(dplyr, warn.conflicts = FALSE)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Study Setup (shared by both methods)                                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝

fr_theme_reset()
fr_theme(
  font_size = 9, font_family = "Courier New", orientation = "landscape",
  hlines = "header", header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})", footnote_separator = FALSE,
  pagehead = list(left = "Protocol: TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

# Population N-counts
n_itt    <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
n_safety <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)

outdir <- file.path(tempdir(), "batch_output")
dir.create(outdir, showWarnings = FALSE)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  METHOD 1: Excel/CSV Manifest                                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# In production, this data frame would come from:
#   readxl::read_excel("TFL_Manifest.xlsx")
# or:
#   read.csv("tfl_manifest.csv")
#
# Each row = one table. Columns define everything needed to render it.

cat("══════════════════════════════════════════════════════════════\n")
cat("  METHOD 1: Excel/CSV Manifest-Driven Batch\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── The manifest (would be an Excel file in production) ──────────────────

manifest <- data.frame(
  # GSK naming: t_{domain}_{shell}{seq}_{variant}
  # domain: sp = study population, saf = safety
  # shell:  dmt = demographics, dst = disposition, ae = adverse event
  table_id   = c("t_sp_dmt01",     "t_sp_dst01",     "t_saf_ae01_all",    "t_saf_ae01_summ"),
  table_num  = c("Table 14.1.1",   "Table 14.1.4",    "Table 14.3.1",       "Table 14.3.1.1"),
  title      = c("Summary of Demographic and Baseline Characteristics",
                  "Subject Disposition",
                  "Summary of Adverse Events by System Organ Class and Preferred Term",
                  "Overall Summary of Treatment-Emergent Adverse Events"),
  popfl      = c("MFASFL",         "ENRLFL",          "SAFFL",             "SAFFL"),
  dataset    = c("tbl_demog",      "tbl_disp",        "tbl_ae_soc",        "tbl_ae_summary"),
  n_pool     = c("n_itt",          "n_itt",           "n_safety",          "n_safety"),
  footnote1  = c("Percentages based on N per treatment group.",
                  "Percentages based on N randomized per arm.",
                  "MedDRA version 26.0.",
                  "Subjects may be counted in more than one category."),
  footnote2  = c("MMSE = Mini-Mental State Examination.",
                  NA,
                  "Subjects counted once per SOC and PT.",
                  NA),
  group_by   = c("group",          NA,                "soc",               NA),
  indent_by  = c(NA,               NA,                "pt",                NA),
  bold_rows  = c(NA,               NA,                "row_type:soc,total", NA),
  blank_after = c("group",         NA,                NA,                  NA),
  title_bold = c(FALSE,            FALSE,             TRUE,                FALSE),
  continuation = c(FALSE,          FALSE,             TRUE,                FALSE),
  stringsAsFactors = FALSE
)

cat("Manifest (would be read from Excel):\n")
print(manifest[, c("table_id", "table_num", "popfl", "dataset", "n_pool")])
cat("\n")

# ── Column definitions per dataset (could also come from Excel) ──────────

col_defs <- list(
  tbl_demog = list(
    cols = list(
      characteristic = list(label = "", width = 2.5),
      placebo   = list(label = "Placebo",        align = "decimal"),
      zom_50mg  = list(label = "Zomerane 50mg",  align = "decimal"),
      zom_100mg = list(label = "Zomerane 100mg", align = "decimal"),
      total     = list(label = "Total",          align = "decimal"),
      group     = list(visible = FALSE)
    )
  ),
  tbl_disp = list(
    cols = list(
      category  = list(label = "", width = 2.5),
      placebo   = list(label = "Placebo",        align = "decimal"),
      zom_50mg  = list(label = "Zomerane 50mg",  align = "decimal"),
      zom_100mg = list(label = "Zomerane 100mg", align = "decimal"),
      total     = list(label = "Total",          align = "decimal")
    )
  ),
  tbl_ae_soc = list(
    cols = list(
      soc       = list(visible = FALSE),
      pt        = list(label = "System Organ Class\n  Preferred Term", width = 3.5),
      row_type  = list(visible = FALSE),
      placebo   = list(label = "Placebo",          align = "decimal"),
      zom_50mg  = list(label = "Zomerane\n50mg",   align = "decimal"),
      zom_100mg = list(label = "Zomerane\n100mg",  align = "decimal"),
      total     = list(label = "Total",            align = "decimal")
    )
  ),
  tbl_ae_summary = list(
    cols = list(
      category  = list(label = "", width = 3.5),
      zom_50mg  = list(label = "Zomerane\n50mg",  align = "decimal"),
      zom_100mg = list(label = "Zomerane\n100mg", align = "decimal"),
      placebo   = list(label = "Placebo",          align = "decimal"),
      total     = list(label = "Total",            align = "decimal")
    )
  )
)

# ── The batch engine: loop over manifest rows ────────────────────────────

build_table_from_manifest <- function(row, col_defs) {
  # Get data
  data <- get(row$dataset, envir = asNamespace("arframe"))

  # Get N-counts
  n_vec <- get(row$n_pool)

  # Build fr_col objects from col_defs
  cdef <- col_defs[[row$dataset]]$cols
  fr_cols_list <- lapply(names(cdef), function(nm) {
    d <- cdef[[nm]]
    if (isFALSE(d$visible)) return(fr_col(visible = FALSE))
    do.call(fr_col, d)
  })
  names(fr_cols_list) <- names(cdef)

  # Start pipeline
  spec <- data |> fr_table()

  # Population label from flag
  pop_map <- c(SAFFL = "Safety Population", MFASFL = "mFAS Population",
               ENRLFL = "Enrolled Population", FASFL = "FAS Population")
  pop_label <- unname(pop_map[row$popfl]) %||% row$popfl

  # Titles
  if (isTRUE(row$title_bold)) {
    spec <- spec |> fr_titles(row$table_num, list(row$title, bold = TRUE), pop_label)
  } else {
    spec <- spec |> fr_titles(row$table_num, row$title, pop_label)
  }

  # Continuation
  if (isTRUE(row$continuation)) {
    spec <- spec |> fr_page(continuation = "(continued)")
  }

  # Columns (using do.call to pass the list)
  col_args <- c(list(spec = spec), fr_cols_list, list(.n = n_vec))
  spec <- do.call(fr_cols, col_args)

  # Row organization
  if (!is.na(row$group_by)) {
    row_args <- list(spec = spec, group_by = row$group_by)
    if (!is.na(row$indent_by))   row_args$indent_by   <- row$indent_by
    if (!is.na(row$blank_after)) row_args$blank_after  <- row$blank_after
    spec <- do.call(fr_rows, row_args)
  }

  # Conditional bold rows (format: "col:val1,val2")
  if (!is.na(row$bold_rows)) {
    parts <- strsplit(row$bold_rows, ":")[[1]]
    col_name <- parts[1]
    values <- strsplit(parts[2], ",")[[1]]
    styles <- lapply(values, function(v) {
      fr_row_style(rows = fr_rows_matches(col_name, value = v), bold = TRUE)
    })
    spec <- do.call(fr_styles, c(list(spec = spec), styles))
  }

  # Footnotes
  fns <- c(row$footnote1, row$footnote2)
  fns <- fns[!is.na(fns)]
  if (length(fns) > 0) {
    spec <- do.call(fr_footnotes, c(list(spec = spec), as.list(fns)))
  }

  spec
}

# ── Run the batch ────────────────────────────────────────────────────────

cat("Running batch render...\n")
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  spec <- build_table_from_manifest(row, col_defs)
  spec |> fr_render(file.path(outdir, paste0(row$table_id, ".pdf")))
  spec |> fr_render(file.path(outdir, paste0(row$table_id, ".rtf")))
  cat(sprintf("  [%d/%d] %s -> PDF + RTF\n", i, nrow(manifest), row$table_id))
}

cat("\nMethod 1 complete:", nrow(manifest), "tables rendered.\n")
cat("Output:", outdir, "\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  METHOD 2: R List Registry — Functional Batch                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# For teams that prefer R over Excel, define a registry of table specs.
# Each entry is a function that returns an fr_spec — lazy evaluation.
# The registry can live in a shared .R file that the team maintains.

cat("══════════════════════════════════════════════════════════════\n")
cat("  METHOD 2: R List Registry — Functional Batch\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── Table registry: each entry is a function returning an fr_spec ────────

table_registry <- list(

  "t_sp_dmt01" = function() {
    tbl_demog |>
      fr_table() |>
      fr_titles("Table 14.1.1", "Summary of Demographic and Baseline Characteristics",
                "mFAS Population") |>
      fr_cols(
        characteristic = fr_col("", width = 2.5),
        placebo   = fr_col("Placebo",        align = "decimal"),
        zom_50mg  = fr_col("Zomerane 50mg",  align = "decimal"),
        zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
        total     = fr_col("Total",          align = "decimal"),
        group     = fr_col(visible = FALSE),
        .n = n_itt
      ) |>
      fr_rows(group_by = "group", blank_after = "group") |>
      fr_footnotes("Percentages based on N per treatment group.",
                   "MMSE = Mini-Mental State Examination.")
  },

  "t_sp_dst01" = function() {
    tbl_disp |>
      fr_table() |>
      fr_titles("Table 14.1.4", "Subject Disposition", "Enrolled Population") |>
      fr_cols(
        category  = fr_col("", width = 2.5),
        placebo   = fr_col("Placebo",        align = "decimal"),
        zom_50mg  = fr_col("Zomerane 50mg",  align = "decimal"),
        zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
        total     = fr_col("Total",          align = "decimal"),
        .n = n_itt
      ) |>
      fr_footnotes("Percentages based on N randomized per arm.")
  },

  "t_saf_ae01_all" = function() {
    tbl_ae_soc |>
      fr_table() |>
      fr_titles("Table 14.3.1",
                list("Summary of Adverse Events by System Organ Class and Preferred Term",
                     bold = TRUE),
                "Safety Population") |>
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
      fr_footnotes("MedDRA version 26.0.",
                   "Subjects counted once per SOC and PT.",
                   "Sorted by descending total incidence.")
  },

  "t_saf_ae01_summ" = function() {
    tbl_ae_summary |>
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
  },

  "t_eff_efft01" = function() {
    tbl_tte |>
      fr_table() |>
      fr_titles("Table 14.2.1",
                list("Time to Study Withdrawal", bold = TRUE),
                "mFAS Population") |>
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
      fr_footnotes("[a] KM estimate with 95% CI.",
                   "[b] Stratified log-rank test.",
                   "NE = Not Estimable.")
  },

  "t_sp_cmt01" = function() {
    tbl_cm |>
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
  }
)

# ── Batch runner ─────────────────────────────────────────────────────────

outdir2 <- file.path(tempdir(), "batch_registry")
dir.create(outdir2, showWarnings = FALSE)

cat("Running registry batch...\n")
for (nm in names(table_registry)) {
  spec <- table_registry[[nm]]()
  spec |> fr_render(file.path(outdir2, paste0(nm, ".pdf")))
  spec |> fr_render(file.path(outdir2, paste0(nm, ".rtf")))
  cat(sprintf("  %s -> PDF + RTF\n", nm))
}

cat("\nMethod 2 complete:", length(table_registry), "tables rendered.\n")
cat("Output:", outdir2, "\n\n")

# ── Selective render (run just one or a few) ─────────────────────────────

cat("── Selective render: run just 2 tables ──\n")
selected <- c("t_sp_dmt01", "t_saf_ae01_all")
for (nm in selected) {
  table_registry[[nm]]() |>
    fr_render(file.path(outdir2, paste0(nm, "_rerun.pdf")))
  cat(sprintf("  %s rerun -> PDF\n", nm))
}


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Comparison: Method 1 vs Method 2                                        ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  METHOD COMPARISON\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("Method 1 (Excel Manifest):\n")
cat("  + Titles/footnotes editable by non-programmers (in Excel)\n")
cat("  + Easy to add/remove tables — just edit a spreadsheet row\n")
cat("  + Column defs can also live in Excel (JSON-like)\n")
cat("  + Good for study teams where stats reviews the manifest\n")
cat("  - Requires a build_table_from_manifest() engine function\n")
cat("  - Complex tables (styles, indent_by) need encoding convention\n\n")

cat("Method 2 (R List Registry):\n")
cat("  + Full R expressiveness — any arframe feature available\n")
cat("  + Each table is self-contained and readable\n")
cat("  + Easy to run selectively: table_registry[['t_saf_ae01_all']]()\n")
cat("  + Version-controlled in git alongside the analysis code\n")
cat("  - Titles/footnotes changes require editing R code\n\n")

cat("Both methods share:\n")
cat("  - One fr_theme() call — all tables inherit formatting\n")
cat("  - One for-loop — renders everything to PDF + RTF\n")
cat("  - No per-table docorator wrapping\n")
cat("  - No tfl_format() helper function needed\n")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  vs GSK pharmaverse batch approach\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("In the GSK codebase, batch execution means:\n")
cat("  - One .R file PER table (t_sp_dmt01.R, t_saf_ae01_all.R, ...)\n")
cat("  - Each file: ~300–600 lines (data prep + tfrmt + gt + docorator)\n")
cat("  - Each file sources rx_setup.R, gsk_styling.R, tfl_format.R\n")
cat("  - A shell script (navigate_shell.sh) runs each .R file\n")
cat("  - No shared theme — every table repeats formatting config\n\n")

cat("With arframe:\n")
cat("  - One .R file for ALL tables (this script)\n")
cat("  - ~15 lines per table (Method 2) or ~1 row per table (Method 1)\n")
cat("  - One theme, one loop, done\n")

fr_theme_reset()
