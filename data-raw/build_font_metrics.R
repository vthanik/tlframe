# build_font_metrics.R — Parse AFM files into R/sysdata.rda
#
# Parses Adobe Font Metrics files shipped with R for the standard 14 PDF fonts.
# Extracts per-character widths (in 1/1000 em units) and stores as a named list
# in R/sysdata.rda, which is auto-loaded at package load time.
#
# Usage: Rscript data-raw/build_font_metrics.R

afm_dir <- file.path(R.home("library"), "grDevices", "afm")

# The 12 font variants we need (4 each for Helvetica, Times, Courier)
afm_files <- c(

  "Helvetica"             = "Helvetica.afm.gz",
  "Helvetica-Bold"        = "Helvetica-Bold.afm.gz",
  "Helvetica-Oblique"     = "Helvetica-Oblique.afm.gz",
  "Helvetica-BoldOblique" = "Helvetica-BoldOblique.afm.gz",

  "Times-Roman"           = "Times-Roman.afm.gz",
  "Times-Bold"            = "Times-Bold.afm.gz",
  "Times-Italic"          = "Times-Italic.afm.gz",
  "Times-BoldItalic"      = "Times-BoldItalic.afm.gz",

  "Courier"               = "Courier.afm.gz",
  "Courier-Bold"          = "Courier-Bold.afm.gz",
  "Courier-Oblique"       = "Courier-Oblique.afm.gz",
  "Courier-BoldOblique"   = "Courier-BoldOblique.afm.gz"
)

parse_afm <- function(filepath) {
  con <- gzfile(filepath, "r")
  on.exit(close(con))
  lines <- readLines(con)

  # Extract character metrics: lines matching "C <code> ; WX <width> ; N <name>"
  metric_lines <- grep("^C\\s", lines, value = TRUE)

  # Parse each line
  widths <- integer(0)
  names_vec <- character(0)

  for (line in metric_lines) {
    # Extract character code
    code_match <- regmatches(line, regexec("^C\\s+(-?\\d+)", line))[[1L]]
    code <- as.integer(code_match[2L])

    # Extract width
    wx_match <- regmatches(line, regexec("WX\\s+(\\d+)", line))[[1L]]
    if (length(wx_match) < 2L) next
    wx <- as.integer(wx_match[2L])

    # For coded characters (C >= 32), store by character
    if (code >= 32L && code <= 255L) {
      ch <- rawToChar(as.raw(code))
      names_vec <- c(names_vec, ch)
      widths <- c(widths, wx)
    }
  }

  names(widths) <- names_vec
  widths
}

# Parse all fonts
afm_metrics <- vector("list", length(afm_files))
names(afm_metrics) <- names(afm_files)

for (font_name in names(afm_files)) {
  filepath <- file.path(afm_dir, afm_files[[font_name]])
  if (!file.exists(filepath)) {
    stop("AFM file not found: ", filepath)
  }
  cat("Parsing:", font_name, "\n")
  afm_metrics[[font_name]] <- parse_afm(filepath)
}

# Verify: Helvetica space should be 278, M should be 889 (or similar)
cat("\nVerification:\n")
cat("  Helvetica 'M' width:", afm_metrics[["Helvetica"]]["M"], "\n")
cat("  Helvetica 'i' width:", afm_metrics[["Helvetica"]]["i"], "\n")
cat("  Helvetica ' ' width:", afm_metrics[["Helvetica"]][" "], "\n")
cat("  Courier 'M' width:  ", afm_metrics[["Courier"]]["M"], "\n")
cat("  Courier 'i' width:  ", afm_metrics[["Courier"]]["i"], "\n")

# Save to R/sysdata.rda
save(afm_metrics, file = "R/sysdata.rda", compress = "xz")
cat("\nSaved to R/sysdata.rda\n")
