# ──────────────────────────────────────────────────────────────────────────────
# render-pdf.R — PDF backend for fr_render()
#
# Thin wrapper: generates .tex via render_latex(), compiles with XeLaTeX.
# ──────────────────────────────────────────────────────────────────────────────


#' Render an fr_spec to PDF via XeLaTeX
#'
#' @param spec Finalized fr_spec object.
#' @param page_groups List of page group lists (data + group_label).
#' @param col_panels List of column name vectors (one per panel).
#' @param path Output file path (.pdf).
#' @noRd
render_pdf <- function(spec, page_groups, col_panels, path) {
  # 1. Write .tex to tempdir
  base_name <- tools::file_path_sans_ext(basename(path))
  tex_dir <- tempdir()
  tex_path <- file.path(tex_dir, paste0(base_name, ".tex"))
  render_latex(spec, page_groups, col_panels, tex_path)

  # 2. Find xelatex
  xelatex <- find_xelatex()
  if (is.null(xelatex)) {
    cli_abort(c(
      "XeLaTeX not found on your system.",
      "i" = "Install TeX Live, MiKTeX, or use {.pkg tinytex}: {.code tinytex::install_tinytex()}."
    ))
  }

  # 3. Compile (run twice for page references like LastPage)
  args <- c("-interaction=nonstopmode", "-output-directory", tex_dir, tex_path)

  res1 <- system2(xelatex, args, stdout = TRUE, stderr = TRUE)
  res2 <- system2(xelatex, args, stdout = TRUE, stderr = TRUE)

  # 4. Check compilation succeeded
  pdf_temp <- file.path(tex_dir, paste0(base_name, ".pdf"))
  if (!file.exists(pdf_temp)) {
    log_file <- file.path(tex_dir, paste0(base_name, ".log"))
    log_tail <- if (file.exists(log_file)) {
      log_lines <- readLines(log_file, warn = FALSE)
      utils::tail(log_lines, 20L)
    } else {
      "No log file found."
    }
    cli_abort(c(
      "XeLaTeX compilation failed.",
      "i" = "Source file: {.path {tex_path}}",
      "i" = "Last 20 lines of log:",
      paste0("  ", log_tail)
    ))
  }

  # 5. Copy PDF to output path
  file.copy(pdf_temp, path, overwrite = TRUE)

  # 6. Cleanup temp files (keep .tex for debugging if needed)
  exts <- c(".aux", ".log", ".out", ".toc")
  for (ext in exts) {
    f <- file.path(tex_dir, paste0(base_name, ext))
    if (file.exists(f)) unlink(f)
  }

  invisible(path)
}


#' Find xelatex binary
#'
#' Checks: (1) Sys.which, (2) tinytex if available.
#'
#' @return Path to xelatex binary, or NULL if not found.
#' @noRd
find_xelatex <- function() {
  # 1. System PATH
  path <- Sys.which("xelatex")
  if (nzchar(path)) return(unname(path))

  # 2. tinytex
  if (requireNamespace("tinytex", quietly = TRUE)) {
    path <- tryCatch(
      tinytex::tinytex_root(),
      error = function(e) NULL
    )
    if (!is.null(path)) {
      bin <- file.path(path, "bin", "*", "xelatex")
      candidates <- Sys.glob(bin)
      if (length(candidates) > 0L) return(candidates[1L])

      # Windows: .exe
      bin_exe <- file.path(path, "bin", "*", "xelatex.exe")
      candidates <- Sys.glob(bin_exe)
      if (length(candidates) > 0L) return(candidates[1L])
    }
  }

  NULL
}
