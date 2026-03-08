# ──────────────────────────────────────────────────────────────────────────────
# constants.R — Single source of truth for all backend-independent lookup data
#
# DESIGN DECISIONS:
#
#   1. TOKEN SYNTAX: {thepage}, {total_pages}, {program}, {datetime}
#      Same single-brace syntax used for inline markup ({fr_super(1)}).
#      Pagehead/pagefoot strings never pass through glue::glue() — tokens
#      are resolved by simple find-and-replace at render time.
#      This is how Python's string.Template, Rust's format!(), and
#      JavaScript's template literals all work: {name} → value.
#      Literal brace in output: "{{" → "{" (standard glue escaping).
#
#   2. PRIVATE ENVIRONMENT: All constants live inside `fr_env`, a private
#      environment (pattern from ggplot2's `ggplot_global`). Nothing leaks
#      into the package namespace.
#
#   3. rlang: All validation helpers use rlang::caller_arg() and
#      rlang::caller_env() instead of deparse(substitute()).
#
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# Private environment — like ggplot2's ggplot_global
# ══════════════════════════════════════════════════════════════════════════════

#' Private environment for tlframe internal constants and state.
#' Not exported. Other packages should not manipulate this directly.
#' @noRd
fr_env <- new.env(parent = emptyenv())


# ══════════════════════════════════════════════════════════════════════════════
# 1. Font Family Classification
# ══════════════════════════════════════════════════════════════════════════════

fr_env$fonts <- list(

  modern = list(
    names          = c("Courier New", "Courier", "Consolas", "Lucida Console",
                        "DejaVu Sans Mono", "Liberation Mono"),
    rtf_family     = "fmodern",
    rtf_prq        = 1L,
    tex_cmd        = "\\ttfamily",
    afm_name       = "Courier",
    afm_bold       = "Courier-Bold",
    afm_italic     = "Courier-Oblique",
    afm_bolditalic = "Courier-BoldOblique"
  ),

  swiss = list(
    names          = c("Arial", "Helvetica", "Calibri", "Verdana", "Tahoma",
                        "Segoe UI", "DejaVu Sans", "Liberation Sans"),
    rtf_family     = "fswiss",
    rtf_prq        = 2L,
    tex_cmd        = "\\sffamily",
    afm_name       = "Helvetica",
    afm_bold       = "Helvetica-Bold",
    afm_italic     = "Helvetica-Oblique",
    afm_bolditalic = "Helvetica-BoldOblique"
  ),

  roman = list(
    names          = c("Times New Roman", "Times", "Georgia", "Palatino",
                        "Book Antiqua", "Cambria", "DejaVu Serif",
                        "Liberation Serif"),
    rtf_family     = "froman",
    rtf_prq        = 2L,
    tex_cmd        = "\\rmfamily",
    afm_name       = "Times-Roman",
    afm_bold       = "Times-Bold",
    afm_italic     = "Times-Italic",
    afm_bolditalic = "Times-BoldItalic"
  )
)


# Liberation font fallback map: family type → Liberation font name
fr_env$liberation_fallback <- c(
  modern = "Liberation Mono",
  swiss  = "Liberation Sans",
  roman  = "Liberation Serif"
)

# Liberation font file prefix map: font name → file prefix
fr_env$liberation_file_prefix <- c(
  "Liberation Mono"  = "LiberationMono",
  "Liberation Sans"  = "LiberationSans",
  "Liberation Serif" = "LiberationSerif"
)


#' Check if a system font is available for XeLaTeX
#'
#' Liberation fonts always return `TRUE` (bundled). On Windows, standard fonts
#' are always present. On Linux/macOS, checks via `fc-list`.
#'
#' @param font_name Character scalar. Font name to check.
#' @return Logical scalar.
#' @noRd
is_system_font_available <- function(font_name) {

  # Bundled Liberation fonts are always available
  if (font_name %in% fr_env$liberation_fallback) return(TRUE)

  os <- tolower(Sys.info()[["sysname"]])

  # Windows always has standard fonts (Courier New, Arial, Times New Roman)
  if (os == "windows") return(TRUE)

  # Linux/macOS: use fc-list to check
  fc_list <- Sys.which("fc-list")
  if (!nzchar(fc_list)) return(TRUE)  # can't check, assume available

  result <- tryCatch(
    system2("fc-list", args = c(":", "family"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )

  if (length(result) == 0L) return(TRUE)  # can't determine, assume available

  # fc-list returns "family1,family2" per line; check if font_name appears
  any(vapply(result, function(line) {
    families <- trimws(strsplit(line, ",", fixed = TRUE)[[1L]])
    font_name %in% families
  }, logical(1L)))
}


#' Resolve a font for LaTeX rendering, with Liberation fallback
#'
#' If the requested font is available on the system, returns it as-is.
#' Otherwise, falls back to the metrically equivalent Liberation font
#' and returns the path to the bundled font files.
#'
#' @param font_name Character scalar. Requested font name.
#' @return Named list with `name` (font name) and `path` (NULL or directory path).
#' @noRd
resolve_latex_font <- function(font_name) {

  if (is_system_font_available(font_name)) {
    return(list(name = font_name, path = NULL))
  }

  # Determine family type and map to Liberation equivalent

  fam <- lookup_font_family(font_name)
  lib_name <- fr_env$liberation_fallback[[fam]]
  lib_path <- system.file("fonts", "liberation", package = "tlframe")

  cli::cli_inform(c(
    "i" = "Font {.val {font_name}} not found on system.",
    "*" = "Falling back to bundled {.val {lib_name}}."
  ))

  list(name = lib_name, path = lib_path)
}


#' OS-aware default font triplet (mono, sans, serif)
#' @return Named list: `mono`, `sans`, `serif`
#' @noRd
os_default_fonts <- function() {
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    list(mono = "Courier New", sans = "Arial",     serif = "Times New Roman")
  } else if (os == "darwin") {
    list(mono = "Courier New", sans = "Helvetica",  serif = "Times New Roman")
  } else {
    list(mono = "Liberation Mono", sans = "Liberation Sans",
         serif = "Liberation Serif")
  }
}


#' Lookup font family type from font name
#' @param font_name Character scalar. e.g. "Courier New"
#' @return Character: "modern", "swiss", or "roman"
#' @noRd
lookup_font_family <- function(font_name) {
  for (fam in names(fr_env$fonts)) {
    if (font_name %in% fr_env$fonts[[fam]]$names) return(fam)
  }
  "modern"
}


#' Resolve AFM font name from font family + bold/italic
#'
#' @param font_family Character. Family type ("modern"/"swiss"/"roman") or
#'   a font name like "Courier New".
#' @param bold Logical.
#' @param italic Logical.
#' @return Character. AFM font name (e.g. "Helvetica-Bold").
#' @noRd
resolve_afm_name <- function(font_family, bold = FALSE, italic = FALSE) {
  fam <- if (font_family %in% names(fr_env$fonts)) {
    font_family
  } else {
    lookup_font_family(font_family)
  }
  info <- fr_env$fonts[[fam]]
  if (bold && italic) return(info$afm_bolditalic)
  if (bold)           return(info$afm_bold)
  if (italic)         return(info$afm_italic)
  info$afm_name
}


#' Get RTF font family keyword
#' @noRd
get_rtf_font_family <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$rtf_family
}


#' Get RTF pitch value
#' @noRd
get_rtf_font_prq <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$rtf_prq
}


#' Get LaTeX font family command
#' @noRd
get_tex_font_cmd <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$tex_cmd
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Paper Dimensions (twips; 1 inch = 1440 twips)
# ══════════════════════════════════════════════════════════════════════════════

fr_env$paper <- list(
  letter = c(width = 12240L, height = 15840L),
  a4     = c(width = 11906L, height = 16838L),
  legal  = c(width = 12240L, height = 20163L)
)


#' Get paper dimensions, accounting for orientation
#' @noRd
paper_dims_twips <- function(paper = "letter", orientation = "landscape") {
  dims <- fr_env$paper[[paper]]
  if (is.null(dims)) {
    cli_warn("Unknown paper size {.val {paper}}, using {.val letter}.")
    dims <- fr_env$paper[["letter"]]
  }
  if (orientation == "landscape") {
    c(width = unname(dims[["height"]]), height = unname(dims[["width"]]))
  } else {
    dims
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. RTF Border Styles + hline Presets
#
# linestyle values follow CSS <line-style> (MDN / W3C):
#   solid | dashed | dotted | double
#
# Preset names follow HTML <table frame=> (RFC 1942 / W3C):
#   void    — no outer rules
#   above   — top rule only
#   below   — bottom rule only
#   hsides  — top and bottom rules
#   box     — all four sides
# Plus booktabs from the LaTeX Companion (3rd ed, §6.6.5).
# ══════════════════════════════════════════════════════════════════════════════

#' RTF border linestyle control words
#'
#' Maps tlframe linestyle names to RTF border style control words.
#' Ref: Word 2007 RTF Specification §2.6.6.3.
#' @noRd
fr_env$linestyle_rtf <- list(
  solid   = "\\brdrs",
  dashed  = "\\brdrdash",
  dotted  = "\\brdrdot",
  double  = "\\brdrdb",
  dashdot = "\\brdrdashd"
)

#' Valid linestyle values
#'
#' Follows CSS <line-style> vocabulary (MDN / W3C) plus "dashdot" from
#' SAS ODS BORDERSTYLE. Used in fr_hlines(), fr_vlines(), and new_fr_rule().
#' @noRd
fr_env$valid_linestyles <- c("solid", "dashed", "dotted", "double", "dashdot")

#' Named line width constants (in points)
#'
#' Used by resolve_line_width() to translate named widths to numeric pt values.
#' Pharma convention: thin (0.5pt) is the standard rule weight.
#' @noRd
fr_env$line_widths <- c(
  hairline = 0.25,
  thin     = 0.50,
  medium   = 1.00,
  thick    = 1.50
)


#' Resolve a line width to a numeric pt value
#'
#' Accepts:
#'   - A named shorthand: "hairline" (0.25pt), "thin" (0.5pt),
#'     "medium" (1pt), "thick" (1.5pt)
#'   - A positive numeric value in points (e.g. 0.75)
#'   - NULL — returns the default width (0.5pt, "thin")
#'
#' @param wd Character shorthand or positive numeric (pt). NULL → 0.5.
#' @return Numeric. Width in points.
#' @noRd
resolve_line_width <- function(wd,
                                arg  = caller_arg(wd),
                                call = caller_env()) {
  if (is.null(wd)) return(0.5)

  if (is.character(wd)) {
    if (length(wd) != 1L) {
      cli_abort("{.arg {arg}} must be a single string or number.", call = call)
    }
    w <- unname(fr_env$line_widths[wd])
    if (is.na(w)) {
      cli_abort(
        c("{.arg {arg}} {.val {wd}} is not a recognised width name.",
          "i" = "Named options: {.val {names(fr_env$line_widths)}}.",
          "i" = "Or pass a positive number in points, e.g. {.code wd = 0.75}."),
        arg = arg, call = call
      )
    }
    return(w)
  }

  check_positive_num(wd, arg = arg, call = call)
  wd
}


fr_env$cell_border_rtf <- list(
  top    = "\\clbrdrt",
  bottom = "\\clbrdrb",
  left   = "\\clbrdrl",
  right  = "\\clbrdrr"
)

fr_env$para_border_rtf <- list(
  top    = "\\brdrt",
  bottom = "\\brdrb",
  left   = "\\brdrl",
  right  = "\\brdrr"
)


# ── Grouped sub-list: presets ──────────────────────────────────────────────
#' Preset constants for rules and line widths.
#' Access via fr_env$presets$hline, fr_env$presets$line_widths, etc.
#' Legacy flat accessors (fr_env$hline_presets, fr_env$line_widths) are
#' preserved below for backward compatibility.
#' @noRd
fr_env$presets <- list()

#' hline preset definitions
#'
#' Names:
#'   header     — single rule below column header (most common in TFL outputs)
#'   open       — rule above header + below header; no bottom border
#'   hsides     — top and bottom rules, no mid-rule (HTML frame="hsides")
#'   above      — top rule only (HTML frame="above")
#'   below      — bottom rule only (HTML frame="below")
#'   box        — full outer border on all four sides
#'   booktabs   — thick top + thin mid + thick bottom (LaTeX booktabs package)
#'   void       — no rules at all
#'
#' Each rule definition: region, side, width (pt), linestyle, fg (hex)
#' box is a special sentinel — backends render all four borders separately.
#' @noRd
fr_env$hline_presets <- list(

  # header — single thin rule below column header only.
  # The most common style in TFL outputs (ICH E3).
  # No top border, no bottom border; footnotes follow the last body row directly.
  header = list(
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # open — rule above header + rule below header; no bottom border.
  # Use when the table bottom is immediately followed by footnotes.
  open = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # hsides — top and bottom rules only, no mid-rule.
  hsides = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "body",   side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # above — single rule above column header only.
  above = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # below — single rule below last body row only.
  below = list(
    list(region = "body", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # box — all four outer sides (sentinel; backends expand to 4 borders).
  box = "box",

  # booktabs — from LaTeX Companion §6.6.5:
  #   \toprule    heavyrulewidth ≈ 1.0pt
  #   \midrule    lightrulewidth ≈ 0.5pt
  #   \bottomrule heavyrulewidth ≈ 1.0pt
  booktabs = list(
    list(region = "header", side = "above", width = 1.0,
         linestyle = "solid", fg = "#000000"),
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "body",   side = "below", width = 1.0,
         linestyle = "solid", fg = "#000000")
  ),

  # void — no rules.
  void = list()
)

# Grouped sub-list aliases for organized access
fr_env$presets$hline       <- fr_env$hline_presets
fr_env$presets$linestyles  <- fr_env$valid_linestyles
fr_env$presets$line_widths <- fr_env$line_widths


# ══════════════════════════════════════════════════════════════════════════════
# 4. LaTeX Special Character Escaping + Unicode Map
# ══════════════════════════════════════════════════════════════════════════════

fr_env$latex_specials <- c(
  "\\" = "\\textbackslash{}",
  "&"  = "\\&",
  "%"  = "\\%",
  "$"  = "\\$",
  "#"  = "\\#",
  "_"  = "\\_",
  "{"  = "\\{",
  "}"  = "\\}",
  "~"  = "\\textasciitilde{}",
  "^"  = "\\textasciicircum{}"
)

fr_env$rtf_specials <- c(
  "\\" = "\\\\",
  "{"  = "\\{",
  "}"  = "\\}"
)

#' Unicode → LaTeX command map
#'
#' Each entry: "unicode_char" = "latex_command"
#' Inline comments show the original character for maintainability.
#' @noRd
fr_env$latex_unicode <- c(
  # ── Math operators & relations ──
  "\u00b1" = "\\ensuremath{\\pm}",            # ± plus-minus
  "\u2264" = "\\ensuremath{\\leq}",           # ≤ less-than-or-equal
  "\u2265" = "\\ensuremath{\\geq}",           # ≥ greater-than-or-equal
  "\u2260" = "\\ensuremath{\\neq}",           # ≠ not-equal
  "\u00d7" = "\\ensuremath{\\times}",         # × multiplication
  "\u00f7" = "\\ensuremath{\\div}",           # ÷ division
  "\u221e" = "\\ensuremath{\\infty}",         # ∞ infinity
  "\u2248" = "\\ensuremath{\\approx}",        # ≈ approximately
  "\u221a" = "\\ensuremath{\\sqrt{}}",        # √ square root

  # ── Superscript digits ──
  "\u00b9" = "\\textsuperscript{1}",          # ¹ superscript one
  "\u00b2" = "\\textsuperscript{2}",          # ² superscript two
  "\u00b3" = "\\textsuperscript{3}",          # ³ superscript three

  # ── Greek letters (common in clinical statistics) ──
  "\u03b1" = "\\ensuremath{\\alpha}",         # α alpha
  "\u03b2" = "\\ensuremath{\\beta}",          # β beta
  "\u03b3" = "\\ensuremath{\\gamma}",         # γ gamma
  "\u03b4" = "\\ensuremath{\\delta}",         # δ delta
  "\u03bb" = "\\ensuremath{\\lambda}",        # λ lambda
  "\u03bc" = "\\ensuremath{\\mu}",            # μ mu
  "\u03c0" = "\\ensuremath{\\pi}",            # π pi
  "\u03c3" = "\\ensuremath{\\sigma}",         # σ sigma
  "\u03c7" = "\\ensuremath{\\chi}",           # χ chi

  # ── Typographic symbols ──
  "\u2020" = "\\textdagger{}",                # † dagger
  "\u2021" = "\\textdaggerdbl{}",             # ‡ double dagger
  "\u00a7" = "\\textsection{}",               # § section sign
  "\u00b6" = "\\textparagraph{}",             # ¶ pilcrow / paragraph
  "\u2022" = "\\textbullet{}",                # • bullet
  "\u2013" = "\\textendash{}",                # – en-dash
  "\u2014" = "\\textemdash{}",                # — em-dash
  "\u2018" = "`",                             # ' left single quote
  "\u2019" = "'",                             # ' right single quote
  "\u201c" = "``",                            # " left double quote
  "\u201d" = "''",                            # " right double quote
  "\u2026" = "\\ldots{}",                     # … horizontal ellipsis

  # ── Legal / trademark ──
  "\u00a9" = "\\textcopyright{}",             # © copyright
  "\u00ae" = "\\textregistered{}",            # ® registered
  "\u2122" = "\\texttrademark{}",             # ™ trademark

  # ── Arrows (shift tables, forest plots) ──
  "\u2190" = "\\ensuremath{\\leftarrow}",     # ← left arrow
  "\u2191" = "\\ensuremath{\\uparrow}",       # ↑ up arrow
  "\u2192" = "\\ensuremath{\\rightarrow}",    # → right arrow
  "\u2193" = "\\ensuremath{\\downarrow}",     # ↓ down arrow

  # ── Fractions & degree ──
  "\u00b0" = "\\textdegree{}",                # ° degree
  "\u00bc" = "\\textonequarter{}",            # ¼ one quarter
  "\u00bd" = "\\textonehalf{}",               # ½ one half
  "\u00be" = "\\textthreequarters{}"          # ¾ three quarters
)

#' Unicode → RTF control word map
#'
#' RTF handles Unicode via \\uN ? where N = decimal codepoint,
#' ? = ANSI fallback. Entries below use ANSI shorthand where available.
#' @noRd
fr_env$rtf_unicode <- c(
  "\u2020" = "\\'86",                         # † dagger
  "\u2021" = "\\'87",                         # ‡ double dagger
  "\u2022" = "\\'95",                         # • bullet
  "\u2013" = "\\'96",                         # – en-dash
  "\u2014" = "\\'97",                         # — em-dash
  "\u2018" = "\\'91",                         # ' left single quote
  "\u2019" = "\\'92",                         # ' right single quote
  "\u201c" = "\\'93",                         # " left double quote
  "\u201d" = "\\'94"                          # " right double quote
)


# ══════════════════════════════════════════════════════════════════════════════
# 5. Color Utilities
# ══════════════════════════════════════════════════════════════════════════════

fr_env$named_colors <- c(
  black   = "#000000",
  white   = "#FFFFFF",
  red     = "#FF0000",
  blue    = "#0000FF",
  green   = "#008000",
  gray    = "#808080",
  grey    = "#808080",
  silver  = "#C0C0C0",
  navy    = "#000080",
  maroon  = "#800000"
)


#' Parse hex color string to integer RGB triple
#' @noRd
hex_to_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3L) {
    hex <- paste0(substr(hex, 1, 1), substr(hex, 1, 1),
                  substr(hex, 2, 2), substr(hex, 2, 2),
                  substr(hex, 3, 3), substr(hex, 3, 3))
  }
  if (nchar(hex) != 6L) {
    cli_abort("Invalid hex color {.val {paste0('#', hex)}}. Expected 3 or 6 hex digits.")
  }
  c(
    r = strtoi(substr(hex, 1, 2), 16L),
    g = strtoi(substr(hex, 3, 4), 16L),
    b = strtoi(substr(hex, 5, 6), 16L)
  )
}


#' Format RGB as RTF color definition
#' @noRd
rgb_to_rtf_color <- function(r, g, b) {
  paste0("\\red", r, "\\green", g, "\\blue", b)
}


#' Format hex as LaTeX xcolor value
#' @noRd
hex_to_latex_color <- function(hex) {
  toupper(sub("^#", "", hex))
}


#' Resolve color: accepts hex strings or named colors
#' @noRd
resolve_color <- function(color, arg = caller_arg(color), call = caller_env()) {
  if (is.null(color) || is.na(color)) return(NULL)
  if (startsWith(color, "#")) return(toupper(color))
  named <- fr_env$named_colors[[tolower(color)]]
  if (is.null(named)) {
    cli_abort(c(
      "Unknown color name {.val {color}}.",
      "i" = "Use a hex string (e.g. {.val #003366}) or one of: {.val {names(fr_env$named_colors)}}."
    ), arg = arg, call = call)
  }
  named
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Token System for pagehead / pagefoot
#
# SYNTAX:  {thepage}  {total_pages}  {program}  {datetime}
#
# Same single-brace syntax as inline markup ({fr_super(1)}).
# Consistent with Python format(), Rust format!(), JS template literals.
#
# Why this works without collision:
#   - Inline markup uses {fr_*(...)} — always starts with "fr_"
#   - Tokens use {name} — never starts with "fr_"
#   - Literal brace: "{{" → "{" (standard glue escaping)
#
# Custom tokens via fr_page(tokens = list(study = "ABC-001")):
#   fr_pagehead(left = "{study}")
#
# ══════════════════════════════════════════════════════════════════════════════

fr_env$builtin_tokens <- c("thepage", "total_pages", "program", "datetime")


#' Resolve {token} placeholders in a string
#'
#' Scans for `{name}` patterns where name is NOT prefixed with "fr_"
#' (those are markup, not tokens). Replaces with values from token_map.
#' Literal "{{" → "{" and "}}" → "}" (standard escaping).
#'
#' @param text Character vector (supports multi-line headers).
#' @param token_map Named list of token_name → value.
#' @param context Character. For error messages.
#' @return Character vector with tokens resolved.
#' @noRd
resolve_tokens <- function(text, token_map, context = "page header/footer") {
  if (is.null(text) || !is.character(text)) return(text)
  vapply(text, function(txt) {
    resolve_tokens_single(txt, token_map, context)
  }, character(1), USE.NAMES = FALSE)
}


#' Resolve tokens in a single string
#' @noRd
resolve_tokens_single <- function(text, token_map, context) {
  # Protect escaped braces: {{ → sentinel, }} → sentinel
  text <- gsub("\\{\\{", "\x01LBRACE\x02", text, fixed = FALSE)
  text <- gsub("\\}\\}", "\x01RBRACE\x02", text, fixed = FALSE)

  # Find all {name} tokens that are NOT {fr_*(...)} markup
  # Token pattern: { word_chars } where word doesn't start with "fr_"
  pattern <- "\\{([A-Za-z_][A-Za-z0-9_]*)\\}"
  m <- gregexpr(pattern, text, perl = TRUE)
  tokens_found <- regmatches(text, m)[[1]]

  if (length(tokens_found) > 0L) {
    token_names <- sub("^\\{", "", sub("\\}$", "", tokens_found))

    # Skip any that look like markup (start with "fr_")
    is_markup <- startsWith(token_names, "fr_")

    for (i in seq_along(token_names)) {
      if (is_markup[[i]]) next
      nm <- token_names[[i]]
      val <- token_map[[nm]]
      if (is.null(val)) {
        cli_abort(c(
          "Unknown token {.val {{{nm}}}} in {context}.",
          "i" = "Available tokens: {.val {names(token_map)}}."
        ))
      }
      text <- sub(paste0("{", nm, "}"), as.character(val), text, fixed = TRUE)
    }
  }

  # Restore escaped braces
  text <- gsub("\x01LBRACE\x02", "{", text, fixed = TRUE)
  text <- gsub("\x01RBRACE\x02", "}", text, fixed = TRUE)
  text
}


# ══════════════════════════════════════════════════════════════════════════════
# 7. Alignment Map
# ══════════════════════════════════════════════════════════════════════════════

fr_env$valid_aligns <- c("left", "center", "right", "decimal")

fr_env$align_to_rtf <- c(
  left    = "\\ql",
  center  = "\\qc",
  right   = "\\qr",
  decimal = "\\ql"
)

fr_env$align_to_latex <- c(
  left    = "L",
  center  = "C",
  right   = "R",
  decimal = "L"
)

# Vertical alignment (cell-level property, orthogonal to horizontal align)
# RTF: \clvertalt (top, default), \clvertalc (center), \clvertalb (bottom)
# LaTeX: t (top), m (middle), b (bottom) — tabularray valign key
fr_env$valid_valigns <- c("top", "middle", "bottom")

fr_env$valign_to_rtf <- c(
  top    = "",
  middle = "\\clvertalc",
  bottom = "\\clvertalb"
)

fr_env$valign_to_latex <- c(
  top    = "t",
  middle = "m",
  bottom = "b"
)


# ══════════════════════════════════════════════════════════════════════════════
# 8. LaTeX Border Linestyle Map
# ══════════════════════════════════════════════════════════════════════════════

#' tabularray border linestyle names
#' @noRd
# ── LaTeX spacing constants ──────────────────────────────────────────────
fr_env$latex_leading_factor <- 1.08
fr_env$latex_rowsep <- "0.5pt"
fr_env$latex_colsep <- "2pt"

# ── RTF rendering constants ────────────────────────────────────────────────
fr_env$rtf_leading_factor    <- 1.4
fr_env$rtf_min_headery       <- 360L
fr_env$rtf_decimal_pad       <- 36L
fr_env$rtf_box_border_wd     <- 0.5
fr_env$rtf_spanner_brdrw     <- 10L

# ── LaTeX rendering constants (additional) ────────────────────────────────
fr_env$latex_space_width_em    <- 0.55
fr_env$latex_fn_sep_width_pt   <- 0.4
fr_env$latex_align_gap_width   <- 0.001  # near-zero inches; adjacent colsep provides visual break
fr_env$points_per_inch         <- 72

fr_env$linestyle_latex <- c(
  solid   = "solid",
  dashed  = "dashed",
  dotted  = "dotted",
  double  = "double",
  dashdot = "dashed"
)


# ── Grouped sub-list: rtf ──────────────────────────────────────────────────
#' RTF-specific constants for convenient grouped access.
#' @noRd
fr_env$rtf <- list(
  linestyle   = fr_env$linestyle_rtf,
  cell_border = fr_env$cell_border_rtf,
  para_border = fr_env$para_border_rtf,
  specials    = fr_env$rtf_specials,
  unicode     = fr_env$rtf_unicode
)

# ── Grouped sub-list: validation ────────────────────────────────────────────
#' Validation-related constants.
#' @noRd
fr_env$validation <- list(
  aligns     = fr_env$valid_aligns,
  valigns    = fr_env$valid_valigns,
  linestyles = fr_env$valid_linestyles
)
