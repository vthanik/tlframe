# ──────────────────────────────────────────────────────────────────────────────
# api-theme.R — Study-level theme management
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — Study-level defaults (ggplot2 theme pattern)
# ══════════════════════════════════════════════════════════════════════════════

#' Set or Update Study-Level Table Theme
#'
#' @description
#'
#' `fr_theme()` sets study-level defaults that are automatically applied to
#' **every** subsequent `fr_table()` call. Define font size, page layout,
#' tokens, running header/footer, and line rules once at the top of your
#' program; all tables inherit these settings without repeating them.
#'
#' @aliases fr_theme_set
#'
#' @details
#' This follows the same pattern as `ggplot2` theme management. Calling
#' `fr_theme(...)` (or its alias `fr_theme_set(...)`) **merges** new settings
#' into the current global theme without discarding keys you did not mention.
#' This is analogous to `ggplot2::theme_update()`.
#'
#' Use [fr_theme_reset()] to clear all settings and return to built-in
#' defaults, or [fr_theme_get()] to inspect the current state.
#'
#' @param orientation `"landscape"` or `"portrait"`. `NULL` leaves unchanged.
#' @param paper `"letter"`, `"a4"`, or `"legal"`. `NULL` leaves unchanged.
#' @param font_family Font family name. `NULL` leaves unchanged.
#' @param font_size Font size in points. `NULL` leaves unchanged.
#' @param margins Margin(s) in inches (same formats as [fr_page()]). `NULL`
#'   leaves unchanged.
#' @param tokens Named list of `{token}` values. `NULL` leaves unchanged.
#' @param pagehead Named list with `left`, `center`, `right`, `font_size`,
#'   `bold` elements — same as [fr_pagehead()] arguments. `NULL` leaves
#'   unchanged.
#' @param pagefoot Named list with `left`, `center`, `right`, `font_size`,
#'   `bold` elements — same as [fr_pagefoot()] arguments. `NULL` leaves
#'   unchanged.
#' @param hlines Horizontal rule preset string (e.g. `"header"`). `NULL`
#'   leaves unchanged.
#' @param vlines Vertical rule preset string (e.g. `"box"`). `NULL` leaves
#'   unchanged.
#' @param spacing Named list with `titles_after`, `footnotes_before`,
#'   `pagehead_after`, `pagefoot_before`, `page_by_after` (integer
#'   blank lines). `NULL` leaves unchanged. See [fr_spacing()].
#' @param header Named list of header defaults. Currently supports:
#'   * `span_gap` — Logical. Insert gap columns between adjacent spans
#'     (default `TRUE`). See [fr_header()] for details.
#'   `NULL` leaves unchanged.
#' @param footnote_separator Logical. Whether to draw a separator above the
#'   footnote block. `NULL` leaves unchanged.
#'
#' @return Invisibly `NULL`.
#'
#' @section How the theme is applied:
#' When `fr_table()` is called, it automatically applies the stored theme to
#' the new `fr_spec`. You can still override any setting per-table by calling
#' the corresponding verb after `fr_table()`. Per-table overrides always win.
#'
#' Theme settings are stored in the package's internal environment and persist
#' for the duration of the R session. Call [fr_theme_reset()] to clear all
#' settings and [fr_theme_get()] to inspect the current theme.
#'
#' @examples
#' ## ── Define study theme at the top of your program ────────────────────────
#'
#' fr_theme(
#'   orientation = "landscape",
#'   paper       = "letter",
#'   font_size   = 9,
#'   tokens      = list(study = "TFRM-2024-001", cutoff = "31DEC2024"),
#'   pagehead    = list(
#'     left  = "Study: {study}",
#'     right = "Database Cutoff: {cutoff}"
#'   ),
#'   pagefoot    = list(
#'     left  = "{program}",
#'     right = "{datetime}"
#'   ),
#'   hlines = "header",
#'   vlines = "box"
#' )
#'
#' # All subsequent fr_table() calls inherit the theme — no need to repeat
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics", "Full Analysis Set")
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.2 Adverse Events", "Safety Analysis Set")
#'
#' ## ── Set section spacing (blank lines between table parts) ─────────────────
#'
#' fr_theme(spacing = list(titles_after = 1L, footnotes_before = 1L))
#'
#' ## ── Merge additional settings (does not discard existing ones) ────────────
#'
#' fr_theme(font_size = 9, hlines = "header")
#' fr_theme(vlines = "box")          # adds vlines; font_size and hlines kept
#' fr_theme_get()                    # inspect
#'
#' ## ── Override a theme setting for one specific table ───────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_page(orientation = "portrait")   # overrides the landscape theme default
#'
#' ## ── Reset all theme settings ──────────────────────────────────────────────
#'
#' fr_theme_reset()
#'
#' @seealso [fr_theme_get()] to inspect, [fr_theme_reset()] to clear,
#'   [fr_page()] for per-table page layout, [fr_pagehead()] and [fr_pagefoot()]
#'   for running headers and footers, [fr_spacing()] for per-table gap control.
#'
#' @export
fr_theme <- function(orientation = NULL, paper = NULL,
                     font_family = NULL, font_size = NULL,
                     margins = NULL, tokens = NULL,
                     pagehead = NULL, pagefoot = NULL,
                     hlines = NULL, vlines = NULL,
                     spacing = NULL,
                     header = NULL,
                     footnote_separator = NULL) {
  call <- caller_env()

  if (!is.null(font_size))          check_positive_num(font_size, arg = "font_size", call = call)
  if (!is.null(footnote_separator)) check_scalar_lgl(footnote_separator, arg = "footnote_separator", call = call)
  if (!is.null(header)) {
    if (!is.list(header)) cli_abort("{.arg header} must be a list.", call = call)
    if (!is.null(header$span_gap)) check_scalar_lgl(header$span_gap, arg = "header$span_gap", call = call)
  }
  if (!is.null(hlines)) {
    hlines <- match_arg_fr(hlines, names(fr_env$hline_presets), call = call)
  }
  if (!is.null(vlines)) {
    vlines <- match_arg_fr(vlines, c("box", "all", "inner", "void"), call = call)
  }

  theme <- fr_env$theme %||% list()

  if (!is.null(orientation))        theme[["orientation"]]        <- orientation
  if (!is.null(paper))              theme[["paper"]]              <- paper
  if (!is.null(font_family))        theme[["font_family"]]        <- font_family
  if (!is.null(font_size))          theme[["font_size"]]          <- font_size
  if (!is.null(margins))            theme[["margins"]]            <- margins
  if (!is.null(tokens))             theme[["tokens"]]             <- tokens
  if (!is.null(pagehead))           theme[["pagehead"]]           <- pagehead
  if (!is.null(pagefoot))           theme[["pagefoot"]]           <- pagefoot
  if (!is.null(hlines))             theme[["hlines"]]             <- hlines
  if (!is.null(vlines))             theme[["vlines"]]             <- vlines
  if (!is.null(spacing))            theme[["spacing"]]            <- spacing
  if (!is.null(header))             theme[["header"]]             <- header
  if (!is.null(footnote_separator)) theme[["footnote_separator"]] <- footnote_separator

  fr_env$theme <- theme
  invisible(NULL)
}

#' @export
fr_theme_set <- fr_theme


#' Get the Current Study-Level Table Theme
#'
#' @description
#'
#' Returns the current study-level theme as a named list. Use this to inspect
#' what defaults have been set via [fr_theme()].
#'
#' @return A named list of current theme settings, or an empty list if no
#'   theme has been set. Possible keys (all optional):
#'   * `orientation` — `"landscape"` or `"portrait"`
#'   * `paper` — `"letter"`, `"a4"`, `"legal"`
#'   * `font_family` — e.g. `"Courier New"`
#'   * `font_size` — numeric (points)
#'   * `margins` — numeric vector (inches)
#'   * `tokens` — named list of token values
#'   * `pagehead` — list with `left`, `center`, `right`, `font_size`, `bold`
#'   * `pagefoot` — list with `left`, `center`, `right`, `font_size`, `bold`
#'   * `hlines` — preset name (e.g. `"header"`)
#'   * `vlines` — preset name (e.g. `"box"`)
#'   * `spacing` — list with `titles_after`, `footnotes_before`,
#'     `pagehead_after`, `pagefoot_before`, `page_by_after` (integer
#'     blank lines)
#'   * `footnote_separator` — logical
#'
#' @examples
#' fr_theme(font_size = 9, hlines = "header", orientation = "landscape")
#' fr_theme_get()
#' # $font_size
#' # [1] 9
#' # $hlines
#' # [1] "header"
#' # $orientation
#' # [1] "landscape"
#'
#' # Programmatic access
#' theme <- fr_theme_get()
#' theme$font_size   # 9
#'
#' @seealso [fr_theme()] to set, [fr_theme_reset()] to clear,
#'   [fr_config_get()] for YAML config inspection.
#'
#' @export
fr_theme_get <- function() {
  as.list(fr_env$theme %||% list())
}


#' Reset the Study-Level Table Theme
#'
#' @description
#'
#' Clears all study-level defaults previously set by [fr_theme()]. After
#' calling `fr_theme_reset()`, [fr_table()] returns to its built-in
#' defaults (or YAML config defaults if [fr_config()] was loaded).
#'
#' Call this between test runs, at the top of scripts, or when switching
#' between studies in the same R session.
#'
#' @return Invisibly `NULL`.
#'
#' @examples
#' fr_theme(font_size = 9, hlines = "header")
#' fr_theme_reset()   # back to built-in defaults
#' fr_theme_get()     # returns list()
#'
#' @seealso [fr_theme()] to set, [fr_theme_get()] to inspect,
#'   [fr_config_reset()] to also clear YAML config.
#'
#' @export
fr_theme_reset <- function() {
  fr_env$theme <- list()
  invisible(NULL)
}
