# rtemis_color_system.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The palette is defined in rtemis.core and re-exported here, so
# `rtemis.draw::rtemis_colors` and `rtemis.core::rtemis_colors` are one object:
# a second definition would mask core's for anyone loading both, and a
# positional lookup such as `rtemis_colors[[2L]]` would depend on which package
# resolved it.
#
# Index it by name (`rtemis_colors[["teal"]]`), never by position.

#' @importFrom rtemis.core rtemis_colors
#' @export
rtemis.core::rtemis_colors


# %% SIGN_COLORS ----
# The package default for a value's sign in every chart that colors by sign:
# significance plots, network edge weights, and the diverging and one-sided
# heatmap and spectrogram scales. Cool for negative, warm for positive, gray for
# neither. It reaches charts only through the theme: theme_light() and
# theme_dark() default to it, and theme_sign_colors() falls back to it, so the
# public way to read or replace the pair is a Theme.
#
# Both ends sit at OKLCH lightness 0.60 and chroma 0.147 (blue on the hue of
# `rtemis_colors[["blue"]]`, a red-shifted orange), so neither sign outweighs
# the other, both keep at least 3.9:1 contrast on light and dark backgrounds,
# and they stay apart from each other and from the gray under deuteranopia and
# protanopia. Remeasure all three, under both deficiencies, when changing
# either end.
SIGN_COLORS <- c(
  negative = "#2D83D3",
  neutral = "#808080",
  positive = "#C55E28"
)


# %% theme_sign_colors ----
#' Sign colors a theme supplies
#'
#' Charts that color by sign compute their colors before the widget exists, so
#' they read them from the theme here rather than in the browser. A [Theme]
#' supplies its own. `NULL` (light/dark auto-detection) reads the built-in
#' light theme: both built-in themes use the same pair, which keeps its
#' contrast on either background. `NA` and a deprecated ECharts theme list
#' carry none, and `SIGN_COLORS` apply.
#'
#' @param theme Optional [Theme], list, or `NA`: The theme, as passed to
#'   [draw()].
#'
#' @return Named character vector: `negative`, `neutral`, `positive`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
theme_sign_colors <- function(theme = NULL) {
  if (is.null(theme)) {
    theme <- theme_light()
  }
  if (!S7::S7_inherits(theme, Theme)) {
    return(SIGN_COLORS)
  }
  c(
    negative = theme@negative_color %||% SIGN_COLORS[["negative"]],
    neutral = theme@neutral_color %||% SIGN_COLORS[["neutral"]],
    positive = theme@positive_color %||% SIGN_COLORS[["positive"]]
  )
} # /rtemis.draw::theme_sign_colors


# %% theme_backgrounds ----
#' Backgrounds a color scale fades to
#'
#' Heatmap and spectrogram scales pin their neutral point to the chart
#' background, so a value of zero disappears into it. The browser chooses the
#' light or dark variant by the background it draws on; this supplies
#' the background each variant is built against. An explicit [Theme], or a
#' deprecated ECharts theme list with `backgroundColor`, gives its background
#' for both; otherwise they are the built-in light and dark themes'.
#'
#' @param theme Optional [Theme], list, or `NA`: The theme, as passed to
#'   [draw()].
#'
#' @return Named character vector: `light`, `dark`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
theme_backgrounds <- function(theme = NULL) {
  bg <- if (S7::S7_inherits(theme, Theme)) {
    theme@bg_color
  } else if (is.list(theme)) {
    theme[["backgroundColor"]]
  }
  if (is.character(bg) && length(bg) == 1L) {
    return(c(light = bg, dark = bg))
  }
  c(
    light = theme_light()@bg_color,
    dark = theme_dark()@bg_color
  )
} # /rtemis.draw::theme_backgrounds
