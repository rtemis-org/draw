# rtemis_color_system.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The palette is owned by rtemis.core. rtemis.draw used to define and export a
# second, different `rtemis_colors` -- 10 unnamed hues against core's 15 named
# ones -- which masked core's for anyone loading both, and made a positional
# lookup such as `rtemis_colors[[2L]]` mean a different color depending on which
# package resolved it. Re-exported here so `rtemis.draw::rtemis_colors` keeps
# working, with one definition behind it.
#
# Index it by name (`rtemis_colors[["teal"]]`), never by position.

#' @importFrom rtemis.core rtemis_colors
#' @export
rtemis.core::rtemis_colors


# %% SIGN_COLORS ----
# The meaning of a value's sign in every chart that colors by sign: significance
# plots, network edge weights, and the diverging and one-sided heatmap and
# spectrogram scales. Cool for negative, warm for positive, gray for neither.
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
