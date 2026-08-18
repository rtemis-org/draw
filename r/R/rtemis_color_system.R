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
