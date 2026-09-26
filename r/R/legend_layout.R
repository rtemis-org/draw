# Semantic legend controls shared by high-level functions and config schemas.
# Geometry is resolved from native ECharts LegendModel/LegendView measurements.

LEGEND_POSITIONS <- c(
  "top",
  "bottom",
  "left",
  "right",
  "top-left",
  "top-right",
  "bottom-left",
  "bottom-right"
)

#' Shared legend properties
#' @param position Character: Default anchor.
#' @param placement Character: Default relation to the plotting area.
#' @return Named list of validated S7 properties with schema metadata.
#' @keywords internal
#' @noRd
legend_properties <- function(position = "top", placement = "outside") {
  list(
    legend_position = prop_string(
      position,
      enum = LEGEND_POSITIONS,
      description = "Legend anchor: an edge or corner of the plotting area."
    ),
    legend_placement = prop_string(
      placement,
      enum = c("outside", "inside"),
      description = "Place the legend outside or inside the plotting area."
    )
  )
}

#' Validated semantic legend layout
#' @return S7 layout object. These settings are not native ECharts fields.
#' @keywords internal
#' @noRd
LegendLayout <- new_class(
  "LegendLayout",
  package = "rtemis.draw",
  properties = legend_properties()
)

#' Build renderer hints from shared legend settings
#' @param legend_position Character: Edge or corner anchor.
#' @param legend_placement Character: Outside or inside the plotting area.
#' @return Named list for the browser and SVG renderers.
#' @keywords internal
#' @noRd
legend_meta <- function(legend_position = "top", legend_placement = "outside") {
  layout <- LegendLayout(
    legend_position = legend_position,
    legend_placement = legend_placement
  )
  list(
    legendPosition = layout@legend_position,
    legendPlacement = layout@legend_placement
  )
}

#' Serialize a semantic legend layout
#' @param x LegendLayout: Validated layout.
#' @return Plain named list with portable configuration fields.
#' @keywords internal
#' @noRd
method(to_list, LegendLayout) <- function(x) {
  list(
    legend_position = x@legend_position,
    legend_placement = x@legend_placement
  )
}
