# config_choropleth.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The choropleth map's config. Column-bound like scatter, but compiling to a
# MapLibre option rather than an ECharts one -- which is the point of `compile()`
# being a generic: the backend is a fact about the chart type, not about the
# config layer.
#
# There is no separate `MapConfig`. `draw_map()` takes an already-built
# `MapModel`, which is the rendering IR; the *chart type* is the choropleth, and
# building the model from bound columns is what its `compile()` does.

# %% ChoroplethConfig ----
#' Choropleth Map Configuration
#'
#' A serializable description of a choropleth map. Build one with
#' [setup_ChoroplethConfig()] rather than calling this constructor directly.
#'
#' @param location Optional Character: Column holding the region identifiers.
#' @param value Optional Character: Column holding the value to shade by.
#' @param tooltip Optional Character: Column holding per-region tooltip text.
#' @param resolution Character \{"country", "state", "county"\}: Which boundary
#'   set the locations refer to.
#' @param classification Character \{"quantile", "equal", "jenks"\}: How values
#'   are binned into classes.
#' @param num_classes Numeric `[2, 12]`: Number of color classes.
#' @param colormap Character: Continuous color scheme for the classes.
#' @param opacity Numeric `[0, 1]`: Fill opacity.
#' @param outline_width Numeric `[0, Inf)`: Boundary line width.
#' @param show_boundaries,show_legend Logical: Whether to draw each.
#' @param legend_position,tooltip_position,report_position Character: Corner
#'   each overlay is anchored to.
#' @param value_label Optional Character: Label for the value in the legend and
#'   tooltip. `NULL` derives it from the bound column.
#' @inheritParams ChartConfig
#'
#' @return `ChoroplethConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_ChoroplethConfig(location = "st", value = "v")@type
ChoroplethConfig <- new_class(
  name = "ChoroplethConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("choropleth"),
    # -- data binding ------------------------------------------------------
    location = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding the region identifiers."
    ),
    value = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding the value to shade by."
    ),
    tooltip = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding per-region tooltip text."
    ),
    # -- semantics ---------------------------------------------------------
    resolution = prop_string(
      "country",
      enum = c("country", "state", "county"),
      description = "Which boundary set the locations refer to."
    ),
    classification = prop_string(
      "quantile",
      enum = map_classifications,
      description = "How values are binned into classes."
    ),
    num_classes = prop_float(
      5,
      min = 2,
      max = 12,
      description = "Number of color classes."
    ),
    # -- appearance --------------------------------------------------------
    colormap = prop_string(
      "blues",
      enum = map_color_schemes,
      description = "Continuous color scheme for the classes."
    ),
    opacity = prop_float(
      1,
      min = 0,
      max = 1,
      description = "Fill opacity."
    ),
    show_boundaries = prop_boolean(
      TRUE,
      description = "Draw region boundaries."
    ),
    outline_width = prop_float(
      0.2,
      min = 0,
      description = "Boundary line width."
    ),
    show_legend = prop_boolean(TRUE, description = "Draw the legend."),
    legend_position = prop_string(
      "bottom-right",
      enum = map_corners,
      description = "Corner the legend is anchored to."
    ),
    tooltip_position = prop_string(
      "top-right",
      enum = map_corners,
      description = "Corner the tooltip is anchored to."
    ),
    report_position = prop_string(
      "bottom-left",
      enum = map_corners,
      description = "Corner the report panel is anchored to."
    ),
    value_label = prop_string(
      NULL,
      nullable = TRUE,
      description = "Label for the value. NULL derives it from the bound column."
    )
  )
) # /rtemis.draw::ChoroplethConfig


# %% CHOROPLETH_ORIGIN_NAMES ----
CHOROPLETH_ORIGIN_NAMES <- setdiff(
  names(ChoroplethConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_ChoroplethConfig ----
#' Set up a Choropleth Map Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams ChoroplethConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [ChoroplethConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' states <- data.frame(st = c("CA", "TX", "NY"), v = c(1, 2, 3))
#' draw(
#'   setup_ChoroplethConfig(location = "st", value = "v", resolution = "state"),
#'   data = states
#' )
setup_ChoroplethConfig <- function(
  location = NULL,
  value = NULL,
  tooltip = NULL,
  resolution = "country",
  classification = "quantile",
  num_classes = 5,
  colormap = "blues",
  opacity = 1,
  show_boundaries = TRUE,
  outline_width = 0.2,
  show_legend = TRUE,
  legend_position = "bottom-right",
  tooltip_position = "top-right",
  report_position = "bottom-left",
  value_label = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), CHOROPLETH_ORIGIN_NAMES)
  ChoroplethConfig(
    location = location,
    value = value,
    tooltip = tooltip,
    resolution = resolution,
    classification = classification,
    num_classes = num_classes,
    colormap = colormap,
    opacity = opacity,
    show_boundaries = show_boundaries,
    outline_width = outline_width,
    show_legend = show_legend,
    legend_position = legend_position,
    tooltip_position = tooltip_position,
    report_position = report_position,
    value_label = value_label,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_ChoroplethConfig


# %% resolve.ChoroplethConfig ----
# The value label is the name of the bound value column -- the same rule the
# cartesian charts use for axis labels.
method(resolve, ChoroplethConfig) <- function(config, data = NULL, ...) {
  config_derive(config, list(value_label = config@value))
}


# %% compile.ChoroplethConfig ----
# Builds the MapModel from the bound columns, then styles it -- the same two
# steps `draw_choropleth()` takes, through the same builder.
method(compile, ChoroplethConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  config <- resolve(config, data = dat)
  if (is.null(config@location) || is.null(config@value)) {
    abort(
      "A ChoroplethConfig needs both `location` and `value` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  model <- map_from_data_frame(
    data = dat,
    location = config@location,
    value = config@value,
    resolution = config@resolution,
    tooltip = config@tooltip,
    value_label = config@value_label
  )
  map_option(
    model = model,
    classification = config@classification,
    colormap = config@colormap,
    num_classes = config@num_classes,
    opacity = config@opacity,
    show_boundaries = config@show_boundaries,
    outline_width = config@outline_width,
    show_legend = config@show_legend,
    legend_position = config@legend_position,
    tooltip_position = config@tooltip_position,
    report_position = config@report_position,
    title = config@title
  )
}
