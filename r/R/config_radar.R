# config_radar.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The radar and funnel configs, drawn by the @rtemis/draw radar and funnel
# builders (spec: draw/shared-builders#radar, draw/shared-builders#funnel).

# %% RadarConfig ----
#' Radar Chart Configuration
#'
#' A serializable description of a radar chart. Build one with
#' [setup_RadarConfig()] rather than calling this constructor directly.
#'
#' @param indicators Optional Character: Numeric columns, one axis each,
#'   clockwise from the top; at least two.
#' @param group Optional Character: Column whose values name the series. `NULL`
#'   draws one series of every row.
#' @param aggregate Character \{"none", "sum", "mean", "median"\}: How the
#'   values of the rows of one series combine. A series is one polygon, so with
#'   `"none"` two rows in one series are an error. Missing values are left out
#'   of each summary.
#' @param area_opacity Numeric `[0, 1]`: Fill opacity of each polygon.
#' @param line_width Numeric `[0, Inf)`: Polygon line width in pixels.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette. `NULL` uses the theme's.
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `RadarConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_RadarConfig(indicators = c("mpg", "hp", "wt"), group = "cyl")@type
RadarConfig <- new_class(
  name = "RadarConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("radar"),
      # -- data binding ------------------------------------------------------
      indicators = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Numeric columns, one axis each, clockwise from the top."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column whose values name the series. Unset draws one series of every row."
      ),
      # -- semantics ---------------------------------------------------------
      aggregate = prop_string(
        "none",
        enum = c("none", "sum", "mean", "median"),
        description = paste(
          "How the values of the rows of one series combine: none requires",
          "one row per series."
        )
      ),
      # -- appearance --------------------------------------------------------
      area_opacity = prop_float(
        0.12,
        min = 0,
        max = 1,
        description = "Fill opacity of each polygon."
      ),
      line_width = prop_float(
        2,
        min = 0,
        description = "Polygon line width in pixels."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Series colors, overriding the theme palette. Unset uses the theme's."
      )
    )
  )
) # /rtemis.draw::RadarConfig


# %% RADAR_ORIGIN_NAMES ----
RADAR_ORIGIN_NAMES <- setdiff(
  names(RadarConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_RadarConfig ----
#' Set up a Radar Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams RadarConfig
#' @inheritParams setup_TreemapConfig
#'
#' @return [RadarConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cars <- aggregate(cbind(mpg, hp, wt) ~ cyl, data = mtcars, FUN = mean)
#' draw(
#'   setup_RadarConfig(indicators = c("mpg", "hp", "wt"), group = "cyl"),
#'   data = cars
#' )
setup_RadarConfig <- function(
  indicators = NULL,
  group = NULL,
  aggregate = "none",
  area_opacity = 0.12,
  line_width = 2,
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  origin <- origin %||% chart_origin(match.call(), RADAR_ORIGIN_NAMES)
  RadarConfig(
    indicators = indicators,
    group = group,
    aggregate = aggregate,
    area_opacity = area_opacity,
    line_width = line_width,
    palette = palette,
    title = title,
    dat_path = dat_path,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_RadarConfig


# %% resolve.RadarConfig ----
method(resolve, RadarConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.RadarConfig ----
# The request for the radar builder, which forms one polygon per group and
# checks the rows of each.
method(compile, RadarConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  if (length(config@indicators) < 2L) {
    abort(
      "A RadarConfig needs at least two `indicators` to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  for (indicator in config@indicators) {
    config_column(data, indicator, "indicators")
  }
  config_column(data, config@group, "group")
  BuildRequest(
    type = "radar",
    data = tabular_json(data, c(config@indicators, config@group)),
    config = config_members(config),
    appearance = list()
  )
}


# %% FunnelConfig ----
#' Funnel Chart Configuration
#'
#' A serializable description of a funnel chart. Build one with
#' [setup_FunnelConfig()] rather than calling this constructor directly.
#'
#' @param values Optional Character: Column holding the stage values.
#' @param labels Optional Character: Column holding the stage labels.
#' @param sort Character \{"descending", "ascending", "none"\}: Stage order: by
#'   value, largest or smallest first, or the order of the data.
#' @param show_labels Logical: Whether each stage carries its label.
#' @param label_format Character \{"name", "value", "name_value"\}: Stage label
#'   content.
#' @param palette Optional Character: Stage colors, in stage order, overriding
#'   the theme palette. `NULL` uses the theme's.
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `FunnelConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_FunnelConfig(values = "n", labels = "step")@type
FunnelConfig <- new_class(
  name = "FunnelConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("funnel"),
      # -- data binding ------------------------------------------------------
      values = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column holding the stage values."
      ),
      labels = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column holding the stage labels."
      ),
      # -- semantics ---------------------------------------------------------
      sort = prop_string(
        "descending",
        enum = c("descending", "ascending", "none"),
        description = "Stage order: by value, largest or smallest first, or the order of the data."
      ),
      # -- appearance --------------------------------------------------------
      show_labels = prop_boolean(
        TRUE,
        description = "Whether each stage carries its label."
      ),
      label_format = prop_string(
        "name",
        enum = c("name", "value", "name_value"),
        description = "Stage label content."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Stage colors, overriding the theme palette. Unset uses the theme's."
      )
    )
  )
) # /rtemis.draw::FunnelConfig


# %% FUNNEL_ORIGIN_NAMES ----
FUNNEL_ORIGIN_NAMES <- setdiff(
  names(FunnelConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_FunnelConfig ----
#' Set up a Funnel Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams FunnelConfig
#' @inheritParams setup_TreemapConfig
#'
#' @return [FunnelConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' steps <- data.frame(step = c("Visit", "Cart", "Buy"), n = c(100, 30, 10))
#' draw(setup_FunnelConfig(values = "n", labels = "step"), data = steps)
setup_FunnelConfig <- function(
  values = NULL,
  labels = NULL,
  sort = "descending",
  show_labels = TRUE,
  label_format = "name",
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  origin <- origin %||% chart_origin(match.call(), FUNNEL_ORIGIN_NAMES)
  FunnelConfig(
    values = values,
    labels = labels,
    sort = sort,
    show_labels = show_labels,
    label_format = label_format,
    palette = palette,
    title = title,
    dat_path = dat_path,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_FunnelConfig


# %% resolve.FunnelConfig ----
method(resolve, FunnelConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.FunnelConfig ----
# The request for the funnel builder, which sums the rows of each label and
# checks the values.
method(compile, FunnelConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  if (is.null(config@values) || is.null(config@labels)) {
    abort(
      "A FunnelConfig needs both `values` and `labels` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  config_column(data, config@values, "values")
  config_column(data, config@labels, "labels")
  BuildRequest(
    type = "funnel",
    data = tabular_json(data, unique(c(config@labels, config@values))),
    config = config_members(config),
    appearance = list()
  )
}
