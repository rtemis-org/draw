# config_bar.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The bar chart's config. The second chart type, and the one that establishes
# how a **multi-series** binding is declared: `y` names one column per series,
# where scatter's `x` and `y` each name exactly one.

# %% BarConfig ----
#' Bar Chart Configuration
#'
#' A serializable description of a bar chart. Build one with
#' [setup_BarConfig()] rather than calling this constructor directly.
#'
#' `x` names the column holding the categories. `y` names **one or more**
#' columns, one series each -- a single column draws one set of bars, several
#' draw a grouped or stacked chart with the column names as the series names.
#' That is the difference from [ScatterConfig], whose `x` and `y` each name
#' exactly one column.
#'
#' Fixed thickness maps to `barWidth` in ECharts' `BaseBarSeriesOption`
#' (`src/chart/bar/BaseBarSeries.ts`). It applies to either orientation.
#'
#' @param x Optional Character: Column holding the categories.
#' @param y Optional Character: Columns holding the values, one series each.
#'   `NULL` draws the number of rows of each category and group.
#' @param order Character `{"input", "increasing", "decreasing"}`: Order categories
#'   by their total across series, retaining ties and placing missing totals last.
#' @param stack Logical: If TRUE, stack the series instead of grouping them.
#' @param horizontal Logical: If TRUE, draw the bars horizontally.
#' @param zoom Logical: Enable a slider, Shift+wheel zoom, and drag panning on
#'   the category axis.
#' @param bar_width Optional Numeric `(0, Inf)`: Bar thickness in pixels.
#'   NULL lets ECharts size bars from the category spacing.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data, and `""` draws no label.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `BarConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_BarConfig(x = "quarter", y = "gwh", aggregate = "sum")@type
BarConfig <- new_class(
  name = "BarConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("bar"),
      # -- data binding ------------------------------------------------------
      x = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column holding the categories."
      ),
      y = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Columns holding the values, one series each."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Column whose values split each value column into one series per",
          "group, over the distinct categories."
        )
      ),
      aggregate = prop_string(
        "none",
        enum = c("none", "sum", "mean", "median"),
        description = paste(
          "How the y values of rows sharing a category and group combine",
          "into one bar: none draws one bar per row."
        )
      ),
      order = prop_string(
        "input",
        enum = c("input", "increasing", "decreasing"),
        description = "Category ordering by total value across series."
      ),
      # -- semantics ---------------------------------------------------------
      stack = prop_boolean(
        FALSE,
        description = "Stack the series instead of grouping them."
      ),
      horizontal = prop_boolean(
        FALSE,
        description = "Draw the bars horizontally."
      ),
      zoom = prop_boolean(
        FALSE,
        description = paste(
          "Enable a slider, Shift+wheel zoom, and drag panning on the",
          "category axis."
        )
      ),
      # -- appearance --------------------------------------------------------
      bar_width = prop_float(
        NULL,
        exclusive_min = 0,
        nullable = TRUE,
        description = "Bar thickness in pixels. Unset uses automatic category spacing."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Series colors, overriding the theme palette. Unset uses the theme's."
      ),
      ylim = prop_float(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        min_items = 2L,
        description = "Value axis limits, minimum and maximum. Unset fits the data."
      ),
      xlab = prop_string(
        NULL,
        min_length = 0L,
        nullable = TRUE,
        description = "X axis label. Unset derives it from the data, and an empty string draws no label."
      ),
      ylab = prop_string(
        NULL,
        min_length = 0L,
        nullable = TRUE,
        description = "Y axis label. Unset derives it from the data, and an empty string draws no label."
      ),
      margin_top = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Top margin in pixels."
      ),
      margin_right = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Right margin in pixels."
      ),
      margin_bottom = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Bottom margin in pixels."
      ),
      margin_left = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Left margin in pixels."
      )
    )
  ),
  validator = config_validator(
    list(
      list(
        schema = list(
          `if` = list(
            required = list("aggregate"),
            properties = list(
              aggregate = list(enum = list("sum", "mean", "median"))
            )
          ),
          then = list(
            properties = list(y = list(not = list(type = "null")))
          )
        ),
        message = paste(
          "aggregate combines the values of y: set y, or unset aggregate to",
          "count rows."
        )
      )
    )
  )
) # /rtemis.draw::BarConfig


# %% BAR_ORIGIN_NAMES ----
# The properties an origin map covers: every settable one.
BAR_ORIGIN_NAMES <- setdiff(
  names(BarConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_BarConfig ----
#' Set up a Bar Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams BarConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [BarConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Quarterly generation by source: `aggregate` sums the months of each
#' # quarter.
#' draw(
#'   setup_BarConfig(
#'     x = "quarter",
#'     y = "gwh",
#'     group = "source",
#'     aggregate = "sum",
#'     ylab = "Generation (GWh)"
#'   ),
#'   data = power_generation
#' )
#'
#' # With `y` unset, each bar counts the rows of its category and group.
#' draw(
#'   setup_BarConfig(x = "site", group = "satisfaction", stack = TRUE),
#'   data = satisfaction_survey
#' )
setup_BarConfig <- function(
  x = NULL,
  y = NULL,
  group = NULL,
  aggregate = "none",
  ylim = NULL,
  stack = FALSE,
  horizontal = FALSE,
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  bar_width = NULL,
  order = "input",
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  zoom = FALSE
) {
  origin <- origin %||% chart_origin(match.call(), BAR_ORIGIN_NAMES)
  BarConfig(
    x = x,
    y = y,
    group = group,
    aggregate = aggregate,
    ylim = if (is.null(ylim)) NULL else as.numeric(ylim),
    stack = stack,
    horizontal = horizontal,
    zoom = zoom,
    bar_width = bar_width,
    order = order,
    # A named color vector, such as a subset of `rtemis_colors`, is a palette.
    palette = if (!is.null(palette)) unname(palette),
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_BarConfig


# %% resolve.BarConfig ----
# Axis labels only: a bar chart's category axis is the data itself and its value
# axis is scaled by ECharts, so there are no limits to derive. The value-axis
# label comes from the single bound column, and is left unset for several --
# there is no one name to use, which is the same "no name, no label" rule.
method(resolve, BarConfig) <- function(config, data = NULL, ...) {
  config_derive(
    config,
    list(
      xlab = if (!config@horizontal) config@x else value_label(config),
      ylab = if (!config@horizontal) value_label(config) else config@x
    )
  )
}


# %% compile.BarConfig ----
# The option is built in JavaScript by @rtemis/draw, so compiling a bar config
# gives the request the widget and SVG export pass to `RtemisDraw.builders.bar`:
# the bound columns with any factor levels, and the config.
# spec: draw/shared-builders#bar
method(compile, BarConfig) <- function(config, data = NULL, theme = NULL, ...) {
  if (is.null(config@x)) {
    abort(
      "A BarConfig needs `x` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (length(config@ylim) > 0L && length(config@ylim) != 2L) {
    abort(
      "`ylim` takes two values, the minimum and the maximum.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  columns <- unique(c(config@x, config@y, config@group))
  for (name in columns) {
    config_column(data, name, if (name %in% config@y) "y" else "x")
  }
  # With `y` unset each bar counts rows; they are counted here, and the
  # builder sums the counts as row frequencies.
  frequency <- NULL
  if (is.null(config@y)) {
    frequency <- ".count"
    data <- aggregate_rows(
      data,
      c(config@x, config@group),
      character(0),
      count = frequency
    )
  } else if (config@aggregate != "none") {
    data <- aggregate_rows(
      data,
      c(config@x, config@group),
      config@y,
      config@aggregate
    )
  }
  BuildRequest(
    type = "bar",
    data = tabular_json(data, columns, frequency = frequency),
    config = config_members(config),
    appearance = list()
  )
}
