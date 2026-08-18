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
#' @param x Optional Character: Column holding the categories.
#' @param y Optional Character: Columns holding the values, one series each.
#' @param stack Logical: If TRUE, stack the series instead of grouping them.
#' @param horizontal Logical: If TRUE, draw the bars horizontally.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams ChartConfig
#'
#' @return `BarConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_BarConfig(x = "cyl", y = "mpg")@type
BarConfig <- new_class(
  name = "BarConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
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
    # -- semantics ---------------------------------------------------------
    stack = prop_boolean(
      FALSE,
      description = "Stack the series instead of grouping them."
    ),
    horizontal = prop_boolean(
      FALSE,
      description = "Draw the bars horizontally."
    ),
    # -- appearance --------------------------------------------------------
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Series colors, overriding the theme palette. NULL uses the theme's."
    ),
    xlab = prop_string(
      NULL,
      nullable = TRUE,
      description = "X axis label. NULL derives it from the data."
    ),
    ylab = prop_string(
      NULL,
      nullable = TRUE,
      description = "Y axis label. NULL derives it from the data."
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
#' draw(setup_BarConfig(x = "cyl", y = "mpg"), data = aggregate(mpg ~ cyl, mtcars, mean))
setup_BarConfig <- function(
  x = NULL,
  y = NULL,
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
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), BAR_ORIGIN_NAMES)
  BarConfig(
    x = x,
    y = y,
    stack = stack,
    horizontal = horizontal,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
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
      xlab = if (!config@horizontal) config@x else bar_value_label(config),
      ylab = if (!config@horizontal) bar_value_label(config) else config@x
    )
  )
}


# %% bar_value_label ----
#' Label for a bar chart's value axis
#'
#' One bound column gives its name; several give none, since no single name
#' describes them and the series legend already names each.
#'
#' @param config [BarConfig]: The chart configuration.
#'
#' @return Character, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
bar_value_label <- function(config) {
  if (length(config@y) == 1L) config@y else NULL
} # /rtemis.draw::bar_value_label


# %% compile.BarConfig ----
# One bound column becomes a single series; several become a named list, one per
# column, which is the shape `bar_option()` already takes from `draw_bar()`.
method(compile, BarConfig) <- function(config, data = NULL, ...) {
  x <- config_column(data, config@x, "x")
  if (is.null(x) || is.null(config@y)) {
    abort(
      "A BarConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  values <- lapply(config@y, function(column) {
    config_column(data, column, "y")
  })
  names(values) <- config@y
  bar_option(
    x = x,
    y = if (length(values) == 1L) values[[1L]] else values,
    palette = config@palette,
    stack = config@stack,
    horizontal = config@horizontal,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
