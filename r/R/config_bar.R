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
#'   the data.
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
#' setup_BarConfig(x = "cyl", y = "mpg")@type
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
        enum = c("none", "sum", "mean", "median", "count"),
        description = paste(
          "How rows sharing a category and group combine into one bar:",
          "none draws one bar per row; count counts non-missing values."
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
        nullable = TRUE,
        description = "X axis label. Unset derives it from the data."
      ),
      ylab = prop_string(
        NULL,
        nullable = TRUE,
        description = "Y axis label. Unset derives it from the data."
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
    palette = palette,
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
# The option is built in JavaScript by @rtemis/draw, so compiling a bar config
# gives the request the widget and SVG export pass to `RtemisDraw.builders.bar`:
# the bound columns with any factor levels, and the config.
# spec: draw/shared-builders#bar
method(compile, BarConfig) <- function(config, data = NULL, theme = NULL, ...) {
  if (is.null(config@x) || is.null(config@y)) {
    abort(
      "A BarConfig needs both `x` and `y` set to draw.",
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
  if (config@aggregate != "none") {
    data <- bar_aggregate(
      data,
      c(config@x, config@group),
      config@y,
      config@aggregate
    )
  }
  BuildRequest(
    type = "bar",
    data = tabular_json(data, columns),
    config = config_members(config),
    appearance = list()
  )
}


# %% bar_aggregate ----
#' Combine the rows that share a category and group
#'
#' One row per distinct combination of the key columns, in order of first
#' appearance, with each value column summarized by `fn`. Missing values are
#' left out of each summary; `count` counts the non-missing values. Factor key
#' columns keep their levels.
#'
#' @param data Data frame: The chart data.
#' @param keys Character: Key columns, the category and the group.
#' @param values Character: Value columns.
#' @param fn Character \{"sum", "mean", "median", "count"\}: Summary.
#'
#' @return Data frame: The key and value columns, one row per combination.
#'
#' @author EDG
#' @keywords internal
#' @noRd
bar_aggregate <- function(data, keys, values, fn) {
  summary <- switch(
    fn,
    sum = function(v) sum(v, na.rm = TRUE),
    mean = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE),
    median = function(v) {
      if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE)
    },
    count = function(v) sum(!is.na(v))
  )
  key <- do.call(
    paste,
    c(lapply(keys, function(k) as.character(data[[k]])), sep = "\r")
  )
  first <- !duplicated(key)
  out <- lapply(keys, function(k) data[[k]][first])
  names(out) <- keys
  index <- match(key, key[first])
  for (v in values) {
    out[[v]] <- vapply(
      split(data[[v]], factor(index, levels = seq_len(sum(first)))),
      summary,
      numeric(1)
    )
  }
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
} # /rtemis.draw::bar_aggregate
