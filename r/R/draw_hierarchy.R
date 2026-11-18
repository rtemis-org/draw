# draw_hierarchy.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Treemaps, sunbursts, radar charts, and funnels, drawn by the @rtemis/draw
# builders from a config (spec: draw/shared-builders#treemap,
# draw/shared-builders#sunburst, draw/shared-builders#radar,
# draw/shared-builders#funnel).

# %% draw_treemap ----
#' Draw a Treemap
#'
#' Treemap of a table's rows nested by level columns, drawn by the
#' `@rtemis/draw` treemap builder. Each row's path is its values of `levels`,
#' outermost first, ending at its first missing level; the rows ending at one
#' node sum their values, and a node that both ends a row and has children is
#' an error. Siblings follow the factor level order of their column, otherwise
#' their order of first appearance.
#'
#' @param x Data frame: The table.
#' @inheritParams TreemapConfig
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file
#'   via [save_drawing()].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' places <- data.frame(
#'   region = c("Europe", "Europe", "Europe", "Asia", "Asia"),
#'   country = c("France", "Spain", "Italy", "Japan", "India"),
#'   n = c(68, 48, 59, 124, 1428)
#' )
#' draw_treemap(places, levels = c("region", "country"), value = "n")
draw_treemap <- function(
  x,
  levels,
  value = NULL,
  show_labels = TRUE,
  border_width = 1,
  palette = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  rtemis.core::check_tabular(x)
  config <- setup_TreemapConfig(
    levels = levels,
    value = value,
    show_labels = show_labels,
    border_width = border_width,
    palette = palette,
    title = title
  )
  draw(
    config,
    data = x,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_treemap


# %% draw_sunburst ----
#' Draw a Sunburst Chart
#'
#' Sunburst of a table's rows nested by level columns, one ring per level,
#' drawn by the `@rtemis/draw` sunburst builder. The rows nest as in
#' [draw_treemap()].
#'
#' @inheritParams draw_treemap
#' @inheritParams SunburstConfig
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' places <- data.frame(
#'   region = c("Europe", "Europe", "Europe", "Asia", "Asia"),
#'   country = c("France", "Spain", "Italy", "Japan", "India"),
#'   n = c(68, 48, 59, 124, 1428)
#' )
#' draw_sunburst(places, levels = c("region", "country"), value = "n")
draw_sunburst <- function(
  x,
  levels,
  value = NULL,
  label_depth = 2L,
  min_angle = 0,
  label_rotate = "radial",
  radius = "85%",
  inner_radius = "18%",
  border_width = 1,
  palette = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  rtemis.core::check_tabular(x)
  config <- setup_SunburstConfig(
    levels = levels,
    value = value,
    label_depth = label_depth,
    min_angle = min_angle,
    label_rotate = label_rotate,
    radius = radius,
    inner_radius = inner_radius,
    border_width = border_width,
    palette = palette,
    title = title
  )
  draw(
    config,
    data = x,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_sunburst


# %% draw_radar ----
#' Draw a Radar Chart
#'
#' Radar chart of numeric columns, one axis per indicator and one polygon per
#' group, drawn by the `@rtemis/draw` radar builder. Each axis runs from 0, or
#' from its smallest value when negative, to its largest value.
#'
#' @param x Data frame: The table.
#' @inheritParams RadarConfig
#' @inheritParams draw_treemap
#' @inheritParams draw_line legend_position legend_placement legend_title
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cars <- mtcars
#' cars[["cyl"]] <- factor(cars[["cyl"]])
#' draw_radar(
#'   cars,
#'   indicators = c("mpg", "hp", "wt", "qsec"),
#'   group = "cyl",
#'   aggregate = "mean"
#' )
draw_radar <- function(
  x,
  indicators,
  group = NULL,
  aggregate = "none",
  area_opacity = 0.12,
  line_width = 2,
  palette = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  rtemis.core::check_tabular(x)
  config <- setup_RadarConfig(
    indicators = indicators,
    group = group,
    aggregate = aggregate,
    area_opacity = area_opacity,
    line_width = line_width,
    palette = palette,
    title = title,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    config,
    data = x,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_radar


# %% draw_funnel ----
#' Draw a Funnel Chart
#'
#' Funnel chart from values and labels, drawn by the `@rtemis/draw` funnel
#' builder. Values that share a label sum into one stage.
#'
#' @param values Numeric or data frame: Stage values, or a two-column table
#'   containing labels followed by values. Values must be finite and nonnegative
#'   with a positive total.
#' @param labels Optional Character: Stage labels; `names(values)` when omitted.
#' @inheritParams FunnelConfig
#' @inheritParams draw_treemap
#' @inheritParams draw_line legend_position legend_placement legend_title
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw_funnel(
#'   values = c(1000, 420, 180, 64),
#'   labels = c("Visit", "Sign up", "Trial", "Purchase")
#' )
draw_funnel <- function(
  values,
  labels = NULL,
  sort = "descending",
  show_labels = TRUE,
  label_format = "name",
  palette = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  input <- pie_input(values, labels)
  config <- setup_FunnelConfig(
    values = ".value",
    labels = ".label",
    sort = sort,
    show_labels = show_labels,
    label_format = label_format,
    palette = palette,
    title = title,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    config,
    data = data.frame(
      .label = input[["labels"]],
      .value = input[["values"]],
      check.names = FALSE
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_funnel
