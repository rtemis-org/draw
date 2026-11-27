# draw_gantt.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% gantt_height ----
#' The height of a Gantt chart from its number of rows
#'
#' 20 pixels per task row and 96 pixels for the legend, toolbox, and time axis,
#' at least 220: `ganttHeight()` in @rtemis/draw, which rtemislive's timeline
#' uses.
#'
#' @param rows Integer: Number of task rows.
#'
#' @return Numeric: Height in pixels.
#'
#' @author EDG
#' @keywords internal
#' @noRd
gantt_height <- function(rows) {
  max(rows * 20 + 96, 220)
} # /rtemis.draw::gantt_height


#' Draw a Timeline / Gantt Chart
#'
#' Draw a timeline (Gantt) chart: one horizontal bar per task, positioned by
#' `start` and `end` on a value or time x-axis and grouped into rows by `label`
#' on the y-axis. The option is built by @rtemis/draw's Gantt builder, which
#' rtemislive's execution timeline also uses, and draws each bar with an
#' ECharts custom series.
#'
#' The legend uses the shared position and placement controls. Tick density
#' follows the available plotting width, and long task labels are truncated;
#' the tooltip shows the full text. Browser resizing, panel figures, and SVG
#' export share this layout. With `height` unset, the chart is 20 pixels per
#' task row plus 96 pixels, at least 220.
#'
#' @param tasks Tabular data (data.frame, data.table, or tibble): One row per
#'   task bar. Must contain columns `label` (row / category), `start`, and
#'   `end`. Repeated `label` values place multiple bars on the same row.
#' @param group Optional Character: Name of a column in `tasks` whose values
#'   color the bars and produce a legend, ordered by factor level, or by first
#'   appearance for other columns. When `NULL`, all bars share one color.
#' @param axis_type Character \{"value", "time"\}: Type of the x-axis. Use
#'   `"value"` for numeric offsets (e.g. milliseconds from start) and `"time"`
#'   for absolute timestamps; `POSIXct` and `Date` `start` and `end` columns
#'   are placed and labeled in UTC.
#' @param bar_height Numeric `(0, 1]`: Bar thickness as a fraction of one
#'   category band.
#' @param bar_radius Numeric `[0, Inf)`: Bar corner radius in pixels.
#' @param guides Logical: If `TRUE`, show an interactive axis pointer -- a guide
#'   line that follows the mouse and labels the time value on the x-axis.
#' @param zoom Logical: If `TRUE`, enable interactive zoom -- Shift+wheel to
#'   zoom and drag to pan on both the time and row axes (`inside` dataZoom),
#'   plus a top-right toolbox with box-zoom, undo, and reset controls.
#' @param tooltip Optional Character: Name of a column in `tasks` to show as the
#'   tooltip text for each bar. When `NULL`, the tooltip shows the task's
#'   label.
#' @param border Optional Character: Name of a logical column in `tasks`; bars
#'   whose value is `TRUE` get an outline (in `border_color`) without changing
#'   their fill -- e.g. to flag failures while the fill still encodes the group.
#' @param border_color Character: Outline color for bars flagged by `border`.
#' @param border_width Numeric `[0, Inf)`: Outline width in pixels.
#' @param xlab Optional Character: x-axis label.
#' @param title Optional Character: Chart title.
#' @param palette Optional Character: Color palette as a single color or character
#'   vector overriding the theme palette. Groups are colored in order.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height. `NULL` sizes
#'   the chart from its number of task rows.
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
#' tasks <- data.frame(
#'   label = c("load", "clean", "train", "predict"),
#'   start = c(0, 12, 30, 95),
#'   end = c(12, 30, 95, 100),
#'   status = c("completed", "completed", "completed", "failed")
#' )
#' draw_gantt(tasks, group = "status")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_gantt <- function(
  tasks,
  group = NULL,
  axis_type = "value",
  bar_height = 0.6,
  bar_radius = 0,
  guides = TRUE,
  zoom = TRUE,
  tooltip = NULL,
  border = NULL,
  border_color = "#E53935",
  border_width = 1.5,
  xlab = NULL,
  title = NULL,
  palette = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  rtemis.core::check_tabular(tasks)
  config <- setup_GanttConfig(
    group = group,
    tooltip = tooltip,
    border = border,
    axis_type = axis_type,
    zoom = zoom,
    guides = guides,
    bar_height = bar_height,
    bar_radius = bar_radius,
    border_color = border_color,
    border_width = border_width,
    palette = palette,
    xlab = xlab,
    title = title,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    config,
    data = tasks,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_gantt
