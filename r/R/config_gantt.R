# config_gantt.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The Gantt chart's config. Table-bound like Sankey: the bound data is a table
# of tasks, and the properties name the columns within it, defaulting to
# `label`, `start`, and `end`.

# %% GanttConfig ----
#' Gantt Chart Configuration
#'
#' A serializable description of a Gantt chart. Build one with
#' [setup_GanttConfig()] rather than calling this constructor directly.
#'
#' The bound data is a table of tasks, one row per bar. `label`, `start` and
#' `end` name its required columns; `group`, `tooltip` and `border` name
#' optional ones.
#'
#' @param label Character: Column naming each task.
#' @param start Character: Column holding each task's start.
#' @param end Character: Column holding each task's end.
#' @param group Optional Character: Column to color bars by.
#' @param tooltip Optional Character: Column holding per-bar tooltip text.
#' @param border Optional Character: Logical column; `TRUE` outlines the bar.
#' @param axis_type Character \{"value", "time"\}: How the time axis is scaled.
#' @param zoom Logical: Enable Shift+wheel zoom, drag panning, and box-zoom
#'   controls on both axes.
#' @param guides Logical: Show guide lines.
#' @param bar_height Numeric `[0, 1]`: Bar height as a fraction of the row.
#' @param bar_radius Numeric `[0, Inf)`: Bar corner radius in pixels.
#' @param border_color Character: Outline color for bordered bars.
#' @param border_width Numeric `[0, Inf)`: Outline width in pixels.
#' @param palette Optional Character: Bar colors, overriding the theme palette
#'   for this chart. `NULL` uses the theme's.
#' @param xlab Optional Character: Time axis label.
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `GanttConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_GanttConfig()@type
GanttConfig <- new_class(
  name = "GanttConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("gantt"),
      # -- data binding: columns of the bound task table ----------------------
      label = prop_string("label", description = "Column naming each task."),
      start = prop_string(
        "start",
        description = "Column holding each task's start."
      ),
      end = prop_string("end", description = "Column holding each task's end."),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column to color bars by."
      ),
      tooltip = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column holding per-bar tooltip text."
      ),
      border = prop_string(
        NULL,
        nullable = TRUE,
        description = "Logical column; a true value outlines the bar."
      ),
      # -- semantics ---------------------------------------------------------
      axis_type = prop_string(
        "value",
        enum = c("value", "time"),
        description = "How the time axis is scaled."
      ),
      zoom = prop_boolean(
        TRUE,
        description = paste(
          "Enable Shift+wheel zoom, drag panning, and box-zoom controls on",
          "both axes."
        )
      ),
      guides = prop_boolean(TRUE, description = "Show guide lines."),
      # -- appearance --------------------------------------------------------
      bar_height = prop_float(
        0.6,
        min = 0,
        max = 1,
        description = "Bar height as a fraction of the row."
      ),
      bar_radius = prop_float(
        0,
        min = 0,
        description = "Bar corner radius in pixels."
      ),
      border_color = prop_string(
        "#E53935",
        description = "Outline color for bordered bars."
      ),
      border_width = prop_float(
        1.5,
        min = 0,
        description = "Outline width in pixels."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Bar colors, overriding the theme palette. Unset uses the theme's."
      ),
      xlab = prop_string(
        NULL,
        nullable = TRUE,
        description = "Time axis label."
      )
    )
  )
) # /rtemis.draw::GanttConfig


# %% GANTT_ORIGIN_NAMES ----
GANTT_ORIGIN_NAMES <- setdiff(
  names(GanttConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_GanttConfig ----
#' Set up a Gantt Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams GanttConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [GanttConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' tasks <- data.frame(
#'   label = c("s1", "s2"),
#'   start = c(0, 5),
#'   end = c(5, 9),
#'   kind = c("a", "b")
#' )
#' draw(setup_GanttConfig(group = "kind"), data = tasks)
setup_GanttConfig <- function(
  label = "label",
  start = "start",
  end = "end",
  group = NULL,
  tooltip = NULL,
  border = NULL,
  axis_type = "value",
  zoom = TRUE,
  guides = TRUE,
  bar_height = 0.6,
  bar_radius = 0,
  border_color = "#E53935",
  border_width = 1.5,
  palette = NULL,
  xlab = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  origin <- origin %||% chart_origin(match.call(), GANTT_ORIGIN_NAMES)
  GanttConfig(
    label = label,
    start = start,
    end = end,
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
    dat_path = dat_path,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_GanttConfig


# %% resolve.GanttConfig ----
# Nothing to derive: the bindings are stated, and the time axis label has no
# single column to take a name from.
method(resolve, GanttConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.GanttConfig ----
# The option is built in JavaScript by @rtemis/draw, so compiling a Gantt
# config gives the request the widget and SVG export pass to
# `RtemisDraw.builders.gantt`: the bound task columns and the config.
# spec: draw/shared-builders#gantt
method(compile, GanttConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  roles <- c("label", "start", "end", "group", "tooltip", "border")
  columns <- character(0)
  for (role in roles) {
    column <- prop(config, role)
    if (!is.null(column)) {
      config_column(data, column, role)
      columns <- c(columns, column)
    }
  }
  # A missing time has no place on the axis.
  if (anyNA(data[[config@start]]) || anyNA(data[[config@end]])) {
    abort(
      "Columns '",
      config@start,
      "' and '",
      config@end,
      "' must not contain missing values (NA).",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  BuildRequest(
    type = "gantt",
    data = tabular_json(data, unique(columns)),
    config = config_members(config),
    appearance = list()
  )
}


# %% render_height.GanttConfig ----
# One row per distinct task label.
method(render_height, GanttConfig) <- function(config, data = NULL) {
  labels <- config_column(data, config@label, "label")
  if (is.null(labels)) NULL else gantt_height(length(unique(labels)))
}
