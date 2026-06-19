# draw_gantt.R
# Timeline / Gantt chart via an ECharts custom series.
#
# ECharts (>= 6.1) has no native Gantt series, so a timeline is drawn with a
# `custom` series: one rectangle per task, positioned on a value/time x-axis
# and a category y-axis. See the renderItem API in
# ~/Code/live/node_modules/echarts/types/dist/shared.d.ts
# (CustomSeriesRenderItemAPI: value(), coord(), size(), visual()).

# -- Internal helpers -----------------------------------------------------------

# Build the renderItem JS for a Gantt custom series.
#
# Each datum value is [rowIndex, start, end]:
#   - rowIndex maps to the category (y) axis position
#   - start / end map to the value or time (x) axis
# The bar height is a fraction of one category band; per-bar fill comes from the
# datum's itemStyle color via api.visual("color").
#
# @param bar_height Numeric (0, 1]: Bar thickness as a fraction of the band.
# @param bar_radius Numeric [0, Inf): Corner radius in pixels.
# @return htmlwidgets::JS object.
# @keywords internal
# @noRd
.gantt_render_item <- function(
  bar_height,
  bar_radius,
  border_color,
  border_width
) {
  # A truthy 4th data value (api.value(3)) outlines the bar in border_color
  # without changing its fill, so callers can flag bars (e.g. failures) while
  # the fill keeps encoding the group. Absent 4th value -> NaN -> no border.
  htmlwidgets::JS(sprintf(
    "function(params,api){
      var rowIndex=api.value(0);
      var start=api.coord([api.value(1),rowIndex]);
      var end=api.coord([api.value(2),rowIndex]);
      var height=api.size([0,1])[1]*%s;
      var width=end[0]-start[0];
      if(width<1){width=1;}
      var style={fill:api.visual('color')};
      if(api.value(3)){style.stroke='%s';style.lineWidth=%s;}
      return{
        type:'rect',
        transition:['shape'],
        shape:{x:start[0],y:start[1]-height/2,width:width,height:height,r:%s},
        style:style
      };
    }",
    format(bar_height, scientific = FALSE),
    border_color,
    format(border_width, scientific = FALSE),
    format(bar_radius, scientific = FALSE)
  ))
}


# Coerce a start/end column to the numeric form ECharts expects.
# POSIXct -> epoch milliseconds (for axis_type = "time"); numerics pass through.
#
# @param x Numeric or POSIXct vector.
# @return Numeric vector.
# @keywords internal
# @noRd
.gantt_time_values <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(as.numeric(x) * 1000)
  }
  if (inherits(x, "Date")) {
    return(as.numeric(as.POSIXct(x)) * 1000)
  }
  as.numeric(x)
}


# -- draw_gantt -----------------------------------------------------------------

#' Draw a Timeline / Gantt Chart
#'
#' Draw a timeline (Gantt) chart: one horizontal bar per task, positioned by
#' `start` and `end` on a value or time x-axis and grouped into rows by `label`
#' on the y-axis. Implemented as an ECharts `custom` series (ECharts has no
#' native Gantt series).
#'
#' @param tasks Tabular data (data.frame, data.table, or tibble): One row per
#'   task bar. Must contain columns `label` (row / category), `start`, and
#'   `end`. Repeated `label` values place multiple bars on the same row.
#' @param group Optional Character: Name of a column in `tasks` whose values
#'   color the bars and produce a legend. When `NULL`, all bars share one color.
#' @param axis_type Character \{"value", "time"\}: Type of the x-axis. Use
#'   `"value"` for numeric offsets (e.g. milliseconds from start) and `"time"`
#'   for absolute timestamps; `POSIXct`/`Date` `start`/`end` columns are
#'   converted to epoch milliseconds automatically.
#' @param bar_height Numeric `(0, 1]`: Bar thickness as a fraction of one
#'   category band.
#' @param bar_radius Numeric `[0, Inf)`: Bar corner radius in pixels.
#' @param guides Logical: If `TRUE`, show an interactive axis pointer -- a guide
#'   line that follows the mouse and labels the time value on the x-axis.
#' @param zoom Logical: If `TRUE`, enable interactive zoom -- mouse-wheel to zoom
#'   and drag to pan on both the time and row axes (`inside` dataZoom), plus a
#'   top-right toolbox with box-zoom, undo, and reset controls.
#' @param tooltip Optional Character: Name of a column in `tasks` to show as the
#'   tooltip text for each bar. When `NULL`, a default `label: start - end`
#'   tooltip is shown.
#' @param border Optional Character: Name of a logical column in `tasks`; bars
#'   whose value is `TRUE` get an outline (in `border_color`) without changing
#'   their fill -- e.g. to flag failures while the fill still encodes the group.
#' @param border_color Character: Outline color for bars flagged by `border`.
#' @param border_width Numeric `[0, Inf)`: Outline width in pixels.
#' @param xlab Optional Character: x-axis label.
#' @param title Optional Character: Chart title.
#' @param color Optional Character: Color palette as a single color or character
#'   vector overriding the theme palette. Groups are colored in order.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
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
#'   status = c("ok", "ok", "ok", "error")
#' )
#' draw_gantt(tasks, group = "status")
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
  color = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
) {
  rtemis.core::check_tabular(tasks)
  required_cols <- c("label", "start", "end")
  missing_cols <- setdiff(required_cols, names(tasks))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg tasks} must have columns {.val {required_cols}}; missing: {.val {missing_cols}}."
    )
  }
  if (!axis_type %in% c("value", "time")) {
    cli::cli_abort(
      "{.arg axis_type} must be one of {.val {c('value', 'time')}}, not {.val {axis_type}}."
    )
  }
  if (!is.null(group) && !group %in% names(tasks)) {
    cli::cli_abort(
      "{.arg group} column {.val {group}} not found in {.arg tasks}."
    )
  }
  if (!is.null(tooltip) && !tooltip %in% names(tasks)) {
    cli::cli_abort(
      "{.arg tooltip} column {.val {tooltip}} not found in {.arg tasks}."
    )
  }
  if (!is.null(border) && !border %in% names(tasks)) {
    cli::cli_abort(
      "{.arg border} column {.val {border}} not found in {.arg tasks}."
    )
  }

  # Category (y) axis: one row per unique label, preserving input order. Bars
  # reference their row by 0-based index (the value ECharts maps to a category).
  labels <- as.character(tasks[["label"]])
  categories <- unique(labels)
  row_index <- match(labels, categories) - 1L

  starts <- .gantt_time_values(tasks[["start"]])
  ends <- .gantt_time_values(tasks[["end"]])
  # NA start/end serialize as JSON null and break the custom-series renderer;
  # fail early with a corrective message instead.
  if (anyNA(starts) || anyNA(ends)) {
    cli::cli_abort(
      "Columns {.val start} and {.val end} in {.arg tasks} must not contain missing values ({.val NA})."
    )
  }

  # Optional per-bar border flag (e.g. to outline failures).
  border_flag <- if (!is.null(border)) as.logical(tasks[[border]]) else NULL

  # Per-bar tooltip text (precomputed; the formatter reads it from data.name).
  if (!is.null(tooltip)) {
    bar_text <- as.character(tasks[[tooltip]])
  } else {
    bar_text <- labels
  }

  # Resolve groups -> colors. With a grouping column we emit one custom series
  # per level so ECharts renders a toggleable legend (mirroring draw_scatter);
  # without one, a single series in the first palette color.
  palette <- color %||% rtemis_colors
  if (!is.null(group)) {
    group_vals <- as.character(tasks[[group]])
    levels_g <- unique(group_vals)
    group_colors <- unname(rep_len(palette, length(levels_g)))
  } else {
    group_vals <- rep_len("", length(labels))
    levels_g <- ""
    group_colors <- palette[[1L]]
  }

  render_item <- .gantt_render_item(
    bar_height,
    bar_radius,
    border_color,
    border_width
  )

  make_series <- function(level, col, name) {
    idx <- which(group_vals == level)
    data_items <- lapply(idx, function(i) {
      val <- if (!is.null(border_flag)) {
        # 4th value flags the border (1/0) for this bar.
        list(
          row_index[[i]],
          starts[[i]],
          ends[[i]],
          if (isTRUE(border_flag[[i]])) 1L else 0L
        )
      } else {
        list(row_index[[i]], starts[[i]], ends[[i]])
      }
      list(
        value = val,
        name = bar_text[[i]],
        itemStyle = list(color = col)
      )
    })
    series <- list(
      type = "custom",
      data = data_items,
      renderItem = render_item,
      encode = list(x = c(1L, 2L), y = 0L),
      clip = TRUE
    )
    if (!is.null(name)) {
      series[["name"]] <- name
    }
    series
  }

  if (!is.null(group)) {
    series <- lapply(seq_along(levels_g), function(i) {
      make_series(levels_g[[i]], group_colors[[i]], levels_g[[i]])
    })
  } else {
    series <- list(make_series("", group_colors[[1L]], NULL))
  }

  # Tooltip shows the bar's precomputed text (data.name).
  gantt_formatter <- htmlwidgets::JS(
    "function(p){var m=p.marker||'';return m+(p.name||'');}"
  )

  # Tight layout: a vertical legend on the right (so it doesn't waste vertical
  # space under the bars), with the grid reserving room for it. draw() injects
  # `outerBoundsMode = "same"`, so these margins are outer bounds and the long
  # category labels still fit inside them.
  has_legend <- !is.null(group)
  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title, left = "center") else NULL,
    tooltip = Tooltip(trigger = "item", formatter = gantt_formatter),
    legend = if (has_legend) {
      Legend(orient = "vertical", right = 8L, top = "middle")
    } else {
      NULL
    },
    # Discoverable zoom controls (top-right): box-zoom (drag a rectangle), undo,
    # and reset. Complements the wheel/drag `inside` zoom below. Theme-neutral
    # grey icons + a subtle selection brush read on both light and dark.
    toolbox = if (isTRUE(zoom)) {
      list(
        show = TRUE,
        right = 12L,
        top = 8L,
        itemSize = 14L,
        itemGap = 8L,
        showTitle = FALSE,
        iconStyle = list(borderColor = "#808080"),
        emphasis = list(iconStyle = list(borderColor = "#3F6FB5")),
        feature = list(
          dataZoom = list(
            filterMode = "none",
            brushStyle = list(
              color = "rgba(128,128,128,0.2)",
              borderColor = "rgba(128,128,128,0.6)",
              borderWidth = 1L
            ),
            title = list(zoom = "Box zoom", back = "Undo zoom")
          ),
          restore = list(title = "Reset")
        )
      )
    } else {
      NULL
    },
    color = palette,
    grid = Grid(
      left = 8L,
      right = if (has_legend) 96L else 16L,
      top = if (!is.null(title)) 40L else 12L,
      bottom = 8L
    ),
    x_axis = Axis(
      type = axis_type,
      name = xlab,
      name_location = if (!is.null(xlab)) "middle" else NULL,
      # `guides`: an interactive axisPointer -- a guide line that follows the
      # mouse and labels the time value on the x-axis. Static split lines are
      # left to the theme (consistent with the other draw_* functions).
      axis_pointer = if (isTRUE(guides)) {
        list(
          show = TRUE,
          type = "line",
          snap = FALSE,
          # Neutral grey label background (echarts defaults to a blue-grey).
          label = list(show = TRUE, backgroundColor = "#666666"),
          lineStyle = list(color = "#808080", width = 1L, type = "dashed")
        )
      } else {
        NULL
      }
    ),
    y_axis = Axis(
      type = "category",
      data = as.list(categories),
      inverse = TRUE,
      boundary_gap = TRUE
    ),
    # Zoom: one `inside` dataZoom per axis -> wheel-zoom + drag-pan on both time
    # and rows. filterMode = "none" is essential for the gantt: zooming the
    # category (row) axis must only reframe the view, never drop bars. Wheel-out
    # returns to the full view, so no slider/toolbox chrome is needed.
    data_zoom = if (isTRUE(zoom)) {
      list(
        list(
          type = "inside",
          xAxisIndex = 0L,
          filterMode = "none",
          zoomOnMouseWheel = TRUE,
          moveOnMouseMove = TRUE,
          moveOnMouseWheel = FALSE
        ),
        list(
          type = "inside",
          yAxisIndex = 0L,
          filterMode = "none",
          zoomOnMouseWheel = TRUE,
          moveOnMouseMove = TRUE,
          moveOnMouseWheel = FALSE
        )
      )
    } else {
      NULL
    },
    series = series
  )

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}
