# zoom.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Zoom presets shared by the high-level ECharts draw_* functions.
# spec: draw/zoom-controls#design

# %% ZOOM_AXES ----
# Preset axes: "x" and "y" pair a slider with an inside zoom on that axis; "xy"
# pairs two inside zooms with the box-zoom toolbox.
ZOOM_AXES <- c("x", "y", "xy")


# %% resolve_zoom ----
#' Resolve the `zoom` argument of `draw_*` functions
#'
#' Translates the `zoom` argument into a list of [DataZoom] specs or `NULL`.
#' `FALSE` and `NULL` give no zoom. `TRUE` gives the preset for `axis`. A single
#' [DataZoom] is wrapped in a list; a list of [DataZoom] objects or plain named
#' lists passes through unchanged.
#'
#' Every preset zooms on Shift+wheel and pans on drag, so that the plain wheel
#' scrolls the host page.
#'
#' @param zoom Logical, [DataZoom], or list: User-facing `zoom` argument.
#' @param axis Character \{"x", "y", "xy"\}: Axis the `TRUE` preset targets.
#'   `"x"` and `"y"` give a slider plus an inside zoom on that axis; `"xy"`
#'   gives an inside zoom on each axis.
#' @param filter_mode Optional Character \{"filter", "weakFilter", "empty",
#'   "none"\}: Handling of data outside the window. `NULL` uses the ECharts
#'   default, `"filter"`.
#' @param axis_index Numeric or Character \{"all"\}: Index of the targeted
#'   axis.
#'
#' @return Optional list of [DataZoom] specs or plain lists: Value for
#'   `EChartsOption(data_zoom = ...)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_zoom <- function(zoom, axis = "x", filter_mode = NULL, axis_index = 0) {
  axis <- match.arg(axis, ZOOM_AXES)
  if (is.null(zoom) || isFALSE(zoom)) {
    return(NULL)
  }
  if (isTRUE(zoom)) {
    inside <- function(axis_args) {
      do.call(
        DataZoom,
        c(
          list(
            type = "inside",
            filter_mode = filter_mode,
            zoom_on_mouse_wheel = "shift",
            move_on_mouse_move = TRUE
          ),
          axis_args
        )
      )
    }
    x_args <- list(x_axis_index = axis_index)
    y_args <- list(y_axis_index = axis_index)
    if (axis == "xy") {
      return(list(inside(x_args), inside(y_args)))
    }
    axis_args <- if (axis == "y") y_args else x_args
    slider <- do.call(
      DataZoom,
      c(
        list(type = "slider", filter_mode = filter_mode, start = 0, end = 100),
        axis_args
      )
    )
    return(list(slider, inside(axis_args)))
  }
  if (S7::S7_inherits(zoom, DataZoom)) {
    return(list(zoom))
  }
  if (is.list(zoom)) {
    return(zoom)
  }
  abort(
    "`zoom` must be TRUE, FALSE, a DataZoom object, or a list; got ",
    class(zoom)[1],
    ".",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /rtemis.draw::resolve_zoom


# %% zoom_toolbox ----
#' Box-zoom, undo, and reset controls
#'
#' A toolbox in the top-right corner with the native box-zoom (drag a
#' rectangle), undo, and reset features. Gray icons and a translucent gray
#' selection brush read on light and dark backgrounds.
#'
#' Corresponds to `ToolboxDataZoomFeatureOption` in
#' `src/component/toolbox/feature/DataZoom.ts`.
#'
#' @param filter_mode Optional Character \{"filter", "weakFilter", "empty",
#'   "none"\}: Handling of data outside the box-zoom window. `NULL` uses the
#'   ECharts default, `"filter"`.
#'
#' @return Named list: Value for `EChartsOption(toolbox = ...)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
zoom_toolbox <- function(filter_mode = NULL) {
  data_zoom <- list(
    filterMode = filter_mode,
    brushStyle = list(
      color = "rgba(128,128,128,0.2)",
      borderColor = "rgba(128,128,128,0.6)",
      borderWidth = 1L
    ),
    title = list(zoom = "Box zoom", back = "Undo zoom")
  )
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
      dataZoom = data_zoom[!vapply(data_zoom, is.null, logical(1))],
      restore = list(title = "Reset")
    )
  )
} # /rtemis.draw::zoom_toolbox


# %% reserve_slider_room ----
#' Make room beside the plot for the zoom slider
#'
#' ECharts places a horizontal slider a fixed distance above the container's
#' bottom edge, where the x-axis title and the legend sit, and a vertical
#' slider at the right edge; it reserves no grid space for either. This places
#' a horizontal slider at the top of the bottom margin, above a bottom legend,
#' and a vertical slider at the inner edge of the right margin, then grows that
#' margin so the axis labels and title clear the slider.
#'
#' Only the preset built by `zoom = TRUE` is placed: a caller who passes
#' [DataZoom] objects controls the layout. A percentage margin is left
#' unchanged, since the slider size is in pixels.
#'
#' @param data_zoom List of [DataZoom]: The resolved zoom specs.
#' @param grid Optional [Grid]: The resolved margins.
#' @param axis Character \{"x", "y"\}: Axis the slider targets; `"x"` uses the
#'   bottom margin and `"y"` the right margin.
#'
#' @return Named list: `data_zoom` and `grid`, placed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
reserve_slider_room <- function(data_zoom, grid, axis = "x") {
  axis <- match.arg(axis, c("x", "y"))
  # ECharts' own numbers: the slider's filler size, the brush handle it draws
  # outside the filler, and its default grid margin when none is given.
  slider_size <- 30
  handle_size <- 7
  gap <- 8
  side <- if (axis == "x") "bottom" else "right"
  margin <- if (is.null(grid) || is.null(S7::prop(grid, side))) {
    if (axis == "x") 80 else 15
  } else {
    S7::prop(grid, side)
  }
  if (!is.numeric(margin)) {
    return(list(data_zoom = data_zoom, grid = grid))
  }
  data_zoom <- lapply(data_zoom, function(dz) {
    if (S7::S7_inherits(dz, DataZoom) && identical(dz@type, "slider")) {
      S7::prop(dz, side) <- margin + handle_size
      if (axis == "x") {
        dz@height <- slider_size
      } else {
        dz@width <- slider_size
      }
    }
    dz
  })
  grid <- grid %||% Grid()
  S7::prop(grid, side) <- margin + handle_size + slider_size + gap
  list(data_zoom = data_zoom, grid = grid)
} # /rtemis.draw::reserve_slider_room


# %% zoom_layout ----
#' Resolve zoom components, margins, and toolbox for a chart
#'
#' Combines `resolve_zoom()`, `reserve_slider_room()`, and `zoom_toolbox()`.
#' For `zoom = TRUE`, the `"x"` and `"y"` presets place their slider, and the
#' `"xy"` preset adds the box-zoom toolbox. Explicit [DataZoom] input leaves
#' the margins unchanged and adds no toolbox.
#'
#' @param zoom Logical, [DataZoom], or list: User-facing `zoom` argument.
#' @param axis Character \{"x", "y", "xy"\}: Axis the `TRUE` preset targets.
#' @param grid Optional [Grid]: The resolved margins.
#' @param filter_mode Optional Character \{"filter", "weakFilter", "empty",
#'   "none"\}: Handling of data outside the window.
#' @param axis_index Numeric or Character \{"all"\}: Index of the targeted
#'   axis.
#'
#' @return Named list: `data_zoom` (optional list), `grid` (optional [Grid]),
#'   and `toolbox` (optional named list).
#'
#' @author EDG
#' @keywords internal
#' @noRd
zoom_layout <- function(
  zoom,
  axis = "x",
  grid = NULL,
  filter_mode = NULL,
  axis_index = 0
) {
  axis <- match.arg(axis, ZOOM_AXES)
  data_zoom <- resolve_zoom(zoom, axis, filter_mode, axis_index)
  toolbox <- NULL
  if (isTRUE(zoom)) {
    if (axis == "xy") {
      toolbox <- zoom_toolbox(filter_mode)
    } else {
      placed <- reserve_slider_room(data_zoom, grid, axis)
      data_zoom <- placed[["data_zoom"]]
      grid <- placed[["grid"]]
    }
  }
  list(data_zoom = data_zoom, grid = grid, toolbox = toolbox)
} # /rtemis.draw::zoom_layout
