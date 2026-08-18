# plot_SupervisedSession.R
# Plot method for rtemis SupervisedSession: render the captured execution graph
# as a timeline / Gantt chart via draw_gantt().

# %% Temp get SupervisedSession ----
SupervisedSession <- utils::getFromNamespace("SupervisedSession", "rtemis")


# %% plot.SupervisedSession ----

#' Plot a SupervisedSession Execution Timeline
#'
#' Render the execution graph captured in a `SupervisedSession` (from rtemis
#' `train()`) as a timeline / Gantt chart: one bar per recorded step, ordered as
#' a depth-first walk of the execution tree, positioned by elapsed time and
#' colored by node kind (failed/aborted steps are outlined in red).
#'
#' The timeline table and the kind-color map come from
#' `rtemis::session_timeline()` and `rtemis::session_kind_colors()`, the shared
#' helpers also used by rtemis.server for the rtemislive web UI, so both
#' renderers stay in sync.
#'
#' @param x `rtemis::SupervisedSession`: Session object, e.g. `model@session`.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file
#'   via [save_drawing()].
#' @param ... Not used.
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
S7::method(plot, SupervisedSession) <- function(
  x,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL,
  ...
) {
  # One row per node, DFS order, ms offsets, unique indented labels, tooltip
  # text, and a `failed` flag -- all computed by the shared rtemis helper.
  tasks <- rtemis::session_timeline(x)

  # Color by event KIND so the legend filters by type and the fill is identical
  # for like events, making same-color overlap read as a parallel process.
  # draw_gantt() zips groups in first-seen (DFS) order, so index the map by
  # that order.
  cols <- rtemis::session_kind_colors(unique(tasks[["kind"]]))

  draw_gantt(
    tasks,
    group = "kind",
    axis_type = "value",
    tooltip = "tip",
    # Outline failed/aborted nodes (fill still encodes the kind, so a parallel
    # failed cell keeps its siblings' color -> the same-color = parallel
    # reading holds, while failures still pop via the red border).
    border = "failed",
    xlab = "Elapsed (ms)",
    title = title,
    color = unname(cols),
    theme = theme,
    width = width,
    height = height,
    filename = filename
  )
}


# %% plot_session
#'  Plot a Supervised object's session timeline
#'
#' Plots the session timeline of a Supervised object using [draw_gantt].
#'
#' @param x `Supervised` object.
#' @param ... Additional arguments passed to [draw_gantt].
#' @return htmlwidget: Widget object.
#' @author EDG
#' @export
plot_session <- new_generic("plot_session", "x")


# %% plot_session.Supervised
Supervised <- utils::getFromNamespace("Supervised", "rtemis")
method(plot_session, Supervised) <- function(x, ...) {
  plot(x@session, ...)
}
SupervisedRes <- utils::getFromNamespace("SupervisedRes", "rtemis")
method(plot_session, SupervisedRes) <- function(x, ...) {
  plot(x@session, ...)
}
