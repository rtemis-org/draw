# plot.SupervisedSession.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Plot methods for rtemis session objects: render the captured execution graph as
# a timeline / Gantt chart via draw_gantt().
#
# These methods belong in rtemis, not here -- they are glue between rtemis's
# result classes and this package's generic `draw_gantt()`. They live here
# temporarily because rtemis cannot depend on rtemis.draw until rtemis.draw is on
# CRAN, which is what the current release is for. Once it is, rtemis takes them
# (along with the conformal plots and the rest that will follow) and this file is
# deleted.
#
# Until then `rtemis` is a *Suggests*, so the dependency is soft: the classes
# these methods dispatch on (`SupervisedSession`, `Supervised`, `SupervisedRes`)
# exist only when rtemis is installed, and the methods are therefore registered
# at load time by `.register_rtemis_methods()` rather than at build time. See
# `R/zzz.R`.

# %% draw_supervised_session ----
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
#' Registered as the `plot()` method for `rtemis::SupervisedSession` when rtemis
#' is installed.
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
draw_supervised_session <- function(
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
    # failed cell keeps its siblings' color -> the same-palette = parallel
    # reading holds, while failures still pop via the red border).
    border = "failed",
    xlab = "Elapsed (ms)",
    title = title,
    palette = unname(cols),
    theme = theme,
    width = width,
    height = height,
    filename = filename
  )
} # /rtemis.draw::draw_supervised_session


# %% plot_session ----
#' Plot a Supervised object's session timeline
#'
#' Plots the session timeline of a `Supervised` or `SupervisedRes` object using
#' [draw_gantt].
#'
#' Requires the rtemis package: the classes this dispatches on are defined there,
#' so the methods are registered only when rtemis is installed.
#'
#' @param x `rtemis::Supervised` or `rtemis::SupervisedRes` object.
#' @param ... Additional arguments passed to [draw_gantt].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # GLM keeps the example dependency-free: rtemis fits it with stats::glm().
#' if (requireNamespace("rtemis", quietly = TRUE)) {
#'   mod <- rtemis::train(
#'     mtcars[, c("wt", "hp", "mpg")],
#'     hyperparameters = rtemis::setup_GLM(),
#'     verbosity = 0L
#'   )
#'   plot_session(mod)
#' }
plot_session <- new_generic("plot_session", "x")


# %% draw_object_session ----
#' Plot the session timeline held on a fitted rtemis object
#'
#' Shared implementation for the `plot_session()` methods on
#' `rtemis::Supervised` and `rtemis::SupervisedRes`: both hold their run's
#' session on `@session`.
#'
#' @param x `rtemis::Supervised` or `rtemis::SupervisedRes` object.
#' @param ... Additional arguments passed to [draw_gantt].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
draw_object_session <- function(x, ...) {
  plot(x@session, ...)
} # /rtemis.draw::draw_object_session


# %% .register_rtemis_methods ----
#' Register the rtemis-dependent S7 methods
#'
#' Called from `.onLoad()`. `rtemis` is a Suggests, so the classes these methods
#' dispatch on may not exist; registering at load time rather than at build time
#' is what lets rtemis.draw install, load and check without it.
#'
#' A no-op when rtemis is absent, so `plot_session()` is still exported and still
#' errors informatively (no method) rather than failing to exist.
#'
#' @return NULL, invisibly. Called for its side effect.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.register_rtemis_methods <- function() {
  if (!requireNamespace("rtemis", quietly = TRUE)) {
    return(invisible(NULL))
  }
  rtemis_class <- function(name) {
    utils::getFromNamespace(name, "rtemis")
  }
  S7::method(plot, rtemis_class("SupervisedSession")) <- draw_supervised_session
  S7::method(plot_session, rtemis_class("Supervised")) <- draw_object_session
  S7::method(plot_session, rtemis_class("SupervisedRes")) <- draw_object_session
  invisible(NULL)
} # /rtemis.draw::.register_rtemis_methods
