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
#' colored by status (`ok`, `error`, `aborted`, `running`).
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
  events <- x@events
  if (length(events) == 0L) {
    abort(
      "This SupervisedSession has no recorded events to plot.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  started <- x@started

  # Elapsed milliseconds from session start.
  to_ms <- function(t) {
    if (is.null(t) || length(t) == 0L || is.na(t)) {
      return(NA_real_)
    }
    as.numeric(difftime(t, started, units = "secs")) * 1000
  }

  # End fallback for nodes that never closed (running / aborted, NA t_end):
  # the session finish time, else the latest known start.
  finished <- x@finished
  starts_ms <- vapply(events, function(e) to_ms(e[["t_start"]]), numeric(1L))
  end_fallback <- if (
    !is.null(finished) && length(finished) > 0L && !is.na(finished)
  ) {
    to_ms(finished)
  } else if (all(is.na(starts_ms))) {
    # Degenerate session (no finish time, no recorded starts): fall back to 0 so
    # we never propagate -Inf (and its warning) into the bar geometry.
    0
  } else {
    max(starts_ms, na.rm = TRUE)
  }

  # -- Rebuild the execution tree and order nodes depth-first ------------------
  # Mirrors the console repr in rtemis: index by node_id, collect children per
  # parent (preserving insertion order), then DFS from the roots.
  ids <- vapply(events, function(e) e[["node_id"]], character(1L))
  by_id <- stats::setNames(events, ids)
  parent_of <- vapply(
    events,
    function(e) {
      p <- e[["parent_id"]]
      if (is.null(p) || length(p) == 0L) NA_character_ else as.character(p)
    },
    character(1L)
  )
  children <- list()
  roots <- character(0L)
  for (i in seq_along(events)) {
    p <- parent_of[[i]]
    if (is.na(p) || is.null(by_id[[p]])) {
      roots <- c(roots, ids[[i]])
    } else {
      children[[p]] <- c(children[[p]], ids[[i]])
    }
  }
  ordered_ids <- character(0L)
  depths <- integer(0L)
  walk <- function(id, depth) {
    ordered_ids[[length(ordered_ids) + 1L]] <<- id
    depths[[length(depths) + 1L]] <<- depth
    for (k in children[[id]]) {
      walk(k, depth + 1L)
    }
  }
  for (r in roots) {
    walk(r, 0L)
  }

  fmt_dur <- function(ms) {
    if (is.na(ms)) {
      return("running")
    }
    if (ms < 0.5) {
      return("<0.5 ms")
    }
    if (ms < 1000) {
      return(sprintf("%.0f ms", ms))
    }
    sprintf("%.1f s", ms / 1000)
  }

  # -- Build the tasks frame in DFS order --------------------------------------
  n <- length(ordered_ids)
  label <- character(n)
  start <- numeric(n)
  end <- numeric(n)
  status <- character(n)
  kinds <- character(n)
  tip <- character(n)
  for (i in seq_len(n)) {
    e <- by_id[[ordered_ids[[i]]]]
    kind <- e[["kind"]]
    kinds[[i]] <- kind
    lbl <- e[["label"]]
    # Non-redundant name: append the label only when it adds to `kind`.
    nm <- if (!is.null(lbl) && nzchar(lbl) && !identical(lbl, kind)) {
      paste(kind, lbl)
    } else {
      kind
    }
    # Indent by depth to convey hierarchy on the category axis.
    label[[i]] <- paste0(strrep("  ", depths[[i]]), nm)
    st <- to_ms(e[["t_start"]])
    en <- to_ms(e[["t_end"]])
    if (is.na(st)) {
      st <- 0
    }
    if (is.na(en)) {
      en <- end_fallback
    }
    if (en < st) {
      en <- st
    }
    start[[i]] <- st
    end[[i]] <- en
    status[[i]] <- e[["status"]] %||% "ok"
    tip[[i]] <- paste0(
      trimws(nm),
      " \u2014 ",
      fmt_dur(en - st),
      " [",
      status[[i]],
      "]"
    )
  }

  # Duplicate display labels would collapse onto one category row; keep one row
  # per node so the timeline matches the execution graph one-to-one.
  label <- make.unique(label, sep = " #")

  tasks <- data.frame(
    label = label,
    start = start,
    end = end,
    kind = kinds,
    # Outline failed/aborted nodes (fill still encodes the kind, so a parallel
    # failed cell keeps its siblings' color -> the same-color = parallel reading
    # holds, while failures still pop via the red border).
    failed = status %in% c("error", "aborted"),
    tip = tip,
    stringsAsFactors = FALSE
  )

  # Color by event KIND so the legend filters by type and -- critically -- the
  # fill is identical for like events, making same-color overlap read as a
  # parallel process (only grid cells run concurrently; containers and
  # sequential steps never share a fill *and* a time span). Fixed map keeps each
  # kind's color stable across runs; draw_gantt() zips groups in first-seen (DFS)
  # order, so index the map by that order.
  kind_colors <- c(
    train = "#607D8B",
    outer_fold = "#8E63CE",
    tune = "#BE2E5F",
    grid_cell = "#466D96",
    train_alg = "#4F662A",
    predict = "#6CA3A0",
    varimp = "#F08904",
    metrics = "#FDB808"
  )
  present <- unique(tasks[["kind"]])
  # Fall back to the default palette for any unmapped kind.
  cols <- kind_colors[present]
  # rep_len recycles the palette so more unmapped kinds than palette colors still
  # get a (repeated) color rather than NA, which would break ECharts rendering.
  cols[is.na(cols)] <- rep_len(rtemis_colors, sum(is.na(cols)))

  draw_gantt(
    tasks,
    group = "kind",
    axis_type = "value",
    tooltip = "tip",
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
