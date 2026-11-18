# draw_survival.R
# ::rtemis.draw::

#' Validate and normalize precomputed survival records
#' @param config [SurvivalConfig]: Column bindings and layer settings.
#' @param data Data frame or list: Curves and optional explicit risk records.
#' @return List containing ordered curve frames and risk records.
#' @keywords internal
#' @noRd
survival_data <- new_generic("survival_data", "config")
method(survival_data, SurvivalConfig) <- function(config, data) {
  if (!is.list(data)) {
    abort(
      "Supply a curve data frame or a list with a `curves` data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  curves <- if (is.data.frame(data)) data else data[["curves"]]
  risk <- if (is.data.frame(data)) NULL else data[["risk"]]
  if (!is.data.frame(curves) || !nrow(curves)) {
    abort(
      "Supply a nonempty curve data frame, or a list with `curves` and optional `risk` frames.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  fields <- c(
    "time",
    "survival",
    "group",
    "lower",
    "upper",
    "n_censor",
    "n_risk"
  )
  d <- setNames(
    lapply(fields, function(nm) config_column(curves, prop(config, nm), nm)),
    fields
  )
  d[["group"]] <- d[["group"]] %||% rep("Survival", nrow(curves))
  if (
    !is.atomic(d[["group"]]) ||
      !is.null(dim(d[["group"]])) ||
      anyNA(d[["group"]]) ||
      any(!nzchar(as.character(d[["group"]])))
  ) {
    abort(
      "Supply a nonmissing, nonempty group label for every curve row.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  labels <- group_levels(d[["group"]])
  d[["group"]] <- as.character(d[["group"]])
  for (nm in setdiff(fields, "group")) {
    v <- d[[nm]]
    if (is.null(v)) {
      next
    }
    if (
      !is.numeric(v) ||
        is.complex(v) ||
        !is.null(dim(v)) ||
        length(v) != nrow(curves) ||
        any(!is.finite(v[!is.na(v)])) ||
        (nm %in% c("time", "survival") && anyNA(v))
    ) {
      abort(
        "Supply finite numeric `",
        nm,
        "` values; only optional fields may contain NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      (nm %in%
        c("survival", "lower", "upper") &&
        any(v < 0 | v > 1, na.rm = TRUE)) ||
        (nm %in% c("n_risk", "n_censor") && any(v < 0, na.rm = TRUE))
    ) {
      abort(
        "Keep survival probabilities and bounds in [0, 1], and counts nonnegative.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (
    !is.null(d[["lower"]]) &&
      (any(d[["lower"]] > d[["survival"]], na.rm = TRUE) ||
        any(d[["upper"]] < d[["survival"]], na.rm = TRUE))
  ) {
    abort(
      "Supply confidence bounds that enclose each survival estimate.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  d <- as.data.frame(d[!vapply(d, is.null, logical(1))])
  groups <- lapply(labels, function(label) {
    g <- d[d[["group"]] == label, , drop = FALSE]
    g <- g[order(g[["time"]]), , drop = FALSE]
    if (anyDuplicated(g[["time"]]) || any(diff(g[["survival"]]) > 1e-12)) {
      abort(
        "Use unique times and nonincreasing survival estimates within each group.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    g
  })
  if (config@risk_table) {
    if (
      !is.data.frame(risk) ||
        !nrow(risk) ||
        !all(c("time", "group", "n_risk") %in% names(risk))
    ) {
      abort(
        "Supply explicit `risk` records with time, group, and n_risk columns to draw a risk table.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      !is.numeric(risk[["time"]]) ||
        is.complex(risk[["time"]]) ||
        any(!is.finite(risk[["time"]])) ||
        !is.numeric(risk[["n_risk"]]) ||
        is.complex(risk[["n_risk"]]) ||
        any(!is.finite(risk[["n_risk"]][!is.na(risk[["n_risk"]])])) ||
        any(risk[["n_risk"]] < 0, na.rm = TRUE) ||
        anyNA(risk[["group"]]) ||
        !setequal(as.character(risk[["group"]]), labels) ||
        anyDuplicated(risk[c("group", "time")])
    ) {
      abort(
        "Supply unique group/time risk rows with finite times and nonnegative counts or NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    times <- sort(unique(risk[["time"]]))
    if (
      min(times) < min(d[["time"]]) ||
        max(times) > max(d[["time"]]) ||
        any(vapply(
          labels,
          function(g) sum(risk[["group"]] == g) != length(times),
          logical(1)
        ))
    ) {
      abort(
        "Use the same risk times for every group, within the chart's recorded time range.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(groups = groups, risk = risk)
}

#' Compute the median of a recorded right-continuous survival curve
#' @param x Data frame: Ordered time and survival columns.
#' @return Numeric median time, or NA if the median is not reached.
#' @keywords internal
#' @noRd
survival_median <- new_generic("survival_median", "x")
method(survival_median, class_data.frame) <- function(x) {
  s <- x[["survival"]]
  t <- x[["time"]]
  tol <- sqrt(.Machine$double.eps)
  hit <- which(s <= .5 + tol)
  if (!length(hit)) {
    return(NA_real_)
  }
  i <- hit[[1L]]
  if (abs(s[[i]] - .5) > tol) {
    return(t[[i]])
  }
  below <- which(seq_along(s) > i & s < .5 - tol)
  end <- if (length(below)) t[[below[[1L]]]] else tail(t, 1L)
  (t[[i]] + end) / 2
}

#' Build the request of a survival chart
#'
#' Validates and orders the curves (`survival_data()`), and hands each curve's
#' records and the risk counts to `@rtemis/draw`'s survival builder, which draws
#' the steps, bands, censor ticks, median droplines, landmarks, and the
#' number-at-risk strip.
#' spec: draw/shared-builders#survival
#' @inheritParams survival_data
#' @return [BuildRequest] of type `survival`.
#' @keywords internal
#' @noRd
survival_request <- new_generic("survival_request", "config")
method(survival_request, SurvivalConfig) <- function(config, data) {
  records <- survival_data(config, data)
  groups <- records[["groups"]]
  labels <- vapply(groups, function(g) g[["group"]][[1L]], character(1))
  values <- function(v) {
    if (is.null(v)) NULL else lapply(v, function(x) if (is.na(x)) NULL else x)
  }
  curves <- lapply(groups, function(g) {
    list(
      # An ungrouped chart's one curve is unnamed and draws no legend.
      name = if (is.null(config@group)) {
        NULL
      } else {
        jsonlite::unbox(g[["group"]][[1L]])
      },
      time = I(g[["time"]]),
      survival = I(g[["survival"]]),
      lower = values(g[["lower"]]),
      upper = values(g[["upper"]]),
      n_censor = values(g[["n_censor"]]),
      n_risk = values(g[["n_risk"]])
    )
  })
  document <- list(curves = curves)
  if (config@risk_table) {
    risk <- records[["risk"]]
    times <- sort(unique(risk[["time"]]))
    document[["risk"]] <- list(
      time = I(times),
      counts = lapply(labels, function(label) {
        r <- risk[as.character(risk[["group"]]) == label, , drop = FALSE]
        values(r[["n_risk"]][match(times, r[["time"]])])
      })
    )
  }
  BuildRequest(
    type = "survival",
    data = as.character(jsonlite::toJSON(
      document,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list()
  )
}


#' Draw Precomputed Survival Curves
#'
#' Use [draw_survfit()] to draw fitted R survival objects. This interface accepts
#' portable records and does not require the survival package.
#' @inheritSection SurvivalConfig Records and statistical semantics
#' @param data Data frame or list: Curve records, or `curves` and `risk` frames.
#' @param ... Additional named settings for [setup_SurvivalConfig()].
#' @param group,lower,upper,n_censor,n_risk Optional Character: Column bindings.
#'   When omitted, a column with the matching name is used if present.
#'   Explicit NULL disables that binding.
#' @inheritParams draw_roc
#' @return An ECharts htmlwidget with native SVG export.
#' @export
#' @examples
#' draw_survival(data.frame(time = c(0, 1, 3), survival = c(1, .8, .4)))
draw_survival <- function(
  data,
  ...,
  group = NULL,
  lower = NULL,
  upper = NULL,
  n_censor = NULL,
  n_risk = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  if (!is.list(data)) {
    abort(
      "Supply a curve data frame or a list with a `curves` data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  curves <- if (is.data.frame(data)) data else data[["curves"]]
  if (missing(group) && "group" %in% names(curves)) {
    group <- "group"
  }
  if (missing(lower) && "lower" %in% names(curves)) {
    lower <- "lower"
  }
  if (missing(upper) && "upper" %in% names(curves)) {
    upper <- "upper"
  }
  if (missing(n_censor) && "n_censor" %in% names(curves)) {
    n_censor <- "n_censor"
  }
  if (missing(n_risk) && "n_risk" %in% names(curves)) {
    n_risk <- "n_risk"
  }
  config <- setup_SurvivalConfig(
    group = group,
    lower = lower,
    upper = upper,
    n_censor = n_censor,
    n_risk = n_risk,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    ...
  )
  draw(
    config,
    data = data,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
