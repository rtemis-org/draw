# draw_survfit.R
# ::rtemis.draw::

#' Extract portable records from a scalar survival fit
#' @param x survfit: Fitted single-state survival curves.
#' @param risk_times Optional Numeric vector: Times for right-censored risk counts.
#' @return List with curves and optional risk records.
#' @keywords internal
#' @noRd
survfit_data <- new_generic("survfit_data", "x")
method(survfit_data, class_any) <- function(x, risk_times = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    abort(
      "Install the survival package to draw survfit objects.",
      class = c("rtemis_dependency_error", "rtemis_input_error")
    )
  }
  if (
    !inherits(x, "survfit") ||
      inherits(x, "survfitms") ||
      is.null(x[["surv"]]) ||
      !is.null(dim(x[["surv"]])) ||
      !length(x[["time"]]) ||
      (!is.null(x[["type"]]) && !x[["type"]] %in% c("right", "counting"))
  ) {
    abort(
      "Supply a single-state right-censored or counting-process survfit; select one predicted curve from matrix-valued fits first.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  # Let the estimator define t0, including conditional/start-time fits, rather
  # than always extending the curve to (0, 1).
  fit <- survival::survfit0(x)
  groups <- if (is.null(fit[["strata"]])) {
    rep("Survival", length(fit[["time"]]))
  } else {
    rep(names(fit[["strata"]]), fit[["strata"]])
  }
  curves <- data.frame(
    time = fit[["time"]],
    survival = fit[["surv"]],
    group = groups
  )
  fields <- c(
    lower = "lower",
    upper = "upper",
    n_censor = "n.censor",
    n_risk = "n.risk"
  )
  for (nm in names(fields)) {
    value <- fit[[fields[[nm]]]]
    if (!is.null(value)) curves[[nm]] <- as.numeric(value)
  }
  # survfit0 copies the next risk count into its synthetic origin. That value
  # is not an observed risk set for delayed entry, so keep it unavailable.
  if (!identical(x[["type"]], "right")) {
    original_groups <- if (is.null(x[["strata"]])) {
      rep("Survival", length(x[["time"]]))
    } else {
      rep(names(x[["strata"]]), x[["strata"]])
    }
    for (label in unique(groups)) {
      origin <- which(
        groups == label &
          !curves[["time"]] %in% x[["time"]][original_groups == label]
      )
      if (length(origin) && "n_risk" %in% names(curves)) {
        curves[["n_risk"]][origin] <- NA_real_
      }
    }
  }
  risk <- NULL
  if (!is.null(risk_times)) {
    if (!identical(x[["type"]], "right")) {
      abort(
        "For delayed-entry or model-predicted fits, supply explicit risk records to draw_survival(); do not infer between-time risk sets from the fit.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      !is.numeric(risk_times) ||
        is.complex(risk_times) ||
        !length(risk_times) ||
        any(!is.finite(risk_times)) ||
        min(risk_times) < min(curves[["time"]]) ||
        max(risk_times) > max(curves[["time"]])
    ) {
      abort(
        "Supply finite risk times within the recorded time range.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    # With no delayed entry, summary's next recorded risk set is valid between
    # events/censoring. Beyond a group's last follow-up its risk count is zero.
    at <- summary(x, times = sort(unique(risk_times)), extend = TRUE)
    risk <- data.frame(
      time = at[["time"]],
      n_risk = at[["n.risk"]],
      group = if (is.null(at[["strata"]])) {
        "Survival"
      } else {
        as.character(at[["strata"]])
      }
    )
  }
  list(curves = curves, risk = risk)
}

#' Draw a Fitted Survival Curve
#'
#' Adapt single-state `survival::survfit` estimates to [draw_survival()]. Stratum
#' labels, estimator-defined starting time, confidence bounds, and censor counts
#' are preserved. Matrix-valued predicted curves must be selected one at a time;
#' multistate and interval-censored fits require a different plotting contract.
#' @param x survfit: Fitted right-censored or counting-process survival curves.
#' @param risk_times Optional Numeric vector: Times for risk-table counts.
#'   Automatic counts require an ordinary right-censored fit. For delayed entry,
#'   pass separately computed risk records through [draw_survival()].
#' @param risk_table Logical: Draw risk counts. If requested without explicit
#'   times, uses approximately five evenly spaced times within the fit's range.
#' @param ... Additional named arguments for [draw_survival()].
#' @return An ECharts htmlwidget with native SVG export.
#' @export
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' draw_survfit(fit, risk_times = c(0, 250, 500, 750, 1000))
draw_survfit <- function(
  x,
  ...,
  risk_times = NULL,
  risk_table = !is.null(risk_times)
) {
  check_logical_scalar(risk_table)
  records <- survfit_data(x)
  if (risk_table) {
    times <- risk_times %||%
      seq(
        min(records[["curves"]][["time"]]),
        max(records[["curves"]][["time"]]),
        length.out = 5L
      )
    records <- survfit_data(x, times)
  }
  draw_survival(records, risk_table = risk_table, ...)
}
