# fit_model.R
# Named fits: GLM, polynomial, loess, and GAM are fitted here, every other
# algorithm by rtemis.
# spec: draw/learner-fits

# The first rtemis release exporting fit_predict().
RTEMIS_FIT_VERSION <- "1.4.1"

#' Fit a named model and predict new data
#'
#' The single fitting step behind `fit =` in scatter, line, fit, and 3D
#' scatter drawings. `"glm"` uses [stats::glm()]; `"polynomial"` a GLM of a
#' raw polynomial of the features, of degree `degree` (default 3); `"loess"`
#' [stats::loess()] with span `span` (default 0.75); and `"gam"` [mgcv::gam()]
#' with a smooth term per feature. The names match in any case. Any other name
#' is an rtemis supervised learning algorithm, trained by
#' `rtemis::fit_predict()`.
#'
#' `params` are passed to the learner: to [stats::glm()] for GLM and
#' polynomial, except `degree`; to [stats::loess()] for loess, `span`
#' included; to [mgcv::gam()] for GAM, except `k`, which sets the basis
#' dimension of every smooth term, and `engine`, which names the GAM
#' implementation and must be `"mgcv"`; and to the algorithm's
#' `rtemis::setup_*()` function otherwise.
#'
#' @param x data.frame: Features, one column per predictor.
#' @param y Numeric: Outcome, one value per row of `x`.
#' @param newdata data.frame: Features to predict, with the columns of `x`.
#' @param fit Character: Model name.
#' @param se Logical: Whether standard errors are wanted.
#' @param params Optional Named list: Learner arguments.
#'
#' @return Named list: `fitted` (Numeric predictions for `newdata`, on the
#'   response scale), `se` (Numeric standard errors, or NULL when not wanted or
#'   unavailable), and `rsq` (Numeric training R-squared, NA for a constant
#'   outcome).
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_model_values <- function(x, y, newdata, fit, se, params = NULL) {
  method <- tolower(fit)
  if (!method %in% FIT_MODELS) {
    out <- rtemis_fit_predict(fit)(
      x,
      y,
      newdata,
      algorithm = fit,
      params = params,
      se = se,
      verbosity = 0L
    )
    return(out[c("fitted", "se", "rsq")])
  }
  reserved <- intersect(names(params), c("formula", "data"))
  if (length(reserved) > 0L) {
    abort(
      "Remove ",
      paste0("`", reserved, "`", collapse = ", "),
      " from fit_params: draw supplies the model formula and data.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  engine <- params[["engine"]]
  if (method == "gam" && !is.null(engine) && !identical(engine, "mgcv")) {
    abort(
      "fit_params engine = \"",
      engine,
      "\" is fitted by rtemislive; rtemis.draw fits gam with mgcv. ",
      "Remove engine from fit_params, or set it to \"mgcv\".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  data <- x
  data[[".outcome"]] <- y
  terms <- names(x)
  learner <- switch(
    method,
    gam = "mgcv::gam",
    loess = "stats::loess",
    "stats::glm"
  )
  model <- tryCatch(
    switch(
      method,
      gam = {
        check_dependencies("mgcv")
        # `k` belongs to each smooth term, not to gam() itself.
        k <- params[["k"]]
        if (!is.null(k)) {
          k <- clean_int(k)
        }
        smooth <- paste0("s(", terms, if (!is.null(k)) paste0(", k = ", k), ")")
        do.call(
          mgcv::gam,
          c(
            list(
              stats::reformulate(smooth, response = ".outcome"),
              data = data
            ),
            params[setdiff(names(params), c("k", "engine"))]
          )
        )
      },
      polynomial = {
        degree <- clean_int(params[["degree"]] %||% 3L)
        term <- paste0(
          "poly(",
          paste(terms, collapse = ", "),
          ", degree = ",
          degree,
          ", raw = TRUE)"
        )
        do.call(
          stats::glm,
          c(
            list(stats::reformulate(term, response = ".outcome"), data = data),
            params[setdiff(names(params), "degree")]
          )
        )
      },
      loess = {
        do.call(
          stats::loess,
          c(
            list(
              stats::reformulate(terms, response = ".outcome"),
              data = data,
              span = params[["span"]] %||% 0.75
            ),
            params[setdiff(names(params), "span")]
          )
        )
      },
      do.call(
        stats::glm,
        c(
          list(stats::reformulate(terms, response = ".outcome"), data = data),
          params
        )
      )
    ),
    rtemis_error = function(e) stop(e),
    error = function(e) {
      abort(
        learner,
        "() failed: ",
        sub("[.]$", "", conditionMessage(e)),
        ". Check fit_params against ?",
        learner,
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  )
  pred <- if (method == "loess") {
    stats::predict(model, newdata = newdata, se = TRUE)
  } else {
    stats::predict(
      model,
      newdata = newdata,
      type = "response",
      se.fit = TRUE
    )
  }
  residual <- if (method == "loess") {
    stats::residuals(model)
  } else {
    stats::residuals(model, type = "response")
  }
  total <- sum((y - mean(y))^2)
  list(
    fitted = unname(as.numeric(pred[["fit"]])),
    se = if (se) unname(as.numeric(pred[["se.fit"]])),
    rsq = if (total > 0) 1 - sum(residual^2) / total else NA_real_
  )
} # /rtemis.draw::fit_model_values


# %% FIT_MODELS ----
# The fit names rtemis.draw computes itself; any other is an rtemis learner.
FIT_MODELS <- c("glm", "polynomial", "loess", "gam")


#' Find rtemis's fitting bridge
#'
#' rtemis is optional: only fits other than GLM and GAM need it.
#'
#' @param fit Character: Model name, for the error message.
#'
#' @return Function: `rtemis::fit_predict()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rtemis_fit_predict <- function(fit) {
  if (!rtemis_fit_available()) {
    abort(
      "fit = \"",
      fit,
      "\" is fitted by rtemis (>= ",
      RTEMIS_FIT_VERSION,
      "): install or update rtemis, or use fit = \"glm\", \"polynomial\", \"loess\", or \"gam\".",
      class = c("rtemis_dependency_error", "rtemis_input_error")
    )
  }
  # Looked up at run time: a static rtemis::fit_predict reference fails
  # R CMD check against rtemis releases older than RTEMIS_FIT_VERSION.
  getExportedValue("rtemis", "fit_predict")
} # /rtemis.draw::rtemis_fit_predict


#' Whether an rtemis release with the fitting bridge is installed
#'
#' Checks for the export itself rather than the version number, so a
#' development build of rtemis that predates the bridge also reads as missing.
#'
#' @return Logical: TRUE when rtemis can be loaded and exports `fit_predict()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rtemis_fit_available <- function() {
  requireNamespace("rtemis", quietly = TRUE) &&
    "fit_predict" %in% getNamespaceExports("rtemis")
} # /rtemis.draw::rtemis_fit_available


# %% fit_curves ----
#' Fit the curves a chart config asks for
#'
#' One curve per `y` column and group level, or per `y` column over every group
#' when `fit_per_group` is `FALSE`, each evaluated at `n_fit` points over the
#' range of its x values, with the interval of `se_times` standard errors when
#' `se` is set and the model has standard errors. The rows fitted are those
#' with a finite x and y, a group when the config binds one, and `keep`.
#' spec: draw/shared-builders#fits
#'
#' @param config [ScatterConfig] or [LineConfig] with `fit` set.
#' @param data data.frame: The rows the chart draws.
#' @param keep Optional Logical: Rows the chart draws, one per row of `data`.
#'
#' @return List of curves, each a named list: `y`, `group` (NULL without a
#'   group and for a pooled fit), `x`, `fitted`, `lower`, `upper` (NULL
#'   without an interval), and `rsq`; NULL when `fit` is unset.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_curves <- function(config, data, keep = NULL) {
  if (is.null(config@fit)) {
    return(NULL)
  }
  x <- data[[config@x]]
  if (!is.numeric(x)) {
    abort(
      "A fit needs a numeric x; column '",
      config@x,
      "' is not numeric.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  group <- if (!is.null(config@group)) data[[config@group]]
  keep <- keep %||% rep(TRUE, nrow(data))
  if (!is.null(group)) {
    keep <- keep & !is.na(group)
  }
  per_group <- !is.null(group) && config@fit_per_group
  levels <- if (per_group) group_levels(group[keep]) else list(NULL)
  curves <- list()
  for (y_name in config@y) {
    y <- data[[y_name]]
    for (level in levels) {
      rows <- keep & is.finite(x) & is.finite(y)
      if (!is.null(level)) {
        rows <- rows & as.character(group) == level
      }
      curve <- fit_curve(x[rows], y[rows], config)
      # The builder labels a group by its JSON text: a logical as true/false.
      label <- if (!is.null(level) && is.logical(group)) {
        tolower(level)
      } else {
        level
      }
      curves[[length(curves) + 1L]] <- c(
        list(y = y_name, group = label),
        curve
      )
    }
  }
  curves
} # /rtemis.draw::fit_curves


# %% fit_curve ----
#' Fit one curve
#'
#' @param x,y Numeric: The finite pairs fitted.
#' @param config [ScatterConfig] or [LineConfig]: The fit members.
#'
#' @return Named list: `x`, `fitted`, `lower`, `upper`, `rsq`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_curve <- function(x, y, config) {
  # A fit needs a residual degree of freedom to state its uncertainty.
  minimum <- if (config@se) 3L else 2L
  if (length(x) < minimum || length(unique(x)) < 2L) {
    abort(
      "Each fitted series needs at least ",
      minimum,
      " complete pairs and two distinct x values; add observations or unset `fit`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  newdata <- data.frame(x = seq(min(x), max(x), length.out = config@n_fit))
  pred <- fit_model_values(
    data.frame(x = x),
    y,
    newdata,
    config@fit,
    config@se,
    config@fit_params
  )
  # A learner without standard errors draws no interval.
  band <- pred[["se"]]
  list(
    x = newdata[["x"]],
    fitted = pred[["fitted"]],
    lower = if (!is.null(band)) pred[["fitted"]] - config@se_times * band,
    upper = if (!is.null(band)) pred[["fitted"]] + config@se_times * band,
    rsq = pred[["rsq"]]
  )
} # /rtemis.draw::fit_curve
