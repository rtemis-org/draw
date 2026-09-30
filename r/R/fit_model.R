# fit_model.R
# Named fits: GLM and GAM are fitted here, every other algorithm by rtemis.
# spec: draw/learner-fits

# The first rtemis release exporting fit_predict().
RTEMIS_FIT_VERSION <- "1.4.1"

#' Fit a named model and predict new data
#'
#' The single fitting step behind `fit =` in scatter, fit and 3D scatter
#' drawings. `"glm"` and `"gam"`, in any case, use [stats::glm()] and
#' [mgcv::gam()] with a smooth term per feature. Any other name is an rtemis
#' supervised learning algorithm, trained by `rtemis::fit_predict()`.
#'
#' `params` are passed to the learner: to [stats::glm()] for GLM; to
#' [mgcv::gam()] for GAM, except `k`, which sets the basis dimension of every
#' smooth term; and to the algorithm's `rtemis::setup_*()` function otherwise.
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
  if (!method %in% c("glm", "gam")) {
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
  data <- x
  data[[".outcome"]] <- y
  terms <- names(x)
  learner <- if (method == "gam") "mgcv::gam" else "stats::glm"
  model <- tryCatch(
    if (method == "gam") {
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
          params[setdiff(names(params), "k")]
        )
      )
    } else {
      do.call(
        stats::glm,
        c(
          list(stats::reformulate(terms, response = ".outcome"), data = data),
          params
        )
      )
    },
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
  pred <- stats::predict(
    model,
    newdata = newdata,
    type = "response",
    se.fit = TRUE
  )
  total <- sum((y - mean(y))^2)
  list(
    fitted = unname(as.numeric(pred[["fit"]])),
    se = if (se) unname(as.numeric(pred[["se.fit"]])),
    rsq = if (total > 0) {
      1 - sum(stats::residuals(model, type = "response")^2) / total
    } else {
      NA_real_
    }
  )
} # /rtemis.draw::fit_model_values


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
      "): install or update rtemis, or use fit = \"glm\" or \"gam\".",
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
