# plot_massglm.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Plot a MassGLM Result
#'
#' `plot(model)` draws a volcano plot when rtemis.draw is loaded.
#' `rtemis.draw::plot_manhattan(model)` draws categorical significance bars,
#' preserving outcome order. Both select one coefficient and use the shared
#' [SignificanceConfig] statistical contract via [draw_volcano()] or
#' [draw_manhattan()].
#'
#' Draw registers its method on the existing [graphics::plot()] generic for
#' the S7 MassGLM class, replacing that class's legacy method while draw is
#' loaded. The prior method is restored on namespace unload if draw still owns
#' the registration. This does not replace rtemis's `plot_manhattan` generic;
#' qualify the draw-owned generic when both packages are attached.
#'
#' @param x `rtemis::MassGLM`: Fitted mass-univariate model.
#' @param coefname Optional Character scalar: Coefficient to plot. Unset selects
#'   the first entry in the object's `coefnames`.
#' @param ... Named settings for [draw_volcano()] or [draw_manhattan()].
#' @return An ECharts htmlwidget.
#' @aliases plot.MassGLM
#' @export
#' @examplesIf requireNamespace("rtemis", quietly = TRUE)
#' model <- rtemis::massGLM(mtcars["wt"], mtcars[c("mpg", "hp", "disp")],
#'                          verbosity = 0L)
#' plot(model, coefname = "wt")
#' rtemis.draw::plot_manhattan(model, coefname = "wt")
plot_manhattan <- new_generic(
  "plot_manhattan",
  "x",
  function(x, coefname = NULL, ...) S7_dispatch()
)


#' Extract and align a MassGLM coefficient across outcomes
#'
#' The summary's Variable column identifies rows; align it to ynames before
#' selecting values so a reordered summary cannot mislabel an outcome.
#'
#' @inheritParams plot_manhattan
#' @return List containing `data` and the selected `coefname`.
#' @keywords internal
#' @noRd
massglm_plot_data <- new_generic("massglm_plot_data", "x")

#' @rdname massglm_plot_data
#' @keywords internal
#' @noRd
extract_massglm_plot_data <- function(x, coefname = NULL) {
  available <- x@coefnames
  if (!length(available)) {
    abort(
      "The MassGLM object has no coefficients; fit at least one estimable term.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  coefname <- coefname %||% available[[1L]]
  if (
    !is.character(coefname) ||
      length(coefname) != 1L ||
      is.na(coefname) ||
      !coefname %in% available
  ) {
    abort(
      "Select one `coefname` from: ",
      paste(available, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  columns <- c(
    "Variable",
    paste0("Coefficient_", coefname),
    paste0("p_value_", coefname)
  )
  table <- x@summary
  labels <- x@ynames
  if (!all(columns %in% names(table)) || anyDuplicated(names(table))) {
    abort(
      "The MassGLM summary must contain unique Variable, coefficient, and p-value columns; rebuild the model summary.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keys <- table[["Variable"]]
  if (
    !is.character(keys) ||
      anyNA(keys) ||
      anyDuplicated(keys) ||
      !length(labels) ||
      anyNA(labels) ||
      anyDuplicated(labels) ||
      any(!nzchar(labels)) ||
      !setequal(keys, labels)
  ) {
    abort(
      "Match unique summary Variable names to the MassGLM outcome names before plotting.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  at <- match(labels, keys)
  data <- significance_input(
    table[[columns[[2L]]]][at],
    table[[columns[[3L]]]][at],
    labels
  )
  list(data = data, coefname = coefname)
}


#' Draw the default volcano view of a MassGLM object
#' @inheritParams plot_manhattan
#' @param title Optional Character: Chart title; unset names the coefficient.
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_massglm_volcano <- function(x, coefname = NULL, ..., title = NULL) {
  extracted <- massglm_plot_data(x, coefname)
  data <- extracted[["data"]]
  if (missing(title)) {
    title <- paste("MassGLM:", extracted[["coefname"]])
  }
  draw_volcano(
    data[["estimate"]],
    data[["p_value"]],
    data[["label"]],
    title = title,
    ...
  )
}


#' Draw the categorical Manhattan view of a MassGLM object
#' @inheritParams draw_massglm_volcano
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_massglm_manhattan <- function(x, coefname = NULL, ..., title = NULL) {
  extracted <- massglm_plot_data(x, coefname)
  data <- extracted[["data"]]
  if (missing(title)) {
    title <- paste("MassGLM:", extracted[["coefname"]])
  }
  draw_manhattan(
    data[["estimate"]],
    data[["p_value"]],
    data[["label"]],
    title = title,
    ...
  )
}

# Keep the previous shared-generic method for namespace unload. A later
# registration by another package must not be overwritten during cleanup.
.massglm_plot_state <- new.env(parent = emptyenv())

#' Register draw's MassGLM method on the shared graphics generic
#' @param cls S7 class: The optional rtemis MassGLM class.
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
register_massglm_plot <- function(cls) {
  previous <- utils::getS3method("plot", "rtemis::MassGLM", optional = TRUE)
  if (!identical(previous, draw_massglm_volcano)) {
    .massglm_plot_state[["previous"]] <- previous
  }
  S7::method(plot, cls) <- draw_massglm_volcano
  invisible(NULL)
}

#' Restore the preceding MassGLM plot method when unloading draw
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
restore_massglm_plot <- function() {
  if (!"rtemis" %in% loadedNamespaces()) {
    return(invisible(NULL))
  }
  current <- utils::getS3method("plot", "rtemis::MassGLM", optional = TRUE)
  previous <- .massglm_plot_state[["previous"]]
  if (identical(current, draw_massglm_volcano) && is.function(previous)) {
    # This is the S3 bridge used by S7 for graphics::plot(). Register directly
    # during unload so cleanup does not enqueue another draw-owned S7 method.
    registerS3method(
      "plot",
      "rtemis::MassGLM",
      previous,
      envir = asNamespace("graphics")
    )
  }
  invisible(NULL)
}
