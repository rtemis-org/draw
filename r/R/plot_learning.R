# plot_learning.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% plot_learning ----
#' Plot a Model's Learning Curve
#'
#' Draw training and validation losses recorded by a rtemis `Supervised`
#' object. This draw-owned S7 generic is separate from `rtemis::plot_learning()`;
#' use `rtemis.draw::plot_learning()` when both packages are attached.
#'
#' The method uses `rtemis::get_learning_curve()` to extract algorithm-specific
#' progress, losses, and the selected step, then [draw_learning_curve()] to
#' render them. Forest curves are averaged at each step over available trees.
#' Models without recorded learning losses produce an informative error.
#' The default title names the algorithm; pass `title = NULL` to omit it or
#' `title = "..."` to supply another title.
#'
#' @param x `rtemis::Supervised`: Fitted regression or classification result.
#' @param ... Additional arguments to [draw_learning_curve()].
#'
#' @return htmlwidget: ECharts learning curve.
#' @export
#' @examplesIf requireNamespace("rtemis", quietly = TRUE)
#' model <- rtemis::train(
#'   mtcars[, c("wt", "mpg")],
#'   hyperparameters = rtemis::setup_LINAD(max_leaves = 3L),
#'   verbosity = 0L
#' )
#' rtemis.draw::plot_learning(model)
plot_learning <- new_generic("plot_learning", "x")


# %% draw_model_learning_curve ----
#' Extract and draw the learning curve of a supervised model
#'
#' Registered on the public generic when the optional rtemis dependency exists.
#' Algorithm extraction stays with the model package; the renderer needs only
#' its data frame and explicit metadata.
#'
#' @inheritParams plot_learning
#' @param title Optional Character: Chart title.
#' @return htmlwidget: ECharts learning curve.
#' @keywords internal
#' @noRd
draw_model_learning_curve <- function(
  x,
  title = paste(x@algorithm, "learning curve"),
  ...
) {
  curve <- rtemis::get_learning_curve(x)
  if (is.null(curve)) {
    abort(
      x@algorithm,
      " records no learning curve. ",
      "Use a model that records losses during training.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  draw_learning_curve(curve, title = title, ...)
}
