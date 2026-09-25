# draw_fit.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% true_pred_data ----
#' Normalize paired regression observations
#'
#' Named lists are matched by name; unnamed lists are matched by position.
#' Each pair is checked before concatenation, so unequal split or fold lengths
#' cannot silently shift observations. Missing pairs are removed together.
#'
#' @param x Numeric vector, single-column matrix, or list: True observations.
#' @param y Numeric vector, single-column matrix, or list: Predicted values.
#' @param group Optional Atomic vector: Groups for vector inputs.
#'
#' @return Data frame with `true`, `predicted`, and optional `sample` columns.
#' @keywords internal
#' @noRd
true_pred_data <- new_generic("true_pred_data", c("x", "y"))


method(true_pred_data, list(class_any, class_any)) <- function(
  x,
  y,
  group = NULL
) {
  # Matrices represent one outcome, while lists/data frames represent sets.
  vector_value <- function(value) {
    is.numeric(value) &&
      (is.null(dim(value)) ||
        (is.matrix(value) && ncol(value) == 1L))
  }
  grouped <- is.list(x) || is.list(y)
  if (grouped) {
    if (
      !is.list(x) || !is.list(y) || length(x) != length(y) || length(x) == 0L
    ) {
      abort(
        "Supply `x` and `y` as nonempty lists with the same number of sets.",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    if (!is.null(group)) {
      abort(
        "Use list names to group paired lists; supply `group` only with vectors.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    for (nms in list(names(x), names(y))) {
      if (
        !is.null(nms) && (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms))
      ) {
        abort(
          "Give every set a unique, nonempty name, or leave all sets unnamed.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    if (!is.null(names(x)) && !is.null(names(y))) {
      if (!setequal(names(x), names(y))) {
        abort(
          "Use the same set names in `x` and `y`.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      y <- y[names(x)]
    }
    labels <- names(x) %||% names(y) %||% paste("Set", seq_along(x))
  } else {
    x <- list(x)
    y <- list(y)
    labels <- NULL
  }

  parts <- lapply(seq_along(x), function(i) {
    a <- x[[i]]
    b <- y[[i]]
    # Resampled results can retain a missing fold as paired NULL entries.
    if (grouped && is.null(a) && is.null(b)) {
      return(NULL)
    }
    if (!vector_value(a) || !vector_value(b)) {
      abort(
        "Supply numeric vectors or single-column numeric matrices for each pair.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    a <- as.numeric(a)
    b <- as.numeric(b)
    if (length(a) != length(b) || length(a) == 0L) {
      abort(
        "Each true/predicted pair must have the same positive length; check set ",
        i,
        ".",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    if (any(is.infinite(a)) || any(is.infinite(b))) {
      abort(
        "Replace infinite true or predicted values with finite values or NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    sample <- if (grouped) rep(labels[[i]], length(a)) else group
    if (
      !is.null(sample) &&
        (!is.atomic(sample) ||
          !is.null(dim(sample)) ||
          length(sample) != length(a))
    ) {
      abort(
        "Supply `group` as a vector with one value per observation.",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    keep <- !is.na(a) & !is.na(b)
    if (!is.null(sample)) {
      keep <- keep & !is.na(sample)
    }
    if (!any(keep)) {
      abort(
        "Each selected set needs at least one complete true/predicted pair.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    out <- data.frame(true = a[keep], predicted = b[keep])
    if (!is.null(sample)) {
      out[["sample"]] <- as.character(sample[keep])
    }
    out
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    abort(
      "Supply at least one set containing true/predicted pairs.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}


# %% draw_fit ----
#' Draw True Versus Predicted Values
#'
#' Draw paired observations with a fitted line, standard-error band, and an
#' identity line. The plotting box is square and both axes share an interval.
#' Named lists support samples of different lengths, with one fit per sample.
#'
#' @details
#' The overlay uses the same implementation as [draw_scatter()] and
#' [ScatterConfig]. The fit is GLM or GAM; `fit = NULL` disables it. Missing
#' pairs and observations with missing groups are dropped together. Infinite
#' values, unequal pair lengths, and multiple response columns are rejected.
#' List names are matched before drawing; unnamed lists are paired by position.
#' Paired `NULL` entries in lists represent unavailable sets and are omitted.
#'
#' The equivalent portable configuration is a [ScatterConfig] binding true,
#' predicted, and sample columns, with `fit`, `se`, `rsq`, `diagonal`, `square`,
#' and `equal_axes` set explicitly. Model objects are not needed to render it.
#'
#' @param x Numeric vector, single-column matrix, or list: True values.
#' @param y Numeric vector, single-column matrix, or list: Predicted values.
#' @inheritParams draw_scatter
#' @param ... Additional arguments to [draw_scatter()], including `theme`,
#'   `palette`, limits, margins, dimensions, and `filename`.
#'
#' @return htmlwidget: ECharts drawing.
#' @export
#' @examples
#' draw_fit(1:5, c(1.2, 1.8, 3.3, 3.8, 5.2))
#' draw_fit(
#'   list(Training = 1:5, Test = 2:5),
#'   list(Training = c(1.2, 1.8, 3.3, 3.8, 5.2), Test = c(2.2, 2.7, 4.3, 4.8))
#' )
draw_fit <- function(
  x,
  y,
  group = NULL,
  fit = "glm",
  se = TRUE,
  se_times = 1.96,
  rsq = TRUE,
  diagonal = TRUE,
  diagonal_color = NULL,
  square = TRUE,
  equal_axes = TRUE,
  xlab = "True",
  ylab = "Predicted",
  ...
) {
  data <- true_pred_data(x, y, group = group)
  # The shared scatter builder validates these settings against ScatterConfig.
  draw_scatter(
    data[["true"]],
    data[["predicted"]],
    group = data[["sample"]],
    fit = fit,
    se = se,
    se_times = se_times,
    rsq = rsq,
    diagonal = diagonal,
    diagonal_color = diagonal_color,
    square = square,
    equal_axes = equal_axes,
    xlab = xlab,
    ylab = ylab,
    ...
  )
}
