# draw_learning_curve.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% learning_curve_data ----
#' Prepare learning losses for a portable line chart
#'
#' Validate recorded steps before averaging trees. Each loss is averaged over
#' its own available values, so absent validation losses never discard training
#' observations. Materialize the selected point as data rather than a callback.
#'
#' @param data Data frame: Recorded learning losses.
#' @param selected Optional Numeric: Selected step, or NA for no selection.
#' @return Data frame with `iteration`, available `Training` and `Validation`
#'   columns, and `Selected` when its loss is available.
#' @keywords internal
#' @noRd
learning_curve_data <- new_generic("learning_curve_data", "data")

method(learning_curve_data, class_data.frame) <- function(
  data,
  selected = NULL
) {
  data <- as.data.frame(data)
  loss_names <- intersect(c("loss_training", "loss_validation"), names(data))
  if (
    anyDuplicated(names(data)) ||
      nrow(data) == 0L ||
      !"iteration" %in% names(data) ||
      length(loss_names) == 0L
  ) {
    abort(
      "Supply a nonempty learning-curve table with unique column names, ",
      "`iteration`, and `loss_training` or `loss_validation`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  iteration <- data[["iteration"]]
  if (
    !is.numeric(iteration) ||
      is.complex(iteration) ||
      !is.null(dim(iteration)) ||
      any(!is.finite(iteration))
  ) {
    abort(
      "Supply finite numeric `iteration` values for every learning step.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.logical(selected) && length(selected) == 1L && is.na(selected)) {
    selected <- NULL
  }
  if (
    !is.null(selected) &&
      (length(selected) != 1L ||
        !is.numeric(selected) ||
        is.complex(selected) ||
        !is.null(dim(selected)) ||
        is.infinite(selected))
  ) {
    abort(
      "Set `selected` to one finite numeric step, NA, or NULL.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keys <- "iteration"
  if ("tree" %in% names(data)) {
    tree <- data[["tree"]]
    if (!is.atomic(tree) || !is.null(dim(tree)) || anyNA(tree)) {
      abort(
        "Supply one nonmissing `tree` identifier per learning-curve row.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    keys <- c("tree", keys)
  }
  if (anyDuplicated(data[keys])) {
    abort(
      "Supply one row per iteration, or per tree and iteration for an ensemble.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  steps <- sort(unique(iteration))
  # Split once for both loss columns; trees may stop at different steps.
  rows <- split(seq_along(iteration), match(iteration, steps))
  rows <- rows[as.character(seq_along(steps))]
  out <- data.frame(iteration = steps)
  for (column in loss_names) {
    values <- data[[column]]
    # An entirely absent series is often supplied as a logical NA column.
    if (is.logical(values) && all(is.na(values))) {
      values <- as.numeric(values)
    }
    if (
      !is.numeric(values) ||
        is.complex(values) ||
        !is.null(dim(values)) ||
        any(is.infinite(values))
    ) {
      abort(
        "Supply finite numeric losses or NA in `",
        column,
        "`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (all(is.na(values))) {
      next
    }
    name <- if (column == "loss_training") "Training" else "Validation"
    out[[name]] <- vapply(
      rows,
      function(at) {
        loss <- values[at]
        if (all(is.na(loss))) NA_real_ else mean(loss, na.rm = TRUE)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  }
  if (ncol(out) == 1L) {
    abort(
      "The learning curve holds no losses; supply at least one finite loss.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Match the object method's existing convention: mark training loss when
  # recorded, otherwise validation loss. Never interpolate an unrecorded step.
  at <- match(selected, steps)
  if (length(at) == 1L && !is.na(at) && !is.na(out[[2L]][at])) {
    out[["Selected"]] <- rep(NA_real_, length(steps))
    out[["Selected"]][at] <- out[[2L]][at]
  }
  out
}


# %% draw_learning_curve ----
#' Draw a Learning Curve from Recorded Losses
#'
#' Draw training and validation losses against training progress using a
#' [LineConfig]. This function accepts an ordinary table and does not require
#' rtemis. Use [rtemis.draw::plot_learning()] to draw the curve of a model.
#'
#' @details
#' Steps are sorted numerically. With a `tree` column, losses are averaged at
#' each step over trees with a nonmissing loss for that series. Trees that have
#' stopped contribute no value at later steps. Entirely missing series are
#' omitted; missing values within a series remain gaps.
#'
#' The selected point uses training loss if that series exists, otherwise
#' validation loss. An absent step or missing loss produces no selected marker.
#' `selected = NULL` disables it. The marker follows the line config's `points`
#' setting, so `points = FALSE` hides it along with the other point symbols.
#'
#' For another interface, materialize columns `iteration`, `Training`,
#' `Validation`, and optionally `Selected` (missing except at the selected
#' step), omitting unavailable loss columns. Bind these with [setup_LineConfig()]
#' using `x = "iteration"` and `y` naming the loss columns. Store the unit as
#' `xlab`. That data and config reproduce the chart without R attributes or a
#' fitted model; aggregation is performed before rendering.
#'
#' @param data Data frame: Numeric `iteration` and at least one of
#'   `loss_training` or `loss_validation`. An optional `tree` column identifies
#'   ensemble members. Iterations must be unique within each tree.
#' @param unit Optional Character: Unit of progress, such as `"epochs"` or
#'   `"leaves"`. Read from the table's `unit` attribute when omitted. NULL uses
#'   `"Iteration"` for the axis label.
#' @param selected Optional Numeric: Selected step. Read from the table's
#'   `selected` attribute when omitted. NA or NULL means no selected step.
#' @param xlab Optional Character: Horizontal axis label, overriding `unit`.
#' @param ylab Optional Character: Vertical axis label.
#' @param title Optional Character: Chart title.
#' @inheritParams draw_line
#' @param theme Optional [Theme]: Theme override. Set `palette` in `...` to
#'   override its series colors.
#' @param ... Additional appearance and axis arguments to [setup_LineConfig()],
#'   such as `palette`, `zoom`, `points`, `xlim`, and `margin_top`. The data
#'   bindings are determined by the loss table.
#'
#' @return htmlwidget: ECharts learning curve.
#' @export
#' @examples
#' losses <- data.frame(
#'   iteration = 1:4,
#'   loss_training = c(4, 2, 1, 0.5),
#'   loss_validation = c(4.5, 2.5, 2, 2.2)
#' )
#' draw_learning_curve(losses, unit = "epochs", selected = 3)
#' @inheritParams draw_line legend_position legend_placement
draw_learning_curve <- function(
  data,
  unit = attr(data, "unit", exact = TRUE),
  selected = attr(data, "selected", exact = TRUE),
  xlab = NULL,
  ylab = "Loss",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  ...,
  legend_position = "top",
  legend_placement = "outside"
) {
  if (!is.null(unit)) {
    check_character_scalar(unit)
  }
  values <- learning_curve_data(data, selected = selected)
  config <- setup_LineConfig(
    x = "iteration",
    y = names(values)[-1L],
    xlab = xlab %||% if (is.null(unit)) "Iteration" else labelify(unit),
    ylab = ylab,
    title = title,
    legend_position = legend_position,
    legend_placement = legend_placement,
    ...
  )
  draw(
    config,
    data = values,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
