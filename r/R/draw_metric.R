# draw_metric.R
# spec: draw/first-cran-release#metric-distributions

#' Materialize a selected metric as aligned sample columns
#' @inheritParams draw_metric
#' @return List: Selected metric, sample column names, and a wide data frame
#'   with an observation ID column. Absent sample/fold pairs become NA.
#' @keywords internal
#' @noRd
metric_data <- new_generic("metric_data", "data")
method(metric_data, class_data.frame) <- function(data, metric = NULL) {
  data <- as.data.frame(data)
  required <- c("fold", "split", "metric", "value")
  if (
    !nrow(data) || anyDuplicated(names(data)) || !all(required %in% names(data))
  ) {
    abort(
      "Supply a nonempty table with unique columns `fold`, `split`, `metric`, and `value`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  for (column in required[1:3]) {
    values <- data[[column]]
    if (
      (!is.character(values) && !is.factor(values)) ||
        anyNA(values) ||
        any(!nzchar(as.character(values)))
    ) {
      abort(
        "Supply nonempty character identities in `",
        column,
        "`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    data[[column]] <- as.character(values)
  }
  measures <- unique(data[["metric"]])
  if (is.null(metric) && length(measures) != 1L) {
    abort(
      "Choose one `metric` from the table: ",
      paste(measures, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  metric <- metric %||% measures[[1L]]
  if (
    !is.character(metric) ||
      length(metric) != 1L ||
      is.na(metric) ||
      !metric %in% measures
  ) {
    abort(
      "Select one available `metric`: ",
      paste(measures, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  data <- data[data[["metric"]] == metric, , drop = FALSE]
  if (anyDuplicated(data[c("fold", "split")])) {
    abort(
      "Supply exactly one value per selected metric, split, and fold.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Validate before widening: no coercion of strings, infinite scores, or
  # list columns, and no assumption that an unavailable score equals zero.
  boxplot_values(data[["value"]])
  splits <- unique(data[["split"]])
  folds <- unique(data[["fold"]])
  observation <- ".observation"
  while (observation %in% splits) {
    observation <- paste0(observation, "_")
  }
  wide <- setNames(data.frame(folds, stringsAsFactors = FALSE), observation)
  for (split in splits) {
    at <- which(data[["split"]] == split)
    wide[[split]] <- as.numeric(data[["value"]][at][match(
      folds,
      data[["fold"]][at]
    )])
  }
  list(
    data = wide,
    columns = splits,
    observation = observation,
    metric = metric
  )
}

#' Draw a Metric's Distribution Across Resamples
#'
#' Draw stored fold scores using [BoxplotConfig], with every available
#' observation overlaid. Fold scores carry equal weight; this is a distribution
#' of recorded scores, not a metric recomputed from pooled predictions or a
#' confidence interval. Negative and unbounded metrics such as R-squared are
#' not clipped to the unit interval.
#'
#' Select one metric per chart. Sample and fold order follow first appearance.
#' Absent sample/fold pairs and explicit NA scores remain missing; available
#' values alone define each box. Entirely unavailable samples retain an empty
#' category, and missing values are disclosed in a caption. Entirely
#' unavailable input is an error. See [draw_boxplot()] for exact quartile,
#' whisker, and point-placement semantics.
#'
#' The portable result uses one numeric column per split and one observation
#' identifier column, bound through [setup_BoxplotConfig()]. No fitted object,
#' R callback, or attribute is needed to render it in another interface.
#'
#' @param data Data frame: Long records with character `fold`, `split`, and
#'   `metric` identities, and numeric `value` scores (finite or NA).
#' @param metric Optional Character: Metric to select. May be omitted when the
#'   table contains one metric.
#' @param boxpoints Character \{"none", "all", "outliers"\}: Values to overlay.
#' @param horizontal Logical: Draw horizontal distributions.
#' @param xlab,ylab Optional Character: Axis labels. The value axis derives its
#'   label from the selected metric when omitted; explicit NULL omits it.
#' @param ... Additional arguments to [setup_BoxplotConfig()], such as
#'   `quartiles`, `whisker`, `point_spread`, `palette`, and `title`.
#' @inheritParams draw_line
#' @return htmlwidget: Metric distributions with optional point overlays.
#' @export
#' @examples
#' scores <- data.frame(fold = rep(c("Fold1", "Fold2", "Fold3"), 2),
#'   split = rep(c("Training", "Test"), each = 3), metric = "rsq",
#'   value = c(.9, .85, .8, .7, .65, .6))
#' draw_metric(scores)
draw_metric <- function(
  data,
  metric = NULL,
  boxpoints = "all",
  horizontal = FALSE,
  xlab = if (horizontal) labelify(metric) else NULL,
  ylab = if (!horizontal) labelify(metric) else NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  ...
) {
  records <- metric_data(data, metric)
  metric <- records[["metric"]]
  config <- setup_BoxplotConfig(
    x = records[["columns"]],
    observation = records[["observation"]],
    boxpoints = boxpoints,
    horizontal = horizontal,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  draw(
    config,
    data = records[["data"]],
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
