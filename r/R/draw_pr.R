# draw_pr.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Compute empirical precision-recall vertices with whole tied-score blocks
#'
#' Higher scores always indicate the named positive class. Each distinct score
#' is one threshold, and a tied block enters at once. Average precision is the
#' step-wise sum \eqn{\sum_k (R_k - R_{k-1}) P_k} over thresholds, which is the
#' area under the stepped curve. Precision has no value before the first
#' threshold, so the curve starts at recall 0 with the precision of the first
#' threshold, adding no area. The curve ends at the threshold that predicts every
#' case positive, where precision is the prevalence of the positive class.
#' A class with no positive observation has an undefined curve.
#' @param x List: Normalized labels and probabilities from `roc_probabilities()`.
#' @return Data frame of class, recall, precision, average precision (`ap`),
#'   prevalence, and omitted-pair counts.
#' @export
#' @examples
#' pr_vertices(roc_probabilities(factor(c("no", "yes")), c(0.1, 0.9)))
pr_vertices <- new_generic("pr_vertices", "x", function(x) S7_dispatch())
method(pr_vertices, class_list) <- function(x) {
  check_curve_scores(x)
  pieces <- lapply(x[["classes"]], function(cl) {
    score <- x[["prob"]][, cl]
    keep <- !is.na(x[["y"]]) & !is.na(score)
    response <- x[["y"]][keep] == cl
    score <- score[keep]
    positives <- sum(response)
    if (!positives) {
      return(data.frame(
        class = cl,
        recall = NA_real_,
        precision = NA_real_,
        ap = NA_real_,
        prevalence = NA_real_,
        omitted = sum(!keep)
      ))
    }
    ord <- order(score, decreasing = TRUE)
    score <- score[ord]
    response <- response[ord]
    ends <- c(which(diff(score) != 0), length(score))
    true_positives <- cumsum(response)[ends]
    precision <- true_positives / ends
    recall <- true_positives / positives
    ap <- sum(diff(c(0, recall)) * precision)
    data.frame(
      class = cl,
      recall = c(0, recall),
      precision = c(precision[[1L]], precision),
      ap = ap,
      prevalence = positives / length(response),
      omitted = sum(!keep)
    )
  })
  do.call(rbind, pieces)
}

#' Prepare named input sets or precomputed precision-recall records
#' @param x Vector, list, or data frame: Reference labels or curve records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Probabilities.
#' @param positive Optional Character: Binary positive class.
#' @return Data frame of precision-recall records.
#' @keywords internal
#' @noRd
pr_input <- new_generic("pr_input", "x")
method(pr_input, class_any) <- function(
  x,
  predicted_prob = NULL,
  positive = NULL
) {
  curve_input(x, predicted_prob, positive, pr_vertices, "recall and precision")
}

#' Draw Precision-Recall Curves
#'
#' Accepts reference labels and predicted probabilities, matching lists of those
#' inputs, or a long table with `recall`, `precision`, and optional `ap`,
#' `prevalence`, `class`, `split`, `fold`, and `omitted` columns. Named input
#' lists are matched by name.
#'
#' Binary input uses the second factor level (second level after factor
#' conversion for nonfactors), unless `positive` names the desired class.
#' Precision and recall are defined on the positive class, so that choice
#' changes the curve. An unnamed probability vector/column describes that class.
#' Named columns identify their classes; multiclass matrices use names, or
#' factor-level order when unnamed. Multiclass curves are one-versus-rest.
#' Missing labels/scores are excluded separately for each curve and disclosed.
#' Scores must be probabilities in `[0, 1]`; direction is fixed, and tied scores
#' enter together. No observations are deduplicated, curves smoothed, or vertices
#' downsampled.
#' @inheritSection PRConfig Statistical semantics
#' @section Legend placement:
#' The shared top/outside layout reserves a horizontal band above the plot.
#' Choose an inside corner to overlay the legend on the data.
#' @param true_labels Factor, vector, list, or data frame: Reference labels or
#'   precision-recall records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Class probabilities.
#' @param positive Optional Character: Positive class for binary input.
#' @param ... Additional named settings for [setup_PRConfig()].
#' @param theme Optional [Theme]: Chart theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_pr(factor(c("no", "yes", "no", "yes")), c(.1, .8, .5, .5))
#' draw_pr(data.frame(recall = c(0, 0.5, 1), precision = c(1, 1, 0.5)))
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_pr <- function(
  true_labels,
  predicted_prob = NULL,
  positive = NULL,
  ...,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  data <- pr_input(true_labels, predicted_prob, positive)
  binding <- function(name) if (name %in% names(data)) name else NULL
  config <- setup_PRConfig(
    ap = binding("ap"),
    prevalence = binding("prevalence"),
    class_label = binding("class"),
    split = binding("split"),
    fold = binding("fold"),
    omitted = binding("omitted"),
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

# spec: draw/first-cran-release#precision-recall-views
method(curve_spec, PRConfig) <- function(config) {
  list(
    name = "PR",
    summary = "AP",
    x = "Recall",
    y = "Precision",
    # "start" rises at the current recall, so precision at a vertex applies to
    # the recall interval ending there, and the drawn area equals AP. Precision
    # is not linear in recall between vertices, so `step = FALSE` would misstate
    # it and draw an area that differs from AP in either direction.
    step = "start",
    # Within one recall value, threshold order runs from high to low precision.
    order = function(x, y) order(x, -y),
    valid = function(x, y) {
      length(x) >= 2L && x[[1L]] == 0 && tail(x, 1L) == 1
    },
    area = function(x, y) sum(diff(x) * tail(y, -1L)),
    coordinates = "Supply aligned numeric recall and precision values in [0, 1].",
    invalid = "Supply complete precision-recall curves from recall 0 to recall 1.",
    undefined = "No defined precision-recall curve: each curve needs positive observations."
  )
}

#' Validate and partition portable precision-recall records
#' @param config [PRConfig]: Bound configuration.
#' @param data Data frame or named list: Long curve records.
#' @return List containing selected curves, groups, and disclosure counts.
#' @keywords internal
#' @noRd
pr_data <- new_generic("pr_data", "config")
method(pr_data, PRConfig) <- function(config, data) {
  curve_data(
    config,
    data,
    x = config_column(data, config@recall, "recall"),
    y = config_column(data, config@precision, "precision"),
    summary = config_column(data, config@ap, "ap"),
    reference = if (config@baseline) {
      config_column(data, config@prevalence, "prevalence")
    }
  )
}

#' Compile precision-recall records to independent native line series
#' @inheritParams pr_data
#' @return An [EChartsOption] with full legend and prevalence baselines.
#' @keywords internal
#' @noRd
pr_option <- new_generic("pr_option", "config")
method(pr_option, PRConfig) <- function(config, data) {
  curve_option(config, pr_data(config, data))
}
