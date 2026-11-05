# draw_roc.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Normalize labels and probability columns by class identity
#' @param x Factor or atomic vector: Reference labels.
#' @param predicted_prob Numeric vector or matrix: Class probabilities.
#' @param positive Optional Character: Binary positive class.
#' @return List containing reference labels, class levels, selected classes, and scores.
#' @export
#' @examples
#' roc_probabilities(factor(c("no", "yes")), c(0.1, 0.9))
roc_probabilities <- new_generic(
  "roc_probabilities",
  "x",
  function(x, predicted_prob, positive = NULL) S7_dispatch()
)
method(roc_probabilities, class_any) <- function(
  x,
  predicted_prob,
  positive = NULL
) {
  if (
    !(is.factor(x) || is.character(x) || is.numeric(x) || is.logical(x)) ||
      !is.null(dim(x)) ||
      is.complex(x) ||
      !length(x)
  ) {
    abort(
      "Supply a nonempty reference-label vector.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  classes <- if (is.factor(x)) levels(x) else levels(as.factor(x))
  if (length(classes) < 2L || anyNA(classes) || any(!nzchar(classes))) {
    abort(
      "Supply at least two nonempty class levels, using a factor to retain absent classes.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  binary <- length(classes) == 2L
  if (
    !is.null(positive) &&
      (!binary ||
        !is.character(positive) ||
        length(positive) != 1L ||
        is.na(positive) ||
        !positive %in% classes)
  ) {
    abort(
      "Set `positive` to one of the two binary class labels, or leave it NULL.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  selected <- if (binary) positive %||% classes[[2L]] else classes
  p <- predicted_prob
  if (
    !is.numeric(p) ||
      is.complex(p) ||
      (!is.null(dim(p)) && length(dim(p)) != 2L) ||
      NROW(p) != length(x) ||
      any(!is.finite(p[!is.na(p)])) ||
      any(p < 0 | p > 1, na.rm = TRUE)
  ) {
    abort(
      "Supply aligned numeric probabilities in [0, 1], with NA for unavailable scores.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.null(dim(p))) {
    p <- matrix(p, ncol = 1L)
  }
  if (binary && ncol(p) == 1L) {
    # A named column identifies its class. An unnamed score follows the
    # selected positive class, allowing an explicit vector convention.
    score_class <- colnames(p) %||% selected
    if (
      length(score_class) != 1L ||
        is.na(score_class) ||
        !score_class %in% classes
    ) {
      abort(
        "Name the binary score column with its class label, or omit its name.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    score <- as.numeric(p)
    p <- cbind(score, 1 - score)
    colnames(p) <- c(score_class, setdiff(classes, score_class))
  } else {
    if (ncol(p) != length(classes)) {
      abort(
        "Supply one probability column per class (or one positive-class column for binary data).",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    if (is.null(colnames(p))) {
      colnames(p) <- classes
    }
    if (
      anyNA(colnames(p)) ||
        anyDuplicated(colnames(p)) ||
        !setequal(colnames(p), classes)
    ) {
      abort(
        "Probability column names must identify every reference class exactly once.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(
    y = as.character(x),
    levels = classes,
    classes = selected,
    prob = p[, classes, drop = FALSE]
  )
}

#' Compute empirical ROC vertices with whole tied-score blocks
#'
#' Higher scores always indicate the named positive class. Consuming a tied
#' block at once yields trapezoidal AUC equivalent to half credit for ties.
#' The direction is never selected from the observed performance.
#' @param x List: Normalized labels and probabilities from roc_probabilities().
#' @return Data frame of class, FPR, TPR, AUC, and omitted-pair counts.
#' @export
#' @examples
#' roc_vertices(roc_probabilities(factor(c("no", "yes")), c(0.1, 0.9)))
roc_vertices <- new_generic("roc_vertices", "x", function(x) S7_dispatch())
method(roc_vertices, class_list) <- function(x) {
  check_curve_scores(x)
  pieces <- lapply(x[["classes"]], function(cl) {
    score <- x[["prob"]][, cl]
    keep <- !is.na(x[["y"]]) & !is.na(score)
    response <- x[["y"]][keep] == cl
    score <- score[keep]
    positives <- sum(response)
    negatives <- length(response) - positives
    if (!positives || !negatives) {
      return(data.frame(
        class = cl,
        fpr = NA_real_,
        tpr = NA_real_,
        auc = NA_real_,
        omitted = sum(!keep)
      ))
    }
    ord <- order(score, decreasing = TRUE)
    score <- score[ord]
    response <- response[ord]
    ends <- c(which(diff(score) != 0), length(score))
    tpr <- c(0, cumsum(response)[ends] / positives)
    fpr <- c(0, cumsum(!response)[ends] / negatives)
    auc <- sum(diff(fpr) * (head(tpr, -1L) + tail(tpr, -1L)) / 2)
    data.frame(
      class = cl,
      fpr = fpr,
      tpr = tpr,
      auc = auc,
      omitted = sum(!keep)
    )
  })
  do.call(rbind, pieces)
}

#' Prepare named input sets or precomputed ROC records
#' @param x Vector, list, or data frame: Reference labels or curve records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Probabilities.
#' @param positive Optional Character: Binary positive class.
#' @return Data frame of ROC records.
#' @keywords internal
#' @noRd
roc_input <- new_generic("roc_input", "x")
method(roc_input, class_any) <- function(
  x,
  predicted_prob = NULL,
  positive = NULL
) {
  curve_input(x, predicted_prob, positive, roc_vertices, "fpr and tpr")
}

#' Draw Receiver Operating Characteristic Curves
#'
#' Accepts reference labels and predicted probabilities, matching lists of those
#' inputs, or a long table with `fpr`, `tpr`, and optional `auc`, `class`, `split`,
#' `fold`, and `omitted` columns. Named input lists are matched by name.
#'
#' Binary input uses the second factor level (second level after factor conversion for
#' nonfactors), unless `positive` names the desired class. An unnamed probability
#' vector/column describes that class. Named columns identify their classes;
#' multiclass matrices use names, or factor-level order when unnamed.
#' Multiclass curves are one-versus-rest. Missing labels/scores are excluded
#' separately for each curve and disclosed. Scores must be probabilities in
#' `[0, 1]`; direction is fixed, and tied scores enter together. No observations
#' are deduplicated, curves smoothed, or vertices downsampled.
#' @inheritSection ROCConfig Statistical semantics
#' @section Legend placement:
#' The shared top/outside layout reserves a horizontal band above the plot.
#' Choose an inside corner to overlay the legend on the data.
#' @param true_labels Factor, vector, list, or data frame: Reference labels or ROC records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Class probabilities.
#' @param positive Optional Character: Positive class for binary input.
#' @param ... Additional named settings for [setup_ROCConfig()].
#' @param theme Optional [Theme]: Chart theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_roc(factor(c("no", "yes", "no", "yes")), c(.1, .8, .5, .5))
#' draw_roc(data.frame(fpr = c(0, 0, 1), tpr = c(0, 1, 1)))
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_roc <- function(
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
  data <- roc_input(true_labels, predicted_prob, positive)
  binding <- function(name) if (name %in% names(data)) name else NULL
  config <- setup_ROCConfig(
    auc = binding("auc"),
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

method(curve_spec, ROCConfig) <- function(config) {
  list(
    name = "ROC",
    summary = "AUC",
    x = "FPR",
    y = "TPR",
    step = FALSE,
    # Sorting both coordinates preserves vertical tie runs, including input
    # from libraries that enumerate thresholds in the reverse direction.
    order = function(x, y) order(x, y),
    valid = function(x, y) {
      length(x) >= 2L &&
        x[[1L]] == 0 &&
        y[[1L]] == 0 &&
        tail(x, 1L) == 1 &&
        tail(y, 1L) == 1 &&
        !any(diff(y) < 0)
    },
    area = function(x, y) sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2),
    coordinates = "Supply aligned numeric FPR and TPR values in [0, 1].",
    invalid = "Supply monotone complete ROC curves from (0, 0) to (1, 1).",
    undefined = "No defined ROC curve: each curve needs both positive and negative observations."
  )
}

#' Validate and partition portable ROC records
#' @param config [ROCConfig]: Bound configuration.
#' @param data Data frame or named list: Long curve records.
#' @return List containing selected curves, groups, and disclosure counts.
#' @keywords internal
#' @noRd
roc_data <- new_generic("roc_data", "config")
method(roc_data, ROCConfig) <- function(config, data) {
  curve_data(
    config,
    data,
    x = config_column(data, config@fpr, "fpr"),
    y = config_column(data, config@tpr, "tpr"),
    summary = config_column(data, config@auc, "auc")
  )
}

#' Compile ROC records to independent native line series
#' @inheritParams roc_data
#' @return An [EChartsOption] with full legend and an independent chance line.
#' @keywords internal
#' @noRd
roc_option <- new_generic("roc_option", "config")
method(roc_option, ROCConfig) <- function(config, data) {
  diagonal <- if (config@diagonal) {
    list(LineSeries(
      data = list(c(0, 0), c(1, 1)),
      show_symbol = FALSE,
      silent = TRUE,
      legend_hover_link = FALSE,
      line_style = LineStyle(
        color = config@diagonal_color,
        type = "dashed",
        width = 1
      ),
      z = 1
    ))
  }
  curve_option(config, roc_data(config, data), before = diagonal)
}
