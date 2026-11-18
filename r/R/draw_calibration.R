# draw_calibration.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Normalize binary calibration inputs into portable observation records
#' @param x Vector, list, or data frame: Binary labels or observation records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Probabilities.
#' @param positive Optional Character: Positive class label.
#' @return Data frame containing observed, probability, and group columns.
#' @keywords internal
#' @noRd
calibration_input <- new_generic("calibration_input", "x")
method(calibration_input, class_any) <- function(
  x,
  predicted_prob = NULL,
  positive = NULL
) {
  if (is.data.frame(x) && is.null(predicted_prob)) {
    if (!is.null(positive)) {
      abort(
        "Omit `positive` for observation records already coded as zero or one.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(x)
  }
  sets <- is.list(x) && !is.data.frame(x)
  if (
    is.null(predicted_prob) ||
      sets != (is.list(predicted_prob) && !is.data.frame(predicted_prob))
  ) {
    abort(
      "Supply paired binary labels and probabilities, matching lists, or an observation table.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  labels <- if (sets) x else list(x)
  probs <- if (sets) predicted_prob else list(predicted_prob)
  if (!length(labels) || length(labels) != length(probs)) {
    abort(
      "Supply the same nonzero number of label and probability sets.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  for (nm in list(names(labels), names(probs))) {
    if (!is.null(nm) && (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))) {
      abort(
        "Use unique nonempty names for calibration input sets.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (!is.null(names(labels)) && !is.null(names(probs))) {
    if (!setequal(names(labels), names(probs))) {
      abort(
        "Label and probability set names must match.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    probs <- probs[names(labels)]
  }
  groups <- names(labels) %||%
    names(probs) %||%
    if (length(labels) == 1L) "Sample" else paste("Sample", seq_along(labels))
  normalized <- lapply(seq_along(labels), function(i) {
    # Share ROC's class-identity and probability-range validation, including
    # complementing a named negative-class score column when needed.
    sample <- roc_probabilities(labels[[i]], probs[[i]], positive)
    if (length(sample[["levels"]]) != 2L) {
      abort(
        "Supply two class levels for binary calibration.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    sample
  })
  selected <- vapply(
    normalized,
    function(sample) sample[["classes"]][[1L]],
    character(1)
  )
  if (length(unique(selected)) != 1L) {
    abort(
      "Use the same positive class in every sample; set `positive` explicitly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  pieces <- lapply(seq_along(normalized), function(i) {
    sample <- normalized[[i]]
    data.frame(
      observed = as.integer(sample[["y"]] == selected[[i]]),
      probability = sample[["prob"]][, selected[[i]]],
      group = groups[[i]]
    )
  })
  do.call(rbind, pieces)
}

#' Validate calibration records and compute bins and unbinned scores
#' @param config [CalibrationConfig]: Bindings and statistical settings.
#' @param data Data frame or named list: Individual binary observation records.
#' @return List of groups with bin statistics, complete probabilities, and Brier scores.
#' @keywords internal
#' @noRd
calibration_data <- new_generic("calibration_data", "config")
method(calibration_data, CalibrationConfig) <- function(config, data) {
  observed <- config_column(data, config@observed, "observed")
  probability <- config_column(data, config@probability, "probability")
  group <- config_column(data, config@group, "group") %||%
    rep("Sample", length(observed))
  if (
    !length(observed) ||
      length(probability) != length(observed) ||
      length(group) != length(observed)
  ) {
    abort(
      "Supply nonempty, equally sized outcome, probability, and group columns.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (
    !(is.numeric(observed) || is.logical(observed)) ||
      is.complex(observed) ||
      !is.null(dim(observed)) ||
      any(!is.na(observed) & !observed %in% c(0, 1))
  ) {
    abort(
      "Code observed outcomes as zero or one, with NA for missing outcomes.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !is.numeric(probability) ||
      is.complex(probability) ||
      !is.null(dim(probability)) ||
      any(!is.finite(probability[!is.na(probability)])) ||
      any(probability < 0 | probability > 1, na.rm = TRUE)
  ) {
    abort(
      "Supply numeric probabilities in [0, 1], with NA for unavailable scores.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !(is.atomic(group) || is.factor(group)) ||
      !is.null(dim(group)) ||
      anyNA(group) ||
      any(!nzchar(as.character(group)))
  ) {
    abort(
      "Supply a nonmissing, nonempty sample or model label for every row.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  labels <- group_levels(group)
  group <- as.character(group)
  lapply(labels, function(label) {
    index <- which(group == label)
    keep <- !is.na(observed[index]) & !is.na(probability[index])
    if ((!config@na_rm && any(!keep)) || !any(keep)) {
      abort(
        "Supply complete observation/probability pairs for ",
        label,
        "; use `na_rm = TRUE` only when complete pairs remain.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    y <- as.numeric(observed[index][keep])
    p <- probability[index][keep]
    breaks <- if (config@bin_method == "equidistant") {
      seq(0, 1, length.out = config@n_bins + 1L)
    } else {
      unique(as.numeric(stats::quantile(
        p,
        seq(0, 1, length.out = config@n_bins + 1L),
        type = 7L
      )))
    }
    # Closed final intervals include p = 1 and the sample maximum. Collapsing
    # boundaries preserves ties; constant scores are a single occupied bin.
    bin <- if (length(breaks) == 1L) {
      rep(1L, length(p))
    } else {
      findInterval(p, breaks, rightmost.closed = TRUE, all.inside = TRUE)
    }
    bins <- do.call(
      rbind,
      lapply(sort(unique(bin)), function(i) {
        ids <- which(bin == i)
        data.frame(
          probability = mean(p[ids]),
          observed = mean(y[ids]),
          n = length(ids),
          lower = breaks[[i]],
          upper = breaks[[min(i + 1L, length(breaks))]]
        )
      })
    )
    list(
      name = label,
      bins = bins,
      probability = p,
      brier = mean((p - y)^2),
      omitted = sum(!keep)
    )
  })
}

#' The calibration builder's data as JSON
#'
#' @param groups List: The groups of `calibration_data()`.
#'
#' @return Character: JSON text of `{groups}`, each group's name, bins (one
#'   array per column), probabilities, Brier score, and omitted pairs.
#'
#' @author EDG
#' @keywords internal
#' @noRd
calibration_json <- function(groups) {
  as.character(jsonlite::toJSON(
    list(
      groups = lapply(groups, function(g) {
        list(
          name = jsonlite::unbox(g[["name"]]),
          bins = as.list(g[["bins"]]),
          probability = g[["probability"]],
          brier = jsonlite::unbox(g[["brier"]]),
          omitted = jsonlite::unbox(g[["omitted"]])
        )
      })
    ),
    auto_unbox = FALSE,
    digits = NA
  ))
}

#' Draw Binary Probability Calibration
#'
#' Compare mean predicted probability with observed frequency. Accepts binary
#' labels and probabilities, named lists of paired samples, or individual
#' observation records with `observed`, `probability`, and optional `group`.
#' Named lists are aligned by name. Binary class identity and probability
#' columns follow [draw_roc()]: `positive` names the event, and otherwise the
#' second factor level is selected. Named probability columns identify their
#' classes. Samples must share the same positive class.
#' @inheritSection CalibrationConfig Statistical semantics
#' @param true_labels Factor, vector, list, or data frame: Binary labels or observation records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Predicted probabilities.
#' @param positive Optional Character: Positive class label.
#' @param ... Additional named settings for [setup_CalibrationConfig()].
#' @param group Optional Character: Group column for observation records. When
#'   omitted, a column named `group` is used if present; explicit NULL pools rows.
#'   Curves follow factor level order, or first appearance for other columns.
#' @inheritParams draw_roc
#' @return An ECharts htmlwidget with vector SVG export.
#' @export
#' @examples
#' # Synthetic predictions from a model that is overconfident at the extremes:
#' # 20 cases at each of ten predicted probabilities, with observed rates pulled
#' # toward one half.
#' probability <- rep(seq(0.05, 0.95, by = 0.1), each = 20)
#' rate <- 0.2 + 0.6 * probability
#' observed <- as.integer(rep(seq_len(20), 10) <= round(20 * rate))
#' labels <- factor(c("no", "yes")[observed + 1L], levels = c("no", "yes"))
#' draw_calibration(labels, probability, n_bins = 10L)
draw_calibration <- function(
  true_labels,
  predicted_prob = NULL,
  positive = NULL,
  ...,
  group = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  data <- calibration_input(true_labels, predicted_prob, positive)
  if (missing(group) && "group" %in% names(data)) {
    group <- "group"
  }
  config <- setup_CalibrationConfig(
    group = group,
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
