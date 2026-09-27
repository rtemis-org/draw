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
  group <- as.character(group)
  lapply(unique(group), function(label) {
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

#' Compile calibration observations into native lines, points, and rugs
#' @inheritParams calibration_data
#' @return An [EChartsOption] with fixed probability axes and native vector marks.
#' @keywords internal
#' @noRd
calibration_option <- new_generic("calibration_option", "config")
method(calibration_option, CalibrationConfig) <- function(config, data) {
  groups <- calibration_data(config, data)
  fmt <- function(x) {
    formatC(x, digits = config@digits, format = "f", decimal.mark = ".")
  }
  labels <- vapply(
    groups,
    function(g) {
      paste0(
        g[["name"]],
        if (config@show_brier) paste0(" (Brier ", fmt(g[["brier"]]), ")")
      )
    },
    character(1)
  )
  series <- list()
  if (config@diagonal) {
    series[[1L]] <- LineSeries(
      data = list(c(0, 0), c(1, 1)),
      show_symbol = FALSE,
      silent = TRUE,
      legend_hover_link = FALSE,
      # Background references must not consume a group palette color.
      item_style = ItemStyle(color = config@diagonal_color),
      line_style = LineStyle(
        color = config@diagonal_color,
        width = 1,
        type = "dashed"
      ),
      z = 1
    )
  }
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    bins <- g[["bins"]]
    color <- if (!is.null(config@palette)) {
      config@palette[[(i - 1L) %% length(config@palette) + 1L]]
    } else {
      NULL
    }
    # Ordinal display dimensions retain controlled tooltip precision without
    # changing the numeric geometry or sending executable formatter code.
    points <- lapply(seq_len(nrow(bins)), function(j) {
      list(
        bins[["probability"]][[j]],
        bins[["observed"]][[j]],
        bins[["n"]][[j]],
        fmt(bins[["probability"]][[j]]),
        fmt(bins[["observed"]][[j]]),
        fmt(bins[["lower"]][[j]]),
        fmt(bins[["upper"]][[j]]),
        fmt(g[["brier"]]),
        g[["omitted"]]
      )
    })
    curve <- to_list(LineSeries(
      name = labels[[i]],
      data = points,
      smooth = FALSE,
      show_symbol = TRUE,
      symbol = "circle",
      symbol_size = config@point_size,
      clip = FALSE,
      connect_nulls = FALSE,
      line_style = LineStyle(
        color = color,
        width = config@line_width,
        opacity = if (config@mode == "markers") 0 else 1
      ),
      item_style = ItemStyle(
        color = color,
        opacity = if (config@mode == "lines" && nrow(bins) > 1L) 0 else 1
      ),
      z = 3
    ))
    curve[["emphasis"]] <- list(itemStyle = list(opacity = 1))
    curve[["dimensions"]] <- c(
      as.list(c("probability", "observed", "Count")),
      lapply(
        c(
          "Mean probability",
          "Observed proportion",
          "Bin lower",
          "Bin upper",
          "Brier"
        ),
        function(label) {
          list(name = label, displayName = label, type = "ordinal")
        }
      ),
      list("Missing pairs")
    )
    curve[["encode"]] <- list(
      x = 0L,
      y = 1L,
      tooltip = as.list(c(3L, 4L, 2L, 5L, 6L, 7L, 8L))
    )
    series[[length(series) + 1L]] <- curve
    if (config@rug) {
      # Symbol offsets are pixels: ticks stay inside the bottom edge under
      # resize and SVG export, without extending the probability-axis range.
      rug <- to_list(ScatterSeries(
        name = labels[[i]],
        data = lapply(g[["probability"]], function(p) list(p, 0, fmt(p))),
        symbol = "rect",
        symbol_size = c(1, config@rug_size),
        symbol_offset = c(0, -config@rug_size / 2),
        clip = FALSE,
        item_style = ItemStyle(color = color, opacity = config@rug_opacity),
        z = 2
      ))
      rug[["dimensions"]] <- list(
        "probability",
        "baseline",
        list(name = "Probability", type = "ordinal")
      )
      rug[["encode"]] <- list(x = 0L, y = 1L, tooltip = list(2L))
      series[[length(series) + 1L]] <- rug
    }
  }
  grid <- to_list(Grid(
    left = 70,
    right = 24,
    top = if (is.null(config@title)) 25 else 50,
    bottom = 65,
    contain_label = FALSE
  ))
  grid[["outerBoundsMode"]] <- "none"
  EChartsOption(
    title = Title(text = config@title, left = "center", text_align = "center"),
    grid = grid,
    x_axis = Axis(
      type = "value",
      min = 0,
      max = 1,
      name = config@xlab,
      name_location = "middle",
      name_gap = 30
    ),
    y_axis = Axis(
      type = "value",
      min = 0,
      max = 1,
      name = config@ylab,
      name_location = "middle",
      name_gap = 45
    ),
    legend = Legend(
      show = config@legend,
      orient = "horizontal",
      padding = 0,
      data = as.list(labels),
      item_style = ItemStyle(opacity = 1),
      text_style = TextStyle(font_size = 12, line_height = 14)
    ),
    tooltip = Tooltip(trigger = "item", confine = TRUE),
    series = series
  )
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
#' @inheritParams draw_roc
#' @return An ECharts htmlwidget with vector SVG export.
#' @export
#' @examples
#' labels <- factor(c("no", "yes", "no", "yes"), levels = c("no", "yes"))
#' draw_calibration(labels, c(.1, .8, .4, .9), n_bins = 2L)
draw_calibration <- function(
  true_labels,
  predicted_prob = NULL,
  positive = NULL,
  ...,
  group = NULL,
  legend_position = "top",
  legend_placement = "outside",
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
