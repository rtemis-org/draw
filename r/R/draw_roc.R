# draw_roc.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Normalize labels and probability columns by class identity
#' @param x Factor or atomic vector: Reference labels.
#' @param predicted_prob Numeric vector or matrix: Class probabilities.
#' @param positive Optional Character: Binary positive class.
#' @return List containing reference labels, class levels, selected classes, and scores.
#' @keywords internal
#' @noRd
roc_probabilities <- new_generic("roc_probabilities", "x")
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
#' spec: draw/first-cran-release#roc-views
#' Higher scores always indicate the named positive class. Consuming a tied
#' block at once yields trapezoidal AUC equivalent to half credit for ties.
#' The direction is never selected from the observed performance.
#' @param x List: Normalized labels and probabilities from roc_probabilities().
#' @return Data frame of class, FPR, TPR, AUC, and omitted-pair counts.
#' @keywords internal
#' @noRd
roc_vertices <- new_generic("roc_vertices", "x")
method(roc_vertices, class_list) <- function(x) {
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
  if (is.null(predicted_prob) && is.data.frame(x)) {
    if (!is.null(positive)) {
      abort(
        "Omit `positive` for already computed curve records.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(x)
  }
  if (is.null(predicted_prob)) {
    abort(
      "Supply probabilities with labels, or a table containing fpr and tpr.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  sets <- is.list(x) && !is.data.frame(x)
  if (sets != (is.list(predicted_prob) && !is.data.frame(predicted_prob))) {
    abort(
      "Supply matching lists of labels and probabilities, or one pair of vectors/matrices.",
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
        "Use unique nonempty names for ROC input sets.",
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
  names_out <- names(labels) %||%
    names(probs) %||%
    if (length(labels) == 1L) "Sample" else paste("Sample", seq_along(labels))
  pieces <- lapply(seq_along(labels), function(i) {
    out <- roc_vertices(roc_probabilities(labels[[i]], probs[[i]], positive))
    out[["split"]] <- names_out[[i]]
    out[["fold"]] <- "aggregate"
    out
  })
  do.call(rbind, pieces)
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
#' Set `legend_position` to `"bottom-right"`, `"top-right"`, `"top-left"`, or
#' `"bottom-left"` through `...`. The lower-right default usually leaves the
#' upper-left region occupied by informative ROC curves unobstructed.
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
draw_roc <- function(
  true_labels,
  predicted_prob = NULL,
  positive = NULL,
  ...,
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

#' Validate and partition portable ROC records
#' @param config [ROCConfig]: Bound configuration.
#' @param data Data frame or named list: Long curve records.
#' @return List containing selected curves, groups, and disclosure counts.
#' @keywords internal
#' @noRd
roc_data <- new_generic("roc_data", "config")
method(roc_data, ROCConfig) <- function(config, data) {
  fpr <- config_column(data, config@fpr, "fpr")
  tpr <- config_column(data, config@tpr, "tpr")
  n <- length(fpr)
  for (v in list(fpr, tpr)) {
    if (
      !is.numeric(v) ||
        is.complex(v) ||
        !is.null(dim(v)) ||
        !n ||
        length(v) != n ||
        any(!is.finite(v[!is.na(v)])) ||
        any(v < 0 | v > 1, na.rm = TRUE)
    ) {
      abort(
        "Supply aligned numeric FPR and TPR values in [0, 1].",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  labels <- function(binding, default, argument) {
    v <- config_column(data, binding, argument) %||% rep(default, n)
    if (is.factor(v)) {
      v <- as.character(v)
    }
    if (
      !is.character(v) ||
        !is.null(dim(v)) ||
        length(v) != n ||
        anyNA(v) ||
        any(!nzchar(v))
    ) {
      abort(
        "Use one nonempty ",
        argument,
        " label per ROC record.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    v
  }
  split <- labels(config@split, "Sample", "split")
  cls <- labels(config@class_label, "ROC", "class")
  fold <- labels(config@fold, "aggregate", "fold")
  auc <- config_column(data, config@auc, "auc")
  omitted <- config_column(data, config@omitted, "omitted") %||% rep(0, n)
  if (
    !is.null(auc) &&
      (!is.numeric(auc) ||
        is.complex(auc) ||
        !is.null(dim(auc)) ||
        length(auc) != n ||
        any(!is.finite(auc[!is.na(auc)])) ||
        any(auc < 0 | auc > 1, na.rm = TRUE))
  ) {
    abort(
      "Supply aligned AUC values in [0, 1], or NA for unknown AUC.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !is.numeric(omitted) ||
      is.complex(omitted) ||
      !is.null(dim(omitted)) ||
      length(omitted) != n ||
      any(!is.finite(omitted)) ||
      any(omitted < 0 | omitted != floor(omitted))
  ) {
    abort(
      "Supply nonnegative integer omitted-pair counts.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  selected <- if (config@variant == "aggregate") {
    fold == "aggregate"
  } else {
    fold != "aggregate"
  }
  if (!any(selected)) {
    abort(
      "No curves match `variant`; supply aggregate rows or per-resample fold labels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keys <- unique(data.frame(
    split = split[selected],
    class = cls[selected],
    stringsAsFactors = FALSE
  ))
  curves <- groups <- list()
  undefined <- max_omitted <- 0L
  for (g in seq_len(nrow(keys))) {
    rows <- which(
      selected & split == keys[["split"]][[g]] & cls == keys[["class"]][[g]]
    )
    folds <- unique(fold[rows])
    scores <- numeric()
    for (id in folds) {
      at <- rows[fold[rows] == id]
      area <- if (is.null(auc)) NA_real_ else unique(auc[at])
      missing_n <- unique(omitted[at])
      if (length(area) != 1L || length(missing_n) != 1L) {
        abort(
          "Keep AUC and omitted counts constant within each split/class/fold curve.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      max_omitted <- max(max_omitted, missing_n)
      if (all(is.na(fpr[at])) && all(is.na(tpr[at])) && is.na(area)) {
        undefined <- undefined + 1L
        next
      }
      if (anyNA(fpr[at]) || anyNA(tpr[at])) {
        abort(
          "Represent an undefined curve with all-missing coordinates; complete curves cannot contain gaps.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      # Sorting both coordinates preserves vertical tie runs, including input
      # from libraries that enumerate thresholds in the reverse direction.
      at <- at[order(fpr[at], tpr[at])]
      xx <- fpr[at]
      yy <- tpr[at]
      if (
        length(at) < 2L ||
          xx[[1L]] != 0 ||
          yy[[1L]] != 0 ||
          tail(xx, 1L) != 1 ||
          tail(yy, 1L) != 1 ||
          any(diff(yy) < 0)
      ) {
        abort(
          "Supply monotone complete ROC curves from (0, 0) to (1, 1).",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      if (is.null(auc)) {
        area <- sum(diff(xx) * (head(yy, -1L) + tail(yy, -1L)) / 2)
      }
      scores <- c(scores, area)
      curves[[length(curves) + 1L]] <- list(
        group = g,
        fold = id,
        fpr = xx,
        tpr = yy,
        auc = area,
        omitted = missing_n
      )
    }
    values <- scores[!is.na(scores)]
    groups[[g]] <- list(
      split = keys[["split"]][[g]],
      class = keys[["class"]][[g]],
      mean = if (length(values)) mean(values) else NA_real_,
      sd = if (length(values) > 1L) stats::sd(values) else NA_real_,
      available = length(values),
      total = length(folds)
    )
  }
  if (!length(curves)) {
    abort(
      "No defined ROC curve: each curve needs both positive and negative observations.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  list(
    curves = curves,
    groups = groups,
    undefined = undefined,
    max_omitted = max_omitted
  )
}

#' Compile ROC records to independent native line series
#' @inheritParams roc_data
#' @return An [EChartsOption] with full legend and an independent chance line.
#' @keywords internal
#' @noRd
roc_option <- new_generic("roc_option", "config")
method(roc_option, ROCConfig) <- function(config, data) {
  prepared <- roc_data(config, data)
  curves <- prepared[["curves"]]
  groups <- prepared[["groups"]]
  per_fold <- config@variant == "per_resample"
  fmt <- function(x) {
    if (is.na(x)) {
      "NA"
    } else {
      formatC(x, format = "f", digits = config@digits, decimal.mark = ".")
    }
  }
  classes <- unique(vapply(groups, `[[`, character(1), "class"))
  splits <- unique(vapply(groups, `[[`, character(1), "split"))
  # Omit the generic single-sample prefix, keeping explicit sample/model names
  # and class identity. Shared native names toggle all folds in a group.
  labels <- vapply(
    groups,
    function(g) {
      paste0(
        if (!identical(splits, "Sample")) paste0(g[["split"]], ": "),
        g[["class"]],
        if (per_fold) {
          paste0(
            " (AUC mean ",
            fmt(g[["mean"]]),
            "; SD ",
            fmt(g[["sd"]]),
            " [",
            g[["available"]],
            "/",
            g[["total"]],
            "])"
          )
        } else {
          paste0(" (AUC ", fmt(g[["mean"]]), ")")
        }
      )
    },
    character(1)
  )
  labels <- make.unique(labels)
  active <- unique(vapply(curves, `[[`, integer(1), "group"))
  legend_names <- labels[active]
  # Inset legends do not consume plotting width or add rows beneath the axes.
  bottom <- 65
  disclosure <- c(
    if (prepared[["undefined"]]) {
      paste(prepared[["undefined"]], "undefined curve(s) omitted")
    },
    if (prepared[["max_omitted"]]) {
      paste0(
        "Missing pairs excluded:\nmax ",
        prepared[["max_omitted"]],
        " per curve"
      )
    }
  )
  top <- 25 + if (!is.null(config@title)) 25 else 0
  if (length(disclosure)) {
    top <- top +
      20 +
      18 * sum(lengths(strsplit(disclosure, "\n", fixed = TRUE)))
  }
  grid <- to_list(Grid(
    left = 70,
    right = 24,
    top = top,
    bottom = bottom,
    contain_label = FALSE
  ))
  grid[["outerBoundsMode"]] <- "none"
  series <- list()
  if (config@diagonal) {
    series[[1L]] <- LineSeries(
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
    )
  }
  for (curve in curves) {
    id <- curve[["group"]]
    g <- groups[[id]]
    color_id <- if (length(classes) > 1L) {
      match(g[["class"]], classes)
    } else {
      match(g[["split"]], splits)
    }
    color <- if (!is.null(config@palette)) {
      config@palette[[(color_id - 1L) %% length(config@palette) + 1L]]
    } else {
      NULL
    }
    points <- lapply(seq_along(curve[["fpr"]]), function(j) {
      list(
        unname(curve[["fpr"]][[j]]),
        unname(curve[["tpr"]][[j]]),
        if (is.na(curve[["auc"]])) NULL else curve[["auc"]],
        curve[["fold"]],
        curve[["omitted"]],
        fmt(curve[["fpr"]][[j]]),
        fmt(curve[["tpr"]][[j]]),
        fmt(curve[["auc"]])
      )
    })
    s <- to_list(LineSeries(
      name = labels[[id]],
      data = points,
      smooth = FALSE,
      step = FALSE,
      show_symbol = TRUE,
      symbol = "circle",
      symbol_size = 8,
      connect_nulls = FALSE,
      clip = TRUE,
      line_style = LineStyle(
        color = color,
        width = config@line_width,
        opacity = if (per_fold) config@fold_opacity else 1,
        type = c("solid", "dashed", "dotted")[[
          (match(g[["split"]], splits) - 1L) %% 3L + 1L
        ]]
      ),
      item_style = ItemStyle(color = color, opacity = 0),
      z = 3
    ))
    # Native transparent symbols provide point hit targets for item tooltips.
    # Only the hovered point becomes visible; unhovered charts and SVG retain
    # the line-only appearance. LineSeriesOption inherits emphasis from
    # SeriesOption (ECharts util/types.ts); no browser-only handler is required.
    s[["emphasis"]] <- list(itemStyle = list(opacity = 1))
    s[["dimensions"]] <- as.list(c(
      "FPR",
      "TPR",
      "AUC",
      "Resample",
      "Missing pairs"
    ))
    # DimensionDefinition (ECharts util/types.ts) separates display text from
    # full-precision values. Ordinal labels survive JSON without callbacks.
    # spec: draw/first-cran-release#roc-views
    s[["dimensions"]] <- c(
      s[["dimensions"]],
      lapply(c("FPR", "TPR", "AUC"), function(label) {
        list(
          name = paste(label, "label"),
          displayName = label,
          type = "ordinal"
        )
      })
    )
    s[["encode"]] <- list(
      x = 0L,
      y = 1L,
      tooltip = as.list(c(5L, 6L, 7L, 3L, 4L))
    )
    series[[length(series) + 1L]] <- s
  }
  EChartsOption(
    title = Title(
      text = config@title,
      subtext = if (length(disclosure)) paste(disclosure, collapse = "\n"),
      left = "center",
      text_align = "center"
    ),
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
      orient = "vertical",
      # Initial native anchors also make the compiled option useful on its own.
      # Shared render geometry refines these after square-grid fitting/resizing.
      left = if (grepl("left$", config@legend_position)) 82,
      right = if (grepl("right$", config@legend_position)) 36,
      top = if (startsWith(config@legend_position, "top")) top + 12,
      bottom = if (startsWith(config@legend_position, "bottom")) bottom + 12,
      align = "left",
      padding = 0,
      item_gap = 6,
      icon = "roundRect",
      item_width = 20,
      item_height = 3,
      # Legend keys stay visible independently of transparent hover targets.
      item_style = ItemStyle(opacity = 1),
      data = as.list(legend_names),
      text_style = TextStyle(font_size = 12, line_height = 14)
    ),
    tooltip = Tooltip(trigger = "item", confine = TRUE),
    series = series
  )
}
