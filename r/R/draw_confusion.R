# draw_confusion.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Normalize confusion inputs to portable frequency records
#'
#' Retains missing-label frequencies so omission is visible after serialization.
#' Matrix dimnames carry class identity; columns are matched by name, never by
#' an assumed diagonal position. No model dependency is needed for data inputs.
#'
#' @param x Matrix, table, data frame, list of matrices, or vector: Counts or labels.
#' @param y Optional vector: Predicted labels paired with reference labels in x.
#' @param classes Optional Character vector: Explicit class order.
#' @return Data frame with reference, predicted, n, and optionally panel columns.
#' @export
#' @examples
#' confusion_input(factor(c("yes", "no")), factor(c("yes", "yes")))
confusion_input <- new_generic(
  "confusion_input",
  "x",
  function(x, y = NULL, classes = NULL) S7_dispatch()
)
method(confusion_input, class_any) <- function(x, y = NULL, classes = NULL) {
  if (!is.null(y)) {
    values <- list(x, y)
    for (i in seq_along(values)) {
      v <- values[[i]]
      if (is.data.frame(v) && ncol(v) == 1L) {
        v <- v[[1L]]
      }
      if (is.matrix(v) && ncol(v) == 1L) {
        v <- v[, 1L]
      }
      if (
        !(is.factor(v) || is.character(v) || is.numeric(v) || is.logical(v)) ||
          is.complex(v) ||
          !is.null(dim(v)) ||
          !length(v) ||
          (is.numeric(v) && any(is.infinite(v)))
      ) {
        abort(
          "Supply nonempty label vectors, or single-column label data.",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      values[[i]] <- v
    }
    if (length(values[[1L]]) != length(values[[2L]])) {
      abort(
        "Supply equally sized reference and predicted label vectors.",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    labels <- classes %||%
      unique(c(
        levels(values[[1L]]),
        levels(values[[2L]]),
        as.character(values[[1L]]),
        as.character(values[[2L]])
      ))
    labels <- labels[!is.na(labels)]
    observed <- unique(c(
      as.character(values[[1L]]),
      as.character(values[[2L]])
    ))
    if (
      !length(labels) ||
        any(!nzchar(labels)) ||
        anyDuplicated(labels) ||
        any(!observed[!is.na(observed)] %in% labels)
    ) {
      abort(
        "Supply unique nonempty `classes` covering every observed label.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    # Missing pairs remain frequency records with missing labels. They are
    # excluded from rates by the compiler, not silently lost in this adapter.
    counts <- table(
      factor(values[[1L]], levels = labels),
      factor(values[[2L]], levels = labels),
      useNA = "ifany"
    )
    out <- as.data.frame(counts, stringsAsFactors = FALSE)
    names(out) <- c("reference", "predicted", "n")
    out[["reference"]] <- as.character(out[["reference"]])
    out[["predicted"]] <- as.character(out[["predicted"]])
    return(out)
  }
  if (is.data.frame(x)) {
    if (!all(c("reference", "predicted", "n") %in% names(x))) {
      abort(
        "Supply frequency columns `reference`, `predicted`, and `n`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(x)
  }
  if (is.matrix(x) || inherits(x, "table")) {
    if (
      length(dim(x)) != 2L ||
        nrow(x) != ncol(x) ||
        nrow(x) == 0L ||
        !is.numeric(x) ||
        is.complex(x)
    ) {
      abort(
        "Supply a nonempty square numeric confusion matrix.",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    rows <- rownames(x)
    cols <- colnames(x)
    if (is.null(rows) && is.null(cols)) {
      rows <- cols <- classes %||% paste("Class", seq_len(nrow(x)))
    }
    if (
      length(rows) != nrow(x) ||
        length(cols) != ncol(x) ||
        anyNA(c(rows, cols)) ||
        any(!nzchar(c(rows, cols))) ||
        anyDuplicated(rows) ||
        anyDuplicated(cols) ||
        !setequal(rows, cols)
    ) {
      abort(
        "Give both matrix dimensions unique matching class names.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    # Reference varies fastest, matching rtemis/live confusion_long records.
    return(data.frame(
      reference = rep(rows, times = length(cols)),
      predicted = rep(cols, each = length(rows)),
      n = as.numeric(x),
      stringsAsFactors = FALSE
    ))
  }
  if (
    is.list(x) &&
      length(x) &&
      !is.null(names(x)) &&
      !anyNA(names(x)) &&
      all(nzchar(names(x))) &&
      !anyDuplicated(names(x))
  ) {
    parts <- lapply(seq_along(x), function(i) {
      if (!is.matrix(x[[i]]) && !inherits(x[[i]], "table")) {
        abort(
          "Supply a named list of confusion matrices for multiple panels.",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      part <- confusion_input(x[[i]], classes = classes)
      part[["panel"]] <- names(x)[[i]]
      part
    })
    return(do.call(rbind, parts))
  }
  abort(
    "Supply a confusion matrix, frequency records, or both label vectors.",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
}


#' Compute confusion summaries from bound frequency records
#'
#' spec: draw/first-cran-release#classification-confusion-views
#' Count records aggregate by class identity. An absent combination is zero;
#' missing labels contribute only to the omitted-pair count. All rates use the
#' complete declared class set, with explicit undefined denominators.
#'
#' @param config [ConfusionConfig]: Resolved configuration.
#' @param data Data frame or named list: Bound frequency columns.
#' @return List with shared classes and per-panel count matrices and rates.
#' @keywords internal
#' @noRd
confusion_data <- new_generic("confusion_data", "config")
method(confusion_data, ConfusionConfig) <- function(config, data) {
  reference <- config_column(data, config@reference, "reference")
  predicted <- config_column(data, config@predicted, "predicted")
  n <- config_column(data, config@count, "count")
  for (v in list(reference, predicted)) {
    if (
      !(is.character(v) || is.factor(v)) ||
        !is.null(dim(v)) ||
        any(!nzchar(as.character(v[!is.na(v)])))
    ) {
      abort(
        "Use nonempty character class labels, with missing values for omitted pairs.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  reference <- as.character(reference)
  predicted <- as.character(predicted)
  if (
    !is.numeric(n) ||
      is.complex(n) ||
      !is.null(dim(n)) ||
      !length(n) ||
      any(!is.finite(n)) ||
      any(n < 0 | n != floor(n)) ||
      sum(n) > 2^53 - 1 ||
      length(reference) != length(n) ||
      length(predicted) != length(n)
  ) {
    abort(
      "Supply aligned, finite nonnegative integer counts with total at most 2^53 - 1.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  panel <- config_column(data, config@panel, "panel")
  if (is.null(panel)) {
    panel <- rep("Confusion matrix", length(n))
  }
  if (is.factor(panel)) {
    panel <- as.character(panel)
  }
  if (
    !is.character(panel) ||
      !is.null(dim(panel)) ||
      length(panel) != length(n) ||
      anyNA(panel) ||
      any(!nzchar(panel))
  ) {
    abort(
      "Supply one nonempty panel label per frequency record.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  classes <- config@classes %||% unique(c(reference, predicted))
  classes <- classes[!is.na(classes)]
  known <- c(reference, predicted)
  if (!length(classes) || any(!known[!is.na(known)] %in% classes)) {
    abort(
      "Set `classes` to include every reference and predicted class.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  k <- length(classes)
  # Explicit division keeps zero-support rates unknown in R and JSON labels.
  ratio <- function(a, b) ifelse(b > 0, a / b, NA_real_)
  panels <- lapply(unique(panel), function(name) {
    at <- which(panel == name)
    keep <- !is.na(reference[at]) & !is.na(predicted[at])
    valid <- at[keep]
    counts <- matrix(0, k, k, dimnames = list(classes, classes))
    if (length(valid)) {
      index <- match(reference[valid], classes) +
        k * (match(predicted[valid], classes) - 1L)
      sums <- rowsum(n[valid], index, reorder = FALSE)
      counts[as.integer(rownames(sums))] <- sums[, 1L]
    }
    total <- sum(counts)
    actual <- rowSums(counts)
    predicted_total <- colSums(counts)
    hits <- diag(counts)
    tn <- total - actual - predicted_total + hits
    recall <- ratio(hits, actual)
    list(
      name = name,
      counts = counts,
      total = total,
      omitted = sum(n[at[!keep]]),
      fraction = counts / ifelse(actual > 0, actual, NA_real_),
      sensitivity = recall,
      specificity = ratio(tn, total - actual),
      ppv = ratio(hits, predicted_total),
      npv = ratio(tn, total - predicted_total),
      accuracy = ratio(sum(hits), total),
      balanced_accuracy = mean(recall)
    )
  })
  list(classes = classes, panels = panels)
}


#' Build the request of a confusion chart
#'
#' Counts every panel's frequency records (`confusion_data()`) and hands the
#' class order and the count matrices to `@rtemis/draw`'s confusion builder,
#' which computes the rates and draws the matrix and its metric strips; the
#' widget and SVG export fit the panels to the canvas (`fitConfusion()`).
#' Cells fade toward the theme's sign colors unless the config sets colors.
#' spec: draw/shared-builders#confusion
#'
#' @inheritParams confusion_data
#' @param theme Optional [Theme]: The theme whose sign colors fill the cells.
#' @return [BuildRequest] of type `"confusion"`.
#' @keywords internal
#' @noRd
confusion_option <- new_generic("confusion_option", "config")
method(confusion_option, ConfusionConfig) <- function(
  config,
  data,
  theme = NULL
) {
  prepared <- confusion_data(config, data)
  for (p in prepared[["panels"]]) {
    if (p[["omitted"]] > 0) {
      msg(
        p[["name"]],
        ": ",
        p[["omitted"]],
        " missing pair(s) omitted",
        sep = ""
      )
    }
  }
  sign <- theme_sign_colors(theme)
  document <- list(
    classes = I(prepared[["classes"]]),
    panels = lapply(prepared[["panels"]], function(p) {
      list(
        name = jsonlite::unbox(p[["name"]]),
        counts = lapply(seq_len(nrow(p[["counts"]])), function(r) {
          I(unname(p[["counts"]][r, ]))
        })
      )
    })
  )
  BuildRequest(
    type = "confusion",
    data = as.character(jsonlite::toJSON(
      document,
      auto_unbox = FALSE,
      digits = NA
    )),
    config = config_members(config),
    appearance = list(
      signColors = list(
        negative = sign[["negative"]],
        positive = sign[["positive"]]
      )
    )
  )
}


#' Render confusion panels at the container width
#'
#' The browser derives the height from the width and wraps panels on narrow
#' hosts. SVG export has no container and uses 560 pixels per column.
#'
#' @inheritParams draw
#' @param data Optional Data frame: Frequency records, or read from dat_path.
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
method(draw, ConfusionConfig) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL
) {
  built <- compile(option, data = data, theme = theme)
  panels <- length(jsonlite::parse_json(built@data)[["panels"]])
  cols <- min(option@ncol, panels)
  rows <- ceiling(panels / cols)
  height <- height %||% (rows * 500 + if (is.null(option@title)) 0 else 40)
  draw(
    built,
    theme = theme,
    width = width %||% "100%",
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = c(
      render_meta(option, built),
      list(exportSize = list(width = cols * 560, height = height))
    ),
    ...
  )
}


#' Draw Confusion Counts and Classification Summaries
#'
#' Accepts a square count matrix, a named list of count matrices, long frequency
#' records (`reference`, `predicted`, `n`, optional `panel`), or paired reference
#' and predicted label vectors. Model packages can add support for metrics
#' objects through [confusion_input()]. Matrix columns are aligned by label identity;
#' their physical order need not match rows. All panels share one class order.
#' @inheritSection ConfusionConfig Statistical semantics
#' @param x Matrix, table, data frame, named list, metrics object, or vector: Counts or reference labels.
#' @param y Optional vector: Predicted labels when x contains reference labels.
#' @inheritParams ConfusionConfig
#' @param ... Additional named settings for [setup_ConfusionConfig()].
#' @param theme Optional [Theme]: Chart theme. Color fades and marginal
#'   backgrounds follow the active theme unless overridden in the config.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget containing every requested panel.
#' @export
#' @examples
#' draw_confusion(factor(c("yes", "yes", "no")), c("yes", "no", "no"))
#' draw_confusion(matrix(c(8, 2, 1, 9), 2,
#'   dimnames = list(c("yes", "no"), c("yes", "no"))))
draw_confusion <- function(
  x,
  y = NULL,
  classes = NULL,
  ...,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  data <- confusion_input(x, y, classes)
  config <- setup_ConfusionConfig(
    classes = classes,
    panel = if ("panel" %in% names(data)) "panel" else NULL,
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
