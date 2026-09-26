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
#' @keywords internal
#' @noRd
confusion_input <- new_generic("confusion_input", "x")
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


#' Compile confusion counts and marginal metrics to native ECharts layers
#'
#' Labels and colors are materialized so the SVG path requires no callbacks.
#' Separate continuous maps for correct/error cells follow the same row-fraction
#' convention as the web interface. Metric cells use a neutral third map.
#'
#' @inheritParams confusion_data
#' @return An [EChartsOption] containing the complete panel composition.
#' @keywords internal
#' @noRd
confusion_option <- new_generic("confusion_option", "config")
method(confusion_option, ConfusionConfig) <- function(config, data) {
  prepared <- confusion_data(config, data)
  classes <- prepared[["classes"]]
  panels <- prepared[["panels"]]
  k <- length(classes)
  cols <- min(config@ncol, length(panels))
  rows <- ceiling(length(panels) / cols)
  series <- maps <- grids <- xs <- ys <- titles <- list()
  if (!is.null(config@title)) {
    titles <- list(Title(text = config@title, left = "center", top = 0L))
  }
  # A compiled native option has a usable light fallback. The shared browser/
  # SVG layout resolves nullable colors against the actual render theme.
  low_color <- config@low_color %||% "#FFFFFF"
  summary_color <- config@summary_color %||% "#EFEFEF"
  # Use linear sRGB luminance when choosing black or white count text.
  contrast <- function(color) {
    rgb <- as.numeric(grDevices::col2rgb(color)) / 255
    linear <- ifelse(rgb <= .04045, rgb / 12.92, ((rgb + .055) / 1.055)^2.4)
    if (sum(linear * c(.2126, .7152, .0722)) > .179) "#000000" else "#FFFFFF"
  }
  format_rate <- function(value) {
    if (is.na(value)) {
      "NA"
    } else {
      formatC(value, format = "f", digits = config@digits, decimal.mark = ".")
    }
  }
  cell_label <- function(text, color) {
    to_list(LabelOption(
      show = TRUE,
      position = "inside",
      formatter = text,
      text_style = TextStyle(
        font_size = config@font_size,
        color = contrast(color)
      )
    ))
  }
  top_offset <- if (is.null(config@title)) 0 else 7
  for (i in seq_along(panels)) {
    p <- panels[[i]]
    left <- 100 * ((i - 1L) %% cols) / cols
    top <- top_offset + (100 - top_offset) * ((i - 1L) %/% cols) / rows
    pw <- 100 / cols
    ph <- (100 - top_offset) / rows
    titles[[length(titles) + 1L]] <- Title(
      text = if (length(panels) > 1L || !is.null(config@panel)) {
        p[["name"]]
      } else {
        NULL
      },
      text_style = TextStyle(font_size = config@font_size + 2),
      left = paste0(left + pw / 2, "%"),
      top = paste0(top, "%"),
      text_align = "center"
    )
    if (p[["omitted"]] > 0) {
      msg(
        p[["name"]],
        ": ",
        p[["omitted"]],
        " missing pair(s) omitted",
        sep = ""
      )
    }
    base <- length(grids)
    # Count matrix, narrow right/bottom metric strips, and overall summary.
    # Percentages provide a native fallback; the shared renderer fits square
    # count cells and aligned metric strips to the measured canvas.
    count_width <- if (config@show_metrics) .56 else .76
    count_height <- if (config@show_metrics) .60 else .74
    layouts <- list(c(.18, .20, count_width, count_height))
    xlabels <- list(classes)
    ylabels <- list(classes)
    if (config@show_metrics) {
      layouts <- c(
        layouts,
        list(
          c(.75, .20, .22, .60),
          c(.18, .82, .56, .15),
          c(.75, .82, .22, .15)
        )
      )
      xlabels <- c(
        xlabels,
        list(c("Sens.", "Spec."), classes, "Summary")
      )
      ylabels <- c(ylabels, list(classes, c("PPV", "NPV"), c("Accuracy", "BA")))
    }
    for (j in seq_along(layouts)) {
      box <- layouts[[j]]
      index <- base + j
      # Disable outer-bound shrinking: every strip must retain the exact same
      # category geometry as the matrix it annotates. Margins reserve labels.
      grid <- to_list(Grid(
        left = paste0(left + pw * box[[1L]], "%"),
        top = paste0(top + ph * box[[2L]], "%"),
        width = paste0(pw * box[[3L]], "%"),
        height = paste0(ph * box[[4L]], "%"),
        contain_label = FALSE
      ))
      grid[["outerBoundsMode"]] <- "none"
      grids[[index]] <- grid
      xs[[index]] <- Axis(
        type = "category",
        grid_index = index - 1L,
        data = as.list(xlabels[[j]]),
        position = "top",
        boundary_gap = TRUE,
        name = if (j == 1L) config@xlab else NULL,
        name_location = "middle",
        name_gap = 30,
        axis_line = AxisLine(show = FALSE),
        split_line = SplitLine(show = FALSE),
        axis_label = AxisLabel(
          show = j %in% c(1L, 2L),
          interval = 0,
          text_style = TextStyle(font_size = config@font_size)
        )
      )
      ys[[index]] <- Axis(
        type = "category",
        grid_index = index - 1L,
        data = as.list(ylabels[[j]]),
        inverse = TRUE,
        boundary_gap = TRUE,
        name = if (j == 1L) config@ylab else NULL,
        name_location = "middle",
        name_gap = 50,
        axis_line = AxisLine(show = FALSE),
        split_line = SplitLine(show = FALSE),
        axis_label = AxisLabel(
          show = j %in% c(1L, 3L),
          interval = 0,
          text_style = TextStyle(font_size = config@font_size)
        )
      )
    }
    for (correct in c(TRUE, FALSE)) {
      at <- which(
        if (correct) diag(TRUE, k) else !diag(TRUE, k),
        arr.ind = TRUE
      )
      hue <- if (correct) config@correct_color else config@incorrect_color
      ramp <- grDevices::colorRamp(c(low_color, hue))
      marks <- lapply(seq_len(nrow(at)), function(j) {
        # which(..., arr.ind = TRUE) can retain row/col names on a scalar;
        # htmlwidgets preserves named scalars as JSON objects, not coordinates.
        r <- unname(at[j, 1L])
        column <- unname(at[j, 2L])
        fraction <- p[["fraction"]][r, column]
        rgb <- ramp(if (is.na(fraction)) 0 else fraction)
        fill <- grDevices::rgb(
          rgb[[1L]],
          rgb[[2L]],
          rgb[[3L]],
          maxColorValue = 255
        )
        list(
          value = list(
            column - 1L,
            r - 1L,
            if (is.na(fraction)) 0 else fraction,
            p[["counts"]][r, column],
            classes[[r]],
            classes[[column]],
            if (is.na(fraction)) NULL else fraction,
            format_rate(fraction)
          ),
          label = cell_label(
            format(p[["counts"]][r, column], scientific = FALSE, trim = TRUE),
            fill
          )
        )
      })
      s <- to_list(HeatmapSeries(
        name = p[["name"]],
        data = marks,
        x_axis_index = base,
        y_axis_index = base,
        item_style = ItemStyle(
          border_width = 1,
          border_color = low_color
        )
      ))
      s[["dimensions"]] <- as.list(c(
        "Column",
        "Row",
        "Color fraction",
        "Count",
        "Reference",
        "Predicted",
        "Row fraction"
      ))
      # DimensionDefinition in ECharts util/types.ts supports an independent
      # displayName and ordinal type. Keep the full numeric fraction above;
      # the native tooltip reads fixed-decimal text without a JS formatter.
      s[["dimensions"]][[8L]] <- list(
        name = "Row fraction label",
        displayName = "Row fraction",
        type = "ordinal"
      )
      s[["encode"]] <- list(
        x = 0L,
        y = 1L,
        tooltip = as.list(c(4L, 5L, 3L, 7L))
      )
      map <- to_list(VisualMap(
        show = FALSE,
        min = 0,
        max = 1,
        in_range = list(color = c(low_color, hue))
      ))
      map[["dimension"]] <- 2L
      map[["seriesIndex"]] <- length(series)
      maps[[length(maps) + 1L]] <- map
      series[[length(series) + 1L]] <- s
    }
    if (config@show_metrics) {
      # Each strip contains only metrics; its positions share the count grid's
      # row or column layout. Store every visible label as plain text.
      right <- bottom <- list()
      for (j in seq_len(k)) {
        for (m in 1:2) {
          right[[length(right) + 1L]] <- list(
            value = list(m - 1L, j - 1L, 0),
            label = cell_label(
              format_rate(p[[c("sensitivity", "specificity")[[m]]]][[j]]),
              summary_color
            )
          )
          bottom[[length(bottom) + 1L]] <- list(
            value = list(j - 1L, m - 1L, 0),
            label = cell_label(
              format_rate(p[[c("ppv", "npv")[[m]]]][[j]]),
              summary_color
            )
          )
        }
      }
      # Stack the two overall summaries so their names do not compete for
      # half of an already narrow strip at phone widths.
      overall <- list(
        list(
          value = list(0L, 0L, 0),
          label = cell_label(
            paste0("Accuracy\n", format_rate(p[["accuracy"]])),
            summary_color
          )
        ),
        list(
          value = list(0L, 1L, 0),
          label = cell_label(
            paste0("BA\n", format_rate(p[["balanced_accuracy"]])),
            summary_color
          )
        )
      )
      for (j in 1:3) {
        map <- to_list(VisualMap(
          show = FALSE,
          min = 0,
          max = 1,
          in_range = list(color = rep(summary_color, 2L))
        ))
        map[["dimension"]] <- 2L
        map[["seriesIndex"]] <- length(series)
        maps[[length(maps) + 1L]] <- map
        series[[length(series) + 1L]] <- HeatmapSeries(
          data = list(right, bottom, overall)[[j]],
          x_axis_index = base + j,
          y_axis_index = base + j,
          silent = TRUE,
          item_style = ItemStyle(
            border_width = 1,
            border_color = low_color
          )
        )
      }
    }
  }
  EChartsOption(
    title = titles,
    grid = grids,
    x_axis = xs,
    y_axis = ys,
    visual_map = maps,
    series = series,
    legend = Legend(show = FALSE),
    tooltip = Tooltip(trigger = "item", confine = TRUE)
  )
}

#' Render confusion panels with enough default room for their labels
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
  built <- compile(option, data = data)
  panels <- length(built@grid) / if (option@show_metrics) 4L else 1L
  cols <- min(option@ncol, panels)
  rows <- ceiling(panels / cols)
  draw(
    built,
    theme = theme,
    width = width %||% (cols * 560),
    height = height %||% (rows * 500 + if (is.null(option@title)) 0 else 40),
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = render_meta(option, built),
    ...
  )
}


#' Draw Confusion Counts and Classification Summaries
#'
#' Accepts a square count matrix, a named list of count matrices, long frequency
#' records (`reference`, `predicted`, `n`, optional `panel`), or paired reference
#' and predicted label vectors. With rtemis installed, classification metrics
#' objects are also accepted. Matrix columns are aligned by label identity;
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
