# curve_chart.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% curve_spec ----
#' Describe a threshold curve chart
#'
#' One definition of what distinguishes a threshold curve chart, read by
#' `curve_data()` and `curve_option()`.
#' @param config [ChartConfig]: Curve chart configuration.
#' @return List of `name`, `summary`, `x`, and `y` labels; the `step` mode;
#'   `order`, `valid`, and `area` functions of the curve coordinates; and the
#'   `coordinates`, `invalid`, and `undefined` error messages.
#' @keywords internal
#' @noRd
curve_spec <- new_generic("curve_spec", "config")


# %% check_curve_scores ----
#' Check normalized labels and probabilities
#'
#' A model adapter may pool normalized folds or retain an empty fold. Checks
#' that assembled records preserve row and class identities.
#' @param x List: Normalized labels and probabilities from `roc_probabilities()`.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_curve_scores <- function(x) {
  labels <- x[["y"]]
  levels <- x[["levels"]]
  classes <- x[["classes"]]
  probabilities <- x[["prob"]]
  if (
    !is.character(labels) ||
      !is.null(dim(labels)) ||
      !is.character(levels) ||
      length(levels) < 2L ||
      anyNA(levels) ||
      any(!nzchar(levels)) ||
      anyDuplicated(levels) ||
      !is.character(classes) ||
      !length(classes) ||
      anyNA(classes) ||
      anyDuplicated(classes) ||
      any(!classes %in% levels) ||
      any(!labels[!is.na(labels)] %in% levels) ||
      !is.matrix(probabilities) ||
      !is.numeric(probabilities) ||
      is.complex(probabilities) ||
      nrow(probabilities) != length(labels) ||
      !identical(colnames(probabilities), levels) ||
      any(!is.finite(probabilities[!is.na(probabilities)])) ||
      any(probabilities < 0 | probabilities > 1, na.rm = TRUE)
  ) {
    abort(
      "Supply normalized records from roc_probabilities(), preserving row and class identities.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(x)
}


# %% curve_input ----
#' Prepare named input sets or precomputed curve records
#' @param x Vector, list, or data frame: Reference labels or curve records.
#' @param predicted_prob Optional Numeric vector, matrix, or list: Probabilities.
#' @param positive Optional Character: Binary positive class.
#' @param vertices Function: Computes curve records from normalized input.
#' @param columns Character: Coordinate columns of a precomputed table.
#' @return Data frame of curve records.
#' @keywords internal
#' @noRd
curve_input <- function(x, predicted_prob, positive, vertices, columns) {
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
      "Supply probabilities with labels, or a table containing ",
      columns,
      ".",
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
        "Use unique nonempty names for curve input sets.",
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
    out <- vertices(roc_probabilities(labels[[i]], probs[[i]], positive))
    out[["split"]] <- names_out[[i]]
    out[["fold"]] <- "aggregate"
    out
  })
  do.call(rbind, pieces)
}


# %% curve_data ----
#' Validate and partition portable curve records
#' @param config [ChartConfig]: Bound curve configuration.
#' @param data Data frame or named list: Long curve records.
#' @param x,y Numeric: Curve coordinates in \[0, 1\], aligned with `data`.
#' @param summary Optional Numeric: Supplied curve summary per record.
#' @param reference Optional Numeric: Chance level of the curve per record.
#' @return List containing selected curves, groups, and disclosure counts.
#' @keywords internal
#' @noRd
curve_data <- function(config, data, x, y, summary = NULL, reference = NULL) {
  spec <- curve_spec(config)
  n <- length(x)
  for (v in list(x, y)) {
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
        spec[["coordinates"]],
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
        " label per ",
        spec[["name"]],
        " record.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    v
  }
  check_fraction <- function(v, label) {
    if (
      !is.null(v) &&
        (!is.numeric(v) ||
          is.complex(v) ||
          !is.null(dim(v)) ||
          length(v) != n ||
          any(!is.finite(v[!is.na(v)])) ||
          any(v < 0 | v > 1, na.rm = TRUE))
    ) {
      abort(
        "Supply aligned ",
        label,
        " values in [0, 1], or NA for unknown ",
        label,
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  split <- labels(config@split, "Sample", "split")
  cls <- labels(config@class_label, spec[["name"]], "class")
  fold <- labels(config@fold, "aggregate", "fold")
  omitted <- config_column(data, config@omitted, "omitted") %||% rep(0, n)
  check_fraction(summary, spec[["summary"]])
  check_fraction(reference, "prevalence")
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
    scores <- references <- numeric()
    for (id in folds) {
      at <- rows[fold[rows] == id]
      area <- if (is.null(summary)) NA_real_ else unique(summary[at])
      level <- if (is.null(reference)) NA_real_ else unique(reference[at])
      missing_n <- unique(omitted[at])
      if (
        length(area) != 1L || length(level) != 1L || length(missing_n) != 1L
      ) {
        abort(
          "Keep curve summaries and omitted counts constant within each split/class/fold curve.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      max_omitted <- max(max_omitted, missing_n)
      if (all(is.na(x[at])) && all(is.na(y[at])) && is.na(area)) {
        undefined <- undefined + 1L
        next
      }
      if (anyNA(x[at]) || anyNA(y[at])) {
        abort(
          "Represent an undefined curve with all-missing coordinates; complete curves cannot contain gaps.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      at <- at[spec[["order"]](x[at], y[at])]
      xx <- x[at]
      yy <- y[at]
      if (!spec[["valid"]](xx, yy)) {
        abort(
          spec[["invalid"]],
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      if (is.null(summary)) {
        area <- spec[["area"]](xx, yy)
      }
      scores <- c(scores, area)
      references <- c(references, level)
      curves[[length(curves) + 1L]] <- list(
        group = g,
        fold = id,
        x = xx,
        y = yy,
        summary = area,
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
      total = length(folds),
      reference = if (any(!is.na(references))) {
        mean(references, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  }
  if (!length(curves)) {
    abort(
      spec[["undefined"]],
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


# %% curve_option ----
#' Compile curve records to independent native line series
#'
#' Curve identity sets the series name, so a legend entry toggles the folds of a
#' group together with its chance level. Each chance level follows the curves:
#' a legend key takes its style from the first series of that name.
#' @param config [ChartConfig]: Bound curve configuration.
#' @param prepared List: Output of `curve_data()`.
#' @param before List: Series drawn ahead of the curves.
#' @return An [EChartsOption] with full legend.
#' @keywords internal
#' @noRd
curve_option <- function(config, prepared, before = list()) {
  spec <- curve_spec(config)
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
            " (",
            spec[["summary"]],
            " mean ",
            fmt(g[["mean"]]),
            "; SD ",
            fmt(g[["sd"]]),
            ")"
          )
        } else {
          paste0(" (", spec[["summary"]], " ", fmt(g[["mean"]]), ")")
        }
      )
    },
    character(1)
  )
  labels <- make.unique(labels)
  active <- unique(vapply(curves, `[[`, integer(1), "group"))
  legend_names <- labels[active]
  group_color <- function(g) {
    color_id <- if (length(classes) > 1L) {
      match(g[["class"]], classes)
    } else {
      match(g[["split"]], splits)
    }
    if (!is.null(config@palette)) {
      config@palette[[(color_id - 1L) %% length(config@palette) + 1L]]
    } else {
      NULL
    }
  }
  # The shared renderer measures legend space independently of curve geometry.
  bottom <- 65
  if (prepared[["undefined"]]) {
    msg(prepared[["undefined"]], "undefined curve(s) omitted")
  }
  if (prepared[["max_omitted"]]) {
    msg(
      "Missing pairs excluded: at most",
      prepared[["max_omitted"]],
      "per curve"
    )
  }
  top <- 25 + if (!is.null(config@title)) 25 else 0
  grid <- to_list(Grid(
    left = 70,
    right = 24,
    top = top,
    bottom = bottom,
    contain_label = FALSE
  ))
  grid[["outerBoundsMode"]] <- "none"
  series <- before
  for (curve in curves) {
    id <- curve[["group"]]
    g <- groups[[id]]
    color <- group_color(g)
    points <- lapply(seq_along(curve[["x"]]), function(j) {
      list(
        unname(curve[["x"]][[j]]),
        unname(curve[["y"]][[j]]),
        if (is.na(curve[["summary"]])) NULL else curve[["summary"]],
        curve[["fold"]],
        curve[["omitted"]],
        fmt(curve[["x"]][[j]]),
        fmt(curve[["y"]][[j]]),
        fmt(curve[["summary"]])
      )
    })
    s <- to_list(LineSeries(
      name = labels[[id]],
      data = points,
      smooth = FALSE,
      step = spec[["step"]],
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
    display <- c(spec[["x"]], spec[["y"]], spec[["summary"]])
    s[["dimensions"]] <- as.list(c(display, "Resample", "Missing pairs"))
    # DimensionDefinition (ECharts util/types.ts) separates display text from
    # full-precision values. Ordinal labels survive JSON without callbacks.
    # spec: draw/first-cran-release#roc-views
    s[["dimensions"]] <- c(
      s[["dimensions"]],
      lapply(display, function(label) {
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
      tooltip = as.list(c(5L, 6L, 7L, 3L))
    )
    series[[length(series) + 1L]] <- s
  }
  for (id in active) {
    level <- groups[[id]][["reference"]]
    if (!is.na(level)) {
      series[[length(series) + 1L]] <- LineSeries(
        name = labels[[id]],
        data = list(c(0, level), c(1, level)),
        show_symbol = FALSE,
        silent = TRUE,
        legend_hover_link = FALSE,
        line_style = LineStyle(
          color = group_color(groups[[id]]),
          type = "dotted",
          width = 1
        ),
        z = 1
      )
    }
  }
  EChartsOption(
    title = Title(
      text = config@title,
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
      orient = "horizontal",
      # Shared render hints position this native legend after grid measurement.
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


# %% square_render_meta ----
#' Render hints of a chart with an optional square plotting grid
#' @param config [ChartConfig]: Configuration with a `square` property.
#' @param option [EChartsOption]: Compiled option.
#' @return List of render hints.
#' @keywords internal
#' @noRd
square_render_meta <- function(config, option) {
  if (!config@square) {
    return(list())
  }
  grid <- option@grid
  list(
    aspect = list(
      ratio = 1,
      leftPx = grid[["left"]],
      rightPx = grid[["right"]],
      topPx = grid[["top"]],
      botPx = grid[["bottom"]]
    )
  )
}


# %% draw_curve_config ----
#' Render a curve config with shared legend layout
#' @inheritParams draw
#' @param data Optional Data frame: Long curve records.
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_curve_config <- function(
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
  grid <- built@grid
  export_size <- list(
    width = 480 + grid[["left"]] + grid[["right"]],
    height = 480 + grid[["top"]] + grid[["bottom"]]
  )
  # Aspect charts derive their height from the width, so a stated dimension
  # takes the natural size of the other. Fully unset, the chart fills the host.
  if (!is.null(width) || !is.null(height)) {
    width <- width %||% export_size[["width"]]
    height <- height %||% export_size[["height"]]
  }
  draw(
    built,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = c(render_meta(option, built), list(exportSize = export_size)),
    ...
  )
}
