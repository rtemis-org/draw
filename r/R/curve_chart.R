# curve_chart.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% curve_spec ----
#' Describe a threshold curve chart
#'
#' One definition of what distinguishes a threshold curve chart, read by
#' `curve_data()` and `curve_request()`.
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


# %% curve_request ----
#' Build the request of a curve chart from grouped records
#'
#' Reports undefined curves and excluded pairs, and hands the groups and the
#' ordered curves to `@rtemis/draw`'s ROC or precision-recall builder, which
#' names, colors, and draws them.
#' spec: draw/shared-builders#curves
#' @param type Character \{"roc", "pr"\}: Builder.
#' @param config [ChartConfig]: Bound curve configuration.
#' @param prepared List: Output of `curve_data()`.
#' @return [BuildRequest] of type `type`.
#' @keywords internal
#' @noRd
curve_request <- function(type, config, prepared) {
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
  na_null <- function(x) if (is.na(x)) NULL else jsonlite::unbox(x)
  document <- list(
    groups = lapply(prepared[["groups"]], function(g) {
      list(
        split = jsonlite::unbox(g[["split"]]),
        class = jsonlite::unbox(g[["class"]]),
        mean = na_null(g[["mean"]]),
        sd = na_null(g[["sd"]]),
        reference = na_null(g[["reference"]])
      )
    }),
    curves = lapply(prepared[["curves"]], function(curve) {
      list(
        group = jsonlite::unbox(curve[["group"]] - 1L),
        fold = jsonlite::unbox(curve[["fold"]]),
        x = I(unname(curve[["x"]])),
        y = I(unname(curve[["y"]])),
        summary = na_null(curve[["summary"]]),
        omitted = jsonlite::unbox(curve[["omitted"]])
      )
    })
  )
  BuildRequest(
    type = type,
    data = as.character(jsonlite::toJSON(
      document,
      auto_unbox = FALSE,
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list()
  )
} # /rtemis.draw::curve_request


# %% curve_legend_title ----
#' Legend title of a curve chart
#'
#' Names what the legend entries' values are, as `@rtemis/draw`'s
#' `curveLegendTitle()` does: `legend_title` when set, else the summary
#' ("AUC", "AP"), or over resamples "AUC: mean (SD)".
#' @param config [ChartConfig]: Curve configuration.
#' @return Character.
#' @keywords internal
#' @noRd
curve_legend_title <- function(config) {
  if (!is.null(config@legend_title)) {
    return(config@legend_title)
  }
  summary <- curve_spec(config)[["summary"]]
  if (config@variant == "per_resample") {
    paste0(summary, ": mean (SD)")
  } else {
    summary
  }
} # /rtemis.draw::curve_legend_title


# %% curve_grid ----
#' Plot margins of a curve chart, as `@rtemis/draw`'s curve builder sets them
#' @param config [ChartConfig]: Curve configuration.
#' @return Named list of `left`, `right`, `top`, and `bottom` pixels.
#' @keywords internal
#' @noRd
curve_grid <- function(config) {
  list(
    left = 70,
    right = 24,
    top = 25 + if (is.null(config@title)) 0 else 25,
    bottom = 65
  )
} # /rtemis.draw::curve_grid


# %% square_render_meta ----
#' Render hints of a chart with an optional square plotting grid
#' @param config [ChartConfig]: Configuration with a `square` property.
#' @param option [EChartsOption] or [BuildRequest]: Compiled option, or the
#'   request of a curve chart, whose margins are `curve_grid()`'s.
#' @return List of render hints.
#' @keywords internal
#' @noRd
square_render_meta <- function(config, option) {
  if (!config@square) {
    return(list())
  }
  grid <- if (S7::S7_inherits(option, BuildRequest)) {
    curve_grid(config)
  } else {
    option@grid
  }
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


# %% curve_render_meta ----
#' Render hints of a ROC or PR chart
#'
#' The square plotting grid of `square_render_meta()`, and `legendTitleAbove`,
#' which draws the legend title on its own row above the entries.
#' @inheritParams square_render_meta
#' @return List of render hints.
#' @keywords internal
#' @noRd
curve_render_meta <- function(config, option) {
  c(square_render_meta(config, option), list(legendTitleAbove = TRUE))
} # /rtemis.draw::curve_render_meta

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
  # A config read from a chart record draws with the record's theme unless
  # the caller passes one. spec: draw/chart-appearance#the-theme
  theme <- theme %||% option@theme
  built <- compile(option, data = data, theme = theme)
  # The legend title names the entries' values, which then read as numbers.
  option@legend_title <- curve_legend_title(option)
  grid <- curve_grid(option)
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
