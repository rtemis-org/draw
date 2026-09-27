# distribution.R
# spec: draw/first-cran-release#histogram-density-foundation

#' Validate and split numeric distribution observations
#' @param x Numeric or List: Vectors to summarize.
#' @inheritParams draw_density
#' @return Named list: Samples in variable/first-appearance group order.
#' @keywords internal
#' @noRd
distribution_samples <- new_generic("distribution_samples", "x")
method(distribution_samples, class_any) <- function(
  x,
  group = NULL,
  na_rm = TRUE,
  verbosity = 1L
) {
  check_integer_scalar(verbosity)
  if (verbosity < 0) {
    abort(
      "Set `verbosity` to a nonnegative integer.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  was_list <- is.list(x)
  if (!was_list) {
    x <- list(x)
  }
  if (!length(x)) {
    abort(
      "Supply at least one numeric distribution sample.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  labels <- names(x)
  if (is.null(labels) || anyNA(labels) || any(!nzchar(labels))) {
    labels <- paste("Series", seq_along(x))
  }
  for (i in seq_along(x)) {
    if (is.logical(x[[i]]) && all(is.na(x[[i]]))) {
      x[[i]] <- as.numeric(x[[i]])
    }
    if (
      !is.numeric(x[[i]]) ||
        is.complex(x[[i]]) ||
        !is.null(dim(x[[i]])) ||
        any(is.infinite(x[[i]]))
    ) {
      abort(
        "Supply finite numeric distribution observations or NA.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    if (!na_rm && anyNA(x[[i]])) {
      abort(
        "Remove missing observations or set `na_rm = TRUE`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  group <- group_values(group, lengths(x))
  levels <- if (is.null(group)) {
    NULL
  } else {
    unique(as.character(group[!is.na(group)]))
  }
  result <- list()
  for (i in seq_along(x)) {
    values <- x[[i]]
    missing <- sum(is.na(values))
    if (missing) {
      msg(
        "Removed",
        missing,
        "NA",
        ngettext(missing, "value", "values"),
        "from",
        if (was_list) labels[[i]] else "x",
        verbosity = verbosity
      )
    }
    if (is.null(group)) {
      result[[labels[[i]]]] <- values[!is.na(values)]
    } else {
      for (level in levels) {
        name <- if (was_list) paste(labels[[i]], level, sep = " - ") else level
        if (name %in% names(result)) {
          abort(
            "Use distinct distribution sample names.",
            class = c("rtemis_value_error", "rtemis_input_error")
          )
        }
        result[[name]] <- values[which(
          !is.na(group) & as.character(group) == level & !is.na(values)
        )]
      }
    }
  }
  if (anyDuplicated(labels)) {
    abort(
      "Use distinct distribution sample names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!sum(lengths(result))) {
    abort(
      "Supply at least one available observation with a nonmissing group.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  result
}

#' Estimate a density using explicit shared smoothing settings
#' @param x Numeric: Finite observations without missing entries.
#' @param config DensityConfig or HistogramConfig: Validated smoothing settings.
#' @return List: Density coordinates; empty samples retain empty coordinates.
#' @keywords internal
#' @noRd
distribution_density <- new_generic("distribution_density", "x")
method(distribution_density, class_numeric) <- function(x, config) {
  if (!length(x)) {
    return(list(x = numeric(), y = numeric()))
  }
  if (is.null(config@bandwidth) && (length(x) < 2L || length(unique(x)) < 2L)) {
    abort(
      "Supply an explicit `bandwidth` for singleton or constant density samples.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  estimate <- tryCatch(
    stats::density(
      x,
      bw = config@bandwidth %||% config@bw,
      adjust = config@adjust,
      kernel = config@kernel,
      n = config@n
    ),
    error = function(error) {
      abort(
        "Choose a suitable numeric `bandwidth` or another bandwidth selector: ",
        conditionMessage(error),
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  )
  if (any(!is.finite(c(estimate[["x"]], estimate[["y"]])))) {
    abort(
      "Rescale the observations or choose a finite density bandwidth.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  estimate[c("x", "y")]
}

#' Build density curves from the shared statistical configuration
#' @inheritParams draw_density
#' @return EChartsOption: Native density lines and fills.
#' @keywords internal
#' @noRd
density_option <- new_generic("density_option", "x")
method(density_option, class_any) <- function(
  x,
  group = NULL,
  n = 512,
  bw = "nrd0",
  na_rm = TRUE,
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margins = DEFAULT_MARGINS,
  verbosity = 1L,
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = .25,
  mode = "overlap",
  order = "input"
) {
  config <- setup_DensityConfig(
    mode = mode,
    order = order,
    n = n,
    bw = bw,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    na_rm = na_rm
  )
  samples <- distribution_order(
    distribution_samples(x, group, na_rm, verbosity),
    order
  )
  series <- lapply(seq_along(samples), function(i) {
    estimate <- distribution_density(samples[[i]], config)
    LineSeries(
      name = names(samples)[[i]],
      data = Map(c, estimate[["x"]], estimate[["y"]]),
      show_symbol = FALSE,
      area_style = AreaStyle(opacity = fill_alpha)
    )
  })
  option <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(
      trigger = "axis",
      value_formatter = number_value_formatter()
    ),
    legend = if (length(samples) > 1L) {
      Legend(data = as.list(names(samples)))
    } else {
      NULL
    },
    x_axis = Axis(
      type = "value",
      name = xlab,
      scale = TRUE,
      name_location = if (!is.null(xlab)) "middle" else NULL
    ),
    y_axis = Axis(
      type = "value",
      min = 0,
      name = ylab,
      name_location = if (!is.null(ylab)) "middle" else NULL
    ),
    grid = resolve_margins(margins),
    series = series,
    color = palette
  )
  distribution_layout(option, names(samples), mode)
}

#' Compute histogram heights on an explicit normalization scale
#' @param x Numeric: Nonnegative bin counts.
#' @param widths Numeric: Positive bin widths.
#' @param size Integer: Available observations in this sample.
#' @inheritParams draw_histogram
#' @return Numeric: Heights; empty samples have zero heights on every scale.
#' @keywords internal
#' @noRd
histogram_heights <- new_generic("histogram_heights", "x")
method(histogram_heights, class_numeric) <- function(
  x,
  widths,
  size,
  normalization
) {
  if (!size) {
    return(rep(0, length(x)))
  }
  switch(
    normalization,
    count = x,
    probability = x / size,
    percent = 100 * x / size,
    density = x / (size * widths),
    count_density = x / widths
  )
}

#' Build numeric-axis histogram bins and optional density curves
#' @inheritParams draw_histogram
#' @return EChartsOption: Native rectangles and optional density lines.
#' @keywords internal
#' @noRd
histogram_option <- new_generic("histogram_option", "x")
method(histogram_option, class_any) <- function(
  x,
  group = NULL,
  breaks = "Sturges",
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margins = DEFAULT_MARGINS,
  bins = NULL,
  bin_edges = NULL,
  normalization = "count",
  density = FALSE,
  n = 512L,
  bw = "nrd0",
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = .25,
  na_rm = TRUE,
  verbosity = 1L,
  mode = "overlap",
  order = "input",
  bin_stat = "count",
  bar_mode = "overlay"
) {
  config <- setup_HistogramConfig(
    bin_stat = bin_stat,
    bar_mode = bar_mode,
    mode = mode,
    order = order,
    breaks = breaks,
    bins = bins,
    bin_edges = bin_edges,
    normalization = normalization,
    density = density,
    n = n,
    bw = bw,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    na_rm = na_rm
  )
  samples <- distribution_order(
    distribution_samples(x, group, na_rm, verbosity),
    order
  )
  values <- unlist(samples, use.names = FALSE)
  edges <- config@bin_edges
  if (
    !is.null(edges) && (min(values) < min(edges) || max(values) > max(edges))
  ) {
    abort(
      "Supply `bin_edges` spanning every retained observation.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.null(edges)) {
    edges <- graphics::hist(
      values,
      breaks = config@bins %||% config@breaks,
      plot = FALSE
    )[["breaks"]]
  }
  widths <- diff(edges)
  equal_width <- max(abs(widths / widths[[1L]] - 1)) < 1e-7
  if (
    bin_stat == "count" &&
      !equal_width &&
      !normalization %in% c("density", "count_density")
  ) {
    abort(
      "Use `normalization = \"density\"` or `\"count_density\"` for unequal-width bins.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  series <- list()
  for (i in seq_along(samples)) {
    sample <- samples[[i]]
    size <- length(sample)
    # Fix interval semantics for every path: (left, right], including the
    # lowest edge, with graphics::hist's documented boundary tolerance.
    counts <- if (size) {
      graphics::hist(
        sample,
        breaks = edges,
        plot = FALSE,
        right = TRUE,
        include.lowest = TRUE
      )[["counts"]]
    } else {
      integer(length(widths))
    }
    heights <- if (bin_stat == "count") {
      histogram_heights(counts, widths, size, normalization)
    } else {
      histogram_stat(sample, edges, bin_stat)
    }
    series[[length(series) + 1L]] <- list(
      type = "custom",
      name = names(samples)[[i]],
      renderItem = "rtemis.histogram.v1",
      clip = TRUE,
      z = 2,
      dimensions = list(
        "Lower",
        "Upper",
        switch(
          normalization,
          count = if (bin_stat == "count") "Height" else bin_stat,
          probability = "Probability",
          percent = "Percent",
          density = "Density",
          count_density = "Count density"
        ),
        "Count"
      ),
      encode = list(
        x = list(0L, 1L),
        y = 2L,
        tooltip = if (normalization == "count" && bin_stat == "count") {
          list(0L, 1L, 3L)
        } else {
          list(0L, 1L, 2L, 3L)
        }
      ),
      itemPayload = list(fillAlpha = fill_alpha),
      data = Map(
        function(lo, hi, height, count) list(lo, hi, height, count),
        head(edges, -1L),
        tail(edges, -1L),
        heights,
        counts
      )
    )
    if (density) {
      estimate <- distribution_density(sample, config)
      # Counts/probabilities per bin need a width factor; density scales do not.
      multiplier <- switch(
        normalization,
        count = size * widths[[1L]],
        probability = widths[[1L]],
        percent = 100 * widths[[1L]],
        density = 1,
        count_density = size
      )
      # Match ROC's native item-tooltip pattern: an unmarked line alone can
      # intercept a bin hover without identifying a data item. Transparent
      # point targets expose evaluated density values; SVG omits the targets.
      curve <- to_list(LineSeries(
        name = names(samples)[[i]],
        data = Map(c, estimate[["x"]], estimate[["y"]] * multiplier),
        show_symbol = TRUE,
        symbol = "circle",
        symbol_size = 8,
        item_style = ItemStyle(opacity = 0),
        line_style = LineStyle(opacity = 1),
        z = 3
      ))
      curve[["emphasis"]] <- list(itemStyle = list(opacity = 1))
      series[[length(series) + 1L]] <- curve
    }
  }
  # Keep the whole height matrix in the renderer payload so legend filtering
  # can recompute dodging/stacking without mutating statistical values.
  bar_indices <- which(vapply(
    series,
    function(s) s[["type"]] == "custom",
    logical(1)
  ))
  height_rows <- lapply(series[bar_indices], function(s) {
    vapply(s[["data"]], `[[`, numeric(1), 3L)
  })
  for (i in bar_indices) {
    series[[i]][["itemPayload"]] <- list(
      fillAlpha = fill_alpha,
      mode = bar_mode,
      seriesIndices = as.list(bar_indices - 1L),
      heights = lapply(height_rows, as.list)
    )
  }
  totals <- if (bar_mode == "stack") {
    c(
      Reduce(`+`, lapply(height_rows, pmax, 0)),
      Reduce(`+`, lapply(height_rows, pmin, 0))
    )
  } else {
    unlist(height_rows, use.names = FALSE)
  }
  limits <- range(c(0, totals))
  option <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(
      trigger = "item",
      value_formatter = number_value_formatter()
    ),
    legend = if (length(samples) > 1L) {
      Legend(
        data = as.list(names(samples)),
        item_style = ItemStyle(opacity = 1)
      )
    } else {
      NULL
    },
    x_axis = Axis(
      type = "value",
      scale = TRUE,
      name = xlab,
      name_location = if (!is.null(xlab)) "middle" else NULL
    ),
    y_axis = Axis(
      type = "value",
      min = if (limits[[1]] < 0) limits[[1]] * 1.05 else 0,
      max = if (bar_mode == "stack" && limits[[2]] > 0) {
        limits[[2]] * 1.05
      } else {
        NULL
      },
      name = ylab,
      name_location = if (!is.null(ylab)) "middle" else NULL
    ),
    grid = resolve_margins(margins),
    series = series,
    color = palette
  )
  distribution_layout(option, names(samples), mode)
}


#' Order complete samples without separating their observations
#' @param x Named list: Validated numeric samples.
#' @param order Character: Input order or decreasing mean/median.
#' @return List: Ordered samples, with empty samples last for summary ordering.
#' @keywords internal
#' @noRd
distribution_order <- new_generic("distribution_order", "x")
method(distribution_order, class_list) <- function(x, order) {
  if (order == "input") {
    return(x)
  }
  summary <- if (order == "mean") mean else stats::median
  scores <- vapply(
    x,
    function(values) {
      if (length(values)) summary(values) else NA_real_
    },
    numeric(1)
  )
  x[base::order(scores, decreasing = TRUE, na.last = TRUE)]
}

#' Align distribution layers on common axes in independent rows
#'
#' Uses GridOption and CartesianAxisOption from ECharts coord/cartesian.
#' Every row retains density/count units; no peak normalization is applied.
#' @param x EChartsOption: Complete distribution layers.
#' @param labels Character: Ordered sample names.
#' @param mode Character: Overlap or ridge layout.
#' @return EChartsOption: Native multi-grid option for ridge mode.
#' @keywords internal
#' @noRd
distribution_layout <- new_generic("distribution_layout", "x")
method(distribution_layout, EChartsOption) <- function(x, labels, mode) {
  if (mode == "overlap") {
    return(x)
  }
  series <- to_list(x)[["series"]]
  limits <- range(unlist(lapply(series, function(s) {
    lapply(s[["data"]], function(d) {
      if (s[["type"]] == "custom") d[1:2] else d[1]
    })
  })))
  heights <- c(
    0,
    unlist(lapply(series, function(s) {
      lapply(s[["data"]], function(d) {
        d[[if (s[["type"]] == "custom") 3L else 2L]]
      })
    }))
  )
  minimum <- min(heights)
  maximum <- max(heights)
  nrow <- length(labels)
  x@series <- lapply(series, function(s) {
    index <- match(s[["name"]], labels) - 1L
    s[["xAxisIndex"]] <- index
    s[["yAxisIndex"]] <- index
    s
  })
  x@legend <- Legend(show = FALSE)
  x@grid <- lapply(seq_along(labels), function(i) {
    list(
      left = 100,
      right = 30,
      top = paste0(8 + (i - 1) * 80 / nrow, "%"),
      height = paste0(64 / nrow, "%"),
      containLabel = FALSE
    )
  })
  x_name <- x@x_axis@name
  x@x_axis <- lapply(seq_along(labels), function(i) {
    Axis(
      type = "value",
      min = limits[[1]],
      max = limits[[2]],
      grid_index = i - 1,
      name = if (i == nrow) x_name else NULL,
      name_location = "middle",
      show = i == nrow
    )
  })
  x@y_axis <- lapply(seq_along(labels), function(i) {
    Axis(
      type = "value",
      min = if (minimum < 0) minimum * 1.05 else 0,
      max = if (maximum > 0) maximum * 1.05 else 1,
      grid_index = i - 1,
      name = labels[[i]],
      name_location = "middle",
      name_gap = 55,
      split_number = 2,
      axis_label = AxisLabel(show_max_label = FALSE, show_min_label = FALSE)
    )
  })
  x
}

#' Aggregate observations inside the histogram boundary convention
#' @param x Numeric: Complete sample observations.
#' @param edges Numeric: Increasing histogram boundaries.
#' @param statistic Character: Sum, mean, minimum, or maximum.
#' @return Numeric: One height per bin, zero for empty bins.
#' @keywords internal
#' @noRd
histogram_stat <- new_generic("histogram_stat", "x")
method(histogram_stat, class_numeric) <- function(x, edges, statistic) {
  widths <- diff(edges)
  if (!length(x)) {
    return(rep(0, length(widths)))
  }
  # Reproduce graphics::hist.default's default 1e-7 boundary tolerance.
  n <- length(edges)
  fuzz <- 1e-7 *
    if (n > 5L) {
      stats::median(widths)
    } else if (n <= 3L) {
      diff(range(x))
    } else {
      min(widths)
    }
  bins <- cut(
    x,
    edges + c(-fuzz, rep(fuzz, n - 1L)),
    right = TRUE,
    include.lowest = TRUE,
    labels = FALSE
  )
  fn <- switch(statistic, sum = sum, mean = mean, min = min, max = max)
  values <- vapply(
    seq_along(widths),
    function(i) {
      selected <- x[bins == i]
      if (length(selected)) fn(selected) else 0
    },
    numeric(1)
  )
  if (any(!is.finite(values))) {
    abort(
      "Rescale observations to keep bin statistics finite.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  values
}
