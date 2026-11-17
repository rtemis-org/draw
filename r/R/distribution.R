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
    group_levels(group)
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

#' Estimate density curves and build their request
#'
#' Estimates each sample's density with `stats::density()`
#' (`distribution_density()`); `@rtemis/draw`'s density builder draws the
#' curves and lays out `mode`.
#' spec: draw/shared-builders#density
#' @inheritParams draw_density
#' @return [BuildRequest] of type `"density"`.
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
  order = "input",
  zoom = FALSE,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  check_zoom_flag(zoom)
  margin <- pixel_margins(margins)
  config <- setup_DensityConfig(
    mode = mode,
    order = order,
    n = n,
    bw = bw,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    na_rm = na_rm,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    zoom = zoom,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  # The value axis says what the curve's height is, unless the author names it.
  config <- config_derive(config, list(ylab = "Density"))
  samples <- distribution_order(
    distribution_samples(x, group, na_rm, verbosity),
    order
  )
  data <- list(
    samples = lapply(names(samples), function(name) {
      estimate <- distribution_density(samples[[name]], config)
      list(
        name = jsonlite::unbox(name),
        n = jsonlite::unbox(length(samples[[name]])),
        x = I(estimate[["x"]]),
        y = I(estimate[["y"]])
      )
    })
  )
  BuildRequest(
    type = "density",
    data = as.character(jsonlite::toJSON(
      data,
      auto_unbox = FALSE,
      na = "null",
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list()
  )
}

#' Default histogram bar opacity
#'
#' Bars are more translucent where they can overlap: overlaid bars of more
#' than one nonempty sample. Dodged, stacked, ridge, and single-sample bars
#' never cover one another and are drawn more solidly.
#'
#' @param samples List: Samples as returned by `distribution_samples()`.
#' @param bar_mode Character \{"overlay", "group", "stack"\}: Histogram group
#'   layout.
#' @param mode Character \{"overlap", "ridge"\}: Distribution layout.
#' @return Numeric scalar: 0.5 when bars overlap, 0.75 otherwise.
#'
#' @author EDG
#' @keywords internal
#' @noRd
histogram_fill_alpha <- function(samples, bar_mode, mode) {
  overlap <- bar_mode == "overlay" &&
    mode == "overlap" &&
    sum(lengths(samples) > 0L) > 1L
  if (overlap) 0.5 else 0.75
} # /rtemis.draw::histogram_fill_alpha


#' Bin histogram samples and build their request
#'
#' Bins every sample on one set of edges with `graphics::hist()` (intervals
#' `(left, right]`, the lowest edge included), and computes each sample's bin
#' statistic and density curve; `@rtemis/draw`'s histogram builder applies
#' the normalization and lays out the bars.
#' spec: draw/shared-builders#histogram
#' @inheritParams draw_histogram
#' @return [BuildRequest] of type `"histogram"`.
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
  fill_alpha = NULL,
  border_alpha = 1,
  na_rm = TRUE,
  verbosity = 1L,
  mode = "overlap",
  order = "input",
  bin_stat = "count",
  bar_mode = "overlay",
  zoom = FALSE,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  check_zoom_flag(zoom)
  margin <- pixel_margins(margins)
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
    border_alpha = border_alpha,
    na_rm = na_rm,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    zoom = zoom,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  # The value axis says what a bar's height is, unless the author names it.
  config <- config_derive(config, list(ylab = histogram_value_label(config)))
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
    config@bin_stat == "count" &&
      !equal_width &&
      !config@normalization %in% c("density", "count_density")
  ) {
    abort(
      "Use `normalization = \"density\"` or `\"count_density\"` for unequal-width bins.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  data <- list(
    edges = I(edges),
    samples = lapply(names(samples), function(name) {
      sample <- samples[[name]]
      size <- length(sample)
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
      out <- list(
        name = jsonlite::unbox(name),
        n = jsonlite::unbox(size),
        counts = I(counts)
      )
      if (config@bin_stat != "count") {
        out[["values"]] <- I(histogram_stat(sample, edges, config@bin_stat))
      }
      if (config@density && size) {
        estimate <- distribution_density(sample, config)
        out[["density"]] <- list(x = I(estimate[["x"]]), y = I(estimate[["y"]]))
      }
      out
    })
  )
  BuildRequest(
    type = "histogram",
    data = as.character(jsonlite::toJSON(
      data,
      auto_unbox = FALSE,
      na = "null",
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list()
  )
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
