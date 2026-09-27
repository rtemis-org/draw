# config_histogram.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Histograms share density settings and numeric-column/group bindings.
# Bin and normalization settings determine their additional statistical contract.

# %% HistogramConfig ----
#' Histogram Configuration
#'
#' A serializable description of a histogram. Build one with
#' [setup_HistogramConfig()] rather than calling this constructor directly.
#'
#' `x` names one or more columns to bin. `group` names a column to split them
#' by, drawing one series per level. The output uses `CustomSeriesOption` in
#' ECharts `src/chart/custom/CustomSeries.ts` and `LineSeriesOption` in
#' `src/chart/line/LineSeries.ts` for optional density overlays.
#'
#' @param x Optional Character: Columns to bin, one distribution per column.
#' @param group Optional Character: Column to split the bins by.
#' @inheritParams draw_histogram breaks bins bin_edges normalization density na_rm bin_stat bar_mode
#' @inheritParams draw_density n bw bandwidth kernel adjust fill_alpha mode order
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams draw_line legend_position legend_placement
#' @inheritParams ChartConfig
#'
#' @return `HistogramConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_HistogramConfig(x = "mpg")@type
HistogramConfig <- new_class(
  name = "HistogramConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    # Reuse the density declarations so shared controls have identical schemas.
    DensityConfig@properties[c(
      "mode",
      "order",
      "n",
      "bw",
      "bandwidth",
      "kernel",
      "adjust",
      "na_rm",
      "fill_alpha"
    )],
    list(
      type = prop_chart_type("histogram"),
      # -- data binding ------------------------------------------------------
      x = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Columns to bin."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column to split the bins by, one series per level."
      ),
      # -- semantics ---------------------------------------------------------
      breaks = prop_string(
        "Sturges",
        enum = c("Sturges", "Scott", "FD", "Freedman-Diaconis"),
        description = "Binning rule."
      ),
      bins = prop_integer(
        NULL,
        nullable = TRUE,
        min = 1L,
        max = 1000000L,
        description = "Suggested bin count, passed through hist's pretty breaks."
      ),
      bin_edges = prop_float(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        min_items = 2L,
        description = "Strictly increasing bin edges spanning every retained value."
      ),
      normalization = prop_string(
        "count",
        enum = c("count", "probability", "percent", "density", "count_density"),
        description = "Histogram height normalization within each sample."
      ),
      bin_stat = prop_string(
        "count",
        enum = c("count", "sum", "mean", "min", "max"),
        description = "Statistic of the x observations falling in each bin."
      ),
      bar_mode = prop_string(
        "overlay",
        enum = c("overlay", "group", "stack"),
        description = "Overlay, dodge, or stack histogram groups; stacks separate positive and negative values."
      ),
      density = prop_boolean(
        FALSE,
        description = "Overlay a kernel density on the histogram scale."
      ),
      # -- appearance --------------------------------------------------------
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Series colors, overriding the theme palette. Unset uses the theme's."
      ),
      xlab = prop_string(
        NULL,
        nullable = TRUE,
        description = "X axis label. Unset derives it from the data."
      ),
      ylab = prop_string(
        NULL,
        nullable = TRUE,
        description = "Y axis label. Unset derives it from the data."
      ),
      margin_top = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Top margin in pixels."
      ),
      margin_right = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Right margin in pixels."
      ),
      margin_bottom = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Bottom margin in pixels."
      ),
      margin_left = prop_integer(
        NULL,
        min = 0L,
        nullable = TRUE,
        description = "Left margin in pixels."
      )
    )
  ),
  validator = function(self) {
    if (
      self@bin_stat != "count" &&
        (self@normalization != "count" || self@density)
    ) {
      return(
        "Use normalization = 'count' and density = FALSE with non-count bin statistics."
      )
    }
    if (self@bar_mode != "overlay" && (self@mode == "ridge" || self@density)) {
      return("Use overlay bars with ridgelines or density curves.")
    }
    if (!is.null(self@bins) && !is.null(self@bin_edges)) {
      return("Supply `bins` or `bin_edges`, not both.")
    }
    if (
      !is.null(self@bin_edges) &&
        any(!is.finite(diff(self@bin_edges)) | diff(self@bin_edges) <= 0)
    ) {
      return("Supply strictly increasing `bin_edges`.")
    }
    NULL
  }
) # /rtemis.draw::HistogramConfig


# %% HISTOGRAM_ORIGIN_NAMES ----
HISTOGRAM_ORIGIN_NAMES <- setdiff(
  names(HistogramConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_HistogramConfig ----
#' Set up a Histogram Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams HistogramConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [HistogramConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw(setup_HistogramConfig(x = "mpg"), data = mtcars)
setup_HistogramConfig <- function(
  x = NULL,
  group = NULL,
  breaks = "Sturges",
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  legend_position = "top",
  legend_placement = "outside",
  bins = NULL,
  bin_edges = NULL,
  normalization = "count",
  density = FALSE,
  n = 512L,
  bw = "nrd0",
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = 0.25,
  na_rm = TRUE,
  mode = "overlap",
  order = "input",
  bin_stat = "count",
  bar_mode = "overlay"
) {
  origin <- origin %||% chart_origin(match.call(), HISTOGRAM_ORIGIN_NAMES)
  if (is.numeric(breaks)) {
    if (!is.null(bins) || !is.null(bin_edges)) {
      abort(
        "Use numeric `breaks` or `bins`/`bin_edges`, not both.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (length(breaks) == 1L) {
      bins <- breaks
      origin[["bins"]] <- "user"
    } else {
      bin_edges <- breaks
      origin[["bin_edges"]] <- "user"
    }
    breaks <- "Sturges"
    origin[["breaks"]] <- "default"
  }
  if (!is.null(bins)) {
    check_integer_scalar(bins)
    bins <- as.integer(bins)
  }
  smoothing <- setup_DensityConfig(
    n = n,
    bw = bw,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    na_rm = na_rm
  )
  if (is.numeric(bw)) {
    origin[["bw"]] <- "default"
    origin[["bandwidth"]] <- "user"
  }
  HistogramConfig(
    bin_stat = bin_stat,
    bar_mode = bar_mode,
    mode = mode,
    order = order,
    bins = bins,
    bin_edges = bin_edges,
    normalization = normalization,
    density = density,
    n = smoothing@n,
    bw = smoothing@bw,
    bandwidth = smoothing@bandwidth,
    kernel = smoothing@kernel,
    adjust = smoothing@adjust,
    fill_alpha = smoothing@fill_alpha,
    na_rm = smoothing@na_rm,
    x = x,
    group = group,
    breaks = breaks,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
    legend_position = legend_position,
    legend_placement = legend_placement,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_HistogramConfig


# %% resolve.HistogramConfig ----
# Only the x label: it is the name of the bound column. The y axis shows a bin count,
# which has no name in the data -- and a constant like "Density" would be an
# invented default, not a derivation. Labels come from names; no name, no label.
# If such a label is wanted it belongs in the builder, applying to the vector
# path too, rather than being written into the document by one of them.
method(resolve, HistogramConfig) <- function(config, data = NULL, ...) {
  config_derive(
    config,
    list(
      xlab = if (length(config@x) == 1L) config@x else NULL
    )
  )
}


# %% compile.HistogramConfig ----
method(compile, HistogramConfig) <- function(config, data = NULL, ...) {
  x <- if (length(config@x) > 1L) {
    setNames(
      lapply(config@x, function(column) config_column(data, column, "x")),
      config@x
    )
  } else {
    config_column(data, config@x, "x")
  }
  if (is.null(x)) {
    abort(
      "A HistogramConfig needs `x` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  histogram_option(
    x = x,
    bin_stat = config@bin_stat,
    bar_mode = config@bar_mode,
    mode = config@mode,
    order = config@order,
    group = config_column(data, config@group, "group"),
    breaks = config@breaks,
    bins = config@bins,
    bin_edges = config@bin_edges,
    normalization = config@normalization,
    density = config@density,
    n = config@n,
    bw = config@bw,
    bandwidth = config@bandwidth,
    kernel = config@kernel,
    adjust = config@adjust,
    fill_alpha = config@fill_alpha,
    na_rm = config@na_rm,
    palette = config@palette,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
