# config_density.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Density settings shared by standalone curves and histogram overlays.
# One or more numeric columns may be split by a grouping column.

# %% DensityConfig ----
#' Density Chart Configuration
#'
#' A serializable description of a kernel density chart. Build one with
#' [setup_DensityConfig()] rather than calling this constructor directly.
#'
#' `x` names one or more numeric columns. `group` optionally names a
#' column to split it by, drawing one curve per level. The output uses
#' `LineSeriesOption` in ECharts `src/chart/line/LineSeries.ts`.
#'
#' @param x Optional Character: Columns to estimate, one distribution per column.
#' @param group Optional Character: Column to split the estimate by.
#' @param n Integer `[2, Inf)`: Points at which the density is estimated.
#' @inheritParams draw_density bw bandwidth kernel adjust fill_alpha
#' @param na_rm Logical: If TRUE, drop `NA` values before estimating.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams draw_line legend_position legend_placement
#' @inheritParams ChartConfig
#'
#' @return `DensityConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_DensityConfig(x = "mpg")@type
DensityConfig <- new_class(
  name = "DensityConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("density"),
      # -- data binding ------------------------------------------------------
      x = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Columns to estimate the density of."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column to split the estimate by, one curve per level."
      ),
      # -- semantics ---------------------------------------------------------
      n = prop_integer(
        512L,
        min = 2L,
        description = "Points at which the density is estimated."
      ),
      bw = prop_string(
        "nrd0",
        enum = c("nrd0", "nrd", "ucv", "bcv", "SJ", "SJ-ste", "SJ-dpi"),
        description = "Bandwidth selector, used when bandwidth is unset."
      ),
      bandwidth = prop_float(
        NULL,
        nullable = TRUE,
        exclusive_min = 0,
        description = "Explicit density bandwidth in measurement units."
      ),
      kernel = prop_string(
        "gaussian",
        enum = c(
          "gaussian",
          "epanechnikov",
          "rectangular",
          "triangular",
          "biweight",
          "cosine",
          "optcosine"
        ),
        description = "Density smoothing kernel."
      ),
      adjust = prop_float(
        1,
        exclusive_min = 0,
        description = "Bandwidth multiplier."
      ),
      fill_alpha = prop_float(
        0.25,
        min = 0,
        max = 1,
        description = "Distribution fill opacity."
      ),
      na_rm = prop_boolean(
        TRUE,
        description = "Drop missing values before estimating."
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
  )
) # /rtemis.draw::DensityConfig


# %% DENSITY_ORIGIN_NAMES ----
DENSITY_ORIGIN_NAMES <- setdiff(
  names(DensityConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_DensityConfig ----
#' Set up a Density Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams DensityConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [DensityConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw(setup_DensityConfig(x = "mpg"), data = mtcars)
setup_DensityConfig <- function(
  x = NULL,
  group = NULL,
  n = 512L,
  bw = "nrd0",
  na_rm = TRUE,
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
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = 0.25
) {
  origin <- origin %||% chart_origin(match.call(), DENSITY_ORIGIN_NAMES)
  check_integer_scalar(n)
  # Normalize the R convenience form to separate portable selector/value fields.
  if (is.numeric(bw)) {
    if (!is.null(bandwidth)) {
      abort(
        "Supply only one numeric bandwidth: `bw` or `bandwidth`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    bandwidth <- bw
    bw <- "nrd0"
    origin[["bw"]] <- "default"
    origin[["bandwidth"]] <- "user"
  }
  DensityConfig(
    x = x,
    group = group,
    n = as.integer(n),
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
} # /rtemis.draw::setup_DensityConfig


# %% resolve.DensityConfig ----
# Only the x label: it is the name of the bound column. The y axis shows an estimated density,
# which has no name in the data -- and a constant like "Density" would be an
# invented default, not a derivation. Labels come from names; no name, no label.
# If such a label is wanted it belongs in the builder, applying to the vector
# path too, rather than being written into the document by one of them.
method(resolve, DensityConfig) <- function(config, data = NULL, ...) {
  config_derive(
    config,
    list(
      xlab = if (length(config@x) == 1L) config@x else NULL
    )
  )
}


# %% compile.DensityConfig ----
# `verbosity` is a render target, so it is not a config property: the config path
# uses the builder's default, exactly as `draw_density()` does when the caller
# says nothing.
method(compile, DensityConfig) <- function(config, data = NULL, ...) {
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
      "A DensityConfig needs `x` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  density_option(
    x = x,
    group = config_column(data, config@group, "group"),
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
