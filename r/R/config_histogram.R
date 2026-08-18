# config_histogram.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The histogram's config. Same binding shape as density -- one bound column plus
# an optional grouping column -- differing only in its semantics.

# %% HistogramConfig ----
#' Histogram Configuration
#'
#' A serializable description of a histogram. Build one with
#' [setup_HistogramConfig()] rather than calling this constructor directly.
#'
#' `x` names the column to bin. `group` optionally names a column to split it
#' by, drawing one series per level.
#'
#' @param x Optional Character: Column to bin.
#' @param group Optional Character: Column to split the bins by.
#' @param breaks Character: Binning rule, passed to [graphics::hist()]. One of
#'   the algorithm names it accepts, e.g. `"Sturges"`, `"Scott"`, `"FD"`.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
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
  properties = list(
    type = prop_chart_type("histogram"),
    # -- data binding ------------------------------------------------------
    x = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column to bin."
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
      description = "Binning rule, passed to graphics::hist()."
    ),
    # -- appearance --------------------------------------------------------
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Series colors, overriding the theme palette. NULL uses the theme's."
    ),
    xlab = prop_string(
      NULL,
      nullable = TRUE,
      description = "X axis label. NULL derives it from the data."
    ),
    ylab = prop_string(
      NULL,
      nullable = TRUE,
      description = "Y axis label. NULL derives it from the data."
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
) # /rtemis.draw::HistogramConfig


# %% HISTOGRAM_ORIGIN_NAMES ----
HISTOGRAM_ORIGIN_NAMES <- setdiff(
  names(HistogramConfig@properties),
  c("type", "origin", "writer")
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
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), HISTOGRAM_ORIGIN_NAMES)
  HistogramConfig(
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
      xlab = config@x
    )
  )
}


# %% compile.HistogramConfig ----
method(compile, HistogramConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  config <- resolve(config, data = dat)
  x <- config_column(dat, config@x, "x")
  if (is.null(x)) {
    abort(
      "A HistogramConfig needs `x` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  histogram_option(
    x = x,
    group = config_column(dat, config@group, "group"),
    breaks = config@breaks,
    palette = config@palette,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
