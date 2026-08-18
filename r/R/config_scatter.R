# config_scatter.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The scatter chart's config, its compile method, and the setup function that
# builds it. This is the reference implementation for the other chart types: the
# property groups, the naming, and the split between what a config states and
# what `draw()` supplies are all meant to be copied from here.

# %% ScatterConfig ----
#' Scatter Chart Configuration
#'
#' A serializable description of a scatter chart: which columns it binds, its
#' semantics, and its appearance. Build one with [setup_ScatterConfig()] rather
#' than calling this constructor directly.
#'
#' The data-binding properties (`x`, `y`, `size`, `group`) hold **column names**,
#' not values. The values come from the `data` argument to [draw()], or from the
#' inherited `dat_path`.
#'
#' Margins are declared as four scalars rather than one named vector: that is
#' what states cleanly in a schema, while [draw_scatter()] keeps the convenient
#' `margins` vector. Sides left `NULL` fall back to the chart's own layout.
#'
#' @param x,y Optional Character: Columns drawn on each axis.
#' @param size Optional Character: Column giving per-point size.
#' @param group Optional Character: Column to group and color points by.
#' @param fit Optional Character \{"glm", "gam"\}: Fit to overlay. `NULL` draws
#'   no fit.
#' @param se Logical: If TRUE, shade the fit standard-error band.
#' @param n_fit Integer `[2, Inf)`: Points used to draw the fit line.
#' @param fit_alpha Numeric `[0, 1]`: Opacity of the standard-error band.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each axis
#'   by when `xlim` / `ylim` are not given. The default matches base R's
#'   `xaxs = "r"`, which extends the range by 4% at each end.
#' @param xlim,ylim Optional Numeric: Axis limits, length 2. `NULL` derives them
#'   from the data, padded by `pad`.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @param dat_path Optional Character: Path to the data, read at draw time. The
#'   serializable alternative to passing `data` to [draw()].
#' @inheritParams ChartConfig
#'
#' @return `ScatterConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm")
#' cfg@type
ScatterConfig <- new_class(
  name = "ScatterConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("scatter"),
    # -- data binding: column names, never values --------------------------
    x = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column drawn on the x axis."
    ),
    y = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column drawn on the y axis."
    ),
    size = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column giving per-point size."
    ),
    group = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column to group and color points by."
    ),
    # -- semantics ---------------------------------------------------------
    fit = prop_string(
      NULL,
      enum = c("glm", "gam"),
      nullable = TRUE,
      description = "Fit to overlay. NULL draws no fit."
    ),
    se = prop_boolean(
      TRUE,
      description = "Shade the fit standard-error band."
    ),
    n_fit = prop_integer(
      200L,
      min = 2L,
      description = "Points used to draw the fit line."
    ),
    # -- appearance --------------------------------------------------------
    fit_alpha = prop_float(
      0.25,
      min = 0,
      max = 1,
      description = "Opacity of the standard-error band."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Series colors, overriding the theme palette. NULL uses the theme's."
    ),
    pad = prop_float(
      DEFAULT_PAD,
      min = 0,
      description = paste(
        "Fraction of the data range to extend each axis by when the limits",
        "are not given."
      )
    ),
    xlim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "X axis limits. NULL derives them from the data."
    ),
    ylim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Y axis limits. NULL derives them from the data."
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
) # /rtemis.draw::ScatterConfig


# %% SCATTER_ORIGIN_NAMES ----
# The properties an origin map covers: every settable one, so a complete map is
# only producible by having actually resolved them all.
SCATTER_ORIGIN_NAMES <- setdiff(
  names(ScatterConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_ScatterConfig ----
#' Set up a Scatter Chart Configuration
#'
#' The seam between convenient input and a complete, validated object: pass the
#' handful of things you care about, get back a `ScatterConfig` with everything
#' else at its default.
#'
#' **Every argument is optional**, which is what lets the published schema
#' require nothing: an authored config is a subset of the full set, and the
#' interface fills in the rest. This is a different entry point from
#' [draw_scatter()], which takes vectors and keeps its mandatory `x` and `y`.
#'
#' @inheritParams ScatterConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config, as
#'   `name` and `version`.
#'
#' @return [ScatterConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm", title = "Cars")
#' draw(cfg, data = mtcars)
setup_ScatterConfig <- function(
  x = NULL,
  y = NULL,
  size = NULL,
  group = NULL,
  fit = NULL,
  se = TRUE,
  n_fit = 200L,
  fit_alpha = 0.25,
  palette = NULL,
  pad = DEFAULT_PAD,
  xlim = NULL,
  ylim = NULL,
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
  # Which values the caller chose, versus which this function filled in. An
  # explicit `origin` (from read_chart_config()) wins: provenance is carried
  # through a round trip, never recomputed, or a defaulted value would harden
  # into a user choice on the first hop.
  origin <- origin %||% chart_origin(match.call(), SCATTER_ORIGIN_NAMES)
  ScatterConfig(
    x = x,
    y = y,
    size = size,
    group = group,
    fit = fit,
    se = se,
    n_fit = as.integer(n_fit),
    fit_alpha = fit_alpha,
    palette = palette,
    pad = pad,
    xlim = xlim,
    ylim = ylim,
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
} # /rtemis.draw::setup_ScatterConfig


# %% resolve.ScatterConfig ----
# Fill in what the data determines: axis labels from the bound column names and
# axis limits from the values. Nothing here touches the display surface, and
# nothing is derived for a property the author already set.
#
# `palette` is deliberately NOT resolved: it belongs to the interface, and baking
# one interface's palette into a document would stop another from applying its
# own. It is left NULL, meaning "use your palette".
method(resolve, ScatterConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  x <- config_column(dat, config@x, "x")
  y <- config_column(dat, config@y, "y")
  config_derive(
    config,
    list(
      # Labels come from names; a config naming no column derives no label.
      xlab = config@x,
      ylab = config@y,
      xlim = if (!is.null(x)) calc_limits(x, config@pad),
      ylim = if (!is.null(y)) calc_limits(y, config@pad)
    )
  )
}


# %% compile.ScatterConfig ----
# Translate a config into the render option. `resolve()` runs first, so every
# derivable value is already present and this is a straight hand-off to the same
# builder `draw_scatter()` uses -- one implementation, two entry points.
method(compile, ScatterConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  config <- resolve(config, data = dat)
  x <- config_column(dat, config@x, "x")
  y <- config_column(dat, config@y, "y")
  if (is.null(x) || is.null(y)) {
    abort(
      "A ScatterConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  scatter_option(
    x = x,
    y = y,
    size = config_column(dat, config@size, "size"),
    group = config_column(dat, config@group, "group"),
    fit = config@fit,
    se = config@se,
    fit_alpha = config@fit_alpha,
    n_fit = config@n_fit,
    palette = config@palette,
    pad = config@pad,
    xlim = config@xlim,
    ylim = config@ylim,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
