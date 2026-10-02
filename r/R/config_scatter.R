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
#' @param fit Optional Character: Fit to overlay: `"glm"`, `"gam"`, or an rtemis
#'   supervised learning algorithm name such as `"LINAD"`, which requires the
#'   rtemis package. `NULL` draws no fit. See [draw_scatter()].
#' @param fit_params Optional Named list: Arguments passed to the learner named
#'   in `fit`, e.g. `list(max_leaves = 8)` for `"LINAD"`. See [draw_scatter()].
#' @param fit_name Optional Character: Label for fitted layers.
#' @param rug Logical: Show marginal marks along the x and y axes.
#' @param hover Optional Character: Column containing per-observation tooltip labels.
#' @param se Logical: If TRUE, shade the fit standard-error band.
#' @param se_times Numeric `[0, Inf)`: Multiplier for the fitted standard error.
#' @param rsq Logical: Include the fitted model's R-squared in series labels.
#' @param diagonal Logical: Draw the identity line within the axis limits.
#' @param diagonal_color Optional Character: Identity-line color.
#' @param n_fit Integer `[2, Inf)`: Points used to draw the fit line.
#' @param fit_alpha Numeric `[0, 1]`: Opacity of the standard-error band.
#' @param point_alpha Optional Numeric `[0, 1]`: Point opacity. `NULL` sets it
#'   from the number of points drawn, falling from 0.9 for a handful of points
#'   to 0.15 for 100,000 or more, so that overlapping points stay visible.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each axis
#'   by when `xlim` / `ylim` are not given. The default matches base R's
#'   `xaxs = "r"`, which extends the range by 4% at each end.
#' @param square Logical: If TRUE, draw the plotting box square -- equal height
#'   and width in pixels, excluding axis labels and margins.
#' @param equal_axes Logical: If TRUE, give one data unit the same size in
#'   pixels on both axes. Set with `square` for a plot that is both, such as a
#'   true-versus-predicted plot whose identity line runs at 45 degrees; the two
#'   axes are then made to span the same interval.
#' @param xlim,ylim Optional Numeric: Axis limits, length 2. `NULL` derives them
#'   from the data, padded by `pad`.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @param dat_path Optional Character: Path to the data, read at draw time. The
#'   serializable alternative to passing `data` to [draw()].
#' @inheritParams draw_line legend_position legend_placement
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
  properties = c(
    legend_properties(),
    list(
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
      hover = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column containing literal observation tooltip labels."
      ),
      fit_name = prop_string(
        NULL,
        nullable = TRUE,
        description = "Label for fitted layers."
      ),
      rug = prop_boolean(
        FALSE,
        description = "Show marginal marks on both axes."
      ),
      # -- semantics ---------------------------------------------------------
      fit = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Fit to overlay: glm, gam, or an rtemis supervised learning",
          "algorithm name. Unset draws no fit."
        )
      ),
      fit_params = prop_bag(
        nullable = TRUE,
        description = paste(
          "Arguments passed to the learner named in fit: to glm() or gam()",
          "(k sets each smooth's basis dimension), or to the rtemis",
          "algorithm's setup function. Unset uses the learner's defaults."
        )
      ),
      se = prop_boolean(
        TRUE,
        description = "Shade the fit standard-error band."
      ),
      se_times = prop_float(
        1.96,
        min = 0,
        description = "Multiplier for the fitted standard error."
      ),
      rsq = prop_boolean(
        FALSE,
        description = "Include the fitted model's R-squared in series labels."
      ),
      diagonal = prop_boolean(
        FALSE,
        description = "Draw the identity line within the axis limits."
      ),
      diagonal_color = prop_string(
        NULL,
        nullable = TRUE,
        description = "Identity-line color. Unset uses a neutral gray."
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
      point_alpha = prop_float(
        NULL,
        min = 0,
        max = 1,
        nullable = TRUE,
        description = paste(
          "Point opacity. Unset sets it from the number of points drawn, so",
          "that overlapping points stay visible."
        )
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Series colors, overriding the theme palette. Unset uses the theme's."
      ),
      square = prop_boolean(
        FALSE,
        description = paste(
          "Draw the plotting box square: equal height and width in pixels,",
          "excluding axis labels and margins."
        )
      ),
      equal_axes = prop_boolean(
        FALSE,
        description = paste(
          "Give one data unit the same size in pixels on both axes. Combined",
          "with `square`, both axes are made to span the same interval."
        )
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
        description = "X axis limits. Unset derives them from the data."
      ),
      ylim = prop_float(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        min_items = 2L,
        description = "Y axis limits. Unset derives them from the data."
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
  validator = config_validator(
    c(CONFIG_LIMIT_RULES, list(FIT_PARAMS_RULE)),
    extra = config_ordered_limits
  )
) # /rtemis.draw::ScatterConfig


# %% SCATTER_ORIGIN_NAMES ----
# The properties an origin map covers: every settable one, so a complete map is
# only producible by having actually resolved them all.
SCATTER_ORIGIN_NAMES <- setdiff(
  names(ScatterConfig@properties),
  c("type", PROVENANCE_PROPS)
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
  fit_params = NULL,
  se = TRUE,
  n_fit = 200L,
  fit_alpha = 0.25,
  palette = NULL,
  pad = DEFAULT_PAD,
  square = FALSE,
  equal_axes = FALSE,
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
  writer = NULL,
  se_times = 1.96,
  rsq = FALSE,
  diagonal = FALSE,
  diagonal_color = NULL,
  legend_position = "top",
  legend_placement = "outside",
  fit_name = NULL,
  rug = FALSE,
  hover = NULL,
  point_alpha = NULL
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
    fit_params = fit_params,
    fit_name = fit_name,
    rug = rug,
    hover = hover,
    se = se,
    se_times = se_times,
    rsq = rsq,
    diagonal = diagonal,
    diagonal_color = diagonal_color,
    n_fit = clean_int(n_fit),
    fit_alpha = fit_alpha,
    point_alpha = point_alpha,
    palette = palette,
    pad = pad,
    square = square,
    equal_axes = equal_axes,
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
    legend_position = legend_position,
    legend_placement = legend_placement,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_ScatterConfig


# %% resolve.ScatterConfig ----
# Fill in what the data determines: axis labels from the bound column names,
# axis limits from the values, and point opacity from how many there are. Nothing here touches the display surface, and
# nothing is derived for a property the author already set.
#
# `palette` is deliberately NOT resolved: it belongs to the interface, and baking
# one interface's palette into a document would stop another from applying its
# own. It is left NULL, meaning "use your palette".
method(resolve, ScatterConfig) <- function(config, data = NULL, ...) {
  x <- config_column(data, config@x, "x")
  y <- config_column(data, config@y, "y")
  # `square` + `equal_axes` is a statement about the limits, so it is settled
  # here rather than in the builder: the document then records the interval the
  # chart is actually drawn on, and reading it back draws the same chart.
  common <- equal_axis_limits(
    x,
    y,
    config@xlim,
    config@ylim,
    config@pad,
    config@square,
    config@equal_axes
  )
  config_derive(
    config,
    list(
      # Labels come from names; a config naming no column derives no label.
      xlab = config@x,
      ylab = config@y,
      xlim = common[["xlim"]] %||% if (!is.null(x)) calc_limits(x, config@pad),
      ylim = common[["ylim"]] %||% if (!is.null(y)) calc_limits(y, config@pad),
      point_alpha = if (!is.null(x) && !is.null(y)) {
        # Count what is drawn: scatter_input() drops incomplete rows.
        group <- config_column(data, config@group, "group")
        keep <- !is.na(x) & !is.na(y)
        if (!is.null(group)) {
          keep <- keep & !is.na(group)
        }
        auto_alpha(sum(keep))
      }
    )
  )
}


# %% compile.ScatterConfig ----
# Translate a config into the render option. `resolve()` runs first, so every
# derivable value is already present and this is a straight hand-off to the same
# builder `draw_scatter()` uses -- one implementation, two entry points.
method(compile, ScatterConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  x <- config_column(data, config@x, "x")
  y <- config_column(data, config@y, "y")
  if (is.null(x) || is.null(y)) {
    abort(
      "A ScatterConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  scatter_option(
    x = x,
    y = y,
    size = config_column(data, config@size, "size"),
    group = config_column(data, config@group, "group"),
    fit = config@fit,
    fit_params = config@fit_params,
    fit_name = config@fit_name,
    rug = config@rug,
    hover = config_column(data, config@hover, "hover"),
    se = config@se,
    se_times = config@se_times,
    rsq = config@rsq,
    diagonal = config@diagonal,
    diagonal_color = config@diagonal_color,
    fit_alpha = config@fit_alpha,
    point_alpha = config@point_alpha,
    n_fit = config@n_fit,
    palette = config@palette,
    pad = config@pad,
    square = config@square,
    equal_axes = config@equal_axes,
    xlim = config@xlim,
    ylim = config@ylim,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}


# %% render_meta.ScatterConfig ----
# A square or equally-scaled plot is solved in the browser, from the ratio and
# padding derived here. The limits are already reconciled by resolve(), so this
# only reads what compile() produced.
method(render_meta, ScatterConfig) <- function(config, option) {
  aspect_meta(
    option,
    square = config@square,
    equal_axes = config@equal_axes
  )
}
