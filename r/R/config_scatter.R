# config_scatter.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The scatter chart's config, the setup function that builds it, and its
# compile method, which returns the build request the @rtemis/draw scatter
# builder draws. spec: draw/shared-builders#scatter

# %% ScatterConfig ----
#' Scatter Chart Configuration
#'
#' A serializable description of a scatter chart: which columns it binds, its
#' semantics, and its appearance. Build one with [setup_ScatterConfig()] rather
#' than calling this constructor directly.
#'
#' The data-binding properties (`x`, `y`, `size`, `group`, `color`, `hover`)
#' hold **column names**, not values. The values come from the `data` argument
#' to [draw()], or from the inherited `dat_path`.
#'
#' The fit members name a model, which rtemis.draw fits in R; the
#' `@rtemis/draw` scatter builder draws the points and the fitted curves.
#'
#' Margins are four scalars, one per side; [draw_scatter()] takes them as one
#' `margins` vector. Sides left `NULL` take the chart's layout.
#'
#' @param x Optional Character: Column drawn on the x axis: numeric, or a date
#'   or date-time.
#' @param y Optional Character: Numeric columns drawn on the y axis, one series
#'   each.
#' @param size Optional Character: Nonnegative numeric column that sets each
#'   symbol's area in proportion to its value, the largest value at 30 pixels
#'   across.
#' @param group Optional Character: Column to group and color points by.
#' @param color Optional Character: Numeric column mapped to a continuous color
#'   scale. Excludes `group`.
#' @param color_scale Character: Color scale of `color`: `"sequential"`, from
#'   the lowest to the highest value, or `"diverging"`, centered on zero.
#' @param colormap Optional Character: Colors of the color scale, low to
#'   high, at least two. `NULL` uses the theme's sign colors.
#' @param fit Optional Character: Model fitted to each series: `"glm"`,
#'   `"polynomial"`, `"loess"`, `"gam"`, or an rtemis supervised learning
#'   algorithm name such as `"LINAD"`, which requires the rtemis package.
#'   `NULL` draws no fit. See [draw_scatter()].
#' @param fit_params Optional Named list: Arguments of the model, e.g.
#'   `list(degree = 2)` for `"polynomial"` or `list(max_leaves = 8)` for
#'   `"LINAD"`. See [draw_scatter()].
#' @param fit_per_group Logical: Fit each series. `FALSE` fits one curve over
#'   every group.
#' @param fit_name Optional Character: Text added to the legend entry of each
#'   fitted series.
#' @param rug Logical: Show marginal marks along the x and y axes.
#' @param hover Optional Character: Column containing per-observation tooltip labels.
#' @param se Logical: If TRUE, shade the interval of the fit's standard error.
#' @param se_times Numeric `[0, Inf)`: Multiplier of the standard error bounding
#'   the interval.
#' @param rsq Logical: Add each fit's R-squared to its legend entry.
#' @param diagonal Logical: Draw the identity line within the axis limits.
#' @param diagonal_color Optional Character: Identity-line color.
#' @param n_fit Integer `[2, Inf)`: Points each fitted curve is evaluated at.
#' @param fit_alpha Numeric `[0, 1]`: Opacity of the interval.
#' @param point_size Numeric `(0, Inf)`: Symbol diameter in pixels of points
#'   without `size`.
#' @param zoom Logical: Enable Shift+wheel zoom, drag panning, and box-zoom
#'   controls on both axes. With `square` or `equal_axes`, the controls omit
#'   box zoom, which sets each axis independently.
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
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `ScatterConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm")
#' cfg@fit
ScatterConfig <- new_class(
  name = "ScatterConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    fit_properties(),
    list(
      type = prop_chart_type("scatter"),
      # -- data binding: column names, never values --------------------------
      x = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column drawn on the x axis: numeric, or a date or date-time."
      ),
      y = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Numeric columns drawn on the y axis, one series each."
      ),
      size = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Nonnegative numeric column that sets each symbol's area in",
          "proportion to its value."
        )
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column to group and color points by."
      ),
      color = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Numeric column mapped to a continuous color scale. Excludes group."
        )
      ),
      hover = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column containing literal observation tooltip labels."
      ),
      rug = prop_boolean(
        FALSE,
        description = "Show marginal marks on both axes."
      ),
      # -- semantics ---------------------------------------------------------
      diagonal = prop_boolean(
        FALSE,
        description = "Draw the identity line within the axis limits."
      ),
      diagonal_color = prop_string(
        NULL,
        nullable = TRUE,
        description = "Identity-line color. Unset uses a neutral gray."
      ),
      zoom = prop_boolean(
        FALSE,
        description = paste(
          "Enable Shift+wheel zoom, drag panning, and box-zoom controls on",
          "both axes. Square or equally scaled plots omit box zoom."
        )
      ),
      # -- appearance --------------------------------------------------------
      color_scale = prop_string(
        "sequential",
        enum = c("sequential", "diverging"),
        description = paste(
          "Color scale of color: sequential from the lowest to the highest",
          "value, or diverging, centered on zero."
        )
      ),
      colormap = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        min_items = 2L,
        description = paste(
          "Colors of the color scale, low to high. Unset uses the theme's",
          "sign colors."
        )
      ),
      point_size = prop_float(
        6,
        exclusive_min = 0,
        description = "Symbol diameter in pixels of points without size."
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
    c(CONFIG_LIMIT_RULES, list(FIT_PARAMS_RULE, COLOR_GROUP_RULE)),
    extra = config_ordered_limits
  )
) # /rtemis.draw::ScatterConfig


# %% SCATTER_ORIGIN_NAMES ----
# The properties an origin map covers: every settable one, so a complete map
# records the provenance of every member.
SCATTER_ORIGIN_NAMES <- setdiff(
  names(ScatterConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_ScatterConfig ----
#' Set up a Scatter Chart Configuration
#'
#' Builds a complete, validated [ScatterConfig] from the arguments given, every
#' other member at its default. **Every argument is optional**, so the
#' published schema requires nothing: an authored config is a subset of the
#' full set. [draw_scatter()] takes vectors instead, and requires `x` and `y`.
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
  color = NULL,
  color_scale = "sequential",
  colormap = NULL,
  fit = NULL,
  fit_params = NULL,
  fit_per_group = TRUE,
  se = TRUE,
  n_fit = 200L,
  fit_alpha = 0.25,
  point_size = 6,
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
  legend_title = NULL,
  fit_name = NULL,
  rug = FALSE,
  hover = NULL,
  point_alpha = NULL,
  zoom = FALSE
) {
  # An explicit `origin` (from read_chart_config()) is carried through a round
  # trip as given, so a defaulted value keeps its provenance.
  origin <- origin %||% chart_origin(match.call(), SCATTER_ORIGIN_NAMES)
  ScatterConfig(
    x = x,
    y = y,
    size = size,
    group = group,
    color = color,
    color_scale = color_scale,
    colormap = colormap,
    fit = fit,
    fit_params = fit_params,
    fit_per_group = fit_per_group,
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
    point_size = point_size,
    point_alpha = point_alpha,
    zoom = zoom,
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
    legend_title = legend_title,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_ScatterConfig


# %% resolve.ScatterConfig ----
# Fills in what the data determines: axis labels from the bound column names,
# axis limits from the rows drawn, and point opacity from their number. A
# property the author set is kept. `palette` stays NULL, so each interface
# applies its own.
method(resolve, ScatterConfig) <- function(config, data = NULL, ...) {
  keep <- if (!is.null(data) && !is.null(config@x) && !is.null(config@y)) {
    scatter_keep(config, data)
  }
  x <- if (!is.null(keep)) config_column(data, config@x, "x")[keep]
  ys <- if (!is.null(keep)) {
    unlist(
      lapply(config@y, function(y) config_column(data, y, "y")[keep]),
      use.names = FALSE
    )
  }
  numeric_x <- if (!is.null(x) && is.numeric(x)) x
  time_x <- if (!is.null(x) && is_time_axis(x)) time_axis_ms(x)
  # `square` with `equal_axes` sets both axes to one interval, recorded in the
  # document so that reading it back draws the same chart.
  common <- equal_axis_limits(
    numeric_x,
    ys,
    config@xlim,
    config@ylim,
    config@pad,
    config@square,
    config@equal_axes
  )
  config_derive(
    config,
    list(
      xlab = config@x,
      ylab = if (length(config@y) == 1L) config@y,
      xlim = common[["xlim"]] %||%
        if (!is.null(numeric_x %||% time_x)) {
          calc_limits(numeric_x %||% time_x, config@pad)
        },
      ylim = common[["ylim"]] %||%
        if (!is.null(ys)) calc_limits(ys, config@pad),
      point_alpha = if (!is.null(keep)) auto_alpha(sum(keep) * length(config@y))
    )
  )
}


# %% scatter_keep ----
#' The rows a scatter chart draws
#'
#' A row is drawn when its x, its group, its size, and its color are present;
#' each `y` column then draws the rows where it has a value.
#'
#' @param config [ScatterConfig].
#' @param data data.frame: The chart data.
#'
#' @return Logical, one per row of `data`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scatter_keep <- function(config, data) {
  keep <- !is.na(config_column(data, config@x, "x"))
  for (name in c(config@group, config@size, config@color)) {
    keep <- keep & !is.na(data[[name]])
  }
  keep &
    Reduce(
      `|`,
      lapply(config@y, function(y) !is.na(config_column(data, y, "y")))
    )
} # /rtemis.draw::scatter_keep


# %% compile.ScatterConfig ----
# A build request for the @rtemis/draw scatter builder: the bound columns, and
# the curves `fit` names, fitted in R.
method(compile, ScatterConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  if (is.null(config@x) || is.null(config@y)) {
    abort(
      "A ScatterConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  columns <- unique(c(
    config@x,
    config@y,
    config@group,
    config@size,
    config@color,
    config@hover
  ))
  for (name in columns) {
    config_column(data, name, if (name %in% config@y) "y" else "x")
  }
  BuildRequest(
    type = "scatter",
    data = tabular_json(
      data,
      columns,
      fits = fit_curves(config, data, keep = scatter_keep(config, data))
    ),
    config = config_members(config),
    appearance = list()
  )
}


# %% render_meta.ScatterConfig ----
# The widget and SVG export place the legend from the legend members; with
# `color`, the legend is the color scale.
method(render_meta, ScatterConfig) <- function(config, option) {
  c(
    legend_meta(
      config@legend_position,
      config@legend_placement,
      config@legend_title
    ),
    if (!is.null(config@color)) list(legendTarget = "visualMap")
  )
}
