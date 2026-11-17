# draw_scatter.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The vector entry point of the scatter chart. It builds a ScatterConfig and a
# build request for the @rtemis/draw scatter builder, the one rtemislive uses.
# spec: draw/shared-builders#scatter

# %% draw_scatter ----
#' Draw a Scatter Plot
#'
#' Scatter plot of `y` against `x`, with optional groups, symbol sizes, a
#' continuous color scale, and fitted curves with their intervals. The
#' `@rtemis/draw` scatter builder, which rtemislive also uses, draws the chart;
#' rtemis.draw computes the fits in R.
#'
#' @details
#' # Fits
#'
#' `fit` names the model fitted to each series: `"glm"` ([stats::glm()]),
#' `"polynomial"` (a GLM of a raw polynomial of `x` of degree
#' `fit_params$degree`, default 3), `"loess"` ([stats::loess()] with span
#' `fit_params$span`, default 0.75), or `"gam"` ([mgcv::gam()] with a smooth
#' of `x`, its basis dimension `fit_params$k`). Any other name, matched
#' case-insensitively, is an rtemis supervised learning algorithm, such as
#' `"LINAD"`, `"CART"`, or `"LightGBM"`, trained with its default
#' hyperparameters; this requires the rtemis package (>= 1.4.1). With `group`,
#' each group is fitted separately unless `fit_per_group = FALSE`, which fits
#' one curve over all groups.
#'
#' # Square and equally scaled plots
#'
#' `square` and `equal_axes` are separate requests. `square` sets the *shape*
#' of the plotting box: equal height and width in pixels, measured on the box
#' itself, excluding axis labels and margins. `equal_axes` sets the *scale*:
#' one data unit occupies the same number of pixels on both axes, so a line of
#' slope 1 runs at 45 degrees.
#'
#' Either works alone. `equal_axes` on a chart whose x spans ten units and
#' whose y spans one gives a wide, short box.
#'
#' Both together set the limits, since the only square box in which both axes
#' are equally scaled is one whose axes span the same interval: over all the
#' values when neither limit is given, or the given one when exactly one is
#' given. Both `xlim` and `ylim` given as different intervals is an error.
#'
#' The widget solves the box from the width of its container, and again on
#' every resize.
#'
#' @param x Numeric: X values.
#' @param y Numeric: Y values.
#' @param size Optional Numeric: Nonnegative values, one per point, that set
#'   each symbol's area in proportion to the value, the largest value at five
#'   times `point_size` across (30 pixels at the default).
#' @param group Optional Atomic vector or single-column data frame: Grouping
#'   variable for multiple series. Series, colors, and legend entries follow
#'   factor level order, or first appearance for other vectors.
#' @param color Optional Numeric: Values, one per point, mapped to a continuous
#'   color scale. Excludes `group`.
#' @param color_scale Character \{"sequential", "diverging"\}: Color scale of
#'   `color`: from the lowest to the highest value, or centered on zero.
#' @param colormap Optional Character: Colors of the color scale, low to
#'   high, at least two. `NULL` uses the theme's sign colors.
#' @param color_title Optional Character: Title of the color scale.
#' @param size_title Optional Character: Title of the size key.
#' @param fit Optional Character: Model fitted to `y` on `x` and drawn as a
#'   curve. See Details. `NULL` draws no fit.
#' @param fit_params Optional Named list: Arguments of the model:
#'   `list(degree = 2)` for `"polynomial"`, `list(span = 0.5)` for `"loess"`,
#'   `list(k = 5)` for `"gam"`, arguments of [stats::glm()] for `"glm"`, e.g.
#'   `list(family = "poisson")`, and arguments of the rtemis setup function
#'   otherwise, e.g. `list(max_leaves = 8)` for `"LINAD"` (see
#'   `?rtemis::setup_LINAD`). `NULL` uses the model's defaults.
#' @param fit_per_group Logical: Fit each group. `FALSE` fits one curve over
#'   every group, drawn in gray under its own legend entry.
#' @param se Logical: Shade the interval of `se_times` standard errors around
#'   each fit. Algorithms without standard errors (every rtemis algorithm
#'   except GLM, GAM, and a few others) draw the curve alone.
#' @param se_times Numeric `[0, Inf)`: Standard-error multiplier for the
#'   interval.
#' @param fit_name Optional Character: Text added to the legend entry of each
#'   fitted series.
#' @param rug Logical: Draw marginal marks on both axes.
#' @param hover Optional Character: One literal tooltip label per observation.
#' @param rsq Logical: Add each fit's R-squared to its legend entry. This
#'   describes the fit, not predictive performance against the identity line.
#'   A constant response has undefined R-squared, labeled `NA`.
#' @param diagonal Logical: Draw a dashed identity line within the axis limits.
#' @param diagonal_color Optional Character: Identity-line color.
#' @param fit_alpha Numeric `[0, 1]`: Opacity of the interval.
#' @param point_size Numeric `(0, Inf)`: Symbol diameter in pixels of points
#'   without `size`; with `size`, the largest value's symbol is five times it
#'   across.
#' @param point_alpha Optional Numeric `[0, 1]`: Point opacity. `NULL` sets it
#'   from the number of points drawn, as a guard against overplotting: 0.75 at
#'   10 points, 0.57 at 150, 0.45 at 1,000 and 0.3 at 10,000, within
#'   `[0.15, 0.9]`.
#' @param n_fit Integer `[2, Inf)`: Points each fitted curve is evaluated at.
#' @param zoom Logical: `TRUE` adds Shift+wheel zoom and drag panning on both
#'   axes and a top-right toolbox with box-zoom, undo, and reset controls.
#'   Wheel zoom and panning scale both axes by the same factor; with `square`
#'   or `equal_axes`, the toolbox omits box zoom, which sets each axis
#'   independently.
#' @param palette Optional Character: Series colors, one per group in order,
#'   overriding the theme palette for this chart.
#' @param xlim Optional Numeric \[length 2\]: X-axis limits `c(min, max)`.
#'   Defaults to the range of `x` extended by `pad`.
#' @param ylim Optional Numeric \[length 2\]: Y-axis limits `c(min, max)`.
#'   Defaults to the range of `y` extended by `pad`.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each
#'   axis by when `xlim` / `ylim` are not given. The default matches base R's
#'   `xaxs = "r"`, which extends the range by 4% at each end.
#' @param square Logical: If TRUE, draw the plotting box square -- equal height
#'   and width in pixels, excluding axis labels and margins.
#' @param equal_axes Logical: If TRUE, give one data unit the same size in
#'   pixels on both axes. See Details.
#' @param xlab Optional Character: X-axis title.
#' @param ylab Optional Character: Y-axis title.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file
#'   via [save_drawing()].
#' @inheritParams draw_line legend_position legend_placement legend_title
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Weight against fuel economy, with a linear fit and its interval.
#' draw_scatter(
#'   mtcars$wt,
#'   mtcars$mpg,
#'   fit = "glm",
#'   xlab = "Weight (1,000 lb)",
#'   ylab = "Miles per gallon"
#' )
#'
#' # True versus predicted: square box, equal scale, identity line at 45 deg.
#' draw_scatter(
#'   x = c(1, 2, 3, 4),
#'   y = c(1.1, 1.9, 3.4, 3.7),
#'   square = TRUE,
#'   equal_axes = TRUE,
#'   diagonal = TRUE,
#'   xlab = "True",
#'   ylab = "Predicted"
#' )
draw_scatter <- function(
  x,
  y,
  size = NULL,
  group = NULL,
  fit = NULL,
  fit_params = NULL,
  se = TRUE,
  fit_alpha = 0.25,
  n_fit = 200L,
  palette = NULL,
  xlim = NULL,
  ylim = NULL,
  pad = DEFAULT_PAD,
  square = FALSE,
  equal_axes = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
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
  zoom = FALSE,
  color = NULL,
  color_scale = "sequential",
  colormap = NULL,
  fit_per_group = TRUE,
  point_size = 6,
  color_title = NULL,
  size_title = NULL
) {
  request <- scatter_request(
    x = x,
    y = y,
    size = size,
    group = group,
    color = color,
    color_scale = color_scale,
    colormap = colormap,
    color_title = color_title,
    size_title = size_title,
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
    fit_alpha = fit_alpha,
    point_size = point_size,
    point_alpha = point_alpha,
    n_fit = n_fit,
    palette = palette,
    xlim = xlim,
    ylim = ylim,
    pad = pad,
    square = square,
    equal_axes = equal_axes,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margins = margins,
    zoom = zoom,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    request,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
} # /rtemis.draw::draw_scatter


# %% scatter_request ----
#' The build request of a scatter chart from vectors
#'
#' Validates the vectors (`scatter_input()` drops incomplete rows), puts them
#' in the columns `x`, `y`, `group`, `size`, `color`, and `hover`, builds the
#' [ScatterConfig], and fits the curves `fit` names.
#'
#' @inheritParams draw_scatter
#'
#' @return [BuildRequest] of type `scatter`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scatter_request <- function(
  x,
  y,
  size = NULL,
  group = NULL,
  color = NULL,
  color_scale = "sequential",
  colormap = NULL,
  color_title = NULL,
  size_title = NULL,
  fit = NULL,
  fit_params = NULL,
  fit_per_group = TRUE,
  fit_name = NULL,
  rug = FALSE,
  hover = NULL,
  se = TRUE,
  se_times = 1.96,
  rsq = FALSE,
  diagonal = FALSE,
  diagonal_color = NULL,
  fit_alpha = 0.25,
  point_size = 6,
  point_alpha = NULL,
  n_fit = 200L,
  palette = NULL,
  xlim = NULL,
  ylim = NULL,
  pad = DEFAULT_PAD,
  square = FALSE,
  equal_axes = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margins = DEFAULT_MARGINS,
  zoom = FALSE,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  check_zoom_flag(zoom)
  margin <- pixel_margins(margins)
  input <- scatter_input(x, y, group, size, hover, color)
  validate_axis_lim(xlim, "xlim")
  validate_axis_lim(ylim, "ylim")
  # The fixed-aspect box is solved in the browser from the four margins, and
  # `square` with `equal_axes` puts both axes on one interval; both are
  # checked here so that an error stops the call rather than the render.
  equal_axis_limits(
    input[["x"]],
    input[["y"]],
    xlim,
    ylim,
    pad,
    square,
    equal_axes
  )
  if (
    (isTRUE(square) || isTRUE(equal_axes)) &&
      any(vapply(margin, is.null, logical(1)))
  ) {
    abort(
      "`square` and `equal_axes` need all four margins in pixels; set ",
      "`margins = c(top = , right = , bottom = , left = )`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  data <- Filter(
    Negate(is.null),
    list(
      x = input[["x"]],
      y = input[["y"]],
      # A logical group is labeled as R prints it: TRUE or FALSE.
      group = if (is.logical(input[["group"]])) {
        as.character(input[["group"]])
      } else {
        input[["group"]]
      },
      size = input[["size"]],
      color = input[["color"]],
      hover = input[["hover"]]
    )
  )
  config <- setup_ScatterConfig(
    x = "x",
    y = "y",
    size = if (!is.null(data[["size"]])) "size",
    group = if (!is.null(data[["group"]])) "group",
    color = if (!is.null(data[["color"]])) "color",
    color_scale = color_scale,
    colormap = colormap,
    color_title = color_title,
    size_title = size_title,
    hover = if (!is.null(data[["hover"]])) "hover",
    fit = fit,
    fit_params = fit_params,
    fit_per_group = fit_per_group,
    fit_name = fit_name,
    rug = rug,
    se = se,
    se_times = se_times,
    rsq = rsq,
    diagonal = diagonal,
    diagonal_color = diagonal_color,
    n_fit = n_fit,
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
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  BuildRequest(
    type = "scatter",
    data = tabular_json(data, names(data), fits = fit_curves(config, data)),
    config = config_members(config),
    appearance = list()
  )
} # /rtemis.draw::scatter_request
