# config_line.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The line chart's config. Multi-series like bar, plus a background-band binding
# (`blocks`) and axis limits like scatter.
#
# `line_style` is deliberately **not** a property: it is a [LineStyle] object,
# part of the rendering IR that mirrors ECharts, and giving it a wire form would
# mean schematizing that IR. A caller wanting it builds the option directly.

# %% LineConfig ----
#' Line Chart Configuration
#'
#' A serializable description of a line chart. Build one with
#' [setup_LineConfig()] rather than calling this constructor directly.
#'
#' `x` names the column along the horizontal axis. `y` names **one or more**
#' columns, one line each, as [BarConfig] does.
#'
#' @param x Optional Character: Column along the horizontal axis.
#' @param y Optional Character: Columns to plot, one line each.
#' @param blocks Optional Character: Column whose contiguous runs shade vertical
#'   background bands.
#' @param smooth Logical: Draw smoothed lines.
#' @param area Logical: Fill the area under each line.
#' @param points Logical: Show a marker at each data value.
#' @param zoom Logical: Enable the zoom control.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param block_color Optional Character: Band colors.
#' @param block_opacity Numeric `[0, 1]`: Band opacity.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each axis
#'   by when the limits are not given.
#' @param xlim,ylim Optional Numeric: Axis limits, length 2. `NULL` derives them
#'   from the data, padded by `pad`.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams ChartConfig
#'
#' @return `LineConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_LineConfig(x = "wt", y = "mpg")@type
LineConfig <- new_class(
  name = "LineConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("line"),
    # -- data binding ------------------------------------------------------
    x = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column along the horizontal axis."
    ),
    y = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Columns to plot, one line each."
    ),
    blocks = prop_string(
      NULL,
      nullable = TRUE,
      description = paste(
        "Column whose contiguous runs shade vertical background bands.",
        "NA entries produce no band."
      )
    ),
    # -- semantics ---------------------------------------------------------
    smooth = prop_boolean(FALSE, description = "Draw smoothed lines."),
    area = prop_boolean(FALSE, description = "Fill the area under each line."),
    points = prop_boolean(
      TRUE,
      description = "Show a marker at each data value."
    ),
    zoom = prop_boolean(FALSE, description = "Enable the zoom control."),
    # -- appearance --------------------------------------------------------
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Series colors, overriding the theme palette. NULL uses the theme's."
    ),
    block_color = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Band colors."
    ),
    block_opacity = prop_float(
      0.2,
      min = 0,
      max = 1,
      description = "Band opacity."
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
) # /rtemis.draw::LineConfig


# %% LINE_ORIGIN_NAMES ----
LINE_ORIGIN_NAMES <- setdiff(
  names(LineConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_LineConfig ----
#' Set up a Line Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams LineConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [LineConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw(setup_LineConfig(x = "wt", y = "mpg"), data = mtcars[order(mtcars$wt), ])
setup_LineConfig <- function(
  x = NULL,
  y = NULL,
  blocks = NULL,
  smooth = FALSE,
  area = FALSE,
  points = TRUE,
  zoom = FALSE,
  palette = NULL,
  block_color = NULL,
  block_opacity = 0.2,
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
  origin <- origin %||% chart_origin(match.call(), LINE_ORIGIN_NAMES)
  LineConfig(
    x = x,
    y = y,
    blocks = blocks,
    smooth = smooth,
    area = area,
    points = points,
    zoom = zoom,
    palette = palette,
    block_color = block_color,
    block_opacity = block_opacity,
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
} # /rtemis.draw::setup_LineConfig


# %% resolve.LineConfig ----
# Labels from the bound columns, as for bar: one y column names the value axis,
# several name none. Limits are derived only for a numeric x, since a
# categorical axis has no range to pad.
method(resolve, LineConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  x <- config_column(dat, config@x, "x")
  y_all <- unlist(
    lapply(config@y, function(column) config_column(dat, column, "y")),
    use.names = FALSE
  )
  config_derive(
    config,
    list(
      xlab = config@x,
      ylab = if (length(config@y) == 1L) config@y,
      xlim = if (!is.null(x) && is.numeric(x)) calc_limits(x, config@pad),
      ylim = if (!is.null(y_all)) calc_limits(y_all, config@pad)
    )
  )
}


# %% compile.LineConfig ----
method(compile, LineConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  config <- resolve(config, data = dat)
  x <- config_column(dat, config@x, "x")
  if (is.null(x) || is.null(config@y)) {
    abort(
      "A LineConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  values <- lapply(config@y, function(column) config_column(dat, column, "y"))
  names(values) <- config@y
  line_option(
    x = x,
    y = if (length(values) == 1L) values[[1L]] else values,
    smooth = config@smooth,
    area = config@area,
    points = config@points,
    blocks = config_column(dat, config@blocks, "blocks"),
    block_color = config@block_color,
    block_opacity = config@block_opacity,
    palette = config@palette,
    pad = config@pad,
    xlim = config@xlim,
    ylim = config@ylim,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    zoom = config@zoom,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
