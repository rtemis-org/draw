# config_line.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The line chart's config. Multi-series like bar, plus a background-band binding
# (`blocks`) and axis limits like scatter. compile() returns a BuildRequest,
# whose option @rtemis/draw's line builder builds in the widget and in Node.
# spec: draw/shared-builders#line

# %% LineConfig ----
#' Line Chart Configuration
#'
#' A serializable description of a line chart. Build one with
#' [setup_LineConfig()] rather than calling this constructor directly.
#'
#' `x` names the column along the horizontal axis. `y` names **one or more**
#' columns, one line each, as [BarConfig] does.
#'
#' @param x Optional Character: Column along the horizontal axis. A numeric
#'   column gets a value axis, a `Date` or `POSIXct` column a time axis with
#'   adaptively chosen labels, and any other column a category axis.
#' @param y Optional Character: Columns to plot, one line each.
#' @param group Optional Character: Column to group and color lines by. Splits
#'   each `y` column into one line per level; cannot be combined with
#'   `blocks`.
#' @param aggregate Character \{"none", "sum", "mean", "median"\}: How the `y`
#'   values of rows sharing an x and group combine into one point. A line
#'   joins one value per x, so with `"none"` two such rows are an error.
#'   Missing values are left out of each summary.
#' @param blocks Optional Character: Column whose contiguous runs shade vertical
#'   background bands.
#' @param smooth Logical: Draw smoothed lines.
#' @param area Logical: Fill the area under each line. A filled area is read
#'   by its height from zero, so the derived y limits then hold zero.
#' @param points Logical: Show a marker at each data value. Without markers, a
#'   value with no neighbor to join keeps its marker.
#' @param stack Logical: Stack the lines, each on the sum of those before it at
#'   the same x. Requires `area`, since the filled bands show each line's
#'   share. A line has no point where a line below it has no value, since the
#'   sum there is unknown.
#' @param zoom Logical: Enable an x-axis slider, Shift+wheel zoom, and drag
#'   panning.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param line_style Optional Character \{"solid", "dashed", "dotted"\}: Dash
#'   pattern of each line, recycled across lines. `NULL` draws solid lines.
#' @param block_color Optional Character: Band colors, one per level of
#'   `blocks`: the factor levels that occur, or the sorted distinct values. `NA`
#'   or `"transparent"` draws no band for its level.
#' @param block_opacity Numeric `[0, 1]`: Band opacity.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each axis
#'   by when the limits are not given.
#' @param square Logical: If TRUE, draw the plotting box square -- equal height
#'   and width in pixels, excluding axis labels and margins.
#' @param equal_axes Logical: If TRUE, give one data unit the same size in
#'   pixels on both axes. Set with `square` for a plot that is both, such as an
#'   ROC curve; the two axes are then made to span the same interval. Requires a
#'   numeric x.
#' @param xlim,ylim Optional Numeric: Axis limits, length 2. `NULL` derives them
#'   from the data, padded by `pad`. On a time axis `xlim` is in epoch
#'   milliseconds, the unit the chart positions by.
#' @param xlab,ylab Optional Character: Axis labels. `NULL` derives them from
#'   the data, and `""` draws no label.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams ScatterConfig fit fit_params fit_per_group se se_times rsq n_fit fit_alpha fit_name
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @inheritParams ChartConfig
#'
#' @return `LineConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_LineConfig(x = "month", y = "gwh", group = "source")@type
LineConfig <- new_class(
  name = "LineConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    fit_properties(),
    list(
      type = prop_chart_type("line"),
      # -- data binding ------------------------------------------------------
      x = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Column along the horizontal axis. Numeric gets a value axis,",
          "date or date-time a time axis, anything else a category axis."
        )
      ),
      y = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Columns to plot, one line each."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column to group and color lines by."
      ),
      aggregate = prop_string(
        "none",
        enum = c("none", "sum", "mean", "median"),
        description = paste(
          "How the y values of rows sharing an x and group combine into one",
          "point: none requires one row per x in each line."
        )
      ),
      blocks = prop_string(
        NULL,
        nullable = TRUE,
        description = paste(
          "Column whose contiguous runs shade vertical background bands.",
          "Missing entries produce no band."
        )
      ),
      # -- semantics ---------------------------------------------------------
      smooth = prop_boolean(FALSE, description = "Draw smoothed lines."),
      area = prop_boolean(
        FALSE,
        description = "Fill the area under each line; derived y limits hold zero."
      ),
      points = prop_boolean(
        FALSE,
        description = paste(
          "Show a marker at each data value. Without markers, a value with no",
          "neighbor to join keeps its marker."
        )
      ),
      stack = prop_boolean(
        FALSE,
        description = paste(
          "Stack the lines, each on the sum of those before it. Requires area."
        )
      ),
      zoom = prop_boolean(
        FALSE,
        description = "Enable an x-axis slider, Shift+wheel zoom, and drag panning."
      ),
      # -- appearance --------------------------------------------------------
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Series colors, overriding the theme palette. Unset uses the theme's."
      ),
      line_style = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        enum = c("solid", "dashed", "dotted"),
        description = "Dash pattern of each line, recycled. Unset draws solid lines."
      ),
      block_color = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Band colors, one per level of `blocks`, in level order."
      ),
      block_opacity = prop_float(
        0.2,
        min = 0,
        max = 1,
        description = "Band opacity."
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
        description = paste(
          "X axis limits. Unset derives them from the data.",
          "On a time axis, epoch milliseconds."
        )
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
        min_length = 0L,
        nullable = TRUE,
        description = "X axis label. Unset derives it from the data, and an empty string draws no label."
      ),
      ylab = prop_string(
        NULL,
        min_length = 0L,
        nullable = TRUE,
        description = "Y axis label. Unset derives it from the data, and an empty string draws no label."
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
    c(
      CONFIG_LIMIT_RULES,
      list(
        list(
          schema = list(
            `if` = list(
              required = list("group"),
              properties = list(group = list(not = list(type = "null")))
            ),
            then = list(
              properties = list(blocks = list(type = "null"))
            )
          ),
          message = "Bind group or blocks, not both."
        ),
        list(
          schema = list(
            `if` = list(
              required = list("stack"),
              properties = list(stack = list(const = TRUE))
            ),
            then = list(
              properties = list(equal_axes = list(const = FALSE))
            )
          ),
          message = "A stacked y axis shows sums: set stack or equal_axes, not both."
        ),
        list(
          schema = list(
            `if` = list(
              required = list("stack"),
              properties = list(stack = list(const = TRUE))
            ),
            then = list(
              properties = list(area = list(const = TRUE))
            )
          ),
          message = paste(
            "stack draws each line on the sum of those below it, which only",
            "filled bands show: set area too."
          )
        ),
        FIT_PARAMS_RULE,
        list(
          schema = list(
            `if` = list(
              required = list("fit"),
              properties = list(fit = list(not = list(type = "null")))
            ),
            then = list(
              properties = list(
                aggregate = list(const = "none"),
                area = list(const = FALSE)
              )
            )
          ),
          message = paste(
            "A fit models observations drawn as lines: set aggregate to none",
            "and area to FALSE, or unset fit."
          )
        )
      )
    ),
    extra = config_ordered_limits
  )
) # /rtemis.draw::LineConfig


# %% LINE_ORIGIN_NAMES ----
LINE_ORIGIN_NAMES <- setdiff(
  names(LineConfig@properties),
  c("type", PROVENANCE_PROPS)
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
#' # Monthly generation by source, one line each.
#' draw(
#'   setup_LineConfig(
#'     x = "month",
#'     y = "gwh",
#'     group = "source",
#'     ylab = "Generation (GWh)"
#'   ),
#'   data = power_generation
#' )
#'
#' # The sources add up to the total generated, so they stack.
#' draw(
#'   setup_LineConfig(
#'     x = "month",
#'     y = "gwh",
#'     group = "source",
#'     area = TRUE,
#'     stack = TRUE,
#'     ylab = "Generation (GWh)"
#'   ),
#'   data = power_generation
#' )
#'
#' # Quarterly totals: `aggregate` combines the months of each quarter.
#' draw(
#'   setup_LineConfig(
#'     x = "quarter",
#'     y = "gwh",
#'     group = "source",
#'     aggregate = "sum",
#'     points = TRUE
#'   ),
#'   data = power_generation
#' )
setup_LineConfig <- function(
  x = NULL,
  y = NULL,
  group = NULL,
  aggregate = "none",
  blocks = NULL,
  smooth = FALSE,
  area = FALSE,
  points = FALSE,
  stack = FALSE,
  zoom = FALSE,
  palette = NULL,
  line_style = NULL,
  block_color = NULL,
  block_opacity = 0.2,
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
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  fit = NULL,
  fit_params = NULL,
  fit_per_group = TRUE,
  se = TRUE,
  se_times = 1.96,
  rsq = FALSE,
  n_fit = 200L,
  fit_alpha = 0.25,
  fit_name = NULL
) {
  origin <- origin %||% chart_origin(match.call(), LINE_ORIGIN_NAMES)
  LineConfig(
    fit = fit,
    fit_params = fit_params,
    fit_per_group = fit_per_group,
    se = se,
    se_times = se_times,
    rsq = rsq,
    n_fit = clean_int(n_fit),
    fit_alpha = fit_alpha,
    fit_name = fit_name,
    x = x,
    y = y,
    group = group,
    aggregate = aggregate,
    blocks = blocks,
    smooth = smooth,
    area = area,
    points = points,
    stack = stack,
    zoom = zoom,
    # A named color vector, such as a subset of `rtemis_colors`, is a palette.
    palette = if (!is.null(palette)) unname(palette),
    line_style = line_style,
    block_color = block_color,
    block_opacity = block_opacity,
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
} # /rtemis.draw::setup_LineConfig


# %% resolve.LineConfig ----
# Labels from the bound columns, as for bar: one y column names the value axis,
# several name none. Limits are derived only for a numeric x, since a
# categorical axis has no range to pad.
method(resolve, LineConfig) <- function(config, data = NULL, ...) {
  # Limits are those of the points drawn: the combined rows.
  data <- line_rows(config, data)
  x <- config_column(data, config@x, "x")
  y_all <- unlist(
    lapply(config@y, function(column) config_column(data, column, "y")),
    use.names = FALSE
  )
  # See resolve.ScatterConfig: the limits a square, equally-scaled plot needs
  # are settled here so the document records them. A categorical x has no
  # interval to share, and the builder rejects that combination. A time x has
  # a position -- epoch milliseconds, as the builder places it -- so its limits
  # are recorded in that unit, but it is not a scale to equalize with y.
  numeric_x <- if (!is.null(x) && is.numeric(x)) x
  time_x <- if (!is.null(x) && is_time_axis(x)) time_axis_ms(x)
  common <- equal_axis_limits(
    numeric_x,
    y_all,
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
      ylab = if (!is.null(config@y)) value_label(config),
      xlim = common[["xlim"]] %||%
        if (!is.null(numeric_x %||% time_x)) {
          calc_limits(numeric_x %||% time_x, config@pad)
        },
      # A stacked axis spans the sums, which the builder derives. A filled
      # area is read by its height from zero, so its axis holds zero.
      ylim = common[["ylim"]] %||%
        if (!is.null(y_all) && !config@stack) {
          if (config@area) {
            calc_limits_from_zero(y_all, config@pad)
          } else {
            calc_limits(y_all, config@pad)
          }
        }
    )
  )
}


# %% compile.LineConfig ----
method(compile, LineConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  if (is.null(config@x) || is.null(config@y)) {
    abort(
      "A LineConfig needs both `x` and `y` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  columns <- unique(c(config@x, config@y, config@group, config@blocks))
  for (name in columns) {
    config_column(data, name, if (name %in% config@y) "y" else "x")
  }
  rows <- line_rows(config, data)
  BuildRequest(
    type = "line",
    data = tabular_json(rows, columns, fits = fit_curves(config, rows)),
    config = config_members(config),
    appearance = list()
  )
}


# %% line_rows ----
#' The rows a line chart draws
#'
#' With `aggregate` set, one row per x, group, and band level, the `y` columns
#' summarized as `aggregate` says; otherwise the data as given.
#'
#' @param config [LineConfig].
#' @param data Optional Data frame: The chart data.
#'
#' @return Data frame, or `data` unchanged.
#'
#' @author EDG
#' @keywords internal
#' @noRd
line_rows <- function(config, data) {
  if (is.null(data) || config@aggregate == "none" || is.null(config@x)) {
    return(data)
  }
  aggregate_rows(
    data,
    c(config@x, config@group, config@blocks),
    config@y,
    config@aggregate
  )
} # /rtemis.draw::line_rows
