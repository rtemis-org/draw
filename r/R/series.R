# series.R
# Series S7 classes: LineSeries, BarSeries, ScatterSeries, PieSeries, BoxplotSeries
#
# TS sources:
#   SeriesOption:        src/util/types.ts (line 1867)
#   LineSeriesOption:    src/chart/line/LineSeries.ts (line 74)
#   BarSeriesOption:     src/chart/bar/BarSeries.ts (line 68)
#   ScatterSeriesOption: src/chart/scatter/ScatterSeries.ts (line 64)
#   PieSeriesOption:     src/chart/pie/PieSeries.ts (line 105)
#   BoxplotSeriesOption: src/chart/boxplot/BoxplotSeries.ts (line 59)

# -- LineSeries -----------------------------------------------------------------

#' Line Series
#'
#' Configuration for a line chart series.
#'
#' Corresponds to `LineSeriesOption` in `src/chart/line/LineSeries.ts` (line 74).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-line}
#'
#' @param name Series name for legend display.
#' @param data Data values. Numeric vector, or list of (x, y) pairs.
#' @param x_axis_index Index of the x axis to use.
#' @param y_axis_index Index of the y axis to use.
#' @param stack Stack group name for stacked area charts.
#' @param smooth Whether to smooth the line. TRUE/FALSE or a number (0-1).
#' @param step Step line mode: FALSE, `"start"`, `"end"`, or `"middle"`.
#' @param connect_nulls Whether to connect across null points.
#' @param clip Whether to clip overflow.
#' @param show_symbol Whether to show symbols on data points.
#' @param symbol Symbol type: `"circle"`, `"rect"`, `"roundRect"`, `"triangle"`,
#'   `"diamond"`, `"pin"`, `"arrow"`, `"none"`, or custom SVG path.
#' @param symbol_size Symbol size in px. Number or length-2 vector.
#' @param color Series color override.
#' @param line_style A [LineStyle] object.
#' @param area_style An [AreaStyle] object (enables area fill).
#' @param item_style An [ItemStyle] for data point markers.
#' @param label A [LabelOption] for data labels.
#' @param legend_hover_link Whether to highlight related legends on hover.
#' @param silent Whether this series is silent (no events).
#' @param z_level Canvas layer index.
#' @param z Front-back order within the same layer.
#' @export
LineSeries <- S7::new_class(
  "LineSeries",
  properties = list(
    # Common series fields
    name = string_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL),
    x_axis_index = numeric_or_null_property(),
    y_axis_index = numeric_or_null_property(),
    stack = string_or_null_property(),
    silent = bool_or_null_property(),
    legend_hover_link = bool_or_null_property(),
    color = color_property(),
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property(),
    # Line-specific
    smooth = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        "must be TRUE/FALSE, a number, or NULL"
      }
    ),
    step = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L && !value) return(NULL)
        if (is.character(value) && length(value) == 1L &&
            value %in% c("start", "end", "middle")) {
          return(NULL)
        }
        "must be FALSE, 'start', 'end', 'middle', or NULL"
      }
    ),
    connect_nulls = bool_or_null_property(),
    clip = bool_or_null_property(),
    show_symbol = bool_or_null_property(),
    symbol = string_or_null_property(),
    symbol_size = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) <= 2L) return(NULL)
        "must be a number, length-2 numeric vector, or NULL"
      }
    ),
    line_style = class_or_null_property(LineStyle),
    area_style = class_or_null_property(AreaStyle),
    item_style = class_or_null_property(ItemStyle),
    label = class_or_null_property(LabelOption)
  )
)

S7::method(to_list, LineSeries) <- function(x, ...) {
  out <- props_to_list(x)
  out$type <- "line"
  out
}

# -- BarSeries ------------------------------------------------------------------

#' Bar Series
#'
#' Configuration for a bar chart series.
#'
#' Corresponds to `BarSeriesOption` in `src/chart/bar/BarSeries.ts` (line 68)
#' and `BaseBarSeriesOption` in `src/chart/bar/BaseBarSeries.ts` (line 38).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-bar}
#'
#' @param name Series name for legend display.
#' @param data Data values.
#' @param x_axis_index Index of the x axis to use.
#' @param y_axis_index Index of the y axis to use.
#' @param stack Stack group name for stacked bars.
#' @param clip Whether to clip overflow.
#' @param bar_width Bar width. Number (px) or percent string.
#' @param bar_max_width Maximum bar width.
#' @param bar_min_width Minimum bar width.
#' @param bar_min_height Minimum bar height.
#' @param bar_gap Gap between bars in the same category. Percent string or number.
#' @param bar_category_gap Gap between categories.
#' @param round_cap Whether to use round cap (polar bar only).
#' @param show_background Whether to show bar background.
#' @param color Series color override.
#' @param item_style An [ItemStyle] for bars.
#' @param label A [LabelOption] for data labels.
#' @param legend_hover_link Whether to highlight related legends on hover.
#' @param silent Whether this series is silent.
#' @param z_level Canvas layer index.
#' @param z Front-back order within the same layer.
#' @export
BarSeries <- S7::new_class(
  "BarSeries",
  properties = list(
    # Common series fields
    name = string_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL),
    x_axis_index = numeric_or_null_property(),
    y_axis_index = numeric_or_null_property(),
    stack = string_or_null_property(),
    silent = bool_or_null_property(),
    legend_hover_link = bool_or_null_property(),
    color = color_property(),
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property(),
    # Bar-specific
    clip = bool_or_null_property(),
    bar_width = numeric_or_string_property(),
    bar_max_width = numeric_or_string_property(),
    bar_min_width = numeric_or_string_property(),
    bar_min_height = numeric_or_null_property(),
    bar_gap = numeric_or_string_property(),
    bar_category_gap = numeric_or_string_property(),
    round_cap = bool_or_null_property(),
    show_background = bool_or_null_property(),
    item_style = class_or_null_property(ItemStyle),
    label = class_or_null_property(LabelOption)
  )
)

S7::method(to_list, BarSeries) <- function(x, ...) {
  out <- props_to_list(x)
  out$type <- "bar"
  out
}

# -- ScatterSeries --------------------------------------------------------------

#' Scatter Series
#'
#' Configuration for a scatter (point) chart series.
#'
#' Corresponds to `ScatterSeriesOption` in `src/chart/scatter/ScatterSeries.ts` (line 64).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-scatter}
#'
#' @param name Series name for legend display.
#' @param data Data values. List of (x, y) pairs or numeric vectors.
#' @param x_axis_index Index of the x axis to use.
#' @param y_axis_index Index of the y axis to use.
#' @param clip Whether to clip overflow.
#' @param symbol Symbol type.
#' @param symbol_size Symbol size. Number, length-2 vector, or NULL.
#' @param color Series color override.
#' @param item_style An [ItemStyle] for data point markers.
#' @param label A [LabelOption] for data labels.
#' @param large Whether to enable large data optimization.
#' @param large_threshold Threshold for large mode.
#' @param legend_hover_link Whether to highlight related legends on hover.
#' @param silent Whether this series is silent.
#' @param z_level Canvas layer index.
#' @param z Front-back order within the same layer.
#' @export
ScatterSeries <- S7::new_class(
  "ScatterSeries",
  properties = list(
    # Common series fields
    name = string_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL),
    x_axis_index = numeric_or_null_property(),
    y_axis_index = numeric_or_null_property(),
    silent = bool_or_null_property(),
    legend_hover_link = bool_or_null_property(),
    color = color_property(),
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property(),
    # Scatter-specific
    clip = bool_or_null_property(),
    symbol = string_or_null_property(),
    symbol_size = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) <= 2L) return(NULL)
        if (is.function(value)) return(NULL)
        "must be a number, length-2 numeric vector, function, or NULL"
      }
    ),
    large = bool_or_null_property(),
    large_threshold = numeric_or_null_property(),
    item_style = class_or_null_property(ItemStyle),
    label = class_or_null_property(LabelOption)
  )
)

S7::method(to_list, ScatterSeries) <- function(x, ...) {
  out <- props_to_list(x)
  out$type <- "scatter"
  out
}

# -- PieSeries ------------------------------------------------------------------

#' Pie Series
#'
#' Configuration for a pie chart series.
#'
#' Corresponds to `PieSeriesOption` in `src/chart/pie/PieSeries.ts` (line 105).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-pie}
#'
#' @param name Series name.
#' @param data Pie data. A list of named lists with `value` and `name` fields,
#'   or a numeric vector.
#' @param center Pie center: length-2 vector of numbers or percent strings.
#' @param radius Pie radius: a single value or `c(inner, outer)`.
#' @param rose_type Nightingale chart type: `"radius"` or `"area"`.
#' @param clockwise Whether to layout clockwise.
#' @param start_angle Starting angle in degrees.
#' @param end_angle Ending angle in degrees or `"auto"`.
#' @param pad_angle Padding angle between sectors in degrees.
#' @param min_angle Minimum sector angle.
#' @param min_show_label_angle Minimum angle to show label.
#' @param selected_offset Offset of selected sector.
#' @param avoid_label_overlap Whether to auto-avoid label overlap.
#' @param percent_precision Decimal precision for percentage.
#' @param still_show_zero_sum Whether to show the pie when all values are 0.
#' @param animation_type Animation type on first render: `"expansion"` or `"scale"`.
#' @param color Series color override.
#' @param item_style An [ItemStyle] for pie sectors.
#' @param label A [LabelOption] for data labels.
#' @param label_line A [LabelLine] for label connector lines.
#' @param legend_hover_link Whether to highlight related legends on hover.
#' @param silent Whether this series is silent.
#' @param z_level Canvas layer index.
#' @param z Front-back order within the same layer.
#' @param left,right,top,bottom Position for the pie chart.
#' @export
PieSeries <- S7::new_class(
  "PieSeries",
  properties = list(
    # Common series fields
    name = string_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL),
    silent = bool_or_null_property(),
    legend_hover_link = bool_or_null_property(),
    color = color_property(),
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property(),
    # Pie-specific
    center = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (length(value) == 2L && (is.numeric(value) || is.character(value))) {
          return(NULL)
        }
        "must be a length-2 vector of numbers or strings, or NULL"
      }
    ),
    radius = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if ((is.numeric(value) || is.character(value)) && length(value) <= 2L) {
          return(NULL)
        }
        "must be a number/string or length-2 vector, or NULL"
      }
    ),
    rose_type = enum_property(c("radius", "area")),
    clockwise = bool_or_null_property(),
    start_angle = numeric_or_null_property(),
    end_angle = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L && value == "auto") {
          return(NULL)
        }
        "must be a number, 'auto', or NULL"
      }
    ),
    pad_angle = numeric_or_null_property(),
    min_angle = numeric_or_null_property(),
    min_show_label_angle = numeric_or_null_property(),
    selected_offset = numeric_or_null_property(),
    avoid_label_overlap = bool_or_null_property(),
    percent_precision = numeric_or_null_property(),
    still_show_zero_sum = bool_or_null_property(),
    animation_type = enum_property(c("expansion", "scale")),
    item_style = class_or_null_property(ItemStyle),
    label = class_or_null_property(LabelOption),
    label_line = class_or_null_property(LabelLine),
    # BoxLayoutOptionMixin
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property()
  )
)

S7::method(to_list, PieSeries) <- function(x, ...) {
  out <- props_to_list(x)
  out$type <- "pie"
  out
}

# -- BoxplotSeries --------------------------------------------------------------

#' Boxplot Series
#'
#' Configuration for a boxplot chart series.
#'
#' Corresponds to `BoxplotSeriesOption` in `src/chart/boxplot/BoxplotSeries.ts` (line 59).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-boxplot}
#'
#' @param name Series name.
#' @param data Boxplot data. Each element is `c(min, Q1, median, Q3, max)`.
#' @param x_axis_index Index of the x axis to use.
#' @param y_axis_index Index of the y axis to use.
#' @param layout Layout orientation: `"horizontal"` or `"vertical"`.
#' @param box_width Box width range: length-2 vector of numbers or strings.
#' @param color Series color override.
#' @param item_style An [ItemStyle] for boxplot elements.
#' @param label A [LabelOption] for data labels.
#' @param legend_hover_link Whether to highlight related legends on hover.
#' @param silent Whether this series is silent.
#' @param z_level Canvas layer index.
#' @param z Front-back order within the same layer.
#' @export
BoxplotSeries <- S7::new_class(
  "BoxplotSeries",
  properties = list(
    # Common series fields
    name = string_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL),
    x_axis_index = numeric_or_null_property(),
    y_axis_index = numeric_or_null_property(),
    silent = bool_or_null_property(),
    legend_hover_link = bool_or_null_property(),
    color = color_property(),
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property(),
    # Boxplot-specific
    layout = enum_property(c("horizontal", "vertical")),
    box_width = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (length(value) == 2L && (is.numeric(value) || is.character(value))) {
          return(NULL)
        }
        "must be a length-2 vector of numbers or strings, or NULL"
      }
    ),
    item_style = class_or_null_property(ItemStyle),
    label = class_or_null_property(LabelOption)
  )
)

S7::method(to_list, BoxplotSeries) <- function(x, ...) {
  out <- props_to_list(x)
  out$type <- "boxplot"
  out
}
