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
#' @param name Optional Character: Series name for legend display.
#' @param data Optional Numeric or list: Data values.
#' @param x_axis_index Optional Numeric `[0, Inf)`: X-axis index.
#' @param y_axis_index Optional Numeric `[0, Inf)`: Y-axis index.
#' @param stack Optional Character: Stack group name.
#' @param smooth Optional Logical or Numeric `[0, 1]`: Line smoothing.
#' @param step Optional Logical or Character \{"start", "end", "middle"\}: Step line mode.
#' @param connect_nulls Optional Logical: Whether to connect across `NULL` points.
#' @param clip Optional Logical: Whether to clip overflow.
#' @param show_symbol Optional Logical: Whether to show data-point symbols.
#' @param symbol Optional Character \{"circle", "rect", "roundRect", "triangle", "diamond", "pin", "arrow", "none"\}: Symbol type.
#'   `"diamond"`, `"pin"`, `"arrow"`, `"none"`, or custom SVG path.
#' @param symbol_size Optional Numeric: Symbol size in pixels.
#' @param color Optional Character: Series color override.
#' @param line_style Optional [LineStyle]: Line styling.
#' @param area_style Optional [AreaStyle]: Area fill styling.
#' @param item_style Optional [ItemStyle]: Data-point marker styling.
#' @param label Optional [LabelOption]: Data-label configuration.
#' @param legend_hover_link Optional Logical: Whether to highlight related legends on hover.
#' @param silent Optional Logical: Whether the series is silent.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same layer.
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
  if (is.list(out$data)) out$data <- unname(out$data)
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
#' @param name Optional Character: Series name for legend display.
#' @param data Optional Vector or list: Data values.
#' @param x_axis_index Optional Numeric `[0, Inf)`: X-axis index.
#' @param y_axis_index Optional Numeric `[0, Inf)`: Y-axis index.
#' @param stack Optional Character: Stack group name.
#' @param clip Optional Logical: Whether to clip overflow.
#' @param bar_width Optional Numeric or Character: Bar width.
#' @param bar_max_width Optional Numeric or Character: Maximum bar width.
#' @param bar_min_width Optional Numeric or Character: Minimum bar width.
#' @param bar_min_height Optional Numeric `[0, Inf)`: Minimum bar height.
#' @param bar_gap Optional Numeric or Character: Gap between bars in the same category.
#' @param bar_category_gap Optional Numeric or Character: Gap between categories.
#' @param round_cap Optional Logical: Whether to use a round cap.
#' @param show_background Optional Logical: Whether to show the bar background.
#' @param color Optional Character: Series color override.
#' @param item_style Optional [ItemStyle]: Bar styling.
#' @param label Optional [LabelOption]: Data-label configuration.
#' @param legend_hover_link Optional Logical: Whether to highlight related legends on hover.
#' @param silent Optional Logical: Whether the series is silent.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same layer.
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
  if (is.list(out$data)) out$data <- unname(out$data)
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
#' @param name Optional Character: Series name for legend display.
#' @param data Optional Vector or list: Data values.
#' @param x_axis_index Optional Numeric `[0, Inf)`: X-axis index.
#' @param y_axis_index Optional Numeric `[0, Inf)`: Y-axis index.
#' @param clip Optional Logical: Whether to clip overflow.
#' @param symbol Optional Character: Symbol type.
#' @param symbol_size Optional Numeric or function: Symbol size.
#' @param color Optional Character: Series color override.
#' @param item_style Optional [ItemStyle]: Data-point marker styling.
#' @param label Optional [LabelOption]: Data-label configuration.
#' @param large Optional Logical: Whether to enable large-data optimization.
#' @param large_threshold Optional Numeric `[0, Inf)`: Threshold for large mode.
#' @param legend_hover_link Optional Logical: Whether to highlight related legends on hover.
#' @param silent Optional Logical: Whether the series is silent.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same layer.
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
  if (is.list(out$data)) out$data <- unname(out$data)
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
#' @param name Optional Character: Series name.
#' @param data Optional Numeric or list: Pie data. A list of named lists with `value` and `name` fields,
#'   or a numeric vector.
#' @param center Optional Numeric or Character: Pie center as a length-2 vector.
#' @param radius Optional Numeric or Character: Pie radius.
#' @param rose_type Optional Character \{"radius", "area"\}: Nightingale chart type.
#' @param clockwise Optional Logical: Whether to lay out sectors clockwise.
#' @param start_angle Optional Numeric: Starting angle in degrees.
#' @param end_angle Optional Numeric or Character \{"auto"\}: Ending angle in degrees.
#' @param pad_angle Optional Numeric `[0, Inf)`: Padding angle between sectors in degrees.
#' @param min_angle Optional Numeric `[0, Inf)`: Minimum sector angle.
#' @param min_show_label_angle Optional Numeric `[0, Inf)`: Minimum angle required to show labels.
#' @param selected_offset Optional Numeric `[0, Inf)`: Offset of a selected sector.
#' @param avoid_label_overlap Optional Logical: Whether to avoid label overlap automatically.
#' @param percent_precision Optional Numeric `[0, Inf)`: Decimal precision for percentages.
#' @param still_show_zero_sum Optional Logical: Whether to show the pie when all values are zero.
#' @param animation_type Optional Character \{"expansion", "scale"\}: First-render animation type.
#' @param color Optional Character: Series color override.
#' @param item_style Optional [ItemStyle]: Pie-sector styling.
#' @param label Optional [LabelOption]: Data-label configuration.
#' @param label_line Optional [LabelLine]: Label connector line configuration.
#' @param legend_hover_link Optional Logical: Whether to highlight related legends on hover.
#' @param silent Optional Logical: Whether the series is silent.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same layer.
#' @param left Optional Numeric or Character: Left position.
#' @param right Optional Numeric or Character: Right position.
#' @param top Optional Numeric or Character: Top position.
#' @param bottom Optional Numeric or Character: Bottom position.
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
  if (is.list(out$data)) out$data <- unname(out$data)
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
#' @param name Optional Character: Series name.
#' @param data Optional list: Boxplot data. Each element is `c(min, Q1, median, Q3, max)`.
#' @param x_axis_index Optional Numeric `[0, Inf)`: X-axis index.
#' @param y_axis_index Optional Numeric `[0, Inf)`: Y-axis index.
#' @param layout Optional Character \{"horizontal", "vertical"\}: Layout orientation.
#' @param box_width Optional Numeric or Character: Box-width range as a length-2 vector.
#' @param color Optional Character: Series color override.
#' @param item_style Optional [ItemStyle]: Boxplot styling.
#' @param label Optional [LabelOption]: Data-label configuration.
#' @param legend_hover_link Optional Logical: Whether to highlight related legends on hover.
#' @param silent Optional Logical: Whether the series is silent.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same layer.
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
  if (is.list(out$data)) out$data <- unname(out$data)
  out
}
