# axis.R
# Axis-related S7 classes: AxisLine, AxisTick, MinorTick, SplitLine,
# MinorSplitLine, SplitArea, AxisLabel, Axis
#
# TS source: src/coord/axisCommonTypes.ts

# -- AxisLine -------------------------------------------------------------------

#' Axis Line
#'
#' Configuration for the axis line itself.
#'
#' Corresponds to `AxisLineOption` in `src/coord/axisCommonTypes.ts` (line 231).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.axisLine}
#'
#' @param show Whether to show the axis line. TRUE, FALSE, or "auto".
#' @param on_zero Whether the axis line is on the zero position of the other axis.
#' @param on_zero_axis_index The index of the other axis on whose zero position
#'   this axis line is drawn.
#' @param symbol Arrow symbols at both ends: a single string or length-2 vector.
#' @param symbol_size Arrow symbol size as numeric vector.
#' @param symbol_offset Arrow symbol offset.
#' @param line_style A [LineStyle] object.
#' @export
AxisLine <- S7::new_class(
  "AxisLine",
  properties = list(
    show = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L && value == "auto") {
          return(NULL)
        }
        "must be TRUE, FALSE, 'auto', or NULL"
      }
    ),
    on_zero = bool_or_null_property(),
    on_zero_axis_index = numeric_or_null_property(),
    symbol = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.character(value) && length(value) <= 2L) return(NULL)
        "must be a string or length-2 character vector, or NULL"
      }
    ),
    symbol_size = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value)) return(NULL)
        "must be a numeric vector or NULL"
      }
    ),
    symbol_offset = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) || is.character(value)) return(NULL)
        "must be a number, string, vector, or NULL"
      }
    ),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, AxisLine) <- function(x, ...) {
  props_to_list(x)
}

# -- AxisTick -------------------------------------------------------------------

#' Axis Tick
#'
#' Configuration for axis tick marks.
#'
#' Corresponds to `AxisTickOption` in `src/coord/axisCommonTypes.ts` (line 244).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.axisTick}
#'
#' @param show Whether to show ticks. TRUE, FALSE, or "auto".
#' @param inside Whether ticks are inside the grid.
#' @param length Tick mark length in pixels.
#' @param align_with_label Whether to align tick with label (category axis).
#' @param line_style A [LineStyle] object.
#' @export
AxisTick <- S7::new_class(
  "AxisTick",
  properties = list(
    show = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L && value == "auto") {
          return(NULL)
        }
        "must be TRUE, FALSE, 'auto', or NULL"
      }
    ),
    inside = bool_or_null_property(),
    length = numeric_or_null_property(),
    align_with_label = bool_or_null_property(),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, AxisTick) <- function(x, ...) {
  props_to_list(x)
}

# -- MinorTick ------------------------------------------------------------------

#' Minor Tick
#'
#' Configuration for minor axis tick marks (between main ticks).
#'
#' Corresponds to `MinorTickOption` in `src/coord/axisCommonTypes.ts` (line 346).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.minorTick}
#'
#' @param show Whether to show minor ticks.
#' @param split_number Number of minor ticks between two main ticks.
#' @param length Minor tick length in pixels.
#' @param line_style A [LineStyle] object.
#' @export
MinorTick <- S7::new_class(
  "MinorTick",
  properties = list(
    show = bool_or_null_property(),
    split_number = numeric_or_null_property(),
    length = numeric_or_null_property(),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, MinorTick) <- function(x, ...) {
  props_to_list(x)
}

# -- SplitLine ------------------------------------------------------------------

#' Split Line
#'
#' Configuration for grid split lines perpendicular to the axis.
#'
#' Corresponds to `SplitLineOption` in `src/coord/axisCommonTypes.ts` (line 353).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.splitLine}
#'
#' @param show Whether to show split lines.
#' @param interval Interval of split lines. "auto", a number, or NULL.
#' @param show_min_line Whether to show the split line at the min end.
#' @param show_max_line Whether to show the split line at the max end.
#' @param line_style A [LineStyle] object.
#' @export
SplitLine <- S7::new_class(
  "SplitLine",
  properties = list(
    show = bool_or_null_property(),
    interval = S7::new_property(
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
    show_min_line = bool_or_null_property(),
    show_max_line = bool_or_null_property(),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, SplitLine) <- function(x, ...) {
  props_to_list(x)
}

# -- MinorSplitLine -------------------------------------------------------------

#' Minor Split Line
#'
#' Configuration for minor split lines (between main split lines).
#'
#' Corresponds to `MinorSplitLineOption` in `src/coord/axisCommonTypes.ts` (line 364).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.minorSplitLine}
#'
#' @param show Whether to show minor split lines.
#' @param line_style A [LineStyle] object.
#' @export
MinorSplitLine <- S7::new_class(
  "MinorSplitLine",
  properties = list(
    show = bool_or_null_property(),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, MinorSplitLine) <- function(x, ...) {
  props_to_list(x)
}

# -- SplitArea ------------------------------------------------------------------

#' Split Area
#'
#' Configuration for alternating colored bands between split lines.
#'
#' Corresponds to `SplitAreaOption` in `src/coord/axisCommonTypes.ts` (line 369).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.splitArea}
#'
#' @param show Whether to show the split area.
#' @param interval Interval of split areas. "auto", a number, or NULL.
#' @param area_style An [AreaStyle] object.
#' @export
SplitArea <- S7::new_class(
  "SplitArea",
  properties = list(
    show = bool_or_null_property(),
    interval = S7::new_property(
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
    area_style = class_or_null_property(AreaStyle)
  )
)

S7::method(to_list, SplitArea) <- function(x, ...) {
  props_to_list(x)
}

# -- AxisLabel ------------------------------------------------------------------

#' Axis Label
#'
#' Configuration for axis labels. Combines label-specific positioning with
#' text styling (flattened in to_list like LabelOption).
#'
#' Corresponds to `AxisLabelBaseOption` in `src/coord/axisCommonTypes.ts` (line 312).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis.axisLabel}
#'
#' @param show Whether to show axis labels.
#' @param inside Whether labels are inside the grid.
#' @param rotate Rotation angle of labels in degrees.
#' @param margin Space between label and axis line in pixels.
#' @param formatter Label text formatter. String template or function.
#' @param show_min_label Whether to show the label at the min end.
#' @param show_max_label Whether to show the label at the max end.
#' @param hide_overlap Whether to hide overlapping labels.
#' @param interval Interval of labels. "auto", a number, or NULL.
#' @param text_style A [TextStyle] object for text appearance.
#' @export
AxisLabel <- S7::new_class(
  "AxisLabel",
  properties = list(
    show = bool_or_null_property(),
    inside = bool_or_null_property(),
    rotate = numeric_or_null_property(),
    margin = numeric_or_null_property(),
    formatter = S7::new_property(class = S7::class_any, default = NULL),
    show_min_label = bool_or_null_property(),
    show_max_label = bool_or_null_property(),
    hide_overlap = bool_or_null_property(),
    interval = S7::new_property(
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
    # Text styling -- flattened into the same JSON object in to_list()
    text_style = class_or_null_property(TextStyle)
  )
)

S7::method(to_list, AxisLabel) <- function(x, ...) {
  out <- props_to_list(x)
  # Flatten text_style into the same level (echarts axisLabel is flat)
  if (!is.null(out$textStyle)) {
    text_fields <- out$textStyle
    out$textStyle <- NULL
    out <- c(out, text_fields)
  }
  out
}

# -- Axis -----------------------------------------------------------------------

#' Axis
#'
#' Configuration for a cartesian axis (xAxis or yAxis). Combines fields from
#' `AxisBaseOptionCommon`, `NumericAxisBaseOptionCommon`, `CategoryAxisBaseOption`,
#' `ValueAxisBaseOption`, `LogAxisBaseOption`, and `TimeAxisBaseOption`.
#'
#' Corresponds to `AxisBaseOption` in `src/coord/axisCommonTypes.ts` (line 376).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#xAxis}
#'
#' @param type Axis type: `"value"`, `"category"`, `"time"`, or `"log"`.
#' @param show Whether to show the axis.
#' @param inverse Whether to invert the axis.
#' @param name Axis name displayed alongside the axis.
#' @param name_location Name position: `"start"`, `"middle"`, `"center"`, `"end"`.
#' @param name_rotate Name rotation angle in degrees.
#' @param name_gap Gap between the axis name and the axis line.
#' @param name_text_style A [TextStyle] for the axis name.
#' @param silent Whether the axis is silent (no events).
#' @param trigger_event Whether the axis triggers events.
#' @param min Minimum axis value. Number, `"dataMin"`, or NULL.
#' @param max Maximum axis value. Number, `"dataMax"`, or NULL.
#' @param scale For value axis: whether to force axis to not contain zero.
#' @param split_number Suggested number of split segments.
#' @param interval Mandatory interval between ticks.
#' @param min_interval Minimum auto-calculated tick interval.
#' @param max_interval Maximum auto-calculated tick interval.
#' @param log_base Log base for log axis.
#' @param boundary_gap Gap at axis boundaries. Logical (category) or length-2
#'   vector of numbers/strings (numeric axes).
#' @param align_ticks Whether to align ticks with the first axis.
#' @param data Category data vector.
#' @param axis_line An [AxisLine] object.
#' @param axis_tick An [AxisTick] object.
#' @param minor_tick A [MinorTick] object.
#' @param axis_label An [AxisLabel] object.
#' @param split_line A [SplitLine] object.
#' @param minor_split_line A [MinorSplitLine] object.
#' @param split_area A [SplitArea] object.
#' @export
Axis <- S7::new_class(
  "Axis",
  properties = list(
    # AxisBaseOptionCommon
    type = enum_property(c("value", "category", "time", "log")),
    show = bool_or_null_property(),
    inverse = bool_or_null_property(),
    name = string_or_null_property(),
    name_location = enum_property(c("start", "middle", "center", "end")),
    name_rotate = numeric_or_null_property(),
    name_gap = numeric_or_null_property(),
    name_text_style = class_or_null_property(TextStyle),
    silent = bool_or_null_property(),
    trigger_event = bool_or_null_property(),
    # Min/max: number, string ("dataMin"/"dataMax"), or NULL
    min = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L &&
            value %in% c("dataMin", "dataMax")) {
          return(NULL)
        }
        "must be a number, 'dataMin', 'dataMax', or NULL"
      }
    ),
    max = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L &&
            value %in% c("dataMin", "dataMax")) {
          return(NULL)
        }
        "must be a number, 'dataMin', 'dataMax', or NULL"
      }
    ),
    # ValueAxisBaseOption
    scale = bool_or_null_property(),
    # NumericAxisBaseOptionCommon
    split_number = numeric_or_null_property(),
    interval = numeric_or_null_property(),
    min_interval = numeric_or_null_property(),
    max_interval = numeric_or_null_property(),
    align_ticks = bool_or_null_property(),
    # LogAxisBaseOption
    log_base = numeric_or_null_property(),
    # CategoryAxisBaseOption: boundaryGap is boolean
    # NumericAxisBaseOption: boundaryGap is [number|string, number|string]
    boundary_gap = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if ((is.numeric(value) || is.character(value)) && length(value) == 2L) {
          return(NULL)
        }
        "must be TRUE/FALSE, or a length-2 vector, or NULL"
      }
    ),
    # Category data
    data = S7::new_property(class = S7::class_any, default = NULL),
    # Sub-components
    axis_line = class_or_null_property(AxisLine),
    axis_tick = class_or_null_property(AxisTick),
    minor_tick = class_or_null_property(MinorTick),
    axis_label = class_or_null_property(AxisLabel),
    split_line = class_or_null_property(SplitLine),
    minor_split_line = class_or_null_property(MinorSplitLine),
    split_area = class_or_null_property(SplitArea)
  )
)

S7::method(to_list, Axis) <- function(x, ...) {
  props_to_list(x)
}
