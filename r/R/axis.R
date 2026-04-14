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
#' @param show Optional Logical or Character \{"auto"\}: Whether to show the axis line.
#' @param on_zero Optional Logical: Whether the axis line is on the zero position of the other axis.
#' @param on_zero_axis_index Optional Numeric `[0, Inf)`: Index of the other axis on whose zero position
#'   this axis line is drawn.
#' @param symbol Optional Character: Arrow symbols at both ends.
#' @param symbol_size Optional Numeric: Arrow symbol size.
#' @param symbol_offset Optional Numeric or Character: Arrow symbol offset.
#' @param line_style Optional [LineStyle]: Line styling.
#' @export
AxisLine <- S7::new_class(
  "AxisLine",
  properties = list(
    show = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.character(value) && length(value) <= 2L) {
          return(NULL)
        }
        "must be a string or length-2 character vector, or NULL"
      }
    ),
    symbol_size = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value)) {
          return(NULL)
        }
        "must be a numeric vector or NULL"
      }
    ),
    symbol_offset = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) || is.character(value)) {
          return(NULL)
        }
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
#' @param show Optional Logical or Character \{"auto"\}: Whether to show ticks.
#' @param inside Optional Logical: Whether ticks are inside the grid.
#' @param length Optional Numeric `[0, Inf)`: Tick mark length in pixels.
#' @param align_with_label Optional Logical: Whether to align ticks with labels.
#' @param line_style Optional [LineStyle]: Line styling.
#' @export
AxisTick <- S7::new_class(
  "AxisTick",
  properties = list(
    show = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param show Optional Logical: Whether to show minor ticks.
#' @param split_number Optional Numeric `[0, Inf)`: Number of minor ticks between two main ticks.
#' @param length Optional Numeric `[0, Inf)`: Minor tick length in pixels.
#' @param line_style Optional [LineStyle]: Line styling.
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
#' @param show Optional Logical: Whether to show split lines.
#' @param interval Optional Numeric or Character \{"auto"\}: Split-line interval.
#' @param show_min_line Optional Logical: Whether to show the split line at the minimum end.
#' @param show_max_line Optional Logical: Whether to show the split line at the maximum end.
#' @param line_style Optional [LineStyle]: Line styling.
#' @export
SplitLine <- S7::new_class(
  "SplitLine",
  properties = list(
    show = bool_or_null_property(),
    interval = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param show Optional Logical: Whether to show minor split lines.
#' @param line_style Optional [LineStyle]: Line styling.
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
#' @param show Optional Logical: Whether to show the split area.
#' @param interval Optional Numeric or Character \{"auto"\}: Split-area interval.
#' @param area_style Optional [AreaStyle]: Area styling.
#' @export
SplitArea <- S7::new_class(
  "SplitArea",
  properties = list(
    show = bool_or_null_property(),
    interval = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param show Optional Logical: Whether to show axis labels.
#' @param inside Optional Logical: Whether labels are inside the grid.
#' @param rotate Optional Numeric: Label rotation angle in degrees.
#' @param margin Optional Numeric `[0, Inf)`: Space between the label and axis line in pixels.
#' @param formatter Optional Character or function: Label formatter.
#' @param show_min_label Optional Logical: Whether to show the label at the minimum end.
#' @param show_max_label Optional Logical: Whether to show the label at the maximum end.
#' @param hide_overlap Optional Logical: Whether to hide overlapping labels.
#' @param interval Optional Numeric or Character \{"auto"\}: Label interval.
#' @param text_style Optional [TextStyle]: Text appearance.
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param type Optional Character \{"value", "category", "time", "log"\}: Axis type.
#' @param show Optional Logical: Whether to show the axis.
#' @param inverse Optional Logical: Whether to invert the axis.
#' @param name Optional Character: Axis name.
#' @param name_location Optional Character \{"start", "middle", "center", "end"\}: Axis name location.
#' @param name_rotate Optional Numeric: Axis name rotation angle in degrees.
#' @param name_gap Optional Numeric `[0, Inf)`: Gap between the axis name and axis line.
#' @param name_text_style Optional [TextStyle]: Axis name text style.
#' @param silent Optional Logical: Whether the axis is silent.
#' @param trigger_event Optional Logical: Whether the axis triggers events.
#' @param min Optional Numeric or Character \{"dataMin", "dataMax"\}: Minimum axis value.
#' @param max Optional Numeric or Character \{"dataMin", "dataMax"\}: Maximum axis value.
#' @param scale Optional Logical: Whether a value axis should avoid forcing zero.
#' @param split_number Optional Numeric `[0, Inf)`: Suggested number of split segments.
#' @param interval Optional Numeric `[0, Inf)`: Tick interval.
#' @param min_interval Optional Numeric `[0, Inf)`: Minimum auto-calculated tick interval.
#' @param max_interval Optional Numeric `[0, Inf)`: Maximum auto-calculated tick interval.
#' @param log_base Optional Numeric `[0, Inf)`: Logarithm base for log axes.
#' @param boundary_gap Optional Logical, Numeric, or Character: Gap at axis boundaries. Logical (category) or length-2
#'   vector of numbers/strings (numeric axes).
#' @param align_ticks Optional Logical: Whether to align ticks with the first axis.
#' @param data Optional Vector: Category data.
#' @param axis_line Optional [AxisLine]: Axis line configuration.
#' @param axis_tick Optional [AxisTick]: Axis tick configuration.
#' @param minor_tick Optional [MinorTick]: Minor tick configuration.
#' @param axis_label Optional [AxisLabel]: Axis label configuration.
#' @param split_line Optional [SplitLine]: Split line configuration.
#' @param minor_split_line Optional [MinorSplitLine]: Minor split line configuration.
#' @param split_area Optional [SplitArea]: Split area configuration.
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("dataMin", "dataMax")
        ) {
          return(NULL)
        }
        "must be a number, 'dataMin', 'dataMax', or NULL"
      }
    ),
    max = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("dataMin", "dataMax")
        ) {
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
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
