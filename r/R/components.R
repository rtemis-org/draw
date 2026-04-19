# components.R
# Component S7 classes: Grid, Title, Legend, Tooltip, VisualMap
#
# TS sources:
#   Grid:      src/coord/cartesian/GridModel.ts (GridOption, line 37)
#   Title:     src/component/title/install.ts (TitleOption, line 48)
#   Legend:    src/component/legend/LegendModel.ts (LegendOption, line 162)
#   Tooltip:   src/component/tooltip/TooltipModel.ts (TooltipOption, line 36)
#              + src/util/types.ts (CommonTooltipOption, line 1527)
#   VisualMap: src/component/visualMap/ContinuousModel.ts (ContinuousVisualMapOption)

# -- Grid -----------------------------------------------------------------------

#' Grid
#'
#' Configuration for the cartesian coordinate grid container.
#'
#' Corresponds to `GridOption` in `src/coord/cartesian/GridModel.ts` (line 37).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#grid}
#'
#' @param show Optional Logical: Whether to show the grid border.
#' @param left Optional Numeric or Character: Distance from the left side of the container.
#' @param right Optional Numeric or Character: Distance from the right side of the container.
#' @param top Optional Numeric or Character: Distance from the top of the container.
#' @param bottom Optional Numeric or Character: Distance from the bottom of the container.
#' @param width Optional Numeric or Character: Grid width.
#' @param height Optional Numeric or Character: Grid height.
#' @param contain_label Optional Logical: Whether the grid region contains axis labels.
#' @param background_color Optional Character: Grid background color.
#' @param border_width Optional Numeric `[0, Inf)`: Grid border width.
#' @param border_color Optional Character: Grid border color.
#' @param shadow_blur Optional Numeric `[0, Inf)`: Shadow blur radius.
#' @param shadow_color Optional Character: Shadow color.
#' @param shadow_offset_x Optional Numeric: Shadow horizontal offset.
#' @param shadow_offset_y Optional Numeric: Shadow vertical offset.
#' @export
Grid <- S7::new_class(
  "Grid",
  properties = list(
    show = bool_or_null_property(),
    # BoxLayoutOptionMixin
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property(),
    width = numeric_or_string_property(),
    height = numeric_or_string_property(),
    contain_label = bool_or_null_property(),
    background_color = color_property(),
    border_width = numeric_or_null_property(),
    border_color = color_property(),
    # ShadowOptionMixin
    shadow_blur = numeric_or_null_property(),
    shadow_color = color_property(),
    shadow_offset_x = numeric_or_null_property(),
    shadow_offset_y = numeric_or_null_property()
  )
)

S7::method(to_list, Grid) <- function(x, ...) {
  props_to_list(x)
}

# -- Title ----------------------------------------------------------------------

#' Title
#'
#' Chart title configuration with main text and optional subtext.
#'
#' Corresponds to `TitleOption` in `src/component/title/install.ts` (line 48).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#title}
#'
#' @param show Optional Logical: Whether to show the title.
#' @param text Optional Character: Main title text. Supports `\\n` for newlines.
#' @param link Optional Character: Hyperlink for the main title.
#' @param target Optional Character \{"self", "blank"\}: Link target.
#' @param subtext Optional Character: Subtitle text.
#' @param sublink Optional Character: Hyperlink for the subtitle.
#' @param subtarget Optional Character \{"self", "blank"\}: Subtitle link target.
#' @param text_align Optional Character \{"auto", "left", "center", "right"\}: Horizontal alignment.
#' @param text_vertical_align Optional Character \{"auto", "top", "middle", "bottom"\}: Vertical alignment.
#' @param padding Optional Numeric: Padding inside the title box.
#' @param item_gap Optional Numeric `[0, Inf)`: Gap between title and subtitle in pixels.
#' @param text_style Optional [TextStyle]: Main title text style.
#' @param subtext_style Optional [TextStyle]: Subtitle text style.
#' @param trigger_event Optional Logical: Whether the title emits events.
#' @param background_color Optional Character: Title-box background color.
#' @param border_color Optional Character: Border color.
#' @param border_width Optional Numeric `[0, Inf)`: Border width.
#' @param border_radius Optional Numeric: Border radius.
#' @param left Optional Numeric or Character: Left position.
#' @param right Optional Numeric or Character: Right position.
#' @param top Optional Numeric or Character: Top position.
#' @param bottom Optional Numeric or Character: Bottom position.
#' @export
Title <- S7::new_class(
  "Title",
  properties = list(
    show = bool_or_null_property(),
    text = string_or_null_property(),
    link = string_or_null_property(),
    target = enum_property(c("self", "blank")),
    subtext = string_or_null_property(),
    sublink = string_or_null_property(),
    subtarget = enum_property(c("self", "blank")),
    text_align = enum_property(c("auto", "left", "center", "right")),
    text_vertical_align = enum_property(c("auto", "top", "middle", "bottom")),
    padding = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) {
          return(NULL)
        }
        "must be a number, or length-2/4 numeric vector, or NULL"
      }
    ),
    item_gap = numeric_or_null_property(),
    text_style = class_or_null_property(TextStyle),
    subtext_style = class_or_null_property(TextStyle),
    trigger_event = bool_or_null_property(),
    background_color = color_property(),
    border_color = color_property(),
    border_width = numeric_or_null_property(),
    border_radius = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) %in% c(1L, 4L)) {
          return(NULL)
        }
        "must be a number or length-4 numeric vector, or NULL"
      }
    ),
    # BoxLayoutOptionMixin
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property()
  )
)

S7::method(to_list, Title) <- function(x, ...) {
  props_to_list(x)
}

# -- Legend ---------------------------------------------------------------------

#' Legend
#'
#' Legend configuration for selecting/filtering series.
#'
#' Corresponds to `LegendOption` in `src/component/legend/LegendModel.ts` (line 162).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#legend}
#'
#' @param show Optional Logical: Whether to show the legend.
#' @param orient Optional Character \{"horizontal", "vertical"\}: Layout orientation.
#' @param align Optional Character \{"auto", "left", "right"\}: Marker and text alignment.
#' @param left Optional Numeric or Character: Left position.
#' @param right Optional Numeric or Character: Right position.
#' @param top Optional Numeric or Character: Top position.
#' @param bottom Optional Numeric or Character: Bottom position.
#' @param width Optional Numeric or Character: Legend box width.
#' @param height Optional Numeric or Character: Legend box height.
#' @param padding Optional Numeric: Padding inside the legend box.
#' @param item_gap Optional Numeric `[0, Inf)`: Gap between legend items.
#' @param item_width Optional Numeric `[0, Inf)`: Legend symbol width.
#' @param item_height Optional Numeric `[0, Inf)`: Legend symbol height.
#' @param icon Optional Character: Default legend icon.
#' @param selected_mode Optional Logical or Character \{"single", "multiple"\}: Selection mode.
#' @param selected Optional Named logical vector: Pre-selected items.
#' @param formatter Optional Character or function: Label formatter.
#' @param inactive_color Optional Character: Color for unselected items.
#' @param inactive_border_color Optional Character: Border color for unselected items.
#' @param text_style Optional [TextStyle]: Legend label text style.
#' @param item_style Optional [ItemStyle]: Legend icon style.
#' @param background_color Optional Character: Background color.
#' @param border_color Optional Character: Border color.
#' @param border_width Optional Numeric `[0, Inf)`: Border width.
#' @param border_radius Optional Numeric: Border radius.
#' @param trigger_event Optional Logical: Whether the legend emits events.
#' @param data Optional Character vector or list: Legend data items.
#' @export
Legend <- S7::new_class(
  "Legend",
  properties = list(
    show = bool_or_null_property(),
    orient = enum_property(c("horizontal", "vertical")),
    align = enum_property(c("auto", "left", "right")),
    # BoxLayoutOptionMixin
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property(),
    width = numeric_or_string_property(),
    height = numeric_or_string_property(),
    padding = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) {
          return(NULL)
        }
        "must be a number, or length-2/4 numeric vector, or NULL"
      }
    ),
    item_gap = numeric_or_null_property(),
    item_width = numeric_or_null_property(),
    item_height = numeric_or_null_property(),
    icon = string_or_null_property(),
    selected_mode = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("single", "multiple")
        ) {
          return(NULL)
        }
        "must be TRUE/FALSE, 'single', 'multiple', or NULL"
      }
    ),
    selected = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && !is.null(names(value))) {
          return(NULL)
        }
        "must be a named logical vector or NULL"
      }
    ),
    formatter = S7::new_property(class = S7::class_any, default = NULL),
    inactive_color = color_property(),
    inactive_border_color = color_property(),
    text_style = class_or_null_property(TextStyle),
    item_style = class_or_null_property(ItemStyle),
    background_color = color_property(),
    border_color = color_property(),
    border_width = numeric_or_null_property(),
    border_radius = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) %in% c(1L, 4L)) {
          return(NULL)
        }
        "must be a number or length-4 numeric vector, or NULL"
      }
    ),
    trigger_event = bool_or_null_property(),
    data = S7::new_property(class = S7::class_any, default = NULL)
  )
)

S7::method(to_list, Legend) <- function(x, ...) {
  props_to_list(x)
}

# -- Tooltip --------------------------------------------------------------------

#' Tooltip
#'
#' Tooltip configuration for showing data on hover/click.
#'
#' Corresponds to `TooltipOption` in `src/component/tooltip/TooltipModel.ts` (line 36)
#' and `CommonTooltipOption` in `src/util/types.ts` (line 1527).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#tooltip}
#'
#' @param show Optional Logical: Whether to show the tooltip.
#' @param trigger Optional Character \{"item", "axis", "none"\}: Trigger type.
#' @param trigger_on Optional Character \{"mousemove", "click", "none", "mousemove|click"\}: Trigger event.
#'   `"mousemove|click"`.
#' @param show_content Optional Logical: Whether to show tooltip content.
#' @param always_show_content Optional Logical: Whether to keep the tooltip visible.
#' @param formatter Optional Character or function: Tooltip content formatter.
#' @param value_formatter Optional function: Formatter for the value portion.
#' @param position Optional Character or Numeric: Tooltip position.
#' @param confine Optional Logical: Whether to confine the tooltip within the chart container.
#' @param enterable Optional Logical: Whether the mouse can enter the tooltip.
#' @param show_delay Optional Numeric `[0, Inf)`: Delay before showing, in milliseconds.
#' @param hide_delay Optional Numeric `[0, Inf)`: Delay before hiding, in milliseconds.
#' @param transition_duration Optional Numeric `[0, Inf)`: Transition duration in seconds.
#' @param background_color Optional Character: Background color.
#' @param border_color Optional Character: Border color.
#' @param border_width Optional Numeric `[0, Inf)`: Border width.
#' @param border_radius Optional Numeric `[0, Inf)`: Border radius.
#' @param padding Optional Numeric: Padding inside the tooltip.
#' @param text_style Optional [TextStyle]: Tooltip text style.
#' @param extra_css_text Optional Character: Extra CSS for HTML render mode.
#' @param order Optional Character \{"seriesAsc", "seriesDesc", "valueAsc", "valueDesc"\}: Tooltip content order.
#' @param class_name Optional Character: CSS class name for the tooltip element.
#' @export
Tooltip <- S7::new_class(
  "Tooltip",
  properties = list(
    # CommonTooltipOption
    show = bool_or_null_property(),
    trigger = enum_property(c("item", "axis", "none")),
    trigger_on = enum_property(c(
      "mousemove",
      "click",
      "none",
      "mousemove|click"
    )),
    show_content = bool_or_null_property(),
    always_show_content = bool_or_null_property(),
    formatter = S7::new_property(class = S7::class_any, default = NULL),
    value_formatter = S7::new_property(class = S7::class_any, default = NULL),
    position = S7::new_property(class = S7::class_any, default = NULL),
    confine = bool_or_null_property(),
    enterable = bool_or_null_property(),
    show_delay = numeric_or_null_property(),
    hide_delay = numeric_or_null_property(),
    transition_duration = numeric_or_null_property(),
    background_color = color_property(),
    border_color = color_property(),
    border_width = numeric_or_null_property(),
    border_radius = numeric_or_null_property(),
    padding = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) {
          return(NULL)
        }
        "must be a number, or length-2/4 numeric vector, or NULL"
      }
    ),
    text_style = class_or_null_property(TextStyle),
    extra_css_text = string_or_null_property(),
    # TooltipOption-specific
    order = enum_property(c(
      "seriesAsc",
      "seriesDesc",
      "valueAsc",
      "valueDesc"
    )),
    class_name = string_or_null_property()
  )
)

S7::method(to_list, Tooltip) <- function(x, ...) {
  # class_name -> className (handled by snake_to_camel)
  # but we need the rename for class_name since className is the correct echarts key
  props_to_list(x)
}

# -- VisualMap ------------------------------------------------------------------

#' Visual Map
#'
#' Continuous visual map component that maps data values to visual properties
#' (typically color). Required for heatmaps.
#'
#' Corresponds to `ContinuousVisualMapOption` in
#' `src/component/visualMap/ContinuousModel.ts`.
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#visualMap-continuous}
#'
#' @param type Character \{"continuous", "piecewise"\}: Visual map type.
#' @param min Optional Numeric: Minimum value of the mapped data range.
#' @param max Optional Numeric: Maximum value of the mapped data range.
#' @param precision Optional Integer `[0, Inf)`: Decimal places shown in colorbar
#'   labels and the draggable-handle tooltip. ECharts defaults to `0`, which
#'   rounds labels to whole numbers — set this to match `value_digits` for
#'   accurate display.
#' @param calculable Optional Logical: Whether to show drag handles for interactive range selection.
#' @param show Optional Logical: Whether to display the visual map legend.
#' @param orient Optional Character \{"vertical", "horizontal"\}: Orientation of the color bar.
#' @param in_range Optional list: Visual encoding for in-range values.
#'   Typically `list(color = c("blue", "white", "red"))`.
#' @param out_of_range Optional list: Visual encoding for out-of-range values.
#' @param left Optional Numeric or Character: Distance from the left of the container.
#' @param right Optional Numeric or Character: Distance from the right of the container.
#' @param top Optional Numeric or Character: Distance from the top of the container.
#' @param bottom Optional Numeric or Character: Distance from the bottom of the container.
#' @export
VisualMap <- S7::new_class(
  "VisualMap",
  properties = list(
    type = enum_property(c("continuous", "piecewise"), default = "continuous"),
    min = numeric_or_null_property(),
    max = numeric_or_null_property(),
    precision = numeric_or_null_property(),
    calculable = bool_or_null_property(),
    show = bool_or_null_property(),
    orient = enum_property(c("vertical", "horizontal")),
    in_range = S7::new_property(class = S7::class_any, default = NULL),
    out_of_range = S7::new_property(class = S7::class_any, default = NULL),
    # BoxLayoutOptionMixin
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property()
  )
)

S7::method(to_list, VisualMap) <- function(x, ...) {
  props_to_list(x)
}

# -- DataZoom -------------------------------------------------------------------

#' Axis-index property validator
#'
#' Accepts a single number, numeric vector, the literal `"all"`, or `NULL`.
#' Used by `DataZoom` axis-index slots.
#' @keywords internal
#' @noRd
axis_index_property <- function() {
  S7::new_property(
    class = S7::class_any,
    default = NULL,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (is.numeric(value)) {
        return(NULL)
      }
      if (
        is.character(value) &&
          length(value) == 1L &&
          identical(value, "all")
      ) {
        return(NULL)
      }
      "must be a number, numeric vector, 'all', or NULL"
    }
  )
}

#' Mouse-modifier property validator
#'
#' Accepts `TRUE`, `FALSE`, or one of `"shift"`, `"ctrl"`, `"alt"`, or `NULL`.
#' Used by the inside-zoom mouse-control slots of `DataZoom`.
#' @keywords internal
#' @noRd
mouse_modifier_property <- function() {
  S7::new_property(
    class = S7::class_any,
    default = NULL,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (is.logical(value) && length(value) == 1L) {
        return(NULL)
      }
      if (
        is.character(value) &&
          length(value) == 1L &&
          value %in% c("shift", "ctrl", "alt")
      ) {
        return(NULL)
      }
      "must be TRUE, FALSE, 'shift', 'ctrl', 'alt', or NULL"
    }
  )
}

#' Data Zoom
#'
#' Axis-range zoom component. ECharts renders `dataZoom` as an array; common
#' usage pairs a `"slider"` (visible scrollbar) with an `"inside"` (mouse-wheel
#' / drag) entry targeting the same axis. Unused styling / mouse-behavior
#' fields stay `NULL` and are dropped from the serialized output.
#'
#' Corresponds to `DataZoomOption` in
#' `src/component/dataZoom/DataZoomModel.ts` (line 38),
#' `SliderDataZoomOption` in `src/component/dataZoom/SliderZoomModel.ts`
#' (line 38), and `InsideDataZoomOption` in
#' `src/component/dataZoom/InsideZoomModel.ts` (line 23).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#dataZoom}
#'
#' @param type Character \{"slider", "inside"\}: Zoom type. `"slider"` shows a
#'   draggable handle; `"inside"` enables zoom/drag directly on the coordinate
#'   system.
#' @param id Optional Character: Component identifier used for updates.
#' @param disabled Optional Logical: Whether the component is disabled.
#' @param x_axis_index Optional Numeric, numeric vector, or Character \{"all"\}:
#'   Index of the x-axis (or axes) this zoom targets.
#' @param y_axis_index Optional Numeric, numeric vector, or Character \{"all"\}:
#'   Index of the y-axis (or axes) this zoom targets.
#' @param radius_axis_index Optional Numeric, numeric vector, or Character
#'   \{"all"\}: Polar radius axis index.
#' @param angle_axis_index Optional Numeric, numeric vector, or Character
#'   \{"all"\}: Polar angle axis index.
#' @param single_axis_index Optional Numeric, numeric vector, or Character
#'   \{"all"\}: Single-axis index.
#' @param filter_mode Optional Character \{"filter", "weakFilter", "empty",
#'   "none"\}: How out-of-window data points are handled.
#' @param start Optional Numeric \[0, 100\]: Left/start position as a
#'   percentage of the full data range.
#' @param end Optional Numeric \[0, 100\]: Right/end position as a percentage
#'   of the full data range.
#' @param start_value Optional Numeric, Character, or Date: Absolute start
#'   value (alternative to `start`).
#' @param end_value Optional Numeric, Character, or Date: Absolute end value
#'   (alternative to `end`).
#' @param min_span Optional Numeric \[0, 100\]: Minimum window size, as a
#'   percentage.
#' @param max_span Optional Numeric \[0, 100\]: Maximum window size, as a
#'   percentage.
#' @param min_value_span Optional Numeric or Character: Minimum window size in
#'   data units.
#' @param max_value_span Optional Numeric or Character: Maximum window size in
#'   data units.
#' @param orient Optional Character \{"horizontal", "vertical"\}: Layout
#'   orientation. Defaults are inferred from the axis the zoom targets.
#' @param throttle Optional Numeric \[0, Inf): Throttle delay in milliseconds
#'   for zoom events.
#' @param range_mode Optional length-2 Character vector of \{"value",
#'   "percent"\}: Per-boundary interpretation for start/end.
#' @param show Optional Logical: Whether the slider is visible
#'   (slider type only).
#' @param background_color Optional Character: Slider background color.
#' @param left Optional Numeric or Character: Slider left position.
#' @param right Optional Numeric or Character: Slider right position.
#' @param top Optional Numeric or Character: Slider top position.
#' @param bottom Optional Numeric or Character: Slider bottom position.
#' @param width Optional Numeric or Character: Slider width.
#' @param height Optional Numeric or Character: Slider height.
#' @param zoom_lock Optional Logical: Whether to lock the window size
#'   (allow move but not zoom).
#' @param zoom_on_mouse_wheel Optional Logical or Character \{"shift", "ctrl",
#'   "alt"\}: Whether/how the mouse wheel zooms (inside type only).
#' @param move_on_mouse_move Optional Logical or Character \{"shift", "ctrl",
#'   "alt"\}: Whether/how mouse drag pans (inside type only).
#' @param move_on_mouse_wheel Optional Logical or Character \{"shift", "ctrl",
#'   "alt"\}: Whether/how the mouse wheel pans (inside type only).
#' @param prevent_default_mouse_move Optional Logical: Whether to call
#'   `event.preventDefault()` on drag (inside type only).
#' @export
DataZoom <- S7::new_class(
  "DataZoom",
  properties = list(
    # Common (DataZoomOption)
    type = enum_property(c("slider", "inside"), default = "slider"),
    id = string_or_null_property(),
    disabled = bool_or_null_property(),
    x_axis_index = axis_index_property(),
    y_axis_index = axis_index_property(),
    radius_axis_index = axis_index_property(),
    angle_axis_index = axis_index_property(),
    single_axis_index = axis_index_property(),
    filter_mode = enum_property(c(
      "filter",
      "weakFilter",
      "empty",
      "none"
    )),
    start = numeric_or_null_property(),
    end = numeric_or_null_property(),
    start_value = S7::new_property(class = S7::class_any, default = NULL),
    end_value = S7::new_property(class = S7::class_any, default = NULL),
    min_span = numeric_or_null_property(),
    max_span = numeric_or_null_property(),
    min_value_span = numeric_or_string_property(),
    max_value_span = numeric_or_string_property(),
    orient = enum_property(c("horizontal", "vertical")),
    throttle = numeric_or_null_property(),
    range_mode = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 2L &&
            all(value %in% c("value", "percent"))
        ) {
          return(NULL)
        }
        "must be a length-2 character vector of 'value'/'percent', or NULL"
      }
    ),
    # SliderDataZoomOption-specific
    show = bool_or_null_property(),
    background_color = color_property(),
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property(),
    width = numeric_or_string_property(),
    height = numeric_or_string_property(),
    zoom_lock = bool_or_null_property(),
    # InsideDataZoomOption-specific
    zoom_on_mouse_wheel = mouse_modifier_property(),
    move_on_mouse_move = mouse_modifier_property(),
    move_on_mouse_wheel = mouse_modifier_property(),
    prevent_default_mouse_move = bool_or_null_property()
  )
)

S7::method(to_list, DataZoom) <- function(x, ...) {
  props_to_list(x)
}
