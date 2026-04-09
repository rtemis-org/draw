# styles.R
# Core style S7 classes: LineStyle, AreaStyle, ItemStyle, TextStyle
#
# These are the building blocks used across all series and components.
# Each corresponds to an interface in echarts src/util/types.ts.

# -- LineStyle -------------------------------------------------------------------

#' Line Style
#'
#' Stroke styling for lines in series, axes, and components.
#'
#' Corresponds to `LineStyleOption` in `src/util/types.ts` (line 1154).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-line.lineStyle}
#'
#' @param color Line color. CSS color string.
#' @param width Line width in pixels.
#' @param opacity Opacity (0-1).
#' @param type Line type: `"solid"`, `"dashed"`, or `"dotted"`.
#' @param cap Line cap: `"butt"`, `"round"`, or `"square"`.
#' @param join Line join: `"bevel"`, `"round"`, or `"miter"`.
#' @param dash_offset Dash offset for dashed lines.
#' @param miter_limit Miter limit for miter joins.
#' @param shadow_blur Shadow blur radius.
#' @param shadow_color Shadow color.
#' @param shadow_offset_x Shadow horizontal offset.
#' @param shadow_offset_y Shadow vertical offset.
#' @export
LineStyle <- S7::new_class(
  "LineStyle",
  properties = list(
    color = color_property(),
    width = numeric_or_null_property(),
    opacity = numeric_or_null_property(),
    type = enum_property(c("solid", "dashed", "dotted")),
    cap = enum_property(c("butt", "round", "square")),
    join = enum_property(c("bevel", "round", "miter")),
    dash_offset = numeric_or_null_property(),
    miter_limit = numeric_or_null_property(),
    # ShadowOptionMixin
    shadow_blur = numeric_or_null_property(),
    shadow_color = color_property(),
    shadow_offset_x = numeric_or_null_property(),
    shadow_offset_y = numeric_or_null_property()
  )
)

S7::method(to_list, LineStyle) <- function(x, ...) {
  props_to_list(x)
}

# -- AreaStyle -------------------------------------------------------------------

#' Area Style
#'
#' Fill styling for areas (polygons, area under curves, etc.).
#'
#' Corresponds to `AreaStyleOption` in `src/util/types.ts` (line 1169).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-line.areaStyle}
#'
#' @param color Fill color. CSS color string.
#' @param opacity Opacity (0-1).
#' @param origin Area origin: `"auto"`, `"start"`, or `"end"`.
#' @param shadow_blur Shadow blur radius.
#' @param shadow_color Shadow color.
#' @param shadow_offset_x Shadow horizontal offset.
#' @param shadow_offset_y Shadow vertical offset.
#' @export
AreaStyle <- S7::new_class(
  "AreaStyle",
  properties = list(
    color = color_property(),
    opacity = numeric_or_null_property(),
    origin = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.character(value) && value %in% c("auto", "start", "end")) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        "must be 'auto', 'start', 'end', a number, or NULL"
      }
    ),
    # ShadowOptionMixin
    shadow_blur = numeric_or_null_property(),
    shadow_color = color_property(),
    shadow_offset_x = numeric_or_null_property(),
    shadow_offset_y = numeric_or_null_property()
  )
)

S7::method(to_list, AreaStyle) <- function(x, ...) {
  props_to_list(x)
}

# -- ItemStyle -------------------------------------------------------------------

#' Item Style
#'
#' Styling for graphical items (bar segments, scatter points, pie slices, etc.).
#' Includes both fill and stroke options.
#'
#' Corresponds to `ItemStyleOption` in `src/util/types.ts` (line 1142).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-bar.itemStyle}
#'
#' @param color Fill color. CSS color string.
#' @param opacity Opacity (0-1).
#' @param border_color Border stroke color.
#' @param border_width Border width in pixels.
#' @param border_type Border line type: `"solid"`, `"dashed"`, or `"dotted"`.
#' @param border_cap Border line cap: `"butt"`, `"round"`, or `"square"`.
#' @param border_join Border line join: `"bevel"`, `"round"`, or `"miter"`.
#' @param border_dash_offset Border dash offset.
#' @param border_miter_limit Border miter limit.
#' @param border_radius Corner border radius. Single number or vector of 4.
#' @param decal Decal pattern for accessibility. `"none"` to disable.
#' @param shadow_blur Shadow blur radius.
#' @param shadow_color Shadow color.
#' @param shadow_offset_x Shadow horizontal offset.
#' @param shadow_offset_y Shadow vertical offset.
#' @export
ItemStyle <- S7::new_class(
  "ItemStyle",
  properties = list(
    color = color_property(),
    opacity = numeric_or_null_property(),
    # BorderOptionMixin
    border_color = color_property(),
    border_width = numeric_or_null_property(),
    border_type = enum_property(c("solid", "dashed", "dotted")),
    border_cap = enum_property(c("butt", "round", "square")),
    border_join = enum_property(c("bevel", "round", "miter")),
    border_dash_offset = numeric_or_null_property(),
    border_miter_limit = numeric_or_null_property(),
    border_radius = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value)) return(NULL)
        if (is.character(value) && length(value) == 1L) return(NULL)
        "must be a number, numeric vector, string, or NULL"
      }
    ),
    decal = S7::new_property(class = S7::class_any, default = NULL),
    # ShadowOptionMixin
    shadow_blur = numeric_or_null_property(),
    shadow_color = color_property(),
    shadow_offset_x = numeric_or_null_property(),
    shadow_offset_y = numeric_or_null_property()
  )
)

S7::method(to_list, ItemStyle) <- function(x, ...) {
  props_to_list(x)
}

# -- TextStyle -------------------------------------------------------------------

#' Text Style
#'
#' Text styling used in labels, titles, tooltips, and other text elements.
#'
#' Corresponds to `TextCommonOption` in `src/util/types.ts` (line 1220).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#title.textStyle}
#'
#' @param color Text color.
#' @param font_style Font style: `"normal"`, `"italic"`, or `"oblique"`.
#' @param font_weight Font weight: `"normal"`, `"bold"`, `"bolder"`, `"lighter"`,
#'   or a number (100-900).
#' @param font_family Font family name.
#' @param font_size Font size in pixels (or string like `"20px"`).
#' @param align Horizontal alignment: `"left"`, `"center"`, or `"right"`.
#' @param vertical_align Vertical alignment: `"top"`, `"middle"`, or `"bottom"`.
#' @param opacity Text opacity (0-1).
#' @param line_height Line height in pixels.
#' @param background_color Background color behind text.
#' @param border_color Text box border color.
#' @param border_width Text box border width.
#' @param border_type Text box border type.
#' @param border_dash_offset Text box border dash offset.
#' @param border_radius Text box corner radius.
#' @param padding Text box padding. Single number or vector of up to 4.
#' @param width Text box width.
#' @param height Text box height.
#' @param text_border_color Stroke color around the text itself.
#' @param text_border_width Stroke width around the text itself.
#' @param text_border_type Stroke type around the text itself.
#' @param text_border_dash_offset Stroke dash offset around the text itself.
#' @param text_shadow_blur Text shadow blur radius.
#' @param text_shadow_color Text shadow color.
#' @param text_shadow_offset_x Text shadow horizontal offset.
#' @param text_shadow_offset_y Text shadow vertical offset.
#' @param overflow Text overflow behavior: `"truncate"`, `"break"`, `"breakAll"`.
#' @param ellipsis Ellipsis string when overflow is `"truncate"`.
#' @param rich Named list of rich text styles (each a TextStyle-like list).
#' @param shadow_blur Box shadow blur (from ShadowOptionMixin).
#' @param shadow_color Box shadow color.
#' @param shadow_offset_x Box shadow horizontal offset.
#' @param shadow_offset_y Box shadow vertical offset.
#' @export
TextStyle <- S7::new_class(
  "TextStyle",
  properties = list(
    color = color_property(),
    font_style = enum_property(c("normal", "italic", "oblique")),
    font_weight = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && value %in% c("normal", "bold", "bolder", "lighter")) {
          return(NULL)
        }
        "must be 'normal', 'bold', 'bolder', 'lighter', a number, or NULL"
      }
    ),
    font_family = string_or_null_property(),
    font_size = numeric_or_string_property(),
    align = enum_property(c("left", "center", "right")),
    vertical_align = enum_property(c("top", "middle", "bottom")),
    opacity = numeric_or_null_property(),
    line_height = numeric_or_null_property(),
    background_color = color_property(),
    border_color = color_property(),
    border_width = numeric_or_null_property(),
    border_type = enum_property(c("solid", "dashed", "dotted")),
    border_dash_offset = numeric_or_null_property(),
    border_radius = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value)) return(NULL)
        "must be a number, numeric vector, or NULL"
      }
    ),
    padding = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value)) return(NULL)
        "must be a number, numeric vector, or NULL"
      }
    ),
    width = numeric_or_string_property(),
    height = numeric_or_null_property(),
    text_border_color = color_property(),
    text_border_width = numeric_or_null_property(),
    text_border_type = enum_property(c("solid", "dashed", "dotted")),
    text_border_dash_offset = numeric_or_null_property(),
    text_shadow_blur = numeric_or_null_property(),
    text_shadow_color = color_property(),
    text_shadow_offset_x = numeric_or_null_property(),
    text_shadow_offset_y = numeric_or_null_property(),
    overflow = enum_property(c("truncate", "break", "breakAll", "none")),
    ellipsis = string_or_null_property(),
    rich = S7::new_property(class = S7::class_any, default = NULL),
    # ShadowOptionMixin (box-level shadow)
    shadow_blur = numeric_or_null_property(),
    shadow_color = color_property(),
    shadow_offset_x = numeric_or_null_property(),
    shadow_offset_y = numeric_or_null_property()
  )
)

S7::method(to_list, TextStyle) <- function(x, ...) {
  props_to_list(x)
}
