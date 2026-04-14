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
#' @param color Optional Character: CSS line color.
#' @param width Optional Numeric `[0, Inf)`: Line width in pixels.
#' @param opacity Optional Numeric `[0, 1]`: Line opacity.
#' @param type Optional Character \{"solid", "dashed", "dotted"\}: Line type.
#' @param cap Optional Character \{"butt", "round", "square"\}: Line cap.
#' @param join Optional Character \{"bevel", "round", "miter"\}: Line join.
#' @param dash_offset Optional Numeric: Dash offset for dashed lines.
#' @param miter_limit Optional Numeric `[0, Inf)`: Miter limit for miter joins.
#' @param shadow_blur Optional Numeric `[0, Inf)`: Shadow blur radius.
#' @param shadow_color Optional Character: Shadow color.
#' @param shadow_offset_x Optional Numeric: Shadow horizontal offset.
#' @param shadow_offset_y Optional Numeric: Shadow vertical offset.
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
#' @param color Optional Character: CSS fill color.
#' @param opacity Optional Numeric `[0, 1]`: Fill opacity.
#' @param origin Optional Character \{"auto", "start", "end"\} or Numeric: Area origin.
#' @param shadow_blur Optional Numeric `[0, Inf)`: Shadow blur radius.
#' @param shadow_color Optional Character: Shadow color.
#' @param shadow_offset_x Optional Numeric: Shadow horizontal offset.
#' @param shadow_offset_y Optional Numeric: Shadow vertical offset.
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.character(value) && value %in% c("auto", "start", "end")) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param color Optional Character: CSS fill color.
#' @param opacity Optional Numeric `[0, 1]`: Fill opacity.
#' @param border_color Optional Character: Border color.
#' @param border_width Optional Numeric `[0, Inf)`: Border width in pixels.
#' @param border_type Optional Character \{"solid", "dashed", "dotted"\}: Border line type.
#' @param border_cap Optional Character \{"butt", "round", "square"\}: Border line cap.
#' @param border_join Optional Character \{"bevel", "round", "miter"\}: Border line join.
#' @param border_dash_offset Optional Numeric: Border dash offset.
#' @param border_miter_limit Optional Numeric `[0, Inf)`: Border miter limit.
#' @param border_radius Optional Numeric, Character, or Numeric vector: Corner border radius.
#' @param decal Optional Any: Decal pattern for accessibility.
#' @param shadow_blur Optional Numeric `[0, Inf)`: Shadow blur radius.
#' @param shadow_color Optional Character: Shadow color.
#' @param shadow_offset_x Optional Numeric: Shadow horizontal offset.
#' @param shadow_offset_y Optional Numeric: Shadow vertical offset.
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value)) {
          return(NULL)
        }
        if (is.character(value) && length(value) == 1L) {
          return(NULL)
        }
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
#' @param color Optional Character: Text color.
#' @param font_style Optional Character \{"normal", "italic", "oblique"\}: Font style.
#' @param font_weight Optional Character \{"normal", "bold", "bolder", "lighter"\} or Numeric `[100, 900]`: Font weight.
#' @param font_family Optional Character: Font family name.
#' @param font_size Optional Numeric or Character: Font size.
#' @param align Optional Character \{"left", "center", "right"\}: Horizontal alignment.
#' @param vertical_align Optional Character \{"top", "middle", "bottom"\}: Vertical alignment.
#' @param opacity Optional Numeric `[0, 1]`: Text opacity.
#' @param line_height Optional Numeric `[0, Inf)`: Line height in pixels.
#' @param background_color Optional Character: Background color behind text.
#' @param border_color Optional Character: Text box border color.
#' @param border_width Optional Numeric `[0, Inf)`: Text box border width.
#' @param border_type Optional Character \{"solid", "dashed", "dotted"\}: Text box border type.
#' @param border_dash_offset Optional Numeric: Text box border dash offset.
#' @param border_radius Optional Numeric or Numeric vector: Text box corner radius.
#' @param padding Optional Numeric or Numeric vector: Text box padding.
#' @param width Optional Numeric or Character: Text box width.
#' @param height Optional Numeric `[0, Inf)`: Text box height.
#' @param text_border_color Optional Character: Stroke color around the text.
#' @param text_border_width Optional Numeric `[0, Inf)`: Stroke width around the text.
#' @param text_border_type Optional Character \{"solid", "dashed", "dotted"\}: Stroke type around the text.
#' @param text_border_dash_offset Optional Numeric: Stroke dash offset around the text.
#' @param text_shadow_blur Optional Numeric `[0, Inf)`: Text shadow blur radius.
#' @param text_shadow_color Optional Character: Text shadow color.
#' @param text_shadow_offset_x Optional Numeric: Text shadow horizontal offset.
#' @param text_shadow_offset_y Optional Numeric: Text shadow vertical offset.
#' @param overflow Optional Character \{"truncate", "break", "breakAll", "none"\}: Text overflow behavior.
#' @param ellipsis Optional Character: Ellipsis string used when truncating.
#' @param rich Optional Named list: Rich text styles.
#' @param shadow_blur Optional Numeric `[0, Inf)`: Box shadow blur radius.
#' @param shadow_color Optional Character: Box shadow color.
#' @param shadow_offset_x Optional Numeric: Box shadow horizontal offset.
#' @param shadow_offset_y Optional Numeric: Box shadow vertical offset.
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            value %in% c("normal", "bold", "bolder", "lighter")
        ) {
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
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value)) {
          return(NULL)
        }
        "must be a number, numeric vector, or NULL"
      }
    ),
    padding = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.numeric(value)) {
          return(NULL)
        }
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
