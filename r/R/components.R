# components.R
# Component S7 classes: Grid, Title, Legend, Tooltip
#
# TS sources:
#   Grid:    src/coord/cartesian/GridModel.ts (GridOption, line 37)
#   Title:   src/component/title/install.ts (TitleOption, line 48)
#   Legend:  src/component/legend/LegendModel.ts (LegendOption, line 162)
#   Tooltip: src/component/tooltip/TooltipModel.ts (TooltipOption, line 36)
#            + src/util/types.ts (CommonTooltipOption, line 1527)

# -- Grid -----------------------------------------------------------------------

#' Grid
#'
#' Configuration for the cartesian coordinate grid container.
#'
#' Corresponds to `GridOption` in `src/coord/cartesian/GridModel.ts` (line 37).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#grid}
#'
#' @param show Whether to show the grid border.
#' @param left Distance from left side of container. Number (px) or percent string.
#' @param right Distance from right side.
#' @param top Distance from top.
#' @param bottom Distance from bottom.
#' @param width Grid width. Number (px) or percent string.
#' @param height Grid height.
#' @param contain_label Whether the grid region contains axis labels.
#' @param background_color Grid background color.
#' @param border_width Grid border width.
#' @param border_color Grid border color.
#' @param shadow_blur Shadow blur size.
#' @param shadow_color Shadow color.
#' @param shadow_offset_x Shadow horizontal offset.
#' @param shadow_offset_y Shadow vertical offset.
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
#' @param show Whether to show the title.
#' @param text Main title text. Supports `\\n` for newlines.
#' @param link Hyperlink for main title.
#' @param target Link target: `"self"` or `"blank"`.
#' @param subtext Subtitle text.
#' @param sublink Hyperlink for subtitle.
#' @param subtarget Subtitle link target.
#' @param text_align Horizontal alignment: `"auto"`, `"left"`, `"center"`, `"right"`.
#' @param text_vertical_align Vertical alignment: `"auto"`, `"top"`, `"middle"`, `"bottom"`.
#' @param padding Padding inside the title box. Single number or length-4 vector.
#' @param item_gap Gap between title and subtitle in pixels.
#' @param text_style A [TextStyle] for the main title text.
#' @param subtext_style A [TextStyle] for the subtitle text.
#' @param trigger_event Whether title emits events.
#' @param background_color Background color of the title box.
#' @param border_color Border color.
#' @param border_width Border width.
#' @param border_radius Border radius. Single number or length-4 vector.
#' @param left,right,top,bottom Position. Number (px) or percent string.
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
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) return(NULL)
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
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) %in% c(1L, 4L)) return(NULL)
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
#' @param show Whether to show the legend.
#' @param orient Layout orientation: `"horizontal"` or `"vertical"`.
#' @param align Legend marker and text alignment: `"auto"`, `"left"`, `"right"`.
#' @param left,right,top,bottom Position.
#' @param width,height Legend box dimensions.
#' @param padding Padding inside the legend box.
#' @param item_gap Gap between legend items.
#' @param item_width Width of legend symbol.
#' @param item_height Height of legend symbol.
#' @param icon Default legend icon.
#' @param selected_mode Selection mode: TRUE, FALSE, `"single"`, or `"multiple"`.
#' @param selected Named logical vector of pre-selected items.
#' @param formatter Label formatter: string template or function.
#' @param inactive_color Color for unselected items.
#' @param inactive_border_color Border color for unselected items.
#' @param text_style A [TextStyle] for legend labels.
#' @param item_style An [ItemStyle] for legend icons.
#' @param background_color Background color.
#' @param border_color Border color.
#' @param border_width Border width.
#' @param border_radius Border radius.
#' @param trigger_event Whether legend emits events.
#' @param data Legend data items. Character vector or list.
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
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) return(NULL)
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
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L &&
            value %in% c("single", "multiple")) {
          return(NULL)
        }
        "must be TRUE/FALSE, 'single', 'multiple', or NULL"
      }
    ),
    selected = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && !is.null(names(value))) return(NULL)
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
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) %in% c(1L, 4L)) return(NULL)
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
#' @param show Whether to show the tooltip.
#' @param trigger Trigger type: `"item"`, `"axis"`, or `"none"`.
#' @param trigger_on Trigger event: `"mousemove"`, `"click"`, `"none"`,
#'   `"mousemove|click"`.
#' @param show_content Whether to show the tooltip content.
#' @param always_show_content Whether to keep the tooltip visible.
#' @param formatter Tooltip content formatter. String template or function.
#' @param value_formatter A function to format the value portion.
#' @param position Tooltip position. String, numeric vector, or NULL.
#' @param confine Whether to confine tooltip within the chart container.
#' @param enterable Whether the mouse can enter the tooltip.
#' @param show_delay Delay (ms) before showing.
#' @param hide_delay Delay (ms) before hiding.
#' @param transition_duration Transition animation duration (s).
#' @param background_color Background color.
#' @param border_color Border color.
#' @param border_width Border width.
#' @param border_radius Border radius.
#' @param padding Padding inside tooltip.
#' @param text_style A [TextStyle] for tooltip text.
#' @param extra_css_text Extra CSS for the tooltip (HTML render mode only).
#' @param order Tooltip content order mode.
#' @param class_name CSS class name for tooltip element.
#' @export
Tooltip <- S7::new_class(
  "Tooltip",
  properties = list(
    # CommonTooltipOption
    show = bool_or_null_property(),
    trigger = enum_property(c("item", "axis", "none")),
    trigger_on = enum_property(c("mousemove", "click", "none", "mousemove|click")),
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
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) %in% c(1L, 2L, 4L)) return(NULL)
        "must be a number, or length-2/4 numeric vector, or NULL"
      }
    ),
    text_style = class_or_null_property(TextStyle),
    extra_css_text = string_or_null_property(),
    # TooltipOption-specific
    order = enum_property(c("seriesAsc", "seriesDesc", "valueAsc", "valueDesc")),
    class_name = string_or_null_property()
  )
)

S7::method(to_list, Tooltip) <- function(x, ...) {
  # class_name -> className (handled by snake_to_camel)
  # but we need the rename for class_name since className is the correct echarts key
  props_to_list(x)
}
