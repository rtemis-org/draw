# option.R
# Top-level EChartsOption S7 class
#
# TS sources:
#   EChartsOption: src/export/option.ts (line 258)
#   ECUnitOption:  src/util/types.ts (line 697)
#   ECBasicOption: src/util/types.ts (line 756)

#' ECharts Option
#'
#' The top-level configuration object passed to `echarts.setOption()`.
#' Holds all chart components, series, and global settings.
#'
#' Corresponds to `EChartsOption` in `src/export/option.ts` (line 258)
#' and `ECUnitOption` in `src/util/types.ts` (line 697).
#'
#' @param title Optional [Title] or list: Title configuration.
#' @param legend Optional [Legend] or list: Legend configuration.
#' @param grid Optional [Grid] or list: Grid configuration.
#' @param x_axis Optional [Axis] or list: X-axis configuration (`xAxis`).
#' @param y_axis Optional [Axis] or list: Y-axis configuration (`yAxis`).
#' @param tooltip Optional [Tooltip]: Tooltip configuration.
#' @param visual_map Optional [VisualMap] or list: Visual map configuration (`visualMap`).
#'   Required for heatmaps to map data values to colors.
#' @param data_zoom Optional [DataZoom] or list of [DataZoom]: Axis zoom
#'   component(s) (`dataZoom`). Pass a list to combine, e.g., a `"slider"` with
#'   an `"inside"` zoom.
#' @param series Optional series object or list: Series configuration ([LineSeries],
#'   [BarSeries], [ScatterSeries], [PieSeries], [BoxplotSeries], [HeatmapSeries]).
#' @param color Optional Character: Color palette.
#' @param background_color Optional Character: Chart background color.
#' @param text_style Optional [TextStyle]: Global default text style.
#' @param animation Optional Logical: Whether to enable animation.
#' @param animation_threshold Optional Numeric `[0, Inf)`: Data-count threshold that disables animation.
#' @param animation_duration Optional Numeric `[0, Inf)`: Initial animation duration in milliseconds.
#' @param animation_easing Optional Character: Animation easing function name.
#' @param animation_delay Optional Numeric `[0, Inf)`: Initial animation delay in milliseconds.
#' @param dark_mode Optional Logical or Character \{"auto"\}: Dark-mode setting.
#' @param use_utc Optional Logical: Whether to use UTC for time axes.
#' @export
EChartsOption <- S7::new_class(
  "EChartsOption",
  properties = list(
    # Components (single or list)
    title = S7::new_property(class = S7::class_any, default = NULL),
    legend = S7::new_property(class = S7::class_any, default = NULL),
    grid = S7::new_property(class = S7::class_any, default = NULL),
    x_axis = S7::new_property(class = S7::class_any, default = NULL),
    y_axis = S7::new_property(class = S7::class_any, default = NULL),
    tooltip = class_or_null_property(Tooltip),
    visual_map = S7::new_property(class = S7::class_any, default = NULL),
    data_zoom = S7::new_property(class = S7::class_any, default = NULL),
    # Series (single or list)
    series = S7::new_property(class = S7::class_any, default = NULL),
    # Global settings
    color = color_palette_property(),
    background_color = color_property(),
    text_style = class_or_null_property(TextStyle),
    # Animation
    animation = bool_or_null_property(),
    animation_threshold = numeric_or_null_property(),
    animation_duration = numeric_or_null_property(),
    animation_easing = string_or_null_property(),
    animation_delay = numeric_or_null_property(),
    # Other
    dark_mode = S7::new_property(
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
    use_utc = bool_or_null_property()
  )
)

#' Convert a component property that may be a single S7 object or a list of them.
#'
#' @param value Optional S7 object or list: Component value to convert.
#' @return Optional list: Converted component value.
#' @keywords internal
#' @noRd
convert_component <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (S7::S7_inherits(value)) {
    return(to_list(value))
  }
  if (is.list(value)) {
    return(lapply(value, function(v) {
      if (S7::S7_inherits(v)) to_list(v) else v
    }))
  }
  value
}

S7::method(to_list, EChartsOption) <- function(x, ...) {
  out <- list()

  # Components with rename mapping (snake_case property -> camelCase JSON key)
  component_map <- list(
    title = "title",
    legend = "legend",
    grid = "grid",
    x_axis = "xAxis",
    y_axis = "yAxis",
    tooltip = "tooltip",
    visual_map = "visualMap"
  )

  for (prop_name in names(component_map)) {
    val <- S7::prop(x, prop_name)
    converted <- convert_component(val)
    if (!is.null(converted)) {
      out[[component_map[[prop_name]]]] <- converted
    }
  }

  # Series
  series_val <- x@series
  if (!is.null(series_val)) {
    if (S7::S7_inherits(series_val)) {
      out$series <- list(to_list(series_val))
    } else if (is.list(series_val)) {
      out$series <- lapply(series_val, function(s) {
        if (S7::S7_inherits(s)) to_list(s) else s
      })
    }
  }

  # dataZoom — always serialize as an array (matches echarts conventions)
  data_zoom_val <- x@data_zoom
  if (!is.null(data_zoom_val)) {
    if (S7::S7_inherits(data_zoom_val)) {
      out$dataZoom <- list(to_list(data_zoom_val))
    } else if (is.list(data_zoom_val)) {
      out$dataZoom <- lapply(data_zoom_val, function(dz) {
        if (S7::S7_inherits(dz)) to_list(dz) else dz
      })
    }
  }

  # Simple properties
  simple_props <- c(
    "color",
    "background_color",
    "animation",
    "animation_threshold",
    "animation_duration",
    "animation_easing",
    "animation_delay",
    "dark_mode",
    "use_utc"
  )
  for (prop_name in simple_props) {
    val <- S7::prop(x, prop_name)
    if (!is.null(val)) {
      json_name <- snake_to_camel(prop_name)
      out[[json_name]] <- val
    }
  }

  # text_style (S7 object)
  if (!is.null(x@text_style)) {
    out$textStyle <- to_list(x@text_style)
  }

  out
}

#' Convert EChartsOption to JSON
#'
#' @param x [EChartsOption]: Option object to serialize.
#' @param pretty Logical: Whether to pretty-print the JSON.
#' @param auto_unbox Logical: Whether to auto-unbox single-element vectors.
#' @param ... Dots: Additional arguments passed to [jsonlite::toJSON()].
#' @return Character: JSON string.
#' @export
to_json <- function(x, pretty = FALSE, auto_unbox = TRUE, ...) {
  jsonlite::toJSON(
    to_list(x),
    pretty = pretty,
    auto_unbox = auto_unbox,
    null = "null",
    ...
  )
}
