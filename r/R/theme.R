# theme.R
# Theme S7 class for echarts theme configuration
#
# TS sources:
#   globalDefault.ts: src/model/globalDefault.ts (default theme values)
#   dark.ts:          src/theme/dark.ts (dark theme overrides)
#
# ECharts themes are plain objects that override the global defaults.
# The Theme class captures the most commonly used theme settings and
# serializes to a named list that can be passed to echarts.init() or
# merged into the option.

#' Theme
#'
#' A chart theme that controls default colors, text styles, and
#' component appearance. Corresponds to the object passed as the second
#' argument to `echarts.init(dom, theme)` or registered via
#' `echarts.registerTheme()`.
#'
#' TS source: `src/model/globalDefault.ts`, `src/theme/dark.ts`.
#'
#' @param color Color palette: character vector.
#' @param background_color Chart background color.
#' @param text_style A [TextStyle] for global default text.
#' @param title Named list of title overrides (e.g., `list(textStyle = list(...))`).
#' @param legend Named list of legend overrides.
#' @param tooltip Named list of tooltip overrides.
#' @param line Named list of line series defaults.
#' @param bar Named list of bar series defaults.
#' @param pie Named list of pie series defaults.
#' @param scatter Named list of scatter series defaults.
#' @param category_axis Named list of category axis defaults.
#' @param value_axis Named list of value axis defaults.
#' @param log_axis Named list of log axis defaults.
#' @param time_axis Named list of time axis defaults.
#' @export
Theme <- S7::new_class(
  "Theme",
  properties = list(
    color = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.character(value)) return(NULL)
        "must be a character vector of colors or NULL"
      }
    ),
    background_color = color_property(),
    text_style = class_or_null_property(TextStyle),
    # Component overrides (plain lists)
    title = S7::new_property(class = S7::class_any, default = NULL),
    legend = S7::new_property(class = S7::class_any, default = NULL),
    tooltip = S7::new_property(class = S7::class_any, default = NULL),
    # Series defaults (plain lists)
    line = S7::new_property(class = S7::class_any, default = NULL),
    bar = S7::new_property(class = S7::class_any, default = NULL),
    pie = S7::new_property(class = S7::class_any, default = NULL),
    scatter = S7::new_property(class = S7::class_any, default = NULL),
    # Axis defaults (plain lists)
    category_axis = S7::new_property(class = S7::class_any, default = NULL),
    value_axis = S7::new_property(class = S7::class_any, default = NULL),
    log_axis = S7::new_property(class = S7::class_any, default = NULL),
    time_axis = S7::new_property(class = S7::class_any, default = NULL)
  )
)

S7::method(to_list, Theme) <- function(x, ...) {
  out <- list()

  if (!is.null(x@color)) out$color <- x@color
  if (!is.null(x@background_color)) out$backgroundColor <- x@background_color
  if (!is.null(x@text_style)) out$textStyle <- to_list(x@text_style)

  # Plain list properties with their JSON keys
  overrides <- list(
    title = "title", legend = "legend", tooltip = "tooltip",
    line = "line", bar = "bar", pie = "pie", scatter = "scatter",
    category_axis = "categoryAxis", value_axis = "valueAxis",
    log_axis = "logAxis", time_axis = "timeAxis"
  )
  for (prop_name in names(overrides)) {
    val <- S7::prop(x, prop_name)
    if (!is.null(val)) {
      out[[overrides[[prop_name]]]] <- val
    }
  }

  out
}

# -- Built-in theme constructors ------------------------------------------------

#' Light Theme
#'
#' Returns the default light theme based on echarts defaults.
#'
#' @return A [Theme] object.
#' @export
light_theme <- function() {
  Theme(
    color = c(
      "#5470c6", "#91cc75", "#fac858", "#ee6666",
      "#73c0de", "#3ba272", "#fc8452", "#9a60b4", "#ea7ccc"
    ),
    text_style = TextStyle(
      font_family = "sans-serif",
      font_size = 12
    )
  )
}

#' Dark Theme
#'
#' Returns a dark theme based on the echarts built-in dark theme.
#'
#' @return A [Theme] object.
#' @export
dark_theme <- function() {
  Theme(
    color = c(
      "#4992ff", "#7cffb2", "#fddd60", "#ff6e76",
      "#58d9f9", "#05c091", "#ff8a45", "#8d48e3", "#dd79ff"
    ),
    background_color = "#100C2A",
    text_style = TextStyle(color = "rgba(255, 255, 255, 0.7)"),
    title = list(
      textStyle = list(color = "rgba(255, 255, 255, 0.9)"),
      subtextStyle = list(color = "rgba(255, 255, 255, 0.5)")
    ),
    legend = list(
      textStyle = list(color = "rgba(255, 255, 255, 0.7)")
    ),
    tooltip = list(
      backgroundColor = "rgba(20, 20, 40, 0.9)",
      borderColor = "rgba(255, 255, 255, 0.1)",
      textStyle = list(color = "rgba(255, 255, 255, 0.7)")
    ),
    value_axis = list(
      axisLine = list(lineStyle = list(color = "rgba(255, 255, 255, 0.3)")),
      splitLine = list(lineStyle = list(color = "rgba(255, 255, 255, 0.1)"))
    ),
    category_axis = list(
      axisLine = list(lineStyle = list(color = "rgba(255, 255, 255, 0.3)")),
      splitLine = list(show = FALSE)
    ),
    line = list(symbol = "circle")
  )
}
