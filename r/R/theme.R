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
#' @param color Optional Character: Color palette.
#' @param background_color Optional Character: Chart background color.
#' @param text_style Optional [TextStyle]: Global default text style.
#' @param title Optional Named list: Title overrides.
#' @param legend Optional Named list: Legend overrides.
#' @param tooltip Optional Named list: Tooltip overrides.
#' @param line Optional Named list: Line-series defaults.
#' @param bar Optional Named list: Bar-series defaults.
#' @param pie Optional Named list: Pie-series defaults.
#' @param scatter Optional Named list: Scatter-series defaults.
#' @param category_axis Optional Named list: Category-axis defaults.
#' @param value_axis Optional Named list: Value-axis defaults.
#' @param log_axis Optional Named list: Log-axis defaults.
#' @param time_axis Optional Named list: Time-axis defaults.
#' @export
Theme <- S7::new_class(
  "Theme",
  properties = list(
    color = color_palette_property(),
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

  if (!is.null(x@color)) {
    out$color <- x@color
  }
  if (!is.null(x@background_color)) {
    out$backgroundColor <- x@background_color
  }
  if (!is.null(x@text_style)) {
    out$textStyle <- to_list(x@text_style)
  }

  # Plain list properties with their JSON keys
  overrides <- list(
    title = "title",
    legend = "legend",
    tooltip = "tooltip",
    line = "line",
    bar = "bar",
    pie = "pie",
    scatter = "scatter",
    category_axis = "categoryAxis",
    value_axis = "valueAxis",
    log_axis = "logAxis",
    time_axis = "timeAxis"
  )
  for (prop_name in names(overrides)) {
    val <- S7::prop(x, prop_name)
    if (!is.null(val)) {
      out[[overrides[[prop_name]]]] <- val
    }
  }

  # Propagate global textStyle to component defaults.
  # ECharts component built-in defaults (e.g. title fontSize: 18) override the
  # theme's global textStyle, so we merge the global textStyle into each
  # component's textStyle as a fallback to ensure properties like fontSize
  # actually take effect.
  if (!is.null(out$textStyle)) {
    ts <- out$textStyle
    for (comp in c("title", "legend", "tooltip")) {
      if (is.null(out[[comp]])) {
        out[[comp]] <- list()
      }
      if (is.null(out[[comp]]$textStyle)) {
        out[[comp]]$textStyle <- list()
      }
      for (key in names(ts)) {
        if (is.null(out[[comp]]$textStyle[[key]])) {
          out[[comp]]$textStyle[[key]] <- ts[[key]]
        }
      }
    }
    # Subtitle inherits from global textStyle too
    if (is.null(out$title$subtextStyle)) {
      out$title$subtextStyle <- list()
    }
    for (key in names(ts)) {
      if (is.null(out$title$subtextStyle[[key]])) {
        out$title$subtextStyle[[key]] <- ts[[key]]
      }
    }
  }

  out
}

# -- Built-in theme constructors ------------------------------------------------

#' Build a Theme from high-level parameters
#'
#' Internal helper shared by [theme_light()] and [theme_dark()].
#' Resolves font sizes from `base_font_size`, derives `legend_color`
#' and `tooltip_color` from `fg_color` when not set, and assembles
#' the full [Theme] object.
#'
#' @keywords internal
#' @noRd
build_theme <- function(
  base_font_size,
  title_font_size,
  subtitle_font_size,
  axis_label_font_size,
  legend_font_size,
  tooltip_font_size,
  font_family,
  color,
  bg_color,
  fg_color,
  title_color,
  subtitle_color,
  legend_color,
  axis_color,
  grid_color,
  tooltip_bg,
  tooltip_border_color,
  tooltip_color
) {
  # Resolve font sizes from base
  title_font_size <- title_font_size %||% round(base_font_size * 1.2)
  subtitle_font_size <- subtitle_font_size %||% base_font_size
  axis_label_font_size <- axis_label_font_size %||% base_font_size
  legend_font_size <- legend_font_size %||% base_font_size
  tooltip_font_size <- tooltip_font_size %||% base_font_size

  # Derive component text colors from fg_color when not set
  legend_color <- legend_color %||% fg_color
  tooltip_color <- tooltip_color %||% fg_color

  # Tooltip config
  tooltip_cfg <- drop_nulls(list(
    backgroundColor = tooltip_bg,
    borderColor = tooltip_border_color,
    textStyle = drop_nulls(list(
      color = tooltip_color,
      fontSize = tooltip_font_size
    ))
  ))

  # Axis label style (fontSize + color for tick labels)
  axis_label_style <- drop_nulls(list(
    fontSize = axis_label_font_size,
    color = fg_color
  ))

  # Axis configs
  value_axis_cfg <- drop_nulls(list(
    axisLabel = if (length(axis_label_style) > 0L) axis_label_style,
    axisLine = if (!is.null(axis_color)) {
      list(lineStyle = list(color = axis_color))
    },
    splitLine = if (!is.null(grid_color)) {
      list(lineStyle = list(color = grid_color))
    }
  ))

  category_axis_cfg <- drop_nulls(list(
    axisLabel = if (length(axis_label_style) > 0L) axis_label_style,
    axisLine = if (!is.null(axis_color)) {
      list(lineStyle = list(color = axis_color))
    },
    splitLine = list(show = FALSE)
  ))

  Theme(
    color = color,
    background_color = bg_color,
    text_style = TextStyle(
      color = fg_color,
      font_family = font_family,
      font_size = base_font_size
    ),
    title = list(
      textStyle = drop_nulls(list(
        color = title_color,
        fontSize = title_font_size
      )),
      subtextStyle = drop_nulls(list(
        color = subtitle_color,
        fontSize = subtitle_font_size
      ))
    ),
    legend = list(
      textStyle = drop_nulls(list(
        color = legend_color,
        fontSize = legend_font_size
      ))
    ),
    tooltip = if (length(tooltip_cfg) > 0L) tooltip_cfg else NULL,
    value_axis = if (length(value_axis_cfg) > 0L) value_axis_cfg else NULL,
    category_axis = if (length(category_axis_cfg) > 0L) {
      category_axis_cfg
    } else {
      NULL
    },
    line = list(symbol = "circle")
  )
}

#' Light Theme
#'
#' Returns a light theme. All parameters have sensible defaults; most
#' users only need `base_font_size`. Title font size defaults to 1.2x
#' the base; all other text defaults to 1x.
#'
#' @param base_font_size Numeric `[0, Inf)`: Base font size in pixels for all text.
#' @param title_font_size Optional Numeric `[0, Inf)`: Title font size.
#' @param subtitle_font_size Optional Numeric `[0, Inf)`: Subtitle font size.
#' @param axis_label_font_size Optional Numeric `[0, Inf)`: Axis-label font size.
#' @param legend_font_size Optional Numeric `[0, Inf)`: Legend font size.
#' @param tooltip_font_size Optional Numeric `[0, Inf)`: Tooltip font size.
#' @param font_family Character: Font family string.
#' @param color Character: Color palette.
#' @param bg_color Optional Character: Chart background color.
#' @param fg_color Optional Character: Global text color.
#' @param title_color Optional Character: Title text color.
#' @param subtitle_color Optional Character: Subtitle text color.
#' @param legend_color Optional Character: Legend text color.
#' @param axis_color Optional Character: Axis line color.
#' @param grid_color Optional Character: Grid or split-line color.
#' @param tooltip_bg Optional Character: Tooltip background color.
#' @param tooltip_border_color Optional Character: Tooltip border color.
#' @param tooltip_color Optional Character: Tooltip text color.
#' @return [Theme]: Theme object.
#' @export
theme_light <- function(
  base_font_size = 12,
  title_font_size = NULL,
  subtitle_font_size = NULL,
  axis_label_font_size = NULL,
  legend_font_size = NULL,
  tooltip_font_size = NULL,
  font_family = "sans-serif",
  color = rtemis_colors,
  bg_color = "#ffffff",
  fg_color = "rgba(0, 0, 0, 0.7)",
  title_color = NULL,
  subtitle_color = NULL,
  legend_color = NULL,
  axis_color = NULL,
  grid_color = NULL,
  tooltip_bg = NULL,
  tooltip_border_color = NULL,
  tooltip_color = NULL
) {
  build_theme(
    base_font_size = base_font_size,
    title_font_size = title_font_size,
    subtitle_font_size = subtitle_font_size,
    axis_label_font_size = axis_label_font_size,
    legend_font_size = legend_font_size,
    tooltip_font_size = tooltip_font_size,
    font_family = font_family,
    color = color,
    bg_color = bg_color,
    fg_color = fg_color,
    title_color = title_color,
    subtitle_color = subtitle_color,
    legend_color = legend_color,
    axis_color = axis_color,
    grid_color = grid_color,
    tooltip_bg = tooltip_bg,
    tooltip_border_color = tooltip_border_color,
    tooltip_color = tooltip_color
  )
}

#' Dark Theme
#'
#' Returns a dark theme. All parameters have sensible defaults; most
#' users only need `base_font_size`. Title font size defaults to 1.2x
#' the base; all other text defaults to 1x.
#'
#' @inheritParams theme_light
#' @return [Theme]: Theme object.
#' @export
theme_dark <- function(
  base_font_size = 12,
  title_font_size = NULL,
  subtitle_font_size = NULL,
  axis_label_font_size = NULL,
  legend_font_size = NULL,
  tooltip_font_size = NULL,
  font_family = "sans-serif",
  color = rtemis_colors,
  bg_color = "#181818",
  fg_color = "rgba(255, 255, 255, 0.7)",
  title_color = "rgba(255, 255, 255, 0.9)",
  subtitle_color = "rgba(255, 255, 255, 0.5)",
  legend_color = NULL,
  axis_color = "rgba(255, 255, 255, 0.3)",
  grid_color = "rgba(255, 255, 255, 0.1)",
  tooltip_bg = "rgba(20, 20, 20, 0.9)",
  tooltip_border_color = "rgba(255, 255, 255, 0.1)",
  tooltip_color = NULL
) {
  build_theme(
    base_font_size = base_font_size,
    title_font_size = title_font_size,
    subtitle_font_size = subtitle_font_size,
    axis_label_font_size = axis_label_font_size,
    legend_font_size = legend_font_size,
    tooltip_font_size = tooltip_font_size,
    font_family = font_family,
    color = color,
    bg_color = bg_color,
    fg_color = fg_color,
    title_color = title_color,
    subtitle_color = subtitle_color,
    legend_color = legend_color,
    axis_color = axis_color,
    grid_color = grid_color,
    tooltip_bg = tooltip_bg,
    tooltip_border_color = tooltip_border_color,
    tooltip_color = tooltip_color
  )
}
