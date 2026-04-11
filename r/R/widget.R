# widget.R
# htmlwidget binding and Tier 1 draw_* functions

#' Render an ECharts option as an htmlwidget
#'
#' Low-level function that takes an [EChartsOption] (or a plain list) and
#' renders it as an interactive htmlwidget.
#'
#' @param option An [EChartsOption] object or a named list.
#' @param theme A [Theme] object, or NULL for the default theme.
#' @param renderer Rendering engine: `"canvas"` (default) or `"svg"`.
#' @param width Widget width (CSS units or NULL for auto).
#' @param height Widget height (CSS units or NULL for auto).
#' @param elementId Optional explicit element ID.
#' @return An htmlwidget object.
#' @export
draw <- function(
  option,
  theme = NULL,
  renderer = "canvas",
  width = NULL,
  height = NULL,
  elementId = NULL
) {
  # Convert S7 objects to plain lists
  if (S7::S7_inherits(option)) {
    option <- to_list(option)
  }
  theme_list <- NULL
  if (!is.null(theme) && S7::S7_inherits(theme)) {
    theme_list <- to_list(theme)
  } else if (is.list(theme)) {
    theme_list <- theme
  }

  payload <- list(
    option = option,
    theme = theme_list,
    renderer = renderer
  )

  htmlwidgets::createWidget(
    name = "draw",
    x = payload,
    width = width,
    height = height,
    package = "rtemis.draw",
    elementId = elementId
  )
}

#' Shiny output for draw widget
#' @param outputId Shiny output ID.
#' @param width CSS width.
#' @param height CSS height.
#' @export
drawOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "draw",
    width = width,
    height = height,
    package = "rtemis.draw"
  )
}

#' Shiny render function for draw widget
#' @param expr Expression that returns a draw widget.
#' @param env Environment.
#' @param quoted Whether expr is quoted.
#' @export
renderDraw <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, drawOutput, env, quoted = TRUE)
}

# -- Tier 1: draw_* convenience functions ---------------------------------------

#' Draw a Line Chart
#'
#' Quick line chart from x/y data.
#'
#' @param x X-axis values (categories or numeric).
#' @param y Numeric vector or named list of numeric vectors (for multiple series).
#' @param names Series names (if y is a list).
#' @param smooth Whether to smooth lines.
#' @param area Whether to show area fill.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_line <- function(
  x,
  y,
  names = NULL,
  smooth = FALSE,
  area = FALSE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  # Determine axis type
  x_type <- if (is.numeric(x)) "value" else "category"

  # Build series
  if (is.list(y) && !is.null(names(y))) {
    series_names <- names(y)
    series <- lapply(seq_along(y), function(i) {
      LineSeries(
        name = series_names[i],
        data = y[[i]],
        smooth = smooth,
        area_style = if (area) AreaStyle() else NULL
      )
    })
  } else if (is.list(y)) {
    series_names <- names %||% paste0("Series ", seq_along(y))
    series <- lapply(seq_along(y), function(i) {
      LineSeries(
        name = series_names[i],
        data = y[[i]],
        smooth = smooth,
        area_style = if (area) AreaStyle() else NULL
      )
    })
  } else {
    series <- list(LineSeries(
      data = y,
      smooth = smooth,
      area_style = if (area) AreaStyle() else NULL
    ))
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(type = x_type, data = if (x_type == "category") x else NULL),
    y_axis = Axis(type = "value"),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height)
}

#' Draw a Bar Chart
#'
#' Quick bar chart from x/y data.
#'
#' @param x Category labels.
#' @param y Numeric vector or named list of numeric vectors.
#' @param stack Whether to stack bars. If TRUE, all series share a stack group.
#' @param horizontal Whether to draw horizontal bars.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_bar <- function(
  x,
  y,
  stack = FALSE,
  horizontal = FALSE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  stack_group <- if (stack) "total" else NULL

  if (is.list(y) && !is.null(names(y))) {
    series_names <- names(y)
    series <- lapply(seq_along(y), function(i) {
      BarSeries(name = series_names[i], data = y[[i]], stack = stack_group)
    })
  } else {
    series <- list(BarSeries(data = y, stack = stack_group))
  }

  if (horizontal) {
    x_ax <- Axis(type = "value")
    y_ax <- Axis(type = "category", data = x)
  } else {
    x_ax <- Axis(type = "category", data = x)
    y_ax <- Axis(type = "value")
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = x_ax,
    y_axis = y_ax,
    series = series
  )

  draw(opt, theme = theme, width = width, height = height)
}

#' Draw a Scatter Plot
#'
#' Quick scatter plot from x/y data.
#'
#' @param x Numeric x values.
#' @param y Numeric y values.
#' @param size Optional numeric vector for symbol sizes.
#' @param group Optional grouping factor for multiple series.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_scatter <- function(
  x,
  y,
  size = NULL,
  group = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  if (!is.null(group)) {
    groups <- unique(group)
    series <- lapply(groups, function(g) {
      idx <- group == g
      dat <- mapply(c, x[idx], y[idx], SIMPLIFY = FALSE)
      ScatterSeries(
        name = as.character(g),
        data = dat,
        symbol_size = if (!is.null(size)) size[idx][1] else NULL
      )
    })
  } else {
    dat <- mapply(c, x, y, SIMPLIFY = FALSE)
    series <- list(ScatterSeries(
      data = dat,
      symbol_size = size
    ))
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "item"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height)
}

#' Draw a Pie Chart
#'
#' Quick pie chart from values and labels.
#'
#' @param values Numeric vector of values.
#' @param labels Character vector of labels.
#' @param radius Pie radius. Single value or c(inner, outer) for donut.
#' @param rose_type Nightingale chart type: NULL, `"radius"`, or `"area"`.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_pie <- function(
  values,
  labels,
  radius = "75%",
  rose_type = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  data_items <- mapply(
    function(v, n) list(value = v, name = n),
    values,
    labels,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title, left = "center") else NULL,
    tooltip = Tooltip(trigger = "item"),
    legend = Legend(orient = "vertical", left = "left"),
    series = PieSeries(
      data = data_items,
      radius = radius,
      rose_type = rose_type,
      avoid_label_overlap = TRUE
    )
  )

  draw(opt, theme = theme, width = width, height = height)
}

#' Draw a Boxplot
#'
#' Quick boxplot from a list of numeric vectors. Uses an opaque color for box
#' and whisker borders, and a semi-transparent version for the box fill.
#'
#' @param data A list of numeric vectors, one per box. Each should be
#'   `c(min, Q1, median, Q3, max)`.
#' @param labels Category labels for each box.
#' @param horizontal Whether to draw horizontal boxplots.
#' @param color Box color. Used at full opacity for borders and at `fill_alpha`
#'   opacity for the box fill. Defaults to the first rtemis color.
#' @param fill_alpha Alpha (opacity) for the box fill color, between 0 and 1.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_boxplot <- function(
  data,
  labels = NULL,
  horizontal = FALSE,
  color = rtemis_colors[[1]],
  fill_alpha = 0.25,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  if (horizontal) {
    x_ax <- Axis(type = "value")
    y_ax <- Axis(type = "category", data = labels)
    layout <- "horizontal"
  } else {
    x_ax <- Axis(type = "category", data = labels)
    y_ax <- Axis(type = "value")
    layout <- "vertical"
  }

  # Opaque border, semi-transparent fill
  fill_color <- color_with_alpha(color, fill_alpha)
  item_style <- ItemStyle(color = fill_color, border_color = color)

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "item"),
    x_axis = x_ax,
    y_axis = y_ax,
    series = BoxplotSeries(
      data = data,
      layout = layout,
      item_style = item_style
    )
  )

  draw(opt, theme = theme, width = width, height = height)
}
