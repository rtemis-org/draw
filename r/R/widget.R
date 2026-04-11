# widget.R
# htmlwidget binding and Tier 1 draw_* functions

#' Render an ECharts option as an htmlwidget
#'
#' Low-level function that takes an [EChartsOption] (or a plain list) and
#' renders it as an interactive htmlwidget.
#'
#' @param option An [EChartsOption] object or a named list.
#' @param theme A [Theme] object, a plain list, `NULL` (default) for
#'   auto-detection of light/dark mode, or `NA` for no theme (raw ECharts
#'   defaults). When `NULL`, the widget detects dark mode from VS Code,
#'   RStudio, or the browser's `prefers-color-scheme` and applies
#'   [theme_light()] or [theme_dark()] accordingly.
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

  auto_theme <- FALSE
  theme_list <- NULL
  theme_dark_list <- NULL

  if (identical(theme, NA)) {
    # NA = no theme (raw ECharts defaults)
    theme_list <- NULL
  } else if (is.null(theme)) {
    # NULL = auto-detect: send both themes, let JS pick based on dark mode
    auto_theme <- TRUE
    theme_list <- to_list(theme_light())
    theme_dark_list <- to_list(theme_dark())
  } else if (S7::S7_inherits(theme, Theme)) {
    theme_list <- to_list(theme)
  } else if (is.list(theme)) {
    theme_list <- theme
  }

  payload <- list(
    option = option,
    theme = theme_list,
    renderer = renderer,
    autoTheme = if (auto_theme) TRUE else NULL,
    themeDark = theme_dark_list
  )

  htmlwidgets::createWidget(
    name = "draw",
    x = payload,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      browser.padding = 0,
      viewer.padding = 0,
      fill = TRUE
    ),
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

  # Format series data: value axes need [x, y] pairs; category axes need y only
  pair_xy <- function(y_vals) {
    if (x_type == "value") {
      mapply(c, x, y_vals, SIMPLIFY = FALSE)
    } else {
      y_vals
    }
  }

  # Build series
  if (is.list(y) && !is.null(names(y))) {
    series_names <- names(y)
    series <- lapply(seq_along(y), function(i) {
      LineSeries(
        name = series_names[i],
        data = pair_xy(y[[i]]),
        smooth = smooth,
        area_style = if (area) AreaStyle() else NULL
      )
    })
  } else if (is.list(y)) {
    series_names <- names %||% paste0("Series ", seq_along(y))
    series <- lapply(seq_along(y), function(i) {
      LineSeries(
        name = series_names[i],
        data = pair_xy(y[[i]]),
        smooth = smooth,
        area_style = if (area) AreaStyle() else NULL
      )
    })
  } else {
    series <- list(LineSeries(
      data = pair_xy(y),
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
#' Quick scatter plot from x/y data with optional fitted line and
#' confidence band.
#'
#' @param x Numeric x values.
#' @param y Numeric y values.
#' @param size Optional numeric vector for symbol sizes.
#' @param group Optional grouping factor for multiple series.
#' @param fit Fit method: `NULL` (no fit), `"glm"` for [stats::glm()], or
#'   `"gam"` for [mgcv::gam()]. The fitted line and 95\% confidence band
#'   are computed per group when `group` is provided.
#' @param se Whether to show the confidence band around the fit line.
#' @param fit_alpha Alpha (opacity) for the confidence band fill.
#' @param n_fit Number of equally spaced points at which to evaluate the fit.
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
  fit = NULL,
  se = TRUE,
  fit_alpha = 0.25,
  n_fit = 200,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  if (!is.null(fit)) {
    fit <- match.arg(fit, c("glm", "gam"))
  }

  # Helper: compute fit line and CI band for one group
  compute_fit <- function(xv, yv, fit_method, n_pts) {
    df <- data.frame(.x = xv, .y = yv)
    if (fit_method == "gam") {
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("Package 'mgcv' is required for fit = \"gam\"", call. = FALSE)
      }
      model <- mgcv::gam(.y ~ s(.x), data = df)
    } else {
      model <- stats::glm(.y ~ .x, data = df)
    }
    newdata <- data.frame(.x = seq(min(xv), max(xv), length.out = n_pts))
    pred <- stats::predict(model, newdata = newdata, se.fit = TRUE)
    list(
      x = newdata$.x,
      fitted = pred$fit,
      lower = pred$fit - 1.96 * pred$se.fit,
      upper = pred$fit + 1.96 * pred$se.fit
    )
  }

  # Helper: build fit + CI series for one group
  fit_series <- function(xv, yv, fit_method, n_pts, group_name, color) {
    p <- compute_fit(xv, yv, fit_method, n_pts)
    fit_data <- mapply(c, p$x, p$fitted, SIMPLIFY = FALSE)
    fit_name <- if (!is.null(group_name)) {
      paste0(group_name, " fit")
    } else {
      "fit"
    }

    out <- list()

    if (se) {
      # CI band as a closed polygon: upper bound L->R, lower bound R->L.
      # areaStyle fills the enclosed region. No stacking needed.
      upper <- mapply(c, p$x, p$upper, SIMPLIFY = FALSE)
      lower <- mapply(c, rev(p$x), rev(p$lower), SIMPLIFY = FALSE)
      ci_data <- c(upper, lower)
      out$ci <- LineSeries(
        name = paste0(fit_name, " CI"),
        data = ci_data,
        show_symbol = FALSE,
        line_style = LineStyle(opacity = 0),
        area_style = AreaStyle(color = color, opacity = fit_alpha),
        silent = TRUE,
        z = 0L,
        legend_hover_link = FALSE
      )
    }

    # Fit line (solid, on top)
    out$fit <- LineSeries(
      name = fit_name,
      data = fit_data,
      show_symbol = FALSE,
      line_style = LineStyle(color = color)
    )

    out
  }

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

  # Add fit series
  if (!is.null(fit)) {
    if (!is.null(group)) {
      groups <- unique(group)
      for (i in seq_along(groups)) {
        g <- groups[i]
        idx <- group == g
        color <- rtemis_colors[((i - 1L) %% length(rtemis_colors)) + 1L]
        fs <- fit_series(x[idx], y[idx], fit, n_fit, as.character(g), color)
        series <- c(series, unname(fs))
      }
    } else {
      color <- rtemis_colors[[1]]
      fs <- fit_series(x, y, fit, n_fit, NULL, color)
      series <- c(series, unname(fs))
    }
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

#' Draw a Density Plot
#'
#' Kernel density estimation plot from numeric data, with optional grouping
#' for multiple traces.
#'
#' @param x Numeric vector of values.
#' @param group Optional grouping factor for multiple density traces.
#' @param n Number of equally spaced points for density estimation.
#' @param bw Bandwidth for density estimation. Passed to [stats::density()].
#' @param na.rm Logical: if `TRUE` (default), remove `NA` values before
#'   computing densities.
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @param verbosity Integer: if > 0, print messages about removed `NA` values.
#' @return An htmlwidget.
#' @export
draw_density <- function(
  x,
  group = NULL,
  n = 512,
  bw = "nrd0",
  na.rm = TRUE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  verbosity = 1L
) {
  if (na.rm) {
    na_idx <- is.na(x)
    n_na <- sum(na_idx)
    if (n_na > 0L) {
      msg("Removed", n_na, "NA", ngettext(n_na, "value", "values"),
          "from x", verbosity = verbosity)
      if (!is.null(group)) group <- group[!na_idx]
      x <- x[!na_idx]
    }
  }

  if (!is.null(group)) {
    groups <- unique(group)
    series <- lapply(groups, function(g) {
      d <- stats::density(x[group == g], n = n, bw = bw)
      dat <- mapply(c, d$x, d$y, SIMPLIFY = FALSE)
      LineSeries(
        name = as.character(g),
        data = dat,
        show_symbol = FALSE,
        area_style = AreaStyle(opacity = 0.25)
      )
    })
  } else {
    d <- stats::density(x, n = n, bw = bw)
    dat <- mapply(c, d$x, d$y, SIMPLIFY = FALSE)
    series <- list(LineSeries(
      data = dat,
      show_symbol = FALSE,
      area_style = AreaStyle(opacity = 0.25)
    ))
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height)
}

#' Draw a Histogram
#'
#' Histogram from numeric data, with optional grouping for multiple traces.
#' Bins are computed using [graphics::hist()] with consistent break points
#' across groups.
#'
#' @param x Numeric vector of values.
#' @param group Optional grouping factor for multiple series.
#' @param breaks Binning method. A single number (number of bins), a character
#'   string naming an algorithm (e.g. `"Sturges"`, `"Scott"`, `"FD"`), or a
#'   numeric vector of break points. Passed to [graphics::hist()].
#' @param title Chart title string.
#' @param theme A [Theme] object or NULL.
#' @param width,height Widget dimensions.
#' @return An htmlwidget.
#' @export
draw_histogram <- function(
  x,
  group = NULL,
  breaks = "Sturges",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL
) {
  # Compute bin structure from full data for consistent breaks across groups
  h <- graphics::hist(x, breaks = breaks, plot = FALSE)
  bin_labels <- formatC(h$mids, format = "g")

  if (!is.null(group)) {
    groups <- unique(group)
    series <- lapply(groups, function(g) {
      hg <- graphics::hist(x[group == g], breaks = h$breaks, plot = FALSE)
      BarSeries(name = as.character(g), data = hg$counts)
    })
  } else {
    series <- list(BarSeries(
      data = h$counts,
      bar_category_gap = "0%"
    ))
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(type = "category", data = bin_labels),
    y_axis = Axis(type = "value"),
    series = series
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
