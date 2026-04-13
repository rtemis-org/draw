# widget.R
# htmlwidget binding and Tier 1 draw_* functions

#' Render an ECharts option as an htmlwidget
#'
#' Low-level function that takes an [EChartsOption] (or a plain list) and
#' renders it as an interactive htmlwidget.
#'
#' @param option [EChartsOption] or named list: Option object to render.
#' @param theme Optional [Theme], list, or `NA`: Theme override. `NULL` enables
#'   auto-detection of light/dark mode, or `NA` for no theme (raw ECharts
#'   defaults). When `NULL`, the widget detects dark mode from VS Code,
#'   RStudio, or the browser's `prefers-color-scheme` and applies
#'   [theme_light()] or [theme_dark()] accordingly.
#' @param renderer Character \{"canvas", "svg"\}: Rendering engine.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param elementId Optional Character: Explicit element ID.
#' @param filename Optional Character: If provided, the widget is also written to
#'   this file via [save_drawing()]. Extension determines the format (currently
#'   only `.svg` is supported).
#' @return htmlwidget: Widget object.
#' @export
draw <- function(
  option,
  theme = NULL,
  renderer = "canvas",
  width = NULL,
  height = NULL,
  elementId = NULL,
  filename = NULL
) {
  # Convert S7 objects to plain lists
  if (S7::S7_inherits(option)) {
    option <- to_list(option)
  }

  # Auto-add legend when multiple distinctly named series are present
  if (is.null(option$legend) &&
      is.list(option$series) && length(option$series) > 1L) {
    series_names <- vapply(
      option$series, function(s) s$name %||% "", character(1)
    )
    if (length(unique(series_names[series_names != ""])) > 1L) {
      option$legend <- list()
    }
  }

  # Ensure empty S7-derived lists serialize as JSON objects {} not arrays [].
  # props_to_list() returns list() (unnamed) for objects with all-NULL
  # properties (e.g. Legend(), AreaStyle()). jsonlite serializes unnamed
  # list() as [] but named list() as {}. ECharts requires {}.
  fix_empty_objects <- function(x) {
    if (is.list(x)) {
      if (length(x) == 0L && is.null(names(x))) {
        return(stats::setNames(list(), character(0)))
      }
      x[] <- lapply(x, fix_empty_objects)
    }
    x
  }
  option <- fix_empty_objects(option)

  # Hide default axis tick marks for a clean look
  for (axis_key in c("xAxis", "yAxis")) {
    ax <- option[[axis_key]]
    if (!is.null(ax)) {
      if (is.null(names(ax))) {
        # Array of axis configs (e.g. dual y-axes)
        for (i in seq_along(ax)) {
          if (is.null(ax[[i]]$axisTick)) ax[[i]]$axisTick <- list()
          ax[[i]]$axisTick$show <- FALSE
        }
      } else {
        # Single axis config
        if (is.null(ax$axisTick)) ax$axisTick <- list()
        ax$axisTick$show <- FALSE
      }
      option[[axis_key]] <- ax
    }
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

  widget <- htmlwidgets::createWidget(
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

  if (!is.null(filename)) {
    save_drawing(widget, filename)
  }

  widget
}

#' Shiny output for draw widget
#' @param outputId Character: Shiny output ID.
#' @param width Character or Numeric: CSS width.
#' @param height Character or Numeric: CSS height.
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
#' @param expr Expression: Expression that returns a draw widget.
#' @param env Environment: Evaluation environment.
#' @param quoted Logical: Whether `expr` is quoted.
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
#' @param x Vector: X-axis values.
#' @param y Numeric or named list: Y values.
#' @param names Optional Character: Series names used when `y` is an unnamed list.
#' @param smooth Logical: Whether to smooth lines.
#' @param area Logical: Whether to show area fill.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
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
  height = NULL,
  filename = NULL
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
    x_axis = Axis(
      type = x_type,
      data = if (x_type == "category") x else NULL,
      scale = if (x_type == "value") TRUE else NULL
    ),
    y_axis = Axis(type = "value", scale = TRUE),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Bar Chart
#'
#' Quick bar chart from x/y data.
#'
#' @param x Character: Category labels.
#' @param y Numeric or named list: Bar heights.
#' @param stack Logical: Whether to stack bars.
#' @param horizontal Logical: Whether to draw horizontal bars.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_bar <- function(
  x,
  y,
  stack = FALSE,
  horizontal = FALSE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
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

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Scatter Plot
#'
#' Quick scatter plot from x/y data with optional fitted line and
#' confidence band.
#'
#' @param x Numeric: X values.
#' @param y Numeric: Y values.
#' @param size Optional Numeric: Symbol sizes.
#' @param group Optional Vector: Grouping variable for multiple series.
#' @param fit Optional Character \{"glm", "gam"\}: Fit method. `NULL` disables fitting.
#'   `"gam"` for [mgcv::gam()]. The fitted line and 95\% confidence band
#'   are computed per group when `group` is provided.
#' @param se Logical: Whether to show the confidence band.
#' @param fit_alpha Numeric `[0, 1]`: Opacity for the confidence-band fill.
#' @param n_fit Numeric `[1, Inf)`: Number of evaluation points for the fit.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
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
  height = NULL,
  filename = NULL
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

  # Helper: build fit + CI series for one group.
  # Uses the scatter group's name so ECharts groups them together in the
  # legend — clicking a group toggles scatter + fit + CI as a unit.
  fit_series <- function(xv, yv, fit_method, n_pts, group_name, color) {
    p <- compute_fit(xv, yv, fit_method, n_pts)
    fit_data <- mapply(c, p$x, p$fitted, SIMPLIFY = FALSE)

    out <- list()

    if (se) {
      # CI band as a closed polygon: upper bound L->R, lower bound R->L.
      # areaStyle fills the enclosed region. No stacking needed.
      upper <- mapply(c, p$x, p$upper, SIMPLIFY = FALSE)
      lower <- mapply(c, rev(p$x), rev(p$lower), SIMPLIFY = FALSE)
      ci_data <- c(upper, lower)
      out$ci <- LineSeries(
        name = group_name,
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
      name = group_name,
      data = fit_data,
      show_symbol = FALSE,
      line_style = LineStyle(color = color),
      silent = TRUE
    )

    out
  }

  if (!is.null(group)) {
    groups <- unique(group)
    # Assign explicit colors so scatter and fit/CI match
    group_colors <- rtemis_colors[
      ((seq_along(groups) - 1L) %% length(rtemis_colors)) + 1L
    ]
    series <- lapply(seq_along(groups), function(i) {
      g <- groups[i]
      idx <- group == g
      dat <- mapply(c, x[idx], y[idx], SIMPLIFY = FALSE)
      ScatterSeries(
        name = as.character(g),
        data = dat,
        symbol_size = if (!is.null(size)) size[idx][1] else NULL,
        item_style = ItemStyle(color = group_colors[i])
      )
    })
  } else {
    dat <- mapply(c, x, y, SIMPLIFY = FALSE)
    series <- list(ScatterSeries(
      data = dat,
      symbol_size = size
    ))
  }

  # Add fit series (share the scatter group name for legend grouping)
  if (!is.null(fit)) {
    if (!is.null(group)) {
      for (i in seq_along(groups)) {
        g <- groups[i]
        idx <- group == g
        fs <- fit_series(
          x[idx], y[idx], fit, n_fit, as.character(g), group_colors[i]
        )
        series <- c(series, unname(fs))
      }
    } else {
      color <- rtemis_colors[[1]]
      fs <- fit_series(x, y, fit, n_fit, NULL, color)
      series <- c(series, unname(fs))
    }
  }

  # Tooltip formatter showing (x, y) with ddSci-style number formatting.
  # Show series name only when there are multiple scatter groups.
  show_name <- !is.null(group)
  scatter_formatter <- htmlwidgets::JS(sprintf(
    "function(p) {
      function ddSci(x, dp) {
        dp = dp || 2;
        var a = Math.abs(x);
        if (a === 0) return '0.' + '0'.repeat(dp);
        if (a >= 1e6 || a < Math.pow(10, -dp)) return x.toExponential(1);
        return x.toFixed(dp);
      }
      var m = p.marker || '';
      var n = %s ? (p.seriesName + '<br/>') : '';
      return m + n + ddSci(p.value[0]) + ', ' + ddSci(p.value[1]);
    }",
    if (show_name) "true" else "false"
  ))

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "item", formatter = scatter_formatter),
    legend = if (!is.null(group)) Legend() else NULL,
    x_axis = Axis(type = "value", scale = TRUE),
    y_axis = Axis(type = "value", scale = TRUE),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Pie Chart
#'
#' Quick pie chart from values and labels.
#'
#' @param values Numeric: Slice values.
#' @param labels Character: Slice labels.
#' @param radius Numeric or Character: Pie radius.
#' @param rose_type Optional Character \{"radius", "area"\}: Nightingale chart type.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_pie <- function(
  values,
  labels,
  radius = "75%",
  rose_type = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
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

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Density Plot
#'
#' Kernel density estimation plot from numeric data, with optional grouping
#' for multiple traces. A list input creates one density trace per vector when
#' ungrouped, or one trace per variable/group combination when `group` is
#' supplied.
#'
#' @param x Numeric or list: Values used for density estimation. An ungrouped
#'   list creates one density trace per element; with `group`, each list
#'   element is split by group into separate traces.
#' @param group Optional Vector: Grouping variable for multiple density traces.
#' @param n Numeric `[1, Inf)`: Number of equally spaced points for density estimation.
#' @param bw Character or Numeric: Bandwidth passed to [stats::density()].
#' @param na.rm Logical: Whether to remove `NA` values before
#'   computing densities.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param verbosity Integer `[0, Inf)`: Verbosity level for removed-`NA` messages.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
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
  verbosity = 1L,
  filename = NULL
) {
  if (is.list(x)) {
    series_names <- names(x)
    if (is.null(series_names) || !all(nzchar(series_names))) {
      series_names <- paste0("Series ", seq_along(x))
    }

    if (!is.null(group)) {
      lens <- vapply(x, length, integer(1))
      if (any(lens != length(group))) {
        stop(
          "All elements of `x` must match length(group) when `group` is provided.",
          call. = FALSE
        )
      }

      group_ok <- !is.na(group)
      if (any(!group_ok)) {
        group <- group[group_ok]
        x <- lapply(x, function(vals) vals[group_ok])
      }

      groups <- unique(group)
      group_labels <- as.character(groups)
      series <- unlist(lapply(seq_along(x), function(i) {
        vals <- x[[i]]
        group_i <- group

        if (na.rm) {
          na_idx <- is.na(vals)
          n_na <- sum(na_idx)
          if (n_na > 0L) {
            msg(
              "Removed",
              n_na,
              "NA",
              ngettext(n_na, "value", "values"),
              "from",
              series_names[[i]],
              verbosity = verbosity
            )
            vals <- vals[!na_idx]
            group_i <- group_i[!na_idx]
          }
        }

        lapply(seq_along(groups), function(j) {
          g <- groups[[j]]
          d <- stats::density(vals[group_i == g], n = n, bw = bw)
          dat <- mapply(c, d$x, d$y, SIMPLIFY = FALSE)
          LineSeries(
            name = paste(series_names[[i]], group_labels[[j]], sep = " - "),
            data = dat,
            show_symbol = FALSE,
            area_style = AreaStyle(opacity = 0.25)
          )
        })
      }), recursive = FALSE)
    } else {
      series <- lapply(seq_along(x), function(i) {
        vals <- x[[i]]

        if (na.rm) {
          na_idx <- is.na(vals)
          n_na <- sum(na_idx)
          if (n_na > 0L) {
            msg(
              "Removed",
              n_na,
              "NA",
              ngettext(n_na, "value", "values"),
              "from",
              series_names[[i]],
              verbosity = verbosity
            )
            vals <- vals[!na_idx]
          }
        }

        d <- stats::density(vals, n = n, bw = bw)
        dat <- mapply(c, d$x, d$y, SIMPLIFY = FALSE)
        LineSeries(
          name = series_names[[i]],
          data = dat,
          show_symbol = FALSE,
          area_style = AreaStyle(opacity = 0.25)
        )
      })
    }
  } else {
    if (na.rm) {
      na_idx <- is.na(x)
      n_na <- sum(na_idx)
      if (n_na > 0L) {
        msg(
          "Removed",
          n_na,
          "NA",
          ngettext(n_na, "value", "values"),
          "from x",
          verbosity = verbosity
        )
        if (!is.null(group)) {
          group <- group[!na_idx]
        }
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
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(type = "value", scale = TRUE),
    y_axis = Axis(type = "value"),
    series = series
  )

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Histogram
#'
#' Histogram from numeric data, with optional grouping for multiple traces.
#' Bins are computed using [graphics::hist()] with consistent break points
#' across groups.
#'
#' @param x Numeric: Values used for histogram binning.
#' @param group Optional Vector: Grouping variable for multiple series.
#' @param breaks Numeric, Character, or Numeric vector: Binning method. A single number (number of bins), a character
#'   string naming an algorithm (e.g. `"Sturges"`, `"Scott"`, `"FD"`), or a
#'   numeric vector of break points. Passed to [graphics::hist()].
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_histogram <- function(
  x,
  group = NULL,
  breaks = "Sturges",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
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

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Draw a Boxplot
#'
#' Quick boxplot from raw data with optional grouping for multiple traces.
#' Boxplot statistics (min, Q1, median, Q3, max) are computed automatically
#' using [grDevices::boxplot.stats()].
#'
#' @param data Numeric or list: A list of numeric vectors (one per box), or a single numeric
#'   vector when `group` is provided. For ungrouped named lists, `names(data)`
#'   are used as labels when `labels` is not supplied.
#' @param labels Optional Character: Category labels for each box. Ignored when `group` is
#'   provided (group levels are used instead). When omitted for ungrouped named
#'   lists, `names(data)` are used.
#' @param group Optional Vector: Grouping variable. When provided, `data` must be a
#'   numeric vector, and boxplot statistics are computed per group. Each group
#'   gets its own colored series.
#' @param horizontal Logical: Whether to draw horizontal boxplots.
#' @param color Optional Character: Box color or colors. For ungrouped boxplots, a single color used at
#'   full opacity for borders and at `fill_alpha` opacity for the fill.
#'   For grouped boxplots, defaults to `rtemis_colors`; recycled as needed.
#' @param fill_alpha Numeric `[0, 1]`: Opacity for the box fill color.
#' @param na.rm Logical: Whether to remove `NA` values before
#'   computing boxplot statistics.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param verbosity Integer `[0, Inf)`: Verbosity level for removed-`NA` messages.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_boxplot <- function(
  data,
  labels = NULL,
  group = NULL,
  horizontal = FALSE,
  color = NULL,
  fill_alpha = 0.25,
  na.rm = TRUE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  verbosity = 1L,
  filename = NULL
) {
  # Note: BoxplotSeries `layout` is auto-detected from the category axis
  # orientation, so we do not set it explicitly. (Our previous mapping
  # was inverted relative to ECharts conventions.)

  # Grouped + single-element (possibly named) list => unwrap to numeric.
  # Lets callers write draw_boxplot(list(`Body Mass` = x), group = g) and
  # have the list name label the value axis.
  value_axis_name <- NULL
  if (!is.null(group) && is.list(data) && length(data) == 1L) {
    if (!is.null(names(data)) && nzchar(names(data)[[1]])) {
      value_axis_name <- names(data)[[1]]
    }
    data <- data[[1]]
  }

  # Bare numeric vector without group => single box
  if (is.numeric(data) && is.null(group)) {
    if (is.null(labels)) {
      labels <- labelify(deparse(substitute(data)))
    }
    data <- list(data)
  }

  # Use names from an ungrouped named list as category labels unless
  # labels are supplied explicitly.
  if (is.null(group) &&
      is.list(data) &&
      is.null(labels) &&
      !is.null(names(data)) &&
      all(nzchar(names(data)))) {
    labels <- names(data)
    data <- unname(data)
  }

  # Ensure labels serialize as a JSON array, not a bare string
  if (!is.null(labels) && length(labels) == 1L) {
    labels <- as.list(labels)
  }

  if (!is.null(group) && is.list(data)) {
    # Multi-variable grouped: categories = variable names, one series per
    # group level. Each series has one box per variable; ECharts dodges
    # boxes within each variable's slot — exactly what we want here.
    var_labels <- names(data)
    if (is.null(var_labels) || !all(nzchar(var_labels))) {
      var_labels <- paste0("Var ", seq_along(data))
    }
    data <- unname(data)

    lens <- vapply(data, length, integer(1))
    if (any(lens != length(group))) {
      stop(
        "All elements of `data` must match length(group) when `group` is provided.",
        call. = FALSE
      )
    }

    # Exclude observations with missing group assignments so they do not
    # create an extra boxplot series and shift the visible groups off-center.
    group_ok <- !is.na(group)
    if (any(!group_ok)) {
      group <- group[group_ok]
      data <- lapply(data, function(v) v[group_ok])
    }

    groups <- unique(group)
    group_labels <- as.character(groups)
    colors <- color %||% rtemis_colors
    colors <- rep_len(colors, length(groups))

    # One series per group level. Each series has length(var_labels)
    # data points (one box per variable). NAs are dropped per (variable,
    # group) cell so different variables with different NA patterns are
    # handled correctly.
    series <- lapply(seq_along(groups), function(i) {
      g_idx <- group == groups[i]
      stats_per_var <- lapply(data, function(v) {
        vals <- v[g_idx]
        if (na.rm) vals <- vals[!is.na(vals)]
        grDevices::boxplot.stats(vals)$stats
      })
      col <- colors[i]
      fill <- color_with_alpha(col, fill_alpha)
      BoxplotSeries(
        name = group_labels[i],
        data = stats_per_var,
        item_style = ItemStyle(color = fill, border_color = col)
      )
    })

    if (horizontal) {
      x_ax <- Axis(type = "value", scale = TRUE)
      y_ax <- Axis(type = "category", data = var_labels)
    } else {
      x_ax <- Axis(type = "category", data = var_labels)
      y_ax <- Axis(type = "value", scale = TRUE)
    }

    opt <- EChartsOption(
      title = if (!is.null(title)) Title(text = title) else NULL,
      tooltip = Tooltip(trigger = "item"),
      legend = Legend(),
      x_axis = x_ax,
      y_axis = y_ax,
      series = series
    )
  } else if (!is.null(group)) {
    # Grouped: single numeric vector split by group factor
    group_ok <- !is.na(group)
    if (any(!group_ok)) {
      group <- group[group_ok]
      data <- data[group_ok]
    }

    if (na.rm) {
      na_idx <- is.na(data)
      n_na <- sum(na_idx)
      if (n_na > 0L) {
        msg(
          "Removed",
          n_na,
          "NA",
          ngettext(n_na, "value", "values"),
          "from data",
          verbosity = verbosity
        )
        group <- group[!na_idx]
        data <- data[!na_idx]
      }
    }

    groups <- unique(group)
    group_labels <- as.character(groups)
    colors <- color %||% rtemis_colors
    colors <- rep_len(colors, length(groups))

    # Single boxplot series with per-item colors. Using one series per
    # group would cause ECharts to dodge them like grouped bars, shifting
    # each box away from its category tick.
    box_items <- lapply(seq_along(groups), function(i) {
      vals <- data[group == groups[i]]
      bs <- grDevices::boxplot.stats(vals)
      col <- colors[i]
      fill <- color_with_alpha(col, fill_alpha)
      list(
        name = group_labels[i],
        value = bs$stats,
        itemStyle = list(color = fill, borderColor = col)
      )
    })

    series <- BoxplotSeries(data = box_items)

    if (horizontal) {
      x_ax <- Axis(type = "value", scale = TRUE, name = value_axis_name)
      y_ax <- Axis(type = "category", data = group_labels)
    } else {
      x_ax <- Axis(type = "category", data = group_labels)
      y_ax <- Axis(type = "value", scale = TRUE, name = value_axis_name)
    }

    opt <- EChartsOption(
      title = if (!is.null(title)) Title(text = title) else NULL,
      tooltip = Tooltip(trigger = "item"),
      x_axis = x_ax,
      y_axis = y_ax,
      series = series
    )
  } else {
    # Ungrouped: list of raw numeric vectors, one per box
    col <- color %||% rtemis_colors[[1]]
    fill <- color_with_alpha(col, fill_alpha)
    item_style <- ItemStyle(color = fill, border_color = col)

    # Compute boxplot stats from raw data, removing NAs per box
    box_data <- lapply(data, function(v) {
      if (na.rm) {
        n_na <- sum(is.na(v))
        if (n_na > 0L) {
          msg(
            "Removed",
            n_na,
            "NA",
            ngettext(n_na, "value", "values"),
            verbosity = verbosity
          )
          v <- v[!is.na(v)]
        }
      }
      grDevices::boxplot.stats(v)$stats
    })
    box_data <- unname(box_data)

    if (horizontal) {
      x_ax <- Axis(type = "value", scale = TRUE)
      y_ax <- Axis(type = "category", data = labels)
    } else {
      x_ax <- Axis(type = "category", data = labels)
      y_ax <- Axis(type = "value", scale = TRUE)
    }

    opt <- EChartsOption(
      title = if (!is.null(title)) Title(text = title) else NULL,
      tooltip = Tooltip(trigger = "item"),
      x_axis = x_ax,
      y_axis = y_ax,
      series = BoxplotSeries(
        data = box_data,
        item_style = item_style
      )
    )
  }

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}
