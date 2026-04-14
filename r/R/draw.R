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
#' @param meta Optional named list: Extra fields merged into the widget payload.
#'   Used internally (e.g. by [draw_heatmap()] to pass square-cell layout
#'   parameters to the JS binding).
#' @return htmlwidget: Widget object.
#' @export
draw <- function(
  option,
  theme = NULL,
  renderer = "canvas",
  width = NULL,
  height = NULL,
  elementId = NULL,
  filename = NULL,
  meta = list()
) {
  # Convert S7 objects to plain lists
  if (S7::S7_inherits(option)) {
    option <- to_list(option)
  }

  # Auto-add legend when multiple distinctly named series are present
  if (
    is.null(option$legend) &&
      is.list(option$series) &&
      length(option$series) > 1L
  ) {
    series_names <- vapply(
      option$series,
      function(s) s$name %||% "",
      character(1)
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
          if (is.null(ax[[i]]$axisTick)) {
            ax[[i]]$axisTick <- list()
          }
          ax[[i]]$axisTick$show <- FALSE
        }
      } else {
        # Single axis config
        if (is.null(ax$axisTick)) {
          ax$axisTick <- list()
        }
        ax$axisTick$show <- FALSE
      }
      option[[axis_key]] <- ax
    }
  }

  # Default grid: containLabel keeps axis labels inside the drawing area.
  if (!is.null(option$xAxis) || !is.null(option$yAxis)) {
    if (is.null(option$grid)) {
      option$grid <- list(containLabel = TRUE)
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

  payload <- c(
    list(
      option = option,
      theme = theme_list,
      renderer = renderer,
      autoTheme = if (auto_theme) TRUE else NULL,
      themeDark = theme_dark_list
    ),
    meta
  )

  # Respect explicit pixel dimensions: when a numeric width/height is given,
  # disable browser.fill/viewer.fill so the widget keeps the computed size
  # instead of expanding to fill the container.
  should_fill <- is.null(width) || is.character(width)

  widget <- htmlwidgets::createWidget(
    name = "draw",
    x = payload,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = should_fill,
      browser.padding = 0,
      viewer.fill = should_fill,
      viewer.padding = 0,
      fill = should_fill
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
#' @param color Optional Character: Series color palette — a single color string or
#'   character vector that overrides the theme palette for this chart.
#'   `color` takes precedence over the theme palette (it sets `option.color`).
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
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
  color = NULL,
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
    color = color,
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
#' @param color Optional Character: Bar color or colors. For multiple series,
#'   colors are applied per series and recycled as needed. For a single series,
#'   a single color styles the whole series; multiple colors are recycled
#'   across individual bars. `color` takes precedence over the theme palette.
#' @param stack Logical: Whether to stack bars.
#' @param horizontal Logical: Whether to draw horizontal bars.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_bar <- function(
  x,
  y,
  color = NULL,
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
    colors <- color %||% rtemis_colors
    colors <- rep_len(colors, length(y))
    series <- lapply(seq_along(y), function(i) {
      BarSeries(
        name = series_names[i],
        data = y[[i]],
        stack = stack_group,
        color = colors[i]
      )
    })
  } else {
    if (is.null(color) || length(color) <= 1L) {
      series <- list(BarSeries(data = y, stack = stack_group, color = color))
    } else {
      colors <- rep_len(color, length(y))
      data_items <- lapply(seq_along(y), function(i) {
        list(
          value = y[[i]],
          itemStyle = list(color = colors[[i]])
        )
      })
      series <- list(BarSeries(data = data_items, stack = stack_group))
    }
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
#' @param color Optional Character: Series color palette — a single color string or
#'   character vector that overrides the theme palette for this chart.
#'   When `group` is set, colors are assigned per group in order. `color` takes
#'   precedence over the theme palette.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
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
  color = NULL,
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
    # Drop NA pairs before fitting
    ok <- !is.na(xv) & !is.na(yv)
    xv <- xv[ok]
    yv <- yv[ok]
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
    # Assign explicit colors so scatter and fit/CI match.
    # Use the caller-supplied palette when provided, else fall back to rtemis_colors.
    palette <- color %||% rtemis_colors
    group_colors <- palette[((seq_along(groups) - 1L) %% length(palette)) + 1L]
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
          x[idx],
          y[idx],
          fit,
          n_fit,
          as.character(g),
          group_colors[i]
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
    color = if (is.null(group)) color else NULL,
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
#' @param color Optional Character: Series color palette — a single color string or
#'   character vector that overrides the theme palette for this chart.
#'   `color` takes precedence over the theme palette (it sets `option.color`).
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
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
  color = NULL,
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
    color = color,
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
      series <- unlist(
        lapply(seq_along(x), function(i) {
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
        }),
        recursive = FALSE
      )
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

  density_formatter <- htmlwidgets::JS(
    "function(params) {
      function ddSci(x, dp) {
        dp = dp || 2;
        var a = Math.abs(x);
        if (a === 0) return '0.' + '0'.repeat(dp);
        if (a >= 1e6 || a < Math.pow(10, -dp)) return x.toExponential(1);
        return x.toFixed(dp);
      }
      var out = ddSci(params[0].value[0]) + '<br/>';
      for (var i = 0; i < params.length; i++) {
        var p = params[i];
        var name = p.seriesName ? p.seriesName + ': ' : '';
        out += p.marker + name + ddSci(p.value[1]) + '<br/>';
      }
      return out;
    }"
  )

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis", formatter = density_formatter),
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
  if (
    is.null(group) &&
      is.list(data) &&
      is.null(labels) &&
      !is.null(names(data)) &&
      all(nzchar(names(data)))
  ) {
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
        if (na.rm) {
          vals <- vals[!is.na(vals)]
        }
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

# -- draw_heatmap ---------------------------------------------------------------

#' Draw a Heatmap
#'
#' Quick heatmap from a numeric matrix. Designed to handle all common use cases
#' including correlation matrices (square cells, diverging color scale),
#' general rectangular heatmaps, and clustered heatmaps.
#'
#' Color encoding is driven by a continuous [VisualMap] component.
#' Hierarchical clustering is performed in R via [hclust()]; rows and columns
#' are reordered accordingly (dendrogram rendering is not yet supported).
#'
#' @param x Numeric matrix: Input data. Rows map to y-axis categories and columns
#'   to x-axis categories.
#' @param row_names Optional Character: Row labels. Defaults to `rownames(x)`, or
#'   `"1"`, `"2"`, ... when row names are absent.
#' @param col_names Optional Character: Column labels. Defaults to `colnames(x)`.
#' @param triangle Optional Character \{"upper", "lower"\}: Mask one triangle of the
#'   matrix to `NA`. `"upper"` keeps only the upper triangle (and diagonal);
#'   `"lower"` keeps only the lower triangle (and diagonal). Useful for symmetric
#'   matrices such as correlation matrices.
#' @param cluster_rows Logical: Whether to reorder rows via hierarchical clustering.
#' @param cluster_cols Logical: Whether to reorder columns via hierarchical clustering.
#' @param dist_method Character: Distance method passed to [stats::dist()].
#'   Common values: `"euclidean"`, `"manhattan"`.
#' @param hclust_method Character: Linkage method passed to [stats::hclust()].
#'   Common values: `"complete"`, `"ward.D2"`, `"average"`.
#' @param square_cells Optional Logical: Whether to compute widget dimensions so
#'   cells are square. `NULL` (default) enables this automatically for square
#'   matrices (e.g. correlation matrices). When `TRUE`, both `width` and `height`
#'   are calculated from the number of cells; supply explicit `width`/`height` to
#'   override.
#' @param color Optional Character: Color palette — a vector of 2 or more colors
#'   defining the continuous color scale from `zlim[1]` to `zlim[2]`. When `NULL`
#'   (default) a diverging teal–white–orange palette is used when data spans zero,
#'   otherwise a sequential single-hue palette is used.
#' @param zlim Optional Numeric: Length-2 vector `c(min, max)` for the color scale.
#'   Defaults to the observed data range. For correlation matrices, `c(-1, 1)` is
#'   recommended.
#' @param show_values Logical: Whether to print the cell value as a label inside
#'   each cell.
#' @param value_digits Integer: Decimal places used in cell labels and the tooltip.
#' @param show_colorbar Logical: Whether to display the continuous color-scale bar.
#' @param colorbar_orient Character \{"vertical", "horizontal"\}: Orientation of the
#'   color bar.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. `NULL` auto-detects light/dark mode.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
draw_heatmap <- function(
  x,
  row_names = NULL,
  col_names = NULL,
  triangle = NULL,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  dist_method = "euclidean",
  hclust_method = "complete",
  square_cells = NULL,
  color = NULL,
  zlim = NULL,
  show_values = FALSE,
  value_digits = 2L,
  show_colorbar = TRUE,
  colorbar_orient = "vertical",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
) {
  # -- 1. Validate & coerce ------------------------------------------------------
  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.numeric(x)) cli::cli_abort("{.arg x} must be a numeric matrix.")
  if (!is.null(triangle)) {
    triangle <- match.arg(triangle, c("upper", "lower"))
  }
  colorbar_orient <- match.arg(colorbar_orient, c("vertical", "horizontal"))

  n_rows <- nrow(x)
  n_cols <- ncol(x)

  # -- 2. Row / column labels ----------------------------------------------------
  rn <- row_names %||% rownames(x) %||% as.character(seq_len(n_rows))
  cn <- col_names %||% colnames(x) %||% as.character(seq_len(n_cols))

  # -- 3. Triangle masking -------------------------------------------------------
  if (!is.null(triangle)) {
    mask <- if (triangle == "upper") lower.tri(x) else upper.tri(x)
    x[mask] <- NA
  }

  # -- 4. Hierarchical clustering (reorders matrix in place) ---------------------
  if (cluster_rows && n_rows > 1L) {
    complete <- !apply(x, 1L, function(r) all(is.na(r)))
    if (sum(complete) > 1L) {
      d <- stats::dist(x[complete, , drop = FALSE], method = dist_method)
      h <- stats::hclust(d, method = hclust_method)
      ord <- seq_len(n_rows)
      ord[complete] <- which(complete)[h$order]
      x  <- x[ord, , drop = FALSE]
      rn <- rn[ord]
    }
  }
  if (cluster_cols && n_cols > 1L) {
    complete <- !apply(x, 2L, function(cv) all(is.na(cv)))
    if (sum(complete) > 1L) {
      d <- stats::dist(t(x[, complete, drop = FALSE]), method = dist_method)
      h <- stats::hclust(d, method = hclust_method)
      ord <- seq_len(n_cols)
      ord[complete] <- which(complete)[h$order]
      x  <- x[, ord, drop = FALSE]
      cn <- cn[ord]
    }
  }

  # -- 5. Color limits -----------------------------------------------------------
  if (is.null(zlim)) {
    zlim <- range(x, na.rm = TRUE)
  }

  # -- 6. Color palette ----------------------------------------------------------
  if (is.null(color)) {
    if (zlim[1L] < 0 && zlim[2L] > 0) {
      # Diverging: teal -> white -> orange
      color <- c(rtemis_colors[[1L]], "#ffffff", rtemis_colors[[2L]])
    } else if (zlim[2L] <= 0) {
      # All non-positive: orange -> white
      color <- c(rtemis_colors[[2L]], "#ffffff")
    } else {
      # All non-negative: white -> teal
      color <- c("#ffffff", rtemis_colors[[1L]])
    }
  }

  # -- 7. Heatmap data: list of [col_idx, row_idx, value] ------------------------
  # x index = column (j-1), y index = row (i-1).
  # With inverse = TRUE on the y-axis, row 0 appears at the top (matrix convention).
  # NA values become NULL so ECharts leaves those cells uncolored.
  data_list <- vector("list", n_rows * n_cols)
  k <- 1L
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      val <- x[i, j]
      data_list[[k]] <- list(j - 1L, i - 1L, if (is.na(val)) NULL else val)
      k <- k + 1L
    }
  }

  # -- 8. Layout margins (always computed; used for grid, title, and dimensions) -
  # These margins define the space outside the plot area:
  #   left_px  — room for y-axis (row) labels; ~8px per character + padding
  #   right_px — room for the vertical colorbar, or minimal padding otherwise
  #   top_px   — room for the chart title, or minimal padding otherwise
  #   bot_px   — room for x-axis (column) labels; may be rotated 45°
  # With containLabel = FALSE (explicit grid), grid.left IS the plot-area origin,
  # so title.left = left_px gives pixel-precise alignment with the first column.
  rotate   <- if (n_cols > 8L || max(nchar(cn)) > 6L) 45L else 0L
  left_px  <- min(20L + max(nchar(rn)) * 8L, 200L)
  right_px <- if (show_colorbar && colorbar_orient == "vertical") 90L else 20L
  top_px   <- if (!is.null(title)) 40L else 10L
  bot_px   <- if (rotate > 0L) min(20L + max(nchar(cn)) * 5L, 140L) else 36L

  if (is.null(square_cells)) square_cells <- (n_rows == n_cols)

  # For square-cell heatmaps, compute R-side dimensions as a static fallback
  # (e.g. knitr/quarto output where JS resize callbacks may not run).
  # The JS binding also enforces square cells dynamically on init and resize,
  # so these pixel values are used as the initial widget allocation.
  if (square_cells && is.null(width) && is.null(height)) {
    cell_px <- 40L
    width   <- n_cols * cell_px + left_px + right_px
    height  <- n_rows * cell_px + top_px + bot_px
  }

  # -- 9. Tooltip: shows "row x col: value" -------------------------------------
  cols_json <- jsonlite::toJSON(cn, auto_unbox = FALSE)
  rows_json <- jsonlite::toJSON(rn, auto_unbox = FALSE)
  tooltip_fmt <- htmlwidgets::JS(paste0(
    "(function(){",
    "var cn=", cols_json, ";",
    "var rn=", rows_json, ";",
    "return function(p){",
    "if(!p.value||p.value[2]===null||p.value[2]===undefined)return'NA';",
    "return rn[p.value[1]]+' \u00d7 '+cn[p.value[0]]+': '+p.value[2].toFixed(", value_digits, ");",
    "}})()"
  ))

  # -- 10. Optional in-cell value labels -----------------------------------------
  label_opt <- if (show_values) {
    LabelOption(
      show = TRUE,
      formatter = htmlwidgets::JS(paste0(
        "function(p){",
        "if(!p.value||p.value[2]===null||p.value[2]===undefined)return'';",
        "return p.value[2].toFixed(", value_digits, ");",
        "}"
      ))
    )
  } else {
    NULL
  }

  # -- 11. Assemble and render ---------------------------------------------------
  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title, left = left_px) else NULL,
    tooltip = Tooltip(trigger = "item", formatter = tooltip_fmt),
    # Explicit grid with containLabel = FALSE so that left_px is the exact
    # distance from the container edge to the plot-area left edge, enabling
    # pixel-precise title alignment (title.left = left_px).
    grid = Grid(
      left = left_px,
      right = right_px,
      top = top_px,
      bottom = bot_px,
      contain_label = FALSE
    ),
    x_axis = Axis(
      type = "category",
      data = as.list(cn),
      split_area = SplitArea(show = FALSE),
      axis_label = AxisLabel(rotate = rotate),
      boundary_gap = TRUE
    ),
    y_axis = Axis(
      type = "category",
      data = as.list(rn),
      inverse = TRUE,
      split_area = SplitArea(show = FALSE),
      boundary_gap = TRUE
    ),
    visual_map = VisualMap(
      type = "continuous",
      min = zlim[[1L]],
      max = zlim[[2L]],
      # Match colorbar label precision to the cell-value precision so that the
      # min/max labels and the draggable-handle tooltip are never rounded to
      # whole numbers (ECharts default precision = 0).
      precision = value_digits,
      calculable = TRUE,
      show = show_colorbar,
      orient = colorbar_orient,
      in_range = list(color = as.list(color)),
      # Vertical: pin to the right edge, centered with the plot area.
      # Horizontal: centered at the bottom.
      right  = if (colorbar_orient == "vertical")   "right"  else NULL,
      top    = if (colorbar_orient == "vertical")   "middle" else NULL,
      left   = if (colorbar_orient == "horizontal") "center" else NULL,
      bottom = if (colorbar_orient == "horizontal") "bottom" else NULL
    ),
    series = HeatmapSeries(
      data  = data_list,
      label = label_opt
    )
  )

  # Pass square-cell layout parameters to the JS binding so it can enforce
  # square cells dynamically on init and resize (viewer/browser resize events).
  heatmap_meta <- if (square_cells) {
    list(
      squareCells = TRUE,
      nRows = n_rows,
      nCols = n_cols,
      leftPx = left_px,
      rightPx = right_px,
      topPx = top_px,
      botPx = bot_px
    )
  } else {
    list()
  }

  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    filename = filename,
    meta = heatmap_meta
  )
}
