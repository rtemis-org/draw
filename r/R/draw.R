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
#' @param points Logical: Whether to show point markers on each data value.
#'   Defaults to `TRUE`; set to `FALSE` on long time-series where the symbols
#'   add visual noise.
#' @param blocks Optional factor, integer, character, or logical of length
#'   `length(x)`: Per-x-value group label used to shade vertical background
#'   bands. Contiguous runs of the same level become one band. `NA` entries
#'   produce no band.
#' @param block_color Optional Character vector or list of length k, where
#'   k is the number of unique levels in `blocks`: Fill color for each level.
#'   Match by name to factor levels if named, else positional. `NA`, `NULL`,
#'   or `"transparent"` entries skip that level.
#' @param block_opacity Numeric \[0, 1\]: Fill opacity applied to all bands.
#'   Defaults to `0.2`.
#' @param color Optional Character: Series color palette — a single color string or
#'   character vector that overrides the theme palette for this chart.
#'   `color` takes precedence over the theme palette (it sets `option.color`).
#' @param xlim Optional Numeric \[length 2\]: X-axis limits `c(min, max)`.
#'   Ignored when `x` is not numeric. Defaults to `range(x)` (no padding) when
#'   `x` is numeric.
#' @param ylim Optional Numeric \[length 2\]: Y-axis limits `c(min, max)`.
#'   Defaults to the range of all `y` values across series (no padding).
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
#' @param zoom Logical, [DataZoom], or list of [DataZoom]: Enable x-axis zoom.
#'   `TRUE` adds a slider plus mouse-wheel/drag zoom on the x-axis; `FALSE`
#'   (default) disables zoom. Pass [DataZoom] objects (or a list of them) for
#'   full control over zoom behavior and styling.
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
  points = TRUE,
  blocks = NULL,
  block_color = NULL,
  block_opacity = 0.2,
  color = NULL,
  xlim = NULL,
  ylim = NULL,
  title = NULL,
  theme = NULL,
  zoom = FALSE,
  width = NULL,
  height = NULL,
  filename = NULL
) {
  validate_axis_lim(xlim, "xlim")
  validate_axis_lim(ylim, "ylim")

  # Determine axis type
  x_type <- if (is.numeric(x)) "value" else "category"

  # `xlim` only makes sense on a numeric (value) x-axis.
  if (!is.null(xlim) && x_type != "value") {
    cli::cli_abort("{.arg xlim} only applies when {.arg x} is numeric.")
  }

  # Resolve axis limits. Defaults: exact data range (no padding) so bands and
  # lines fill the plot width; users can pass `xlim`/`ylim` for explicit bounds.
  y_all <- if (is.list(y)) unlist(y, use.names = FALSE) else y
  x_lim <- if (x_type == "value") (xlim %||% range(x, na.rm = TRUE)) else NULL
  y_lim <- ylim %||% range(y_all, na.rm = TRUE)

  # Format series data: value axes need [x, y] pairs; category axes need y only
  pair_xy <- function(y_vals) {
    if (x_type == "value") {
      mapply(c, x, y_vals, SIMPLIFY = FALSE)
    } else {
      y_vals
    }
  }

  # ECharts treats missing `showSymbol` as TRUE; only pass FALSE when hiding.
  show_symbol <- if (isTRUE(points)) NULL else FALSE

  # Build series
  if (is.list(y) && !is.null(names(y))) {
    series_names <- names(y)
    series <- lapply(seq_along(y), function(i) {
      LineSeries(
        name = series_names[i],
        data = pair_xy(y[[i]]),
        smooth = smooth,
        show_symbol = show_symbol,
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
        show_symbol = show_symbol,
        area_style = if (area) AreaStyle() else NULL
      )
    })
  } else {
    series <- list(LineSeries(
      data = pair_xy(y),
      smooth = smooth,
      show_symbol = show_symbol,
      area_style = if (area) AreaStyle() else NULL
    ))
  }

  # Optional vertical background bands from `blocks` + `block_color`.
  if (!is.null(blocks)) {
    mark_area <- build_block_mark_area(
      x = x,
      blocks = blocks,
      block_color = block_color,
      block_opacity = block_opacity
    )
    if (!is.null(mark_area)) {
      series[[1]]@mark_area <- mark_area
    }
  }

  # Resolve `zoom` argument into an (optional) list of DataZoom specs.
  data_zoom <- resolve_zoom(zoom, axis = "x")

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "axis"),
    legend = if (length(series) > 1L) Legend() else NULL,
    x_axis = Axis(
      type = x_type,
      data = if (x_type == "category") x else NULL,
      scale = if (x_type == "value") TRUE else NULL,
      min = if (!is.null(x_lim)) x_lim[[1L]] else NULL,
      max = if (!is.null(x_lim)) x_lim[[2L]] else NULL,
      split_line = if (x_type == "value") no_corner_split_line() else NULL,
      axis_label = if (x_type == "value") no_corner_axis_label() else NULL,
      axis_line = if (x_type == "value") {
        axis_line_for_orthogonal(y_lim)
      } else {
        NULL
      }
    ),
    y_axis = Axis(
      type = "value",
      scale = TRUE,
      min = y_lim[[1L]],
      max = y_lim[[2L]],
      split_line = no_corner_split_line(),
      axis_label = no_corner_axis_label(),
      axis_line = axis_line_for_orthogonal(x_lim)
    ),
    color = color,
    data_zoom = data_zoom,
    series = series
  )

  draw(opt, theme = theme, width = width, height = height, filename = filename)
}

#' Default split-line configuration suppressing corner grid lines
#'
#' Returns a [SplitLine] with `show_min_line = FALSE` and `show_max_line = FALSE`
#' so the first and last split lines (which sit on the plot border) are not
#' drawn. Used as the default for value axes in `draw_line` and `draw_scatter`.
#'
#' @return [SplitLine]: Split-line configuration.
#' @keywords internal
#' @noRd
no_corner_split_line <- function() {
  SplitLine(show_min_line = FALSE, show_max_line = FALSE)
}

#' Default axis-label configuration suppressing min/max endpoint labels
#'
#' Returns an [AxisLabel] with `show_min_label = FALSE` and `show_max_label = FALSE`
#' so echarts does not render unrounded endpoint labels (e.g. `0.64`, `30.36`)
#' that arise when `min`/`max` are pinned to padded data ranges. Used as the
#' default for value axes in `draw_line` and `draw_scatter`.
#'
#' @return [AxisLabel]: Axis-label configuration.
#' @keywords internal
#' @noRd
no_corner_axis_label <- function() {
  AxisLabel(show_min_label = FALSE, show_max_label = FALSE)
}

#' Axis line configuration with `onZero` keyed off the orthogonal axis limits
#'
#' Returns an [AxisLine] with `on_zero = TRUE` when the orthogonal axis's
#' visible range includes 0 (so this axis sits on the zero line of the other,
#' visually emphasizing zero), and `on_zero = FALSE` otherwise (so it sits at
#' the plot edge). Returns `NULL` when `ortho_lim` is `NULL` (e.g. category
#' orthogonal axis) to leave echarts' default behavior in place.
#'
#' @param ortho_lim Optional Numeric \[length 2\]: `c(min, max)` of the
#'   *orthogonal* axis — for an x-axis config this is the y-axis limits, and
#'   vice versa.
#' @return Optional [AxisLine]: Axis-line configuration.
#' @keywords internal
#' @noRd
axis_line_for_orthogonal <- function(ortho_lim) {
  if (is.null(ortho_lim)) {
    return(NULL)
  }
  AxisLine(on_zero = ortho_lim[[1L]] <= 0 && ortho_lim[[2L]] >= 0)
}

#' Build a `MarkArea` from `blocks` + `block_color` arguments
#'
#' Run-length-encodes `blocks` along `x` and produces one [MarkAreaDataPoint]
#' pair per contiguous run whose level has a drawable color. Levels whose
#' color is `NA`, `NULL`, or `"transparent"` are skipped.
#'
#' @param x Vector: X-axis values (numeric or character).
#' @param blocks Atomic vector of length `length(x)`: Per-x-value group label.
#'   `NA` entries break runs and produce no band.
#' @param block_color Character vector or list, length equal to the number of
#'   unique levels in `blocks`: Color per level. If named, matched to level
#'   names; else positional (factor levels for factors, `sort(unique(...))`
#'   otherwise).
#' @param block_opacity Numeric \[0, 1\]: Fill opacity applied via ItemStyle.
#' @return Optional [MarkArea]: `MarkArea` object, or `NULL` if there are no
#'   bands to draw.
#' @keywords internal
#' @noRd
build_block_mark_area <- function(x, blocks, block_color, block_opacity) {
  if (length(blocks) != length(x)) {
    cli::cli_abort(
      "{.arg blocks} must have the same length as {.arg x} ({length(x)}); got {length(blocks)}."
    )
  }
  if (!is.atomic(blocks)) {
    cli::cli_abort(
      "{.arg blocks} must be an atomic vector (factor, integer, character, or logical)."
    )
  }
  if (is.null(block_color)) {
    cli::cli_abort(
      "{.arg block_color} must be provided when {.arg blocks} is set."
    )
  }
  if (!is.numeric(block_opacity) || length(block_opacity) != 1L) {
    cli::cli_abort("{.arg block_opacity} must be a single number.")
  }

  # Determine level order so positional matching is stable.
  levels_vec <- if (is.factor(blocks)) {
    levels(blocks)
  } else {
    sort(unique(blocks[!is.na(blocks)]))
  }
  k <- length(levels_vec)
  as_key <- function(v) as.character(v)
  level_keys <- as_key(levels_vec)

  # Resolve color-per-level lookup.
  bc_names <- names(block_color)
  if (!is.null(bc_names) && !any(bc_names == "")) {
    missing_levels <- setdiff(level_keys, bc_names)
    if (length(missing_levels) > 0L) {
      cli::cli_abort(
        "{.arg block_color} is missing entries for {.val {missing_levels}}."
      )
    }
    color_for <- function(lvl) block_color[[as_key(lvl)]]
  } else {
    if (length(block_color) != k) {
      cli::cli_abort(
        "{.arg block_color} must have length {k} (one per level of {.arg blocks})."
      )
    }
    color_for <- function(lvl) {
      block_color[[match(as_key(lvl), level_keys)]]
    }
  }

  # Skip predicate: NA, NULL, "transparent" -> no band.
  is_blank <- function(v) {
    is.null(v) ||
      (length(v) == 1L && is.na(v)) ||
      (is.character(v) && identical(v, "transparent"))
  }

  # Run-length-encode over blocks, skipping NA-level runs.
  keys <- ifelse(is.na(blocks), NA_character_, as_key(blocks))
  r <- rle(keys)
  n_runs <- length(r[["values"]])
  run_end <- cumsum(r[["lengths"]])
  run_start <- c(1L, utils::head(run_end, -1L) + 1L)

  # Which runs will be drawn (non-NA level, non-blank color).
  drawn <- vapply(
    seq_len(n_runs),
    function(i) {
      key <- r[["values"]][i]
      !is.na(key) && !is_blank(color_for(key))
    },
    logical(1)
  )

  # On a value (numeric) x-axis a band drawn from x[first] to x[last] leaves
  # an unshaded strip between adjacent runs (e.g. x=8 to x=9). Extend each
  # drawn run's end to the next drawn run's start so they meet exactly. On a
  # category axis, echarts shades full cells inclusive of both endpoints, so
  # extending would overlap — keep the simple per-run endpoints there.
  extend_to_next <- is.numeric(x)

  pairs <- list()
  for (i in seq_len(n_runs)) {
    if (!drawn[i]) {
      next
    }
    key <- r[["values"]][i]
    col <- color_for(key)
    x0 <- x[run_start[i]]
    x1 <- if (extend_to_next && i < n_runs && drawn[i + 1L]) {
      x[run_start[i + 1L]]
    } else {
      x[run_end[i]]
    }
    pairs[[length(pairs) + 1L]] <- list(
      MarkAreaDataPoint(
        x_axis = x0,
        name = key,
        item_style = ItemStyle(color = col, opacity = block_opacity)
      ),
      MarkAreaDataPoint(x_axis = x1)
    )
  }

  if (length(pairs) == 0L) {
    return(NULL)
  }
  MarkArea(data = pairs, silent = TRUE)
}

#' Resolve the `zoom` argument of `draw_*` functions
#'
#' Translates the ergonomic `zoom` argument into a list of [DataZoom] specs (or
#' `NULL`). Accepts `FALSE`/`NULL` (no zoom), `TRUE` (default slider + inside
#' pair on the given axis), a single [DataZoom], or a list of [DataZoom] /
#' plain named lists (passed through untouched).
#'
#' @param zoom Logical, [DataZoom], or list: User-facing `zoom` argument.
#' @param axis Character \{"x", "y"\}: Which axis the default `TRUE` preset
#'   should target.
#' @return Optional list of [DataZoom] specs (or plain lists): Value suitable
#'   for `EChartsOption(data_zoom = ...)`.
#' @keywords internal
#' @noRd
resolve_zoom <- function(zoom, axis = "x") {
  if (is.null(zoom) || isFALSE(zoom)) {
    return(NULL)
  }
  if (isTRUE(zoom)) {
    axis_args <- if (identical(axis, "y")) {
      list(y_axis_index = 0)
    } else {
      list(x_axis_index = 0)
    }
    return(list(
      do.call(
        DataZoom,
        c(list(type = "slider", start = 0, end = 100), axis_args)
      ),
      do.call(
        DataZoom,
        c(
          list(
            type = "inside",
            zoom_on_mouse_wheel = TRUE,
            move_on_mouse_move = TRUE
          ),
          axis_args
        )
      )
    ))
  }
  if (S7::S7_inherits(zoom, DataZoom)) {
    return(list(zoom))
  }
  if (is.list(zoom)) {
    return(zoom)
  }
  cli::cli_abort(
    "{.arg zoom} must be {.code TRUE}, {.code FALSE}, a {.cls DataZoom} object, or a list; got {.cls {class(zoom)[1]}}."
  )
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
#' @param xlim Optional Numeric \[length 2\]: X-axis limits `c(min, max)`.
#'   Defaults to `range(x)` padded by 4\\% of the span on each side.
#' @param ylim Optional Numeric \[length 2\]: Y-axis limits `c(min, max)`.
#'   Defaults to `range(y)` padded by 4\\% of the span on each side.
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
  xlim = NULL,
  ylim = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
) {
  validate_axis_lim(xlim, "xlim")
  validate_axis_lim(ylim, "ylim")

  # Resolve axis limits. Defaults apply 4% symmetric padding so points aren't
  # drawn on the axis edges — matches base R `xaxs = "r"` and ggplot2 ~5%.
  x_lim <- xlim %||% calc_limits(x)
  y_lim <- ylim %||% calc_limits(y)

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
    x_axis = Axis(
      type = "value",
      scale = TRUE,
      min = x_lim[[1L]],
      max = x_lim[[2L]],
      split_line = no_corner_split_line(),
      axis_label = no_corner_axis_label(),
      axis_line = axis_line_for_orthogonal(y_lim)
    ),
    y_axis = Axis(
      type = "value",
      scale = TRUE,
      min = y_lim[[1L]],
      max = y_lim[[2L]],
      split_line = no_corner_split_line(),
      axis_label = no_corner_axis_label(),
      axis_line = axis_line_for_orthogonal(x_lim)
    ),
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

# -- hclust_to_dendro_data ------------------------------------------------------

#' Convert an hclust object to ECharts custom-series segment data
#'
#' Extracts the merge tree from an `hclust` result and returns a list of
#' 5-element tuples `[left_pos, right_pos, left_h, right_h, merge_h]`, one per
#' internal merge.  Each tuple encodes the four-point L/U polyline drawn by the
#' ECharts `custom` renderItem for that merge.
#'
#' @param h `hclust` object returned by [stats::hclust()].
#' @param uniform Logical: Whether to use uniform heights for rendering.
#' @return Named list with three elements:
#'   \describe{
#'     \item{`data`}{Length `n - 1` list of 5-element numeric lists.}
#'     \item{`min_height`}{Numeric: height of the lowest merge.}
#'     \item{`max_height`}{Numeric: height of the root merge (used for axis scaling).}
#'   }
#' @keywords internal
#' @noRd
hclust_to_dendro_data <- function(h, uniform = FALSE) {
  n <- length(h[["order"]])

  # leaf_pos[i] = 0-based position of leaf i in the reordered sequence
  leaf_pos <- numeric(n)
  for (p in seq_len(n)) {
    leaf_pos[h[["order"]][p]] <- p - 1L
  }

  # node_pos[k] = midpoint position of internal node k
  node_pos <- numeric(n - 1L)
  for (k in seq_len(n - 1L)) {
    l <- h[["merge"]][k, 1L]
    r <- h[["merge"]][k, 2L]
    lp <- if (l < 0L) leaf_pos[-l] else node_pos[l]
    rp <- if (r < 0L) leaf_pos[-r] else node_pos[r]
    node_pos[k] <- (lp + rp) / 2.0
  }

  # Heights used for rendering: actual merge distances or uniform levels 1..n-1.
  # Uniform heights give each merge step equal visual height, making the
  # dendrogram easier to read when merge distances span a wide range.
  heights <- if (uniform) seq_len(n - 1L) else h[["height"]]

  # Build one 5-tuple per merge: [left_pos, right_pos, left_h, right_h, merge_h]
  data <- vector("list", n - 1L)
  for (k in seq_len(n - 1L)) {
    l <- h[["merge"]][k, 1L]
    r <- h[["merge"]][k, 2L]
    lp <- if (l < 0L) leaf_pos[-l] else node_pos[l]
    rp <- if (r < 0L) leaf_pos[-r] else node_pos[r]
    lh <- if (l < 0L) 0.0 else heights[l]
    rh <- if (r < 0L) 0.0 else heights[r]
    data[[k]] <- list(lp, rp, lh, rh, heights[k])
  }

  list(
    data = data,
    min_height = heights[1L],
    max_height = heights[n - 1L]
  )
}

# Build a diverging colour palette where the midpoint colour falls exactly at
# value 0, even when zlim is asymmetric (e.g. c(-0.74, 1.0)).
# Stops are allocated proportionally: abs(zlim[1])/span of the stops cover the
# negative side, the rest cover the positive side.
#
# @param neg_color Character: Colour at zlim[1].
# @param mid_color Character: Colour at 0.
# @param pos_color Character: Colour at zlim[2].
# @param zlim Numeric: Length-2 vector c(min, max) with zlim[1] < 0 < zlim[2].
# @param n Integer: Total number of colour stops (odd is conventional).
# @return Character vector of hex colours of length n.
# @keywords internal
# @noRd
diverging_palette <- function(neg_color, mid_color, pos_color, zlim, n = 101L) {
  span <- zlim[[2L]] - zlim[[1L]]
  n_neg <- round(abs(zlim[[1L]]) / span * (n - 1L)) + 1L
  n_neg <- max(2L, min(n - 1L, n_neg))
  n_pos <- n - n_neg + 1L
  n_pos <- max(2L, n_pos)
  neg_ramp <- grDevices::colorRampPalette(c(neg_color, mid_color))(n_neg)
  pos_ramp <- grDevices::colorRampPalette(c(mid_color, pos_color))(n_pos)
  # Remove the duplicated midpoint that both ramps share
  c(neg_ramp, pos_ramp[-1L])
}

# Compute the axis minimum for a dendrogram height axis so that leaf stubs
# occupy `stub_frac` of the visible range rather than spanning all the way
# to zero.  When every merge is in a tight band near the top, this prevents
# the stems from dominating the panel with empty space.
#
# @param min_h Numeric: height of the lowest merge.
# @param max_h Numeric: height of the root merge.
# @param stub_frac Numeric `(0, 1)`: fraction of visible range allocated to stubs.
# @return Numeric: axis min value (may be negative for very compressed ranges).
# @keywords internal
# @noRd
dendro_axis_min <- function(min_h, max_h, stub_frac = 0.1) {
  if (min_h >= max_h) {
    return(min_h * (1 - stub_frac))
  }
  (min_h - stub_frac * max_h) / (1 - stub_frac)
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
#' are reordered accordingly.  When `cluster_rows` or `cluster_cols` is `TRUE`
#' (and the corresponding `show_*_dendro` flag is not `FALSE`), a dendrogram
#' panel is rendered alongside the heatmap as an ECharts custom series.
#'
#' @param x Numeric matrix: Input data. Rows map to y-axis categories and columns
#'   to x-axis categories.
#' @param row_names Optional Character: Row labels. Defaults to `rownames(x)`, or
#'   `"1"`, `"2"`, ... when row names are absent.
#' @param col_names Optional Character: Column labels. Defaults to `colnames(x)`.
#' @param triangle Optional Character \{"upper", "lower"\}: Mask one triangle of the
#'   matrix to `NA`. `"upper"` keeps only the upper triangle; `"lower"` keeps
#'   only the lower triangle. The diagonal is always masked, as it is
#'   uninformative for symmetric matrices (e.g. always 1 for correlations).
#' @param cluster_rows Logical: Whether to reorder rows via hierarchical clustering.
#' @param cluster_cols Logical: Whether to reorder columns via hierarchical clustering.
#' @param dist_method Character: Distance method passed to [stats::dist()].
#'   Common values: `"euclidean"`, `"manhattan"`.
#' @param hclust_method Character: Linkage method passed to [stats::hclust()].
#'   Common values: `"complete"`, `"ward.D2"`, `"average"`.
#' @param show_row_dendro Logical: Whether to render the row dendrogram panel
#'   when `cluster_rows = TRUE`. Set to `FALSE` to reorder rows but suppress the
#'   visual dendrogram.
#' @param show_col_dendro Logical: Whether to render the col dendrogram panel
#'   when `cluster_cols = TRUE`.
#' @param dendro_row_width Optional Numeric `[1, Inf)`: Pixel width of the row
#'   dendrogram panel (between row labels and the heatmap grid).
#' @param dendro_col_height Optional Numeric `[1, Inf)`: Pixel height of the
#'   col dendrogram panel (between the chart title and the heatmap grid).
#' @param dendro_color Optional Character: Stroke color for dendrogram branch
#'   lines. `NULL` (default) uses a semi-transparent grey (`"#99999988"`),
#'   which works in both light and dark themes.
#' @param dendro_uniform Logical: Whether to render dendrograms with uniform level
#'   heights — each merge step occupies equal visual space — rather than heights
#'   proportional to actual merge distances. `FALSE` (default) preserves merge
#'   distances in the visual scaling.
#' @param dendro_row_side Character \{"right", "left"\}: Side of the heatmap on which
#'   the row dendrogram is placed. `"right"` (default) keeps row labels on the left
#'   with the dendrogram on the right for a clean, symmetric layout.
#' @param dendro_col_side Character \{"top", "bottom"\}: Side of the heatmap on which
#'   the column dendrogram is placed. `"top"` (default) places it above the heatmap.
#'   `"bottom"` automatically moves column labels to the top.
#' @param square_cells Optional Logical: Whether to compute widget dimensions so
#'   cells are square. `NULL` (default) enables this automatically for square
#'   matrices (e.g. correlation matrices). When `TRUE`, both `width` and `height`
#'   are calculated from the number of cells; supply explicit `width`/`height` to
#'   override.
#' @param color Optional Character: Color palette — a vector of 2 or more colors
#'   defining the continuous color scale from `zlim[1]` to `zlim[2]`. When `NULL`
#'   (default) a diverging teal–background–orange palette is used when data spans
#'   zero (with the background colour pinned exactly at 0, even for asymmetric
#'   ranges), otherwise a sequential single-hue palette is used. Two variants
#'   (light / dark) are computed automatically and the JS binding selects the
#'   correct one based on the active theme.
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
  show_row_dendro = TRUE,
  show_col_dendro = TRUE,
  dendro_row_width = 60,
  dendro_col_height = 60,
  dendro_color = NULL,
  dendro_uniform = FALSE,
  dendro_row_side = "right",
  dendro_col_side = "top",
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
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric matrix.")
  }
  if (!is.null(triangle)) {
    triangle <- match.arg(triangle, c("upper", "lower"))
  }
  colorbar_orient <- match.arg(colorbar_orient, c("vertical", "horizontal"))
  dendro_row_side <- match.arg(dendro_row_side, c("right", "left"))
  dendro_col_side <- match.arg(dendro_col_side, c("top", "bottom"))

  n_rows <- nrow(x)
  n_cols <- ncol(x)

  # -- 2. Row / column labels ----------------------------------------------------
  rn <- row_names %||% rownames(x) %||% as.character(seq_len(n_rows))
  cn <- col_names %||% colnames(x) %||% as.character(seq_len(n_cols))

  # -- 3. Triangle masking -------------------------------------------------------
  # For symmetric matrices (e.g. correlation), the diagonal is uninformative
  # (always 1 for correlations), so mask it along with the hidden triangle.
  # After masking, drop any rows/cols that are entirely NA — these are the
  # "outer" labels (first variable on y-axis, last on x-axis for lower triangle)
  # that have no visible cells and would otherwise float label-only in the plot.
  if (!is.null(triangle)) {
    mask <- if (triangle == "upper") lower.tri(x) else upper.tri(x)
    x[mask] <- NA
    diag(x) <- NA
    rows_keep <- apply(x, 1L, function(r) any(!is.na(r)))
    cols_keep <- apply(x, 2L, function(c) any(!is.na(c)))
    x <- x[rows_keep, cols_keep, drop = FALSE]
    rn <- rn[rows_keep]
    cn <- cn[cols_keep]
    n_rows <- nrow(x)
    n_cols <- ncol(x)
  }

  # -- 4. Hierarchical clustering (reorders matrix in place) ---------------------
  # hclust objects are retained so dendrograms can be rendered (when requested).
  row_h <- NULL
  col_h <- NULL
  if (cluster_rows && n_rows > 1L) {
    complete <- rowSums(!is.na(x)) > 0L
    if (sum(complete) > 1L) {
      d <- stats::dist(x[complete, , drop = FALSE], method = dist_method)
      row_h <- stats::hclust(d, method = hclust_method)
      ord <- seq_len(n_rows)
      ord[complete] <- which(complete)[row_h[["order"]]]
      x <- x[ord, , drop = FALSE]
      rn <- rn[ord]
    }
  }
  if (cluster_cols && n_cols > 1L) {
    complete <- colSums(!is.na(x)) > 0L
    if (sum(complete) > 1L) {
      d <- stats::dist(t(x[, complete, drop = FALSE]), method = dist_method)
      col_h <- stats::hclust(d, method = hclust_method)
      ord <- seq_len(n_cols)
      ord[complete] <- which(complete)[col_h[["order"]]]
      x <- x[, ord, drop = FALSE]
      cn <- cn[ord]
    }
  }

  # -- 5. Color limits -----------------------------------------------------------
  if (is.null(zlim)) {
    zlim <- range(x, na.rm = TRUE)
  }

  # -- 6. Color palette ----------------------------------------------------------
  # When color = NULL, build theme-aware palettes where the background colour
  # maps to 0 (diverging) or to the "empty" end of single-sided ranges.
  # Two variants are computed and passed to JS; the binding selects the one
  # that matches the active theme at render time.
  color_auto <- is.null(color)

  if (color_auto) {
    bg_light <- "#ffffff"
    bg_dark <- "#181818" # default bg from theme_dark()

    if (zlim[1L] < 0 && zlim[2L] > 0) {
      # Diverging: teal(neg) -> bg -> orange(pos), midpoint pinned to 0
      color_light <- diverging_palette(
        rtemis_colors[[1L]],
        bg_light,
        rtemis_colors[[2L]],
        zlim
      )
      color_dark <- diverging_palette(
        rtemis_colors[[1L]],
        bg_dark,
        rtemis_colors[[2L]],
        zlim
      )
    } else if (zlim[2L] <= 0) {
      # All non-positive: orange -> bg
      color_light <- grDevices::colorRampPalette(
        c(rtemis_colors[[2L]], bg_light)
      )(101L)
      color_dark <- grDevices::colorRampPalette(
        c(rtemis_colors[[2L]], bg_dark)
      )(101L)
    } else {
      # All non-negative: bg -> teal
      color_light <- grDevices::colorRampPalette(
        c(bg_light, rtemis_colors[[1L]])
      )(101L)
      color_dark <- grDevices::colorRampPalette(
        c(bg_dark, rtemis_colors[[1L]])
      )(101L)
    }

    # Embed the light palette in the option; JS substitutes the dark one
    # when a dark theme is active.
    color <- color_light
  } else {
    color_light <- NULL
    color_dark <- NULL
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
  # Base margins around the heatmap plot area:
  #   left_px  — row labels (~8 px/char + padding)
  #   right_px — vertical colorbar, or minimal padding
  #   top_px   — chart title, or minimal padding
  #   bot_px   — column labels (height varies with rotation)
  #
  # Dendrogram panels are placed outside the heatmap grid on the chosen side.
  # hm_left/hm_right/hm_top/hm_bottom are the final heatmap grid margins that
  # incorporate dendrogram panel widths/heights on each side.
  #
  # Row dendrogram side = "right" (default):
  #   layout: [row labels | heatmap | row dendro | colorbar]
  # Row dendrogram side = "left":
  #   layout: [row dendro | row labels | heatmap | colorbar]
  #
  # Col dendrogram side = "top" (default):
  #   layout: [col dendro] above [heatmap], col labels at bottom
  # Col dendrogram side = "bottom":
  #   col dendro is below the heatmap; col labels move to the top automatically
  #   to avoid overlap with the dendrogram panel.
  #
  # With containLabel = FALSE (explicit pixel grids), title.left = hm_left gives
  # pixel-precise alignment with the heatmap's left edge.
  rotate <- if (max(nchar(cn)) > 4L) 45L else 0L
  left_px <- min(20L + max(nchar(rn)) * 8L, 200L)
  right_px <- if (show_colorbar && colorbar_orient == "vertical") 90L else 20L
  top_px <- if (!is.null(title)) 40L else 10L
  bot_px <- if (rotate > 0L) min(20L + max(nchar(cn)) * 5L, 140L) else 36L

  # Determine which dendrogram panels to show
  # (only possible when there are enough rows/cols to cluster)
  dendro_rows_shown <- !is.null(row_h) && show_row_dendro
  dendro_cols_shown <- !is.null(col_h) && show_col_dendro

  # Heatmap grid margins: add dendro panel space on the appropriate side.
  # When col dendro is on bottom, col labels migrate to the top so that
  # the dendro panel sits cleanly between the heatmap and the chart edge.
  col_labels_top <- dendro_cols_shown && dendro_col_side == "bottom"
  hm_left <- left_px +
    if (dendro_rows_shown && dendro_row_side == "left") dendro_row_width else 0L
  hm_right <- right_px +
    if (dendro_rows_shown && dendro_row_side == "right") {
      dendro_row_width
    } else {
      0L
    }
  hm_top <- top_px +
    (if (dendro_cols_shown && dendro_col_side == "top") {
      dendro_col_height
    } else {
      0L
    }) +
    (if (col_labels_top) bot_px else 0L)
  hm_bottom <- if (col_labels_top) {
    dendro_col_height
  } else {
    bot_px +
      if (dendro_cols_shown && dendro_col_side == "bottom") {
        dendro_col_height
      } else {
        0L
      }
  }

  if (is.null(square_cells)) {
    square_cells <- (n_rows == n_cols)
  }

  # For square-cell heatmaps, compute R-side dimensions as a static fallback
  # (e.g. knitr/quarto output where JS resize callbacks may not run).
  # The JS binding also enforces square cells dynamically on init and resize,
  # so these pixel values are used as the initial widget allocation.
  if (square_cells && is.null(width) && is.null(height)) {
    cell_px <- 40L
    width <- n_cols * cell_px + hm_left + hm_right
    height <- n_rows * cell_px + hm_top + hm_bottom
  }

  # -- 9. Tooltip: shows "row x col: value" -------------------------------------
  cols_json <- jsonlite::toJSON(cn, auto_unbox = FALSE)
  rows_json <- jsonlite::toJSON(rn, auto_unbox = FALSE)
  tooltip_fmt <- htmlwidgets::JS(paste0(
    "(function(){",
    "var cn=",
    cols_json,
    ";",
    "var rn=",
    rows_json,
    ";",
    "return function(p){",
    "if(!p.value||p.value[2]===null||p.value[2]===undefined)return'NA';",
    "return rn[p.value[1]]+' \u00d7 '+cn[p.value[0]]+': '+p.value[2].toFixed(",
    value_digits,
    ");",
    "}})()"
  ))

  # -- 10. Optional in-cell value labels -----------------------------------------
  label_opt <- if (show_values) {
    LabelOption(
      show = TRUE,
      formatter = htmlwidgets::JS(paste0(
        "function(p){",
        "if(!p.value||p.value[2]===null||p.value[2]===undefined)return'';",
        "return p.value[2].toFixed(",
        value_digits,
        ");",
        "}"
      ))
    )
  } else {
    NULL
  }

  # -- 11. Assemble multi-grid ECharts option ------------------------------------
  # Grid index assignments (depends on which dendro panels are shown):
  #   both:     grid[0]=row dendro, grid[1]=col dendro, grid[2]=heatmap
  #   row only: grid[0]=row dendro, grid[1]=heatmap
  #   col only: grid[0]=col dendro, grid[1]=heatmap
  #   neither:  grid[0]=heatmap (no gridIndex needed)

  dcolor <- dendro_color %||% "#99999988"

  if (!dendro_rows_shown && !dendro_cols_shown) {
    # ── No dendrograms: single grid, unchanged from original logic ───────────
    grids <- Grid(
      left = left_px,
      right = right_px,
      top = top_px,
      bottom = bot_px,
      contain_label = FALSE
    )
    x_axes <- Axis(
      type = "category",
      data = as.list(cn),
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE),
      axis_label = AxisLabel(rotate = rotate),
      boundary_gap = TRUE
    )
    y_axes <- Axis(
      type = "category",
      data = as.list(rn),
      inverse = TRUE,
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE),
      boundary_gap = TRUE
    )
    series_list <- list(HeatmapSeries(data = data_list, label = label_opt))
  } else {
    # ── One or two dendrogram panels: multiple grids ─────────────────────────
    # Compute dendrogram segment data (needed for custom series renderItem)
    row_dendro <- if (dendro_rows_shown) {
      hclust_to_dendro_data(row_h, uniform = dendro_uniform)
    } else {
      NULL
    }
    col_dendro <- if (dendro_cols_shown) {
      hclust_to_dendro_data(col_h, uniform = dendro_uniform)
    } else {
      NULL
    }

    # Determine ECharts axis/grid indices for the heatmap series
    hm_grid_idx <- if (dendro_rows_shown && dendro_cols_shown) 2L else 1L
    hm_x_ax_idx <- hm_grid_idx # xAxis array mirrors grid ordering
    hm_y_ax_idx <- hm_grid_idx # yAxis array mirrors grid ordering

    # Build the heatmap grid
    hm_grid <- Grid(
      left = hm_left,
      right = hm_right,
      top = hm_top,
      bottom = hm_bottom,
      contain_label = FALSE
    )

    # Axes for the heatmap grid (always last in their respective arrays).
    # When col labels migrate to the top (dendro_col_side = "bottom"), the
    # x-axis position is "top" so labels appear above the heatmap grid.
    hm_x_axis <- Axis(
      type = "category",
      data = as.list(cn),
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE),
      axis_label = AxisLabel(rotate = rotate),
      position = if (col_labels_top) "top" else NULL,
      boundary_gap = TRUE,
      grid_index = hm_grid_idx
    )
    hm_y_axis <- Axis(
      type = "category",
      data = as.list(rn),
      inverse = TRUE,
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE),
      boundary_gap = TRUE,
      grid_index = hm_grid_idx
    )

    # renderItem JS for row dendrogram (x-axis = height, y-axis = row position)
    # Each datum: [left_pos, right_pos, left_h, right_h, merge_h]
    # The U-shape: (left_h, lp) → (merge_h, lp) → (merge_h, rp) → (right_h, rp)
    row_render_js <- htmlwidgets::JS(paste0(
      "function(params,api){",
      "var lp=api.value(0),rp=api.value(1),",
      "lh=api.value(2),rh=api.value(3),mh=api.value(4);",
      "return{type:'polyline',",
      "shape:{points:[api.coord([lh,lp]),api.coord([mh,lp]),",
      "api.coord([mh,rp]),api.coord([rh,rp])]},",
      "style:{stroke:",
      jsonlite::toJSON(dcolor, auto_unbox = TRUE),
      ",lineWidth:1,fill:null}};}"
    ))

    # renderItem JS for col dendrogram (x-axis = col position, y-axis = height)
    # Each datum: [left_pos, right_pos, left_h, right_h, merge_h]
    # The U-shape: (lp, left_h) → (lp, merge_h) → (rp, merge_h) → (rp, right_h)
    col_render_js <- htmlwidgets::JS(paste0(
      "function(params,api){",
      "var lp=api.value(0),rp=api.value(1),",
      "lh=api.value(2),rh=api.value(3),mh=api.value(4);",
      "return{type:'polyline',",
      "shape:{points:[api.coord([lp,lh]),api.coord([lp,mh]),",
      "api.coord([rp,mh]),api.coord([rp,rh])]},",
      "style:{stroke:",
      jsonlite::toJSON(dcolor, auto_unbox = TRUE),
      ",lineWidth:1,fill:null}};}"
    ))

    # Build reusable dendro grids and position axes.
    #
    # Row dendro grid: placed on the left (root far-left, leaves adj. to heatmap)
    # or right (leaves adj. to heatmap, root far-right). The height x-axis is
    # inverse=TRUE for "left" so the root is on the far-left side.
    row_dendro_grid <- if (dendro_row_side == "right") {
      Grid(
        right = right_px,
        width = dendro_row_width,
        top = hm_top,
        bottom = hm_bottom
      )
    } else {
      Grid(
        left = 0L,
        width = dendro_row_width,
        top = hm_top,
        bottom = hm_bottom
      )
    }
    row_height_x_axis <- function(idx) {
      Axis(
        type = "value",
        inverse = (dendro_row_side == "left"),
        show = FALSE,
        min = dendro_axis_min(
          row_dendro[["min_height"]],
          row_dendro[["max_height"]]
        ),
        max = row_dendro[["max_height"]],
        grid_index = idx
      )
    }

    # Col dendro grid: placed on top (root far-top, leaves adj. to heatmap)
    # or bottom (leaves adj. to heatmap, root far-bottom). The height y-axis is
    # inverse=TRUE for "bottom" so the root is at the bottom.
    col_dendro_grid <- if (dendro_col_side == "top") {
      Grid(
        left = hm_left,
        right = hm_right,
        top = top_px,
        height = dendro_col_height
      )
    } else {
      Grid(
        left = hm_left,
        right = hm_right,
        bottom = 0L,
        height = dendro_col_height
      )
    }
    col_height_y_axis <- function(idx) {
      Axis(
        type = "value",
        show = FALSE,
        inverse = (dendro_col_side == "bottom"),
        min = dendro_axis_min(
          col_dendro[["min_height"]],
          col_dendro[["max_height"]]
        ),
        max = col_dendro[["max_height"]],
        grid_index = idx
      )
    }

    # Assemble grids, axes, and series lists depending on which panels are shown
    if (dendro_rows_shown && dendro_cols_shown) {
      # grid[0]=row dendro, grid[1]=col dendro, grid[2]=heatmap
      grids <- list(row_dendro_grid, col_dendro_grid, hm_grid)
      x_axes <- list(
        # [0] row dendro: height axis
        row_height_x_axis(0L),
        # [1] col dendro: col position axis (integer positions 0..n-1 align
        # with category cell centers in the heatmap grid)
        Axis(
          type = "value",
          min = -0.5,
          max = n_cols - 0.5,
          show = FALSE,
          grid_index = 1L
        ),
        hm_x_axis
      )
      y_axes <- list(
        # [0] row dendro: row position axis (integer positions 0..n-1 align
        # with category cell centers in the heatmap grid)
        Axis(
          type = "value",
          min = -0.5,
          max = n_rows - 0.5,
          inverse = TRUE,
          show = FALSE,
          grid_index = 0L
        ),
        # [1] col dendro: height axis
        col_height_y_axis(1L),
        hm_y_axis
      )
      series_list <- list(
        list(
          type = "custom",
          xAxisIndex = 0L,
          yAxisIndex = 0L,
          data = row_dendro[["data"]],
          silent = TRUE,
          animation = FALSE,
          renderItem = row_render_js
        ),
        list(
          type = "custom",
          xAxisIndex = 1L,
          yAxisIndex = 1L,
          data = col_dendro[["data"]],
          silent = TRUE,
          animation = FALSE,
          renderItem = col_render_js
        ),
        HeatmapSeries(
          data = data_list,
          label = label_opt,
          x_axis_index = hm_x_ax_idx,
          y_axis_index = hm_y_ax_idx
        )
      )
    } else if (dendro_rows_shown) {
      # grid[0]=row dendro, grid[1]=heatmap
      grids <- list(row_dendro_grid, hm_grid)
      x_axes <- list(row_height_x_axis(0L), hm_x_axis)
      y_axes <- list(
        Axis(
          type = "value",
          min = -0.5,
          max = n_rows - 0.5,
          inverse = TRUE,
          show = FALSE,
          grid_index = 0L
        ),
        hm_y_axis
      )
      series_list <- list(
        list(
          type = "custom",
          xAxisIndex = 0L,
          yAxisIndex = 0L,
          data = row_dendro[["data"]],
          silent = TRUE,
          animation = FALSE,
          renderItem = row_render_js
        ),
        HeatmapSeries(
          data = data_list,
          label = label_opt,
          x_axis_index = hm_x_ax_idx,
          y_axis_index = hm_y_ax_idx
        )
      )
    } else {
      # grid[0]=col dendro, grid[1]=heatmap
      grids <- list(col_dendro_grid, hm_grid)
      x_axes <- list(
        Axis(
          type = "value",
          min = -0.5,
          max = n_cols - 0.5,
          show = FALSE,
          grid_index = 0L
        ),
        hm_x_axis
      )
      y_axes <- list(col_height_y_axis(0L), hm_y_axis)
      series_list <- list(
        list(
          type = "custom",
          xAxisIndex = 0L,
          yAxisIndex = 0L,
          data = col_dendro[["data"]],
          silent = TRUE,
          animation = FALSE,
          renderItem = col_render_js
        ),
        HeatmapSeries(
          data = data_list,
          label = label_opt,
          x_axis_index = hm_x_ax_idx,
          y_axis_index = hm_y_ax_idx
        )
      )
    }
  }

  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title, left = hm_left) else NULL,
    tooltip = Tooltip(trigger = "item", formatter = tooltip_fmt),
    grid = grids,
    x_axis = x_axes,
    y_axis = y_axes,
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
      right = if (colorbar_orient == "vertical") "right" else NULL,
      top = if (colorbar_orient == "vertical") "middle" else NULL,
      left = if (colorbar_orient == "horizontal") "center" else NULL,
      bottom = if (colorbar_orient == "horizontal") "bottom" else NULL
    ),
    series = series_list
  )

  # Pass square-cell layout parameters to the JS binding so it can enforce
  # square cells dynamically on init and resize (viewer/browser resize events).
  # hm_left/hm_right/hm_top/hm_bottom are the actual heatmap grid margins so
  # squareCellHeight() measures only the heatmap grid, not the dendro panels.
  heatmap_meta <- list()
  if (square_cells) {
    heatmap_meta <- c(
      heatmap_meta,
      list(
        squareCells = TRUE,
        nRows = n_rows,
        nCols = n_cols,
        leftPx = hm_left,
        rightPx = hm_right,
        topPx = hm_top,
        botPx = hm_bottom
      )
    )
  }
  # Pass both theme-matched colour arrays so JS can select the right one.
  if (color_auto) {
    heatmap_meta <- c(
      heatmap_meta,
      list(
        colorLight = as.list(color_light),
        colorDark = as.list(color_dark)
      )
    )
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
