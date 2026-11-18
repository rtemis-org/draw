# draw.R
# htmlwidget binding and Tier 1 draw_* functions

# Constants ----
DEFAULT_MARGINS <- c(top = 36, right = 36, bottom = 36, left = 36)

# %% equal_axis_limits ----
#' Reconcile axis limits for a square, equally-scaled plot
#'
#' `equal_axes` and `square` are independent statements -- one about scale, one
#' about shape -- and each is satisfiable on its own. Asking for **both** is a
#' statement about the *limits*: a square box in which one data unit is the same
#' size on both axes is a box whose axes span the same interval. So that is what
#' this produces.
#'
#' @details
#' Which interval depends on what the caller already pinned down:
#'
#' - Neither limit given: one common interval over all the values, padded, so a
#'   true-versus-predicted plot gets its identity line at 45 degrees without the
#'   caller working out the range.
#' - One given: the other matches it. The caller stated the window; both axes
#'   use it.
#' - Both given, spanning different intervals: an error. The caller asked for
#'   two requests that cannot both hold, and overriding either without a message would
#'   discard a choice they made explicitly.
#'
#' With only one of the two flags set there is nothing to reconcile and the
#' limits pass through untouched.
#'
#' @param x,y Numeric: The values on each axis. `y` may be a list of series.
#' @param xlim,ylim Optional Numeric: Author-supplied limits, length 2.
#' @param pad Numeric `[0, Inf)`: Fraction of the range to pad a derived
#'   interval by.
#' @param square,equal_axes Logical: The two requests.
#'
#' @return Named list: `xlim` and `ylim`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
equal_axis_limits <- function(x, y, xlim, ylim, pad, square, equal_axes) {
  if (!square || !equal_axes) {
    return(list(xlim = xlim, ylim = ylim))
  }
  if (!is.null(xlim) && !is.null(ylim)) {
    if (!isTRUE(all.equal(as.numeric(xlim), as.numeric(ylim)))) {
      abort(
        "`square` and `equal_axes` together require both axes to span the ",
        "same interval, but `xlim` is [",
        paste(xlim, collapse = ", "),
        "] and `ylim` is [",
        paste(ylim, collapse = ", "),
        "]. Give them the same limits, or drop one of the two flags.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(list(xlim = xlim, ylim = ylim))
  }
  # One stated window governs both axes; with neither stated, the data does.
  common <- xlim %||% ylim
  if (is.null(common)) {
    values <- c(
      if (is.null(x)) NULL else as.numeric(x),
      if (is.list(y)) unlist(y, use.names = FALSE) else as.numeric(y)
    )
    # Nothing stated and nothing to measure: resolve() is called without data
    # too, and there is no interval to be had. Leave the limits unset.
    if (length(values) == 0L) {
      return(list(xlim = xlim, ylim = ylim))
    }
    common <- calc_limits(values, pad)
  }
  list(xlim = common, ylim = common)
} # /rtemis.draw::equal_axis_limits


# %% aspect_meta ----
#' Derive the `aspect` render hint from a built option
#'
#' The pixel geometry a square or equally-scaled plot needs, read off the option
#' that was built rather than recomputed: the resolved axis limits and the
#' grid margins are both already on it, and reading them is what keeps the hint
#' and the chart from disagreeing.
#'
#' The ratio is the whole point. `equal_axes` wants one data unit to occupy the
#' same number of pixels on both axes, which makes the grid's height-to-width
#' ratio the y range over the x range. `square` wants a ratio of 1. Asked for
#' together, `equal_axis_limits()` has already made the two spans equal, so the
#' `equal_axes` formula yields 1 and the two agree by construction.
#'
#' @param option [EChartsOption]: The built option.
#' @param square,equal_axes Logical: The two requests. With neither set there is
#'   no hint to give and the result is an empty list.
#'
#' @return Named list: `aspect`, or empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
aspect_meta <- function(option, square = FALSE, equal_axes = FALSE) {
  if (!square && !equal_axes) {
    return(list())
  }
  x_lim <- axis_span(option@x_axis, "x", square, equal_axes)
  y_lim <- axis_span(option@y_axis, "y", square, equal_axes)
  # `widthPx` is deliberately absent: with no preferred width the box fills the
  # container, and `width` on draw() is the one place to constrain it.
  list(
    aspect = c(
      list(ratio = if (equal_axes) diff(y_lim) / diff(x_lim) else 1),
      grid_px(option@grid)
    )
  )
} # /rtemis.draw::aspect_meta


# %% axis_span ----
#' Read one axis's resolved numeric span off a built option
#'
#' @param axis [Axis]: The axis.
#' @param which Character: `"x"` or `"y"`, for the error message.
#' @param square,equal_axes Logical: Which request is being served, so the error
#'   names the argument the caller passed.
#'
#' @return Numeric: Length-2 `c(min, max)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
axis_span <- function(axis, which, square, equal_axes) {
  span <- c(axis@min, axis@max)
  if (!is.numeric(span) || length(span) != 2L || !all(is.finite(span))) {
    abort(
      "`",
      if (square) "square" else "equal_axes",
      "` needs a numeric ",
      which,
      " axis with known limits, and this chart's is ",
      if (identical(axis@type, "category")) {
        "categorical"
      } else {
        "unbounded"
      },
      ". Plot numeric values, or set `",
      which,
      "lim`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  as.numeric(span)
} # /rtemis.draw::axis_span


# %% grid_px ----
#' Read a grid's four margins as pixel numbers
#'
#' The browser solves the box from the container width, which means it has to
#' know how much room to leave outside the grid. A margin ECharts is left to
#' choose, or one given as a percentage, is not a number this side knows -- so
#' rather than guess one, this says which side is the problem.
#'
#' @param grid Optional [Grid]: The option's grid.
#'
#' @return Named list: `leftPx`, `rightPx`, `topPx`, `botPx`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
grid_px <- function(grid) {
  sides <- list(
    leftPx = "left",
    rightPx = "right",
    topPx = "top",
    botPx = "bottom"
  )
  out <- lapply(names(sides), function(key) {
    value <- if (is.null(grid)) NULL else prop(grid, sides[[key]])
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      abort(
        "`square` and `equal_axes` need all four margins as pixel numbers, ",
        "and `",
        sides[[key]],
        "` is ",
        if (is.null(value)) "unset" else paste0("'", value, "'"),
        ". Pass `margins = c(top = , right = , bottom = , left = )` in pixels.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    as.numeric(value)
  })
  stats::setNames(out, names(sides))
} # /rtemis.draw::grid_px


#' Resolve a theme and create a backend widget
#'
#' Shared tail of every [draw()] method. Resolves the uniform `theme` argument
#' into the `autoTheme` / `theme` / `themeDark` payload fields that every backend
#' JS binding reads, applies the standard sizing policy, and creates the widget.
#' Backends differ only in the `name` (which JS binding) and the `payload` they
#' build; theme handling and sizing are identical, so they live here.
#'
#' @param name Character: htmlwidget name (`"rtemis-draw"`, `"rtemis-graph"`, ...).
#' @param payload Named list: Backend-specific widget payload. The theme fields
#'   are merged into this list.
#' @param theme Optional [Theme], list, or `NA`: Theme override; `NULL` (default)
#'   enables light/dark auto-detection.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit element ID.
#' @return htmlwidget: Widget object.
#' @keywords internal
#' @noRd
render_widget <- function(
  name,
  payload,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL
) {
  # Theme resolution is uniform across backends:
  #   NA   -> no theme (raw backend defaults)
  #   NULL -> auto-detect: send both light + dark, let JS pick by dark mode
  #   Theme / list -> an explicit theme
  auto_theme <- FALSE
  theme_list <- NULL
  theme_dark_list <- NULL
  if (identical(theme, NA)) {
    theme_list <- NULL
  } else if (is.null(theme)) {
    auto_theme <- TRUE
    theme_list <- to_list(theme_light())
    theme_dark_list <- to_list(theme_dark())
  } else if (S7::S7_inherits(theme, Theme)) {
    theme_list <- to_list(theme)
  } else if (is.list(theme)) {
    theme_list <- theme
  }

  # htmlwidgets serializes with `keep_vec_names = TRUE`, so a *named* palette
  # reaches the browser as a JSON object where ECharts expects an array, and the
  # chart renders with no colors. The palette is unnamed at every site that
  # builds one; this is the backstop for a Theme assembled some other way.
  theme_list[["color"]] <- palette_colors(theme_list[["color"]])
  theme_dark_list[["color"]] <- palette_colors(theme_dark_list[["color"]])

  payload <- c(
    payload,
    list(
      autoTheme = if (auto_theme) TRUE else NULL,
      theme = theme_list,
      themeDark = theme_dark_list
    )
  )

  # htmlwidgets stops filling the host once either dimension is set and sizes an
  # unset width at 960 pixels, so an unset width beside a set height follows the
  # container.
  if (is.null(width) && !is.null(height)) {
    width <- "100%"
  }

  # Respect explicit pixel dimensions: when a numeric width/height is given,
  # disable browser.fill/viewer.fill so the widget keeps the computed size
  # instead of expanding to fill the container.
  should_fill <- is.null(width) || is.character(width)

  htmlwidgets::createWidget(
    name = name,
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
    # `elementId` is htmlwidgets' own argument name and stays camelCase; this
    # package's is `element_id`. This is the only place the two meet.
    elementId = element_id
  )
}

#' Render a chart configuration or backend option as an htmlwidget
#'
#' `draw()` renders chart configurations and backend option objects as
#' interactive visualizations. It is an 'S7' generic that dispatches on the
#' class of `option`:
#'
#' - [ChartConfig] subclasses -> compile the configuration with its data, then
#'   render using the corresponding backend.
#' - [EChartsOption] -> 'ECharts' (`rtemis-draw` widget).
#' - [SigmaOption] -> 'Sigma.js' (`rtemis-graph` widget).
#' - [MapLibreOption] -> 'MapLibre' (`rtemis-map` widget).
#' - [PeleOption] -> 'Pele' (`rtemis-diagram` widget).
#'
#' A plain named list is treated as a raw 'ECharts' option.
#'
#' Theme handling is uniform across backends and lives on `draw()`, not on the
#' option object: pass `NULL` (default) for light/dark auto-detection (from VS
#' Code, RStudio, or the browser's `prefers-color-scheme`), `NA` for no theme
#' (raw backend defaults), or a [Theme] / list to force one.
#'
#' @param option [ChartConfig] subclass, [EChartsOption], [SigmaOption],
#'   [MapLibreOption], [PeleOption], or named list: Chart configuration or
#'   render spec to draw.
#'   All concrete chart configuration classes listed below are supported.
#' @param theme Optional [Theme], list, or `NA`: Theme override. `NULL` enables
#'   light/dark auto-detection; `NA` disables theming. Applied to every backend.
#' @param width Optional Character or Numeric: Widget width. Unset follows the
#'   width of the viewer, browser, or document column.
#' @param height Optional Character or Numeric: Widget height. With `width`
#'   also unset, the widget fills the viewer or browser window.
#' @param element_id Optional Character: Explicit element ID.
#' @param filename Optional Character: If provided, the widget is also written to
#'   this file via [save_drawing()]. Extension determines the format (currently
#'   only `.svg`).
#' @param ... Named arguments for the selected method. For [ChartConfig]
#'   subclasses, `data` supplies the data to plot; its required structure depends
#'   on the configuration class. If `data` is omitted or `NULL`, the config's
#'   `dat_path` is read instead.
#'
#'   Additional arguments are passed to the rendering backend. ECharts accepts
#'   `renderer` (Character \{"canvas", "svg"\}) and `meta` (named list of extra
#'   payload fields, used internally e.g. by [draw_heatmap()]). Any other name
#'   is an error rather than a silent no-op, so a typo cannot quietly change
#'   what is drawn.
#'
#'   Two `meta` fields are read by the ECharts JS binding and are part of its
#'   contract, because they need the container width, which only the browser
#'   knows:
#'
#'   - `squareCells`: see [draw_heatmap()].
#'   - `aspect`: a named list with `ratio` (required grid height / grid width),
#'     `widthPx` (preferred grid width; the box shrinks below this only when
#'     the container is too narrow), and `leftPx`, `rightPx`, `topPx`, `botPx`
#'     (padding to reserve outside the grid). The binding solves for the grid
#'     box on render and on resize, and sets the container height to match.
#'     Use it for charts whose *pixel* geometry carries meaning -- most often
#'     equal axis scaling, where one data unit must be the same number of
#'     pixels on both axes and `ratio` is therefore the y range over the
#'     x range. ECharts has no equal-aspect option, and R cannot know the
#'     container width, so neither side can enforce this alone. A widget that
#'     fills its host fits the box within both host dimensions.
#'
#'   One `meta` field is read by [save_drawing()]: `exportSize`, a named list
#'   with `width` and `height`, is the SVG size in pixels of a widget that
#'   follows its container.
#' @param animation Optional Logical: Whether ECharts animates the chart.
#'   `NULL` (default) leaves ECharts' own default in place. `FALSE` disables
#'   animation, which matters for charts with many points, where animating
#'   every interactive update is expensive. Ignored by the Sigma, MapLibre, and
#'   Pele backends, which have no ECharts animation to disable.
#' @return htmlwidget: Widget object.
#' @export
#'
#' @section Chart configurations:
#' Supported [ChartConfig] subclasses are [A3Config], [BarConfig],
#' [BoxplotConfig], [CalibrationConfig], [ChoroplethConfig], [ConfusionConfig],
#' [DensityConfig], [GanttConfig], [HeatmapConfig], [HistogramConfig],
#' [LineConfig], [MermaidConfig], [NetworkConfig], [PieConfig], [PRConfig],
#' [ROCConfig], [SankeyConfig],
#' [ScatterConfig], [Scatter3DConfig], [SignificanceConfig], [SpectrogramConfig],
#' [SurvivalConfig], and [TimeSeriesConfig]. Use their `setup_*()` functions to
#' create configurations, then pass the data to `draw()` with `data =` or set
#' `dat_path` on the configuration. See [compile()] to obtain the backend option
#' without rendering it.
#'
#' @examples
#' # Render a chart configuration with data.
#' config <- setup_ScatterConfig(x = "wt", y = "mpg")
#' draw(config, data = mtcars)
#'
#' # Render a backend option directly.
#' option <- EChartsOption(
#'   x_axis = Axis(type = "category", data = c("Mon", "Tue", "Wed")),
#'   y_axis = Axis(type = "value"),
#'   series = BarSeries(data = c(120, 200, 150))
#' )
#' draw(option)
draw <- S7::new_generic(
  "draw",
  "option",
  function(
    option,
    theme = NULL,
    width = NULL,
    height = NULL,
    element_id = NULL,
    filename = NULL,
    animation = NULL,
    ...
  ) {
    S7::S7_dispatch()
  }
)

#' Render an ECharts option (already a plain list) as an htmlwidget
#'
#' Internal worker shared by the [EChartsOption] and bare-list [draw()] methods.
#' Holds all ECharts-specific option post-processing (legend auto-add, empty-
#' object fix, axis tick hiding, grid outer-bounds), then hands off to
#' `render_widget()`.
#'
#' @param option Named list: ECharts option (already converted from S7).
#' @param renderer Character \{"canvas", "svg"\}: Rendering engine.
#' @param theme,width,height,element_id,filename,animation See [draw()].
#' @param meta Named list: Extra fields merged into the widget payload.
#' @return htmlwidget: Widget object.
#' @keywords internal
#' @noRd
draw_echarts_option <- function(
  option,
  renderer = "canvas",
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  meta = list()
) {
  # Animation is a *render* concern, not a fact about the chart: the same chart
  # animates in an IDE pane and does not in a web canvas redrawing 100k points
  # on every interaction. So it is set here, on the way out, rather than being a
  # config property. NULL leaves whatever the option already carries, which is
  # ECharts' own default.
  if (!is.null(animation)) {
    option[["animation"]] <- animation
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

  # Default grid: echarts 6's outerBoundsMode = "same" makes the grid rect
  # (left/right/top/bottom) the *outer* bounds — axis labels and names must fit
  # inside, so the grid auto-shrinks to contain them. This replaces the
  # deprecated `containLabel = TRUE` and yields tighter margins with no dead
  # space on the sides.
  #
  # When a user-supplied grid is already present (e.g. via `margins`), this still
  # injects `outerBoundsMode = "same"` so that unspecified sides auto-fit — but
  # skip when `containLabel` is set explicitly (heatmap uses `containLabel =
  # FALSE` for exact pixel margins).
  if (!is.null(option$xAxis) || !is.null(option$yAxis)) {
    # ECharts 6.1's `outerBoundsContain` decides whether a grid's outer bounds
    # reserve room for the axis *name* ("all") or only the tick labels
    # ("axisLabel"). Using "axisLabel" when no axis name is shown avoids a dead
    # gap below the labels. Mirrors rtemislive's chartOptionBuilder.ts.
    has_axis_name <- function(axis) {
      if (is.null(axis)) {
        return(FALSE)
      }
      axes <- if (is.null(names(axis))) axis else list(axis)
      any(vapply(
        axes,
        function(a) {
          nm <- a[["name"]]
          is.character(nm) && length(nm) == 1L && nzchar(nm)
        },
        logical(1L)
      ))
    }
    outer_contain <- if (
      has_axis_name(option$xAxis) || has_axis_name(option$yAxis)
    ) {
      "all"
    } else {
      "axisLabel"
    }
    inject_obm <- function(g) {
      if (is.null(g[["containLabel"]])) {
        if (is.null(g[["outerBoundsMode"]])) {
          g[["outerBoundsMode"]] <- "same"
        }
        if (is.null(g[["outerBoundsContain"]])) {
          g[["outerBoundsContain"]] <- outer_contain
        }
      }
      g
    }
    if (is.null(option$grid)) {
      option$grid <- inject_obm(list())
    } else if (is.list(option$grid) && !is.null(names(option$grid))) {
      # Single grid (named list).
      option$grid <- inject_obm(option$grid)
    } else if (is.list(option$grid)) {
      # List of grids.
      option$grid <- lapply(option$grid, inject_obm)
    }
  }

  payload <- c(
    list(
      option = option,
      renderer = renderer
    ),
    meta
  )

  widget <- render_widget(
    "rtemis-draw",
    payload,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id
  )

  if (!is.null(option[["grid3D"]])) {
    widget[["dependencies"]] <- c(
      widget[["dependencies"]],
      list(echarts_gl_dependency())
    )
  }
  if (!is.null(filename)) {
    save_drawing(widget, filename)
  }

  widget
}

# ECharts backend: convert the option object to a list, then render.
S7::method(draw, EChartsOption) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  renderer = "canvas",
  meta = list()
) {
  check_dots_empty(...)
  draw_echarts_option(
    to_list(option),
    renderer = renderer,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = meta
  )
}

# A bare named list is treated as a raw ECharts option (back-compat with the
# original list-accepting `draw()`).
S7::method(draw, S7::class_list) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  renderer = "canvas",
  meta = list()
) {
  check_dots_empty(...)
  draw_echarts_option(
    option,
    renderer = renderer,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = meta
  )
}

#' Shiny output for draw widget
#' @param outputId Character: Shiny output ID.
#' @param width Character or Numeric: CSS width.
#' @param height Character or Numeric: CSS height.
#'
#' @return Shiny output element for a draw widget.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   drawOutput("chart", height = "600px")
#' }
drawOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "rtemis-draw",
    width = width,
    height = height,
    package = "rtemis.draw"
  )
}

#' Shiny render function for draw widget
#' @param expr Expression: Expression that returns a draw widget.
#' @param env Environment: Evaluation environment.
#' @param quoted Logical: Whether `expr` is quoted.
#'
#' @return Shiny render function for a draw widget.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   renderDraw(draw_bar(x = c("A", "B", "C"), y = c(3, 7, 2)))
#' }
renderDraw <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, drawOutput, env, quoted = TRUE)
}

# -- Tier 1: draw_* convenience functions ---------------------------------------

#' Epoch milliseconds for a time axis, keeping the wall-clock reading
#'
#' ECharts places and labels a time axis by epoch milliseconds, formatted in
#' the browser's timezone unless the option sets `useUTC`. A chart built in
#' one timezone and viewed in another would then move every point: a date at
#' midnight, sent as the instant it names, reads as the previous evening on a
#' browser west of it. So the value sent is the instant whose *UTC* reading is
#' the timestamp's own wall-clock reading, and the option sets `useUTC`, which
#' together render the timestamp exactly as R prints it wherever it is viewed.
#'
#' A `Date` is a count of days from the epoch and needs no timezone at all.
#' A `POSIXct` is formatted in its own timezone and re-read as UTC.
#'
#' @param x Date, POSIXct, or Numeric: Time values, or milliseconds already.
#'
#' @return Numeric: Epoch milliseconds, `NA` where `x` is.
#'
#' @author EDG
#' @keywords internal
#' @noRd
time_axis_ms <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  }
  if (inherits(x, "POSIXt")) {
    wall <- format(x, "%Y-%m-%d %H:%M:%OS6")
    utc <- as.POSIXct(wall, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
    return(round(as.numeric(utc) * 1000))
  }
  as.numeric(x)
} # /rtemis.draw::time_axis_ms


#' Is a vector a time for a line chart's x axis?
#'
#' @param x Vector: The candidate.
#'
#' @return Logical: TRUE for `Date` and `POSIXt`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_time_axis <- function(x) {
  inherits(x, c("Date", "POSIXt"))
} # /rtemis.draw::is_time_axis


#' Tooltip value formatter for plain numbers
#'
#' Formats the value part of a tooltip row while leaving ECharts' own layout
#' -- axis label header, series marker and name, right-aligned value -- in
#' place. Integers print as they are; other numbers with three significant
#' digits, keeping every digit of the integer part, or in exponent form below
#' 1e-3 or from 1e15; anything that is not a finite number passes through.
#' The same rule as `@rtemis/draw`'s `formatNumber()`, which formats the
#' tooltips of the charts it builds.
#'
#' @return `JS` function: For `Tooltip(value_formatter = )`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
number_value_formatter <- function() {
  htmlwidgets::JS(
    "function (v) {
      if (typeof v !== 'number' || !isFinite(v)) return v;
      if (Number.isInteger(v)) return String(v);
      var a = Math.abs(v);
      if (a < 1e-3 || a >= 1e15) return v.toExponential(2);
      var r = Number(v.toPrecision(3));
      var d = Math.max(0, 2 - Math.floor(Math.log10(Math.abs(r))));
      return r.toFixed(d);
    }"
  )
} # /rtemis.draw::number_value_formatter


#' Draw a Line Chart
#'
#' Line chart from x/y data.
#'
#' @param legend_position Character \{"top", "bottom", "left", "right",
#'   "top-left", "top-right", "bottom-left", "bottom-right"\}: Legend anchor.
#'   Top/bottom anchors use horizontal rows; left/right anchors use a vertical
#'   column. Corner anchors align within the top or bottom row.
#' @param legend_placement Character \{"outside", "inside"\}: Relation to the
#'   plotting area. Outside placement reserves space for the complete legend;
#'   inside placement overlays the data. Neither setting adds a missing legend.
#' @param legend_title Optional Character: Title drawn with the legend or
#'   colorbar: above left/right legends and vertical colorbars, before the
#'   first row of top/bottom legends and horizontal colorbars. `NULL` draws no
#'   title. A title does not add a missing legend.
#'
#' @details
#' Axis baselines emphasize zero only when it is in the visible orthogonal
#' range. Categorical x axes follow the same rule as numeric and time axes.
#'
#' # Square and equally-scaled plots
#'
#' `square` and `equal_axes` are separate requests. `square` is about the
#' *shape* of the plotting box: equal height and width in pixels, measured on
#' the box itself, not including axis labels or margins. `equal_axes` is about
#' *scale*: one data unit occupies the same number of pixels on both axes, so a
#' 45-degree line is a slope of 1.
#'
#' Either works alone. `equal_axes` on a chart whose x spans ten units and whose
#' y spans one gives a wide, short box, which is what equal scaling means there.
#'
#' Asking for both is a statement about the limits, since the only square box in
#' which both axes are equally scaled is one whose axes span the same interval.
#' So both axes are made to span one interval: over all the values when neither
#' limit was given, or the given one when exactly one was given. Giving both
#' `xlim` and `ylim` as different intervals is an error rather than a silent
#' override.
#'
#' The box itself is solved in the browser, which is the only side that knows
#' how wide the container is, and re-solved whenever it resizes.
#'
#' @param x Vector: X-axis values. Numeric values get a value axis. `Date`
#'   and `POSIXct` values get a time axis: points are spaced by elapsed time
#'   and the labels are chosen adaptively for the span shown (years, then
#'   months, then days, down to seconds), and they read as the timestamps do
#'   in R whatever timezone the chart is viewed in. Anything else gets a
#'   category axis with one slot per distinct value: a factor's levels that
#'   occur, in level order, or other values in order of appearance.
#' @param y Numeric or named list: Y values.
#' @param names Optional Character: Series names used when `y` is an unnamed list.
#' @param group Optional Atomic vector or single-column data frame: Grouping
#'   variable, one value per point. Splits each `y` series into one line per
#'   level, as [draw_scatter()] does for points: named by the level for a
#'   single `y`, and `"<series> / <level>"` for several. Cannot be combined
#'   with `blocks`. Points whose group is `NA` are dropped. Lines and legend
#'   entries follow factor level order, or first appearance for other vectors.
#' @inheritParams LineConfig aggregate
#' @param smooth Logical: Whether to smooth lines.
#' @param area Logical: Fill the area under each line. The y limits, unless
#'   given, then hold zero, padded only away from it.
#' @param points Logical: Show a marker at each data value. Without markers, a
#'   value with no neighbor to join keeps its marker.
#' @param stack Logical: Stack the lines, each on the sum of those before it at
#'   the same x. Requires `area`. A line has no point where a line below it
#'   has no value, since the sum there is unknown.
#' @param blocks Optional factor, integer, character, or logical of length
#'   `length(x)`: Per-x-value group label for the shaded vertical background
#'   bands. Contiguous runs of the same level become one band. `NA` entries
#'   produce no band.
#' @param block_color Optional Character vector or list of length k, where
#'   k is the number of unique levels in `blocks`: Fill color for each level.
#'   Match by name to factor levels if named, else positional. `NA`, `NULL`,
#'   or `"transparent"` entries skip that level.
#' @param block_opacity Numeric \[0, 1\]: Fill opacity applied to all bands.
#'   Defaults to `0.2`.
#' @param palette Optional Character: Series color palette — a single color string or
#'   character vector that overrides the theme palette for this chart.
#'   `color` takes precedence over the theme palette (it sets `option.color`).
#' @param line_style Optional Character \{"solid", "dashed", "dotted"\}: Line dash
#'   style — one value per series, recycled to match the number of series.
#' @param xlim Optional Numeric, Date, or POSIXct \[length 2\]: X-axis limits
#'   `c(min, max)`. Only supported when `x` is numeric or a time; passing
#'   `xlim` with a categorical `x` errors. On a time axis, give it in the same
#'   class as `x` (or as epoch milliseconds). Defaults to the range of `x`
#'   extended by `pad`.
#' @param ylim Optional Numeric \[length 2\]: Y-axis limits `c(min, max)`.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range to extend each
#'   axis by when `xlim` / `ylim` are not given. The default matches base R's
#'   `xaxs = "r"`, which extends the range by 4% at each end.
#'   Defaults to the range of all `y` values across series (no padding).
#' @param square Logical: If TRUE, draw the plotting box square -- equal height
#'   and width in pixels, excluding axis labels and margins.
#' @param equal_axes Logical: If TRUE, give one data unit the same size in
#'   pixels on both axes. Set both for a plot that is square *and* to scale,
#'   such as an ROC curve; the two axes are then made to span the same
#'   interval. Requires a numeric `x`. See Details.
#' @param xlab Optional Character: X-axis title. `""` draws no title.
#' @param ylab Optional Character: Y-axis title. `""` draws no title.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `color` argument.
#' @param zoom Logical: Enable x-axis zoom. `TRUE` adds a slider below the
#'   axis title, Shift+wheel zoom, and drag panning on the x-axis;
#'   double-click resets the view.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`, e.g.
#'   `c(left = 80, right = 20)`. With none given, every side is 36 pixels;
#'   otherwise only the sides given are fixed. The grid's outer bounds hold the
#'   axis labels and names.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @param fit Optional Character: Model fitted to each line and drawn as a
#'   dashed curve: `"glm"`, `"polynomial"`, `"loess"`, `"gam"`, or an rtemis
#'   learner name, as in [draw_scatter()]. Requires a numeric `x`, `aggregate =
#'   "none"`, and `area = FALSE`. `NULL` draws no fit.
#' @inheritParams draw_scatter fit_params fit_per_group se se_times rsq n_fit fit_alpha fit_name
#' @return htmlwidget: Widget object.
#'
#' @examples
#' # Monthly generation by source (synthetic; see ?power_generation).
#' draw_line(
#'   power_generation$month,
#'   power_generation$gwh,
#'   group = power_generation$source,
#'   ylab = "Generation (GWh)"
#' )
#'
#' # An ROC curve is square by nature, on axes that share their [0, 1] range.
#' draw_line(
#'   x = c(0, 0.2, 0.6, 1),
#'   y = c(0, 0.55, 0.85, 1),
#'   points = TRUE,
#'   square = TRUE,
#'   equal_axes = TRUE,
#'   xlab = "False positive rate",
#'   ylab = "True positive rate"
#' )
#'
#' @export
draw_line <- function(
  x,
  y,
  names = NULL,
  group = NULL,
  aggregate = "none",
  smooth = FALSE,
  area = FALSE,
  points = FALSE,
  stack = FALSE,
  blocks = NULL,
  block_color = NULL,
  block_opacity = 0.2,
  palette = NULL,
  line_style = NULL,
  pad = DEFAULT_PAD,
  xlim = NULL,
  ylim = NULL,
  square = FALSE,
  equal_axes = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  zoom = FALSE,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  fit = NULL,
  fit_params = NULL,
  fit_per_group = TRUE,
  se = TRUE,
  se_times = 1.96,
  rsq = FALSE,
  n_fit = 200L,
  fit_alpha = 0.25,
  fit_name = NULL
) {
  check_logical_scalar(points)
  check_zoom_flag(zoom)
  check_prob_scalar(block_opacity)
  margin <- pixel_margins(margins)
  values <- if (is.list(y)) y else list(y = y)
  if (is.null(names(values))) {
    names(values) <- names %||% paste0("Series ", seq_along(values))
  }
  for (name in names(values)) {
    if (length(values[[name]]) != length(x)) {
      abort(
        "`y` must have the same length as `x` (",
        length(x),
        "); '",
        name,
        "' has ",
        length(values[[name]]),
        ".",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
  }
  if (is.list(block_color)) {
    block_color <- vapply(
      block_color,
      function(v) if (is.null(v)) NA_character_ else as.character(v),
      character(1)
    )
  }
  # A config holds no missing values; "transparent" also draws no band.
  if (!is.null(block_color)) {
    block_color[is.na(block_color)] <- "transparent"
  }
  data <- c(list(.x = x), values)
  if (!is.null(group)) {
    data[[".group"]] <- group_values(group, length(x))
  }
  if (!is.null(blocks)) {
    if (length(blocks) != length(x)) {
      abort(
        "`blocks` must have the same length as `x` (",
        length(x),
        "); got ",
        length(blocks),
        ".",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    if (is.null(block_color)) {
      abort(
        "`block_color` must be provided when `blocks` is set.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    data[[".blocks"]] <- blocks
    if (!is.null(names(block_color))) {
      block_color <- block_colors_by_level(blocks, block_color)
    }
  }
  # On a time axis, positions are epoch milliseconds (see time_axis_ms()), and
  # `xlim` may be given as dates, date-times, or milliseconds.
  if (is_time_axis(x) && !is.null(xlim)) {
    xlim <- time_axis_ms(xlim)
  }
  validate_axis_lim(xlim, "xlim")
  validate_axis_lim(ylim, "ylim")
  config <- setup_LineConfig(
    x = ".x",
    y = names(values),
    group = if (!is.null(group)) ".group",
    aggregate = aggregate,
    blocks = if (!is.null(blocks)) ".blocks",
    smooth = smooth,
    area = area,
    points = points,
    stack = stack,
    zoom = zoom,
    palette = palette,
    line_style = line_style,
    block_color = block_color,
    block_opacity = block_opacity,
    pad = pad,
    square = square,
    equal_axes = equal_axes,
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    fit = fit,
    fit_params = fit_params,
    fit_per_group = fit_per_group,
    se = se,
    se_times = se_times,
    rsq = rsq,
    n_fit = n_fit,
    fit_alpha = fit_alpha,
    fit_name = fit_name
  )
  # resolve() names the axes after the bound columns, which here are internal,
  # so the request carries only the limits it derives; the axes are named only
  # when `xlab` or `ylab` is given.
  data <- line_rows(config, data)
  resolved <- resolve(config, data = data)
  config@xlim <- resolved@xlim
  config@ylim <- resolved@ylim
  draw(
    BuildRequest(
      type = "line",
      data = tabular_json(
        data,
        names(data),
        fits = fit_curves(config, as.data.frame(data, check.names = FALSE))
      ),
      config = config_members(config),
      appearance = list()
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
}


# %% check_zoom_flag ----
#' Check a `zoom` argument of a chart built by @rtemis/draw
#'
#' @param zoom The argument.
#'
#' @return `NULL`, invisibly; an error unless `zoom` is TRUE or FALSE.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_zoom_flag <- function(zoom) {
  if (!isTRUE(zoom) && !isFALSE(zoom)) {
    abort(
      "`zoom` takes TRUE or FALSE.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.draw::check_zoom_flag


# %% pixel_margins ----
#' Parse a `margins` argument into whole pixels per side
#'
#' The `margin_*` members of a config built by @rtemis/draw hold whole pixels.
#'
#' @param margins The `margins` argument, as `parse_margins()` reads it.
#'
#' @return Named list: `top`, `right`, `bottom`, `left`, each an integer or
#'   `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pixel_margins <- function(margins) {
  margin <- parse_margins(margins)
  for (side in names(margin)) {
    if (!is.null(margin[[side]]) && !is.numeric(margin[[side]])) {
      abort(
        "`margins` takes pixels; '",
        side,
        "' is not a number.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  lapply(margin, function(v) if (is.null(v)) NULL else as.integer(round(v)))
} # /rtemis.draw::pixel_margins


# %% block_colors_by_level ----
#' Band colors named by level, in the level order the builder reads
#'
#' The line builder matches `block_color` to the levels of `blocks` by
#' position: the factor levels that occur, or the sorted distinct values.
#'
#' @param blocks Atomic vector: The `blocks` column.
#' @param block_color Named character: Colors named by level.
#'
#' @return Character: Colors in level order, `NA` for no band.
#'
#' @author EDG
#' @keywords internal
#' @noRd
block_colors_by_level <- function(blocks, block_color) {
  levels <- if (is.factor(blocks)) {
    levels(droplevels(blocks))
  } else {
    as.character(sort(unique(blocks[!is.na(blocks)])))
  }
  missing <- setdiff(levels, names(block_color))
  if (length(missing) > 0L) {
    abort(
      "`block_color` is missing entries for: ",
      paste(missing, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  unname(block_color[levels])
} # /rtemis.draw::block_colors_by_level


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

#' Axis line configuration emphasizing zero only when zero is visible
#'
#' Returns an [AxisLine] with `on_zero = TRUE` (so the axis sits at 0 of the
#' orthogonal axis when possible) and `show` gated on whether 0 is inside the
#' orthogonal range:
#'   - `show = TRUE` when `0 ∈ ortho_lim` — the axis line is drawn at the
#'     zero position, giving the "bold zero line" effect;
#'   - `show = FALSE` otherwise — echarts would otherwise fall back to the
#'     lowest orthogonal value and draw an unwanted emphasis line at the plot
#'     edge, which `show = FALSE` suppresses.
#'
#' Returns `NULL` when `ortho_lim` is `NULL` (e.g. category orthogonal axis) to
#' leave echarts' default behavior in place.
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
  zero_in_range <- ortho_lim[[1L]] <= 0 && ortho_lim[[2L]] >= 0
  AxisLine(show = zero_in_range, on_zero = TRUE)
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
    abort(
      "`blocks` must have the same length as `x` (",
      length(x),
      "); got ",
      length(blocks),
      ".",
      class = c("rtemis_dim_error", "rtemis_input_error")
    )
  }
  if (!is.atomic(blocks)) {
    abort(
      "`blocks` must be an atomic vector (factor, integer, character, or logical).",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(block_color)) {
    abort(
      "`block_color` must be provided when `blocks` is set.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_prob_scalar(block_opacity)

  # Determine level order so positional matching is stable. For factors, drop
  # unused levels so `block_color` doesn't need entries for absent categories.
  levels_vec <- if (is.factor(blocks)) {
    levels(droplevels(blocks))
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
      abort(
        "`block_color` is missing entries for: ",
        paste(missing_levels, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    color_for <- function(lvl) block_color[[as_key(lvl)]]
  } else {
    if (length(block_color) != k) {
      abort(
        "`block_color` must have length ",
        k,
        " (one per level of `blocks`).",
        class = c("rtemis_dim_error", "rtemis_input_error")
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
  # extending would overlap — keep the per-run endpoints there.
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


#' Draw a Bar Chart
#'
#' Bar chart from x/y data.
#'
#' @param x Character or Numeric: Category labels, or values when y is omitted.
#'   A matrix or two-dimensional table supplies categories in rows and series
#'   in columns. A one-dimensional table supplies counts and category names.
#' @inheritParams BarConfig order
#' @param y Numeric or named list: Bar heights.
#' @param palette Optional Character: Bar color or colors. For multiple series,
#'   colors are applied per series and recycled as needed. For a single series,
#'   a single color styles the whole series; multiple colors are recycled
#'   across individual bars. `palette` takes precedence over the theme palette.
#' @param stack Logical: Whether to stack bars.
#' @param horizontal Logical: Whether to draw horizontal bars.
#' @inheritParams BarConfig
#' @param xlab Optional Character: X-axis title (bottom axis). `""` draws no
#'   title.
#' @param ylab Optional Character: Y-axis title (left axis). `""` draws no
#'   title.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. The palette inside the theme can be
#'   overridden per-chart with the `palette` argument.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`. See
#'   [draw_line()].
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @param zoom Logical: Enable zoom on the category axis (the y-axis when
#'   `horizontal = TRUE`). `TRUE` adds a slider, Shift+wheel zoom, and drag
#'   panning; double-click resets the view.
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' draw_bar(x = c("A", "B", "C"), y = c(3, 7, 2), title = "Counts")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_bar <- function(
  x,
  y = NULL,
  palette = NULL,
  stack = FALSE,
  horizontal = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  bar_width = NULL,
  order = "input",
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  zoom = FALSE
) {
  check_zoom_flag(zoom)
  margin <- pixel_margins(margins)
  # The categories and value series, in `order`, as columns: `.category` and
  # one column per series. The request is built without `resolve()`, which
  # would name the axes after these columns, so the axes are named only when
  # `xlab` or `ylab` is given.
  input <- bar_input(x, y, order)
  values <- if (is.list(input[["y"]])) {
    input[["y"]]
  } else {
    list(.value = input[["y"]])
  }
  data <- c(list(.category = input[["x"]]), values)
  if (!is.list(input[["y"]]) && length(palette) > 1L) {
    palette <- rep_len(palette, length(input[["x"]]))[input[["index"]]]
  }
  config <- setup_BarConfig(
    x = ".category",
    y = names(values),
    stack = stack,
    horizontal = horizontal,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    bar_width = bar_width,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    zoom = zoom
  )
  draw(
    BuildRequest(
      type = "bar",
      data = tabular_json(data, names(data)),
      config = config_members(config),
      appearance = list()
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
}


#' Draw a Pie Chart
#'
#' Pie chart from values and labels, drawn by the `@rtemis/draw` pie builder.
#' Values that share a label sum into one slice, in the factor level order of
#' `labels`, otherwise in order of first appearance.
#'
#' @param values Numeric or data frame: Slice values, or a two-column table
#'   containing labels followed by values. Values must be finite and nonnegative
#'   with a positive total.
#' @param label_format Character `{"name", "value", "percent", "name_value", "name_percent"}`: Slice label content.
#' @param percent_digits Integer `[0, 6]`: Percentage precision.
#' @param show_labels Logical: Whether each slice carries its label.
#' @param labels Optional Character: Slice labels; names(values) when omitted.
#' @param radius Numeric or Character: Outer radius, or the inner and outer
#'   radius of a donut, in pixels or as a CSS percentage.
#' @param rose_type Optional Character \{"radius", "area"\}: Nightingale chart type.
#' @param palette Optional Character: Slice colors, in slice order, overriding
#'   the theme palette for this chart.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' draw_pie(
#'   values = c(1048, 735, 580),
#'   labels = c("Chrome", "Firefox", "Safari")
#' )
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_pie <- function(
  values,
  labels = NULL,
  radius = "75%",
  rose_type = NULL,
  palette = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  label_format = "name",
  percent_digits = 1L,
  show_labels = TRUE
) {
  input <- pie_input(values, labels)
  if (
    !(is.character(radius) || is.numeric(radius)) ||
      !length(radius) %in% 1:2 ||
      anyNA(radius)
  ) {
    abort(
      "Supply `radius` as one outer radius, or an inner and an outer radius.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  radius <- as.character(radius)
  config <- setup_PieConfig(
    values = ".value",
    labels = ".label",
    radius = radius[[length(radius)]],
    inner_radius = if (length(radius) == 2L) radius[[1L]],
    rose_type = rose_type,
    label_format = label_format,
    percent_digits = percent_digits,
    show_labels = show_labels,
    palette = palette,
    title = title,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    config,
    data = data.frame(
      .label = input[["labels"]],
      .value = input[["values"]],
      check.names = FALSE
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_pie

#' Draw a Density Plot
#'
#' Kernel density estimation plot from numeric data, with optional grouping
#' for multiple traces. A list input creates one density trace per vector when
#' ungrouped, or one trace per variable/group combination when `group` is
#' supplied. Curves use [stats::density()] with its bandwidth-scaled kernels
#' and evaluation range extending three bandwidths beyond the data. Empty
#' samples retain their identity without a curve. Singleton and constant
#' samples require an explicit bandwidth; no spread is inferred for them.
#' Missing groups are excluded, and infinite/nonnumeric observations rejected.
#'
#' @param mode Character `{"overlap", "ridge"}`: Overlay samples or align rows
#'   on common x and y scales. Ridge rows use sample names in place of a legend.
#'   Increase figure height for many rows.
#' @param order Character `{"input", "mean", "median"}`: Sample order. Summary
#'   orders are decreasing, retain ties in input order, and place empty samples last.
#' @param x Numeric or list: Values used for density estimation. An ungrouped
#'   list creates one density trace per element; with `group`, each list
#'   element is split by group into separate traces.
#' @param group Optional Atomic vector or single-column data frame: Grouping
#'   variable for multiple density traces. Traces, colors, and legend entries
#'   follow factor level order, or first appearance for other vectors.
#' @param n Integer `[2, Inf)`: Number of density evaluation points.
#' @param bw Character or Numeric: Bandwidth selector (`"nrd0"`, `"nrd"`,
#'   `"ucv"`, `"bcv"`, `"SJ"`, `"SJ-ste"`, `"SJ-dpi"`). Numeric input is
#'   normalized to `bandwidth` by the setup function.
#' @param bandwidth Optional Numeric `(0, Inf)`: Explicit finite smoothing
#'   bandwidth in measurement units, overriding the selector.
#' @param kernel Character: One of `"gaussian"`, `"epanechnikov"`, `"rectangular"`,
#'   `"triangular"`, `"biweight"`, `"cosine"`, or `"optcosine"`.
#' @param adjust Numeric `(0, Inf)`: Finite bandwidth multiplier.
#' @param fill_alpha Numeric `[0, 1]`: Distribution fill opacity.
#' @param na_rm Logical: Whether to remove `NA` values before
#'   computing densities.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab Optional Character: X-axis title. `""` draws no title.
#' @param ylab Optional Character: Y-axis title. `NULL` names it "Density",
#'   and `""` draws no title.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`. See
#'   [draw_line()].
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param verbosity Integer `[0, Inf)`: Verbosity level for removed-`NA` messages.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @param zoom Logical: Enable x-axis zoom: a slider below the axis title,
#'   Shift+wheel zoom, and drag panning. Ridge rows zoom together without a
#'   slider. Curves crossing the window edge stay whole, clipped by the plot.
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' draw_density(iris[["Sepal.Length"]], xlab = "Sepal length")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_density <- function(
  x,
  group = NULL,
  n = 512,
  bw = "nrd0",
  na_rm = TRUE,
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  verbosity = 1L,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = .25,
  mode = "overlap",
  order = "input",
  zoom = FALSE
) {
  opt <- density_option(
    x = x,
    group = group,
    n = n,
    bw = bw,
    na_rm = na_rm,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margins = margins,
    verbosity = verbosity,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    mode = mode,
    order = order,
    zoom = zoom,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )

  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
}

#' Draw a Histogram
#'
#' Histogram from numeric data, with optional grouping for multiple traces.
#' Bins are computed using [graphics::hist()] with consistent break points
#' across groups. Rectangles span their actual numeric intervals, including
#' unequal widths. Grouped samples overlap with translucent fills. Intervals
#' are right-closed, with the lowest edge included, using `hist()` boundary
#' tolerance. Missing groups are excluded; empty samples have zero counts.
#'
#' Normalization is within each sample: `"count"` is the raw count;
#' `"probability"` and `"percent"` divide by sample size (and multiply by 100
#' for percent). `"density"` divides by sample size and bin width, so total
#' area is one. `"count_density"` divides only by width, so area is sample size.
#' Unequal-width bins require one of the two density scales.
#'
#' With `density = TRUE`, each curve uses the same settings as [draw_density()]
#' and is scaled to the chosen histogram units. Count, probability, and percent
#' curves multiply the estimated density by the common bin width and the
#' appropriate sample-size factor. Density/count-density curves need no width
#' factor. The histogram and curve share a color and legend toggle.
#'
#' @param x Numeric or List: Values or named numeric vectors to bin.
#' @inheritParams draw_density mode order
#' @param group Optional Atomic vector or single-column data frame: Grouping
#'   variable for multiple series. Series, colors, and legend entries follow
#'   factor level order, or first appearance for other vectors.
#' @param breaks Numeric, Character, or Numeric vector: Binning method. A single number (number of bins), a character
#'   string naming an algorithm (e.g. `"Sturges"`, `"Scott"`, `"FD"`), or a
#'   numeric vector of break points. Numeric forms are normalized to `bins` or
#'   `bin_edges` by the setup function. Bin counts are suggestions to `hist()`.
#' @param bins Optional Integer `[1, 1000000]`: Suggested bin count.
#' @param bin_edges Optional Numeric vector: Finite, strictly increasing edges
#'   spanning every retained observation. Overrides `breaks`; excludes `bins`.
#' @param normalization Character: `"count"`, `"probability"`, `"percent"`,
#'   `"density"`, or `"count_density"`.
#' @param bin_stat Character `{"count", "sum", "mean", "min", "max"}`: Statistic of
#'   x observations in each bin. Empty bins have height zero. Non-count
#'   statistics require normalization = "count" and density = FALSE.
#' @param bar_mode Character `{"overlay", "group", "stack"}`: Histogram group layout.
#'   Stacks accumulate positive and negative values separately. Group/stack
#'   layouts require mode = "overlap" and density = FALSE.
#' @param density Logical: Overlay a kernel density in histogram units.
#' @param fill_alpha Optional Numeric `[0, 1]`: Bar fill opacity. `NULL` uses
#'   0.5 when overlaid groups overlap (`bar_mode = "overlay"` with more than
#'   one nonempty group outside ridge mode) and 0.75 otherwise.
#' @param border_alpha Numeric `[0, 1]`: Opacity of the bar borders, drawn in
#'   the series color. 0 draws no border.
#' @inheritParams draw_density n bw bandwidth kernel adjust na_rm verbosity
#' @param zoom Logical: Enable x-axis zoom: a slider below the axis title,
#'   Shift+wheel zoom, and drag panning. Ridge rows zoom together without a
#'   slider. Bins crossing the window edge stay whole, clipped by the plot.
#' @param palette Optional Character: Series colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @param xlab Optional Character: X-axis title. `""` draws no title.
#' @param ylab Optional Character: Y-axis title. `NULL` names it after a
#'   bar's height: the normalization ("Count", "Percent", ...) or the bin
#'   statistic of the column ("Mean X"). `""` draws no title.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`. See
#'   [draw_line()].
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' draw_histogram(iris[["Sepal.Length"]], xlab = "Sepal length")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_histogram <- function(
  x,
  group = NULL,
  breaks = "Sturges",
  palette = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  bins = NULL,
  bin_edges = NULL,
  normalization = "count",
  density = FALSE,
  n = 512L,
  bw = "nrd0",
  bandwidth = NULL,
  kernel = "gaussian",
  adjust = 1,
  fill_alpha = NULL,
  border_alpha = 1,
  na_rm = TRUE,
  verbosity = 1L,
  mode = "overlap",
  order = "input",
  bin_stat = "count",
  bar_mode = "overlay",
  zoom = FALSE
) {
  opt <- histogram_option(
    x = x,
    group = group,
    breaks = breaks,
    palette = palette,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margins = margins,
    bins = bins,
    bin_edges = bin_edges,
    normalization = normalization,
    density = density,
    n = n,
    bw = bw,
    bandwidth = bandwidth,
    kernel = kernel,
    adjust = adjust,
    fill_alpha = fill_alpha,
    border_alpha = border_alpha,
    na_rm = na_rm,
    verbosity = verbosity,
    mode = mode,
    order = order,
    bin_stat = bin_stat,
    bar_mode = bar_mode,
    zoom = zoom
  )

  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
}

# -- draw_heatmap ---------------------------------------------------------------

#' Draw a Heatmap
#'
#' Heatmap from a numeric matrix. Designed to handle all common use cases
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
#'   lines. `NULL` (default) uses a semi-transparent gray (`"#99999988"`),
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
#'   cells are square in the browser and SVG exports. In panels and exports,
#'   the matrix and dendrograms fit within the allocated canvas.
#'   `NULL` (default) enables this automatically for square
#'   matrices (e.g. correlation matrices). When `TRUE`, both `width` and `height`
#'   are calculated from the number of cells; supply explicit `width`/`height` to
#'   override.
#' @param colormap Optional Character: Color palette — a vector of 2 or more colors
#'   defining the continuous color scale from `zlim[1]` to `zlim[2]`. When `NULL`
#'   (default) a diverging negative-background-positive palette in the theme's
#'   sign colors is used when data spans zero (with the theme's background
#'   pinned exactly at 0, even for asymmetric ranges), otherwise the half of
#'   that scale matching the sign of the data: negative to background when no
#'   value is positive, background to positive when no value is negative. Two
#'   variants (light / dark) are computed automatically and the JS binding
#'   selects the correct one based on the active theme.
#' @param zlim Optional Numeric: Length-2 vector `c(min, max)` for the color scale.
#'   Defaults to the observed data range. For correlation matrices, `c(-1, 1)` is
#'   recommended.
#' @param row_tree,col_tree Optional hclust, dendrogram, or List: Supplied trees.
#'   A plain tree record contains `merge`, `height`, `order` and optional `labels`.
#'   Labeled leaves match original matrix dimnames; unlabeled trees are positional.
#'   Supplied trees determine order and take precedence over computed clustering.
#'   Triangle masking is unavailable with supplied trees.
#' @param cell_notes Optional Character matrix: Literal cell notes for hover.
#'   Named dimensions align to original matrix identities; otherwise positional.
#' @param row_colors,col_colors Optional Character vector, matrix, or Data frame:
#'   R colors, one row per matrix row/column and one column per annotation track.
#'   Row names, when supplied, match the original matrix identities.
#' @param show_notes Logical: Print cell notes instead of numeric value labels.
#'   Long notes truncate to the cell width; hover retains the full text.
#' @param row_cut,col_cut Optional Integer: Number of clusters for branch coloring.
#'   A cut requires a supplied or computed tree. Cross-cluster branches stay gray
#'   (or `dendro_color`); within-cluster branches share a deterministic color.
#' @param show_values Logical: Whether to print the cell value as a label inside
#'   each cell.
#' @param value_digits Integer: Decimal places used in cell labels and the
#'   tooltip, written as [rtemis.core::ddSci()] writes them.
#' @param show_colorbar Logical: Whether to display the continuous color-scale bar.
#' @param colorbar_orient Character \{"vertical", "horizontal"\}: Orientation of the
#'   color bar.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. `NULL` auto-detects light/dark mode.
#' @param margins Optional Named numeric vector or named list: Override the
#'   auto-computed heatmap plot-area margins in pixels (or percentage strings)
#'   for any of `"top"`, `"right"`, `"bottom"`, `"left"` — e.g.
#'   `c(left = 200)` to widen the left gutter for long row labels. Unspecified
#'   sides use the values computed from label lengths, title, colorbar, and
#'   dendrogram panels. See [draw_line()] for the general convention.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file via
#'   [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' draw_heatmap(cor(mtcars[, 1:5]), title = "Correlation")
#' @inheritParams draw_line legend_position legend_placement legend_title
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
  colormap = NULL,
  zlim = NULL,
  show_values = FALSE,
  value_digits = 2L,
  show_colorbar = TRUE,
  colorbar_orient = "vertical",
  title = NULL,
  theme = NULL,
  margins = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  legend_position = if (colorbar_orient == "horizontal") "bottom" else "right",
  legend_placement = "outside",
  legend_title = NULL,
  row_tree = NULL,
  col_tree = NULL,
  cell_notes = NULL,
  row_colors = NULL,
  col_colors = NULL,
  show_notes = FALSE,
  row_cut = NULL,
  col_cut = NULL
) {
  margin <- pixel_margins(margins)
  config <- setup_HeatmapConfig(
    row_names = row_names,
    col_names = col_names,
    triangle = triangle,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    dist_method = dist_method,
    hclust_method = hclust_method,
    square_cells = square_cells,
    show_row_dendro = show_row_dendro,
    show_col_dendro = show_col_dendro,
    dendro_row_width = dendro_row_width,
    dendro_col_height = dendro_col_height,
    dendro_color = dendro_color,
    dendro_uniform = dendro_uniform,
    dendro_row_side = dendro_row_side,
    dendro_col_side = dendro_col_side,
    colormap = colormap,
    zlim = zlim,
    show_notes = show_notes,
    row_cut = row_cut,
    col_cut = col_cut,
    show_values = show_values,
    value_digits = value_digits,
    show_colorbar = show_colorbar,
    colorbar_orient = colorbar_orient,
    title = title,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    heatmap_request(
      config,
      x,
      row_tree = row_tree,
      col_tree = col_tree,
      cell_notes = cell_notes,
      row_colors = row_colors,
      col_colors = col_colors,
      theme = theme
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = render_meta(config, NULL)
  )
} # /rtemis.draw::draw_heatmap


# -- draw_sankey ----------------------------------------------------------------

#' Draw a Sankey Diagram
#'
#' Sankey diagram from a data frame of directed links. Node names are derived
#' from the unique values in the `source` and `target` columns; no separate
#' node list is required. The option is built by @rtemis/draw's Sankey
#' builder, which rtemislive also uses. Rows with the same source and target
#' are summed into one link. A missing, infinite, or negative value, or links
#' that form a cycle, stop with an error.
#'
#' @param links data.frame: Directed links with columns `source` (Character),
#'   `target` (Character), and `value` (Numeric, nonnegative).
#' @param orient Optional Character \{"horizontal", "vertical"\}: Flow direction.
#' @param node_width Optional Numeric `[0, Inf)`: Node rectangle width in pixels.
#' @param node_gap Optional Numeric `[0, Inf)`: Vertical gap between nodes in pixels.
#' @param node_align Optional Character \{"justify", "left", "right"\}: Node
#'   alignment within each column.
#' @param title Optional Character: Chart title.
#' @param palette Optional Character: Node color palette — a single color string or
#'   a character vector that overrides the theme palette for this chart.
#' @param link_color Character \{"gradient", "source", "target"\} or a color:
#'   How ribbons are colored. `"gradient"`, the default, runs from the color of
#'   the node a ribbon leaves to the color of the node it enters.
#' @param link_opacity Numeric `[0, 1]`: Ribbon opacity. Below 1 so that
#'   crossing ribbons blend rather than hide one another.
#' @param margins Optional named list or numeric: Space around the plotting box,
#'   as any of `left`, `right`, `top`, `bottom` in pixels. Sides left unnamed
#'   are derived: a node's label is drawn to its right, so a horizontal
#'   diagram's right margin holds the longest label of the last column.
#' @param label_font_size Numeric: Node label font size in pixels. Also what the
#'   derived right margin is estimated from.
#' @param theme Optional [Theme]: Theme override.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this file
#'   via [save_drawing()].
#' @return htmlwidget: Widget object.
#' @examples
#' links <- data.frame(
#'   source = c("A", "A", "B", "C"),
#'   target = c("B", "C", "D", "D"),
#'   value  = c(8, 4, 6, 3)
#' )
#' draw_sankey(links)
#' @export
draw_sankey <- function(
  links,
  orient = "horizontal",
  node_width = NULL,
  node_gap = NULL,
  node_align = NULL,
  title = NULL,
  palette = NULL,
  link_color = "gradient",
  link_opacity = 0.2,
  margins = NULL,
  label_font_size = 12,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  rtemis.core::check_tabular(links)
  margin <- pixel_margins(margins)
  config <- setup_SankeyConfig(
    orient = orient,
    node_align = node_align,
    node_width = node_width,
    node_gap = node_gap,
    palette = palette,
    link_color = link_color,
    link_opacity = link_opacity,
    label_font_size = label_font_size,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    title = title
  )
  draw(
    config,
    data = links,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_sankey


#' Normalize bar table/vector input and stable category ordering
#' @inheritParams draw_bar
#' @return List: Categories, value series, and their row permutation.
#' @keywords internal
#' @noRd
bar_input <- new_generic("bar_input", "x")
method(bar_input, class_any) <- function(x, y = NULL, order = "input") {
  order <- BarConfig(order = order)@order
  if (inherits(y, "table") && length(dim(y)) == 1L) {
    y <- as.vector(y)
  }
  if (is.data.frame(x) && is.null(y)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      abort(
        "Supply numeric columns for bar values.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    x <- as.matrix(x)
  }
  if (is.null(y)) {
    if (length(dim(x)) == 2L) {
      if (!is.numeric(x)) {
        abort(
          "Supply a numeric bar matrix or table.",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      y <- setNames(
        lapply(seq_len(ncol(x)), function(i) as.numeric(x[, i])),
        colnames(x) %||% paste("Series", seq_len(ncol(x)))
      )
      x <- rownames(x) %||% as.character(seq_len(nrow(x)))
    } else {
      y <- as.vector(x)
      x <- if (inherits(x, "table")) {
        dimnames(x)[[1]]
      } else {
        names(x) %||% as.character(seq_along(x))
      }
    }
  }
  category_list <- is.list(x)
  x <- as.character(x)
  values <- if (is.list(y)) y else list(y)
  if (
    !length(x) ||
      anyNA(x) ||
      anyDuplicated(x) ||
      !length(values) ||
      any(
        !vapply(
          values,
          function(v) {
            is.numeric(v) &&
              !is.complex(v) &&
              is.null(dim(v)) &&
              length(v) == length(x) &&
              !any(is.infinite(v))
          },
          logical(1)
        )
      )
  ) {
    abort(
      "Supply distinct categories and equally sized numeric bar series, allowing NA.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    is.list(y) &&
      (is.null(names(y)) ||
        anyNA(names(y)) ||
        any(!nzchar(names(y))) ||
        anyDuplicated(names(y)))
  ) {
    abort(
      "Name every bar series distinctly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  index <- if (order == "input") {
    seq_along(x)
  } else {
    base::order(
      Reduce(`+`, values),
      decreasing = order == "decreasing",
      na.last = TRUE
    )
  }
  list(
    x = if (category_list) as.list(x[index]) else x[index],
    y = if (is.list(y)) lapply(y, `[`, index) else y[index],
    index = index
  )
}

#' Normalize and validate pie slices without recycling labels
#' @inheritParams draw_pie
#' @return List: Numeric values, and labels as a factor, which keeps its level
#'   order, or as text.
#' @keywords internal
#' @noRd
pie_input <- new_generic("pie_input", "values")
method(pie_input, class_any) <- function(values, labels = NULL) {
  if (is.data.frame(values) || is.matrix(values)) {
    if (ncol(values) != 2L || !is.null(labels)) {
      abort(
        "Supply a two-column label/value table alone, or separate values and labels.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    labels <- values[, 1]
    values <- values[, 2]
  }
  labels <- labels %||% names(values) %||% paste("Slice", seq_along(values))
  if (
    !is.numeric(values) ||
      is.complex(values) ||
      !is.null(dim(values)) ||
      !length(values) ||
      any(!is.finite(values)) ||
      any(values < 0) ||
      !is.finite(sum(values)) ||
      sum(values) <= 0 ||
      length(labels) != length(values) ||
      anyNA(labels)
  ) {
    abort(
      "Supply finite nonnegative slice values with a positive total and matching labels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  list(
    values = unname(values),
    labels = if (is.factor(labels)) labels else as.character(labels)
  )
}
