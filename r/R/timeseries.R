# timeseries.R
# Observation-window summaries are materialized before native rendering.

#' Time Series Configuration
#'
#' Numeric or date-time observations with optional rolling summaries and a
#' secondary value axis. The option is built by @rtemis/draw's time series
#' builder, which rtemislive's Time series chart also uses, from
#' LineSeriesOption and CartesianAxisOption in ECharts src/chart/line and
#' src/coord/cartesian. Rolling windows count sorted
#' observations, not elapsed time. A window summarizes the values present in
#' it when they number at least `min_obs`; a position beyond either end of the
#' series counts as missing. Even centered windows favor following values,
#' matching data.table's centered alignment used by rtemis.
#' @param x,y Optional Character: Time and value column bindings. Multiple y
#'   columns share x; group splits a single y column.
#' @param group Optional Character: Group column binding. It splits every `y`
#'   and `y2` column; with several columns bound, a series is named
#'   "column / level".
#' @param x2,y2 Optional Character: Secondary time/value bindings. Unset x2 uses x.
#' @param window Integer `[1, Inf)`: Number of observations per rolling window.
#' @param roll_fn Character `{"none", "mean", "median", "min", "max", "sum"}`: Summary.
#' @param align Character `{"center", "left", "right"}`: Window alignment.
#' @param min_obs Optional Integer `[1, window]`: Fewest values present a window
#'   needs for a summary; with fewer it is missing. `NULL` requires a majority
#'   of the window (`window %/% 2 + 1`), and the complete window for `"sum"`,
#'   since a sum over fewer values is smaller by construction.
#' @param roll_width Numeric `(0, Inf)`: Rolling line width in pixels.
#' @param roll_color Optional Character: Rolling line color; unset uses the group color.
#' @param raw_opacity Numeric `[0, 1]`: Raw observation opacity.
#' @param points Logical: Show raw points; FALSE draws raw lines.
#' @param zt,shade_bin Optional Character: Columns of zeitgeber labels and zero/one shading flags.
#' @param show_zt_every Integer `[1, Inf)`: Zeitgeber label stride; phase zero remains visible.
#' @param y2lab Optional Character: Secondary axis name.
#' @inheritParams LineConfig
#' @return TimeSeriesConfig: Validated portable settings.
#' @export
#' @examples
#' setup_TimeSeriesConfig(x = "day", y = "value", window = 3L)
TimeSeriesConfig <- new_class(
  "TimeSeriesConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    LineConfig@properties[c(
      "x",
      "y",
      "group",
      "xlab",
      "ylab",
      "palette",
      "zoom"
    )],
    list(
      type = prop_chart_type("timeseries"),
      x2 = prop_string(NULL, nullable = TRUE),
      zt = prop_string(NULL, nullable = TRUE),
      shade_bin = prop_string(NULL, nullable = TRUE),
      show_zt_every = prop_integer(1L, min = 1L),
      y2 = prop_string(NULL, nullable = TRUE, vector = TRUE),
      y2lab = prop_string(NULL, nullable = TRUE),
      window = prop_integer(7L, min = 1L),
      roll_fn = prop_string(
        "none",
        enum = c("none", "mean", "median", "min", "max", "sum")
      ),
      align = prop_string("center", enum = c("center", "left", "right")),
      min_obs = prop_integer(NULL, nullable = TRUE, min = 1L),
      roll_width = prop_float(2, exclusive_min = 0),
      roll_color = prop_string(NULL, nullable = TRUE),
      raw_opacity = prop_float(.5, min = 0, max = 1),
      points = prop_boolean(TRUE)
    )
  )
)

#' Set Up a Time Series Configuration
#' @inheritParams TimeSeriesConfig
#' @inheritParams ChartConfig
#' @inheritParams draw_line legend_position legend_placement legend_title
#' @return TimeSeriesConfig: Validated settings.
#' @export
#' @examples
#' draw(setup_TimeSeriesConfig(x = "Time", y = "demand"), data = BOD)
setup_TimeSeriesConfig <- function(
  x = NULL,
  y = NULL,
  group = NULL,
  x2 = NULL,
  y2 = NULL,
  window = 7L,
  roll_fn = "none",
  align = "center",
  min_obs = NULL,
  roll_width = 2,
  roll_color = NULL,
  raw_opacity = .5,
  points = TRUE,
  xlab = NULL,
  ylab = NULL,
  y2lab = NULL,
  palette = NULL,
  zoom = FALSE,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  zt = NULL,
  shade_bin = NULL,
  show_zt_every = 1L
) {
  check_integer_scalar(window)
  check_integer_scalar(show_zt_every)
  if (!is.null(min_obs)) {
    check_integer_scalar(min_obs)
    if (min_obs > window) {
      abort(
        "`min_obs` (",
        min_obs,
        ") cannot exceed `window` (",
        window,
        "): a window holds at most `window` values.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  origin <- origin %||%
    chart_origin(
      match.call(),
      setdiff(names(TimeSeriesConfig@properties), c("type", PROVENANCE_PROPS))
    )
  TimeSeriesConfig(
    x = x,
    y = y,
    group = group,
    x2 = x2,
    y2 = y2,
    zt = zt,
    shade_bin = shade_bin,
    show_zt_every = clean_int(show_zt_every),
    window = as.integer(window),
    roll_fn = roll_fn,
    align = align,
    min_obs = if (!is.null(min_obs)) as.integer(min_obs),
    roll_width = roll_width,
    roll_color = roll_color,
    raw_opacity = raw_opacity,
    points = points,
    xlab = xlab,
    ylab = ylab,
    y2lab = y2lab,
    palette = palette,
    zoom = zoom,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
}

# %% timeseries_table ----
#' The table of a time series drawn from vectors
#'
#' Validates the vector inputs of [draw_ts()] and [draw_xt()] and writes them
#' as one table the time series builder binds: with one shared time vector,
#' one row per time and one column per series; with a time vector per series,
#' the series stacked, named in a `.series` column, which the config groups
#' by. Secondary series with their own times take rows of their own, with
#' `.x2` as their time.
#'
#' @param x Numeric or temporal vector, or list of them: Times.
#' @param y Numeric vector or named list: Values.
#' @param group Optional vector: Groups of one value vector.
#' @param x2,y2 Optional: Secondary times and values.
#' @param zt,shade_bin Optional Numeric: Labels and flags aligned with `x`.
#'
#' @return Named list: `data` (data frame) and `bindings` (named list of
#'   config members naming its columns).
#'
#' @author EDG
#' @keywords internal
#' @noRd
timeseries_table <- function(
  x,
  y,
  group = NULL,
  x2 = NULL,
  y2 = NULL,
  zt = NULL,
  shade_bin = NULL
) {
  series_list <- function(values, prefix = "Series") {
    if (!is.list(values)) {
      values <- list(values)
    }
    labels <- names(values) %||% paste(prefix, seq_along(values))
    if (anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
      abort(
        "Supply distinct nonempty time-series names.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    stats::setNames(values, labels)
  }
  check_pairs <- function(tx, ty) {
    if (is_time_axis(tx)) {
      tx <- time_axis_ms(tx)
    }
    if (
      !is.numeric(tx) ||
        !is.numeric(ty) ||
        is.complex(tx) ||
        is.complex(ty) ||
        !is.null(dim(tx)) ||
        !is.null(dim(ty)) ||
        length(tx) != length(ty) ||
        any(is.infinite(tx)) ||
        any(is.infinite(ty))
    ) {
      abort(
        "Supply equally sized finite numeric/time coordinates and numeric values, allowing NA.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  if (!length(y) || (is.list(x) && length(x) != length(y))) {
    abort(
      "Supply one time vector per value vector, or one shared time vector.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  y <- series_list(y)
  if (!is.null(group) && length(y) != 1L) {
    abort(
      "Group one value vector, or supply named independent series.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(y2)) {
    y2 <- series_list(y2)
    clash <- names(y2) %in% names(y)
    names(y2)[clash] <- paste0(names(y2)[clash], " (right)")
  } else if (!is.null(x2)) {
    abort(
      "Supply y2 with x2, or omit both secondary inputs.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  times <- c(if (is.list(x)) x else list(x), if (!is.null(x2)) list(x2))
  temporal <- vapply(times, is_time_axis, logical(1))
  if (length(unique(temporal)) != 1L) {
    abort(
      "Use numeric time throughout, or Date/POSIXct time throughout.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if ((!is.null(zt) || !is.null(shade_bin)) && (is.list(x) || !is.null(x2))) {
    abort(
      "Use one shared x vector for shading flags or zeitgeber labels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # A column of a time type keeps its type when stacked.
  stack_times <- function(parts) {
    out <- do.call(c, unname(parts))
    if (inherits(parts[[1L]], "POSIXct")) {
      attr(out, "tzone") <- attr(parts[[1L]], "tzone")
    }
    out
  }
  missing_like <- function(v, n) rep(v[NA_integer_], n)

  if (is.list(x)) {
    for (i in seq_along(y)) {
      check_pairs(x[[i]], y[[i]])
    }
    sizes <- lengths(y)
    data <- data.frame(
      .x = stack_times(x),
      .y = unlist(y, use.names = FALSE),
      .series = factor(rep(names(y), sizes), levels = names(y)),
      check.names = FALSE
    )
    bindings <- list(x = ".x", y = ".y", group = ".series")
  } else {
    for (v in y) {
      check_pairs(x, v)
    }
    data <- data.frame(.x = x, check.names = FALSE)
    for (name in names(y)) {
      data[[name]] <- y[[name]]
    }
    bindings <- list(x = ".x", y = names(y))
    if (!is.null(group)) {
      group <- group_values(group, length(y[[1L]]))
      data[[".group"]] <- group
      bindings[["group"]] <- ".group"
    }
    if (!is.null(zt)) {
      data[[".zt"]] <- zt
      bindings[["zt"]] <- ".zt"
    }
    if (!is.null(shade_bin)) {
      data[[".shade_bin"]] <- shade_bin
      bindings[["shade_bin"]] <- ".shade_bin"
    }
  }
  if (is.null(y2)) {
    return(list(data = data, bindings = bindings))
  }
  shared <- is.null(x2) && !is.list(x) && all(lengths(y2) == nrow(data))
  if (shared) {
    for (v in y2) {
      check_pairs(x, v)
    }
    for (name in names(y2)) {
      data[[name]] <- y2[[name]]
    }
  } else {
    tx2 <- x2 %||% x
    if (is.list(tx2)) {
      abort(
        "Supply one shared time vector for the secondary series.",
        class = c("rtemis_length_error", "rtemis_input_error")
      )
    }
    for (v in y2) {
      check_pairs(tx2, v)
    }
    n1 <- nrow(data)
    n2 <- length(tx2)
    extra <- as.data.frame(
      lapply(data, function(col) missing_like(col, n2)),
      check.names = FALSE
    )
    data <- rbind(data, extra)
    data[[".x2"]] <- stack_times(list(missing_like(tx2, n1), tx2))
    for (name in names(y2)) {
      data[[name]] <- c(rep(NA_real_, n1), y2[[name]])
    }
    bindings[["x2"]] <- ".x2"
  }
  bindings[["y2"]] <- names(y2)
  list(data = data, bindings = bindings)
} # /rtemis.draw::timeseries_table


# %% timeseries_request ----
#' The build request of a time series config and its data
#'
#' @param config [TimeSeriesConfig]: The chart configuration.
#' @param data Data frame: The data its columns name.
#'
#' @return [BuildRequest] of type `timeseries`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
timeseries_request <- function(config, data) {
  if (!length(config@y)) {
    abort(
      "Bind at least one value column in y.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (
    (!is.null(config@zt) || !is.null(config@shade_bin)) && !is.null(config@x2)
  ) {
    abort(
      "Use one shared x binding with zeitgeber labels or shading flags.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(config@x2) && !length(config@y2)) {
    abort(
      "Supply y2 with x2, or omit both secondary bindings.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  roles <- c("x", "y", "group", "x2", "y2", "zt", "shade_bin")
  columns <- character(0)
  for (role in roles) {
    for (name in prop(config, role)) {
      config_column(data, name, role)
      columns <- c(columns, name)
    }
  }
  BuildRequest(
    type = "timeseries",
    data = tabular_json(data, unique(columns)),
    config = config_members(config),
    appearance = list()
  )
} # /rtemis.draw::timeseries_request


method(resolve, TimeSeriesConfig) <- function(config, data = NULL, ...) {
  config_derive(
    config,
    list(
      xlab = config@x,
      ylab = if (length(config@y) == 1L) config@y else NULL,
      y2lab = if (length(config@y2) == 1L) config@y2 else NULL
    )
  )
}

# The option is built in JavaScript by @rtemis/draw, so compiling a time
# series config gives the request the widget and SVG export pass to
# `RtemisDraw.builders.timeseries`: the bound columns and the config.
# spec: draw/shared-builders#time-series
method(compile, TimeSeriesConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  timeseries_request(config, data)
}

#' Draw Time Series with Rolling Summaries
#' @param x Numeric or named list: Observations.
#' @param time Numeric, Date, POSIXct, or list: Shared or per-series times.
#' @param group Optional vector: Group identities for one series, ordered by
#'   factor level, or by first appearance for other vectors.
#' @inheritParams TimeSeriesConfig
#' @param ... Additional settings passed to [setup_TimeSeriesConfig()].
#' @inheritParams draw_line theme width height element_id filename
#' @return htmlwidget: Raw observations and rolling curves with shared group legends.
#' @export
#' @examples
#' draw_ts(BOD$demand, BOD$Time, window = 3L)
draw_ts <- function(
  x,
  time,
  window = 7L,
  group = NULL,
  roll_fn = "mean",
  align = "center",
  ...,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  config <- setup_TimeSeriesConfig(
    window = window,
    roll_fn = roll_fn,
    align = align,
    ...
  )
  table <- timeseries_table(time, x, group = group)
  draw(
    timeseries_request(
      bound_config(config, table[["bindings"]]),
      table[["data"]]
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(
      config@legend_position,
      config@legend_placement,
      config@legend_title
    )
  )
}

#' Draw Independent Time Series on Two Value Axes
#' @param x,y Numeric, temporal vector, or named list: Primary time/value records.
#' @param x2,y2 Optional vector or list: Secondary records, with independent times.
#'   When x2 is unset, secondary values share x.
#' @param zt Optional Numeric: Zeitgeber labels aligned with a single x vector.
#' @param show_zt_every Integer `[1, Inf)`: Keep every nth label, always including zero phase.
#' @param shade_bin Optional Numeric: Zero/one shading flags aligned with x.
#'   Mutually exclusive with shade_interval; contiguous runs span their first
#'   and last observation. A single observation has zero interval width.
#' @param shade_interval List: Increasing intervals on the x scale, in epoch
#'   milliseconds for Date/POSIXct axes. Applied using [draw_annotate()].
#' @param ... Additional settings passed to [setup_TimeSeriesConfig()].
#' @inheritParams draw_line theme width height element_id filename
#' @return htmlwidget: Independent series sharing the time axis.
#' @export
#' @examples
#' draw_xt(1:5, list(Temperature = 20:24), y2 = list(Pressure = 100:104))
draw_xt <- function(
  x,
  y,
  x2 = NULL,
  y2 = NULL,
  shade_interval = list(),
  shade_bin = NULL,
  zt = NULL,
  show_zt_every = 1L,
  ...,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  if (!is.null(shade_bin) && length(shade_interval)) {
    abort(
      "Supply shade_bin or shade_interval, not both.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  config <- setup_TimeSeriesConfig(show_zt_every = show_zt_every, ...)
  table <- timeseries_table(
    x,
    y,
    x2 = x2,
    y2 = y2,
    zt = zt,
    shade_bin = shade_bin
  )
  plot <- draw(
    timeseries_request(
      bound_config(config, table[["bindings"]]),
      table[["data"]]
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    meta = legend_meta(
      config@legend_position,
      config@legend_placement,
      config@legend_title
    )
  )
  if (length(shade_interval)) {
    plot <- add_cartesian_layer(
      plot,
      list(to_list(AnnotationLayer(bands = shade_interval))),
      legend = NULL,
      x_axis = 0L,
      y_axis = 0L
    )
  }
  if (!is.null(filename)) {
    save_drawing(plot, filename)
  }
  plot
}

# %% bound_config ----
#' A time series config with its column bindings set
#'
#' @param config [TimeSeriesConfig]: The chart configuration.
#' @param bindings Named list: Members naming columns.
#'
#' @return [TimeSeriesConfig].
#'
#' @author EDG
#' @keywords internal
#' @noRd
bound_config <- function(config, bindings) {
  for (name in names(bindings)) {
    prop(config, name) <- bindings[[name]]
  }
  config
} # /rtemis.draw::bound_config
