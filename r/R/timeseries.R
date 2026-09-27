# timeseries.R
# Observation-window summaries are materialized before native rendering.

#' Time Series Configuration
#'
#' Numeric or date-time observations with optional rolling summaries and a
#' secondary value axis. Uses LineSeriesOption and CartesianAxisOption from
#' ECharts src/chart/line and src/coord/cartesian. Rolling windows count sorted
#' observations, not elapsed time; incomplete windows and windows containing
#' missing values remain missing. Even centered windows favor following values,
#' matching data.table's centered alignment used by rtemis.
#' @param x,y Optional Character: Time and value column bindings. Multiple y
#'   columns share x; group splits a single y column.
#' @param group Optional Character: Group column binding.
#' @param x2,y2 Optional Character: Secondary time/value bindings. Unset x2 uses x.
#' @param window Integer `[1, Inf)`: Number of observations per rolling window.
#' @param roll_fn Character `{"none", "mean", "median", "min", "max", "sum"}`: Summary.
#' @param align Character `{"center", "left", "right"}`: Window alignment.
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
#' @inheritParams draw_line legend_position legend_placement
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
  zt = NULL,
  shade_bin = NULL,
  show_zt_every = 1L
) {
  check_integer_scalar(window)
  check_integer_scalar(show_zt_every)
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
    legend_placement = legend_placement
  )
}

#' Materialize a complete observation-window statistic
#' @param x Numeric: Ordered values, with missing slots preserved.
#' @param config TimeSeriesConfig: Window and alignment settings.
#' @return Numeric: A summary or NA at each original observation position.
#' @keywords internal
#' @noRd
rolling_values <- new_generic("rolling_values", "x")
method(rolling_values, class_numeric) <- function(x, config) {
  if (config@roll_fn == "none") {
    return(rep(NA_real_, length(x)))
  }
  width <- config@window
  offset <- switch(
    config@align,
    right = width - 1L,
    left = 0L,
    center = floor((width - 1L) / 2)
  )
  summarize <- switch(
    config@roll_fn,
    mean = mean,
    median = stats::median,
    min = min,
    max = max,
    sum = sum
  )
  vapply(
    seq_along(x),
    function(i) {
      start <- i - offset
      end <- start + width - 1L
      if (start < 1 || end > length(x)) {
        return(NA_real_)
      }
      values <- x[seq.int(start, end)]
      if (anyNA(values)) NA_real_ else summarize(values)
    },
    numeric(1)
  )
}

#' Validate and sort independent time/value samples
#' @param x Numeric, Date, POSIXct, or list: Time coordinates.
#' @param y Numeric or list: Corresponding value vectors.
#' @param group Optional vector: Group identities for one value vector.
#' @return List: Named records with sorted x/y and time-axis identity.
#' @keywords internal
#' @noRd
timeseries_samples <- new_generic("timeseries_samples", "x")
method(timeseries_samples, class_any) <- function(x, y, group = NULL) {
  if (!is.list(y)) {
    y <- list(y)
  }
  if (!is.list(x)) {
    x <- rep(list(x), length(y))
  }
  if (!length(y) || length(x) != length(y)) {
    abort(
      "Supply one time vector per value vector, or one shared time vector.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  labels <- names(y) %||% paste("Series", seq_along(y))
  if (anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
    abort(
      "Supply distinct nonempty time-series names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  records <- list()
  if (!is.null(group)) {
    if (length(y) != 1L) {
      abort(
        "Group one value vector, or supply named independent series.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    group <- group_values(group, length(y[[1]]))
  }
  for (i in seq_along(y)) {
    tx <- x[[i]]
    ty <- y[[i]]
    temporal <- is_time_axis(tx)
    if (temporal) {
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
    groups <- if (is.null(group)) {
      labels[[i]]
    } else {
      unique(as.character(group[!is.na(group)]))
    }
    for (label in groups) {
      keep <- which(
        !is.na(tx) &
          if (is.null(group)) {
            TRUE
          } else {
            !is.na(group) & as.character(group) == label
          }
      )
      index <- keep[order(tx[keep])]
      records[[label]] <- list(
        x = tx[index],
        y = ty[index],
        temporal = temporal
      )
    }
  }
  if (
    !length(records) ||
      !any(vapply(records, function(r) any(!is.na(r[["y"]])), logical(1)))
  ) {
    abort(
      "Supply at least one available time/value pair.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  records
}

#' Build native time-series layers and optional independent secondary axis
#' @param x,y,x2,y2 See [draw_xt()].
#' @param group See [draw_ts()].
#' @param config TimeSeriesConfig: Validated settings.
#' @return EChartsOption: Materialized raw and rolling records.
#' @keywords internal
#' @noRd
timeseries_option <- new_generic("timeseries_option", "x")
method(timeseries_option, class_any) <- function(
  x,
  y,
  config,
  group = NULL,
  x2 = NULL,
  y2 = NULL
) {
  records <- timeseries_samples(x, y, group)
  nprimary <- length(records)
  if (!is.null(y2)) {
    secondary <- timeseries_samples(x2 %||% x, y2)
    if (any(names(secondary) %in% names(records))) {
      names(secondary) <- paste0(names(secondary), " (right)")
    }
    records <- c(records, secondary)
  } else if (!is.null(x2)) {
    abort(
      "Supply y2 with x2, or omit both secondary inputs.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  temporal <- vapply(records, `[[`, logical(1), "temporal")
  if (length(unique(temporal)) != 1L) {
    abort(
      "Use numeric time throughout, or Date/POSIXct time throughout.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  series <- list()
  for (i in seq_along(records)) {
    record <- records[[i]]
    raw <- to_list(LineSeries(
      name = names(records)[[i]],
      data = Map(
        function(a, b) list(a, if (is.na(b)) NULL else b),
        record[["x"]],
        record[["y"]]
      ),
      show_symbol = config@points,
      symbol_size = 5,
      line_style = LineStyle(
        width = if (config@points) 0 else 1,
        opacity = config@raw_opacity
      ),
      item_style = ItemStyle(opacity = config@raw_opacity),
      y_axis_index = as.integer(i > nprimary)
    ))
    series[[length(series) + 1L]] <- raw
    if (config@roll_fn != "none") {
      rolled <- rolling_values(record[["y"]], config)
      if (any(!is.finite(rolled) & !is.na(rolled))) {
        abort(
          "Rescale values to keep rolling summaries finite.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      curve <- raw
      curve[["data"]] <- Map(
        function(a, b) list(a, if (is.na(b)) NULL else b),
        record[["x"]],
        rolled
      )
      curve[["showSymbol"]] <- FALSE
      curve[["lineStyle"]] <- to_list(LineStyle(
        width = config@roll_width,
        color = config@roll_color
      ))
      curve[["itemStyle"]] <- list(opacity = 1)
      series[[length(series) + 1L]] <- curve
    }
  }
  grid <- resolve_margins(DEFAULT_MARGINS)
  zoom <- resolve_zoom(config@zoom, axis = "x")
  if (config@zoom) {
    placed <- reserve_slider_room(zoom, grid)
    zoom <- placed[["data_zoom"]]
    grid <- placed[["grid"]]
  }
  axes <- list(Axis(
    type = "value",
    scale = TRUE,
    name = config@ylab,
    name_location = "middle"
  ))
  if (!is.null(y2)) {
    axes[[2L]] <- Axis(
      type = "value",
      scale = TRUE,
      position = "right",
      name = config@y2lab,
      name_location = "middle",
      split_line = SplitLine(show = FALSE)
    )
  }
  EChartsOption(
    series = series,
    use_utc = if (temporal[[1]]) TRUE else NULL,
    x_axis = Axis(
      type = if (temporal[[1]]) "time" else "value",
      scale = TRUE,
      name = config@xlab,
      name_location = "middle"
    ),
    y_axis = axes,
    legend = Legend(
      data = as.list(names(records)),
      item_style = ItemStyle(opacity = 1)
    ),
    tooltip = Tooltip(
      trigger = "axis",
      value_formatter = number_value_formatter()
    ),
    grid = grid,
    data_zoom = zoom,
    color = config@palette,
    title = if (!is.null(config@title)) Title(text = config@title) else NULL
  )
}

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

method(compile, TimeSeriesConfig) <- function(config, data = NULL, ...) {
  values <- lapply(config@y, function(name) config_column(data, name, "y"))
  names(values) <- config@y
  if (!length(values)) {
    abort(
      "Bind at least one value column in y.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  secondary <- if (length(config@y2)) {
    setNames(
      lapply(config@y2, function(name) config_column(data, name, "y2")),
      config@y2
    )
  } else {
    NULL
  }
  option <- timeseries_option(
    config_column(data, config@x, "x"),
    values,
    config,
    group = config_column(data, config@group, "group"),
    x2 = config_column(data, config@x2, "x2"),
    y2 = secondary
  )
  if (
    (!is.null(config@zt) || !is.null(config@shade_bin)) && !is.null(config@x2)
  ) {
    abort(
      "Use one shared x binding with zeitgeber labels or shading flags.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  timeseries_layers(
    option,
    config_column(data, config@x, "x"),
    zt = config_column(data, config@zt, "zt"),
    shade_bin = config_column(data, config@shade_bin, "shade_bin"),
    every = config@show_zt_every
  )
}

#' Draw Time Series with Rolling Summaries
#' @param x Numeric or named list: Observations.
#' @param time Numeric, Date, POSIXct, or list: Shared or per-series times.
#' @param group Optional vector: Group identities for one series.
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
  draw(
    timeseries_option(time, x, config, group),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(config@legend_position, config@legend_placement)
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
  if ((!is.null(zt) || !is.null(shade_bin)) && (is.list(x) || !is.null(x2))) {
    abort(
      "Use one shared x vector for shading flags or zeitgeber labels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  config <- setup_TimeSeriesConfig(...)
  plot <- draw(
    timeseries_layers(
      timeseries_option(x, y, config, x2 = x2, y2 = y2),
      x,
      zt = zt,
      shade_bin = shade_bin,
      intervals = shade_interval,
      every = show_zt_every
    ),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    meta = legend_meta(config@legend_position, config@legend_placement)
  )
  if (!is.null(filename)) {
    save_drawing(plot, filename)
  }
  plot
}


#' Materialize nonuniform x-axis labels without changing time coordinates
#' @param x List: ECharts option.
#' @param time Numeric, Date, or POSIXct: Original time positions.
#' @param labels Numeric: Zeitgeber phases corresponding to time.
#' @param every Integer: Label stride, retaining zero phase positions.
#' @return List: Option with native custom vector text at time coordinates.
#' @keywords internal
#' @noRd
timeseries_tick_labels <- new_generic("timeseries_tick_labels", "x")
method(timeseries_tick_labels, class_list) <- function(x, time, labels, every) {
  check_integer_scalar(every)
  if (
    every < 1L ||
      !is.numeric(labels) ||
      length(labels) != length(time) ||
      any(!is.finite(labels))
  ) {
    abort(
      "Supply finite zeitgeber labels aligned with time and a positive integer label stride.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  tx <- if (is_time_axis(time)) time_axis_ms(time) else time
  index <- which(!is.na(tx))[order(tx[!is.na(tx)])]
  keep <- index[(seq_along(index) - 1L) %% every == 0L | labels[index] == 0]
  x[["xAxis"]][["axisLabel"]] <- list(show = FALSE)
  x[["series"]] <- c(
    x[["series"]],
    list(list(
      type = "custom",
      name = "",
      renderItem = "rtemis.axis_labels.v1",
      silent = TRUE,
      clip = FALSE,
      dimensions = list(
        list(name = "time", type = "float"),
        list(name = "text", type = "ordinal")
      ),
      encode = list(x = 0L, y = -1L, tooltip = -1L),
      data = Map(
        function(a, b) list(a, as.character(b)),
        tx[keep],
        labels[keep]
      )
    ))
  )
  x
}

#' Apply shared time-series annotations before rendering
#' @param x EChartsOption: Compiled time-series geometry.
#' @param time Numeric or temporal: Shared observation coordinates.
#' @param zt,shade_bin Optional Numeric: Phase labels or binary shading flags.
#' @param intervals List: Explicit shading intervals.
#' @param every Integer: Phase-label stride.
#' @return EChartsOption: Geometry with portable annotation layers.
#' @keywords internal
#' @noRd
timeseries_layers <- new_generic("timeseries_layers", "x")
method(timeseries_layers, EChartsOption) <- function(
  x,
  time,
  zt = NULL,
  shade_bin = NULL,
  intervals = list(),
  every = 1L
) {
  if (!is.null(zt)) {
    option <- timeseries_tick_labels(to_list(x), time, zt, every)
    x@series <- option[["series"]]
    x@x_axis@axis_label <- AxisLabel(show = FALSE)
  }
  if (!is.null(shade_bin)) {
    if (
      !is.numeric(shade_bin) ||
        length(shade_bin) != length(time) ||
        anyNA(shade_bin) ||
        any(!shade_bin %in% c(0, 1))
    ) {
      abort(
        "Supply one zero/one shading flag per time value.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    tx <- if (is_time_axis(time)) time_axis_ms(time) else time
    index <- which(!is.na(tx))[order(tx[!is.na(tx)])]
    mark <- build_block_mark_area(
      tx[index],
      shade_bin[index],
      c("0" = "transparent", "1" = "#888888"),
      .15
    )
    series <- to_list(x)[["series"]]
    series[[1]][["markArea"]] <- if (!is.null(mark)) {
      area <- to_list(mark)
      area[["label"]] <- list(show = FALSE)
      area
    } else {
      NULL
    }
    x@series <- series
  }
  if (length(intervals)) {
    x@series <- c(
      to_list(x)[["series"]],
      list(to_list(AnnotationLayer(bands = intervals)))
    )
  }
  x
}
