# -- Internal helpers -----------------------------------------------------------

# Map a window name string to the corresponding signal-package window vector.
# @param window Character or Numeric: window name or pre-built window vector.
# @param n Integer: window length in samples (used only when window is a string).
# @return Numeric vector of length n.
# @keywords internal
# @noRd
.specgram_window <- function(window, n) {
  if (is.numeric(window)) {
    if (length(window) != n) {
      abort(
        "Custom `window` vector has length ",
        length(window),
        ", expected ",
        n,
        " (= `n_fft`).",
        class = c("rtemis_dim_error", "rtemis_input_error")
      )
    }
    return(window)
  }
  fns <- list(
    hanning = signal::hanning,
    hamming = signal::hamming,
    blackman = signal::blackman,
    bartlett = signal::bartlett,
    rectangular = function(n) rep(1, n)
  )
  fn <- fns[[window]]
  if (is.null(fn)) {
    abort(
      "`window` '",
      window,
      "' is not a recognized window type. Use one of ",
      paste(names(fns), collapse = ", "),
      " or supply a numeric vector of length `n_fft`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  fn(n)
}

# Named color schemes of the spectrogram builder (@rtemis/draw's COLORMAPS),
# and the diverging scale in the theme's sign colors.
SPECTROGRAM_COLORMAPS <- c(
  "magma",
  "inferno",
  "plasma",
  "viridis",
  "cividis",
  "rocket",
  "mako",
  "turbo",
  "diverging"
)

# -- spectrogram_data ----------------------------------------------------------

#' The spectrogram builder's data from a signal or a spectrum matrix
#'
#' Validates the arguments, computes the short-time Fourier transform of a raw
#' signal ([signal::specgram()]), takes the magnitude or power of a complex
#' spectrum, and converts to decibels with the `db_range` floor. The builder
#' applies the frequency scale, the ranges, the units, and the color scale;
#' the ranges are checked here so that an empty range stops where the chart is
#' set up.
#'
#' @inheritParams draw_spectrogram
#'
#' @return Named list: `time` (seconds), `frequency` (Hz), and `values`, the
#'   matrix with one row per frequency, non-finite values as `NA`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spectrogram_data <- function(
  x,
  sample_rate = NULL,
  time = NULL,
  frequency = NULL,
  n_fft = 256L,
  window = "hanning",
  overlap = NULL,
  power = TRUE,
  db = TRUE,
  db_range = 80,
  freq_scale = "linear",
  freq_range = NULL,
  time_range = NULL,
  colormap = "magma",
  n_colors = 256L,
  zlim = NULL,
  verbosity = 1L
) {
  if (!is.numeric(x) && !is.complex(x)) {
    abort(
      "`x` must be a numeric or complex matrix (freq x time) ",
      "or a numeric vector (raw signal).",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.complex(x) && !is.matrix(x)) {
    abort(
      "`x` is a complex vector, which is not a valid raw signal. ",
      "Pass the STFT output as a complex matrix (freq x time), ",
      "e.g. `signal::specgram(x)$S`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  is_raw_signal <- is.numeric(x) && is.vector(x) && !is.matrix(x)
  freq_scale <- match.arg(freq_scale, c("linear", "log"))
  for (arg in c("power", "db")) {
    v <- get(arg)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      abort(
        "`",
        arg,
        "` must be a single logical value (TRUE or FALSE).",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  if (
    !is.numeric(db_range) ||
      length(db_range) != 1L ||
      is.na(db_range) ||
      db_range <= 0
  ) {
    abort(
      "`db_range` must be a single positive number. Got ",
      paste(db_range, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !is.numeric(n_colors) ||
      length(n_colors) != 1L ||
      is.na(n_colors) ||
      n_colors < 2L
  ) {
    abort(
      "`n_colors` must be an integer >= 2.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !is.character(colormap) ||
      (length(colormap) == 1L && !colormap %in% SPECTROGRAM_COLORMAPS)
  ) {
    abort(
      "`colormap` must be one of ",
      paste(SPECTROGRAM_COLORMAPS, collapse = ", "),
      ", or a character vector of >= 2 hex colors.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  validate_range <- function(r, arg) {
    if (is.null(r)) {
      return(invisible(NULL))
    }
    if (!is.numeric(r) || length(r) != 2L || anyNA(r) || r[[1L]] >= r[[2L]]) {
      abort(
        "`",
        arg,
        "` must be a length-2 numeric vector with ",
        arg,
        "[1] < ",
        arg,
        "[2]. Got ",
        paste(r, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  validate_range(freq_range, "freq_range")
  validate_range(time_range, "time_range")
  validate_range(zlim, "zlim")

  if (is_raw_signal) {
    if (
      is.null(sample_rate) ||
        !is.numeric(sample_rate) ||
        length(sample_rate) != 1L ||
        is.na(sample_rate) ||
        sample_rate <= 0
    ) {
      abort(
        "`sample_rate` is required when `x` is a raw signal vector, as one ",
        "positive number in Hz, e.g. `sample_rate = 44100`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (!is.numeric(n_fft) || length(n_fft) != 1L || is.na(n_fft)) {
      abort(
        "`n_fft` must be a single integer >= 2. Got ",
        paste(n_fft, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    n_fft <- as.integer(n_fft)
    if (n_fft < 2L) {
      abort(
        "`n_fft` must be >= 2. Got ",
        n_fft,
        ".",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
    if (n_fft > length(x)) {
      abort(
        "`n_fft` (",
        n_fft,
        ") exceeds the signal length (",
        length(x),
        "). Reduce `n_fft` or supply a longer signal.",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
    win_vec <- .specgram_window(window, n_fft)
    if (
      !is.null(overlap) &&
        (!is.numeric(overlap) || length(overlap) != 1L || is.na(overlap))
    ) {
      abort(
        "`overlap` must be a single non-negative integer or NULL. Got ",
        paste(overlap, collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    ovlp <- as.integer(overlap %||% ceiling(n_fft / 2L))
    if (ovlp < 0L || ovlp >= n_fft) {
      abort(
        "`overlap` must be in [0, n_fft - 1] = [0, ",
        n_fft - 1L,
        "]. Got ",
        ovlp,
        ".",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
    sg <- signal::specgram(
      x,
      n = n_fft,
      Fs = sample_rate,
      window = win_vec,
      overlap = ovlp
    )
    S <- sg[["S"]]
    freq_hz <- sg[["f"]]
    time_s <- sg[["t"]]
  } else {
    if (!is.matrix(x)) {
      abort(
        "`x` must be a numeric or complex matrix (freq x time) ",
        "or a numeric vector (raw signal).",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    S <- x
    if (!is.null(time)) {
      if (!is.numeric(time) || length(time) != ncol(S) || anyNA(time)) {
        abort(
          "`time` must be a numeric vector of length ",
          ncol(S),
          " (= ncol(x)) with no NAs.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    if (!is.null(frequency)) {
      if (
        !is.numeric(frequency) ||
          length(frequency) != nrow(S) ||
          anyNA(frequency)
      ) {
        abort(
          "`frequency` must be a numeric vector of length ",
          nrow(S),
          " (= nrow(x)) with no NAs.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    time_s <- time %||% (seq_len(ncol(S)) - 1L)
    freq_hz <- frequency %||% (seq_len(nrow(S)) - 1L)
  }

  # A complex spectrum takes |S| or |S|^2; a real matrix is drawn as given.
  if (is.complex(S)) {
    amp <- Mod(S)
    spec <- if (power) amp^2 else amp
  } else {
    spec <- S
  }
  if (db) {
    if (!is.complex(S) && any(spec < 0, na.rm = TRUE)) {
      warn(
        "Pre-computed matrix contains negative values; `db = TRUE` ",
        "will produce NaN for those entries. ",
        "Pass `db = FALSE` if the matrix is already in dB, ",
        "or ensure values are non-negative before dB conversion."
      )
    }
    eps <- .Machine[["double.eps"]]
    # 10*log10 for power, 20*log10 for amplitude - see @param power docs.
    spec <- if (power) 10 * log10(spec + eps) else 20 * log10(spec + eps)
    peak <- max(spec, na.rm = TRUE)
    spec <- pmax(spec, peak - db_range)
  }
  spec[!is.finite(spec)] <- NA_real_

  shown_f <- if (freq_scale == "log") {
    freq_hz > 0
  } else {
    rep(TRUE, length(freq_hz))
  }
  if (!any(shown_f)) {
    abort(
      "`freq_scale = 'log'` needs a frequency above 0 Hz.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(freq_range)) {
    shown_f <- shown_f &
      freq_hz >= freq_range[[1L]] &
      freq_hz <= freq_range[[2L]]
    if (!any(shown_f)) {
      abort(
        "No frequency bins fall within `freq_range` [",
        freq_range[[1L]],
        ", ",
        freq_range[[2L]],
        "] Hz. Check `freq_range` against the signal's Nyquist limit (",
        max(freq_hz),
        " Hz).",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
  }
  if (!is.null(time_range)) {
    if (!any(time_s >= time_range[[1L]] & time_s <= time_range[[2L]])) {
      abort(
        "No time frames fall within `time_range` [",
        time_range[[1L]],
        ", ",
        time_range[[2L]],
        "] s. Check `time_range` against the signal duration (",
        max(time_s),
        " s).",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
  }
  if (is.null(zlim) && !any(is.finite(spec))) {
    abort(
      "Cannot determine color-scale limits: spectrogram contains no finite values. ",
      "Check the input signal, or supply `zlim` explicitly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  n_cells <- length(spec)
  if (n_cells > 500000L && verbosity > 0L) {
    warn(
      "Spectrogram has ",
      n_cells,
      " cells; browser rendering may be slow."
    )
    # `n_fft`/signal length only apply when the STFT is computed here; a
    # pre-computed matrix must be shrunk by the caller before plotting.
    if (is_raw_signal) {
      info(
        "Consider reducing `n_fft` (currently ",
        n_fft,
        ") or the signal length."
      )
    } else {
      info(
        "Consider reducing the matrix resolution before plotting, ",
        "e.g. by aggregating neighboring frequency bins or time frames."
      )
    }
  }
  list(
    time = as.numeric(time_s),
    frequency = as.numeric(freq_hz),
    values = spec
  )
} # /rtemis.draw::spectrogram_data


# -- spectrogram_request -------------------------------------------------------

#' The build request of a spectrogram
#'
#' The data of `spectrogram_data()` for `config`, and, with a [Theme], the
#' sign colors and background of the diverging color scheme.
#'
#' @param config [SpectrogramConfig].
#' @param x Numeric vector, or numeric or complex matrix: The signal or the
#'   spectrum.
#' @param time,frequency Optional Numeric: Axis values of a matrix.
#' @param window Character or Numeric: Window name or vector.
#' @param overlap Optional Integer: Overlap in samples; unset converts the
#'   config's fraction of `n_fft`.
#' @param verbosity Integer: Verbosity level.
#' @param theme Optional [Theme].
#' @param data Optional List: The output of `spectrogram_data()`, computed
#'   from `x` when unset.
#'
#' @return [BuildRequest] of type `spectrogram`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spectrogram_request <- function(
  config,
  x,
  time = NULL,
  frequency = NULL,
  window = config@window,
  overlap = NULL,
  verbosity = 1L,
  theme = NULL,
  data = NULL
) {
  data <- data %||%
    spectrogram_data(
      x,
      sample_rate = config@sample_rate,
      time = time,
      frequency = frequency,
      n_fft = config@n_fft,
      window = window,
      overlap = overlap %||%
        if (!is.null(config@overlap)) round(config@overlap * config@n_fft),
      power = config@power,
      db = config@db,
      db_range = config@db_range,
      freq_scale = config@freq_scale,
      freq_range = config@freq_range,
      time_range = config@time_range,
      colormap = config@colormap,
      n_colors = config@n_colors,
      zlim = config@zlim,
      verbosity = verbosity
    )
  appearance <- list()
  if (S7::S7_inherits(theme, Theme)) {
    sign <- theme_sign_colors(theme)
    appearance[["signColors"]] <- list(
      negative = sign[["negative"]],
      positive = sign[["positive"]]
    )
    appearance[["background"]] <- theme@bg_color
  }
  BuildRequest(
    type = "spectrogram",
    data = as.character(jsonlite::toJSON(
      list(
        time = data[["time"]],
        frequency = data[["frequency"]],
        values = data[["values"]]
      ),
      auto_unbox = FALSE,
      na = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = appearance
  )
} # /rtemis.draw::spectrogram_request


#' Draw a Spectrogram
#'
#' Renders an interactive time-frequency spectrogram as an ECharts heatmap
#' widget. Accepts either a raw signal vector (STFT is computed internally via
#' [signal::specgram()]) or a pre-computed spectrogram matrix (freq x time).
#' R computes the spectrum; the `@rtemis/draw` spectrogram builder applies the
#' frequency scale, ranges, units, and color scale, and draws it.
#'
#' Corresponds to `HeatmapSeriesOption` in `src/chart/heatmap/HeatmapSeries.ts`.
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-heatmap}
#'
#' @param x Numeric matrix (freq x time) or numeric vector (raw signal).
#'   A matrix is used directly; `time` and `frequency` vectors supply axis
#'   values (defaults to sample indices when absent). A complex matrix (raw
#'   STFT output, e.g. from [signal::specgram()]\code{$S}) is also accepted.
#'   A vector triggers STFT computation via [signal::specgram()]; `sample_rate`
#'   is required.
#' @param sample_rate Optional Numeric `(0, Inf)`: Sampling frequency in Hz.
#'   Required when `x` is a raw signal vector.
#' @param time Optional Numeric: Time axis values in seconds, length `ncol(x)`.
#'   Only used when `x` is a matrix.
#' @param frequency Optional Numeric: Frequency axis values in Hz, length
#'   `nrow(x)`. Only used when `x` is a matrix.
#' @param n_fft Integer `[2, Inf)`: FFT window size in samples. Passed to
#'   [signal::specgram()] as `n`. Only used when `x` is a raw signal.
#' @param window Character \{"hanning", "hamming", "blackman", "bartlett",
#'   "rectangular"\} or Numeric: Window function name or a pre-built window
#'   vector. Passed to [signal::specgram()]. Only used when `x` is a raw signal.
#' @param overlap Optional Integer `[0, n_fft)`: Overlap between consecutive
#'   frames in samples. Defaults to `n_fft / 2`. Only used when `x` is a raw
#'   signal.
#' @param power Logical: Treat spectral values as power (`TRUE`) or amplitude
#'   (`FALSE`). When `x` is complex, controls whether the STFT magnitude is
#'   squared (`|S|^2`) or left as-is (`|S|`). When `db = TRUE`, also determines
#'   the dB scaling for real matrices: `10 * log10()` for power, `20 * log10()`
#'   for amplitude.
#' @param db Logical: Convert to dB. For power: `10 * log10()`; for amplitude:
#'   `20 * log10()`. Set `FALSE` when passing a pre-computed dB matrix.
#' @param db_range Numeric `(0, Inf)`: Dynamic range to display in dB below the
#'   spectral peak. Values below `peak - db_range` are clipped to the floor.
#' @param freq_scale Character \{"linear", "log"\}: Frequency axis scale. With
#'   `"log"` the DC component (0 Hz) is automatically dropped.
#' @param freq_range Optional Numeric\[2\]: Frequency range to display in Hz,
#'   e.g. `c(20, 8000)`. Applied after STFT computation.
#' @param freq_unit Character \{"Hz", "kHz"\}: Unit for the frequency axis.
#' @param time_range Optional Numeric\[2\]: Time range to display in seconds.
#' @param time_unit Character \{"s", "ms"\}: Unit for the time axis.
#' @param colormap Character: Color palette. Accepts the name of a colormap of
#'   \pkg{viridisLite} (`"magma"` (default), `"inferno"`, `"plasma"`,
#'   `"viridis"`, `"cividis"`, `"mako"`, `"rocket"`, `"turbo"`), `"diverging"`
#'   for a negative-background-positive scale in the theme's sign colors
#'   (suitable for signed data such as EEG/MEG amplitudes), or a character vector of >= 2 hex colors for a
#'   custom ramp.
#' @param colormap_reverse Logical: Reverse the colormap direction.
#' @param n_colors Integer `[2, Inf)`: Number of discrete colors in the
#'   generated palette.
#' @param zlim Optional Numeric\[2\]: Color-scale limits after all
#'   transformations (dB clipping, unit conversion). Defaults to the data range.
#' @param show_colorbar Logical: Show the continuous visual-map colorbar.
#' @param colorbar_title Optional Character: Colorbar label. Default: `"dB"`,
#'   `"Power"`, or `"Amplitude"` derived from `db` and `power`.
#' @param title Optional Character: Chart title.
#' @param xlab Optional Character: X-axis label. Default: `"Time (s)"` or
#'   `"Time (ms)"` depending on `time_unit`.
#' @param ylab Optional Character: Y-axis label. Default: `"Frequency (Hz)"`
#'   or `"Frequency (kHz)"` depending on `freq_unit`.
#' @param theme Optional [Theme], list, or `NA`: Theme override passed to
#'   [draw()].
#' @param margins Optional Named numeric vector or list: Plot margins in
#'   pixels. Valid names: `"top"`, `"right"`, `"bottom"`, `"left"`. Unset
#'   margins are 36 pixels, 90 at the right, and 50 at the top with a title.
#' @param width Optional Numeric or Character: Widget width.
#' @param height Optional Numeric or Character: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param verbosity Integer `[0, Inf)`: Verbosity level. `0` silences the
#'   large-spectrogram performance advisory.
#' @param filename Optional Character: If provided, the widget is saved via
#'   [save_drawing()].
#' @return htmlwidget
#' @examples
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   t_vec <- seq(0, 2, by = 1 / 8000)
#'   sig   <- signal::chirp(t_vec, 200, 2, 2000)
#'   draw_spectrogram(sig, sample_rate = 8000)
#' }
#' @export
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_spectrogram <- function(
  x,
  sample_rate = NULL,
  time = NULL,
  frequency = NULL,
  n_fft = 256L,
  window = "hanning",
  overlap = NULL,
  power = TRUE,
  db = TRUE,
  db_range = 80,
  freq_scale = "linear",
  freq_range = NULL,
  freq_unit = "Hz",
  time_range = NULL,
  time_unit = "s",
  colormap = "magma",
  colormap_reverse = FALSE,
  n_colors = 256L,
  zlim = NULL,
  show_colorbar = TRUE,
  colorbar_title = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = NULL,
  margins = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  verbosity = 1L,
  legend_position = "right",
  legend_placement = "outside",
  legend_title = NULL
) {
  margins <- pixel_margins(margins)
  # The arguments are checked, and the spectrum computed, before the config
  # takes them.
  data <- spectrogram_data(
    x,
    sample_rate = sample_rate,
    time = time,
    frequency = frequency,
    n_fft = n_fft,
    window = window,
    overlap = overlap,
    power = power,
    db = db,
    db_range = db_range,
    freq_scale = freq_scale,
    freq_range = freq_range,
    time_range = time_range,
    colormap = colormap,
    n_colors = n_colors,
    zlim = zlim,
    verbosity = verbosity
  )
  config <- setup_SpectrogramConfig(
    sample_rate = sample_rate,
    n_fft = n_fft,
    window = if (is.character(window)) window else "hanning",
    overlap = if (!is.null(overlap) && is.numeric(n_fft)) overlap / n_fft,
    power = power,
    db = db,
    db_range = db_range,
    freq_scale = freq_scale,
    freq_range = freq_range,
    time_range = time_range,
    freq_unit = freq_unit,
    time_unit = time_unit,
    colormap = colormap,
    colormap_reverse = colormap_reverse,
    n_colors = n_colors,
    zlim = zlim,
    show_colorbar = show_colorbar,
    colorbar_title = colorbar_title,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margins[["top"]],
    margin_right = margins[["right"]],
    margin_bottom = margins[["bottom"]],
    margin_left = margins[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    spectrogram_request(config, x, theme = theme, data = data),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = render_meta(config, NULL)
  )
}
