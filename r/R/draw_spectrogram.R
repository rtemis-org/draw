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
      cli::cli_abort(
        "Custom {.arg window} vector has length {length(window)}, \\
         expected {n} (= {.arg n_fft}).",
        call = NULL
      )
    }
    return(window)
  }
  fns <- list(
    hann = signal::hanning,
    hamming = signal::hamming,
    blackman = signal::blackman,
    bartlett = signal::bartlett,
    rectangular = function(n) rep(1, n)
  )
  fn <- fns[[window]]
  if (is.null(fn)) {
    cli::cli_abort(
      c(
        "{.arg window} {.val {window}} is not a recognised window type.",
        "i" = "Use one of {.val {paste(names(fns), collapse = ', ')}} \\
               or supply a numeric vector of length {.arg n_fft}."
      ),
      call = NULL
    )
  }
  fn(n)
}

# Build a color vector for the ECharts visualMap component.
#
# Returns a character vector of hex colours.  For the "diverging" palette an
# additional "dark_variant" attribute carries the dark-theme version so the JS
# binding can switch midpoint colour on the fly (consistent with draw_heatmap).
#
# @param palette Character(1) or Character(n>=2): palette name or hex vector.
# @param n_colors Integer: number of colours to generate.
# @param reverse Logical: reverse palette direction.
# @param zlim Numeric[2]: data range (needed to split diverging palette).
# @return Character vector of hex colours.
# @keywords internal
# @noRd
.spectrogram_palette <- function(palette, n_colors, reverse, zlim) {
  # viridisLite option letters (viridis() `option` parameter)
  viridis_opts <- c(
    magma = "A",
    inferno = "B",
    plasma = "C",
    viridis = "D",
    cividis = "E",
    rocket = "F",
    mako = "G",
    turbo = "H"
  )

  if (is.character(palette) && length(palette) == 1L) {
    # Named viridisLite palette
    if (palette %in% names(viridis_opts)) {
      return(viridisLite::viridis(
        n_colors,
        option = viridis_opts[[palette]],
        direction = if (reverse) -1L else 1L
      ))
    }

    # Diverging (theme-aware, like draw_heatmap)
    if (identical(palette, "diverging")) {
      # Symmetrize zlim around 0 when data spans both signs so the midpoint
      # colour maps exactly to 0.
      zlim_sym <- if (!is.null(zlim) && zlim[[1L]] < 0 && zlim[[2L]] > 0) {
        m <- max(abs(zlim))
        c(-m, m)
      } else {
        zlim %||% c(-1, 1)
      }
      cols_light <- diverging_palette(
        rtemis_colors[[1L]],
        "#ffffff",
        rtemis_colors[[2L]],
        zlim_sym,
        n = n_colors
      )
      cols_dark <- diverging_palette(
        rtemis_colors[[1L]],
        "#181818",
        rtemis_colors[[2L]],
        zlim_sym,
        n = n_colors
      )
      if (reverse) {
        cols_light <- rev(cols_light)
        cols_dark <- rev(cols_dark)
      }
      attr(cols_light, "dark_variant") <- cols_dark
      return(cols_light)
    }

    cli::cli_abort(
      c(
        "{.arg palette} {.val {palette}} is not recognised.",
        "i" = paste0(
          "Named options: {.val ",
          paste(names(viridis_opts), collapse = "}, {.val "),
          "}}, {.val diverging}.",
          " Or supply a character vector of >= 2 hex colours."
        )
      ),
      call = NULL
    )
  }

  # Custom colour ramp (>= 2 hex strings supplied directly)
  if (is.character(palette) && length(palette) >= 2L) {
    cols <- grDevices::colorRampPalette(palette)(n_colors)
    if (reverse) {
      cols <- rev(cols)
    }
    return(cols)
  }

  cli::cli_abort(
    paste0(
      "{.arg palette} must be a named palette string or a character vector \\
       of >= 2 hex colours."
    ),
    call = NULL
  )
}

# -- draw_spectrogram -----------------------------------------------------------

#' Draw a Spectrogram
#'
#' Renders an interactive time-frequency spectrogram as an ECharts heatmap
#' widget. Accepts either a raw signal vector (STFT is computed internally via
#' [signal::specgram()]) or a pre-computed spectrogram matrix (freq x time).
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
#' @param window Character \{"hann", "hamming", "blackman", "bartlett",
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
#' @param palette Character: Colour palette. Accepts a \pkg{viridisLite} palette
#'   name (`"magma"` (default), `"inferno"`, `"plasma"`,
#'   `"viridis"`, `"cividis"`, `"mako"`, `"rocket"`, `"turbo"`), `"diverging"`
#'   for the rtemis teal-background-orange scale (suitable for signed data
#'   such as EEG/MEG amplitudes), or a character vector of >= 2 hex colours for a
#'   custom ramp.
#' @param palette_reverse Logical: Reverse the palette direction.
#' @param n_colors Integer `[2, Inf)`: Number of discrete colours in the
#'   generated palette.
#' @param zlim Optional Numeric\[2\]: Colour-scale limits after all
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
#' @param margins Optional Named numeric or character vector / list: Plot
#'   margins in pixels. Valid names: `"top"`, `"right"`, `"bottom"`, `"left"`.
#' @param width Optional Numeric or Character: Widget width.
#' @param height Optional Numeric or Character: Widget height.
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
draw_spectrogram <- function(
  x,
  sample_rate = NULL,
  time = NULL,
  frequency = NULL,
  n_fft = 256L,
  window = "hann",
  overlap = NULL,
  power = TRUE,
  db = TRUE,
  db_range = 80,
  freq_scale = "linear",
  freq_range = NULL,
  freq_unit = "Hz",
  time_range = NULL,
  time_unit = "s",
  palette = "magma",
  palette_reverse = FALSE,
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
  filename = NULL
) {
  # -- 1. Validate scalar arguments ---------------------------------------------
  if (!is.numeric(x) && !is.complex(x)) {
    cli::cli_abort(
      "{.arg x} must be a numeric or complex matrix (freq x time) \\
       or a numeric vector (raw signal)."
    )
  }

  if (is.complex(x) && !is.matrix(x)) {
    cli::cli_abort(
      c(
        "{.arg x} is a complex vector, which is not a valid raw signal.",
        "i" = "Pass the STFT output as a complex matrix (freq x time), \\
               e.g. {.code signal::specgram(x)$S}."
      )
    )
  }

  is_raw_signal <- is.numeric(x) && is.vector(x) && !is.matrix(x)

  freq_scale <- match.arg(freq_scale, c("linear", "log"))
  freq_unit <- match.arg(freq_unit, c("Hz", "kHz"))
  time_unit <- match.arg(time_unit, c("s", "ms"))

  if (!is.logical(power) || length(power) != 1L) {
    cli::cli_abort(
      "{.arg power} must be a single logical value (TRUE or FALSE)."
    )
  }
  if (!is.logical(db) || length(db) != 1L) {
    cli::cli_abort("{.arg db} must be a single logical value (TRUE or FALSE).")
  }
  if (!is.logical(palette_reverse) || length(palette_reverse) != 1L) {
    cli::cli_abort(
      "{.arg palette_reverse} must be a single logical value (TRUE or FALSE)."
    )
  }
  if (!is.logical(show_colorbar) || length(show_colorbar) != 1L) {
    cli::cli_abort(
      "{.arg show_colorbar} must be a single logical value (TRUE or FALSE)."
    )
  }
  if (
    !is.numeric(db_range) ||
      length(db_range) != 1L ||
      is.na(db_range) ||
      db_range <= 0
  ) {
    cli::cli_abort(
      "{.arg db_range} must be a single positive number. \\
       Got {.val {db_range}}."
    )
  }
  if (
    !is.numeric(n_colors) ||
      length(n_colors) != 1L ||
      is.na(n_colors) ||
      n_colors < 2L
  ) {
    cli::cli_abort("{.arg n_colors} must be an integer >= 2.")
  }
  n_colors <- as.integer(n_colors)

  # Validate freq_range / time_range / zlim (all must be length-2 increasing)
  validate_range <- function(r, arg) {
    if (is.null(r)) {
      return(invisible(NULL))
    }
    if (!is.numeric(r) || length(r) != 2L || anyNA(r) || r[[1L]] >= r[[2L]]) {
      cli::cli_abort(
        "{.arg {arg}} must be a length-2 numeric vector with {arg}[1] < {arg}[2]. \\
         Got {.val {r}}."
      )
    }
  }
  validate_range(freq_range, "freq_range")
  validate_range(time_range, "time_range")
  validate_range(zlim, "zlim")

  # Auto margins if title is set
  if (is.null(margins)) {
    margins <- DEFAULT_MARGINS
    margins[["right"]] <- "90"
    if (!is.null(title)) {
      margins[["top"]] <- "50"
    }
  }

  # -- 2. STFT (raw signal) or matrix input ------------------------------------
  if (is_raw_signal) {
    if (is.null(sample_rate)) {
      cli::cli_abort(
        c(
          "{.arg sample_rate} is required when {.arg x} is a raw signal vector.",
          "i" = "Provide the sampling rate in Hz, e.g. {.code sample_rate = 44100}."
        )
      )
    }
    if (
      !is.numeric(sample_rate) ||
        length(sample_rate) != 1L ||
        is.na(sample_rate) ||
        sample_rate <= 0
    ) {
      cli::cli_abort(
        "{.arg sample_rate} must be a single positive number (Hz). \\
         Got {.val {sample_rate}}."
      )
    }
    if (!is.numeric(n_fft) || length(n_fft) != 1L || is.na(n_fft)) {
      cli::cli_abort(
        "{.arg n_fft} must be a single integer >= 2. Got {.val {n_fft}}."
      )
    }
    n_fft <- as.integer(n_fft)
    if (n_fft < 2L) {
      cli::cli_abort("{.arg n_fft} must be >= 2. Got {n_fft}.")
    }
    if (n_fft > length(x)) {
      cli::cli_abort(
        "{.arg n_fft} ({n_fft}) exceeds the signal length ({length(x)}). \\
         Reduce {.arg n_fft} or supply a longer signal."
      )
    }

    win_vec <- .specgram_window(window, n_fft)
    if (
      !is.null(overlap) &&
        (!is.numeric(overlap) || length(overlap) != 1L || is.na(overlap))
    ) {
      cli::cli_abort(
        "{.arg overlap} must be a single non-negative integer or NULL. \\
         Got {.val {overlap}}."
      )
    }
    ovlp <- as.integer(overlap %||% ceiling(n_fft / 2L))
    if (ovlp < 0L || ovlp >= n_fft) {
      cli::cli_abort(
        "{.arg overlap} must be in [0, n_fft - 1] = [0, {n_fft - 1L}]. \\
         Got {ovlp}."
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
    # Pre-computed matrix (real or complex)
    if (!is.matrix(x)) {
      cli::cli_abort(
        "{.arg x} must be a numeric or complex matrix (freq x time) \\
         or a numeric vector (raw signal)."
      )
    }
    S <- x
    n_freq <- nrow(S)
    n_time <- ncol(S)

    if (!is.null(time)) {
      if (!is.numeric(time) || length(time) != n_time || anyNA(time)) {
        cli::cli_abort(
          "{.arg time} must be a numeric vector of length {n_time} \\
           (= ncol(x)) with no NAs."
        )
      }
      time_s <- time
    } else {
      time_s <- seq_len(n_time) - 1L
    }

    if (!is.null(frequency)) {
      if (
        !is.numeric(frequency) ||
          length(frequency) != n_freq ||
          anyNA(frequency)
      ) {
        cli::cli_abort(
          "{.arg frequency} must be a numeric vector of length {n_freq} \\
           (= nrow(x)) with no NAs."
        )
      }
      freq_hz <- frequency
    } else {
      freq_hz <- seq_len(n_freq) - 1L
    }
  }

  # -- 3. Magnitude / power spectrum -------------------------------------------
  # For complex input (raw STFT), apply |S| or |S|^2.
  # For a real matrix passed directly, treat as already computed and skip.
  if (is.complex(S)) {
    amp <- Mod(S)
    spec <- if (power) amp^2 else amp
  } else {
    spec <- S
  }

  # -- 4. dB conversion --------------------------------------------------------
  if (db) {
    if (!is.complex(S) && any(spec < 0, na.rm = TRUE)) {
      cli::cli_warn(
        c(
          "Pre-computed matrix contains negative values; {.code db = TRUE} \\
           will produce {.val NaN} for those entries.",
          "i" = "Pass {.code db = FALSE} if the matrix is already in dB, \\
                 or ensure values are non-negative before dB conversion."
        )
      )
    }
    eps <- .Machine[["double.eps"]]
    # 10*log10 for power, 20*log10 for amplitude - see @param power docs.
    spec <- if (power) 10 * log10(spec + eps) else 20 * log10(spec + eps)

    peak <- max(spec, na.rm = TRUE)
    spec <- pmax(spec, peak - db_range)
  }

  # -- 5. Log-frequency: drop DC (0 Hz, undefined in log scale) ---------------
  if (freq_scale == "log") {
    keep_f <- freq_hz > 0
    if (!any(keep_f)) {
      cli::cli_abort(
        "No positive-frequency bins found for {.code freq_scale = 'log'}. \\
         This should not happen; please check the signal or frequency vector."
      )
    }
    spec <- spec[keep_f, , drop = FALSE]
    freq_hz <- freq_hz[keep_f]
  }

  # -- 6. Range filtering ------------------------------------------------------
  if (!is.null(freq_range)) {
    keep_f <- freq_hz >= freq_range[[1L]] & freq_hz <= freq_range[[2L]]
    if (!any(keep_f)) {
      cli::cli_abort(
        "No frequency bins fall within {.arg freq_range} \\
         [{freq_range[1]}, {freq_range[2]}] Hz. \\
         Check {.arg freq_range} against the signal's Nyquist limit \\
         ({max(freq_hz)} Hz)."
      )
    }
    spec <- spec[keep_f, , drop = FALSE]
    freq_hz <- freq_hz[keep_f]
  }
  if (!is.null(time_range)) {
    keep_t <- time_s >= time_range[[1L]] & time_s <= time_range[[2L]]
    if (!any(keep_t)) {
      cli::cli_abort(
        "No time frames fall within {.arg time_range} \\
         [{time_range[1]}, {time_range[2]}] s. \\
         Check {.arg time_range} against the signal duration \\
         ({max(time_s)} s)."
      )
    }
    spec <- spec[, keep_t, drop = FALSE]
    time_s <- time_s[keep_t]
  }

  # -- 7. Log-frequency resampling ---------------------------------------------
  # ECharts heatmap sizing is driven by getBandWidth(), which for category axes
  # divides canvas pixels by the number of categories - giving correctly sized
  # cells.  For value axes it divides by the axis value range, producing cells
  # orders of magnitude too large or too small.  We therefore always use
  # category axes (integer-indexed data), matching draw_heatmap's approach.
  #
  # For freq_scale = "log", we resample the spectrum matrix onto logarithmically
  # spaced frequency bins (same count as the original) so the rendered rows are
  # visually equidistant on a log scale.
  if (freq_scale == "log") {
    n_log_bins <- length(freq_hz)
    freq_log <- exp(
      seq(
        log(freq_hz[[1L]]),
        log(freq_hz[[n_log_bins]]),
        length.out = n_log_bins
      )
    )
    spec <- matrix(
      apply(spec, 2L, function(col) {
        approx(freq_hz, col, xout = freq_log, rule = 2L)[["y"]]
      }),
      nrow = n_log_bins
    )
    freq_hz <- freq_log
  }

  # -- 8. Unit conversion (display axes) ----------------------------------------
  time_disp <- if (time_unit == "ms") time_s * 1000 else time_s
  freq_disp <- if (freq_unit == "kHz") freq_hz / 1000 else freq_hz

  # -- 9. Colour-scale limits --------------------------------------------------
  if (is.null(zlim)) {
    if (!any(is.finite(spec))) {
      cli::cli_abort(
        c(
          "Cannot determine colour-scale limits: spectrogram contains no finite values.",
          "i" = "Check the input signal, or supply {.arg zlim} explicitly."
        )
      )
    }
    zlim <- range(spec, na.rm = TRUE)
  }

  # -- 10. Palette --------------------------------------------------------------
  pal <- .spectrogram_palette(palette, n_colors, palette_reverse, zlim)
  dark_pal <- attr(pal, "dark_variant")

  # -- 11. Performance warning for very large spectrograms ---------------------
  n_freq_disp <- nrow(spec)
  n_time_disp <- ncol(spec)
  n_cells <- n_freq_disp * n_time_disp
  if (n_cells > 500000L) {
    cli::cli_warn(
      c(
        "Spectrogram has {n_cells} cells; browser rendering may be slow.",
        "i" = "Consider reducing {.arg n_fft} or the signal length."
      )
    )
  }

  # -- 12. Axis label defaults and formatted label vectors ---------------------
  time_digits <- if (time_unit == "ms") 1L else 3L
  freq_digits <- if (freq_unit == "kHz") 3L else 1L
  val_digits <- if (db) 1L else 3L

  xlab <- xlab %||% paste0("Time (", time_unit, ")")
  ylab <- ylab %||% paste0("Frequency (", freq_unit, ")")
  colorbar_title <- colorbar_title %||%
    if (db) {
      "dB"
    } else if (power) {
      "Power"
    } else {
      "Amplitude"
    }

  # String label vectors for category axis data and tooltip lookup.
  time_fmt <- formatC(time_disp, digits = time_digits, format = "f")
  freq_fmt <- formatC(freq_disp, digits = freq_digits, format = "f")

  # -- 13. Flatten to [col_idx, row_idx, value] triples -----------------------
  # Category-axis convention: x = time column index (0-based),
  #                           y = frequency row index (0-based).
  # Low frequencies (row 0) map to category 0, which ECharts places at the
  # bottom of a category y-axis - the conventional spectrogram orientation.
  col_idx <- rep(seq_len(n_time_disp) - 1L, each = n_freq_disp)
  row_idx <- rep(seq_len(n_freq_disp) - 1L, times = n_time_disp)
  vals <- as.vector(spec)
  vals[!is.finite(vals)] <- NA_real_
  data_list <- mapply(
    function(ci, ri, v) list(ci, ri, if (is.na(v)) NULL else v),
    col_idx,
    row_idx,
    vals,
    SIMPLIFY = FALSE
  )

  # -- 14. Tooltip formatter ---------------------------------------------------
  # Embed label arrays so the formatter can look up display values by index.
  time_json <- jsonlite::toJSON(time_fmt, auto_unbox = FALSE)
  freq_json <- jsonlite::toJSON(freq_fmt, auto_unbox = FALSE)

  tooltip_fmt <- htmlwidgets::JS(paste0(
    "(function(){",
    "var t=",
    time_json,
    ";var f=",
    freq_json,
    ";",
    "return function(p){",
    "if(!p.value||p.value[2]===null||p.value[2]===undefined)return'N/A';",
    "return 'Time:\\u00A0'+t[p.value[0]]+'\\u00A0",
    time_unit,
    "<br/>Freq:\\u00A0'+f[p.value[1]]+'\\u00A0",
    freq_unit,
    "<br/>'+p.value[2].toFixed(",
    val_digits,
    ")+'\\u00A0",
    colorbar_title,
    "';",
    "}})()"
  ))

  # -- 15. Assemble ECharts option ---------------------------------------------
  opt <- EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "item", formatter = tooltip_fmt),
    grid = resolve_margins(margins),
    x_axis = Axis(
      type = "category",
      data = as.list(time_fmt),
      name = xlab,
      name_location = if (!is.null(xlab)) "middle" else NULL,
      boundary_gap = TRUE,
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE)
    ),
    y_axis = Axis(
      type = "category",
      data = as.list(freq_fmt),
      name = ylab,
      name_location = if (!is.null(ylab)) "middle" else NULL,
      boundary_gap = TRUE,
      split_area = SplitArea(show = FALSE),
      axis_line = AxisLine(show = FALSE)
    ),
    visual_map = VisualMap(
      type = "continuous",
      min = zlim[[1L]],
      max = zlim[[2L]],
      precision = val_digits,
      calculable = TRUE,
      show = show_colorbar,
      orient = "vertical",
      right = "right",
      top = "middle",
      in_range = list(color = as.list(pal))
    ),
    series = list(HeatmapSeries(data = data_list))
  )

  # Pass diverging dark-theme palette variant to JS (mirrors draw_heatmap)
  meta <- list()
  if (!is.null(dark_pal)) {
    meta[["colorLight"]] <- as.list(pal)
    meta[["colorDark"]] <- as.list(dark_pal)
  }

  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    filename = filename,
    meta = meta
  )
}
