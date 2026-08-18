# config_spectrogram.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The spectrogram's config. Bound data is a structure -- either a raw signal
# vector or a precomputed frequency-by-time matrix -- so it declares no column
# names. Its render hints are the two theme-matched color arrays the browser
# picks between by dark mode, which is the interface's business and never
# serialized.

# %% SpectrogramConfig ----
#' Spectrogram Configuration
#'
#' A serializable description of a spectrogram. Build one with
#' [setup_SpectrogramConfig()] rather than calling this constructor directly.
#'
#' The bound data is either a numeric signal vector, which is transformed with a
#' short-time Fourier transform, or a precomputed frequency-by-time matrix.
#'
#' @param sample_rate Optional Numeric `[0, Inf)`: Signal sample rate in Hz.
#' @param n_fft Integer `[2, Inf)`: FFT window length.
#' @param window Character: Window function.
#' @param overlap Optional Numeric `[0, 1)`: Fractional window overlap.
#' @param power Logical: Use the power spectrum rather than magnitude.
#' @param db Logical: Convert to decibels.
#' @param db_range Numeric `[0, Inf)`: Dynamic range in dB below the peak.
#' @param freq_scale Character \{"linear", "log", "mel"\}: Frequency axis scale.
#' @param freq_range,time_range Optional Numeric: Axis limits, length 2.
#' @param freq_unit,time_unit Character: Axis units, for the labels.
#' @param colormap Character: Continuous color scheme, or two or more hex
#'   colors defining one.
#' @param colormap_reverse Logical: Reverse the color scheme.
#' @param n_colors Integer `[2, Inf)`: Number of discrete colors.
#' @param zlim Optional Numeric: Color-scale limits, length 2.
#' @param show_colorbar Logical: Draw the color bar.
#' @param colorbar_title Optional Character: Color bar title.
#' @param xlab,ylab Optional Character: Axis labels.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Override the auto-computed margins, per side.
#' @inheritParams ChartConfig
#'
#' @return `SpectrogramConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_SpectrogramConfig(n_fft = 512L)@type
SpectrogramConfig <- new_class(
  name = "SpectrogramConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("spectrogram"),
    # -- semantics: the transform ------------------------------------------
    sample_rate = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = "Signal sample rate in Hz."
    ),
    n_fft = prop_integer(256L, min = 2L, description = "FFT window length."),
    window = prop_string("hanning", description = "Window function."),
    overlap = prop_float(
      NULL,
      min = 0,
      exclusive_max = 1,
      nullable = TRUE,
      description = "Fractional window overlap."
    ),
    power = prop_boolean(
      TRUE,
      description = "Use the power spectrum rather than magnitude."
    ),
    db = prop_boolean(TRUE, description = "Convert to decibels."),
    db_range = prop_float(
      80,
      min = 0,
      description = "Dynamic range in dB below the peak."
    ),
    freq_scale = prop_string(
      "linear",
      enum = c("linear", "log", "mel"),
      description = "Frequency axis scale."
    ),
    freq_range = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Frequency axis limits."
    ),
    time_range = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Time axis limits."
    ),
    # -- appearance --------------------------------------------------------
    freq_unit = prop_string("Hz", description = "Frequency axis unit."),
    time_unit = prop_string("s", description = "Time axis unit."),
    colormap = prop_string(
      "magma",
      vector = TRUE,
      description = paste(
        "Continuous color scheme, or two or more hex colors defining one."
      )
    ),
    colormap_reverse = prop_boolean(
      FALSE,
      description = "Reverse the color scheme."
    ),
    n_colors = prop_integer(
      256L,
      min = 2L,
      description = "Number of discrete colors."
    ),
    zlim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Color-scale limits."
    ),
    show_colorbar = prop_boolean(TRUE, description = "Draw the color bar."),
    colorbar_title = prop_string(
      NULL,
      nullable = TRUE,
      description = "Color bar title."
    ),
    xlab = prop_string(NULL, nullable = TRUE, description = "X axis label."),
    ylab = prop_string(NULL, nullable = TRUE, description = "Y axis label."),
    margin_top = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Top margin override in pixels."
    ),
    margin_right = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Right margin override in pixels."
    ),
    margin_bottom = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Bottom margin override in pixels."
    ),
    margin_left = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Left margin override in pixels."
    )
  )
) # /rtemis.draw::SpectrogramConfig


# %% SPECTROGRAM_ORIGIN_NAMES ----
SPECTROGRAM_ORIGIN_NAMES <- setdiff(
  names(SpectrogramConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_SpectrogramConfig ----
#' Set up a Spectrogram Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams SpectrogramConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [SpectrogramConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' m <- matrix(abs(rnorm(64 * 20)) + 1, nrow = 64)
#' draw(setup_SpectrogramConfig(), data = m)
setup_SpectrogramConfig <- function(
  sample_rate = NULL,
  n_fft = 256L,
  window = "hanning",
  overlap = NULL,
  power = TRUE,
  db = TRUE,
  db_range = 80,
  freq_scale = "linear",
  freq_range = NULL,
  time_range = NULL,
  freq_unit = "Hz",
  time_unit = "s",
  colormap = "magma",
  colormap_reverse = FALSE,
  n_colors = 256L,
  zlim = NULL,
  show_colorbar = TRUE,
  colorbar_title = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), SPECTROGRAM_ORIGIN_NAMES)
  SpectrogramConfig(
    sample_rate = sample_rate,
    n_fft = as.integer(n_fft),
    window = window,
    overlap = overlap,
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
    n_colors = as.integer(n_colors),
    zlim = zlim,
    show_colorbar = show_colorbar,
    colorbar_title = colorbar_title,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_SpectrogramConfig


# %% resolve.SpectrogramConfig ----
# Nothing to derive: the axes are computed by the transform, and their labels
# are built by the builder from the units.
method(resolve, SpectrogramConfig) <- function(config, data = NULL, ...) {
  config
}


# %% spectrogram_built ----
#' Build a spectrogram's option and render hints together
#'
#' @param config [SpectrogramConfig]: The chart configuration.
#' @param data Optional Numeric or Matrix: Signal or spectrogram matrix.
#'
#' @return Named list: `option` and `render`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spectrogram_built <- function(config, data = NULL) {
  bound <- config_data(config, data)
  config <- resolve(config, data = bound)
  spectrogram_option(
    x = bound,
    sample_rate = config@sample_rate,
    n_fft = config@n_fft,
    window = config@window,
    overlap = config@overlap,
    power = config@power,
    db = config@db,
    db_range = config@db_range,
    freq_scale = config@freq_scale,
    freq_range = config@freq_range,
    freq_unit = config@freq_unit,
    time_range = config@time_range,
    time_unit = config@time_unit,
    colormap = config@colormap,
    colormap_reverse = config@colormap_reverse,
    n_colors = config@n_colors,
    zlim = config@zlim,
    show_colorbar = config@show_colorbar,
    colorbar_title = config@colorbar_title,
    title = config@title,
    xlab = config@xlab,
    ylab = config@ylab,
    margins = config_margins(config)
  )
} # /rtemis.draw::spectrogram_built


# %% compile.SpectrogramConfig ----
method(compile, SpectrogramConfig) <- function(config, data = NULL, ...) {
  spectrogram_built(config, data)[["option"]]
}


# %% draw.SpectrogramConfig ----
# Overrides the generic method so the two theme-matched color arrays reach the
# browser, which picks between them by dark mode.
method(draw, SpectrogramConfig) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL
) {
  built <- spectrogram_built(option, data)
  draw(
    built[["option"]],
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = built[["render"]][["meta"]],
    ...
  )
}
