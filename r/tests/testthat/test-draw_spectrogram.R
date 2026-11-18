test_that(".specgram_window returns correct vector for named windows", {
  for (nm in c("hanning", "hamming", "blackman", "bartlett", "rectangular")) {
    w <- rtemis.draw:::.specgram_window(nm, 64L)
    expect_length(w, 64L)
    expect_true(is.numeric(w))
  }
})

test_that(".specgram_window accepts a pre-built numeric vector", {
  w <- rep(1, 32L)
  expect_identical(rtemis.draw:::.specgram_window(w, 32L), w)
})

test_that(".specgram_window errors on unknown name", {
  expect_error(
    rtemis.draw:::.specgram_window("bogus", 64L),
    class = "rtemis_error"
  )
})

test_that(".specgram_window errors when custom vector length mismatches n", {
  expect_error(
    rtemis.draw:::.specgram_window(rep(1, 10L), 32L),
    class = "rtemis_error"
  )
})

# ---------------------------------------------------------------------------
# draw_spectrogram — input validation
# ---------------------------------------------------------------------------

test_that("draw_spectrogram errors when x is not numeric", {
  expect_error(draw_spectrogram("hello"), class = "rtemis_error")
})

test_that("draw_spectrogram errors when x is a complex vector (not matrix)", {
  expect_error(
    draw_spectrogram(complex(real = 1:10, imaginary = 1:10)),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors when raw signal missing sample_rate", {
  sig <- rnorm(1000)
  expect_error(draw_spectrogram(sig), class = "rtemis_error")
})

test_that("draw_spectrogram errors when sample_rate is non-positive", {
  sig <- rnorm(1000)
  expect_error(draw_spectrogram(sig, sample_rate = 0), class = "rtemis_error")
  expect_error(
    draw_spectrogram(sig, sample_rate = -100),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors when n_fft exceeds signal length", {
  sig <- rnorm(100)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, n_fft = 200L),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on invalid overlap", {
  sig <- rnorm(1000)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, n_fft = 64L, overlap = 64L),
    class = "rtemis_error"
  )
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, n_fft = 64L, overlap = -1L),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on bad db_range", {
  sig <- rnorm(1000)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, db_range = -10),
    class = "rtemis_error"
  )
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, db_range = 0),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on bad freq_range", {
  sig <- rnorm(1000)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, freq_range = c(500, 200)),
    class = "rtemis_error"
  )
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, freq_range = c(1, 2, 3)),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors when freq_range excludes all bins", {
  sig <- rnorm(1000)
  # 1000 Hz sample rate → Nyquist at 500 Hz; requesting above that
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, freq_range = c(600, 800)),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on bad time_range", {
  sig <- rnorm(1000)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, time_range = c(2, 1)),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on bad zlim", {
  sig <- rnorm(1000)
  expect_error(
    draw_spectrogram(sig, sample_rate = 1000, zlim = c(10, 5)),
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on mismatched time vector for matrix input", {
  mat <- matrix(abs(rnorm(128 * 50)), nrow = 128, ncol = 50)
  expect_error(
    draw_spectrogram(mat, time = seq_len(30)), # wrong length
    class = "rtemis_error"
  )
})

test_that("draw_spectrogram errors on mismatched frequency vector for matrix input", {
  mat <- matrix(abs(rnorm(128 * 50)), nrow = 128, ncol = 50)
  expect_error(
    draw_spectrogram(mat, frequency = seq_len(10)), # wrong length
    class = "rtemis_error"
  )
})

# ---------------------------------------------------------------------------
# draw_spectrogram — ECharts option structure
# ---------------------------------------------------------------------------

test_that("draw_spectrogram uses category axes and integer-indexed data", {
  # ECharts heatmap on cartesian2d REQUIRES category axes (throws in dev mode
  # otherwise). Data must be [col_idx, row_idx, value] 0-based integers.
  skip_if_not_installed("signal")
  sig <- signal::chirp(seq(0, 0.5, by = 1 / 4000), 100, 0.5, 500)
  w <- draw_spectrogram(sig, sample_rate = 4000)
  expect_identical(w[["x"]][["build"]][["type"]], "spectrogram")
  opt <- raw_option(w)

  expect_identical(opt[["xAxis"]][["type"]], "category")
  expect_identical(opt[["yAxis"]][["type"]], "category")

  # First data point: [0, 0, value] — both indices are 0
  first <- opt[["series"]][[1L]][["data"]][[1L]]
  expect_equal(first[[1L]], 0)
  expect_equal(first[[2L]], 0)
})

test_that("draw_spectrogram category axis label count matches matrix dims", {
  skip_if_not_installed("signal")
  sig <- signal::chirp(seq(0, 0.5, by = 1 / 4000), 100, 0.5, 500)
  w <- draw_spectrogram(sig, sample_rate = 4000, n_fft = 64L)
  opt <- raw_option(w)

  n_time <- length(opt[["xAxis"]][["data"]])
  n_freq <- length(opt[["yAxis"]][["data"]])
  n_data <- length(opt[["series"]][[1L]][["data"]])

  expect_equal(n_data, n_time * n_freq)
})

# ---------------------------------------------------------------------------
# draw_spectrogram — successful output
# ---------------------------------------------------------------------------

test_that("draw_spectrogram returns an htmlwidget from a raw signal", {
  skip_if_not_installed("signal")
  sig <- signal::chirp(seq(0, 1, by = 1 / 4000), 100, 1, 500)
  w <- draw_spectrogram(sig, sample_rate = 4000)
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram returns an htmlwidget from a pre-computed matrix", {
  mat <- matrix(runif(64 * 40, min = -80, max = 0), nrow = 64, ncol = 40)
  t <- seq(0, 1, length.out = 40)
  f <- seq(0, 2000, length.out = 64)
  w <- draw_spectrogram(mat, time = t, frequency = f, db = FALSE)
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram works with log freq_scale (drops DC)", {
  skip_if_not_installed("signal")
  sig <- rnorm(4000)
  w <- draw_spectrogram(sig, sample_rate = 4000, freq_scale = "log")
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram works with diverging palette", {
  # Simulate EEG-like signed amplitude matrix
  mat <- matrix(rnorm(64 * 40, sd = 50), nrow = 64, ncol = 40)
  w <- draw_spectrogram(mat, db = FALSE, power = FALSE, colormap = "diverging")
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram works with kHz and ms units", {
  skip_if_not_installed("signal")
  sig <- signal::chirp(seq(0, 1, by = 1 / 8000), 200, 1, 3000)
  w <- draw_spectrogram(
    sig,
    sample_rate = 8000,
    freq_unit = "kHz",
    time_unit = "ms"
  )
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram works with custom hex palette", {
  mat <- matrix(runif(64 * 40), nrow = 64, ncol = 40)
  w <- draw_spectrogram(
    mat,
    db = FALSE,
    colormap = c("#000004", "#51127c", "#b5367a", "#fb8861", "#fcfdbf")
  )
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram applies freq_range and time_range correctly", {
  skip_if_not_installed("signal")
  sig <- rnorm(8000)
  w <- draw_spectrogram(
    sig,
    sample_rate = 8000,
    freq_range = c(100, 2000),
    time_range = c(0.1, 0.8)
  )
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram handles amplitude spectrum (power = FALSE)", {
  skip_if_not_installed("signal")
  sig <- rnorm(4000)
  w <- draw_spectrogram(sig, sample_rate = 4000, power = FALSE, db = TRUE)
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_spectrogram accepts a complex matrix (raw STFT)", {
  skip_if_not_installed("signal")
  sig <- rnorm(4000)
  sg <- signal::specgram(sig, n = 128L, Fs = 4000)
  w <- draw_spectrogram(sg[["S"]], time = sg[["t"]], frequency = sg[["f"]])
  expect_s3_class(w, "htmlwidget")
})
test_that("the diverging scale centers on the theme background and reverses", {
  values <- matrix(c(-2, -1, 0, 1, 2, 2), 2L)
  colors <- function(widget) {
    unlist(raw_option(widget)[["visualMap"]][["inRange"]][["color"]])
  }
  forward <- draw_spectrogram(
    values,
    db = FALSE,
    colormap = "diverging",
    n_colors = 5L,
    theme = theme_light()
  )
  backward <- draw_spectrogram(
    values,
    db = FALSE,
    colormap = "diverging",
    n_colors = 5L,
    colormap_reverse = TRUE,
    theme = theme_light()
  )
  expect_identical(colors(backward), rev(colors(forward)))
  expect_identical(
    toupper(colors(forward)[[3L]]),
    toupper(theme_light()@background_color)
  )
  expect_identical(
    unlist(raw_option(draw_spectrogram(
      values,
      db = FALSE,
      colormap = c("#000000", "#ffffff"),
      n_colors = 3L,
      colormap_reverse = TRUE
    ))[["visualMap"]][["inRange"]][["color"]]),
    c("#FFFFFF", "#7F7F7F", "#000000")
  )
  expect_error(draw_spectrogram(values, colormap = "jet"), "colormap")
})

test_that("spectrogram data validates scalar controls and matrix dimensions", {
  values <- matrix(1:6, 2L)
  for (name in c("power", "db")) {
    for (bad in list(1, c(TRUE, FALSE))) {
      expect_error(
        do.call(
          spectrogram_data,
          c(list(x = values), setNames(list(bad), name))
        ),
        name,
        class = "rtemis_type_error"
      )
    }
  }
  for (name in c("colormap_reverse", "show_colorbar")) {
    expect_error(do.call(
      draw_spectrogram,
      c(list(x = values), setNames(list(1), name))
    ))
  }
  expect_error(spectrogram_data(values, n_colors = 1), "n_colors")
  expect_error(spectrogram_data(values, time = c(1, NA, 3)), "time")
  expect_error(
    spectrogram_data(array(1:8, c(2L, 2L, 2L))),
    "matrix",
    class = "rtemis_type_error"
  )
  expect_error(
    spectrogram_data(values, freq_scale = "log", frequency = c(-1, 0)),
    "above 0 Hz"
  )
  expect_error(
    spectrogram_data(values, time_range = c(10, 20)),
    "No time frames"
  )
  expect_error(
    spectrogram_data(matrix(NA_real_, 2L, 3L), db = FALSE),
    "no finite values"
  )
})

test_that("spectrogram data validates raw-signal sampling and window controls", {
  signal <- sin(seq(0, 4 * pi, length.out = 32L))
  expect_error(spectrogram_data(signal, sample_rate = 0), "sample_rate")
  for (bad in list("eight", NA_real_, c(4, 8))) {
    expect_error(
      spectrogram_data(signal, sample_rate = 32, n_fft = bad),
      "n_fft"
    )
  }
  expect_error(
    spectrogram_data(signal, sample_rate = 32, n_fft = 1),
    "n_fft",
    class = "rtemis_range_error"
  )
  for (bad in list("half", NA_real_, c(1, 2))) {
    expect_error(
      spectrogram_data(signal, sample_rate = 32, n_fft = 8, overlap = bad),
      "overlap"
    )
  }
})

test_that("spectrogram titles reserve top space and time filtering preserves cells", {
  option <- raw_option(draw_spectrogram(
    matrix(1:6, 2L),
    db = FALSE,
    title = "Spectrum",
    time = c(0, 1, 2),
    time_range = c(.5, 2)
  ))
  expect_equal(option[["grid"]][["top"]], 50)
  expect_identical(option[["title"]][["text"]], "Spectrum")
  expect_length(option[["xAxis"]][["data"]], 2L)
  expect_equal(
    vapply(option[["series"]][[1L]][["data"]], `[[`, numeric(1), 3L),
    c(3, 4, 5, 6)
  )
})

test_that("a config's fractional overlap reaches the transform in samples", {
  skip_if_not_installed("signal")
  sig <- sin(seq(0, 40 * pi, length.out = 512L))
  config <- setup_SpectrogramConfig(
    sample_rate = 512,
    n_fft = 64L,
    overlap = .75
  )
  data <- jsonlite::fromJSON(compile(config, sig)@data)
  expect_length(
    data[["time"]],
    length(signal::specgram(
      sig,
      n = 64L,
      Fs = 512,
      overlap = 48L
    )[["t"]])
  )
  expect_equal(
    draw_spectrogram(sig, sample_rate = 512, n_fft = 64L, overlap = 48L)[[
      "x"
    ]][["build"]][["data"]],
    compile(config, sig)@data
  )
})
