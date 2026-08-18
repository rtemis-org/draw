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
# .spectrogram_palette
# ---------------------------------------------------------------------------

test_that(".spectrogram_palette returns correct length for viridis names", {
  for (nm in c(
    "magma",
    "inferno",
    "plasma",
    "viridis",
    "cividis",
    "mako",
    "rocket",
    "turbo"
  )) {
    pal <- rtemis.draw:::.spectrogram_palette(nm, 128L, FALSE, c(0, 1))
    expect_length(pal, 128L)
    expect_true(all(grepl("^#", pal)))
  }
})

test_that(".spectrogram_palette respects reverse flag", {
  fwd <- rtemis.draw:::.spectrogram_palette("magma", 64L, FALSE, c(0, 1))
  rev_pal <- rtemis.draw:::.spectrogram_palette("magma", 64L, TRUE, c(0, 1))
  expect_identical(fwd, rev(rev_pal))
})

test_that(".spectrogram_palette diverging returns dark_variant attribute", {
  pal <- rtemis.draw:::.spectrogram_palette("diverging", 64L, FALSE, c(-1, 1))
  expect_length(pal, 64L)
  dark <- attr(pal, "dark_variant")
  expect_length(dark, 64L)
  # Light and dark variants share the same endpoint colours but differ at the
  # midpoint (white vs dark background), so compare middle elements.
  mid <- as.integer(ceiling(length(pal) / 2L))
  expect_false(identical(pal[[mid]], dark[[mid]]))
})

test_that(".spectrogram_palette diverging symmetrises zlim around 0", {
  # c(-1.5, 2) is asymmetric but both endpoints have |max| = 2, so after
  # symmetrization both calls produce the same palette as c(-2, 2).
  sym <- rtemis.draw:::.spectrogram_palette("diverging", 101L, FALSE, c(-2, 2))
  asym <- rtemis.draw:::.spectrogram_palette(
    "diverging",
    101L,
    FALSE,
    c(-1.5, 2)
  )
  expect_identical(sym, asym)
  expect_identical(
    attr(sym, "dark_variant"),
    attr(asym, "dark_variant")
  )
})

test_that(".spectrogram_palette accepts custom hex vector", {
  pal <- rtemis.draw:::.spectrogram_palette(
    c("#000000", "#ffffff"),
    50L,
    FALSE,
    c(0, 1)
  )
  expect_length(pal, 50L)
})

test_that(".spectrogram_palette errors on unknown name", {
  expect_error(
    rtemis.draw:::.spectrogram_palette("unknown_pal", 64L, FALSE, c(0, 1)),
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
  opt <- w[["x"]][["option"]]

  expect_identical(opt[["xAxis"]][["type"]], "category")
  expect_identical(opt[["yAxis"]][["type"]], "category")

  # First data point: [0, 0, value] — both indices are 0
  first <- opt[["series"]][[1L]][["data"]][[1L]]
  expect_identical(first[[1L]], 0L)
  expect_identical(first[[2L]], 0L)
})

test_that("draw_spectrogram category axis label count matches matrix dims", {
  skip_if_not_installed("signal")
  sig <- signal::chirp(seq(0, 0.5, by = 1 / 4000), 100, 0.5, 500)
  w <- draw_spectrogram(sig, sample_rate = 4000, n_fft = 64L)
  opt <- w[["x"]][["option"]]

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
