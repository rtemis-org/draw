# test-draw_harmonograph.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% harmonograph_params ----

test_that("harmonograph_params draws 12 parameters within their ranges", {
  params <- rtemis.draw:::harmonograph_params(seed = 1L)
  expect_named(params, c("frequency", "damping", "phase"))
  expect_true(all(lengths(params) == 4L))
  # jitter() on a single value perturbs by at most a fiftieth of it.
  near <- pmin(abs(params$frequency - 2) / 2, abs(params$frequency - 3) / 3)
  expect_true(all(near <= 1 / 50))
  expect_true(all(params$damping >= 0 & params$damping <= 1e-2))
  expect_true(all(params$phase >= 0 & params$phase <= pi))
})

test_that("harmonograph_params reproduces the rtemis 0.98 draw sequence", {
  # The sequence of mplot3_harmonograph() in rtemis 0.98.
  set.seed(9)
  f <- vapply(1:4, function(i) jitter(sample(c(2, 3), 1)), numeric(1))
  d <- vapply(1:4, function(i) runif(1, 0, 1e-02), numeric(1))
  p <- vapply(1:4, function(i) runif(1, 0, pi), numeric(1))
  params <- rtemis.draw:::harmonograph_params(seed = 9L)
  expect_equal(params$frequency, f)
  expect_equal(params$damping, d)
  expect_equal(params$phase, p)
})

test_that("harmonograph_params restores the caller's RNG state", {
  set.seed(42)
  before <- .Random.seed
  rtemis.draw:::harmonograph_params(seed = 9L)
  expect_identical(.Random.seed, before)
})

test_that("harmonograph_params leaves no RNG state where there was none", {
  genv <- globalenv()
  saved <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    get(".Random.seed", envir = genv)
  }
  on.exit(
    if (!is.null(saved)) assign(".Random.seed", saved, envir = genv),
    add = TRUE
  )
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    rm(".Random.seed", envir = genv)
  }
  rtemis.draw:::harmonograph_params(seed = 9L)
  expect_false(exists(".Random.seed", envir = genv, inherits = FALSE))
})

test_that("supplied vectors replace only their drawn counterparts", {
  drawn <- rtemis.draw:::harmonograph_params(seed = 17L)
  phase <- c(0, 1, 2, 3)
  params <- rtemis.draw:::harmonograph_params(phase = phase, seed = 17L)
  expect_identical(params$phase, phase)
  expect_identical(params$frequency, drawn$frequency)
  expect_identical(params$damping, drawn$damping)
})

test_that("harmonograph_params rejects malformed vectors", {
  expect_error(
    rtemis.draw:::harmonograph_params(frequency = c(2, 3)),
    class = "rtemis_type_error"
  )
  expect_error(
    rtemis.draw:::harmonograph_params(phase = c(0, NA, 0, 0)),
    class = "rtemis_type_error"
  )
  expect_error(
    rtemis.draw:::harmonograph_params(frequency = letters[1:4]),
    class = "rtemis_type_error"
  )
  expect_error(
    rtemis.draw:::harmonograph_params(damping = c(0, -0.1, 0, 0)),
    class = "rtemis_value_error"
  )
})


# %% harmonograph_curve ----

test_that("harmonograph_curve sums two damped sinusoids per axis", {
  params <- list(
    frequency = c(2, 3, 3, 2),
    damping = c(0, 0.01, 0.02, 0),
    phase = c(0, pi / 2, pi / 4, 0)
  )
  t <- c(0, 1, 10)
  curve <- rtemis.draw:::harmonograph_curve(t, params)
  expect_equal(curve$x, sin(2 * t) + exp(-0.01 * t) * sin(3 * t + pi / 2))
  expect_equal(curve$y, exp(-0.02 * t) * sin(3 * t + pi / 4) + sin(2 * t))
})


# %% draw_harmonograph ----

# Points of the curve in drawing order, from the overlapping segments, each a
# two-column matrix.
curve_points <- function(series) {
  pts <- do.call(
    rbind,
    lapply(seq_along(series), function(i) {
      data <- series[[i]]$data
      if (i > 1L) data[-1L, , drop = FALSE] else data
    })
  )
  list(x = pts[, 1L], y = pts[, 2L])
}

# Distinct values of one series field, read along a path, across segments.
series_values <- function(series, ...) {
  unique(lapply(series, function(s) s[[c(...)]]))
}

test_that("draw_harmonograph draws silent line segments of one color", {
  w <- draw_harmonograph(seed = 9L, t = seq(1, 50, by = 0.1))
  expect_s3_class(w, "htmlwidget")
  opt <- w$x$option
  expect_identical(series_values(opt$series, "type"), list("line"))
  expect_identical(series_values(opt$series, "name"), list("harmonograph"))
  expect_identical(series_values(opt$series, "showSymbol"), list(FALSE))
  expect_identical(series_values(opt$series, "silent"), list(TRUE))
  expect_equal(series_values(opt$series, "lineStyle", "opacity"), list(0.2))
  expect_equal(series_values(opt$series, "lineStyle", "width"), list(1))
  # color = NULL leaves the theme palette in charge.
  expect_identical(series_values(opt$series, "color"), list(NULL))
  expect_false(opt$animation)
  expect_null(opt$color)
  # One series name: no legend.
  expect_null(opt$legend)
})

test_that("draw_harmonograph segments are bounded in number and continuous", {
  t <- seq(1, 500, by = 0.01)
  series <- draw_harmonograph(seed = 9L, t = t)$x$option$series
  expect_lte(length(series), rtemis.draw:::HARMONOGRAPH_SEGMENTS)
  first <- lapply(series[-1L], function(s) s$data[1L, ])
  last <- lapply(series[-length(series)], function(s) s$data[nrow(s$data), ])
  expect_identical(first, last)
  expect_length(curve_points(series)$x, length(t))
  # Two time points make one segment.
  expect_length(draw_harmonograph(seed = 9L, t = 1:2)$x$option$series, 1L)
})

test_that("draw_harmonograph plots the evaluated curve", {
  t <- seq(1, 20, by = 0.5)
  w <- draw_harmonograph(seed = 26L, t = t)
  params <- rtemis.draw:::harmonograph_params(seed = 26L)
  curve <- rtemis.draw:::harmonograph_curve(t, params)
  pts <- curve_points(w$x$option$series)
  expect_equal(pts$x, round(curve$x, 4L))
  expect_equal(pts$y, round(curve$y, 4L))
})

test_that("draw_harmonograph sends each segment as [x, y] pairs", {
  w <- draw_harmonograph(seed = 9L, t = seq(1, 5, by = 0.5))
  wire <- widget_wire(w)
  data <- wire$option$series[[1]]$data
  expect_true(all(lengths(data) == 2L))
  expect_equal(unlist(data[[1]]), unname(w$x$option$series[[1]]$data[1L, ]))
})

test_that("draw_harmonograph hides equal-span axes in a square box", {
  w <- draw_harmonograph(seed = 9L, t = seq(1, 50, by = 0.1), pad = 0)
  opt <- w$x$option
  expect_false(opt$xAxis$show)
  expect_false(opt$yAxis$show)
  x_span <- opt$xAxis$max - opt$xAxis$min
  y_span <- opt$yAxis$max - opt$yAxis$min
  expect_equal(x_span, y_span)
  pts <- curve_points(opt$series)
  x <- pts$x
  y <- pts$y
  expect_true(all(x >= opt$xAxis$min & x <= opt$xAxis$max))
  expect_true(all(y >= opt$yAxis$min & y <= opt$yAxis$max))
  expect_equal(x_span, max(diff(range(x)), diff(range(y))), tolerance = 1e-3)
  expect_equal(w$x$aspect$ratio, 1)
})

test_that("draw_harmonograph passes appearance arguments through", {
  w <- draw_harmonograph(
    seed = 9L,
    t = seq(1, 10, by = 0.1),
    color = "#ff6600",
    alpha = 0.6,
    line_width = 2,
    title = "Harmonograph"
  )
  opt <- w$x$option
  expect_identical(series_values(opt$series, "color"), list("#ff6600"))
  expect_equal(series_values(opt$series, "lineStyle", "opacity"), list(0.6))
  expect_equal(series_values(opt$series, "lineStyle", "width"), list(2))
  expect_identical(opt$title$text, "Harmonograph")
})

test_that("draw_harmonograph rejects invalid arguments", {
  expect_error(draw_harmonograph(t = 1), class = "rtemis_type_error")
  expect_error(draw_harmonograph(t = c(1, Inf)), class = "rtemis_type_error")
  expect_error(draw_harmonograph(alpha = 2), class = "rtemis_input_error")
  expect_error(draw_harmonograph(line_width = 0), class = "rtemis_input_error")
  expect_error(draw_harmonograph(color = c("red", "blue")))
})
