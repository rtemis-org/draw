# test-widget.R
# Tests for htmlwidget binding and draw_* functions

test_that("draw() creates an htmlwidget from EChartsOption", {
  opt <- EChartsOption(
    x_axis = Axis(type = "category", data = c("A", "B", "C")),
    y_axis = Axis(type = "value"),
    series = BarSeries(data = c(10, 20, 30))
  )
  w <- draw(opt)
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$xAxis$type, "category")
  expect_equal(w$x$option$series[[1]]$type, "bar")
})

test_that("draw() accepts a plain list", {
  opt <- list(
    xAxis = list(type = "value"),
    yAxis = list(type = "value"),
    series = list(list(type = "scatter", data = list(c(1, 2))))
  )
  w <- draw(opt)
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$xAxis$type, "value")
})

test_that("draw() includes theme when provided", {
  opt <- EChartsOption(series = LineSeries(data = c(1, 2, 3)))
  th <- theme_dark()
  w <- draw(opt, theme = th)
  expect_false(is.null(w$x$theme))
  expect_equal(w$x$theme$backgroundColor, "#181818")
})

test_that("draw() passes renderer option", {
  opt <- EChartsOption(series = LineSeries(data = c(1, 2)))
  w <- draw(opt, renderer = "svg")
  expect_equal(w$x$renderer, "svg")
})

# -- draw_line ------------------------------------------------------------------

test_that("draw_line creates widget from category vectors", {
  w <- draw_line(
    x = c("Mon", "Tue", "Wed"),
    y = c(10, 20, 15)
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$xAxis$type, "category")
  expect_equal(w$x$option$series[[1]]$type, "line")
  # Category axis: data is y values only
  expect_equal(w$x$option$series[[1]]$data, c(10, 20, 15))
})

test_that("draw_line works with numeric x", {
  w <- draw_line(
    x = c(1, 2, 3, 4),
    y = c(10, 20, 15, 25)
  )
  expect_equal(w$x$option$xAxis$type, "value")
  # Value axis: data is [x, y] pairs
  expect_equal(w$x$option$series[[1]]$data[[1]], c(1, 10))
  expect_equal(w$x$option$series[[1]]$data[[4]], c(4, 25))
})

test_that("draw_line handles named list for multiple series", {
  w <- draw_line(
    x = c("A", "B", "C"),
    y = list("Series 1" = c(1, 2, 3), "Series 2" = c(3, 2, 1))
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "Series 1")
  expect_equal(w$x$option$series[[2]]$name, "Series 2")
  # Legend should be present for multiple series
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_line smooth and area options work", {
  w <- draw_line(
    x = c("A", "B"),
    y = c(1, 2),
    smooth = TRUE,
    area = TRUE
  )
  expect_equal(w$x$option$series[[1]]$smooth, TRUE)
  expect_false(is.null(w$x$option$series[[1]]$areaStyle))
})

test_that("draw_line with title", {
  w <- draw_line(x = c("A", "B"), y = c(1, 2), title = "My Line")
  expect_equal(w$x$option$title$text, "My Line")
})

test_that("draw_line points = TRUE is the default and leaves symbols visible", {
  w <- draw_line(x = 1:5, y = 1:5)
  # Default: showSymbol is not set (echarts treats missing as TRUE)
  expect_null(w$x$option$series[[1]]$showSymbol)
})

test_that("draw_line points = FALSE hides symbols across all series", {
  w1 <- draw_line(x = 1:5, y = 1:5, points = FALSE)
  expect_equal(w1$x$option$series[[1]]$showSymbol, FALSE)

  w2 <- draw_line(
    x = c("A", "B", "C"),
    y = list("S1" = c(1, 2, 3), "S2" = c(3, 2, 1)),
    points = FALSE
  )
  expect_equal(w2$x$option$series[[1]]$showSymbol, FALSE)
  expect_equal(w2$x$option$series[[2]]$showSymbol, FALSE)
})

test_that("draw_line blocks attaches markArea to the first series", {
  w <- draw_line(
    x = 1:8,
    y = c(1, 2, 3, 4, 5, 6, 7, 8),
    blocks = factor(c("A", "A", "A", "B", "B", "A", "A", "A")),
    block_color = c(A = "red", B = "blue"),
    block_opacity = 0.3
  )
  s <- w$x$option$series[[1]]
  expect_false(is.null(s$markArea))
  # Three runs: A(1-3), B(4-5), A(6-8). On a value axis, each run's end is
  # extended to the next drawn run's start so bands meet with no gap.
  expect_equal(length(s$markArea$data), 3L)
  expect_equal(s$markArea$data[[1]][[1]]$xAxis, 1)
  expect_equal(s$markArea$data[[1]][[2]]$xAxis, 4) # extended to B's start
  expect_equal(s$markArea$data[[1]][[1]]$itemStyle$color, "red")
  expect_equal(s$markArea$data[[1]][[1]]$itemStyle$opacity, 0.3)
  expect_equal(s$markArea$data[[2]][[1]]$xAxis, 4)
  expect_equal(s$markArea$data[[2]][[2]]$xAxis, 6) # extended to next A's start
  expect_equal(s$markArea$data[[2]][[1]]$itemStyle$color, "blue")
  expect_equal(s$markArea$data[[3]][[1]]$xAxis, 6)
  expect_equal(s$markArea$data[[3]][[2]]$xAxis, 8) # last run: own end
})

test_that("draw_line blocks leaves gap when a skipped run sits between", {
  # B is transparent, so runs either side should NOT extend across it.
  w <- draw_line(
    x = 1:6,
    y = 1:6,
    blocks = c("A", "A", "B", "B", "C", "C"),
    block_color = c(A = "red", B = "transparent", C = "blue")
  )
  data <- w$x$option$series[[1]]$markArea$data
  expect_equal(length(data), 2L)
  expect_equal(data[[1]][[1]]$xAxis, 1)
  expect_equal(data[[1]][[2]]$xAxis, 2) # not extended, next drawn run is C
  expect_equal(data[[2]][[1]]$xAxis, 5)
  expect_equal(data[[2]][[2]]$xAxis, 6)
})

test_that("draw_line blocks does not extend on category x-axis", {
  w <- draw_line(
    x = c("Mon", "Tue", "Wed", "Thu", "Fri"),
    y = c(1, 2, 3, 4, 5),
    blocks = c("A", "A", "A", "B", "B"),
    block_color = c(A = "red", B = "blue")
  )
  data <- w$x$option$series[[1]]$markArea$data
  expect_equal(length(data), 2L)
  expect_equal(data[[1]][[1]]$xAxis, "Mon")
  expect_equal(data[[1]][[2]]$xAxis, "Wed") # own last category, not Thu
  expect_equal(data[[2]][[1]]$xAxis, "Thu")
  expect_equal(data[[2]][[2]]$xAxis, "Fri")
})

test_that("draw_line blocks skips NA-level and transparent entries", {
  w <- draw_line(
    x = 1:6,
    y = c(1, 2, 3, 4, 5, 6),
    blocks = c("A", "A", NA, "B", "B", "C"),
    block_color = c(A = "red", B = "transparent", C = NA)
  )
  data <- w$x$option$series[[1]]$markArea$data
  # A run kept, NA-run skipped, B "transparent" skipped, C NA skipped.
  expect_equal(length(data), 1L)
  expect_equal(data[[1]][[1]]$xAxis, 1)
  expect_equal(data[[1]][[2]]$xAxis, 2)
  expect_equal(data[[1]][[1]]$itemStyle$color, "red")
})

test_that("draw_line blocks accepts positional block_color", {
  w <- draw_line(
    x = c("Mon", "Tue", "Wed", "Thu"),
    y = c(1, 2, 3, 4),
    blocks = c(1L, 1L, 2L, 2L),
    block_color = c("#111", "#222")
  )
  data <- w$x$option$series[[1]]$markArea$data
  expect_equal(length(data), 2L)
  expect_equal(data[[1]][[1]]$xAxis, "Mon")
  expect_equal(data[[1]][[2]]$xAxis, "Tue")
  expect_equal(data[[1]][[1]]$itemStyle$color, "#111")
  expect_equal(data[[2]][[1]]$xAxis, "Wed")
  expect_equal(data[[2]][[2]]$xAxis, "Thu")
  expect_equal(data[[2]][[1]]$itemStyle$color, "#222")
})

test_that("draw_line blocks errors on length / missing color", {
  expect_error(
    draw_line(x = 1:3, y = 1:3, blocks = c("A", "B"), block_color = "red")
  )
  expect_error(
    draw_line(x = 1:3, y = 1:3, blocks = c("A", "A", "B"))
  )
  expect_error(
    draw_line(
      x = 1:3,
      y = 1:3,
      blocks = c("A", "A", "B"),
      block_color = c(A = "red") # missing B
    )
  )
})

test_that("draw_line blocks attaches only to first series for multi-series y", {
  w <- draw_line(
    x = 1:4,
    y = list("S1" = c(1, 2, 3, 4), "S2" = c(4, 3, 2, 1)),
    blocks = c("A", "A", "B", "B"),
    block_color = c(A = "red", B = "blue")
  )
  expect_false(is.null(w$x$option$series[[1]]$markArea))
  expect_null(w$x$option$series[[2]]$markArea)
})

test_that("draw_line zoom = FALSE omits dataZoom", {
  w <- draw_line(x = c(1, 2, 3), y = c(1, 2, 3))
  expect_null(w$x$option$dataZoom)
})

test_that("draw_line zoom = TRUE emits slider + inside x-axis zooms", {
  w <- draw_line(x = 1:10, y = 1:10, zoom = TRUE)
  dz <- w$x$option$dataZoom
  expect_true(is.list(dz))
  expect_equal(length(dz), 2L)
  expect_equal(dz[[1]]$type, "slider")
  expect_equal(dz[[1]]$xAxisIndex, 0)
  expect_equal(dz[[1]]$start, 0)
  expect_equal(dz[[1]]$end, 100)
  expect_equal(dz[[2]]$type, "inside")
  expect_equal(dz[[2]]$xAxisIndex, 0)
  expect_equal(dz[[2]]$zoomOnMouseWheel, TRUE)
  expect_equal(dz[[2]]$moveOnMouseMove, TRUE)
})

test_that("draw_line accepts a single DataZoom for zoom", {
  w <- draw_line(
    x = 1:5,
    y = 1:5,
    zoom = DataZoom(type = "slider", x_axis_index = 0, start = 40, end = 60)
  )
  dz <- w$x$option$dataZoom
  expect_equal(length(dz), 1L)
  expect_equal(dz[[1]]$type, "slider")
  expect_equal(dz[[1]]$start, 40)
  expect_equal(dz[[1]]$end, 60)
})

test_that("draw_line rejects invalid zoom values", {
  expect_error(draw_line(x = 1:3, y = 1:3, zoom = "yes"))
  expect_error(draw_line(x = 1:3, y = 1:3, zoom = 1))
})

test_that("draw_line defaults xlim/ylim to exact data range (no padding)", {
  w <- draw_line(x = c(1, 2, 3, 4), y = c(10, 20, 15, 25))
  expect_equal(w$x$option$xAxis$min, 1)
  expect_equal(w$x$option$xAxis$max, 4)
  expect_equal(w$x$option$yAxis$min, 10)
  expect_equal(w$x$option$yAxis$max, 25)
})

test_that("draw_line honors explicit xlim and ylim", {
  w <- draw_line(
    x = c(1, 2, 3, 4),
    y = c(10, 20, 15, 25),
    xlim = c(0, 5),
    ylim = c(-5, 30)
  )
  expect_equal(w$x$option$xAxis$min, 0)
  expect_equal(w$x$option$xAxis$max, 5)
  expect_equal(w$x$option$yAxis$min, -5)
  expect_equal(w$x$option$yAxis$max, 30)
})

test_that("draw_line ylim spans all series in a list", {
  w <- draw_line(
    x = c("A", "B", "C"),
    y = list(S1 = c(1, 2, 3), S2 = c(10, 20, 30))
  )
  expect_equal(w$x$option$yAxis$min, 1)
  expect_equal(w$x$option$yAxis$max, 30)
})

test_that("draw_line omits x-axis min/max when x is non-numeric", {
  w <- draw_line(x = c("A", "B", "C"), y = c(1, 2, 3))
  expect_null(w$x$option$xAxis$min)
  expect_null(w$x$option$xAxis$max)
})

test_that("draw_line rejects xlim on non-numeric x", {
  expect_error(
    draw_line(x = c("A", "B"), y = c(1, 2), xlim = c(0, 3)),
    "xlim"
  )
})

test_that("draw_line rejects malformed xlim/ylim", {
  expect_error(draw_line(x = 1:3, y = 1:3, xlim = 1))
  expect_error(draw_line(x = 1:3, y = 1:3, ylim = c(1, NA)))
  expect_error(draw_line(x = 1:3, y = 1:3, ylim = "bad"))
})

test_that("draw_line suppresses corner split lines on value axes", {
  w <- draw_line(x = 1:4, y = c(10, 20, 15, 25))
  expect_equal(w$x$option$xAxis$splitLine$showMinLine, FALSE)
  expect_equal(w$x$option$xAxis$splitLine$showMaxLine, FALSE)
  expect_equal(w$x$option$yAxis$splitLine$showMinLine, FALSE)
  expect_equal(w$x$option$yAxis$splitLine$showMaxLine, FALSE)
})

test_that("draw_line omits split_line on category x-axis", {
  # Category axes have splitLine.show = false by default, so no override needed.
  w <- draw_line(x = c("A", "B", "C"), y = c(1, 2, 3))
  expect_null(w$x$option$xAxis$splitLine)
  expect_equal(w$x$option$yAxis$splitLine$showMinLine, FALSE)
})

test_that("draw_line suppresses min/max endpoint axis labels on value axes", {
  w <- draw_line(x = 1:4, y = c(10, 20, 15, 25))
  expect_equal(w$x$option$xAxis$axisLabel$showMinLabel, FALSE)
  expect_equal(w$x$option$xAxis$axisLabel$showMaxLabel, FALSE)
  expect_equal(w$x$option$yAxis$axisLabel$showMinLabel, FALSE)
  expect_equal(w$x$option$yAxis$axisLabel$showMaxLabel, FALSE)
})

test_that("draw_line omits axis_label on category x-axis (uses echarts default)", {
  w <- draw_line(x = c("A", "B", "C"), y = c(1, 2, 3))
  expect_null(w$x$option$xAxis$axisLabel)
  expect_equal(w$x$option$yAxis$axisLabel$showMinLabel, FALSE)
})

test_that("draw_line axisLine: on_zero always TRUE; show gated on 0 in orthogonal range", {
  # x: 1..4 (no 0), y: [-5, 5] (contains 0).
  #   xAxis: orthogonal = y -> 0 in range -> show = TRUE.
  #   yAxis: orthogonal = x -> 0 NOT in range -> show = FALSE.
  w1 <- draw_line(x = 1:4, y = c(-5, 2, 0, 5))
  expect_equal(w1$x$option$xAxis$axisLine$onZero, TRUE)
  expect_equal(w1$x$option$xAxis$axisLine$show, TRUE)
  expect_equal(w1$x$option$yAxis$axisLine$onZero, TRUE)
  expect_equal(w1$x$option$yAxis$axisLine$show, FALSE)

  # 0 outside both ranges -> show = FALSE on both.
  w2 <- draw_line(x = 10:13, y = c(100, 200, 300, 400))
  expect_equal(w2$x$option$xAxis$axisLine$show, FALSE)
  expect_equal(w2$x$option$yAxis$axisLine$show, FALSE)
  expect_equal(w2$x$option$xAxis$axisLine$onZero, TRUE)
  expect_equal(w2$x$option$yAxis$axisLine$onZero, TRUE)
})

test_that("draw_line omits axis_line on category x-axis", {
  w <- draw_line(x = c("A", "B", "C"), y = c(-1, 0, 1))
  # Category x-axis: no explicit axis_line override.
  expect_null(w$x$option$xAxis$axisLine)
  # yAxis.axis_line sentinel is x_lim, which is NULL for category x -> NULL.
  expect_null(w$x$option$yAxis$axisLine)
})

# -- draw_bar -------------------------------------------------------------------

test_that("draw_bar creates widget", {
  w <- draw_bar(
    x = c("Q1", "Q2", "Q3"),
    y = c(100, 200, 150)
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "bar")
})

test_that("draw_bar applies a single color to a single series", {
  w <- draw_bar(
    x = c("Q1", "Q2", "Q3"),
    y = c(100, 200, 150),
    color = "#ff0000"
  )
  expect_equal(w$x$option$series[[1]]$color, "#ff0000")
})

test_that("draw_bar recycles colors across individual bars", {
  w <- draw_bar(
    x = c("Q1", "Q2", "Q3"),
    y = c(100, 200, 150),
    color = c("#ff0000", "#00ff00")
  )
  expect_equal(w$x$option$series[[1]]$data[[1]]$itemStyle$color, "#ff0000")
  expect_equal(w$x$option$series[[1]]$data[[2]]$itemStyle$color, "#00ff00")
  expect_equal(w$x$option$series[[1]]$data[[3]]$itemStyle$color, "#ff0000")
})

test_that("draw_bar stacked", {
  w <- draw_bar(
    x = c("A", "B"),
    y = list("X" = c(10, 20), "Y" = c(5, 15)),
    stack = TRUE
  )
  expect_equal(w$x$option$series[[1]]$stack, "total")
  expect_equal(w$x$option$series[[2]]$stack, "total")
})

test_that("draw_bar recycles colors across multiple series", {
  w <- draw_bar(
    x = c("A", "B"),
    y = list("X" = c(10, 20), "Y" = c(5, 15), "Z" = c(7, 9)),
    color = c("#ff0000", "#00ff00")
  )
  expect_equal(w$x$option$series[[1]]$color, "#ff0000")
  expect_equal(w$x$option$series[[2]]$color, "#00ff00")
  expect_equal(w$x$option$series[[3]]$color, "#ff0000")
})

test_that("draw_bar horizontal", {
  w <- draw_bar(
    x = c("A", "B"),
    y = c(10, 20),
    horizontal = TRUE
  )
  expect_equal(w$x$option$xAxis$type, "value")
  expect_equal(w$x$option$yAxis$type, "category")
})

# -- draw_scatter ---------------------------------------------------------------

test_that("draw_scatter creates widget", {
  w <- draw_scatter(x = c(1, 2, 3), y = c(4, 5, 6))
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "scatter")
  expect_equal(w$x$option$xAxis$type, "value")
})

test_that("draw_scatter with groups", {
  w <- draw_scatter(
    x = c(1, 2, 3, 4),
    y = c(5, 6, 7, 8),
    group = c("A", "A", "B", "B")
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_scatter fit = 'glm' adds fit line and CI polygon", {
  set.seed(1)
  xv <- 1:20
  yv <- 2 * xv + rnorm(20)
  w <- draw_scatter(xv, yv, fit = "glm")
  # 1 scatter + 1 CI polygon + 1 fit line = 3
  expect_equal(length(w$x$option$series), 3L)
  types <- sapply(w$x$option$series, "[[", "type")
  expect_equal(types, c("scatter", "line", "line"))
  # Fit line: 200 points, no symbols, no name (ungrouped)
  fit_s <- w$x$option$series[[3]]
  expect_null(fit_s$name)
  expect_equal(length(fit_s$data), 200L)
  expect_equal(fit_s$showSymbol, FALSE)
  # CI polygon: upper L->R + lower R->L = 400 points, has area fill
  ci_s <- w$x$option$series[[2]]
  expect_equal(length(ci_s$data), 400L)
  expect_false(is.null(ci_s$areaStyle))
  expect_equal(ci_s$lineStyle$opacity, 0)
  expect_equal(ci_s$z, 0L)
})

test_that("draw_scatter fit = 'gam' works", {
  set.seed(1)
  xv <- seq(0, 4 * pi, length.out = 50)
  yv <- sin(xv) + rnorm(50, sd = 0.3)
  w <- draw_scatter(xv, yv, fit = "gam")
  # 1 scatter + 1 CI polygon + 1 fit line = 3
  expect_equal(length(w$x$option$series), 3L)
  expect_null(w$x$option$series[[3]]$name)
})

test_that("draw_scatter fit with se = FALSE omits CI band", {
  set.seed(1)
  xv <- 1:20
  yv <- xv + rnorm(20)
  w <- draw_scatter(xv, yv, fit = "glm", se = FALSE)
  # 1 scatter + 1 fit line = 2
  expect_equal(length(w$x$option$series), 2L)
  types <- sapply(w$x$option$series, "[[", "type")
  expect_equal(types, c("scatter", "line"))
})

test_that("draw_scatter fit with groups adds per-group fits", {
  set.seed(1)
  xv <- c(1:10, 1:10)
  yv <- c(1:10 + rnorm(10), 20:11 + rnorm(10))
  g <- rep(c("A", "B"), each = 10)
  w <- draw_scatter(xv, yv, group = g, fit = "glm")
  # 2 scatter + 2 * (CI polygon + fit line) = 6
  expect_equal(length(w$x$option$series), 6L)
  types <- sapply(w$x$option$series, "[[", "type")
  expect_equal(sum(types == "scatter"), 2L)
  expect_equal(sum(types == "line"), 4L)
})

test_that("draw_scatter fit respects n_fit", {
  set.seed(1)
  xv <- 1:20
  yv <- xv + rnorm(20)
  w <- draw_scatter(xv, yv, fit = "glm", n_fit = 50)
  fit_s <- w$x$option$series[[3]]
  expect_equal(length(fit_s$data), 50L)
})

test_that("draw_scatter defaults xlim/ylim to range + 4% padding", {
  w <- draw_scatter(x = c(0, 10), y = c(0, 100))
  # 4% of span (10) = 0.4; 4% of span (100) = 4
  expect_equal(w$x$option$xAxis$min, -0.4)
  expect_equal(w$x$option$xAxis$max, 10.4)
  expect_equal(w$x$option$yAxis$min, -4)
  expect_equal(w$x$option$yAxis$max, 104)
})

test_that("draw_scatter honors explicit xlim and ylim (no padding)", {
  w <- draw_scatter(
    x = c(0, 10),
    y = c(0, 100),
    xlim = c(0, 10),
    ylim = c(0, 100)
  )
  expect_equal(w$x$option$xAxis$min, 0)
  expect_equal(w$x$option$xAxis$max, 10)
  expect_equal(w$x$option$yAxis$min, 0)
  expect_equal(w$x$option$yAxis$max, 100)
})

test_that("draw_scatter rejects malformed xlim/ylim", {
  expect_error(draw_scatter(x = 1:3, y = 1:3, xlim = 1))
  expect_error(draw_scatter(x = 1:3, y = 1:3, ylim = c(NA, 1)))
  expect_error(draw_scatter(x = 1:3, y = 1:3, xlim = "bad"))
})

test_that("draw_scatter suppresses corner split lines on both axes", {
  w <- draw_scatter(x = 1:5, y = 1:5)
  expect_equal(w$x$option$xAxis$splitLine$showMinLine, FALSE)
  expect_equal(w$x$option$xAxis$splitLine$showMaxLine, FALSE)
  expect_equal(w$x$option$yAxis$splitLine$showMinLine, FALSE)
  expect_equal(w$x$option$yAxis$splitLine$showMaxLine, FALSE)
})

test_that("draw_scatter suppresses min/max endpoint axis labels on both axes", {
  w <- draw_scatter(x = 1:5, y = 1:5)
  expect_equal(w$x$option$xAxis$axisLabel$showMinLabel, FALSE)
  expect_equal(w$x$option$xAxis$axisLabel$showMaxLabel, FALSE)
  expect_equal(w$x$option$yAxis$axisLabel$showMinLabel, FALSE)
  expect_equal(w$x$option$yAxis$axisLabel$showMaxLabel, FALSE)
})

test_that("draw_scatter axisLine: on_zero always TRUE; show gated on 0 in orthogonal range", {
  # x: [-2, 2] (0 in padded range), y: [10, 20] (no 0).
  #   xAxis.show <- (0 in y) -> FALSE.
  #   yAxis.show <- (0 in x) -> TRUE.
  w1 <- draw_scatter(x = c(-2, -1, 0, 1, 2), y = c(10, 12, 15, 18, 20))
  expect_equal(w1$x$option$xAxis$axisLine$show, FALSE)
  expect_equal(w1$x$option$yAxis$axisLine$show, TRUE)
  expect_equal(w1$x$option$xAxis$axisLine$onZero, TRUE)
  expect_equal(w1$x$option$yAxis$axisLine$onZero, TRUE)

  # Neither axis contains 0 -> show = FALSE on both; onZero still TRUE.
  w2 <- draw_scatter(x = c(10, 11, 12), y = c(100, 200, 300))
  expect_equal(w2$x$option$xAxis$axisLine$show, FALSE)
  expect_equal(w2$x$option$yAxis$axisLine$show, FALSE)
  expect_equal(w2$x$option$xAxis$axisLine$onZero, TRUE)
  expect_equal(w2$x$option$yAxis$axisLine$onZero, TRUE)
})

# -- draw_pie -------------------------------------------------------------------

test_that("draw_pie creates widget", {
  w <- draw_pie(
    values = c(100, 200, 150),
    labels = c("A", "B", "C")
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "pie")
  expect_equal(length(w$x$option$series[[1]]$data), 3L)
})

test_that("draw_pie donut", {
  w <- draw_pie(
    values = c(100, 200),
    labels = c("A", "B"),
    radius = c("40%", "70%")
  )
  expect_equal(w$x$option$series[[1]]$radius, c("40%", "70%"))
})

# -- draw_boxplot ---------------------------------------------------------------

test_that("draw_boxplot computes stats and creates widget", {
  w <- draw_boxplot(
    data = list(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    ),
    labels = c("Group 1", "Group 2")
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "boxplot")
  expect_equal(w$x$option$xAxis$data, c("Group 1", "Group 2"))
  # Stats computed: each box is a 5-number summary
  expect_equal(length(w$x$option$series[[1]]$data[[1]]), 5L)
  expect_equal(length(w$x$option$series[[1]]$data[[2]]), 5L)
  # Default color: opaque border, semi-transparent fill (alpha 0.25)
  is <- w$x$option$series[[1]]$itemStyle
  expect_equal(is$borderColor, rtemis_colors[[1]])
  expect_match(is$color, "^rgba\\(")
})

test_that("draw_boxplot uses names from an ungrouped named list as labels", {
  w <- draw_boxplot(
    data = list(
      "Control" = c(1, 2, 3, 4, 5, 6, 7),
      "Treatment" = c(3, 4, 5, 6, 7, 8, 9)
    )
  )
  expect_equal(w$x$option$xAxis$data, c("Control", "Treatment"))
  expect_null(names(w$x$option$series[[1]]$data))
  expect_equal(length(w$x$option$series[[1]]$data[[1]]), 5L)
  expect_equal(length(w$x$option$series[[1]]$data[[2]]), 5L)
})

test_that("draw_boxplot respects explicit labels over named-list labels", {
  w <- draw_boxplot(
    data = list(
      "Control" = c(1, 2, 3, 4, 5, 6, 7),
      "Treatment" = c(3, 4, 5, 6, 7, 8, 9)
    ),
    labels = c("Group 1", "Group 2")
  )
  expect_equal(w$x$option$xAxis$data, c("Group 1", "Group 2"))
})

test_that("draw_boxplot fill_alpha controls fill opacity", {
  w <- draw_boxplot(
    data = list(c(1, 2, 3, 4, 5, 6, 7)),
    labels = c("G1"),
    color = "#ff0000",
    fill_alpha = 0.5
  )
  is <- w$x$option$series[[1]]$itemStyle
  expect_equal(is$borderColor, "#ff0000")
  expect_equal(is$color, "rgba(255, 0, 0, 0.5)")
})

test_that("draw_boxplot horizontal", {
  w <- draw_boxplot(
    data = list(c(1, 2, 3, 4, 5, 6, 7, 8)),
    labels = c("G1"),
    horizontal = TRUE
  )
  expect_equal(w$x$option$xAxis$type, "value")
  expect_equal(w$x$option$yAxis$type, "category")
  expect_null(w$x$option$series[[1]]$layout)
})

test_that("draw_boxplot handles NAs in ungrouped data", {
  expect_message(
    w <- draw_boxplot(
      data = list(c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10)),
      labels = c("G1"),
      verbosity = 1L
    ),
    "Removed 2 NA values"
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(length(w$x$option$series[[1]]$data[[1]]), 5L)
})

test_that("draw_boxplot with group computes stats in a single series", {
  set.seed(1)
  vals <- c(rnorm(50, 10, 2), rnorm(50, 15, 3))
  g <- rep(c("Control", "Treatment"), each = 50)
  w <- draw_boxplot(data = vals, group = g)
  expect_s3_class(w, "htmlwidget")
  # Single BoxplotSeries with one data item per group
  expect_equal(length(w$x$option$series), 1L)
  expect_equal(w$x$option$series[[1]]$type, "boxplot")
  # Category axis uses group labels
  expect_equal(w$x$option$xAxis$data, c("Control", "Treatment"))
  expect_null(w$x$option$legend)
  expect_equal(
    vapply(w$x$option$series[[1]]$data, `[[`, character(1), "name"),
    c("Control", "Treatment")
  )
  expect_equal(
    vapply(
      w$x$option$series[[1]]$data,
      function(item) length(item$value),
      integer(1)
    ),
    c(5L, 5L)
  )
  expect_equal(
    vapply(
      w$x$option$series[[1]]$data,
      function(item) item$itemStyle$borderColor,
      character(1)
    ),
    rtemis_colors[1:2]
  )
})

test_that("draw_boxplot with group drops missing group values", {
  vals <- c(10, 11, 12, 20, 21, 22, 30)
  g <- c("Control", "Control", NA, "Treatment", "Treatment", NA, "Control")

  w <- draw_boxplot(data = vals, group = g)

  expect_equal(w$x$option$xAxis$data, c("Control", "Treatment"))
  expect_equal(length(w$x$option$series), 1L)
  expect_equal(length(w$x$option$series[[1]]$data), 2L)
  expect_equal(
    vapply(w$x$option$series[[1]]$data, `[[`, character(1), "name"),
    c("Control", "Treatment")
  )
  expect_false(any(is.na(vapply(
    w$x$option$series[[1]]$data,
    `[[`,
    character(1),
    "name"
  ))))
})

test_that("draw_boxplot with grouped variables drops missing group values", {
  data <- list(
    "Bill Length" = c(40, 42, 41, 38, 39, 37),
    "Bill Depth" = c(18, 19, 18.5, 15, 14.5, 15.5)
  )
  g <- c("male", "male", NA, "female", "female", NA)

  w <- draw_boxplot(data = data, group = g)

  expect_equal(length(w$x$option$series), 2L)
  expect_equal(
    vapply(w$x$option$series, `[[`, character(1), "name"),
    c("male", "female")
  )
  expect_false(any(is.na(vapply(
    w$x$option$series,
    `[[`,
    character(1),
    "name"
  ))))
  expect_equal(w$x$option$xAxis$data, c("Bill Length", "Bill Depth"))
  expect_equal(length(w$x$option$series[[1]]$data), 2L)
  expect_equal(length(w$x$option$series[[2]]$data), 2L)
})

test_that("draw_boxplot with group horizontal", {
  set.seed(1)
  vals <- c(rnorm(30, 5), rnorm(30, 8))
  g <- rep(c("A", "B"), each = 30)
  w <- draw_boxplot(data = vals, group = g, horizontal = TRUE)
  expect_equal(w$x$option$xAxis$type, "value")
  expect_equal(w$x$option$yAxis$type, "category")
  expect_equal(w$x$option$yAxis$data, c("A", "B"))
  expect_null(w$x$option$series[[1]]$layout)
})

# -- draw_density --------------------------------------------------------------

test_that("draw_density creates widget", {
  set.seed(1)
  w <- draw_density(rnorm(100))
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "line")
  expect_equal(w$x$option$xAxis$type, "value")
  # 512 density points by default, each an [x, y] pair
  expect_equal(length(w$x$option$series[[1]]$data), 512L)
  expect_equal(length(w$x$option$series[[1]]$data[[1]]), 2L)
  # No symbols on density curves
  expect_equal(w$x$option$series[[1]]$showSymbol, FALSE)
  # Area fill enabled
  expect_false(is.null(w$x$option$series[[1]]$areaStyle))
})

test_that("draw_density with groups", {
  set.seed(1)
  w <- draw_density(
    x = c(rnorm(50, 0), rnorm(50, 3)),
    group = rep(c("A", "B"), each = 50)
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "A")
  expect_equal(w$x$option$series[[2]]$name, "B")
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_density with a named list creates one series per variable", {
  set.seed(1)
  w <- draw_density(
    list(
      "Flipper Length" = rnorm(50, 200, 10),
      "Bill Length" = rnorm(50, 45, 4)
    )
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "Flipper Length")
  expect_equal(w$x$option$series[[2]]$name, "Bill Length")
  expect_equal(length(w$x$option$series[[1]]$data), 512L)
  expect_equal(length(w$x$option$series[[2]]$data), 512L)
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_density with a list removes NAs per variable", {
  expect_message(
    w <- draw_density(
      list(
        "Flipper Length" = c(200, 202, NA, 205, 207),
        "Bill Length" = c(40, 41, 42, NA, 44)
      ),
      verbosity = 1L
    ),
    "Removed 1 NA value from Flipper Length"
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "Flipper Length")
  expect_equal(w$x$option$series[[2]]$name, "Bill Length")
})

test_that("draw_density with grouped list creates variable-group series", {
  w <- draw_density(
    list(
      "Flipper Length" = c(200, 202, 205, 207, 210, 212),
      "Bill Length" = c(40, 41, 42, 44, 45, 46)
    ),
    group = rep(c("A", "B"), each = 3)
  )

  expect_equal(length(w$x$option$series), 4L)
  expect_equal(
    vapply(w$x$option$series, `[[`, character(1), "name"),
    c(
      "Flipper Length - A",
      "Flipper Length - B",
      "Bill Length - A",
      "Bill Length - B"
    )
  )
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_density with grouped list drops missing group values", {
  w <- draw_density(
    list(
      "Flipper Length" = c(200, 202, 205, 207, 210, 212),
      "Bill Length" = c(40, 41, 42, 44, 45, 46)
    ),
    group = c("A", "A", NA, "B", "B", NA)
  )

  expect_equal(length(w$x$option$series), 4L)
  expect_equal(
    vapply(w$x$option$series, `[[`, character(1), "name"),
    c(
      "Flipper Length - A",
      "Flipper Length - B",
      "Bill Length - A",
      "Bill Length - B"
    )
  )
})

test_that("draw_density with grouped list removes NAs per variable and keeps groups aligned", {
  expect_message(
    w <- draw_density(
      list(
        "Flipper Length" = c(200, 202, NA, 207, 210, 212),
        "Bill Length" = c(40, 41, 42, 44, NA, 46)
      ),
      group = rep(c("A", "B"), each = 3),
      verbosity = 1L
    ),
    "Removed 1 NA value from Flipper Length"
  )

  expect_equal(length(w$x$option$series), 4L)
  expect_equal(w$x$option$series[[1]]$name, "Flipper Length - A")
  expect_equal(w$x$option$series[[4]]$name, "Bill Length - B")
})

test_that("draw_density respects n parameter", {
  set.seed(1)
  w <- draw_density(rnorm(100), n = 256)
  expect_equal(length(w$x$option$series[[1]]$data), 256L)
})

test_that("draw_density removes NAs and messages", {
  set.seed(1)
  xv <- c(rnorm(50), NA, NA)
  expect_message(
    w <- draw_density(xv, verbosity = 1L),
    "Removed 2 NA values"
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(length(w$x$option$series[[1]]$data), 512L)
})

test_that("draw_density removes NAs silently with verbosity = 0", {
  set.seed(1)
  xv <- c(rnorm(50), NA)
  expect_no_message(
    w <- draw_density(xv, verbosity = 0L)
  )
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_density with group removes NAs and keeps group aligned", {
  set.seed(1)
  xv <- c(rnorm(20), NA, rnorm(20), NA, NA)
  g <- c(rep("A", 21), rep("B", 22))
  expect_message(
    w <- draw_density(xv, group = g, verbosity = 1L),
    "Removed 3 NA values"
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "A")
  expect_equal(w$x$option$series[[2]]$name, "B")
})

# -- draw_histogram ------------------------------------------------------------

test_that("draw_histogram creates widget", {
  set.seed(1)
  w <- draw_histogram(rnorm(200))
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "bar")
  expect_equal(w$x$option$xAxis$type, "category")
  # No gap between bars for histogram look
  expect_equal(w$x$option$series[[1]]$barCategoryGap, "0%")
})

test_that("draw_histogram with groups", {
  set.seed(1)
  w <- draw_histogram(
    x = c(rnorm(100, 0), rnorm(100, 3)),
    group = rep(c("A", "B"), each = 100)
  )
  expect_equal(length(w$x$option$series), 2L)
  expect_equal(w$x$option$series[[1]]$name, "A")
  expect_equal(w$x$option$series[[2]]$name, "B")
  expect_false(is.null(w$x$option$legend))
})

test_that("draw_histogram respects breaks parameter", {
  set.seed(1)
  w <- draw_histogram(rnorm(200), breaks = 20)
  # More breaks = more bins = more category labels
  expect_true(length(w$x$option$xAxis$data) >= 15L)
})
