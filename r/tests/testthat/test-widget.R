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
  th <- dark_theme()
  w <- draw(opt, theme = th)
  expect_false(is.null(w$x$theme))
  expect_equal(w$x$theme$backgroundColor, "#100C2A")
})

test_that("draw() passes renderer option", {
  opt <- EChartsOption(series = LineSeries(data = c(1, 2)))
  w <- draw(opt, renderer = "svg")
  expect_equal(w$x$renderer, "svg")
})

# -- draw_line ------------------------------------------------------------------

test_that("draw_line creates widget from vectors", {
  w <- draw_line(
    x = c("Mon", "Tue", "Wed"),
    y = c(10, 20, 15)
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$xAxis$type, "category")
  expect_equal(w$x$option$series[[1]]$type, "line")
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

# -- draw_bar -------------------------------------------------------------------

test_that("draw_bar creates widget", {
  w <- draw_bar(
    x = c("Q1", "Q2", "Q3"),
    y = c(100, 200, 150)
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "bar")
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

test_that("draw_boxplot creates widget", {
  w <- draw_boxplot(
    data = list(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6)),
    labels = c("Group 1", "Group 2")
  )
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$option$series[[1]]$type, "boxplot")
  expect_equal(w$x$option$xAxis$data, c("Group 1", "Group 2"))
})

test_that("draw_boxplot horizontal", {
  w <- draw_boxplot(
    data = list(c(1, 2, 3, 4, 5)),
    labels = c("G1"),
    horizontal = TRUE
  )
  expect_equal(w$x$option$xAxis$type, "value")
  expect_equal(w$x$option$yAxis$type, "category")
  expect_equal(w$x$option$series[[1]]$layout, "horizontal")
})
