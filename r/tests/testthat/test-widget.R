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
    data = list(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)),
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
    vapply(w$x$option$series[[1]]$data, function(item) length(item$value), integer(1)),
    c(5L, 5L)
  )
  expect_equal(
    vapply(w$x$option$series[[1]]$data, function(item) item$itemStyle$borderColor, character(1)),
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
  expect_false(any(is.na(vapply(w$x$option$series[[1]]$data, `[[`, character(1), "name"))))
})

test_that("draw_boxplot with grouped variables drops missing group values", {
  data <- list(
    "Bill Length" = c(40, 42, 41, 38, 39, 37),
    "Bill Depth" = c(18, 19, 18.5, 15, 14.5, 15.5)
  )
  g <- c("male", "male", NA, "female", "female", NA)

  w <- draw_boxplot(data = data, group = g)

  expect_equal(length(w$x$option$series), 2L)
  expect_equal(vapply(w$x$option$series, `[[`, character(1), "name"), c("male", "female"))
  expect_false(any(is.na(vapply(w$x$option$series, `[[`, character(1), "name"))))
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
