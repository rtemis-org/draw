test_that("draw_line line_style: NULL produces no lineStyle in series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = NULL)
  opt <- w$x$option
  ls <- opt$series[[1]]$lineStyle
  expect_null(ls)
})

test_that("draw_line line_style: single value applied to single series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = "dashed")
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "dashed")
})

test_that("draw_line line_style: per-series values applied correctly", {
  y <- list(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
  w <- draw_line(1:3, y, line_style = c("solid", "dashed", "dotted"))
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
  expect_equal(opt$series[[3]]$lineStyle$type, "dotted")
})

test_that("draw_line line_style: recycled when fewer values than series", {
  y <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  w <- draw_line(1:3, y, line_style = c("solid", "dashed"))
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
  expect_equal(opt$series[[3]]$lineStyle$type, "solid")
  expect_equal(opt$series[[4]]$lineStyle$type, "dashed")
})

test_that("draw_line line_style: invalid value errors", {
  expect_error(
    draw_line(1:3, 1:3, line_style = "bold"),
    "line_style"
  )
})

test_that("draw_line line_style: all valid values accepted", {
  for (ls in c("solid", "dashed", "dotted")) {
    expect_no_error(draw_line(1:3, 1:3, line_style = ls))
  }
})

# %% group ----

test_that("draw_line group: one series per level, named by level, with legend", {
  x <- c(1, 2, 3, 1, 2, 3)
  y <- c(1, 2, 3, 4, 5, 6)
  g <- c("A", "A", "A", "B", "B", "B")
  w <- draw_line(x, y, group = g)
  opt <- w$x$option
  expect_length(opt$series, 2L)
  expect_equal(opt$series[[1]]$name, "A")
  expect_equal(opt$series[[2]]$name, "B")
  expect_equal(opt$series[[1]]$data, list(c(1, 1), c(2, 2), c(3, 3)))
  expect_equal(opt$series[[2]]$data, list(c(1, 4), c(2, 5), c(3, 6)))
  expect_false(is.null(opt$legend))
})

test_that("draw_line group: factor levels keep their order and NA is dropped", {
  x <- c(1, 2, 1, 2, 1)
  y <- c(1, 2, 3, 4, 5)
  g <- factor(c("b", "b", "a", "a", NA), levels = c("b", "a"))
  opt <- draw_line(x, y, group = g)$x$option
  expect_equal(vapply(opt$series, function(s) s$name, ""), c("b", "a"))
  expect_equal(opt$series[[2]]$data, list(c(1, 3), c(2, 4)))
})

test_that("draw_line group: category x pairs each point with its axis index", {
  x <- c("Q1", "Q2", "Q1", "Q2")
  y <- c(1, 2, 3, 4)
  g <- c("A", "A", "B", "B")
  opt <- draw_line(x, y, group = g)$x$option
  expect_equal(opt$xAxis$type, "category")
  expect_equal(opt$xAxis$data, c("Q1", "Q2"))
  expect_equal(opt$series[[2]]$data, list(c(0, 3), c(1, 4)))
})

test_that("draw_line group: a factor x is matched by index, not by its label", {
  x <- factor(c("b", "a", "b", "a"), levels = c("b", "a"))
  y <- c(1, 2, 3, 4)
  g <- c("A", "A", "B", "B")
  opt <- draw_line(x, y, group = g)$x$option
  expect_equal(opt$xAxis$type, "category")
  expect_equal(opt$series[[1]]$data, list(c(0, 1), c(1, 2)))
  expect_equal(opt$series[[2]]$data, list(c(0, 3), c(1, 4)))
})

test_that("draw_line: ungrouped category axis data is unchanged", {
  opt <- draw_line(c("a", "b", "c"), c(1, 2, 3))$x$option
  expect_equal(opt$xAxis$data, c("a", "b", "c"))
  expect_equal(opt$series[[1]]$data, c(1, 2, 3))
})

test_that("draw_line group: line_style recycles across groups", {
  x <- c(1, 2, 1, 2)
  y <- 1:4
  g <- c("A", "A", "B", "B")
  opt <- draw_line(x, y, group = g, line_style = c("solid", "dashed"))$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
})

test_that("draw_line group: rejects a list y, a length mismatch, and blocks", {
  expect_error(
    draw_line(1:2, list(a = 1:2, b = 3:4), group = c("A", "B")),
    "not both"
  )
  expect_error(
    draw_line(1:4, 1:4, group = c("A", "B")),
    "same length as `y`"
  )
  expect_error(
    draw_line(
      c(1, 2, 1, 2),
      1:4,
      group = c("A", "A", "B", "B"),
      blocks = c(1, 1, 2, 2),
      block_color = c("red", "blue")
    ),
    "blocks"
  )
})

# %% time axis ----

test_that("draw_line: a Date x gets a time axis in epoch milliseconds, in UTC", {
  d <- as.Date("2025-09-07") + c(0, 7, 14)
  opt <- draw_line(d, c(1, 2, 3))$x$option
  expect_equal(opt$xAxis$type, "time")
  expect_null(opt$xAxis$data)
  expect_true(opt$useUTC)
  ms <- as.numeric(d) * 86400000
  expect_equal(opt$series[[1]]$data, list(c(ms[1], 1), c(ms[2], 2), c(ms[3], 3)))
  expect_equal(c(opt$xAxis$min, opt$xAxis$max), calc_limits(ms))
})

test_that("draw_line: a POSIXct x keeps its wall-clock reading in any timezone", {
  # 10:00 in Los Angeles is sent as the instant whose UTC reading is 10:00,
  # so the chart labels it 10:00 wherever it is viewed.
  x <- as.POSIXct("2024-01-01 10:00:00", tz = "America/Los_Angeles")
  opt <- draw_line(x + c(0, 3600), c(1, 2))$x$option
  expect_equal(opt$xAxis$type, "time")
  ten_utc <- as.numeric(as.POSIXct("2024-01-01 10:00:00", tz = "UTC")) * 1000
  expect_equal(opt$series[[1]]$data[[1]][1], ten_utc)
  expect_equal(opt$series[[1]]$data[[2]][1], ten_utc + 3600000)
})

test_that("draw_line: grouped POSIXct x pairs each point with its time", {
  x <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * c(0:1, 0:1)
  g <- c("A", "A", "B", "B")
  opt <- draw_line(x, c(1, 2, 3, 4), group = g)$x$option
  ms <- as.numeric(x) * 1000
  expect_equal(opt$xAxis$type, "time")
  expect_equal(opt$series[[1]]$data, list(c(ms[1], 1), c(ms[2], 2)))
  expect_equal(opt$series[[2]]$data, list(c(ms[3], 3), c(ms[4], 4)))
})

test_that("draw_line: xlim on a time axis accepts dates or milliseconds", {
  d <- as.Date("2025-09-07") + 0:2
  lim <- as.Date(c("2025-09-01", "2025-09-30"))
  opt <- draw_line(d, 1:3, xlim = lim)$x$option
  expect_equal(c(opt$xAxis$min, opt$xAxis$max), as.numeric(lim) * 86400000)
  opt2 <- draw_line(d, 1:3, xlim = as.numeric(lim) * 86400000)$x$option
  expect_equal(opt2$xAxis$min, opt$xAxis$min)
})

test_that("draw_line: blocks on a time axis are placed in milliseconds", {
  d <- as.Date("2025-09-07") + 0:3
  opt <- draw_line(
    d,
    1:4,
    blocks = c("a", "a", "b", "b"),
    block_color = c(a = "red", b = "blue")
  )$x$option
  bands <- opt$series[[1]]$markArea$data
  expect_equal(bands[[1]][[1]]$xAxis, as.numeric(d[1]) * 86400000)
})

test_that("draw_line: equal_axes rejects a time x", {
  d <- as.Date("2025-09-07") + 0:2
  expect_error(draw_line(d, 1:3, equal_axes = TRUE), "date or time")
})

test_that("draw_line: numeric and character x are unchanged by the time axis", {
  expect_equal(draw_line(1:3, 1:3)$x$option$xAxis$type, "value")
  expect_null(draw_line(1:3, 1:3)$x$option$useUTC)
  expect_equal(draw_line(c("a", "b"), 1:2)$x$option$xAxis$type, "category")
})

test_that("time_axis_ms: Date, POSIXct, numeric, and NA", {
  expect_equal(time_axis_ms(as.Date("1970-01-02")), 86400000)
  expect_equal(
    time_axis_ms(as.POSIXct("1970-01-01 00:00:01", tz = "UTC")),
    1000
  )
  expect_equal(time_axis_ms(c(1, 2)), c(1, 2))
  expect_true(is.na(time_axis_ms(as.Date(NA))))
})

# %% zoom slider placement ----

test_that("draw_line zoom = TRUE gives the slider its own band under the plot", {
  opt <- draw_line(1:5, 1:5, zoom = TRUE)$x$option
  slider <- Filter(function(z) identical(z$type, "slider"), opt$dataZoom)[[1]]
  # Default margins: the slider sits above the 36 px margin plus its 7 px
  # brush handle, and the grid clears slider, handle, and an 8 px gap.
  expect_equal(slider$bottom, 36 + 7)
  expect_equal(slider$height, 30)
  expect_equal(opt$grid$bottom, 36 + 7 + 30 + 8)
})

test_that("draw_line zoom = TRUE builds on an explicit or absent bottom margin", {
  opt <- draw_line(1:5, 1:5, zoom = TRUE, margins = c(bottom = 50))$x$option
  expect_equal(opt$grid$bottom, 50 + 45)
  # No margins at all: ECharts' own default grid bottom of 80 is the base.
  opt <- draw_line(1:5, 1:5, zoom = TRUE, margins = NULL)$x$option
  expect_equal(opt$grid$bottom, 80 + 45)
})

test_that("draw_line zoom leaves a percentage margin and explicit DataZoom alone", {
  opt <- draw_line(1:5, 1:5, zoom = TRUE, margins = list(bottom = "20%"))$x$option
  expect_equal(opt$grid$bottom, "20%")
  slider <- Filter(function(z) identical(z$type, "slider"), opt$dataZoom)[[1]]
  expect_null(slider$bottom)
  opt <- draw_line(1:5, 1:5, zoom = DataZoom(type = "slider"))$x$option
  expect_null(opt$dataZoom[[1]]$bottom)
  expect_equal(opt$grid$bottom, 36)
})

# %% tooltip ----

test_that("draw_line tooltip formats values but keeps the axis trigger", {
  opt <- draw_line(1:3, c(1.23456, 2, 3))$x$option
  expect_equal(opt$tooltip$trigger, "axis")
  expect_s3_class(opt$tooltip$valueFormatter, "JS_EVAL")
  expect_match(opt$tooltip$valueFormatter, "toFixed\\(2\\)")
})
