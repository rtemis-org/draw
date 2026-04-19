# test-option.R
# Tests for EChartsOption S7 class

test_that("EChartsOption creates with defaults", {
  opt <- EChartsOption()
  expect_true(S7::S7_inherits(opt, EChartsOption))
  expect_equal(to_list(opt), list())
})

test_that("EChartsOption simple line chart", {
  opt <- EChartsOption(
    x_axis = Axis(
      type = "category",
      data = c("Mon", "Tue", "Wed", "Thu", "Fri")
    ),
    y_axis = Axis(type = "value"),
    series = LineSeries(
      data = c(150, 230, 224, 218, 135),
      smooth = TRUE
    )
  )
  out <- to_list(opt)
  expect_equal(out$xAxis$type, "category")
  expect_equal(out$xAxis$data, c("Mon", "Tue", "Wed", "Thu", "Fri"))
  expect_equal(out$yAxis$type, "value")
  # Series should be a list of one series
  expect_true(is.list(out$series))
  expect_equal(length(out$series), 1L)
  expect_equal(out$series[[1]]$type, "line")
  expect_equal(out$series[[1]]$smooth, TRUE)
})

test_that("EChartsOption multiple series", {
  opt <- EChartsOption(
    x_axis = Axis(type = "category", data = c("A", "B", "C")),
    y_axis = Axis(type = "value"),
    series = list(
      LineSeries(name = "Line", data = c(1, 2, 3)),
      BarSeries(name = "Bar", data = c(3, 2, 1))
    )
  )
  out <- to_list(opt)
  expect_equal(length(out$series), 2L)
  expect_equal(out$series[[1]]$type, "line")
  expect_equal(out$series[[1]]$name, "Line")
  expect_equal(out$series[[2]]$type, "bar")
  expect_equal(out$series[[2]]$name, "Bar")
})

test_that("EChartsOption with title and legend", {
  opt <- EChartsOption(
    title = Title(text = "My Chart", left = "center"),
    legend = Legend(show = TRUE, top = "bottom"),
    series = PieSeries(
      name = "Browser",
      data = list(
        list(value = 1048, name = "Chrome"),
        list(value = 735, name = "Firefox")
      ),
      radius = "50%"
    )
  )
  out <- to_list(opt)
  expect_equal(out$title$text, "My Chart")
  expect_equal(out$title$left, "center")
  expect_equal(out$legend$show, TRUE)
  expect_equal(out$legend$top, "bottom")
  expect_equal(out$series[[1]]$type, "pie")
  expect_equal(out$series[[1]]$radius, "50%")
})

test_that("EChartsOption with tooltip", {
  opt <- EChartsOption(
    tooltip = Tooltip(trigger = "axis"),
    x_axis = Axis(type = "category"),
    y_axis = Axis(type = "value"),
    series = BarSeries(data = c(10, 20, 30))
  )
  out <- to_list(opt)
  expect_equal(out$tooltip$trigger, "axis")
})

test_that("EChartsOption multiple grids and axes", {
  opt <- EChartsOption(
    grid = list(
      Grid(left = "10%", right = "10%", top = 60, bottom = "55%"),
      Grid(left = "10%", right = "10%", top = "55%", bottom = 60)
    ),
    x_axis = list(
      Axis(type = "category", data = c("A", "B")),
      Axis(type = "category", data = c("X", "Y"))
    ),
    y_axis = list(
      Axis(type = "value"),
      Axis(type = "value")
    )
  )
  out <- to_list(opt)
  expect_equal(length(out$grid), 2L)
  expect_equal(out$grid[[1]]$left, "10%")
  expect_equal(out$grid[[1]]$top, 60)
  expect_equal(length(out$xAxis), 2L)
  expect_equal(out$xAxis[[1]]$data, c("A", "B"))
  expect_equal(out$xAxis[[2]]$data, c("X", "Y"))
})

test_that("EChartsOption global settings", {
  opt <- EChartsOption(
    color = c("#5470c6", "#91cc75", "#fac858"),
    background_color = "#fff",
    animation = TRUE,
    animation_duration = 1000,
    animation_easing = "cubicOut",
    dark_mode = FALSE,
    use_utc = TRUE
  )
  out <- to_list(opt)
  expect_equal(out$color, c("#5470c6", "#91cc75", "#fac858"))
  expect_equal(out$backgroundColor, "#fff")
  expect_equal(out$animation, TRUE)
  expect_equal(out$animationDuration, 1000)
  expect_equal(out$animationEasing, "cubicOut")
  expect_equal(out$darkMode, FALSE)
  expect_equal(out$useUtc, TRUE)
})

test_that("EChartsOption global text_style", {
  opt <- EChartsOption(
    text_style = TextStyle(font_family = "Arial", font_size = 14)
  )
  out <- to_list(opt)
  expect_equal(out$textStyle$fontFamily, "Arial")
  expect_equal(out$textStyle$fontSize, 14)
})

test_that("EChartsOption color validates", {
  expect_error(EChartsOption(color = 123))
})

test_that("EChartsOption dark_mode validates", {
  expect_equal(EChartsOption(dark_mode = TRUE)@dark_mode, TRUE)
  expect_equal(EChartsOption(dark_mode = "auto")@dark_mode, "auto")
  expect_error(EChartsOption(dark_mode = "bad"))
})

test_that("to_json produces valid JSON", {
  opt <- EChartsOption(
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = ScatterSeries(
      data = list(c(1, 2), c(3, 4))
    )
  )
  json <- to_json(opt)
  expect_true(is.character(json))
  # Should parse back
  parsed <- jsonlite::fromJSON(as.character(json), simplifyVector = FALSE)
  expect_equal(parsed$xAxis$type, "value")
  expect_equal(parsed$series[[1]]$type, "scatter")
})

test_that("to_json pretty works", {
  opt <- EChartsOption(
    title = Title(text = "Test")
  )
  json_compact <- to_json(opt, pretty = FALSE)
  json_pretty <- to_json(opt, pretty = TRUE)
  expect_true(nchar(json_pretty) > nchar(json_compact))
})

test_that("EChartsOption data_zoom wraps a single DataZoom into an array", {
  opt <- EChartsOption(
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = LineSeries(data = list(c(1, 1), c(2, 2))),
    data_zoom = DataZoom(type = "slider", x_axis_index = 0, start = 0, end = 50)
  )
  out <- to_list(opt)
  expect_true(is.list(out$dataZoom))
  expect_equal(length(out$dataZoom), 1L)
  expect_equal(out$dataZoom[[1]]$type, "slider")
  expect_equal(out$dataZoom[[1]]$xAxisIndex, 0)
  expect_equal(out$dataZoom[[1]]$start, 0)
  expect_equal(out$dataZoom[[1]]$end, 50)
})

test_that("EChartsOption data_zoom passes a list through as array", {
  opt <- EChartsOption(
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = LineSeries(data = list(c(1, 1), c(2, 2))),
    data_zoom = list(
      DataZoom(type = "slider", x_axis_index = 0),
      DataZoom(
        type = "inside",
        x_axis_index = 0,
        zoom_on_mouse_wheel = TRUE
      )
    )
  )
  out <- to_list(opt)
  expect_equal(length(out$dataZoom), 2L)
  expect_equal(out$dataZoom[[1]]$type, "slider")
  expect_equal(out$dataZoom[[2]]$type, "inside")
  expect_equal(out$dataZoom[[2]]$zoomOnMouseWheel, TRUE)
})

test_that("EChartsOption omits dataZoom when data_zoom is NULL", {
  opt <- EChartsOption(
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = LineSeries(data = list(c(1, 1)))
  )
  out <- to_list(opt)
  expect_null(out$dataZoom)
})

test_that("EChartsOption full stacked bar chart", {
  opt <- EChartsOption(
    title = Title(text = "Stacked Bar"),
    tooltip = Tooltip(trigger = "axis"),
    legend = Legend(data = c("A", "B")),
    x_axis = Axis(type = "category", data = c("Q1", "Q2", "Q3", "Q4")),
    y_axis = Axis(type = "value"),
    series = list(
      BarSeries(name = "A", data = c(10, 20, 30, 40), stack = "total"),
      BarSeries(name = "B", data = c(15, 25, 35, 45), stack = "total")
    )
  )
  out <- to_list(opt)
  expect_equal(out$title$text, "Stacked Bar")
  expect_equal(out$tooltip$trigger, "axis")
  expect_equal(out$legend$data, c("A", "B"))
  expect_equal(length(out$series), 2L)
  expect_equal(out$series[[1]]$stack, "total")
  expect_equal(out$series[[2]]$stack, "total")
})
