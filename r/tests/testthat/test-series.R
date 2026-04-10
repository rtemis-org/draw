# test-series.R
# Tests for series S7 classes

# -- LineSeries -----------------------------------------------------------------

test_that("LineSeries creates with defaults", {
  ls <- LineSeries()
  expect_true(S7::S7_inherits(ls, LineSeries))
  out <- to_list(ls)
  expect_equal(out$type, "line")
  expect_equal(length(out), 1L)  # only type
})

test_that("LineSeries to_list() converts names", {
  ls <- LineSeries(
    name = "Series A",
    data = c(1, 2, 3, 4, 5),
    x_axis_index = 0,
    y_axis_index = 0,
    stack = "total",
    smooth = TRUE,
    connect_nulls = TRUE,
    show_symbol = FALSE
  )
  out <- to_list(ls)
  expect_equal(out$type, "line")
  expect_equal(out$name, "Series A")
  expect_equal(out$data, c(1, 2, 3, 4, 5))
  expect_equal(out$xAxisIndex, 0)
  expect_equal(out$yAxisIndex, 0)
  expect_equal(out$stack, "total")
  expect_equal(out$smooth, TRUE)
  expect_equal(out$connectNulls, TRUE)
  expect_equal(out$showSymbol, FALSE)
})

test_that("LineSeries smooth accepts number", {
  ls <- LineSeries(smooth = 0.6)
  expect_equal(ls@smooth, 0.6)
})

test_that("LineSeries smooth rejects invalid", {
  expect_error(LineSeries(smooth = "bad"))
})

test_that("LineSeries step validation", {
  expect_equal(LineSeries(step = FALSE)@step, FALSE)
  expect_equal(LineSeries(step = "start")@step, "start")
  expect_equal(LineSeries(step = "end")@step, "end")
  expect_equal(LineSeries(step = "middle")@step, "middle")
  expect_error(LineSeries(step = TRUE))
  expect_error(LineSeries(step = "invalid"))
})

test_that("LineSeries symbol_size accepts number and vector", {
  ls1 <- LineSeries(symbol_size = 10)
  expect_equal(ls1@symbol_size, 10)
  ls2 <- LineSeries(symbol_size = c(10, 20))
  expect_equal(ls2@symbol_size, c(10, 20))
})

test_that("LineSeries line_style nests correctly", {
  ls <- LineSeries(
    name = "A",
    data = c(1, 2, 3),
    line_style = LineStyle(color = "#f00", width = 3, type = "dashed")
  )
  out <- to_list(ls)
  expect_equal(out$lineStyle$color, "#f00")
  expect_equal(out$lineStyle$width, 3)
  expect_equal(out$lineStyle$type, "dashed")
})

test_that("LineSeries area_style enables area chart", {
  ls <- LineSeries(
    data = c(1, 2, 3),
    area_style = AreaStyle(opacity = 0.5)
  )
  out <- to_list(ls)
  expect_equal(out$areaStyle$opacity, 0.5)
})

test_that("LineSeries item_style and label nest", {
  ls <- LineSeries(
    item_style = ItemStyle(color = "#333"),
    label = LabelOption(show = TRUE, position = "top")
  )
  out <- to_list(ls)
  expect_equal(out$itemStyle$color, "#333")
  expect_equal(out$label$show, TRUE)
  expect_equal(out$label$position, "top")
})

# -- BarSeries ------------------------------------------------------------------

test_that("BarSeries creates with defaults", {
  bs <- BarSeries()
  out <- to_list(bs)
  expect_equal(out$type, "bar")
  expect_equal(length(out), 1L)
})

test_that("BarSeries to_list() converts names", {
  bs <- BarSeries(
    name = "Revenue",
    data = c(10, 20, 30),
    stack = "total",
    bar_width = "60%",
    bar_gap = "30%",
    bar_category_gap = "20%",
    bar_min_height = 5,
    round_cap = TRUE,
    show_background = TRUE
  )
  out <- to_list(bs)
  expect_equal(out$type, "bar")
  expect_equal(out$name, "Revenue")
  expect_equal(out$stack, "total")
  expect_equal(out$barWidth, "60%")
  expect_equal(out$barGap, "30%")
  expect_equal(out$barCategoryGap, "20%")
  expect_equal(out$barMinHeight, 5)
  expect_equal(out$roundCap, TRUE)
  expect_equal(out$showBackground, TRUE)
})

test_that("BarSeries bar_width accepts number and string", {
  bs1 <- BarSeries(bar_width = 30)
  expect_equal(bs1@bar_width, 30)
  bs2 <- BarSeries(bar_width = "50%")
  expect_equal(bs2@bar_width, "50%")
})

test_that("BarSeries item_style and label nest", {
  bs <- BarSeries(
    item_style = ItemStyle(color = "#4CAF50", border_radius = c(4, 4, 0, 0)),
    label = LabelOption(show = TRUE, position = "inside")
  )
  out <- to_list(bs)
  expect_equal(out$itemStyle$color, "#4CAF50")
  expect_equal(out$itemStyle$borderRadius, c(4, 4, 0, 0))
  expect_equal(out$label$show, TRUE)
  expect_equal(out$label$position, "inside")
})

# -- ScatterSeries --------------------------------------------------------------

test_that("ScatterSeries creates with defaults", {
  ss <- ScatterSeries()
  out <- to_list(ss)
  expect_equal(out$type, "scatter")
  expect_equal(length(out), 1L)
})

test_that("ScatterSeries to_list() converts names", {
  ss <- ScatterSeries(
    name = "Points",
    data = list(c(1, 2), c(3, 4), c(5, 6)),
    symbol = "circle",
    symbol_size = 10,
    large = TRUE,
    large_threshold = 2000
  )
  out <- to_list(ss)
  expect_equal(out$type, "scatter")
  expect_equal(out$name, "Points")
  expect_equal(out$symbol, "circle")
  expect_equal(out$symbolSize, 10)
  expect_equal(out$large, TRUE)
  expect_equal(out$largeThreshold, 2000)
})

test_that("ScatterSeries symbol_size accepts function", {
  fn <- function(value) sqrt(value[3]) * 10
  ss <- ScatterSeries(symbol_size = fn)
  expect_true(is.function(ss@symbol_size))
})

test_that("ScatterSeries item_style nests", {
  ss <- ScatterSeries(
    item_style = ItemStyle(opacity = 0.7, color = "#2196F3")
  )
  out <- to_list(ss)
  expect_equal(out$itemStyle$opacity, 0.7)
  expect_equal(out$itemStyle$color, "#2196F3")
})

# -- PieSeries ------------------------------------------------------------------

test_that("PieSeries creates with defaults", {
  ps <- PieSeries()
  out <- to_list(ps)
  expect_equal(out$type, "pie")
  expect_equal(length(out), 1L)
})

test_that("PieSeries to_list() converts names", {
  ps <- PieSeries(
    name = "Browser",
    data = list(
      list(value = 1048, name = "Chrome"),
      list(value = 735, name = "Firefox")
    ),
    center = c("50%", "50%"),
    radius = c("40%", "70%"),
    clockwise = TRUE,
    start_angle = 90,
    pad_angle = 2,
    avoid_label_overlap = TRUE
  )
  out <- to_list(ps)
  expect_equal(out$type, "pie")
  expect_equal(out$name, "Browser")
  expect_equal(out$center, c("50%", "50%"))
  expect_equal(out$radius, c("40%", "70%"))
  expect_equal(out$clockwise, TRUE)
  expect_equal(out$startAngle, 90)
  expect_equal(out$padAngle, 2)
  expect_equal(out$avoidLabelOverlap, TRUE)
})

test_that("PieSeries rose_type validation", {
  expect_equal(PieSeries(rose_type = "radius")@rose_type, "radius")
  expect_equal(PieSeries(rose_type = "area")@rose_type, "area")
  expect_error(PieSeries(rose_type = "invalid"))
})

test_that("PieSeries center validation", {
  ps1 <- PieSeries(center = c(300, 200))
  expect_equal(ps1@center, c(300, 200))
  ps2 <- PieSeries(center = c("50%", "50%"))
  expect_equal(ps2@center, c("50%", "50%"))
  expect_error(PieSeries(center = c(1, 2, 3)))
})

test_that("PieSeries radius accepts single and pair", {
  ps1 <- PieSeries(radius = "75%")
  expect_equal(ps1@radius, "75%")
  ps2 <- PieSeries(radius = c("40%", "70%"))
  expect_equal(ps2@radius, c("40%", "70%"))
  ps3 <- PieSeries(radius = 100)
  expect_equal(ps3@radius, 100)
})

test_that("PieSeries end_angle accepts number and 'auto'", {
  ps1 <- PieSeries(end_angle = 360)
  expect_equal(ps1@end_angle, 360)
  ps2 <- PieSeries(end_angle = "auto")
  expect_equal(ps2@end_angle, "auto")
  expect_error(PieSeries(end_angle = "bad"))
})

test_that("PieSeries label_line nests correctly", {
  ps <- PieSeries(
    label_line = LabelLine(show = TRUE, length = 20, length2 = 10)
  )
  out <- to_list(ps)
  expect_equal(out$labelLine$show, TRUE)
  expect_equal(out$labelLine$length, 20)
  expect_equal(out$labelLine$length2, 10)
})

test_that("PieSeries animation_type validation", {
  expect_equal(PieSeries(animation_type = "expansion")@animation_type, "expansion")
  expect_equal(PieSeries(animation_type = "scale")@animation_type, "scale")
  expect_error(PieSeries(animation_type = "fade"))
})

# -- BoxplotSeries --------------------------------------------------------------

test_that("BoxplotSeries creates with defaults", {
  bp <- BoxplotSeries()
  out <- to_list(bp)
  expect_equal(out$type, "boxplot")
  expect_equal(length(out), 1L)
})

test_that("BoxplotSeries to_list() converts names", {
  bp <- BoxplotSeries(
    name = "Distribution",
    data = list(c(850, 940, 960, 980, 1070),
                c(750, 800, 840, 890, 1020)),
    layout = "horizontal"
  )
  out <- to_list(bp)
  expect_equal(out$type, "boxplot")
  expect_equal(out$name, "Distribution")
  expect_equal(out$layout, "horizontal")
})

test_that("BoxplotSeries layout validation", {
  expect_equal(BoxplotSeries(layout = "horizontal")@layout, "horizontal")
  expect_equal(BoxplotSeries(layout = "vertical")@layout, "vertical")
  expect_error(BoxplotSeries(layout = "diagonal"))
})

test_that("BoxplotSeries box_width validation", {
  bp <- BoxplotSeries(box_width = c(7, 50))
  expect_equal(bp@box_width, c(7, 50))
  bp2 <- BoxplotSeries(box_width = c("10%", "50%"))
  expect_equal(bp2@box_width, c("10%", "50%"))
  expect_error(BoxplotSeries(box_width = 30))  # must be length 2
})

test_that("BoxplotSeries item_style nests", {
  bp <- BoxplotSeries(
    item_style = ItemStyle(color = "#fff", border_color = "#333", border_width = 2)
  )
  out <- to_list(bp)
  expect_equal(out$itemStyle$color, "#fff")
  expect_equal(out$itemStyle$borderColor, "#333")
  expect_equal(out$itemStyle$borderWidth, 2)
})

# -- Common series fields -------------------------------------------------------

test_that("Series z_level and z work", {
  ls <- LineSeries(z_level = 1, z = 2)
  out <- to_list(ls)
  expect_equal(out$zLevel, 1)  # snake_to_camel should not touch this (zLevel)
  expect_equal(out$z, 2)
})

test_that("Series legend_hover_link works", {
  bs <- BarSeries(legend_hover_link = FALSE)
  out <- to_list(bs)
  expect_equal(out$legendHoverLink, FALSE)
})

test_that("Series color works", {
  ss <- ScatterSeries(color = "#FF5722")
  out <- to_list(ss)
  expect_equal(out$color, "#FF5722")
})
