# test-axis.R
# Tests for axis-related S7 classes

# -- AxisLine -------------------------------------------------------------------

test_that("AxisLine creates with defaults (all NULL)", {
  al <- AxisLine()
  expect_true(S7::S7_inherits(al, AxisLine))
  expect_null(al@show)
  expect_null(al@on_zero)
  expect_null(al@on_zero_axis_index)
  expect_null(al@symbol)
  expect_null(al@symbol_size)
  expect_null(al@symbol_offset)
  expect_null(al@line_style)
})

test_that("AxisLine to_list() drops NULLs", {
  expect_equal(to_list(AxisLine()), list())
})

test_that("AxisLine to_list() converts names", {
  al <- AxisLine(
    show = TRUE,
    on_zero = FALSE,
    on_zero_axis_index = 1,
    symbol = c("none", "arrow"),
    symbol_size = c(10, 15)
  )
  out <- to_list(al)
  expect_equal(out$show, TRUE)
  expect_equal(out$onZero, FALSE)
  expect_equal(out$onZeroAxisIndex, 1)
  expect_equal(out$symbol, c("none", "arrow"))
  expect_equal(out$symbolSize, c(10, 15))
})

test_that("AxisLine show accepts 'auto'", {
  al <- AxisLine(show = "auto")
  expect_equal(al@show, "auto")
})

test_that("AxisLine show rejects invalid", {
  expect_error(AxisLine(show = "bad"))
})

test_that("AxisLine line_style nests correctly", {
  al <- AxisLine(
    show = TRUE,
    line_style = LineStyle(color = "#333", width = 2)
  )
  out <- to_list(al)
  expect_equal(out$lineStyle$color, "#333")
  expect_equal(out$lineStyle$width, 2)
})

# -- AxisTick -------------------------------------------------------------------

test_that("AxisTick creates with defaults", {
  at <- AxisTick()
  expect_true(S7::S7_inherits(at, AxisTick))
  expect_equal(to_list(at), list())
})

test_that("AxisTick to_list() converts names", {
  at <- AxisTick(
    show = TRUE,
    inside = FALSE,
    length = 5,
    align_with_label = TRUE
  )
  out <- to_list(at)
  expect_equal(out$show, TRUE)
  expect_equal(out$inside, FALSE)
  expect_equal(out$length, 5)
  expect_equal(out$alignWithLabel, TRUE)
})

test_that("AxisTick show accepts 'auto'", {
  at <- AxisTick(show = "auto")
  expect_equal(at@show, "auto")
})

test_that("AxisTick line_style nests correctly", {
  at <- AxisTick(line_style = LineStyle(color = "#ccc"))
  out <- to_list(at)
  expect_equal(out$lineStyle$color, "#ccc")
})

# -- MinorTick ------------------------------------------------------------------

test_that("MinorTick creates with defaults", {
  mt <- MinorTick()
  expect_equal(to_list(mt), list())
})

test_that("MinorTick to_list() converts names", {
  mt <- MinorTick(show = TRUE, split_number = 5, length = 3)
  out <- to_list(mt)
  expect_equal(out$show, TRUE)
  expect_equal(out$splitNumber, 5)
  expect_equal(out$length, 3)
})

# -- SplitLine ------------------------------------------------------------------

test_that("SplitLine creates with defaults", {
  sl <- SplitLine()
  expect_equal(to_list(sl), list())
})

test_that("SplitLine to_list() converts names", {
  sl <- SplitLine(
    show = TRUE,
    interval = 2,
    show_min_line = FALSE,
    show_max_line = TRUE
  )
  out <- to_list(sl)
  expect_equal(out$show, TRUE)
  expect_equal(out$interval, 2)
  expect_equal(out$showMinLine, FALSE)
  expect_equal(out$showMaxLine, TRUE)
})

test_that("SplitLine interval accepts 'auto'", {
  sl <- SplitLine(interval = "auto")
  expect_equal(sl@interval, "auto")
})

test_that("SplitLine interval rejects invalid", {
  expect_error(SplitLine(interval = "bad"))
})

test_that("SplitLine line_style nests correctly", {
  sl <- SplitLine(
    show = TRUE,
    line_style = LineStyle(type = "dashed", color = "#eee")
  )
  out <- to_list(sl)
  expect_equal(out$lineStyle$type, "dashed")
  expect_equal(out$lineStyle$color, "#eee")
})

# -- MinorSplitLine -------------------------------------------------------------

test_that("MinorSplitLine creates with defaults", {
  msl <- MinorSplitLine()
  expect_equal(to_list(msl), list())
})

test_that("MinorSplitLine to_list() works", {
  msl <- MinorSplitLine(
    show = TRUE,
    line_style = LineStyle(type = "dotted")
  )
  out <- to_list(msl)
  expect_equal(out$show, TRUE)
  expect_equal(out$lineStyle$type, "dotted")
})

# -- SplitArea ------------------------------------------------------------------

test_that("SplitArea creates with defaults", {
  sa <- SplitArea()
  expect_equal(to_list(sa), list())
})

test_that("SplitArea to_list() converts names", {
  sa <- SplitArea(show = TRUE, interval = 1)
  out <- to_list(sa)
  expect_equal(out$show, TRUE)
  expect_equal(out$interval, 1)
})

test_that("SplitArea area_style nests correctly", {
  sa <- SplitArea(
    show = TRUE,
    area_style = AreaStyle(opacity = 0.3)
  )
  out <- to_list(sa)
  expect_equal(out$areaStyle$opacity, 0.3)
})

# -- AxisLabel ------------------------------------------------------------------

test_that("AxisLabel creates with defaults", {
  al <- AxisLabel()
  expect_true(S7::S7_inherits(al, AxisLabel))
  expect_equal(to_list(al), list())
})

test_that("AxisLabel to_list() converts names", {
  al <- AxisLabel(
    show = TRUE,
    inside = FALSE,
    rotate = 45,
    margin = 8,
    show_min_label = TRUE,
    show_max_label = FALSE,
    hide_overlap = TRUE
  )
  out <- to_list(al)
  expect_equal(out$show, TRUE)
  expect_equal(out$inside, FALSE)
  expect_equal(out$rotate, 45)
  expect_equal(out$margin, 8)
  expect_equal(out$showMinLabel, TRUE)
  expect_equal(out$showMaxLabel, FALSE)
  expect_equal(out$hideOverlap, TRUE)
})

test_that("AxisLabel flattens text_style", {
  al <- AxisLabel(
    show = TRUE,
    rotate = 30,
    text_style = TextStyle(color = "#666", font_size = 12)
  )
  out <- to_list(al)
  # Flattened
  expect_equal(out$color, "#666")
  expect_equal(out$fontSize, 12)
  # No nested textStyle
  expect_null(out$textStyle)
  # AxisLabel fields preserved
  expect_equal(out$show, TRUE)
  expect_equal(out$rotate, 30)
})

test_that("AxisLabel interval accepts 'auto' and number", {
  al1 <- AxisLabel(interval = "auto")
  expect_equal(al1@interval, "auto")
  al2 <- AxisLabel(interval = 2)
  expect_equal(al2@interval, 2)
})

test_that("AxisLabel interval rejects invalid", {
  expect_error(AxisLabel(interval = "bad"))
})

test_that("AxisLabel formatter accepts string", {
  al <- AxisLabel(formatter = "{value} kg")
  expect_equal(al@formatter, "{value} kg")
})

# -- Axis -----------------------------------------------------------------------

test_that("Axis creates with defaults", {
  ax <- Axis()
  expect_true(S7::S7_inherits(ax, Axis))
  expect_equal(to_list(ax), list())
})

test_that("Axis type validation", {
  for (tp in c("value", "category", "time", "log")) {
    ax <- Axis(type = tp)
    expect_equal(ax@type, tp)
  }
  expect_error(Axis(type = "invalid"))
})

test_that("Axis to_list() converts all names correctly", {
  ax <- Axis(
    type = "value",
    show = TRUE,
    inverse = FALSE,
    name = "X Axis",
    name_location = "middle",
    name_rotate = 0,
    name_gap = 30,
    split_number = 5,
    min_interval = 1,
    max_interval = 100,
    align_ticks = TRUE,
    trigger_event = FALSE
  )
  out <- to_list(ax)
  expect_equal(out$type, "value")
  expect_equal(out$show, TRUE)
  expect_equal(out$inverse, FALSE)
  expect_equal(out$name, "X Axis")
  expect_equal(out$nameLocation, "middle")
  expect_equal(out$nameRotate, 0)
  expect_equal(out$nameGap, 30)
  expect_equal(out$splitNumber, 5)
  expect_equal(out$minInterval, 1)
  expect_equal(out$maxInterval, 100)
  expect_equal(out$alignTicks, TRUE)
  expect_equal(out$triggerEvent, FALSE)
})

test_that("Axis min/max accept number and data strings", {
  ax1 <- Axis(min = 0, max = 100)
  expect_equal(ax1@min, 0)
  expect_equal(ax1@max, 100)

  ax2 <- Axis(min = "dataMin", max = "dataMax")
  expect_equal(ax2@min, "dataMin")
  expect_equal(ax2@max, "dataMax")
})

test_that("Axis min/max reject invalid strings", {
  expect_error(Axis(min = "bad"))
  expect_error(Axis(max = "bad"))
})

test_that("Axis boundary_gap accepts logical and vector", {
  ax1 <- Axis(type = "category", boundary_gap = TRUE)
  expect_equal(ax1@boundary_gap, TRUE)

  ax2 <- Axis(type = "value", boundary_gap = c("5%", "5%"))
  expect_equal(ax2@boundary_gap, c("5%", "5%"))

  ax3 <- Axis(type = "value", boundary_gap = c(0, 0.1))
  expect_equal(ax3@boundary_gap, c(0, 0.1))
})

test_that("Axis boundary_gap rejects invalid", {
  expect_error(Axis(boundary_gap = "bad"))
})

test_that("Axis name_location validation", {
  for (loc in c("start", "middle", "center", "end")) {
    ax <- Axis(name_location = loc)
    expect_equal(ax@name_location, loc)
  }
  expect_error(Axis(name_location = "invalid"))
})

test_that("Axis log_base works for log axis", {
  ax <- Axis(type = "log", log_base = 10)
  out <- to_list(ax)
  expect_equal(out$type, "log")
  expect_equal(out$logBase, 10)
})

test_that("Axis scale works for value axis", {
  ax <- Axis(type = "value", scale = TRUE)
  out <- to_list(ax)
  expect_equal(out$scale, TRUE)
})

test_that("Axis category data", {
  ax <- Axis(
    type = "category",
    data = c("Mon", "Tue", "Wed", "Thu", "Fri")
  )
  out <- to_list(ax)
  expect_equal(out$type, "category")
  expect_equal(out$data, c("Mon", "Tue", "Wed", "Thu", "Fri"))
})

test_that("Axis sub-components nest correctly", {
  ax <- Axis(
    type = "value",
    axis_line = AxisLine(show = TRUE, line_style = LineStyle(color = "#000")),
    axis_tick = AxisTick(show = TRUE, length = 5),
    minor_tick = MinorTick(show = TRUE, split_number = 3),
    axis_label = AxisLabel(show = TRUE, rotate = 45,
                           text_style = TextStyle(font_size = 11)),
    split_line = SplitLine(show = TRUE,
                           line_style = LineStyle(type = "dashed")),
    minor_split_line = MinorSplitLine(show = FALSE),
    split_area = SplitArea(show = FALSE)
  )
  out <- to_list(ax)

  # axisLine
  expect_equal(out$axisLine$show, TRUE)
  expect_equal(out$axisLine$lineStyle$color, "#000")
  # axisTick
  expect_equal(out$axisTick$show, TRUE)
  expect_equal(out$axisTick$length, 5)
  # minorTick
  expect_equal(out$minorTick$show, TRUE)
  expect_equal(out$minorTick$splitNumber, 3)
  # axisLabel (with flattened text_style)
  expect_equal(out$axisLabel$show, TRUE)
  expect_equal(out$axisLabel$rotate, 45)
  expect_equal(out$axisLabel$fontSize, 11)
  expect_null(out$axisLabel$textStyle)
  # splitLine
  expect_equal(out$splitLine$show, TRUE)
  expect_equal(out$splitLine$lineStyle$type, "dashed")
  # minorSplitLine
  expect_equal(out$minorSplitLine$show, FALSE)
  # splitArea
  expect_equal(out$splitArea$show, FALSE)
})

test_that("Axis name_text_style nests correctly", {
  ax <- Axis(
    name = "Y",
    name_text_style = TextStyle(color = "#333", font_weight = "bold")
  )
  out <- to_list(ax)
  expect_equal(out$nameTextStyle$color, "#333")
  expect_equal(out$nameTextStyle$fontWeight, "bold")
})
