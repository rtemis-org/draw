# test-draw_gantt.R
# Tests for draw_gantt() and its internal helpers

tasks_df <- function() {
  data.frame(
    label = c("load", "clean", "train", "predict"),
    start = c(0, 12, 30, 95),
    end = c(12, 30, 95, 100),
    status = c("ok", "ok", "ok", "error"),
    stringsAsFactors = FALSE
  )
}


# -- Internal helpers -----------------------------------------------------------

test_that(".gantt_time_values converts POSIXct to epoch milliseconds", {
  t <- as.POSIXct("2026-01-01 00:00:01", tz = "UTC")
  expect_equal(
    rtemis.draw:::.gantt_time_values(t),
    as.numeric(t) * 1000
  )
})


test_that(".gantt_time_values converts Date to epoch milliseconds", {
  d <- as.Date("2026-01-01")
  expect_equal(
    rtemis.draw:::.gantt_time_values(d),
    as.numeric(as.POSIXct(d)) * 1000
  )
})


test_that(".gantt_time_values passes numeric through unchanged", {
  expect_equal(rtemis.draw:::.gantt_time_values(c(0, 12.5, 30)), c(0, 12.5, 30))
})


test_that(".gantt_render_item embeds params and border and is JS", {
  js <- rtemis.draw:::.gantt_render_item(0.6, 4, "#E53935", 1.5)
  expect_s3_class(js, "JS_EVAL")
  expect_match(as.character(js), "0.6", fixed = TRUE)
  expect_match(as.character(js), "api.visual('color')", fixed = TRUE)
  expect_match(as.character(js), "#E53935", fixed = TRUE)
})


# -- Input validation -----------------------------------------------------------

test_that("draw_gantt errors on missing required columns", {
  bad <- data.frame(label = "a", start = 0)
  expect_error(draw_gantt(bad), "end")
})


test_that("draw_gantt errors on invalid axis_type", {
  expect_error(draw_gantt(tasks_df(), axis_type = "log"), "axis_type")
})


test_that("draw_gantt errors on NA start or end values", {
  bad_start <- tasks_df()
  bad_start[["start"]][2L] <- NA
  expect_error(draw_gantt(bad_start), "missing values")

  bad_end <- tasks_df()
  bad_end[["end"]][3L] <- NA
  expect_error(draw_gantt(bad_end), "missing values")
})


test_that("draw_gantt errors on unknown group column", {
  expect_error(draw_gantt(tasks_df(), group = "nope"), "group")
})


test_that("draw_gantt errors on unknown tooltip column", {
  expect_error(draw_gantt(tasks_df(), tooltip = "nope"), "tooltip")
})


# -- Structure ------------------------------------------------------------------

test_that("draw_gantt returns an htmlwidget", {
  expect_s3_class(draw_gantt(tasks_df()), "htmlwidget")
})


test_that("draw_gantt without group produces one custom series with all bars", {
  w <- draw_gantt(tasks_df())
  series <- w$x$option$series
  expect_length(series, 1L)
  expect_equal(series[[1]]$type, "custom")
  expect_length(series[[1]]$data, 4L)
  # No legend without grouping
  expect_null(w$x$option$legend)
})


test_that("draw_gantt y-axis is an inverse category axis of unique labels", {
  w <- draw_gantt(tasks_df())
  y <- w$x$option$yAxis
  expect_equal(y$type, "category")
  expect_true(isTRUE(y$inverse))
  expect_equal(unlist(y$data), c("load", "clean", "train", "predict"))
})


test_that("draw_gantt x-axis type follows axis_type", {
  expect_equal(draw_gantt(tasks_df())$x$option$xAxis$type, "value")
  expect_equal(
    draw_gantt(tasks_df(), axis_type = "time")$x$option$xAxis$type,
    "time"
  )
})


test_that("draw_gantt encodes each bar as [rowIndex, start, end]", {
  w <- draw_gantt(tasks_df())
  v <- w$x$option$series[[1]]$data[[2]]$value
  # 2nd task "clean": row index 1 (0-based), start 12, end 30
  expect_equal(unlist(v), c(1, 12, 30))
})


test_that("draw_gantt repeated labels collapse onto one row", {
  df <- data.frame(
    label = c("worker", "worker", "worker"),
    start = c(0, 10, 25),
    end = c(10, 25, 40),
    stringsAsFactors = FALSE
  )
  w <- draw_gantt(df)
  # One category, three bars all on row 0
  expect_equal(unlist(w$x$option$yAxis$data), "worker")
  rows <- vapply(
    w$x$option$series[[1]]$data,
    function(d) d$value[[1]],
    numeric(1L)
  )
  expect_equal(rows, c(0, 0, 0))
})


# -- Grouping / color -----------------------------------------------------------

test_that("draw_gantt with group emits one series per level with a legend", {
  w <- draw_gantt(tasks_df(), group = "status")
  series <- w$x$option$series
  names_s <- vapply(series, function(s) s$name %||% "", character(1L))
  expect_setequal(names_s, c("ok", "error"))
  expect_false(is.null(w$x$option$legend))
})


test_that("draw_gantt group series carry distinct itemStyle colors", {
  w <- draw_gantt(tasks_df(), group = "status", color = c("#111111", "#222222"))
  series <- w$x$option$series
  cols <- vapply(
    series,
    function(s) s$data[[1]]$itemStyle$color,
    character(1L)
  )
  expect_equal(cols, c("#111111", "#222222"))
})


test_that("draw_gantt time axis converts POSIXct bars to epoch milliseconds", {
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  df <- data.frame(label = "a", stringsAsFactors = FALSE)
  df$start <- t0
  df$end <- t0 + 5
  w <- draw_gantt(df, axis_type = "time")
  v <- w$x$option$series[[1]]$data[[1]]$value
  expect_equal(v[[2]], as.numeric(t0) * 1000)
  expect_equal(v[[3]], as.numeric(t0 + 5) * 1000)
})


test_that("draw_gantt guides toggles the x-axis axisPointer", {
  on_ap <- draw_gantt(tasks_df(), guides = TRUE)$x$option$xAxis$axisPointer
  expect_true(isTRUE(on_ap$show))
  expect_equal(on_ap$type, "line")
  expect_null(draw_gantt(tasks_df(), guides = FALSE)$x$option$xAxis$axisPointer)
})


test_that("draw_gantt zoom adds inside dataZoom and a toolbox with reset", {
  w <- draw_gantt(tasks_df(), zoom = TRUE)
  dz <- w$x$option$dataZoom
  expect_length(dz, 2L)
  expect_true(all(vapply(dz, function(z) z$type, character(1L)) == "inside"))
  tb <- w$x$option$toolbox
  expect_false(is.null(tb$feature$dataZoom))
  expect_false(is.null(tb$feature$restore))
  # Disabled -> neither present.
  w0 <- draw_gantt(tasks_df(), zoom = FALSE)
  expect_null(w0$x$option$dataZoom)
  expect_null(w0$x$option$toolbox)
})


test_that("draw_gantt border flags bars with a 4th data value", {
  df <- tasks_df()
  df$bad <- c(FALSE, FALSE, FALSE, TRUE)
  w <- draw_gantt(df, border = "bad")
  flags <- vapply(
    w$x$option$series[[1]]$data,
    function(d) d$value[[4]],
    integer(1L)
  )
  expect_equal(flags, c(0L, 0L, 0L, 1L))
})


test_that("draw_gantt errors on unknown border column", {
  expect_error(draw_gantt(tasks_df(), border = "nope"), "border")
})


test_that("draw_gantt grid gets ECharts 6.1 outerBoundsContain via draw()", {
  # x-axis name present -> reserve room for it ("all"); none -> tight ("axisLabel").
  expect_equal(
    draw_gantt(
      tasks_df(),
      xlab = "Elapsed (ms)"
    )$x$option$grid$outerBoundsContain,
    "all"
  )
  expect_equal(
    draw_gantt(tasks_df())$x$option$grid$outerBoundsContain,
    "axisLabel"
  )
})
