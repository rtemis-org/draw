test_that("rolling windows preserve alignment, missing slots and full windows", {
  for (align in c("left", "right", "center")) {
    config <- setup_TimeSeriesConfig(
      window = 4L,
      roll_fn = "mean",
      align = align
    )
    got <- rolling_values(1:8, config)
    expected <- switch(
      align,
      left = c(2.5, 3.5, 4.5, 5.5, 6.5, NA, NA, NA),
      right = c(NA, NA, NA, 2.5, 3.5, 4.5, 5.5, 6.5),
      center = c(NA, 2.5, 3.5, 4.5, 5.5, 6.5, NA, NA)
    )
    expect_equal(got, expected)
  }
  for (fn in c("mean", "median", "min", "max", "sum")) {
    config <- setup_TimeSeriesConfig(window = 3L, roll_fn = fn, align = "right")
    expected <- switch(fn, mean = 3, median = 2, min = 1, max = 6, sum = 9)
    expect_equal(rolling_values(c(1, 2, 6), config), c(NA, NA, expected))
    expect_true(all(is.na(rolling_values(c(1, NA, 6), config))))
    expect_true(all(is.na(rolling_values(c(1, 2), config))))
  }
  expect_error(setup_TimeSeriesConfig(window = 2.5))
  expect_error(setup_TimeSeriesConfig(window = 0L))
  expect_error(setup_TimeSeriesConfig(roll_fn = "invalid"))
  expect_error(setup_TimeSeriesConfig(raw_opacity = 2))
})

test_that("independent time samples sort pairs without interpolating missing records", {
  samples <- timeseries_samples(
    list(c(3, 1, 2), c(2, 1)),
    list(A = c(30, 10, NA), B = c(8, 7))
  )
  expect_equal(samples[["A"]][["x"]], 1:3)
  expect_equal(samples[["A"]][["y"]], c(10, NA, 30))
  expect_equal(samples[["B"]][["y"]], c(7, 8))
  grouped <- timeseries_samples(c(3, 1, 2), c(6, 2, 4), c("B", "A", "B"))
  expect_named(grouped, c("B", "A"))
  expect_equal(grouped[["B"]][["y"]], c(4, 6))
  expect_error(timeseries_samples(1:2, 1:3))
  expect_error(timeseries_samples(c(1, Inf), 1:2))
  expect_error(timeseries_samples(1:2, c(NA_real_, NA_real_)))
  expect_error(timeseries_samples(
    1:2,
    list(A = 1:2, B = 2:3),
    group = c("a", "b")
  ))
})

test_that("native time series share legends but preserve independent axes and scales", {
  widget <- draw_ts(
    list(A = c(3, 1, 2), B = c(6, 2, 4)),
    c(3, 1, 2),
    window = 3L,
    roll_fn = "sum",
    zoom = TRUE
  )
  option <- widget[["x"]][["option"]]
  expect_equal(option[["series"]][[2]][["data"]][[2]], list(2, 6))
  expect_equal(option[["series"]][[4]][["data"]][[2]], list(2, 12))
  expect_equal(
    option[["series"]][[1]][["name"]],
    option[["series"]][[2]][["name"]]
  )
  expect_null(option[["yAxis"]][[1]][["max"]])
  expect_length(option[["dataZoom"]], 2)
  dual <- draw_xt(1:3, c(2, 4, 6), x2 = c(.5, 2.5), y2 = c(100, 200))[["x"]][[
    "option"
  ]]
  expect_length(dual[["yAxis"]], 2)
  expect_equal(dual[["series"]][[2]][["yAxisIndex"]], 1L)
  expect_equal(dual[["series"]][[2]][["data"]][[1]], list(.5, 100))
  temporal <- draw_ts(1:3, as.Date("2020-01-01") + 0:2, roll_fn = "none")[[
    "x"
  ]][["option"]]
  expect_equal(temporal[["xAxis"]][["type"]], "time")
  expect_equal(
    diff(vapply(temporal[["series"]][[1]][["data"]], `[[`, 0, 1)),
    rep(86400000, 2)
  )
  expect_error(
    draw_xt(1:3, 1:3, x2 = as.Date("2020-01-01") + 0:2, y2 = 1:3),
    "throughout"
  )
  cfg <- setup_TimeSeriesConfig(
    x = "Time",
    y = "demand",
    roll_fn = "mean",
    window = 3L
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(cfg, path)
  expect_equal(
    to_list(compile(read_chart_config(path), BOD)),
    to_list(compile(cfg, BOD))
  )
})

test_that("p-value bar convenience selects one-minus transformation", {
  p <- c(.01, .3, NA, .06)
  actual <- draw_pvals(p, xnames = LETTERS[1:4], p_adjust_method = "holm")
  expected <- draw_manhattan(
    rep(0, 4),
    p,
    xnames = LETTERS[1:4],
    p_adjust_method = "holm",
    p_transform = "one_minus",
    annotate_n = 0L
  )
  expect_equal(actual[["x"]], expected[["x"]])
  expect_error(draw_pvals(c(.1, 1.5)))
})

test_that("zeitgeber labels preserve irregular time spacing and shading alignment", {
  opt <- draw_xt(
    c(0, 2, 8, 24),
    c(1, 3, 2, 4),
    zt = c(0, 2, 8, 0),
    show_zt_every = 3L,
    shade_bin = c(0, 1, 1, 0)
  )[["x"]][["option"]]
  labels <- tail(opt[["series"]], 1)[[1]]
  expect_equal(labels[["renderItem"]], "rtemis.axis_labels.v1")
  expect_equal(labels[["data"]], list(list(0, "0"), list(24, "0")))
  expect_false(opt[["xAxis"]][["axisLabel"]][["show"]])
  expect_error(draw_xt(1:3, 1:3, zt = 1:2))
  expect_error(draw_xt(1:3, 1:3, shade_bin = c(0, 2, 0)))
  expect_error(draw_xt(
    1:3,
    1:3,
    shade_bin = c(0, 1, 0),
    shade_interval = list(c(1, 2))
  ))
})

test_that("time annotations use the same data-binding and functional path", {
  data <- data.frame(
    time = 1:6,
    value = 2:7,
    phase = c(0, 4, 8, 12, 16, 20),
    night = c(0, 0, 1, 1, 0, 0)
  )
  cfg <- setup_TimeSeriesConfig(
    x = "time",
    y = "value",
    zt = "phase",
    shade_bin = "night",
    show_zt_every = 2L
  )
  compiled <- to_list(compile(cfg, data))
  direct <- draw_xt(
    data[["time"]],
    list(value = data[["value"]]),
    zt = data[["phase"]],
    shade_bin = data[["night"]],
    show_zt_every = 2L
  )[["x"]][["option"]]
  expect_equal(compiled[["series"]], direct[["series"]])
  expect_equal(
    to_list(compile(
      read_chart_config(write_chart_config(cfg, tempfile(fileext = ".json"))),
      data
    )),
    compiled
  )
  expect_false(compiled[["xAxis"]][["axisLabel"]][["show"]])
})
