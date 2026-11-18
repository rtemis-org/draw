test_that("rolling summaries come from the builder, as configured", {
  skip_if_no_node()
  o <- raw_option(draw_ts(
    1:8,
    1:8,
    window = 4L,
    roll_fn = "mean",
    align = "left"
  ))
  rolled <- vapply(
    o[["series"]][[2L]][["data"]],
    function(p) p[[2L]] %||% NA_real_,
    0
  )
  expect_equal(rolled, c(2.5, 3.5, 4.5, 5.5, 6.5, 7, NA, NA))
  o <- raw_option(draw_ts(
    c(1, 2, 6),
    1:3,
    window = 3L,
    roll_fn = "sum",
    align = "right"
  ))
  rolled <- vapply(
    o[["series"]][[2L]][["data"]],
    function(p) p[[2L]] %||% NA_real_,
    0
  )
  expect_equal(rolled, c(NA, NA, 9))
  expect_error(
    setup_TimeSeriesConfig(window = 3L, min_obs = 4L),
    "cannot exceed"
  )
  expect_error(setup_TimeSeriesConfig(window = 2.5))
  expect_error(setup_TimeSeriesConfig(window = 0L))
  expect_error(setup_TimeSeriesConfig(roll_fn = "invalid"))
  expect_error(setup_TimeSeriesConfig(raw_opacity = 2))
})

test_that("independent time samples stack into one table without interpolating", {
  table <- timeseries_table(
    list(c(3, 1, 2), c(2, 1)),
    list(A = c(30, 10, NA), B = c(8, 7))
  )
  expect_identical(
    table[["bindings"]],
    list(x = ".x", y = ".y", group = ".series")
  )
  expect_equal(table[["data"]][[".y"]], c(30, 10, NA, 8, 7))
  expect_identical(levels(table[["data"]][[".series"]]), c("A", "B"))
  grouped <- timeseries_table(c(3, 1, 2), c(6, 2, 4), c("B", "A", "B"))
  expect_identical(grouped[["bindings"]][["group"]], ".group")
  expect_error(timeseries_table(1:2, 1:3))
  expect_error(timeseries_table(c(1, Inf), 1:2))
  expect_error(timeseries_table(
    1:2,
    list(A = 1:2, B = 2:3),
    group = c("a", "b")
  ))
  skip_if_no_node()
  o <- raw_option(draw_ts(
    list(A = c(30, 10, NA), B = c(8, 7)),
    list(c(3, 1, 2), c(2, 1)),
    roll_fn = "none"
  ))
  expect_identical(vapply(o[["series"]], `[[`, "", "name"), c("A", "B"))
  expect_equal(
    o[["series"]][[1L]][["data"]],
    list(list(1, 10), list(2, NULL), list(3, 30))
  )
  expect_error(
    built_option(draw_ts(c(NA_real_, NA_real_), 1:2)),
    "at least one time"
  )
})

test_that("native time series share legends but preserve independent axes and scales", {
  skip_if_no_node()
  option <- raw_option(draw_ts(
    list(A = c(3, 1, 2), B = c(6, 2, 4)),
    c(3, 1, 2),
    window = 3L,
    roll_fn = "sum",
    zoom = TRUE
  ))
  expect_equal(option[["series"]][[2]][["data"]][[2]], list(2, 6))
  expect_equal(option[["series"]][[4]][["data"]][[2]], list(2, 12))
  expect_equal(
    option[["series"]][[1]][["name"]],
    option[["series"]][[2]][["name"]]
  )
  expect_null(option[["yAxis"]][["max"]])
  expect_length(option[["dataZoom"]], 2)
  dual <- raw_option(draw_xt(
    1:3,
    c(2, 4, 6),
    x2 = c(.5, 2.5),
    y2 = c(100, 200)
  ))
  expect_length(dual[["yAxis"]], 2)
  expect_equal(dual[["series"]][[2]][["yAxisIndex"]], 1L)
  expect_equal(dual[["series"]][[2]][["data"]][[1]], list(.5, 100))
  temporal <- raw_option(draw_ts(
    1:3,
    as.Date("2020-01-01") + 0:2,
    roll_fn = "none"
  ))
  expect_equal(temporal[["xAxis"]][["type"]], "time")
  expect_equal(
    diff(vapply(temporal[["series"]][[1]][["data"]], function(p) p[[1L]], 0)),
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
  expect_identical(compile(read_chart_config(path), BOD), compile(cfg, BOD))
  expect_identical(compile(cfg, BOD)@type, "timeseries")
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
  skip_if_no_node()
  opt <- raw_option(draw_xt(
    c(0, 2, 8, 24),
    c(1, 3, 2, 4),
    zt = c(0, 2, 8, 0),
    show_zt_every = 3L,
    shade_bin = c(0, 1, 1, 0)
  ))
  labels <- tail(opt[["series"]], 1)[[1]]
  expect_equal(labels[["renderItem"]], "rtemis.axis_labels.v1")
  expect_equal(labels[["data"]], list(list(0, "0"), list(24, "0")))
  expect_false(opt[["xAxis"]][["axisLabel"]][["show"]])
  area <- opt[["series"]][[1L]][["markArea"]][["data"]][[1L]]
  expect_equal(c(area[[1L]][["xAxis"]], area[[2L]][["xAxis"]]), c(2, 8))
  expect_error(draw_xt(1:3, 1:3, zt = 1:2))
  expect_error(
    built_option(draw_xt(1:3, 1:3, shade_bin = c(0, 2, 0))),
    "0 or 1"
  )
  expect_error(draw_xt(
    1:3,
    1:3,
    shade_bin = c(0, 1, 0),
    shade_interval = list(c(1, 2))
  ))
})

test_that("time annotations use the same data-binding and functional path", {
  skip_if_no_node()
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
  compiled <- built_option(compile(cfg, data), simplify = FALSE)
  direct <- raw_option(draw_xt(
    data[["time"]],
    list(value = data[["value"]]),
    zt = data[["phase"]],
    shade_bin = data[["night"]],
    show_zt_every = 2L
  ))
  expect_equal(compiled[["series"]], direct[["series"]])
  expect_identical(
    compile(
      read_chart_config(write_chart_config(cfg, tempfile(fileext = ".json"))),
      data
    ),
    compile(cfg, data)
  )
  expect_false(compiled[["xAxis"]][["axisLabel"]][["show"]])
})

test_that("time series validate names, time-vector counts and secondary inputs", {
  expect_error(
    timeseries_table(1:2, list()),
    "one time vector",
    class = "rtemis_length_error"
  )
  expect_error(
    timeseries_table(list(1:2), list(A = 1:2, B = 3:4)),
    "one time vector"
  )
  expect_error(
    timeseries_table(1:2, list(A = 1:2, A = 3:4)),
    "distinct nonempty"
  )
  expect_error(draw_xt(1:2, 1:2, x2 = 1:2), "Supply y2")
  expect_error(draw_xt(list(1:2), list(A = 1:2), zt = 1:2), "shared x vector")
  expect_error(
    compile(setup_TimeSeriesConfig(x = "Time"), BOD),
    "Bind at least one"
  )
  skip_if_no_node()
  expect_error(
    built_option(draw_ts(
      rep(1e308, 3),
      time = 1:3,
      roll_fn = "sum",
      window = 2L
    )),
    "rolling summaries finite"
  )
})

test_that("time-series bindings preserve secondary axes and explicit intervals", {
  data <- data.frame(
    time = 1:3,
    value = 2:4,
    secondary = 20:22,
    phase = c(0, 6, 12)
  )
  config <- setup_TimeSeriesConfig(
    x = "time",
    y = "value",
    y2 = "secondary",
    roll_fn = "none"
  )
  invalid <- setup_TimeSeriesConfig(
    x = "time",
    y = "value",
    x2 = "time",
    y2 = "secondary",
    zt = "phase"
  )
  expect_error(compile(invalid, data), "shared x binding")
  skip_if_no_node()
  option <- built_option(compile(config, data), simplify = FALSE)
  expect_identical(option[["series"]][[2L]][["name"]], "secondary")
  expect_equal(option[["series"]][[2L]][["yAxisIndex"]], 1L)
  shaded <- raw_option(draw_xt(1:3, 2:4, shade_interval = list(c(1, 2))))
  expect_true(length(shaded[["series"]]) > 1L)
  expect_equal(
    tail(shaded[["series"]], 1L)[[1L]][["markArea"]][["data"]][[1L]][[1L]][[
      "xAxis"
    ]],
    1
  )
})
