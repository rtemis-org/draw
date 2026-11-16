test_that("draw_line line_style: NULL produces no lineStyle in series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = NULL)
  opt <- node_option(w)
  ls <- opt$series[[1]]$lineStyle
  expect_null(ls)
})

test_that("draw_line line_style: single value applied to single series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = "dashed")
  opt <- node_option(w)
  expect_equal(opt$series[[1]]$lineStyle$type, "dashed")
})

test_that("draw_line line_style: per-series values applied correctly", {
  y <- list(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
  w <- draw_line(1:3, y, line_style = c("solid", "dashed", "dotted"))
  opt <- node_option(w)
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
  expect_equal(opt$series[[3]]$lineStyle$type, "dotted")
})

test_that("draw_line line_style: recycled when fewer values than series", {
  y <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  w <- draw_line(1:3, y, line_style = c("solid", "dashed"))
  opt <- node_option(w)
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
  opt <- node_option(w)
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
  opt <- node_option(draw_line(x, y, group = g))
  expect_equal(vapply(opt$series, function(s) s$name, ""), c("b", "a"))
  expect_equal(opt$series[[2]]$data, list(c(1, 3), c(2, 4)))
})

test_that("draw_line group: category x pairs each point with its axis index", {
  x <- c("Q1", "Q2", "Q1", "Q2")
  y <- c(1, 2, 3, 4)
  g <- c("A", "A", "B", "B")
  opt <- node_option(draw_line(x, y, group = g))
  expect_equal(opt$xAxis$type, "category")
  expect_equal(opt$xAxis$data, c("Q1", "Q2"))
  expect_equal(opt$series[[2]]$data, list(c(0, 3), c(1, 4)))
})

test_that("draw_line group: a factor x is matched by index, not by its label", {
  x <- factor(c("b", "a", "b", "a"), levels = c("b", "a"))
  y <- c(1, 2, 3, 4)
  g <- c("A", "A", "B", "B")
  opt <- node_option(draw_line(x, y, group = g))
  expect_equal(opt$xAxis$type, "category")
  expect_equal(opt$series[[1]]$data, list(c(0, 1), c(1, 2)))
  expect_equal(opt$series[[2]]$data, list(c(0, 3), c(1, 4)))
})

test_that("draw_line: ungrouped category axis data is unchanged", {
  opt <- node_option(draw_line(c("a", "b", "c"), c(1, 2, 3)))
  expect_equal(opt$xAxis$data, c("a", "b", "c"))
  expect_equal(opt$series[[1]]$data, c(1, 2, 3))
})

test_that("draw_line group: line_style recycles across groups", {
  x <- c(1, 2, 1, 2)
  y <- 1:4
  g <- c("A", "A", "B", "B")
  opt <- node_option(draw_line(
    x,
    y,
    group = g,
    line_style = c("solid", "dashed")
  ))
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
})

test_that("draw_line group: splits each series of a list y, and rejects a length mismatch and blocks", {
  opt <- node_option(draw_line(
    c(1, 2, 1, 2),
    list(a = 1:4, b = 5:8),
    group = c("A", "A", "B", "B")
  ))
  expect_equal(
    vapply(opt$series, function(s) s$name, ""),
    c("a / A", "b / A", "a / B", "b / B")
  )
  expect_error(
    draw_line(1:4, 1:4, group = c("A", "B")),
    "same length",
    class = "rtemis_length_error"
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
  opt <- node_option(draw_line(d, c(1, 2, 3)))
  expect_equal(opt$xAxis$type, "time")
  expect_null(opt$xAxis$data)
  expect_true(opt$useUTC)
  ms <- as.numeric(d) * 86400000
  expect_equal(
    opt$series[[1]]$data,
    list(c(ms[1], 1), c(ms[2], 2), c(ms[3], 3))
  )
  expect_equal(c(opt$xAxis$min, opt$xAxis$max), calc_limits(ms))
})

test_that("draw_line: a POSIXct x keeps its wall-clock reading in any timezone", {
  # 10:00 in Los Angeles is sent as the instant whose UTC reading is 10:00,
  # so the chart labels it 10:00 wherever it is viewed.
  x <- as.POSIXct("2024-01-01 10:00:00", tz = "America/Los_Angeles")
  opt <- node_option(draw_line(x + c(0, 3600), c(1, 2)))
  expect_equal(opt$xAxis$type, "time")
  ten_utc <- as.numeric(as.POSIXct("2024-01-01 10:00:00", tz = "UTC")) * 1000
  expect_equal(opt$series[[1]]$data[[1]][1], ten_utc)
  expect_equal(opt$series[[1]]$data[[2]][1], ten_utc + 3600000)
})

test_that("draw_line: grouped POSIXct x pairs each point with its time", {
  x <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * c(0:1, 0:1)
  g <- c("A", "A", "B", "B")
  opt <- node_option(draw_line(x, c(1, 2, 3, 4), group = g))
  ms <- as.numeric(x) * 1000
  expect_equal(opt$xAxis$type, "time")
  expect_equal(opt$series[[1]]$data, list(c(ms[1], 1), c(ms[2], 2)))
  expect_equal(opt$series[[2]]$data, list(c(ms[3], 3), c(ms[4], 4)))
})

test_that("draw_line: xlim on a time axis accepts dates or milliseconds", {
  d <- as.Date("2025-09-07") + 0:2
  lim <- as.Date(c("2025-09-01", "2025-09-30"))
  opt <- node_option(draw_line(d, 1:3, xlim = lim))
  expect_equal(c(opt$xAxis$min, opt$xAxis$max), as.numeric(lim) * 86400000)
  opt2 <- node_option(draw_line(d, 1:3, xlim = as.numeric(lim) * 86400000))
  expect_equal(opt2$xAxis$min, opt$xAxis$min)
})

test_that("draw_line: blocks on a time axis are placed in milliseconds", {
  d <- as.Date("2025-09-07") + 0:3
  opt <- node_option(draw_line(
    d,
    1:4,
    blocks = c("a", "a", "b", "b"),
    block_color = c(a = "red", b = "blue")
  ))
  bands <- opt$series[[1]]$markArea$data
  expect_equal(bands[[1]][[1]]$xAxis, as.numeric(d[1]) * 86400000)
})

test_that("draw_line: equal_axes rejects a time x", {
  d <- as.Date("2025-09-07") + 0:2
  expect_error(
    node_option(draw_line(d, 1:3, equal_axes = TRUE)),
    "date or time"
  )
})

test_that("draw_line: numeric and character x are unchanged by the time axis", {
  expect_equal(node_option(draw_line(1:3, 1:3))$xAxis$type, "value")
  expect_null(node_option(draw_line(1:3, 1:3))$useUTC)
  expect_equal(node_option(draw_line(c("a", "b"), 1:2))$xAxis$type, "category")
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
  opt <- node_option(draw_line(1:5, 1:5, zoom = TRUE))
  slider <- Filter(function(z) identical(z$type, "slider"), opt$dataZoom)[[1]]
  # Default margins: the slider sits above the 36 px margin plus its 7 px
  # brush handle, and the grid clears slider, handle, and an 8 px gap.
  expect_equal(slider$bottom, 36 + 7)
  expect_equal(slider$height, 30)
  expect_equal(opt$grid$bottom, 36 + 7 + 30 + 8)
})

test_that("draw_line zoom = TRUE builds on an explicit or absent bottom margin", {
  opt <- node_option(draw_line(1:5, 1:5, zoom = TRUE, margins = c(bottom = 50)))
  expect_equal(opt$grid$bottom, 50 + 45)
  # No margins at all: every side takes the 36 px default.
  opt <- node_option(draw_line(1:5, 1:5, zoom = TRUE, margins = NULL))
  expect_equal(opt$grid$bottom, 36 + 45)
})

test_that("draw_line takes margins in pixels and zoom as TRUE or FALSE", {
  expect_error(
    draw_line(1:5, 1:5, zoom = TRUE, margins = list(bottom = "20%")),
    "pixels"
  )
  expect_error(
    draw_line(1:5, 1:5, zoom = DataZoom(type = "slider")),
    "TRUE or FALSE"
  )
})

# %% tooltip ----

test_that("draw_line tooltip triggers on the axis", {
  # The value formatter is a function, which JSON drops; @rtemis/draw's
  # tests cover it (formatNumber).
  opt <- node_option(draw_line(1:3, c(1.23456, 2, 3)))
  expect_equal(opt$tooltip$trigger, "axis")
})

test_that("categorical line baselines depend on the visible value range; area axes hold zero", {
  years <- c("2007", "2008", "2009")
  for (area in c(FALSE, TRUE)) {
    for (values in list(c(20, 30, 40), c(-40, -30, -20), c(-20, 0, 20))) {
      opt <- node_option(draw_line(years, values, area = area))
      expect_identical(
        opt[["xAxis"]][["axisLine"]],
        list(
          show = area || (min(values) <= 0 && max(values) >= 0),
          onZero = TRUE
        )
      )
      expect_identical(opt[["xAxis"]][["data"]], years)
      expect_equal(opt[["series"]][[1]][["data"]], values)
    }
    # Explicit limits determine whether zero is visible, not the raw data.
    clipped <- node_option(draw_line(
      years,
      c(-20, 30, 40),
      ylim = c(25, 45),
      area = area
    ))
    expanded <- node_option(draw_line(
      years,
      c(20, 30, 40),
      ylim = c(0, 45),
      area = area
    ))
    expect_false(clipped[["xAxis"]][["axisLine"]][["show"]])
    expect_true(expanded[["xAxis"]][["axisLine"]][["show"]])
    config <- setup_LineConfig(x = "year", y = "count", area = area)
    compiled <- node_option(compile(
      config,
      data.frame(year = years, count = c(20, 30, 40))
    ))
    expect_identical(
      compiled[["xAxis"]][["axisLine"]],
      list(show = area, onZero = TRUE)
    )
  }
})

test_that("a filled area's derived y limits hold zero, padded away from it", {
  data <- data.frame(x = 1:3, up = c(20, 30, 40), down = c(-40, -30, -20))
  ylim <- function(y, ...) {
    resolve(setup_LineConfig(x = "x", y = y, ...), data = data)@ylim
  }
  expect_equal(ylim("up", area = TRUE), c(0, 41.6))
  expect_equal(ylim("down", area = TRUE), c(-41.6, 0))
  expect_equal(ylim("up"), calc_limits(c(20, 30, 40)))
  expect_equal(ylim("up", area = TRUE, ylim = c(10, 50)), c(10, 50))
  expect_null(ylim(c("up", "down"), area = TRUE, stack = TRUE))
  expect_equal(calc_limits_from_zero(c(-20, 0, 20)), calc_limits(c(-20, 0, 20)))
})

test_that("native categorical line SVG draws emphasis only at zero", {
  skip_if_no_node()
  charts <- list()
  for (theme in list(theme_light(), theme_dark())) {
    for (area in c(FALSE, TRUE)) {
      for (values in list(c(20, 30, 40), c(-40, -30, -20), c(-20, 0, 20))) {
        option <- node_option(draw_line(
          c("2007", "2008", "2009"),
          values,
          area = area
        ))
        charts[[length(charts) + 1L]] <- list(
          option = option,
          theme = to_list(theme),
          horizontal = FALSE,
          zero = area || (min(values) <= 0 && max(values) >= 0)
        )
      }
    }
  }
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = charts,
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    digits = NA
  )
  output <- suppressWarnings(system2(
    "node",
    c(
      shQuote(test_path("fixtures", "axis_baseline.js")),
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})


# %% aggregate and one row per x ----

test_that("aggregate combines the rows of each x and group, and names the axis", {
  skip_if_no_node()
  config <- setup_LineConfig(
    x = "quarter",
    y = "gwh",
    group = "source",
    aggregate = "sum"
  )
  resolved <- resolve(config, data = power_generation)
  expect_identical(resolved@ylab, "Sum of gwh")
  opt <- node_option(compile(config, data = power_generation))
  expect_identical(opt$xAxis$data, c("Q1", "Q2", "Q3", "Q4"))
  solar <- power_generation[power_generation$source == "Solar", ]
  expect_equal(
    vapply(opt$series[[1]]$data, function(p) p[[2]], numeric(1)),
    as.vector(tapply(solar$gwh, solar$quarter, sum))
  )
  # The limits are those of the sums drawn.
  expect_equal(
    resolved@ylim,
    calc_limits(c(
      79,
      191,
      161,
      49,
      227.3,
      152.7,
      132.7,
      207.3,
      126.3,
      163.7,
      173.7,
      136.3
    ))
  )
})

test_that("a line holds one row per x, and draw_line() takes aggregate", {
  skip_if_no_node()
  expect_error(
    node_option(draw_line(c(1, 1, 2), c(3, 5, 4))),
    "Several rows hold x = 1"
  )
  opt <- node_option(draw_line(c(1, 1, 2), c(3, 5, 4), aggregate = "mean"))
  expect_equal(opt$series[[1]]$data, list(c(1, 4), c(2, 4)))
})

test_that("stack requires area, and a line has no point where a line below has none", {
  skip_if_no_node()
  expect_error(setup_LineConfig(stack = TRUE), "set area too")
  data <- data.frame(
    x = c(1, 2, 3, 1, 2, 3),
    v = c(5, NA, 1, 2, 4, 3),
    g = rep(c("a", "b"), each = 3L)
  )
  opt <- node_option(compile(
    setup_LineConfig(x = "x", y = "v", group = "g", stack = TRUE, area = TRUE),
    data = data
  ))
  expect_true(is.na(opt$series[[2]]$data[[2]]$value[[2]]))
  expect_identical(opt$yAxis$min, 0L)
})
