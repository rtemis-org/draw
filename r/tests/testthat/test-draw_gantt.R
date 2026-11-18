# test-draw_gantt.R
# Tests for draw_gantt(), whose option @rtemis/draw's Gantt builder builds.

tasks_df <- function() {
  data.frame(
    label = c("load", "clean", "train", "predict"),
    start = c(0, 12, 30, 95),
    end = c(12, 30, 95, 100),
    status = c("ok", "ok", "ok", "error"),
    stringsAsFactors = FALSE
  )
}

bar_values <- function(series) {
  lapply(series[["data"]], function(d) unlist(d[["value"]]))
}


# -- Request --------------------------------------------------------------------

test_that("draw_gantt sends a Gantt build request with the bound columns", {
  w <- draw_gantt(tasks_df(), group = "status", bar_radius = 4)
  build <- w[["x"]][["build"]]
  expect_identical(build[["type"]], "gantt")
  expect_identical(build[["config"]][["group"]], "status")
  expect_identical(build[["config"]][["bar_radius"]], 4)
  data <- jsonlite::fromJSON(build[["data"]])
  expect_setequal(
    names(data[["columns"]]),
    c("label", "start", "end", "status")
  )
  expect_identical(
    w[["x"]],
    draw(
      setup_GanttConfig(group = "status", bar_radius = 4),
      data = tasks_df()
    )[["x"]]
  )
})


test_that("the Gantt renderer settings are plain JSON parameters", {
  o <- raw_option(draw_gantt(
    tasks_df(),
    bar_height = 0.6,
    bar_radius = 4,
    border_color = "#E53935",
    border_width = 1.5
  ))
  series <- o[["series"]][[1L]]
  expect_identical(series[["renderItem"]], "rtemis.gantt.v1")
  expect_equal(
    series[["itemPayload"]],
    list(
      barHeight = 0.6,
      barRadius = 4,
      borderColor = "#E53935",
      borderWidth = 1.5
    )
  )
})


test_that("draw_gantt sizes an unset height from the task rows", {
  many <- data.frame(label = paste("task", 1:30), start = 0:29, end = 1:30)
  expect_identical(draw_gantt(many)[["height"]], 30 * 20 + 96)
  expect_identical(draw_gantt(tasks_df())[["height"]], 220)
  expect_identical(draw_gantt(many, height = 300)[["height"]], 300)
  expect_identical(
    draw(setup_GanttConfig(), data = many)[["height"]],
    30 * 20 + 96
  )
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


test_that("draw_gantt errors on unknown group, tooltip, and border columns", {
  expect_error(draw_gantt(tasks_df(), group = "nope"), "group")
  expect_error(draw_gantt(tasks_df(), tooltip = "nope"), "tooltip")
  expect_error(draw_gantt(tasks_df(), border = "nope"), "border")
})


# -- Built option ---------------------------------------------------------------

test_that("draw_gantt without group draws one series and no legend", {
  o <- raw_option(draw_gantt(tasks_df()))
  expect_length(o[["series"]], 1L)
  expect_identical(o[["series"]][[1L]][["type"]], "custom")
  expect_length(o[["series"]][[1L]][["data"]], 4L)
  expect_false(o[["legend"]][["show"]])
})


test_that("draw_gantt y-axis is an inverse category axis of unique labels", {
  o <- raw_option(draw_gantt(tasks_df()))
  expect_identical(o[["yAxis"]][["type"]], "category")
  expect_true(o[["yAxis"]][["inverse"]])
  expect_identical(
    unlist(o[["yAxis"]][["data"]]),
    c("load", "clean", "train", "predict")
  )
})


test_that("draw_gantt x-axis type follows axis_type", {
  expect_identical(
    raw_option(draw_gantt(tasks_df()))[["xAxis"]][["type"]],
    "value"
  )
  expect_identical(
    raw_option(draw_gantt(tasks_df(), axis_type = "time"))[["xAxis"]][["type"]],
    "time"
  )
})


test_that("draw_gantt encodes each bar as [row, start, end]", {
  o <- raw_option(draw_gantt(tasks_df()))
  expect_equal(bar_values(o[["series"]][[1L]])[[2L]], c(1, 12, 30))
})


test_that("draw_gantt repeated labels collapse onto one row", {
  df <- data.frame(
    label = c("worker", "worker", "worker"),
    start = c(0, 10, 25),
    end = c(10, 25, 40)
  )
  o <- raw_option(draw_gantt(df))
  expect_identical(unlist(o[["yAxis"]][["data"]]), "worker")
  rows <- vapply(bar_values(o[["series"]][[1L]]), `[[`, numeric(1L), 1L)
  expect_equal(rows, c(0, 0, 0))
})


test_that("draw_gantt with group draws one colored series per level with a legend", {
  o <- raw_option(draw_gantt(
    tasks_df(),
    group = "status",
    palette = c("#111111", "#222222")
  ))
  series <- o[["series"]]
  expect_identical(vapply(series, `[[`, "", "name"), c("ok", "error"))
  expect_identical(
    vapply(series, function(s) s[["itemStyle"]][["color"]], ""),
    c("#111111", "#222222")
  )
  expect_true(o[["legend"]][["show"]])
})


test_that("draw_gantt time axis places POSIXct bars at epoch milliseconds", {
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  df <- data.frame(label = "a")
  df$start <- t0
  df$end <- t0 + 5
  o <- raw_option(draw_gantt(df, axis_type = "time"))
  v <- bar_values(o[["series"]][[1L]])[[1L]]
  expect_equal(v[2:3], as.numeric(c(t0, t0 + 5)) * 1000)
  expect_true(o[["useUTC"]])
})


test_that("draw_gantt guides toggles the x-axis axisPointer", {
  on_ap <- raw_option(draw_gantt(tasks_df(), guides = TRUE))[["xAxis"]][[
    "axisPointer"
  ]]
  expect_true(on_ap[["show"]])
  expect_identical(on_ap[["type"]], "line")
  expect_null(raw_option(draw_gantt(tasks_df(), guides = FALSE))[["xAxis"]][[
    "axisPointer"
  ]])
})


test_that("draw_gantt zoom adds inside dataZoom and a toolbox with reset", {
  o <- raw_option(draw_gantt(tasks_df(), zoom = TRUE))
  dz <- o[["dataZoom"]]
  expect_length(dz, 2L)
  expect_true(all(vapply(dz, `[[`, "", "type") == "inside"))
  expect_false(is.null(o[["toolbox"]][["feature"]][["dataZoom"]]))
  expect_false(is.null(o[["toolbox"]][["feature"]][["restore"]]))
  o0 <- raw_option(draw_gantt(tasks_df(), zoom = FALSE))
  expect_null(o0[["dataZoom"]])
  expect_null(o0[["toolbox"]])
})


test_that("draw_gantt border flags bars with a fourth data value", {
  df <- tasks_df()
  df$bad <- c(FALSE, FALSE, FALSE, TRUE)
  o <- raw_option(draw_gantt(df, border = "bad"))
  flags <- vapply(bar_values(o[["series"]][[1L]]), `[[`, numeric(1L), 4L)
  expect_equal(flags, c(0, 0, 0, 1))
})


test_that("draw_gantt tooltip shows the tooltip column, else the label", {
  df <- tasks_df()
  df$tip <- paste(df$label, "<ms>")
  o <- raw_option(draw_gantt(df, tooltip = "tip"))
  expect_identical(o[["tooltip"]][["formatter"]], "{b}")
  expect_identical(o[["series"]][[1L]][["data"]][[1L]][["name"]], "load <ms>")
  o <- raw_option(draw_gantt(df))
  expect_identical(o[["series"]][[1L]][["data"]][[1L]][["name"]], "load")
})


test_that("draw_gantt grid contains the axis name when there is one", {
  expect_identical(
    raw_option(draw_gantt(tasks_df(), xlab = "Elapsed (ms)"))[["grid"]][[
      "outerBoundsContain"
    ]],
    "all"
  )
  expect_identical(
    raw_option(draw_gantt(tasks_df()))[["grid"]][["outerBoundsContain"]],
    "axisLabel"
  )
})


test_that("Gantt layout preserves usable axes and state across narrow resizes", {
  skip_if_no_node()
  tasks <- data.frame(
    label = c(
      "train GLM Regression",
      "outer_fold 1/5",
      "train_alg GLM",
      "predict",
      "varimp GLM",
      "metrics"
    ),
    start = c(0, 10, 15, 40, 45, 50),
    end = c(100, 90, 40, 45, 50, 80),
    kind = c("train", "outer_fold", "train_alg", "predict", "varimp", "metrics")
  )
  # Exercise both numeric and absolute time, plus the ungrouped path.
  widgets <- list(draw_gantt(
    tasks,
    group = "kind",
    title = "Five-fold execution",
    xlab = "Elapsed (ms)"
  ))
  timed <- tasks
  timed[["start"]] <- as.POSIXct("2026-01-01", tz = "UTC") + tasks[["start"]]
  timed[["end"]] <- as.POSIXct("2026-01-01", tz = "UTC") + tasks[["end"]]
  widgets[[2L]] <- draw_gantt(
    timed,
    group = "kind",
    axis_type = "time",
    title = "Five-fold execution",
    xlab = "Time"
  )
  widgets[[3L]] <- draw_gantt(tasks, title = "Five-fold execution")
  payloads <- lapply(widgets, built_payload)
  # A stored initial selection must not replace later interactive changes.
  payloads[[1L]][["option"]][["legend"]][["selected"]] <- list(train = TRUE)
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  jsonlite::write_json(
    list(
      payloads = payloads,
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      ),
      binding = system.file(
        "htmlwidgets/rtemis-draw.js",
        package = "rtemis.draw"
      )
    ),
    input,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  out <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "gantt_geometry.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  expect_match(paste(out, collapse = "\n"), "Gantt bounds")
})
