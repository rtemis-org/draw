test_that("one-column grouping preserves the original column without coercion", {
  for (values in list(
    c("second", NA, "first"),
    factor(c("second", NA, "first"), levels = c("unused", "first", "second")),
    ordered(c("second", NA, "first"), levels = c("first", "second")),
    c(2L, NA_integer_, 1L),
    c(TRUE, NA, FALSE)
  )) {
    frame <- data.frame(Species = values)
    original <- serialize(frame, NULL)
    expect_identical(group_values(frame, 3L), values)
    expect_identical(group_values(values, 3L), values)
    expect_identical(serialize(frame, NULL), original)
  }
  expect_null(group_values(NULL, 3L))
  expect_identical(
    group_values(data.frame(Species = character()), 0L),
    character()
  )
  expect_error(group_values(data.frame()), "exactly one column")
  expect_error(group_values(data.frame(a = 1:3, b = 4:6)), "exactly one column")
  expect_error(group_values(data.frame(a = I(list(1, 2, 3)))), "atomic vector")
  expect_error(group_values(data.frame(a = I(matrix(1:6, 3)))), "atomic vector")
  expect_error(group_values(list(a = 1:3)), "atomic vector")
  expect_error(group_values(matrix(1:3)), "atomic vector")
  expect_error(group_values(data.frame(a = 1:3), c(3L, 4L)), "same length")
})

test_that("grouped plots accept vector and one-column data-frame inputs equally", {
  values <- c(1, 2, 3, 2, 4, 6)
  groups <- ordered(
    rep(c("second", "first"), each = 3),
    levels = c("first", "second")
  )
  calls <- list(
    function(group) draw_boxplot(data.frame(Measure = values), group = group),
    function(group) {
      draw_boxplot(
        list(A = values, B = values + 1),
        group = group,
        boxpoints = "all"
      )
    },
    function(group) draw_line(seq_along(values), values, group = group),
    function(group) {
      draw_scatter(seq_along(values), values, group = group, fit = "glm")
    },
    function(group) draw_density(values, group = group),
    function(group) {
      draw_density(list(A = values, B = values + 1), group = group)
    },
    function(group) draw_histogram(values, group = group),
    function(group) draw_fit(seq_along(values), values, group = group)
  )
  for (call in calls) {
    vector <- call(groups)
    frame <- call(data.frame(Species = groups))
    expect_identical(frame[["x"]], vector[["x"]])
    expect_identical(
      widget_payload_json(frame),
      widget_payload_json(vector)
    )
    expect_error(call(data.frame(a = groups, b = groups)), "exactly one column")
    expect_error(call(data.frame(a = groups[-1L])), "same length")
  }
})

test_that("the penguins example gains neither a legend nor a heading", {
  for (horizontal in c(FALSE, TRUE)) {
    vector <- suppressMessages(draw_boxplot(
      penguins["bill_len"],
      labels = "Bill length (mm)",
      group = penguins[["species"]],
      horizontal = horizontal
    ))
    frame <- suppressMessages(draw_boxplot(
      penguins["bill_len"],
      labels = "Bill length (mm)",
      group = penguins["species"],
      horizontal = horizontal
    ))
    expect_identical(frame[["x"]], vector[["x"]])
    expect_null(frame[["x"]][["option"]][["legend"]])
    expect_null(frame[["x"]][["option"]][["title"]])
  }
  # Missing groups and unused levels produce no category.
  values <- data.frame(score = c(1, 2, 3, 4))
  groups <- factor(c("b", NA, "a", "b"), levels = c("unused", "a", "b"))
  a <- boxplot_option(values, group = groups, boxpoints = "all")
  b <- boxplot_option(
    values,
    group = data.frame(Group = groups),
    boxpoints = "all"
  )
  expect_identical(to_list(a), to_list(b))
  expect_identical(to_list(b)[["xAxis"]][["data"]], c("a", "b"))
})

test_that("data-frame grouping matches the portable column binding", {
  cfg <- setup_BoxplotConfig(x = "Sepal.Length", group = "Species")
  expected <- draw_boxplot(iris["Sepal.Length"], group = iris["Species"])
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  for (complete in c(FALSE, TRUE)) {
    write_chart_config(cfg, path, complete = complete)
    restored <- read_chart_config(path)
    expect_identical(restored@group, "Species")
    expect_identical(draw(restored, data = iris)[["x"]], expected[["x"]])
  }
})

test_that("group_levels orders factors by level and other vectors by appearance", {
  expect_identical(
    group_levels(factor(c("e", "b", NA, "a", "b"))),
    c("a", "b", "e")
  )
  expect_identical(
    group_levels(factor(c("a", "c"), levels = c("c", "unused", "a"))),
    c("c", "a")
  )
  expect_identical(
    group_levels(ordered(c("low", "high"), levels = c("high", "low"))),
    c("high", "low")
  )
  expect_identical(group_levels(c("e", NA, "b", "e")), c("e", "b"))
  expect_identical(group_levels(c(2, 1, 2, NA)), c("2", "1"))
  expect_identical(group_levels(c(TRUE, NA, FALSE)), c("TRUE", "FALSE"))
  expect_identical(group_levels(character()), character())
  # Levels without drawn rows drop once `group` is filtered to those rows.
  expect_identical(group_levels(factor(c("a", "b"))[2L]), "b")
})

# Names of the series in a widget's option, in drawing order.
series_names <- function(widget) {
  vapply(
    widget[["x"]][["option"]][["series"]],
    function(s) s[["name"]] %||% "",
    character(1L)
  )
}

test_that("grouped charts draw series in factor-level order", {
  group <- factor(rep(c("e", "b", "c", "a"), each = 4L))
  x <- rep(c(1, 2, 3, 4), 4L)
  y <- x + rep(c(0, 1, 2, 3), each = 4L)
  expected <- c("a", "b", "c", "e")
  charts <- list(
    draw_scatter(x, y, group = group),
    draw_scatter(x, y, group = data.frame(group = group)),
    draw_line(x, y, group = group),
    draw_density(y, group = group),
    draw_histogram(y, group = group),
    draw_fit(x, y, group = group, fit = NULL, se = FALSE),
    draw_ts(y, as.Date("2026-01-01") + seq_along(y), window = 2L, group = group)
  )
  for (chart in charts) {
    drawn <- series_names(chart)
    expect_identical(unique(drawn[drawn %in% expected]), expected)
  }
  # The first level takes the first palette color.
  opt <- draw_scatter(x, y, group = group, palette = c("#111111", "#222222"))
  series <- opt[["x"]][["option"]][["series"]]
  expect_identical(series[[1L]][["name"]], "a")
  expect_identical(series[[1L]][["itemStyle"]][["color"]], "#111111")
  expect_length(series[[1L]][["data"]], 4L)
  expect_identical(series[[1L]][["data"]][[1L]], c(1, 4))
})

test_that("character groups keep first-appearance order", {
  group <- rep(c("e", "b", "a"), each = 3L)
  x <- rep(1:3, 3L)
  y <- seq_along(x)
  expect_identical(
    series_names(draw_scatter(x, y, group = group)),
    c("e", "b", "a")
  )
  expect_identical(
    series_names(draw_line(x, y, group = group)),
    c("e", "b", "a")
  )
})

test_that("levels without drawn rows produce no series", {
  group <- factor(c("b", "b", "a", "a", NA), levels = c("unused", "a", "b"))
  x <- c(1, 2, 1, 2, 3)
  y <- c(1, 2, 3, 4, 5)
  expect_identical(series_names(draw_scatter(x, y, group = group)), c("a", "b"))
  expect_identical(series_names(draw_line(x, y, group = group)), c("a", "b"))
  # Every row of level "a" has a missing coordinate.
  expect_identical(
    series_names(draw_scatter(
      c(1, 2, NA, NA),
      c(1, 2, 3, 4),
      group = group[1:4]
    )),
    "b"
  )
})

test_that("portable column bindings keep factor-level order", {
  data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(2, 1, 4, 3),
    group = factor(c("b", "b", "a", "a"))
  )
  chart <- draw(
    setup_ScatterConfig(x = "x", y = "y", group = "group"),
    data = data
  )
  expect_identical(series_names(chart), c("a", "b"))
})

test_that("boxplots order groups by factor level", {
  values <- data.frame(score = c(1, 2, 3, 4, 5, 6))
  group <- factor(rep(c("b", "a", "c"), each = 2L))
  single <- to_list(boxplot_option(values, group = group))
  expect_identical(single[["xAxis"]][["data"]], c("a", "b", "c"))
  multi <- draw_boxplot(list(A = 1:6, B = 2:7), group = group)
  expect_identical(
    unique(setdiff(series_names(multi), "")),
    c("a", "b", "c")
  )
})

test_that("3D scatter, Gantt, significance, calibration, and survival follow factor levels", {
  group <- factor(rep(c("b", "a"), each = 8L))
  x <- rep(1:8, 2L)
  y <- c(sin(1:8), cos(1:8))
  z <- x * 0.5 + y
  expect_identical(
    series_names(draw_scatter3d(x, y, z, group = group)),
    c("a", "b")
  )
  tasks <- data.frame(
    label = c("one", "two", "three"),
    start = c(0, 1, 2),
    end = c(1, 2, 3),
    status = factor(c("b", "a", "b"))
  )
  gantt <- series_names(draw_gantt(tasks, group = "status"))
  expect_identical(gantt[gantt != ""], c("a", "b"))
  expect_identical(
    series_names(draw_gantt(
      transform(tasks, status = as.character(status)),
      group = "status"
    ))[1:2],
    c("b", "a")
  )
  volcano <- draw_volcano(
    c(-2, -1, 1, 2),
    c(0.01, 0.2, 0.2, 0.01),
    xnames = paste0("v", 1:4),
    group = factor(c("b", "a", "a", NA)),
    annotate_n = 0L
  )
  expect_identical(series_names(volcano)[1:2], c("a", "b"))
  observed <- rep(c(0L, 1L, 0L, 1L), 5L)
  records <- data.frame(
    observed = observed,
    probability = seq(0.1, 0.9, length.out = 20L),
    group = factor(rep(c("b", "a"), each = 10L))
  )
  calibration <- calibration_data(
    setup_CalibrationConfig(
      observed = "observed",
      probability = "probability",
      group = "group"
    ),
    records
  )
  expect_identical(
    vapply(calibration, function(g) g[["name"]], character(1L)),
    c("a", "b")
  )
  curves <- data.frame(
    time = c(0, 1, 2, 0, 1, 2),
    survival = c(1, 0.8, 0.6, 1, 0.9, 0.7),
    group = factor(rep(c("b", "a"), each = 3L))
  )
  survival <- draw_survival(curves)
  expect_identical(
    unlist(survival[["x"]][["option"]][["legend"]][["data"]]),
    c("a", "b")
  )
})

test_that("true_pred_data retains the levels of a factor group", {
  group <- factor(c("b", "a", "b", "a"), levels = c("b", "a", "unused"))
  data <- true_pred_data(1:4, c(1, 2, 3, 4), group = group)
  expect_identical(data[["sample"]], group)
})
