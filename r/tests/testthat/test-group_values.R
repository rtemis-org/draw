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
      htmlwidgets:::toJSON(frame[["x"]]),
      htmlwidgets:::toJSON(vector[["x"]])
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
  # Missing groups and unused levels retain the existing first-appearance rule.
  values <- data.frame(score = c(1, 2, 3, 4))
  groups <- factor(c("b", NA, "a", "b"), levels = c("unused", "a", "b"))
  a <- boxplot_option(values, group = groups, boxpoints = "all")
  b <- boxplot_option(
    values,
    group = data.frame(Group = groups),
    boxpoints = "all"
  )
  expect_identical(to_list(a), to_list(b))
  expect_identical(to_list(b)[["xAxis"]][["data"]], c("b", "a"))
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
