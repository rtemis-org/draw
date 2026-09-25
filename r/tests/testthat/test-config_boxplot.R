test_that("boxplot settings and bindings survive schema and config conversion", {
  cfg <- setup_BoxplotConfig(
    x = "score",
    observation = "fold",
    quartiles = "hinges",
    whisker = 0,
    boxpoints = "all",
    point_spread = 0
  )
  expect_s7_class(cfg, BoxplotConfig)
  expect_identical(cfg@origin[["boxpoints"]], "user")
  expect_false("group" %in% names(to_list(cfg)))
  for (args in list(
    list(quartiles = "bad"),
    list(whisker = -1),
    list(whisker = Inf),
    list(boxpoints = TRUE),
    list(point_spread = 2),
    list(point_size = 0),
    list(point_alpha = NA),
    list(observation = "")
  )) {
    expect_error(do.call(setup_BoxplotConfig, args))
  }
  data <- data.frame(score = c(1, 2, 3, 7), fold = letters[1:4])
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_equal(back@whisker, 0)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
  }
  schema <- chart_schema(
    BoxplotConfig,
    id = "https://example.org/box.json",
    title = "Boxplot",
    description = "Boxplot."
  )[["properties"]]
  expect_equal(
    as.character(schema[["quartiles"]][["enum"]]),
    c("linear", "hinges")
  )
  expect_equal(schema[["point_spread"]][["maximum"]], 1)
  expect_equal(schema[["point_size"]][["exclusiveMinimum"]], 0)
  expect_identical(
    compile(cfg, data),
    boxplot_option(
      list(score = data[["score"]]),
      observation = data[["fold"]],
      quartiles = "hinges",
      whisker = 0,
      boxpoints = "all",
      point_spread = 0
    )
  )
})
