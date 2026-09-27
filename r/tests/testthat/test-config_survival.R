test_that("survival configuration validates and round trips portable settings", {
  cfg <- setup_SurvivalConfig(
    time = "day",
    group = "arm",
    landmarks = c(1, 3),
    show_median = TRUE,
    lower = "lo",
    upper = "hi"
  )
  expect_s7_class(cfg, SurvivalConfig)
  expect_identical(cfg@origin[["time"]], "user")
  expect_identical(resolve(cfg), cfg)
  expect_false("n_risk" %in% names(to_list(cfg)))
  for (args in list(
    list(lower = "lo"),
    list(upper = "hi"),
    list(digits = 1.5),
    list(ci_opacity = 2),
    list(landmarks = Inf),
    list(censor_size = 0),
    list(line_width = Inf),
    list(group = ""),
    list(show_ci = NA)
  )) {
    expect_error(do.call(setup_SurvivalConfig, args))
  }
  d <- data.frame(
    day = c(0, 1, 2),
    survival = c(1, .8, .4),
    arm = "A",
    lo = c(1, .6, .2),
    hi = c(1, .9, .7)
  )
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_identical(to_list(compile(back, d)), to_list(compile(cfg, d)))
    expect_identical(back@origin, cfg@origin)
    unlink(path)
  }
  schema <- chart_schema(
    SurvivalConfig,
    id = "https://example.org/survival.json",
    title = "Survival",
    description = "Survival records."
  )
  expect_identical(schema[["properties"]][["type"]][["const"]], "survival")
  expect_no_error(rtemis.core::assert_config_contract(
    schema,
    "survival",
    structural = "type"
  ))
})
