test_that("calibration configuration validates and round-trips its portable contract", {
  cfg <- setup_CalibrationConfig(
    observed = "event",
    probability = "score",
    n_bins = 3,
    rug = FALSE,
    palette = "#123456"
  )
  expect_s7_class(cfg, CalibrationConfig)
  expect_identical(cfg@type, "calibration")
  expect_identical(cfg@n_bins, 3L)
  expect_identical(cfg@origin[["observed"]], "user")
  expect_identical(cfg@origin[["bin_method"]], "default")
  expect_identical(resolve(cfg), cfg)
  expect_false("group" %in% names(to_list(cfg)))
  for (args in list(
    list(n_bins = 0),
    list(n_bins = 1.5),
    list(n_bins = NA),
    list(bin_method = "equal"),
    list(mode = "smooth"),
    list(rug_size = Inf),
    list(point_size = 0),
    list(line_width = NaN),
    list(rug_opacity = 2),
    list(digits = 9),
    list(na_rm = NA),
    list(observed = ""),
    list(group = character())
  )) {
    expect_error(do.call(setup_CalibrationConfig, args))
  }
  data <- data.frame(event = c(0, 1, 0, 1), score = c(0, .6, .3, 1))
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
    expect_identical(back@origin, cfg@origin)
    unlink(path)
  }
  schema <- chart_schema(
    CalibrationConfig,
    id = "https://example.org/calibration.json",
    title = "Calibration",
    description = "Calibration observations."
  )
  expect_identical(schema[["properties"]][["type"]][["const"]], "calibration")
  expect_equal(schema[["properties"]][["n_bins"]][["minimum"]], 1)
  expect_setequal(
    as.character(schema[["properties"]][["bin_method"]][["enum"]]),
    c("quantile", "equidistant")
  )
  expect_false(any(vapply(
    schema[["properties"]],
    function(p) "default" %in% names(p),
    logical(1)
  )))
  expect_identical(as.character(schema[["required"]]), "type")
  expect_no_error(rtemis.core::assert_config_contract(
    schema,
    "calibration",
    structural = "type"
  ))
  expect_identical(draw(cfg, data = data)[["x"]][["aspect"]][["ratio"]], 1)
  expect_null(draw(
    setup_CalibrationConfig(square = FALSE),
    data = data.frame(observed = 1, probability = .8)
  )[["x"]][["aspect"]])
})
