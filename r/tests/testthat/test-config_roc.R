test_that("ROC configs validate settings and preserve portable bindings", {
  cfg <- setup_ROCConfig(
    fpr = "fp",
    tpr = "tp",
    digits = 2,
    legend_position = "top-right"
  )
  expect_s7_class(cfg, ROCConfig)
  expect_identical(cfg@type, "roc")
  expect_identical(cfg@digits, 2L)
  expect_identical(cfg@origin[["fpr"]], "user")
  expect_identical(cfg@origin[["legend_position"]], "user")
  expect_identical(setup_ROCConfig()@legend_position, "top")
  expect_false("auc" %in% names(to_list(cfg)))
  expect_identical(resolve(cfg), cfg)
  for (args in list(
    list(variant = "folds"),
    list(digits = 1.5),
    list(digits = -1),
    list(digits = 9),
    list(line_width = 0),
    list(line_width = Inf),
    list(fold_opacity = 2),
    list(square = NA),
    list(legend_position = "outside"),
    list(legend_position = NA_character_),
    list(palette = character()),
    list(fpr = "")
  )) {
    expect_error(do.call(setup_ROCConfig, args))
  }
  data <- data.frame(fp = c(0, 0, 1), tp = c(0, 1, 1))
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
  }
  schema <- chart_schema(
    ROCConfig,
    id = "https://example.org/roc.json",
    title = "ROC",
    description = "ROC records."
  )
  p <- schema[["properties"]]
  expect_identical(p[["type"]][["const"]], "roc")
  expect_identical(
    as.character(p[["variant"]][["enum"]]),
    c("aggregate", "per_resample")
  )
  expect_equal(p[["digits"]][["maximum"]], 8)
  expect_equal(p[["fold_opacity"]][["minimum"]], 0)
  expect_identical(
    as.character(p[["legend_position"]][["enum"]]),
    LEGEND_POSITIONS
  )
  expect_false(any(vapply(p, function(x) "default" %in% names(x), logical(1))))
  expect_identical(draw(cfg, data = data)[["x"]][["aspect"]][["ratio"]], 1)
  expect_identical(
    draw(cfg, data = data)[["x"]][["legendPosition"]],
    "top-right"
  )
  expect_null(draw(
    setup_ROCConfig(square = FALSE),
    data = data.frame(fpr = c(0, 1), tpr = c(0, 1))
  )[["x"]][["aspect"]])
})
