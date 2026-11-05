test_that("precision-recall configs validate settings and preserve portable bindings", {
  cfg <- setup_PRConfig(
    recall = "r",
    precision = "p",
    digits = 2,
    legend_position = "top-right"
  )
  expect_s7_class(cfg, PRConfig)
  expect_identical(cfg@type, "pr")
  expect_identical(cfg@digits, 2L)
  expect_identical(cfg@origin[["recall"]], "user")
  expect_identical(cfg@origin[["legend_position"]], "user")
  expect_identical(setup_PRConfig()@legend_position, "top")
  expect_false("ap" %in% names(to_list(cfg)))
  expect_identical(resolve(cfg), cfg)
  for (args in list(
    list(variant = "folds"),
    list(digits = 1.5),
    list(digits = 9),
    list(line_width = 0),
    list(line_width = Inf),
    list(fold_opacity = 2),
    list(square = NA),
    list(baseline = NA),
    list(palette = character()),
    list(recall = "")
  )) {
    expect_error(do.call(setup_PRConfig, args))
  }
  data <- data.frame(r = c(0, 1), p = c(1, .5))
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_s7_class(back, PRConfig)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
  }
  expect_identical(chart_registry()[["pr"]][["setup"]], "setup_PRConfig")
  schema <- chart_schema(
    PRConfig,
    id = "https://example.org/pr.json",
    title = "PR",
    description = "Precision-recall records."
  )
  p <- schema[["properties"]]
  expect_identical(p[["type"]][["const"]], "pr")
  expect_identical(
    as.character(p[["variant"]][["enum"]]),
    c("aggregate", "per_resample")
  )
  expect_equal(p[["digits"]][["maximum"]], 8)
  expect_false(any(vapply(p, function(x) "default" %in% names(x), logical(1L))))
  expect_identical(draw(cfg, data = data)[["x"]][["aspect"]][["ratio"]], 1)
  expect_null(
    draw(
      setup_PRConfig(recall = "r", precision = "p", square = FALSE),
      data = data
    )[[
      "x"
    ]][["aspect"]]
  )
})
