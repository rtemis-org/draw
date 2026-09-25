test_that("confusion configs validate, serialize, and preserve provenance", {
  cfg <- setup_ConfusionConfig(classes = c("B", "A"), digits = 2)
  expect_s7_class(cfg, ConfusionConfig)
  expect_identical(cfg@type, "confusion")
  expect_identical(cfg@digits, 2L)
  expect_identical(cfg@origin[["classes"]], "user")
  expect_false("panel" %in% names(to_list(cfg)))
  for (args in list(
    list(classes = c("A", "A")),
    list(classes = ""),
    list(classes = NA_character_),
    list(digits = -1),
    list(digits = 9),
    list(digits = 1.5),
    list(ncol = 0),
    list(font_size = Inf),
    list(correct_color = "red"),
    list(show_metrics = NA)
  )) {
    expect_error(do.call(setup_ConfusionConfig, args))
  }
  data <- data.frame(
    reference = c("A", "B"),
    predicted = c("A", "B"),
    n = c(2, 3)
  )
  inferred <- resolve(setup_ConfusionConfig(), data)
  expect_identical(inferred@classes, c("A", "B"))
  expect_identical(inferred@origin[["classes"]], "derived")
  expect_identical(resolve(inferred, data), inferred)
  expect_identical(resolve(cfg, data)@classes, c("B", "A"))
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
  }
  schema <- chart_schema(
    ConfusionConfig,
    id = "https://example.org/confusion.json",
    title = "Confusion",
    description = "Confusion counts."
  )
  props <- schema[["properties"]]
  expect_identical(props[["type"]][["const"]], "confusion")
  expect_true(props[["classes"]][["uniqueItems"]])
  expect_equal(props[["digits"]][["maximum"]], 8)
  expect_equal(props[["ncol"]][["minimum"]], 1)
  expect_false(any(vapply(
    props,
    function(p) "default" %in% names(p),
    logical(1)
  )))
})
