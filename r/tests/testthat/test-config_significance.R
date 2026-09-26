test_that("significance configuration validates its portable settings", {
  cfg <- setup_SignificanceConfig()
  expect_s7_class(cfg, SignificanceConfig)
  expect_identical(cfg@type, "significance")
  expect_identical(cfg@p_adjust_method, "holm")
  expect_identical(cfg@view, "volcano")
  expect_identical(cfg@p_transform, "neg_log10")
  expect_setequal(names(cfg@origin), settable_props(cfg))
  expect_identical(to_list(cfg)[["p_adjust_method"]], "holm")
  expect_false("zero_cap" %in% names(to_list(cfg)))
  expect_identical(setup_SignificanceConfig(annotate_n = 2)@annotate_n, 2L)
  for (args in list(
    list(view = "line"),
    list(p_adjust_method = "bad"),
    list(p_transform = function(x) -log10(x)),
    list(p_thresh = 0),
    list(p_thresh = 1.1),
    list(alpha = -0.1),
    list(point_size = 0),
    list(annotate_n = 1.5),
    list(n_tests = 1.5),
    list(n_tests = 0),
    list(xlim = c(1, 0)),
    list(ylim = 1:3),
    list(xlim = c(0, Inf)),
    list(x_thresh = Inf),
    list(zero_cap = Inf),
    list(point_size = Inf),
    list(p_transform = "identity", zero_cap = 3),
    list(view = "manhattan", xlim = c(0, 1))
  )) {
    expect_error(do.call(setup_SignificanceConfig, args))
  }
})


test_that("significance resolve preserves authorship and complete JSON round trips", {
  data <- data.frame(
    effect = c(-2, 0.1, 3),
    p = c(0.001, 0.6, 0),
    outcome = c("A", "B", "C")
  )
  for (view in c("volcano", "manhattan")) {
    cfg <- setup_SignificanceConfig(
      view = view,
      estimate = "effect",
      p_value = "p",
      label = "outcome",
      ylab = "Evidence"
    )
    resolved <- resolve(cfg, data)
    expect_identical(resolved@n_tests, 3L)
    expect_identical(resolved@origin[["n_tests"]], "derived")
    expect_identical(resolved@ylab, "Evidence")
    expect_identical(resolved@origin[["ylab"]], "user")
    expect_identical(resolve(resolved, data), resolved)
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    for (complete in c(FALSE, TRUE)) {
      write_chart_config(resolved, path, complete = complete)
      restored <- read_chart_config(path)
      expect_identical(
        to_list(compile(restored, data)),
        to_list(compile(resolved, data))
      )
      doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      expect_identical(doc[["type"]], "significance")
    }
    expect_identical(cfg@n_tests, NULL)
  }
})


test_that("the significance schema comes from the shared class declaration", {
  schema <- chart_schema(
    SignificanceConfig,
    id = "https://example.org/significance.json",
    title = "Significance",
    description = "Significance plots."
  )
  props <- schema[["properties"]]
  expect_identical(props[["type"]][["const"]], "significance")
  expect_setequal(
    as.character(props[["view"]][["enum"]]),
    c("volcano", "manhattan")
  )
  expect_setequal(
    as.character(props[["p_transform"]][["enum"]]),
    c("neg_log10", "identity", "one_minus")
  )
  expect_equal(props[["p_thresh"]][["exclusiveMinimum"]], 0)
  expect_equal(props[["p_thresh"]][["maximum"]], 1)
  expect_true("null" %in% props[["zero_cap"]][["type"]])
  expect_null(props[["p_adjust_method"]][["default"]])
  expect_identical(
    chart_registry()[["significance"]][["cls"]],
    SignificanceConfig
  )
})
