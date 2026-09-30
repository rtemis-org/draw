# test-fit_model.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# spec: draw/learner-fits

skip_if_no_rtemis_fit <- function() {
  skip_if_not(
    rtemis_fit_available(),
    "rtemis with fit_predict() is not installed"
  )
}

fit_x <- seq(-2, 2, length.out = 40)
fit_y <- fit_x^2 + sin(seq_along(fit_x)) / 5


# %% fit_model_values ----

test_that("GLM and GAM fits match their fitters, in any case", {
  newdata <- data.frame(x = c(-1, 0, 1))
  glm_fit <- fit_model_values(
    data.frame(x = fit_x),
    fit_y,
    newdata,
    "GLM",
    TRUE
  )
  reference <- stats::predict(
    stats::glm(y ~ x, data = data.frame(x = fit_x, y = fit_y)),
    newdata = newdata,
    se.fit = TRUE
  )
  expect_equal(glm_fit[["fitted"]], unname(reference[["fit"]]))
  expect_equal(glm_fit[["se"]], unname(reference[["se.fit"]]))
  expect_null(
    fit_model_values(data.frame(x = fit_x), fit_y, newdata, "glm", FALSE)[[
      "se"
    ]]
  )
  skip_if_not_installed("mgcv")
  gam_fit <- fit_model_values(
    data.frame(x = fit_x),
    fit_y,
    newdata,
    "gam",
    TRUE
  )
  expect_length(gam_fit[["fitted"]], 3L)
  expect_gt(gam_fit[["rsq"]], glm_fit[["rsq"]])
})

test_that("GLM and GAM fit two features for surfaces", {
  x <- expand.grid(x = 1:6, y = 1:6)
  fit <- fit_model_values(x, x[["x"]] + 2 * x[["y"]], x[1:2, ], "glm", FALSE)
  expect_equal(fit[["fitted"]], c(3, 4))
  expect_equal(fit[["rsq"]], 1)
})

test_that("other fit names need rtemis, with a corrective error", {
  local_mocked_bindings(rtemis_fit_available = function() FALSE)
  expect_error(
    fit_model_values(
      data.frame(x = 1:5),
      1:5,
      data.frame(x = 1),
      "linad",
      FALSE
    ),
    "install or update rtemis",
    class = "rtemis_dependency_error"
  )
  expect_error(
    draw_scatter(fit_x, fit_y, fit = "linad"),
    class = "rtemis_dependency_error"
  )
})

test_that("rtemis learners fit by name", {
  skip_if_no_rtemis_fit()
  newdata <- data.frame(x = c(-1, 0, 1))
  # LINAD needs no package beyond rtemis, and has no standard errors.
  fit <- fit_model_values(data.frame(x = fit_x), fit_y, newdata, "linad", TRUE)
  expect_length(fit[["fitted"]], 3L)
  expect_null(fit[["se"]])
  expect_true(is.finite(fit[["rsq"]]))
  expect_error(
    fit_model_values(data.frame(x = fit_x), fit_y, newdata, "nonesuch", FALSE),
    class = "rtemis_input_error"
  )
})


# %% draw_scatter ----

test_that("draw_scatter draws a learner fit without a band", {
  skip_if_no_rtemis_fit()
  o <- draw_scatter(fit_x, fit_y, fit = "linad")[["x"]][["option"]]
  types <- vapply(o[["series"]], `[[`, "", "type")
  expect_identical(types, c("scatter", "line"))
  expect_length(o[["series"]][[2L]][["data"]], 200L)
  expect_null(o[["series"]][[2L]][["areaStyle"]])
})

test_that("draw_scatter fits a learner per group and labels R-squared", {
  skip_if_no_rtemis_fit()
  o <- draw_scatter(
    fit_x,
    fit_y,
    group = fit_x > 0,
    fit = "LINAD",
    rsq = TRUE,
    n_fit = 20L
  )[["x"]][["option"]]
  names <- vapply(o[["series"]], `[[`, "", "name")
  expect_length(names, 4L)
  expect_match(names, "^(FALSE|TRUE) \\(R\\^2 = [0-9.]+\\)$")
  expect_identical(names[1:2], names[3:4])
})

test_that("a config with a learner fit round-trips through JSON", {
  skip_if_no_rtemis_fit()
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "LINAD")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(cfg, path)
  restored <- read_chart_config(path)
  expect_identical(restored@fit, "LINAD")
  expect_equal(
    to_list(compile(restored, mtcars)),
    to_list(compile(cfg, mtcars))
  )
})


# %% draw_scatter3d ----

test_that("draw_scatter3d fits one uncolored surface per group", {
  o <- draw_scatter3d(
    iris[1:3],
    group = iris[["Species"]],
    fit = "glm",
    n_fit = 5L,
    fit_alpha = .5
  )[["x"]][["option"]]
  surfaces <- Filter(function(s) s[["type"]] == "surface", o[["series"]])
  expect_identical(
    vapply(surfaces, `[[`, "", "name"),
    levels(iris[["Species"]])
  )
  expect_identical(surfaces[[1L]][["dataShape"]], list(5L, 5L))
  expect_identical(surfaces[[1L]][["itemStyle"]], list(opacity = .5))
  # The z axis spans the predictions as well as the observations.
  z <- unlist(lapply(surfaces, function(s) lapply(s[["data"]], `[[`, 3L)))
  expect_lte(o[["zAxis3D"]][["min"]], min(z, iris[[3L]]))
  expect_gte(o[["zAxis3D"]][["max"]], max(z, iris[[3L]]))
})

test_that("3D surface predictions follow the fitted plane", {
  x <- rep(1:4, 4)
  y <- rep(1:4, each = 4)
  o <- draw_scatter3d(x, y, x + 2 * y, fit = "glm", n_fit = 2L)[["x"]][[
    "option"
  ]]
  surface <- o[["series"]][[2L]]
  # Corners in ECharts-GL order: x varies fastest.
  expect_equal(
    vapply(surface[["data"]], function(v) v[[3L]], 1),
    c(3, 6, 9, 12)
  )
})

test_that("3D fits validate settings and data", {
  expect_error(setup_Scatter3DConfig(n_fit = 1L))
  expect_error(setup_Scatter3DConfig(fit_alpha = 2))
  expect_identical(setup_Scatter3DConfig(n_fit = 4)@n_fit, 4L)
  expect_error(
    draw_scatter3d(c(1, 2), c(1, 2), c(1, 2), fit = "glm"),
    "at least 3",
    class = "rtemis_value_error"
  )
})

test_that("3D learner surfaces and fit configs round-trip", {
  skip_if_no_rtemis_fit()
  cfg <- setup_Scatter3DConfig(
    x = "Sepal.Length",
    y = "Sepal.Width",
    z = "Petal.Length",
    fit = "LINAD",
    n_fit = 6L
  )
  opt <- to_list(compile(cfg, iris))
  expect_identical(opt[["series"]][[2L]][["type"]], "surface")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(cfg, path)
  expect_equal(to_list(compile(read_chart_config(path), iris)), opt)
})


# %% fit_params ----

test_that("fit_params reach glm() and predictions use the response scale", {
  counts <- c(2, 3, 6, 7, 8, 9, 10, 12, 15, 20)
  x <- data.frame(x = seq_along(counts))
  fit <- fit_model_values(
    x,
    counts,
    x,
    "glm",
    FALSE,
    list(family = "poisson")
  )
  reference <- stats::glm(
    counts ~ x,
    family = "poisson",
    data = data.frame(x, counts = counts)
  )
  expect_equal(fit[["fitted"]], unname(stats::fitted(reference)))
})

test_that("fit_params set the GAM smooth basis dimension through k", {
  skip_if_not_installed("mgcv")
  fit <- fit_model_values(
    data.frame(x = fit_x),
    fit_y,
    data.frame(x = fit_x),
    "gam",
    FALSE,
    list(k = 4, method = "REML")
  )
  reference <- mgcv::gam(
    y ~ s(x, k = 4),
    method = "REML",
    data = data.frame(x = fit_x, y = fit_y)
  )
  expect_equal(fit[["fitted"]], unname(as.numeric(stats::fitted(reference))))
})

test_that("GLM and GAM fit_params errors name the learner", {
  expect_error(
    fit_model_values(
      data.frame(x = fit_x),
      fit_y,
      data.frame(x = 1),
      "glm",
      FALSE,
      list(famly = "poisson")
    ),
    "Check fit_params against \\?stats::glm",
    class = "rtemis_value_error"
  )
  expect_error(
    fit_model_values(
      data.frame(x = fit_x),
      fit_y,
      data.frame(x = 1),
      "glm",
      FALSE,
      list(data = mtcars)
    ),
    "draw supplies the model formula and data",
    class = "rtemis_value_error"
  )
})

test_that("fit_params reach the rtemis setup function", {
  skip_if_no_rtemis_fit()
  expect_error(
    draw_scatter(fit_x, fit_y, fit = "linad", fit_params = list(max_leafs = 2)),
    "does not take `max_leafs`",
    class = "rtemis_value_error"
  )
  one_leaf <- draw_scatter(
    fit_x,
    fit_y,
    fit = "linad",
    fit_params = list(max_leaves = 1),
    n_fit = 10L
  )[["x"]][["option"]][["series"]][[2L]][["data"]]
  # A single leaf is one linear model: a straight line.
  slopes <- diff(vapply(one_leaf, `[[`, 1, 2L)) /
    diff(vapply(one_leaf, `[[`, 1, 1L))
  expect_equal(slopes, rep(slopes[[1L]], length(slopes)))
  expect_error(
    draw_scatter3d(
      iris[1:3],
      fit = "linad",
      fit_params = list(max_leafs = 2)
    ),
    "does not take `max_leafs`"
  )
})

test_that("fit_params need a fit and round-trip through JSON", {
  expect_error(setup_ScatterConfig(fit_params = list(k = 3)), "Set fit")
  expect_error(setup_Scatter3DConfig(fit_params = list(k = 3)), "Set fit")
  expect_error(setup_ScatterConfig(fit = "glm", fit_params = list(1)))
  cfg <- setup_ScatterConfig(
    x = "wt",
    y = "carb",
    fit = "glm",
    fit_params = list(family = "poisson")
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(cfg, path)
  restored <- read_chart_config(path)
  expect_identical(restored@fit_params, list(family = "poisson"))
  expect_equal(
    to_list(compile(restored, mtcars)),
    to_list(compile(cfg, mtcars))
  )
})
