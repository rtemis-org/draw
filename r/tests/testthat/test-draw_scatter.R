# The scatter chart and the fits shared with the line chart, built by the
# @rtemis/draw scatter and line builders. spec: draw/shared-builders#scatter

scatter_data <- function(request) jsonlite::fromJSON(request@data)

# Bill length and depth of the Palmer penguins (datasets, R >= 4.5.0): within
# each species depth rises with length, and over all three it falls.
penguins_bills <- function() {
  skip_if(getRversion() < "4.5.0", "datasets::penguins needs R >= 4.5.0")
  data <- get("penguins", envir = asNamespace("datasets"))
  stats::na.omit(data[c("species", "bill_len", "bill_dep")])
}

test_that("draw_scatter sends its columns, config, and fits as a build request", {
  w <- draw_scatter(mtcars$wt, mtcars$mpg, fit = "glm", n_fit = 5L)
  build <- w[["x"]][["build"]]
  expect_identical(build[["type"]], "scatter")
  expect_identical(build[["config"]][["x"]], "x")
  expect_identical(build[["config"]][["fit"]], "glm")
  data <- jsonlite::fromJSON(build[["data"]])
  expect_equal(data[["columns"]][["x"]], mtcars$wt)
  expect_equal(data[["fits"]][["y"]], "y")
  fit <- stats::glm(mpg ~ wt, data = mtcars)
  grid <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 5L)
  expect_equal(
    data[["fits"]][["fitted"]][[1L]],
    unname(stats::predict(fit, data.frame(wt = grid)))
  )
})

test_that("compile() fits each group, or one curve over every group", {
  cfg <- setup_ScatterConfig(
    x = "bill_len",
    y = "bill_dep",
    group = "species",
    fit = "glm",
    n_fit = 3L
  )
  penguins <- penguins_bills()
  per_group <- scatter_data(compile(cfg, data = penguins))[["fits"]]
  expect_identical(per_group[["group"]], levels(penguins$species))
  cfg@fit_per_group <- FALSE
  pooled <- scatter_data(compile(cfg, data = penguins))[["fits"]]
  expect_identical(nrow(pooled), 1L)
  expect_true(is.na(pooled[["group"]]) || is.null(pooled[["group"]]))
  # Pooled, the slope is negative; within each species it is positive.
  slope <- function(fit) diff(fit[["fitted"]][[1L]][c(1L, 3L)])
  expect_lt(slope(pooled), 0)
  expect_true(all(vapply(
    seq_len(nrow(per_group)),
    function(i) diff(per_group[["fitted"]][[i]][c(1L, 3L)]) > 0,
    logical(1)
  )))
})

test_that("a pooled fit draws under its own legend entry", {
  skip_if_no_node()
  penguins <- penguins_bills()
  w <- draw_scatter(
    penguins$bill_len,
    penguins$bill_dep,
    group = penguins$species,
    fit = "glm",
    fit_per_group = FALSE
  )
  option <- built_option(w, simplify = FALSE)
  expect_identical(
    unlist(option[["legend"]][["data"]]),
    c(levels(penguins$species), "Fit")
  )
  fit <- option[["series"]][[5L]]
  expect_identical(fit[["name"]], "Fit")
  expect_identical(fit[["lineStyle"]][["color"]], "#808080")
})

test_that("polynomial and loess fits match stats", {
  x <- seq(0, 10, length.out = 40L)
  y <- sin(x) + x / 3
  grid <- data.frame(x = seq(0, 10, length.out = 4L))
  poly <- fit_model_values(
    data.frame(x = x),
    y,
    grid,
    "polynomial",
    TRUE,
    list(degree = 2L)
  )
  reference <- stats::glm(y ~ poly(x, 2L, raw = TRUE))
  expect_equal(
    poly[["fitted"]],
    unname(stats::predict(reference, grid))
  )
  smooth <- fit_model_values(
    data.frame(x = x),
    y,
    grid,
    "loess",
    TRUE,
    list(span = 0.5)
  )
  reference <- stats::loess(y ~ x, span = 0.5)
  expect_equal(
    smooth[["fitted"]],
    unname(stats::predict(reference, grid))
  )
  expect_length(smooth[["se"]], 4L)
})

test_that("a GAM engine other than mgcv is an error naming rtemislive", {
  expect_error(
    draw_scatter(
      1:10,
      (1:10)^2,
      fit = "gam",
      fit_params = list(engine = "pygam")
    ),
    "rtemislive",
    class = "rtemis_value_error"
  )
})

test_that("draw_line draws a dashed fit in each line's color", {
  skip_if_no_node()
  w <- draw_line(
    symptom_scores$week,
    symptom_scores$score,
    fit = "loess",
    fit_params = list(span = 0.75),
    n_fit = 20L
  )
  option <- built_option(w, simplify = FALSE)
  types <- vapply(option[["series"]], `[[`, "", "type")
  expect_identical(types, c("line", "line", "line"))
  fit <- option[["series"]][[3L]]
  expect_identical(fit[["lineStyle"]][["type"]], "dashed")
  expect_length(fit[["data"]], 20L)
})

test_that("a line fit requires observations and lines", {
  expect_error(
    setup_LineConfig(x = "a", y = "b", fit = "glm", aggregate = "mean"),
    "aggregate to none"
  )
  expect_error(
    setup_LineConfig(x = "a", y = "b", fit = "glm", area = TRUE),
    "area to FALSE"
  )
  expect_error(
    draw_line(c("a", "b", "c"), 1:3, fit = "glm"),
    "numeric x"
  )
})

test_that("a size column sets symbol area and must be nonnegative", {
  skip_if_no_node()
  data <- data.frame(x = 1:3, y = 1:3, n = c(1, 4, 16))
  cfg <- setup_ScatterConfig(x = "x", y = "y", size = "n")
  points <- built_option(compile(cfg, data = data), simplify = FALSE)[[
    "series"
  ]][[1L]][["data"]]
  expect_equal(
    vapply(points, function(p) p[["symbolSize"]], numeric(1)),
    c(7.5, 15, 30)
  )
  expect_error(draw_scatter(1:2, 1:2, size = c(-1, 2)), "nonnegative")
})

test_that("unset point size uses point_size", {
  skip_if_no_node()
  option <- built_option(draw_scatter(1:3, 1:3, point_size = 9))
  expect_equal(option[["series"]][[1L]][["symbolSize"]], 9)
})

test_that("a missing size drops its row, as a missing color does", {
  skip_if_no_node()
  w <- draw_scatter(1:4, c(2, 3, 1, 4), size = c(10, NA, 30, 40))
  points <- built_option(w, simplify = FALSE)[["series"]][[1L]][["data"]]
  expect_length(points, 3L)
  expect_error(draw_scatter(1:2, 1:2, size = c(1, Inf)), "infinity")
})
