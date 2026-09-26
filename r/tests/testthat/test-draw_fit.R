# test-draw_fit.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

test_that("true/predicted pairs are aligned before missing values are removed", {
  data <- true_pred_data(
    list(Training = c(1, NA, 3), Test = matrix(c(4, 5), ncol = 1L)),
    list(Test = c(40, 50), Training = c(10, 20, NA))
  )
  expect_identical(data[["true"]], c(1, 4, 5))
  expect_identical(data[["predicted"]], c(10, 40, 50))
  expect_identical(data[["sample"]], c("Training", "Test", "Test"))
  expect_identical(
    true_pred_data(1:4, c(1, NA, 3, 4), group = c("a", "a", NA, "b")),
    data.frame(true = c(1, 4), predicted = c(1, 4), sample = c("a", "b"))
  )
  expect_equal(nrow(true_pred_data(list(NULL, 1:3), list(NULL, 2:4))), 3L)
})


test_that("malformed pairs fail before concatenation or recycling", {
  expect_error(true_pred_data(1:3, 1:2), "same positive length")
  expect_error(
    true_pred_data(list(1:2, 1:3), list(1:3, 1:2)),
    "same positive length"
  )
  expect_error(true_pred_data(list(A = 1:3), list(B = 1:3)), "same set names")
  expect_error(true_pred_data(list(A = 1:3, A = 1:3), list(1:3, 1:3)), "unique")
  expect_error(true_pred_data(list(1:3), 1:3), "nonempty lists")
  expect_error(true_pred_data(list(1:3), list(1:3), group = 1:3), "list names")
  expect_error(true_pred_data(matrix(1:6, ncol = 2L), 1:6), "single-column")
  expect_error(true_pred_data(letters[1:3], 1:3), "numeric vectors")
  expect_error(true_pred_data(c(1, Inf), c(1, 2)), "infinite")
  expect_error(true_pred_data(c(NA_real_, 2), c(1, NA_real_)), "complete")
  expect_error(true_pred_data(1:3, 1:3, group = "a"), "one value")
  expect_error(true_pred_data(list(NULL), list(NULL)), "at least one set")
})


test_that("draw_fit supplies true/predicted defaults and a clipped identity line", {
  w <- draw_fit(1:5, c(1.2, 1.8, 3.3, 3.8, 5.2))
  expect_s3_class(w, "htmlwidget")
  o <- w[["x"]][["option"]]
  expect_identical(o[["xAxis"]][["name"]], "True")
  expect_identical(o[["yAxis"]][["name"]], "Predicted")
  expect_equal(o[["xAxis"]][["min"]], o[["yAxis"]][["min"]])
  expect_equal(o[["xAxis"]][["max"]], o[["yAxis"]][["max"]])
  expect_length(o[["series"]], 4L)
  identity <- o[["series"]][[4L]]
  expect_equal(
    identity[["data"]],
    list(rep(o[["xAxis"]][["min"]], 2L), rep(o[["xAxis"]][["max"]], 2L))
  )
  expect_true(identity[["silent"]])
  expect_false(identity[["showSymbol"]])
  expect_null(identity[["name"]])
  expect_identical(identity[["lineStyle"]][["type"]], "dashed")
  expect_match(o[["series"]][[1L]][["name"]], "R\\^2 = ")
  expect_identical(o[["series"]][[1L]][["name"]], o[["series"]][[3L]][["name"]])
  o <- draw_fit(
    1:5,
    1:5,
    fit = NULL,
    rsq = FALSE,
    square = FALSE,
    equal_axes = FALSE,
    xlim = c(0, 4),
    ylim = c(2, 6),
    diagonal_color = "#123456"
  )[["x"]][["option"]]
  expect_equal(o[["series"]][[2L]][["data"]], list(c(2, 2), c(4, 4)))
  expect_identical(o[["series"]][[2L]][["lineStyle"]][["color"]], "#123456")
  expect_length(
    draw_fit(
      1:3,
      4:6,
      fit = NULL,
      square = FALSE,
      equal_axes = FALSE,
      xlim = c(0, 3),
      ylim = c(4, 7)
    )[["x"]][["option"]][["series"]],
    1L
  )
})


test_that("fit coefficients, uncertainty, and R-squared match independent OLS calculations", {
  x <- 1:6
  y <- c(1.1, 2.4, 2.8, 4.5, 4.7, 6.4)
  n <- length(x)
  slope <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  intercept <- mean(y) - slope * mean(x)
  residual_ss <- sum((y - intercept - slope * x)^2)
  residual_sd <- sqrt(residual_ss / (n - 2L))
  xgrid <- seq(1, 6, length.out = 5L)
  fitted <- intercept + slope * xgrid
  se <- residual_sd * sqrt(1 / n + (xgrid - mean(x))^2 / sum((x - mean(x))^2))
  o <- draw_fit(x, y, n_fit = 5L, se_times = 2)[["x"]][["option"]]
  fit <- do.call(rbind, o[["series"]][[3L]][["data"]])
  band <- do.call(rbind, o[["series"]][[2L]][["data"]])
  expect_equal(unname(fit[, 1L]), xgrid)
  expect_equal(unname(fit[, 2L]), fitted, tolerance = 1e-10)
  expect_equal(
    unname(band[, 2L]),
    c(fitted + 2 * se, rev(fitted - 2 * se)),
    tolerance = 1e-10
  )
  r2 <- 1 - residual_ss / sum((y - mean(y))^2)
  expect_identical(
    o[["series"]][[1L]][["name"]],
    paste0("Fit (R^2 = ", formatC(r2, format = "f", digits = 3L), ")")
  )
})


test_that("fits handle insufficient and constant data explicitly", {
  expect_error(draw_fit(rep(1, 4), 1:4), "two distinct x")
  expect_error(draw_fit(1:2, 1:2), "at least 3")
  expect_s3_class(draw_fit(rep(1, 4), 1:4, fit = NULL), "htmlwidget")
  o <- draw_fit(1:5, rep(2, 5))[["x"]][["option"]]
  expect_match(o[["series"]][[1L]][["name"]], "R\\^2 = NA")
  expect_error(draw_fit(1:4, 1:4, fit = "unsupported"))
})


test_that("fit and scatter calls share config validation", {
  expect_error(setup_ScatterConfig(n_fit = 2.5))
  expect_identical(setup_ScatterConfig(n_fit = 5)@n_fit, 5L)
  for (draw_fn in list(draw_fit, draw_scatter)) {
    expect_error(draw_fn(1:4, 1:4, se = NA))
    expect_error(draw_fn(1:4, 1:4, n_fit = 1L))
    expect_error(draw_fn(1:4, 1:4, n_fit = 2.5))
    expect_error(draw_fn(1:4, 1:4, fit_alpha = 2))
    expect_error(draw_fn(1:4, 1:4, square = "yes"))
    expect_error(draw_fn(1:4, 1:4, pad = -1))
  }
})


test_that("fit options validate and survive portable config round trips", {
  expect_error(ScatterConfig(se_times = -1))
  expect_error(ScatterConfig(diagonal = "yes"))
  expect_error(ScatterConfig(rsq = NA))
  expect_error(ScatterConfig(diagonal_color = 1))
  cfg <- setup_ScatterConfig(
    x = "true",
    y = "predicted",
    group = "sample",
    fit = "glm",
    se_times = 2,
    rsq = TRUE,
    diagonal = TRUE,
    diagonal_color = "#123456",
    square = TRUE,
    equal_axes = TRUE,
    xlab = "True",
    ylab = "Predicted"
  )
  data <- true_pred_data(
    list(Train = 1:5, Test = 2:5),
    list(Train = c(1.2, 1.8, 3.3, 3.8, 5.2), Test = c(2.2, 2.7, 4.3, 4.8))
  )
  direct <- draw_fit(
    data[["true"]],
    data[["predicted"]],
    group = data[["sample"]],
    se_times = 2,
    diagonal_color = "#123456"
  )
  expect_identical(draw(cfg, data = data)[["x"]], direct[["x"]])
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_chart_config(cfg, path)
  restored <- read_chart_config(path)
  expect_identical(draw(restored, data = data)[["x"]], direct[["x"]])
  schema <- chart_schema(
    ScatterConfig,
    id = "https://example.org/scatter.json",
    title = "Scatter",
    description = "Scatter chart."
  )
  props <- schema[["properties"]]
  expect_identical(props[["diagonal"]][["type"]], "boolean")
  expect_identical(props[["rsq"]][["type"]], "boolean")
  expect_equal(props[["se_times"]][["minimum"]], 0)
  expect_setequal(props[["diagonal_color"]][["type"]], c("string", "null"))
  expect_false("default" %in% names(props[["se_times"]]))
  expect_true(all(cfg@origin[c("se_times", "rsq", "diagonal")] == "user"))
})
