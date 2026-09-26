# test-draw_learning_curve.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

test_that("learning losses retain gaps and sort steps without changing pairs", {
  data <- data.frame(
    iteration = c(3, 1, 2),
    loss_training = c(1, 4, 2),
    loss_validation = c(2, 5, NA)
  )
  expected <- data.frame(
    iteration = 1:3,
    Training = c(4, 2, 1),
    Validation = c(5, NA, 2),
    Selected = c(NA, 2, NA)
  )
  expect_equal(learning_curve_data(data, selected = 2), expected)
  attr(data, "unit") <- "epochs"
  attr(data, "selected") <- 2L
  option <- draw_learning_curve(data)[["x"]][["option"]]
  expect_identical(option[["xAxis"]][["name"]], "Epochs")
  expect_identical(option[["yAxis"]][["name"]], "Loss")
  expect_identical(
    vapply(option[["series"]], `[[`, "", "name"),
    c("Training", "Validation", "Selected")
  )
  expect_equal(
    option[["series"]][[3L]][["data"]],
    list(c(1, NA), c(2, 2), c(3, NA))
  )
  expect_equal(
    draw_learning_curve(data, selected = NULL)[["x"]][["option"]][["series"]] |>
      length(),
    2L
  )
  expect_identical(
    draw_learning_curve(data, unit = NULL)[["x"]][["option"]][["xAxis"]][[
      "name"
    ]],
    "Iteration"
  )
})

test_that("forest means use each series' available trees at each step", {
  # B stops at step 2; missing validation values must not discard training.
  data <- data.frame(
    tree = c("B", "A", "A", "B", "A"),
    iteration = c(2, 1, 3, 1, 2),
    loss_training = c(6, 8, 2, 12, 4),
    loss_validation = c(NA, 10, 3, 14, NA)
  )
  expect_equal(
    learning_curve_data(data, selected = 2),
    data.frame(
      iteration = 1:3,
      Training = c(10, 5, 2),
      Validation = c(12, NA, 3),
      Selected = c(NA, 5, NA)
    )
  )
  # Over ten steps exercises numeric ordering instead of lexical group order.
  many <- data.frame(iteration = 12:1, loss_training = (12:1)^2)
  expect_equal(learning_curve_data(many)[["Training"]], (1:12)^2)
})

test_that("unavailable loss series and selected steps do not create empty layers", {
  data <- data.frame(
    iteration = c(1, 3),
    loss_training = NA,
    loss_validation = c(4, 2)
  )
  expect_equal(
    learning_curve_data(data, selected = 3),
    data.frame(iteration = c(1, 3), Validation = c(4, 2), Selected = c(NA, 2))
  )
  for (selected in list(NULL, NA, NA_real_, 2, 20)) {
    expect_identical(
      names(learning_curve_data(data, selected = selected)),
      c("iteration", "Validation")
    )
  }
  data[["loss_training"]] <- c(5, NA)
  expect_identical(
    names(learning_curve_data(data, selected = 3)),
    c("iteration", "Training", "Validation")
  )
  one <- data.frame(iteration = 1, loss_training = 2)
  expect_s3_class(draw_learning_curve(one, selected = 1), "htmlwidget")
})

test_that("malformed learning data fail before grouping or drawing", {
  data <- data.frame(iteration = 1:3, loss_training = c(4, 2, 1))
  expect_error(learning_curve_data(list()), class = "S7_error_method_not_found")
  expect_error(learning_curve_data(data[FALSE, ]), "nonempty")
  expect_error(learning_curve_data(data["iteration"]), "loss_training")
  expect_error(learning_curve_data(data["loss_training"]), "iteration")
  bad <- data
  names(bad) <- c("iteration", "iteration")
  expect_error(learning_curve_data(bad), "unique column names")
  for (iterations in list(
    c(1, NA, 3),
    c(1, Inf, 3),
    letters[1:3],
    matrix(1:3),
    (1:3) + 1i
  )) {
    bad <- data
    bad[["iteration"]] <- iterations
    expect_error(learning_curve_data(bad), "finite numeric.*iteration")
  }
  expect_error(learning_curve_data(data[c(1, 1, 2), ]), "one row per iteration")
  bad <- data
  bad[["tree"]] <- c("A", NA, "B")
  expect_error(learning_curve_data(bad), "nonmissing.*tree")
  bad[["tree"]] <- c("A", "A", "B")
  bad[["iteration"]] <- c(1, 1, 2)
  expect_error(learning_curve_data(bad), "one row per iteration")
  for (losses in list(
    c(1, Inf, 2),
    letters[1:3],
    matrix(1:3),
    c(TRUE, FALSE, TRUE),
    (1:3) + 1i
  )) {
    bad <- data
    bad[["loss_training"]] <- losses
    expect_error(learning_curve_data(bad), "finite numeric losses")
  }
  data[["loss_training"]] <- NA_real_
  expect_error(learning_curve_data(data), "holds no losses")
  for (selected in list(
    "2",
    numeric(),
    c(1, 2),
    Inf,
    TRUE,
    matrix(1),
    2 + 1i
  )) {
    expect_error(
      learning_curve_data(data, selected = selected),
      "Set `selected`"
    )
  }
  for (unit in list("", NA_character_, 1, c("epochs", "leaves"))) {
    expect_error(draw_learning_curve(data, unit = unit))
  }
})

test_that("materialized learning data and line config reproduce the convenience API", {
  raw <- data.frame(
    iteration = 1:3,
    loss_training = c(4, 2, 1),
    loss_validation = c(5, 3, 3.5)
  )
  data <- data.frame(
    iteration = 1:3,
    Training = c(4, 2, 1),
    Validation = c(5, 3, 3.5),
    Selected = c(NA, 2, NA)
  )
  config <- setup_LineConfig(
    x = "iteration",
    y = c("Training", "Validation", "Selected"),
    xlab = "Epochs",
    ylab = "Loss",
    title = "Example",
    zoom = TRUE,
    margin_top = 70L,
    palette = c("red", "blue", "black")
  )
  direct <- draw_learning_curve(
    raw,
    unit = "epochs",
    selected = 2,
    title = "Example",
    zoom = TRUE,
    margin_top = 70L,
    palette = c("red", "blue", "black"),
    width = 640,
    height = 480
  )
  expect_equal(direct[["width"]], 640)
  expect_equal(direct[["height"]], 480)
  expect_identical(draw(config, data = data)[["x"]], direct[["x"]])
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  for (complete in c(FALSE, TRUE)) {
    write_chart_config(config, path, complete = complete)
    restored <- read_chart_config(path)
    expect_identical(draw(restored, data = data)[["x"]], direct[["x"]])
  }
  # Check series arrays and gaps as they cross the JavaScript boundary.
  json <- jsonlite::toJSON(
    list(series = direct[["x"]][["option"]][["series"]]),
    auto_unbox = TRUE,
    na = "null"
  )
  wire <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_null(wire[["series"]][[3L]][["data"]][[1L]][[2L]])
  expect_equal(wire[["series"]][[3L]][["data"]][[2L]], list(2, 2))
  expect_error(draw_learning_curve(raw, points = "yes"))
  expect_error(draw_learning_curve(raw, block_opacity = 2))
  expect_false(
    draw_learning_curve(raw, selected = 2, points = FALSE)[["x"]][["option"]][[
      "series"
    ]][[3L]][["showSymbol"]]
  )
  expect_identical(
    draw_learning_curve(raw, xlab = "Steps")[["x"]][["option"]][["xAxis"]][[
      "name"
    ]],
    "Steps"
  )
})
