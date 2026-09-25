confusion_test_matrix <- function() {
  matrix(c(8, 2, 1, 9), 2, dimnames = list(c("yes", "no"), c("yes", "no")))
}

confusion_test_summary <- function(data, ...) {
  cfg <- resolve(setup_ConfusionConfig(...), data)
  confusion_data(cfg, data)
}

test_that("confusion normalization preserves class identity, factor levels, and missing pairs", {
  x <- confusion_test_matrix()
  d <- confusion_input(x[, 2:1])
  p <- confusion_test_summary(d)[["panels"]][[1L]]
  expect_equal(p[["counts"]], x)
  expect_identical(d[["reference"]], c("yes", "no", "yes", "no"))
  labels <- factor(c("B", "A", "B", NA), levels = c("B", "A", "unused"))
  predicted <- factor(c("A", "A", NA, "B"), levels = c("unused", "A", "B"))
  d <- confusion_input(labels, predicted)
  out <- confusion_test_summary(d)
  expect_identical(out[["classes"]], c("B", "A", "unused"))
  p <- out[["panels"]][[1L]]
  expect_equal(p[["total"]], 2)
  expect_equal(p[["omitted"]], 2)
  expect_equal(unname(p[["counts"]][1:2, 1:2]), matrix(c(0, 0, 1, 1), 2))
  expect_true(all(is.na(p[["fraction"]][3L, ])))
  expect_identical(
    confusion_input(
      data.frame(y = labels),
      matrix(as.character(predicted), ncol = 1)
    ),
    d
  )
  one <- confusion_input(matrix(3, 1, 1))
  expect_identical(one[["reference"]], "Class 1")
  expect_equal(one[["n"]], 3)
  panels <- confusion_input(list(Train = x, Test = x[2:1, 2:1]))
  expect_identical(unique(panels[["panel"]]), c("Train", "Test"))
  expect_equal(
    confusion_test_summary(panels, panel = "panel")[["panels"]][[2L]][[
      "counts"
    ]],
    x
  )
})

test_that("confusion rates and macro recall are computed from counts", {
  p <- confusion_test_summary(confusion_input(confusion_test_matrix()))[[
    "panels"
  ]][[1L]]
  expect_equal(unname(p[["sensitivity"]]), c(8 / 9, 9 / 11))
  expect_equal(unname(p[["specificity"]]), c(9 / 11, 8 / 9))
  expect_equal(unname(p[["ppv"]]), c(.8, .9))
  expect_equal(unname(p[["npv"]]), c(.9, .8))
  expect_equal(p[["accuracy"]], .85)
  expect_equal(p[["balanced_accuracy"]], (8 / 9 + 9 / 11) / 2)
  multi <- matrix(
    c(8, 1, 1, 1, 3, 1, 0, 2, 3),
    3,
    byrow = TRUE,
    dimnames = list(LETTERS[1:3], LETTERS[1:3])
  )
  p <- confusion_test_summary(confusion_input(multi))[["panels"]][[1L]]
  expect_equal(p[["balanced_accuracy"]], (.8 + .6 + .6) / 3)
  expect_false(isTRUE(all.equal(
    p[["balanced_accuracy"]],
    mean((p[["sensitivity"]] + p[["specificity"]]) / 2)
  )))
  # Duplicate long records represent disjoint frequency contributions.
  d <- confusion_input(multi)
  repeated <- rbind(d, d)
  p2 <- confusion_test_summary(repeated)[["panels"]][[1L]]
  expect_equal(p2[["counts"]], multi * 2)
  expect_equal(p2[["balanced_accuracy"]], p[["balanced_accuracy"]])
})

test_that("zero denominators and omitted records do not become measured zero rates", {
  x <- matrix(c(5, 0, 0, 0), 2, dimnames = list(c("A", "B"), c("A", "B")))
  p <- confusion_test_summary(confusion_input(x))[["panels"]][[1L]]
  expect_true(is.na(p[["balanced_accuracy"]]))
  expect_true(is.na(p[["sensitivity"]][[2L]]))
  expect_true(is.na(p[["ppv"]][[2L]]))
  expect_true(is.na(p[["specificity"]][[1L]]))
  expect_equal(p[["accuracy"]], 1)
  empty <- confusion_test_summary(confusion_input(x * 0))[["panels"]][[1L]]
  expect_true(is.na(empty[["accuracy"]]))
  expect_true(all(is.na(empty[["fraction"]])))
  absent <- data.frame(
    reference = NA_character_,
    predicted = NA_character_,
    n = 4
  )
  expect_error(confusion_test_summary(absent), "class label")
  empty <- confusion_test_summary(absent, classes = c("A", "B"))[["panels"]][[
    1L
  ]]
  expect_equal(empty[["total"]], 0)
  expect_equal(empty[["omitted"]], 4)
})

test_that("malformed counts and label inputs are rejected", {
  good <- confusion_input(confusion_test_matrix())
  for (n in list(
    c(-1, 2, 3, 4),
    c(NA, 2, 3, 4),
    c(.5, 2, 3, 4),
    c(Inf, 2, 3, 4),
    rep(2^53, 4),
    c("1", "2", "3", "4")
  )) {
    data <- good
    data[["n"]] <- n
    expect_error(confusion_test_summary(data), class = "rtemis_input_error")
  }
  expect_error(confusion_test_summary(good, classes = "yes"), "every reference")
  bad <- good
  bad[["reference"]][[1L]] <- ""
  expect_error(confusion_test_summary(bad), "empty")
  bad <- good
  bad[["panel"]] <- NA_character_
  expect_error(confusion_test_summary(bad, panel = "panel"), "panel label")
  expect_error(confusion_input(1:3, c("A", "B")), "equally sized")
  expect_error(
    confusion_input(c("A", "B"), c("A", "A"), classes = "A"),
    "covering"
  )
  expect_error(confusion_input(matrix(1:6, 3, 2)), "square")
  bad <- confusion_test_matrix()
  colnames(bad) <- c("other", "no")
  expect_error(confusion_input(bad), "matching class")
  expect_error(confusion_input(list(Train = 1:3)), "list of confusion matrices")
  expect_error(
    confusion_input(list(confusion_test_matrix())),
    "Supply a confusion matrix"
  )
})

test_that("functional and config rendering agree and every panel has independent axes", {
  x <- list(
    Train = confusion_test_matrix(),
    Test = confusion_test_matrix()[2:1, 2:1]
  )
  d <- confusion_input(x)
  cfg <- setup_ConfusionConfig(panel = "panel", classes = c("no", "yes"))
  w <- draw_confusion(x, classes = c("no", "yes"))
  expect_identical(w[["x"]], draw(cfg, data = d)[["x"]])
  opt <- w[["x"]][["option"]]
  expect_length(opt[["grid"]], 8L)
  expect_length(opt[["series"]], 10L)
  expect_identical(opt[["xAxis"]][[1L]][["data"]], as.list(c("no", "yes")))
  expect_identical(
    opt[["xAxis"]][[5L]][["data"]],
    opt[["xAxis"]][[1L]][["data"]]
  )
  expect_equal(opt[["series"]][[6L]][["xAxisIndex"]], 4)
  expect_equal(opt[["visualMap"]][[6L]][["seriesIndex"]], 5)
  expect_identical(strip_js(opt), opt)
  wire <- jsonlite::fromJSON(
    jsonlite::toJSON(opt, auto_unbox = TRUE, keep_vec_names = TRUE),
    simplifyVector = FALSE
  )
  expect_identical(
    wire[["series"]][[1L]][["data"]][[1L]][["value"]][1:2],
    list(0L, 0L)
  )
  expect_equal(w[["width"]], 1120)
  expect_equal(w[["height"]], 500)
  simple <- draw_confusion(x, show_metrics = FALSE)[["x"]][["option"]]
  expect_length(simple[["grid"]], 2L)
  expect_length(simple[["series"]], 4L)
  one <- draw_confusion(matrix(3, 1, 1), show_metrics = FALSE)[["x"]][[
    "option"
  ]]
  json <- jsonlite::fromJSON(
    jsonlite::toJSON(one, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_identical(json[["xAxis"]][[1L]][["data"]], list("Class 1"))
  expect_length(json[["series"]][[1L]][["data"]], 1L)
})

test_that("SVG exports contain the entire count and metric composition", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_confusion(
    list(
      Training = confusion_test_matrix(),
      Test = confusion_test_matrix() * 0
    ),
    filename = path
  )
  svg <- readLines(path, warn = FALSE)
  # Four count tiles, eight per-class metric tiles, two overall tiles per panel.
  expect_equal(sum(grepl('<path .*ecmeta_ssr_type="chart"', svg)), 28L)
  for (label in c(
    "Training",
    "Test",
    "Accuracy",
    "BA",
    "PPV",
    "NPV",
    "NA",
    "0.854"
  )) {
    expect_true(
      any(grepl(paste0(">", label, "</text>"), svg, fixed = TRUE)),
      label = label
    )
  }
  expect_true(any(grepl('width="1120"', svg, fixed = TRUE)))
  expect_false(any(grepl("<image|@keyframes|<animate", svg)))
})
