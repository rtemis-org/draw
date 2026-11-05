pr_test_labels <- function() {
  factor(c("yes", "no", "yes", "no"), levels = c("no", "yes"))
}
pr_test_scores <- function() c(.9, .8, .7, .1)
pr_test_records <- function() pr_input(pr_test_labels(), pr_test_scores())
pr_test_config <- function(...) {
  setup_PRConfig(
    ap = "ap",
    prevalence = "prevalence",
    class_label = "class",
    split = "split",
    fold = "fold",
    omitted = "omitted",
    ...
  )
}

# Precision and recall at every distinct score, from their definitions.
pr_by_definition <- function(y, score) {
  thresholds <- sort(unique(score), decreasing = TRUE)
  precision <- vapply(
    thresholds,
    function(t) sum(y & score >= t) / sum(score >= t),
    numeric(1)
  )
  recall <- vapply(
    thresholds,
    function(t) sum(y & score >= t) / sum(y),
    numeric(1)
  )
  list(
    precision = precision,
    recall = recall,
    ap = sum(diff(c(0, recall)) * precision)
  )
}

# Polyline ECharts draws for `step: "start"` on a value x-axis, as in
# `turnPointsIntoStep()` of src/chart/line/LineView.ts.
pr_step_polyline <- function(x, y) {
  n <- length(x)
  rbind(
    cbind(
      c(rbind(x[-n], x[-n])),
      c(rbind(y[-n], y[-1L]))
    ),
    c(x[[n]], y[[n]])
  )
}

test_that("empirical precision-recall matches a worked example", {
  out <- pr_test_records()
  expect_equal(out[["recall"]], c(0, .5, .5, 1, 1))
  expect_equal(out[["precision"]], c(1, 1, 1 / 2, 2 / 3, 1 / 2))
  expect_equal(unique(out[["ap"]]), .5 * 1 + .5 * 2 / 3)
  expect_equal(unique(out[["prevalence"]]), .5)
  expect_identical(unique(out[["class"]]), "yes")
  expect_identical(unique(out[["omitted"]]), 0L)
})

test_that("empirical precision-recall agrees with its definition", {
  set.seed(2026L)
  for (i in seq_len(50L)) {
    n <- sample(5:200, 1L)
    y <- stats::rbinom(n, 1L, stats::runif(1L, .05, .9))
    if (!any(y == 1L) || !any(y == 0L)) {
      next
    }
    # Rounding produces tied blocks with both classes.
    score <- round(stats::runif(n) * .7 + .3 * y, sample(1:3, 1L))
    labels <- factor(ifelse(y == 1L, "yes", "no"), levels = c("no", "yes"))
    out <- pr_input(labels, score)
    truth <- pr_by_definition(y == 1L, score)
    expect_equal(out[["precision"]][-1L], truth[["precision"]])
    expect_equal(out[["recall"]][-1L], truth[["recall"]])
    expect_equal(out[["ap"]][[1L]], truth[["ap"]])
    expect_equal(tail(out[["precision"]], 1L), mean(y))
    expect_equal(out[["prevalence"]][[1L]], mean(y))
  }
})

test_that("tied blocks enter together and direction is fixed", {
  y <- pr_test_labels()
  tied <- pr_input(y, rep(.5, 4L))
  expect_equal(tied[["recall"]], c(0, 1))
  expect_equal(tied[["precision"]], c(.5, .5))
  expect_equal(unique(tied[["ap"]]), .5)
  # A negative top score keeps precision 0 at recall 0.
  worst <- pr_input(y, c(.2, .9, .1, .8))
  expect_equal(worst[["recall"]], c(0, 0, 0, .5, 1))
  expect_equal(worst[["precision"]], c(0, 0, 0, 1 / 3, 1 / 2))
  expect_lt(worst[["ap"]][[1L]], pr_test_records()[["ap"]][[1L]])
})

test_that("the positive class defines the curve", {
  y <- factor(c("yes", "no", "yes", "no", "no"), levels = c("no", "yes"))
  score <- c(.9, .8, .7, .6, .1)
  swapped <- pr_input(y, 1 - score, positive = "no")
  expect_identical(unique(swapped[["class"]]), "no")
  expect_equal(swapped[["precision"]][-1L], c(1, 1, 2 / 3, 3 / 4, 3 / 5))
  expect_equal(
    pr_input(y, score)[["precision"]][-1L],
    c(1, 1 / 2, 2 / 3, 1 / 2, 2 / 5)
  )
  rare <- factor(c("a", "a", "a", "b"))
  expect_equal(unique(pr_input(rare, c(.1, .2, .3, .9))[["prevalence"]]), .25)
  expect_equal(
    unique(pr_input(rare, c(.9, .8, .7, .1), positive = "a")[["prevalence"]]),
    .75
  )
})

test_that("multiclass curves are one-versus-rest with class prevalence", {
  y <- factor(c("A", "A", "A", "B", "B", "C"))
  p <- matrix(
    c(.8, .1, .1, .7, .2, .1, .6, .3, .1, .2, .7, .1, .3, .6, .1, .1, .1, .8),
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(NULL, levels(y))
  )
  out <- pr_input(y, p)
  expect_identical(unique(out[["class"]]), levels(y))
  prevalence <- tapply(out[["prevalence"]], out[["class"]], unique)
  expect_equal(as.numeric(prevalence), c(1 / 2, 1 / 3, 1 / 6))
  expect_equal(unique(out[["ap"]]), 1)
  expect_identical(out, pr_input(y, p[, c(3L, 1L, 2L)]))
})

test_that("missing pairs and classes without positives are disclosed", {
  y <- pr_test_labels()
  missing <- pr_input(y, c(.9, NA, .7, .1))
  expect_identical(unique(missing[["omitted"]]), 1L)
  expect_equal(unique(missing[["prevalence"]]), 2 / 3)
  absent <- pr_input(
    factor(rep("no", 4L), levels = levels(y)),
    pr_test_scores()
  )
  expect_true(is.na(absent[["recall"]]))
  expect_true(is.na(absent[["ap"]]))
  expect_error(draw_pr(absent), "No defined precision-recall")
  # Precision and recall need positives only.
  all_positive <- pr_input(
    factor(rep("yes", 4L), levels = levels(y)),
    pr_test_scores()
  )
  expect_equal(unique(all_positive[["ap"]]), 1)
  expect_equal(unique(all_positive[["prevalence"]]), 1)
})

test_that("input sets pair by name and assembled records keep their identities", {
  y <- pr_test_labels()
  score <- pr_test_scores()
  a <- pr_input(
    list(Test = y, Training = y),
    list(Training = score, Test = 1 - score)
  )
  expect_identical(unique(a[["split"]]), c("Test", "Training"))
  expect_equal(
    a[a[["split"]] == "Training", "ap"][[1L]],
    pr_test_records()[["ap"]][[1L]]
  )
  expect_error(pr_input(y), "recall and precision")
  expect_error(pr_input(data.frame(recall = 0), positive = "yes"), "Omit")
  bad <- roc_probabilities(y, score)
  bad[["levels"]] <- rev(bad[["levels"]])
  expect_error(pr_vertices(bad), class = "rtemis_input_error")
})

test_that("precision-recall tables are ordered, validated, and summarized", {
  d <- pr_test_records()
  out <- pr_data(pr_test_config(), d[nrow(d):1L, ])
  expect_equal(out[["curves"]][[1L]][["x"]], d[["recall"]])
  expect_equal(out[["curves"]][[1L]][["y"]], d[["precision"]])
  coarse <- data.frame(recall = c(0, 1), precision = c(1, .5), ap = .9)
  expect_equal(
    pr_data(setup_PRConfig(ap = "ap"), coarse)[["curves"]][[1L]][["summary"]],
    .9
  )
  expect_equal(
    pr_data(setup_PRConfig(), coarse)[["curves"]][[1L]][["summary"]],
    .5
  )
  # A sklearn-style curve anchored at (0, 1) reads in threshold order.
  anchored <- data.frame(recall = c(1, .5, 0), precision = c(.5, 1, 1))
  expect_equal(
    pr_data(setup_PRConfig(), anchored)[["curves"]][[1L]][["summary"]],
    .75
  )
  edits <- list(
    function(x) transform(x, precision = 2),
    function(x) transform(x, recall = -1),
    function(x) x[x[["recall"]] < 1, ],
    function(x) x[x[["recall"]] > 0, ],
    function(x) transform(x, ap = c(.5, .5, .5, .5, .6)),
    function(x) transform(x, prevalence = c(.5, .5, .5, .5, .6)),
    function(x) transform(x, prevalence = 2),
    function(x) transform(x, ap = 2),
    function(x) transform(x, omitted = -1),
    function(x) {
      x[1L, "recall"] <- NA
      x
    }
  )
  for (edit in edits) {
    expect_error(
      pr_data(pr_test_config(), edit(d)),
      class = "rtemis_value_error"
    )
  }
  expect_error(
    pr_data(pr_test_config(variant = "per_resample"), d),
    "No curves match"
  )
})

test_that("drawn curves are stepped and their area is average precision", {
  set.seed(1L)
  y <- factor(sample(c("no", "yes"), 300L, TRUE, c(.9, .1)))
  score <- round(stats::runif(300L) * .7 + .3 * (y == "yes"), 2L)
  records <- pr_input(y, score)
  option <- draw_pr(records)[["x"]][["option"]]
  curve <- option[["series"]][[1L]]
  expect_identical(curve[["step"]], "start")
  expect_false(curve[["smooth"]])
  x <- vapply(curve[["data"]], `[[`, numeric(1L), 1L)
  yy <- vapply(curve[["data"]], `[[`, numeric(1L), 2L)
  line <- pr_step_polyline(x, yy)
  drawn <- sum(
    diff(line[, 1L]) * (head(line[, 2L], -1L) + tail(line[, 2L], -1L)) / 2
  )
  expect_equal(drawn, records[["ap"]][[1L]])
  # The two summaries differ on this curve, so only `step` satisfies the test.
  trapezoid <- sum(diff(x) * (head(yy, -1L) + tail(yy, -1L)) / 2)
  expect_gt(abs(trapezoid - drawn), 1e-3)
})

test_that("each curve group draws its own prevalence baseline", {
  y <- pr_test_labels()
  skewed <- factor(c("yes", "no", "no", "no"), levels = levels(y))
  records <- pr_input(
    list(Training = y, Test = skewed),
    list(Training = pr_test_scores(), Test = pr_test_scores())
  )
  option <- draw_pr(records, palette = c("#111111", "#222222"))[["x"]][[
    "option"
  ]]
  series <- option[["series"]]
  expect_length(series, 4L)
  # Curves precede their baselines: a legend key styles from the first series.
  expect_identical(series[[1L]][["name"]], option[["legend"]][["data"]][[1L]])
  expect_identical(series[[3L]][["name"]], series[[1L]][["name"]])
  expect_identical(series[[4L]][["name"]], series[[2L]][["name"]])
  expect_equal(
    lapply(series[[3L]][["data"]], as.numeric),
    list(c(0, .5), c(1, .5))
  )
  expect_equal(
    lapply(series[[4L]][["data"]], as.numeric),
    list(c(0, .25), c(1, .25))
  )
  expect_identical(series[[3L]][["lineStyle"]][["color"]], "#111111")
  expect_identical(series[[4L]][["lineStyle"]][["color"]], "#222222")
  expect_identical(series[[3L]][["lineStyle"]][["type"]], "dotted")
  expect_true(series[[3L]][["silent"]])
  expect_length(
    draw_pr(records, baseline = FALSE)[["x"]][["option"]][["series"]],
    2L
  )
  table <- data.frame(recall = c(0, 1), precision = c(1, .5))
  expect_length(draw_pr(table)[["x"]][["option"]][["series"]], 1L)
})

test_that("fold summaries count each fold once and baselines use defined folds", {
  d <- pr_test_records()
  d[["fold"]] <- "one"
  second <- d
  second[["fold"]] <- "two"
  second[["ap"]] <- .5
  second[["prevalence"]] <- .25
  unavailable <- d[1L, ]
  unavailable[["fold"]] <- "three"
  unavailable[c("recall", "precision", "ap", "prevalence")] <- NA_real_
  records <- rbind(d, second, unavailable)
  out <- pr_data(pr_test_config(variant = "per_resample"), records)
  g <- out[["groups"]][[1L]]
  ap <- d[["ap"]][[1L]]
  expect_equal(g[["mean"]], (ap + .5) / 2)
  expect_equal(g[["sd"]], sqrt((ap - .5)^2 / 2))
  expect_equal(g[["available"]], 2L)
  expect_equal(g[["total"]], 3L)
  expect_equal(out[["undefined"]], 1L)
  expect_equal(g[["reference"]], .375)
  expect_message(
    widget <- draw_pr(records, variant = "per_resample", palette = "#123456"),
    "1 undefined curve"
  )
  option <- widget[["x"]][["option"]]
  expect_length(option[["legend"]][["data"]], 1L)
  expect_match(option[["legend"]][["data"]][[1L]], "AP mean")
  expect_length(option[["series"]], 3L)
  expect_identical(
    option[["series"]][[3L]][["name"]],
    option[["series"]][[1L]][["name"]]
  )
  expect_equal(option[["series"]][[1L]][["lineStyle"]][["opacity"]], .45)
  expect_identical(
    widget[["x"]],
    draw(
      pr_test_config(variant = "per_resample", palette = "#123456"),
      data = records
    )[["x"]]
  )
})

test_that("hover labels round without changing numeric vertices or AP", {
  option <- draw_pr(pr_test_records(), digits = 2L)[["x"]][["option"]]
  curve <- option[["series"]][[1L]]
  point <- curve[["data"]][[4L]]
  expect_equal(point[[2L]], 2 / 3)
  expect_identical(point[[7L]], "0.67")
  expect_identical(point[[8L]], "0.83")
  expect_match(option[["legend"]][["data"]][[1L]], "yes \\(AP 0.83\\)")
  expect_identical(
    vapply(curve[["dimensions"]][1:5], identity, character(1L)),
    c("Recall", "Precision", "AP", "Resample", "Missing pairs")
  )
  expect_identical(option[["xAxis"]][["name"]], "Recall")
  expect_identical(option[["yAxis"]][["name"]], "Precision")
})

test_that("precision-recall SVG contains stepped curves, baseline, and labels", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  widget <- draw_pr(pr_test_records(), palette = "#123456", filename = path)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(text, "yes (AP 0.833)", fixed = TRUE)
  expect_match(text, 'stroke="#123456"', fixed = TRUE)
  expect_match(text, 'stroke-dasharray', fixed = TRUE)
  expect_false(grepl("<image", text, fixed = TRUE))
  expect_false(grepl('fill-opacity="0"[^>]*ecmeta_ssr_type="chart"', text))
  grid <- static_aspect(
    widget[["x"]][["option"]],
    widget[["x"]][["aspect"]],
    800,
    500
  )[["grid"]]
  expect_equal(grid[["width"]], grid[["height"]])
})
