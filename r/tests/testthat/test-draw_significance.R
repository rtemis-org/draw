test_that("adjustment precedes display filtering and retains the complete family", {
  cfg <- setup_SignificanceConfig(label = "label")
  data <- data.frame(
    estimate = c(-2, 2, NA, 1),
    p_value = c(.01, .02, .001, NA),
    label = LETTERS[1:4]
  )
  out <- significance_data(cfg, data)
  expect_equal(out[["data"]][["p_adjusted"]], c(.03, .04, .004, NA))
  expect_identical(out[["data"]][["keep"]], c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(out[["data"]][["group"]], c(1L, 3L, 2L, 2L))
  expect_equal(out[["data"]][["value"]][1:2], -log10(c(.03, .04)))
  cfg@p_adjust_method <- "bonferroni"
  expect_equal(
    significance_data(cfg, data)[["data"]][["p_adjusted"]],
    c(.04, .08, .004, NA)
  )
  cfg@n_tests <- 6L
  expect_equal(
    significance_data(cfg, data)[["data"]][["p_adjusted"]],
    c(.06, .12, .006, NA)
  )
  cfg@n_tests <- 3L
  expect_error(
    significance_data(cfg, data),
    "every supplied row",
    class = "rtemis_input_error"
  )
})


test_that("zero heights are visibly capped while raw values and small positives survive", {
  cfg <- setup_SignificanceConfig(p_adjust_method = "none")
  data <- data.frame(estimate = c(-1, 1, 2), p_value = c(0, 1e-310, .5))
  out <- significance_data(cfg, data)
  expect_equal(out[["data"]][["p_adjusted"]], data[["p_value"]])
  expect_equal(out[["data"]][["value"]][[2L]], 310)
  expect_gt(out[["zero_cap"]], 310)
  expect_equal(out[["data"]][["value"]][[1L]], out[["zero_cap"]])
  expect_identical(out[["data"]][["capped"]], c(TRUE, FALSE, FALSE))
  cfg@zero_cap <- 300
  expect_error(
    significance_data(cfg, data),
    "above all finite",
    class = "rtemis_input_error"
  )
  cfg@zero_cap <- 320
  expect_equal(significance_data(cfg, data)[["zero_cap"]], 320)
  expect_error(draw_volcano(-1, 0, ylim = c(0, 1)), "include `zero_cap`")
  # An unplottable zero still contributes to adjustment, but it needs no
  # display cap and cannot invalidate the finite visible points' height.
  omitted_zero <- data
  omitted_zero[["estimate"]][[1L]] <- NA_real_
  cfg@zero_cap <- 300
  expect_null(significance_data(cfg, omitted_zero)[["zero_cap"]])
  for (transform in c("identity", "one_minus")) {
    cfg <- setup_SignificanceConfig(
      p_adjust_method = "none",
      p_transform = transform
    )
    out <- significance_data(cfg, data)
    expect_null(out[["zero_cap"]])
    expect_false(any(out[["data"]][["capped"]]))
    expect_equal(
      out[["data"]][["value"]],
      if (transform == "identity") data[["p_value"]] else 1 - data[["p_value"]]
    )
  }
})


test_that("group and annotation semantics handle boundaries, ties, and missing groups", {
  data <- data.frame(
    estimate = c(-2, -1, 0, 1, 2, 3),
    p_value = c(.01, .01, .01, .05, .01, .01)
  )
  cfg <- setup_SignificanceConfig(p_adjust_method = "none", annotate_n = 1L)
  out <- significance_data(cfg, data)[["data"]]
  expect_identical(out[["group"]], c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_identical(which(out[["annotate"]]), c(1L, 5L))
  cfg@annotate_n <- 0L
  expect_false(any(significance_data(cfg, data)[["data"]][["annotate"]]))
  w <- draw_volcano(c(1, 2), c(.001, .001))
  s <- w[["x"]][["option"]][["series"]]
  expect_length(s, 2L)
  expect_identical(s[[1L]][["name"]], "Significant positive")
  expect_identical(s[[1L]][["itemStyle"]][["color"]], "#0F6A66")
  expect_null(s[[2L]][["name"]])
  expect_length(s[[2L]][["markLine"]][["data"]], 2L)
})


test_that("significance input rejects malformed values before recycling or transformation", {
  cfg <- setup_SignificanceConfig(label = "label")
  good <- list(estimate = c(-1, 2), p_value = c(.01, .1), label = c("A", "B"))
  for (bad in list(
    list(estimate = c(1, Inf)),
    list(p_value = c(.1, Inf)),
    list(p_value = c(-.1, .2)),
    list(p_value = c(1.1, .2)),
    list(p_value = c(".1", ".2")),
    list(estimate = c(1 + 1i, 2)),
    list(label = c("", "B")),
    list(label = c("A", NA)),
    list(label = "A"),
    list(estimate = c(NA, NA)),
    list(p_value = c(NA, NA))
  )) {
    data <- good
    data[names(bad)] <- bad
    expect_error(significance_data(cfg, data), class = "rtemis_input_error")
  }
  expect_error(significance_input(1:2, .1), "equally sized")
  expect_error(significance_input(1:2, c(.1, .2), "A"), "one outcome label")
  expect_error(significance_input(matrix(1:2, 2), c(.1, .2)), "vectors")
  expect_identical(significance_input(c(A = 1), .1)[["label"]], "A")
})


test_that("both functional views compile through the same portable config", {
  data <- data.frame(
    estimate = c(-2, .1, 3),
    p_value = c(.001, .6, .002),
    label = c("A", "B", "C")
  )
  for (view in c("volcano", "manhattan")) {
    cfg <- setup_SignificanceConfig(
      view = view,
      label = "label",
      p_adjust_method = "BH",
      title = "Evidence"
    )
    w <- do.call(
      get(paste0("draw_", view)),
      list(
        x = data[["estimate"]],
        pvals = data[["p_value"]],
        xnames = data[["label"]],
        p_adjust_method = "BH",
        title = "Evidence"
      )
    )
    expect_identical(w[["x"]], draw(cfg, data = data)[["x"]])
    expect_identical(strip_js(w[["x"]][["option"]]), w[["x"]][["option"]])
  }
  w <- draw_manhattan(
    c(-1, NA, 2),
    c(.001, NA, 0),
    xnames = c("same", "missing", "same")
  )
  opt <- w[["x"]][["option"]]
  expect_identical(
    opt[["xAxis"]][["data"]],
    as.list(c("same", "missing", "same"))
  )
  expect_match(opt[["title"]][["subtext"]], "1 outcome.*3 tests")
  expect_identical(tail(opt[["series"]], 1)[[1]][["symbol"]], "triangle")
  expect_identical(
    tail(opt[["series"]], 1)[[1]][["name"]],
    "Significant positive"
  )
})


test_that("significance SVGs contain marks, annotations, thresholds, and zero-cap disclosure", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  for (view in c("volcano", "manhattan")) {
    w <- do.call(
      get(paste0("draw_", view)),
      list(
        x = c(-2, .1, 3),
        pvals = c(.001, .6, 0),
        xnames = c("Alpha", "Beta", "Gamma"),
        title = "Evidence"
      )
    )
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path), add = TRUE)
    save_drawing(w, path)
    svg <- readLines(path, warn = FALSE)
    marks <- grep(
      '<path .*fill="#[[:xdigit:]]{6}".*ecmeta_ssr_type="chart"',
      svg,
      value = TRUE
    )
    expect_length(marks, if (view == "volcano") 3L else 4L)
    for (color in c("#BE2E5F", "#808080", "#0F6A66")) {
      expect_true(any(grepl(color, marks, fixed = TRUE)))
    }
    for (label in c("Alpha", "Gamma", "Evidence")) {
      expect_true(any(grepl(paste0(">", label, "</text>"), svg, fixed = TRUE)))
    }
    expect_true(any(grepl("p = 0 shown at", svg, fixed = TRUE)))
    expect_true(any(grepl("p &lt; 0.05", svg, fixed = TRUE)))
    expect_true(any(grepl('stroke-dasharray=', svg, fixed = TRUE)))
    expect_false(any(grepl("<image|@keyframes|<animate", svg)))
  }
})
