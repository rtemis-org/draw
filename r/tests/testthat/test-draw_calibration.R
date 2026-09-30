test_that("calibration bins include both endpoints and preserve every observation", {
  d <- data.frame(
    observed = c(0, 1, 0, 1, 1),
    probability = c(0, .25, .5, .75, 1)
  )
  g <- calibration_data(
    setup_CalibrationConfig(n_bins = 2L, bin_method = "equidistant"),
    d
  )[[1L]]
  expect_equal(g[["bins"]][["n"]], c(2L, 3L))
  expect_equal(g[["bins"]][["probability"]], c(.125, .75))
  expect_equal(g[["bins"]][["observed"]], c(.5, 2 / 3))
  expect_equal(g[["bins"]][["lower"]], c(0, .5))
  expect_equal(g[["bins"]][["upper"]], c(.5, 1))
  expect_equal(g[["brier"]], (0 + .75^2 + .5^2 + .25^2 + 0) / 5)
  for (method in c("equidistant", "quantile")) {
    for (n in c(1L, 2L, 10L)) {
      out <- calibration_data(
        setup_CalibrationConfig(n_bins = n, bin_method = method),
        d
      )[[1L]]
      expect_equal(sum(out[["bins"]][["n"]]), nrow(d))
      expect_equal(
        weighted.mean(out[["bins"]][["probability"]], out[["bins"]][["n"]]),
        mean(d[["probability"]])
      )
      expect_equal(
        weighted.mean(out[["bins"]][["observed"]], out[["bins"]][["n"]]),
        mean(d[["observed"]])
      )
      expect_identical(out[["brier"]], g[["brier"]])
    }
  }
})

test_that("quantile calibration merges tied boundaries and handles constant scores", {
  d <- data.frame(observed = c(0, 0, 1, 1), probability = c(0, 0, .5, 1))
  g <- calibration_data(setup_CalibrationConfig(n_bins = 4L), d)[[1L]]
  expect_equal(g[["bins"]][["probability"]], c(0, .5, 1))
  expect_equal(g[["bins"]][["n"]], c(2L, 1L, 1L))
  for (score in c(0, .5, 1)) {
    d[["probability"]] <- score
    g <- calibration_data(setup_CalibrationConfig(), d)[[1L]]
    expect_equal(g[["bins"]][["n"]], 4L)
    expect_equal(g[["bins"]][["observed"]], .5)
    expect_equal(g[["bins"]][["lower"]], score)
    expect_equal(g[["bins"]][["upper"]], score)
    widget <- draw_calibration(d, mode = "lines")
    expect_equal(
      widget[["x"]][["option"]][["series"]][[2L]][["itemStyle"]][["opacity"]],
      1
    )
  }
})

test_that("binary class identity and named sample alignment match ROC conventions", {
  y <- factor(c("no", "yes", "no", "yes"), levels = c("no", "yes"))
  p <- c(.1, .8, .4, .9)
  d <- calibration_input(y, p)
  expect_equal(d[["observed"]], c(0L, 1L, 0L, 1L))
  named <- matrix(1 - p, ncol = 1L, dimnames = list(NULL, "no"))
  expect_equal(calibration_input(y, named)[["probability"]], p)
  expect_equal(
    calibration_input(y, p, positive = "no")[["observed"]],
    1L - d[["observed"]]
  )
  multi <- calibration_input(
    list(Train = y, Test = y),
    list(Test = 1 - p, Train = p)
  )
  expect_equal(multi[["probability"]], c(p, 1 - p))
  expect_identical(unique(multi[["group"]]), c("Train", "Test"))
  expect_identical(calibration_input(d), d)
  for (args in list(
    list(y),
    list(y, p[1:2]),
    list(y, c(0, 0, 0, Inf)),
    list(list(a = y), list(b = p)),
    list(list(y), p),
    list(list(), list()),
    list(list(a = y, a = y), list(p, p)),
    list(d, positive = "yes"),
    list(factor(c("a", "b", "c")), matrix(1 / 3, 3, 3)),
    list(list(y, factor(y, levels = rev(levels(y)))), list(p, p))
  )) {
    expect_error(do.call(calibration_input, args))
  }
  expect_no_error(calibration_input(
    list(y, factor(y, levels = rev(levels(y)))),
    list(p, p),
    positive = "yes"
  ))
})

test_that("missing pairs are removed jointly and invalid records are rejected", {
  d <- data.frame(observed = c(0, 1, NA, 1), probability = c(0, 1, .3, NA))
  g <- calibration_data(setup_CalibrationConfig(), d)[[1L]]
  expect_equal(g[["omitted"]], 2L)
  expect_equal(g[["probability"]], c(0, 1))
  expect_equal(g[["brier"]], 0)
  expect_error(
    calibration_data(setup_CalibrationConfig(na_rm = FALSE), d),
    "complete"
  )
  for (bad in list(
    data.frame(observed = c(0, 2), probability = c(.1, .8)),
    data.frame(observed = c("0", "1"), probability = c(.1, .8)),
    data.frame(observed = c(0, 1), probability = c(-.1, .8)),
    data.frame(observed = c(0, 1), probability = c(.1, Inf)),
    data.frame(observed = c(0, 1), probability = c(".1", ".8")),
    data.frame(observed = c(NA, NA), probability = c(.1, .8)),
    list(observed = c(0, 1), probability = .5),
    list(observed = numeric(), probability = numeric())
  )) {
    expect_error(calibration_data(setup_CalibrationConfig(), bad))
  }
  expect_error(
    calibration_data(
      setup_CalibrationConfig(group = "g"),
      data.frame(observed = 0, probability = .2, g = NA)
    ),
    "label"
  )
})

test_that("calibration direct and config paths share native lines, rugs, and full precision", {
  d <- data.frame(observed = c(0, 1, 1), probability = c(.123456789, .6, 1))
  cfg <- setup_CalibrationConfig(n_bins = 2L, digits = 2L, palette = "#123456")
  direct <- draw_calibration(d, n_bins = 2L, digits = 2L, palette = "#123456")
  expect_identical(direct[["x"]], draw(cfg, data = d)[["x"]])
  option <- direct[["x"]][["option"]]
  expect_length(option[["series"]], 3L)
  expect_equal(option[["series"]][[2L]][["data"]][[1L]][[1L]], .123456789)
  expect_identical(option[["series"]][[2L]][["data"]][[1L]][[4L]], "0.12")
  expect_identical(
    option[["series"]][[2L]][["name"]],
    option[["series"]][[3L]][["name"]]
  )
  expect_identical(
    option[["series"]][[2L]][["lineStyle"]][["color"]],
    "#123456"
  )
  expect_identical(
    option[["series"]][[3L]][["itemStyle"]][["color"]],
    "#123456"
  )
  expect_length(
    draw_calibration(d, diagonal = FALSE, rug = FALSE)[["x"]][["option"]][[
      "series"
    ]],
    1L
  )
  marker <- draw_calibration(d, mode = "markers", show_brier = FALSE)[["x"]][[
    "option"
  ]]
  expect_equal(marker[["series"]][[2L]][["lineStyle"]][["opacity"]], 0)
  expect_identical(marker[["legend"]][["data"]], list("Sample"))
  wire <- widget_payload_json(direct)
  expect_false(grepl("function\\s*\\(", wire))
  expect_no_error(jsonlite::fromJSON(wire))
})

test_that("calibration observation tables support explicit group bindings and pooling", {
  d <- data.frame(
    observed = c(0, 1, 0, 1),
    probability = c(.1, .8, .3, .9),
    model = c("A", "A", "B", "B")
  )
  expect_identical(
    draw_calibration(d, group = "model")[["x"]],
    draw(setup_CalibrationConfig(group = "model"), data = d)[["x"]]
  )
  names(d)[[3L]] <- "group"
  expect_length(
    draw_calibration(d)[["x"]][["option"]][["legend"]][["data"]],
    2L
  )
  expect_identical(
    draw_calibration(d, group = NULL)[["x"]],
    draw(setup_CalibrationConfig(), data = d)[["x"]]
  )
})

test_that("calibration SVG retains actual curve and rug marks", {
  skip_if_no_node()
  d <- data.frame(observed = c(0, 1, 0, 1), probability = c(0, .8, .4, 1))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  for (theme in list(theme_light(), theme_dark())) {
    widget <- draw_calibration(
      d,
      n_bins = 2L,
      palette = "#123456",
      theme = theme
    )
    save_drawing(widget, path, width = 390, height = 600)
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(svg, "Brier", fixed = TRUE)
    expect_match(svg, "#123456", fixed = TRUE)
    expect_false(grepl("<image|NaN|Infinity", svg))
    # Compare with the native rug-free scene: the additional paths must be
    # actual marks, not just a nonempty SVG or retained legend text.
    with_rug <- lengths(regmatches(svg, gregexpr("<path", svg, fixed = TRUE)))
    save_drawing(
      draw_calibration(d, n_bins = 2L, rug = FALSE, theme = theme),
      path,
      width = 390,
      height = 600
    )
    without <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_gte(
      with_rug -
        lengths(regmatches(without, gregexpr("<path", without, fixed = TRUE))),
      nrow(d)
    )
    # A reference diagonal must not shift the theme's group palette.
    save_drawing(
      draw_calibration(d, theme = theme),
      path,
      width = 390,
      height = 600
    )
    with_diagonal <- paste(readLines(path, warn = FALSE), collapse = "\n")
    save_drawing(
      draw_calibration(d, diagonal = FALSE, theme = theme),
      path,
      width = 390,
      height = 600
    )
    no_diagonal <- paste(readLines(path, warn = FALSE), collapse = "\n")
    first_color <- to_list(theme)[["color"]][[1L]]
    expect_match(with_diagonal, first_color, fixed = TRUE)
    expect_match(no_diagonal, first_color, fixed = TRUE)
  }
})
