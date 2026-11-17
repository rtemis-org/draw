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
    p_value = c(.01, .01, .01, .05, .01, .01),
    label = LETTERS[1:6]
  )
  cfg <- setup_SignificanceConfig(
    label = "label",
    p_adjust_method = "none",
    annotate_n = 1L
  )
  s <- raw_option(draw(cfg, data = data))[["series"]]
  # A significant estimate at x_thresh is positive; p equal to p_thresh is not
  # significant.
  names_of <- function(s) vapply(s[["data"]], function(d) d[["name"]], "")
  expect_identical(
    vapply(s[1:3], function(x) x[["name"]], ""),
    c("Significant negative", "Not significant", "Significant positive")
  )
  expect_identical(
    lapply(s[1:3], names_of),
    list(c("A", "B"), "D", c("C", "E", "F"))
  )
  # One label per side: the smallest raw p-value, ties in input order.
  labeled <- function(s) {
    names_of(s)[vapply(s[["data"]], function(d) !is.null(d[["label"]]), TRUE)]
  }
  expect_identical(unlist(lapply(s[1:3], labeled)), c("A", "C"))
  cfg@annotate_n <- 0L
  s <- raw_option(draw(cfg, data = data))[["series"]]
  expect_length(unlist(lapply(s[1:3], labeled)), 0L)
  w <- draw_volcano(c(1, 2), c(.001, .001))
  s <- raw_option(w)[["series"]]
  expect_length(s, 2L)
  expect_identical(s[[1L]][["name"]], "Significant positive")
  expect_identical(
    s[[1L]][["itemStyle"]][["color"]],
    SIGN_COLORS[["positive"]]
  )
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
    expect_identical(widget_wire(w)[["build"]][["type"]], "significance")
  }
  w <- draw_manhattan(
    c(-1, NA, 2),
    c(.001, NA, 0),
    xnames = c("same", "missing", "same")
  )
  opt <- raw_option(w)
  expect_identical(
    opt[["xAxis"]][["data"]],
    as.list(c("same", "missing", "same"))
  )
  expect_match(
    opt[["title"]][["subtext"]],
    "^p = 0 shown at .* [(]triangles[)]$"
  )
  expect_identical(tail(opt[["series"]], 1)[[1]][["symbol"]], "triangle")
  expect_identical(
    tail(opt[["series"]], 1)[[1]][["name"]],
    "Significant positive"
  )
})


test_that("significance SVGs contain marks, annotations, thresholds, and zero-cap disclosure", {
  skip_if_no_node()
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
    for (color in SIGN_COLORS) {
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


test_that("display groups preserve full-family adjustment and threshold annotations", {
  data <- data.frame(
    estimate = c(-2, 1, -1, 2, 3),
    p_value = c(.001, .001, .03, .5, .0001),
    label = LETTERS[1:5],
    category = c("B", "B", "A", "A", NA)
  )
  cfg <- setup_SignificanceConfig(
    group = "category",
    label = "label",
    annotate_n = 1L,
    palette = c("#123456", "#abcdef")
  )
  prepared <- significance_data(cfg, data)[["data"]]
  expect_equal(
    prepared[["p_adjusted"]],
    stats::p.adjust(data[["p_value"]], "holm", n = 5)
  )
  expect_identical(prepared[["keep"]], c(TRUE, TRUE, TRUE, TRUE, FALSE))
  option <- raw_option(draw(cfg, data = data))
  expect_equal(option[["legend"]][["data"]], list("B", "A"))
  expect_equal(option[["series"]][[1]][["itemStyle"]][["color"]], "#123456")
  expect_length(option[["series"]][[1]][["data"]], 2L)
  # Annotations rank within significance sides, whatever the display group.
  labeled <- Filter(
    function(d) !is.null(d[["label"]]),
    option[["series"]][[1]][["data"]]
  )
  expect_identical(vapply(labeled, function(d) d[["name"]], ""), c("A", "B"))
  expect_length(option[["series"]][[3]][["markLine"]][["data"]], 2L)
  expect_null(option[["series"]][[3]][["name"]])
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  write_chart_config(cfg, file)
  expect_equal(raw_option(draw(read_chart_config(file), data = data)), option)
  for (view in c("volcano", "manhattan")) {
    cfg@view <- view
    widget <- do.call(
      get(paste0("draw_", view)),
      list(
        x = data[["estimate"]],
        pvals = data[["p_value"]],
        xnames = data[["label"]],
        group = data[["category"]],
        annotate_n = 1L,
        palette = cfg@palette
      )
    )
    expect_equal(raw_option(widget), raw_option(draw(cfg, data = data)))
  }
  expect_error(draw_volcano(1:2, c(.1, .2), group = "A"), "one value")
  expect_error(draw_volcano(1:2, c(.1, .2), group = c("", "A")), "nonempty")
  expect_error(draw_volcano(1:2, c(.1, .2), group = c(NA, NA)), "complete")
  expect_error(setup_SignificanceConfig(palette = "red"), "group binding")
  expect_error(setup_SignificanceConfig(group = "g", palette = character()))
})


test_that("custom significance groups and capped bars use vector palette colors", {
  skip_if_no_node()
  for (view in c("volcano", "manhattan")) {
    for (palette in list(NULL, "#123456", c("#123456", "#abcdef"))) {
      widget <- do.call(
        get(paste0("draw_", view)),
        list(
          x = c(-2, 0, 2),
          pvals = c(.01, .5, 0),
          group = c("B", "A", "B"),
          p_adjust_method = "none",
          palette = palette
        )
      )
      file <- tempfile(fileext = ".svg")
      on.exit(unlink(file), add = TRUE)
      save_drawing(widget, file)
      svg <- paste(readLines(file, warn = FALSE), collapse = "\n")
      expect_false(grepl("<image|NaN", svg))
      marks <- grep(
        '<path .*fill="#[[:xdigit:]]{6}".*ecmeta_ssr_type="chart"',
        strsplit(svg, "\n", fixed = TRUE)[[1]],
        value = TRUE
      )
      mark_colors <- sub('.*fill="(#[[:xdigit:]]{6})".*', "\\1", marks)
      expect_length(marks, if (view == "volcano") 3L else 4L)
      expected_counts <- if (length(palette) == 1L) {
        length(marks)
      } else {
        c(1L, if (view == "volcano") 2L else 3L)
      }
      expect_equal(sort(as.integer(table(mark_colors))), expected_counts)
      for (label in c("B", "A")) {
        expect_match(svg, paste0(">", label, "</text>"), fixed = TRUE)
      }
      if (!is.null(palette)) {
        for (color in palette) {
          expect_match(svg, color, fixed = TRUE)
        }
      }
    }
  }
})


test_that("volcano annotations and threshold labels clear marks and each other", {
  # Clustered strong effects put many annotations in one corner of the plot.
  estimate <- c(
    -1.80,
    -1.78,
    -1.50,
    -1.45,
    -1.35,
    -1.32,
    1.20,
    1.22,
    1.25,
    1.83,
    1.85,
    seq(-0.5, 0.5, length.out = 20)
  )
  p_value <- c(
    1e-6,
    2e-6,
    3e-6,
    4e-6,
    5e-6,
    2e-6,
    1e-6,
    2e-6,
    3e-6,
    4e-6,
    5e-6,
    rep(0.5, 20)
  )
  label <- paste0("feature_", seq_along(estimate))
  volcano <- draw_volcano(estimate, p_value, label, annotate_n = 11L)
  manhattan <- draw_manhattan(estimate, p_value, label)
  series <- raw_option(volcano)[["series"]]
  named <- Filter(function(s) !is.null(s[["name"]]), series)
  # The flag is the whole contract: ECharts' static labelLayout cannot move a
  # label attached to a scatter symbol.
  for (s in named) {
    expect_true(s[["rtemisRepelLabels"]])
    expect_null(s[["labelLayout"]])
  }
  for (w in list(volcano, manhattan)) {
    opt <- raw_option(w)
    ref <- Filter(function(s) !is.null(s[["markLine"]]), opt[["series"]])[[1L]]
    expect_identical(
      ref[["markLine"]][["data"]][[1L]][["label"]][["position"]],
      "end"
    )
    # 8 characters at 0.6 em of 12 px, 5 px from the line end, 8 px spare.
    expect_gte(opt[["grid"]][["right"]], 71)
  }
  # A supplied margin wins over the reserved space.
  expect_equal(
    raw_option(draw_manhattan(estimate, p_value, label, margin_right = 10L))[[
      "grid"
    ]][["right"]],
    10
  )

  skip_if_no_node()
  options <- tempfile(fileext = ".json")
  on.exit(unlink(options), add = TRUE)
  writeLines(
    jsonlite::toJSON(
      list(
        volcano = raw_option(volcano),
        manhattan = raw_option(manhattan)
      ),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    options
  )
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "significance_labels.js")),
      shQuote(system.file(package = "rtemis.draw")),
      shQuote(options)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(
    paste(output, collapse = "\n"),
    "Significance labels clear marks and each other"
  )
})


test_that("a significance chart takes annotation layers", {
  w <- draw_annotate(draw_volcano(c(-2, 1, 3), c(.001, .5, .002)), vline = 1)
  expect_length(widget_wire(w)[["build"]][["layers"]], 1L)
  series <- raw_option(w)[["series"]]
  expect_identical(
    tail(series, 1L)[[1L]][["markLine"]][["data"]][[1L]][["xAxis"]],
    1L
  )
})
