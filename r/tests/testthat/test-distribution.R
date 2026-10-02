test_that("distribution samples validate values and preserve grouping alignment", {
  for (bad in list(
    c(1, Inf),
    c(1, -Inf),
    c("1", "2"),
    matrix(1:4, 2),
    complex(real = 1)
  )) {
    expect_error(distribution_samples(bad), "finite numeric")
  }
  expect_error(distribution_samples(c(1, NA), na_rm = FALSE), "na_rm")
  expect_error(distribution_samples(numeric()), "available")
  expect_error(distribution_samples(list()), "at least one")
  expect_error(distribution_samples(list(A = 1, A = 2)), "distinct")
  expect_error(distribution_samples(1:3, group = 1:2), "same length")
  expect_error(distribution_samples(1:3, group = rep(NA, 3)), "available")
  expect_error(distribution_samples(1:3, verbosity = -1L), "nonnegative")
  expect_message(
    samples <- distribution_samples(
      list(A = c(1, NA, 3, 99), B = c(4, 5, NA, 99)),
      group = c("first", "second", "first", NA)
    ),
    "Removed 1 NA value from A"
  )
  expect_identical(
    names(samples),
    c("A - first", "A - second", "B - first", "B - second")
  )
  expect_identical(samples[["A - first"]], c(1, 3))
  expect_identical(samples[["A - second"]], numeric())
  expect_identical(samples[["B - second"]], 5)
  expect_message(distribution_samples(c(1, NA), verbosity = 0L), NA)
})

test_that("density kernels and bandwidth controls agree with stats independently", {
  values <- c(-2, -1, 0, .5, 1, 4, 8)
  for (kernel in c(
    "gaussian",
    "epanechnikov",
    "rectangular",
    "triangular",
    "biweight",
    "cosine",
    "optcosine"
  )) {
    cfg <- setup_DensityConfig(
      n = 64L,
      bandwidth = .7,
      adjust = 1.2,
      kernel = kernel
    )
    expected <- stats::density(
      values,
      n = 64,
      bw = .7,
      adjust = 1.2,
      kernel = kernel
    )
    actual <- distribution_density(values, cfg)
    expect_equal(actual[["x"]], expected[["x"]])
    expect_equal(actual[["y"]], expected[["y"]])
  }
  cfg <- setup_DensityConfig()
  expect_error(distribution_density(2, cfg), "explicit.*bandwidth")
  expect_error(distribution_density(rep(2, 5), cfg), "explicit.*bandwidth")
  expect_identical(
    distribution_density(numeric(), cfg),
    list(x = numeric(), y = numeric())
  )
  cfg <- setup_DensityConfig(bandwidth = .5)
  expect_equal(
    distribution_density(2, cfg)[["y"]],
    stats::density(2, bw = .5)[["y"]]
  )
  expect_error(
    distribution_density(
      values,
      setup_DensityConfig(bandwidth = 1e308, adjust = 10)
    ),
    "bandwidth"
  )
  for (args in list(
    list(n = 64.5),
    list(bw = "wrong"),
    list(bandwidth = 0),
    list(adjust = Inf),
    list(kernel = "wrong"),
    list(fill_alpha = 2),
    list(bw = .5, bandwidth = .6)
  )) {
    expect_error(do.call(setup_DensityConfig, args))
  }
})

test_that("histogram edges and every normalization use actual sample sizes and widths", {
  samples <- list(A = c(0, 1, 1, 2, 4), B = c(0, 2, 4))
  edges <- c(0, 1, 2, 4)
  for (normalization in c("density", "count_density")) {
    opt <- to_list(histogram_option(
      samples,
      bin_edges = edges,
      normalization = normalization
    ))
    for (i in seq_along(samples)) {
      bins <- opt[["series"]][[i]][["data"]]
      counts <- vapply(bins, function(x) x[[4L]], numeric(1))
      heights <- vapply(bins, function(x) x[[3L]], numeric(1))
      expected <- graphics::hist(samples[[i]], breaks = edges, plot = FALSE)
      expect_equal(counts, expected[["counts"]])
      expect_equal(
        vapply(bins, function(x) x[[1L]], numeric(1)),
        head(edges, -1)
      )
      expect_equal(
        vapply(bins, function(x) x[[2L]], numeric(1)),
        tail(edges, -1)
      )
      expect_equal(
        sum(heights * diff(edges)),
        if (normalization == "density") 1 else length(samples[[i]])
      )
    }
  }
  for (normalization in c("count", "probability", "percent")) {
    opt <- to_list(histogram_option(
      samples,
      breaks = c(0, 2, 4),
      normalization = normalization
    ))
    for (i in seq_along(samples)) {
      heights <- vapply(
        opt[["series"]][[i]][["data"]],
        function(x) x[[3L]],
        numeric(1)
      )
      expect_equal(
        sum(heights),
        switch(
          normalization,
          count = length(samples[[i]]),
          probability = 1,
          percent = 100
        )
      )
    }
    expect_error(
      histogram_option(
        samples,
        bin_edges = edges,
        normalization = normalization
      ),
      "unequal-width"
    )
  }
  # A row excluded by its group must not distort shared bin edges.
  a <- to_list(histogram_option(c(1, 2, 1e6), group = c("A", "A", NA)))
  b <- to_list(histogram_option(c(1, 2), group = c("A", "A")))
  expect_identical(a, b)
  expect_error(histogram_option(1:5, bin_edges = c(1, 3)), "spanning")
  for (args in list(
    list(bins = 2.5),
    list(bins = 0),
    list(bin_edges = c(0, 0, 1)),
    list(bin_edges = c(0, Inf)),
    list(bins = 2, bin_edges = c(0, 1)),
    list(breaks = 2, bins = 3),
    list(normalization = "bad")
  )) {
    expect_error(do.call(setup_HistogramConfig, args))
  }
})

test_that("density overlays carry the exact histogram units and preserve empty samples", {
  values <- c(-2, -1, 0, .5, 1, 4, 8)
  expected <- stats::density(values, bw = .7, n = 64)
  for (normalization in c(
    "count",
    "probability",
    "percent",
    "density",
    "count_density"
  )) {
    opt <- to_list(histogram_option(
      values,
      bin_edges = c(-2, 0, 2, 4, 6, 8),
      density = TRUE,
      normalization = normalization,
      bandwidth = .7,
      n = 64L
    ))
    line <- opt[["series"]][[2L]]
    expect_true(line[["showSymbol"]])
    expect_equal(line[["itemStyle"]][["opacity"]], 0)
    expect_equal(line[["lineStyle"]][["opacity"]], 1)
    expect_equal(line[["emphasis"]][["itemStyle"]][["opacity"]], 1)
    curve <- do.call(rbind, opt[["series"]][[2L]][["data"]])
    multiplier <- switch(
      normalization,
      count = 14,
      probability = 2,
      percent = 200,
      density = 1,
      count_density = 7
    )
    expect_equal(curve[, 1], expected[["x"]])
    expect_equal(curve[, 2], expected[["y"]] * multiplier)
    expect_identical(
      opt[["series"]][[1L]][["name"]],
      opt[["series"]][[2L]][["name"]]
    )
  }
  opt <- to_list(histogram_option(
    list(A = values, Empty = numeric()),
    density = TRUE
  ))
  expect_true(all(vapply(
    opt[["series"]][[3L]][["data"]],
    function(x) x[[3L]] == 0,
    logical(1)
  )))
  expect_length(opt[["series"]][[4L]][["data"]], 0)
})

test_that("functional and config density controls and numeric conveniences round trip", {
  data <- list(A = c(1, 2, 4, 8), B = c(2, 3, 5, 9))
  for (setup in list(setup_DensityConfig, setup_HistogramConfig)) {
    cfg <- setup(
      x = c("A", "B"),
      bw = .7,
      n = 64,
      kernel = "triangular",
      adjust = 1.3
    )
    expect_equal(cfg@bandwidth, .7)
    expect_identical(cfg@bw, "nrd0")
    expect_identical(cfg@origin[["bandwidth"]], "user")
    for (complete in c(FALSE, TRUE)) {
      path <- tempfile(fileext = ".json")
      on.exit(unlink(path), add = TRUE)
      write_chart_config(cfg, path, complete = complete)
      expect_identical(
        to_list(compile(read_chart_config(path), data)),
        to_list(compile(cfg, data))
      )
    }
  }
  cfg <- setup_HistogramConfig(
    x = c("A", "B"),
    breaks = c(0, 1, 4, 10),
    density = TRUE,
    normalization = "density",
    bandwidth = .7,
    n = 64
  )
  expect_equal(cfg@bin_edges, c(0, 1, 4, 10))
  expect_identical(cfg@origin[["bin_edges"]], "user")
  expect_identical(
    to_list(compile(cfg, data)),
    to_list(histogram_option(
      data,
      breaks = c(0, 1, 4, 10),
      density = TRUE,
      normalization = "density",
      bandwidth = .7,
      n = 64
    ))
  )
  expect_identical(setup_HistogramConfig(breaks = 8)@bins, 8L)
  density_schema <- chart_schema(
    DensityConfig,
    "https://example.org/density",
    "Density",
    "Density"
  )[["properties"]]
  hist_schema <- chart_schema(
    HistogramConfig,
    "https://example.org/histogram",
    "Histogram",
    "Histogram"
  )[["properties"]]
  for (field in c(
    "n",
    "bw",
    "bandwidth",
    "kernel",
    "adjust",
    "na_rm"
  )) {
    expect_identical(hist_schema[[field]], density_schema[[field]])
  }
  expect_equal(hist_schema[["bin_edges"]][["minItems"]], 2L)
  # Bar opacity is the histogram's own: unset resolves from the data.
  expect_identical(
    hist_schema[["fill_alpha"]][["type"]],
    c("number", "null")
  )
})


test_that("histogram bars are more translucent where overlaid groups overlap", {
  payload <- function(...) {
    series <- to_list(histogram_option(...))[["series"]]
    bars <- Filter(function(s) identical(s[["type"]], "custom"), series)
    lapply(bars, `[[`, "itemPayload")
  }
  bar_alpha <- function(...) {
    unique(vapply(payload(...), function(p) p[["fillAlpha"]], 1))
  }
  two <- list(A = c(1, 2, 2, 3), B = c(2, 3, 3, 4))
  expect_identical(bar_alpha(c(1, 2, 2, 3)), 0.75)
  expect_identical(bar_alpha(two), 0.5)
  expect_identical(bar_alpha(two, bar_mode = "group"), 0.75)
  expect_identical(bar_alpha(two, bar_mode = "stack"), 0.75)
  expect_identical(bar_alpha(two, mode = "ridge"), 0.75)
  # An empty sample draws no bars, so it cannot overlap.
  expect_identical(
    bar_alpha(list(A = c(1, 2, 2, 3), B = numeric()), verbosity = 0L),
    0.75
  )
  expect_identical(bar_alpha(two, fill_alpha = 0.2), 0.2)
  expect_identical(bar_alpha(c(1, 2, 2, 3), fill_alpha = 1), 1)
  expect_error(setup_HistogramConfig(fill_alpha = 2))
})


test_that("histogram bar borders are drawn at border_alpha", {
  border_alpha <- function(...) {
    series <- to_list(histogram_option(...))[["series"]]
    bars <- Filter(function(s) identical(s[["type"]], "custom"), series)
    unique(vapply(bars, function(s) s[["itemPayload"]][["borderAlpha"]], 1))
  }
  two <- list(A = c(1, 2, 2, 3), B = c(2, 3, 3, 4))
  expect_identical(border_alpha(two), 1)
  expect_identical(border_alpha(two, border_alpha = 0), 0)
  expect_identical(border_alpha(two, bar_mode = "stack", border_alpha = .4), .4)
  expect_identical(setup_HistogramConfig()@border_alpha, 1)
  expect_error(setup_HistogramConfig(border_alpha = -1))
  expect_error(setup_HistogramConfig(border_alpha = NULL))
  cfg <- setup_HistogramConfig(x = "mpg", border_alpha = .3)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_chart_config(cfg, path)
  expect_identical(read_chart_config(path)@border_alpha, .3)
})


test_that("resolving a histogram config records the bar opacity drawn", {
  one <- resolve(setup_HistogramConfig(x = "mpg"), data = mtcars)
  expect_identical(one@fill_alpha, 0.75)
  grouped <- resolve(
    setup_HistogramConfig(x = "mpg", group = "am"),
    data = mtcars
  )
  expect_identical(grouped@fill_alpha, 0.5)
  dodged <- resolve(
    setup_HistogramConfig(x = "mpg", group = "am", bar_mode = "group"),
    data = mtcars
  )
  expect_identical(dodged@fill_alpha, 0.75)
  columns <- resolve(
    setup_HistogramConfig(x = c("mpg", "qsec")),
    data = mtcars
  )
  expect_identical(columns@fill_alpha, 0.5)
  explicit <- resolve(
    setup_HistogramConfig(x = "mpg", group = "am", fill_alpha = 0.3),
    data = mtcars
  )
  expect_identical(explicit@fill_alpha, 0.3)
  expect_null(resolve(setup_HistogramConfig())@fill_alpha)
})

test_that("distribution tooltips retain the shared numeric formatting", {
  for (option in list(density_option(1:5), histogram_option(1:5))) {
    tooltip <- to_list(option)[["tooltip"]]
    expect_identical(tooltip[["valueFormatter"]], number_value_formatter())
    expect_null(tooltip[["formatter"]])
  }
})

test_that("numeric histograms and density overlays export exact native geometry", {
  skip_if_no_node()
  charts <- list()
  for (theme in list(theme_light(), theme_dark())) {
    for (normalization in c(
      "count",
      "probability",
      "percent",
      "density",
      "count_density"
    )) {
      for (overlay in c(FALSE, TRUE)) {
        charts[[length(charts) + 1L]] <- list(
          option = to_list(histogram_option(
            list(A = c(0, .5, 1, 2, 3), B = c(1, 3)),
            bin_edges = c(0, 1, 2, 3),
            normalization = normalization,
            density = overlay,
            bandwidth = .4,
            n = 64,
            palette = c("#123456", "#abcdef")
          )),
          theme = to_list(theme)
        )
      }
    }
    for (values in list(
      list(A = c(0, 1, 4), B = c(1, 4)),
      list(A = 2),
      list(A = 1:4, Empty = numeric())
    )) {
      charts[[length(charts) + 1L]] <- list(
        option = to_list(histogram_option(
          values,
          bin_edges = c(0, 1, 4),
          normalization = "density",
          density = TRUE,
          bandwidth = .4,
          n = 64
        )),
        theme = to_list(theme)
      )
    }
    for (layout in c("overlay", "group", "stack")) {
      for (stat in c("count", "sum", "mean", "min", "max")) {
        charts[[length(charts) + 1L]] <- list(
          option = to_list(histogram_option(
            list(A = c(-2, -1, 0, 1, 2), B = c(-1, 1)),
            bin_edges = c(-2, 0, 2),
            bin_stat = stat,
            bar_mode = layout
          )),
          theme = to_list(theme)
        )
      }
    }
    charts[[length(charts) + 1L]] <- list(
      option = to_list(histogram_option(
        list(A = 1:5, B = 3:7),
        mode = "ridge",
        bins = 3
      )),
      theme = to_list(theme)
    )
    # A single numeric bin must remain a rectangle, not disappear via unboxing.
    charts[[length(charts) + 1L]] <- list(
      option = to_list(histogram_option(2, bin_edges = c(1, 3))),
      theme = to_list(theme)
    )
  }
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      # Apply the real export sanitizer: tooltip callbacks do not affect SVG.
      charts = lapply(charts, strip_js),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      renderers = system.file(
        "htmlwidgets/lib/draw/renderers.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    digits = NA
  )
  output <- system2(
    "node",
    c(
      shQuote(test_path("fixtures", "distribution_geometry.js")),
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})

test_that("ridge layouts retain common statistical scales and stable ordering", {
  samples <- list(Low = 1:8, High = 9:16, Empty = numeric())
  expect_named(distribution_order(samples, "mean"), c("High", "Low", "Empty"))
  for (builder in list(density_option, histogram_option)) {
    opt <- to_list(builder(samples, mode = "ridge", order = "median"))
    expect_false(opt[["legend"]][["show"]])
    expect_length(opt[["grid"]], 3)
    expect_equal(
      vapply(opt[["yAxis"]], `[[`, "", "name"),
      c("High", "Low", "Empty")
    )
    expect_length(unique(vapply(opt[["yAxis"]], `[[`, 0, "max")), 1)
    expect_length(unique(vapply(opt[["xAxis"]], `[[`, 0, "min")), 1)
    expect_equal(vapply(opt[["series"]], `[[`, 0L, "xAxisIndex"), 0:2)
  }
  expect_error(setup_DensityConfig(mode = "bad"))
  expect_error(setup_HistogramConfig(order = "bad"))
  config <- setup_DensityConfig(
    x = "Sepal.Length",
    group = "Species",
    mode = "ridge"
  )
  expect_length(to_list(compile(config, iris))[["grid"]], 3)
  expect_equal(
    read_chart_config(write_chart_config(
      config,
      tempfile(fileext = ".json")
    ))@mode,
    "ridge"
  )
})


test_that("histogram statistics retain counts and explicit normalization semantics", {
  x <- c(-2, -1, 0, 1, 2)
  expected <- list(
    sum = c(-3, 3),
    mean = c(-1, 1.5),
    min = c(-2, 1),
    max = c(0, 2)
  )
  for (stat in names(expected)) {
    opt <- to_list(histogram_option(
      x,
      bin_edges = c(-2, 0, 2),
      bin_stat = stat
    ))
    bins <- opt[["series"]][[1]][["data"]]
    expect_equal(vapply(bins, `[[`, numeric(1), 3L), expected[[stat]])
    expect_equal(vapply(bins, `[[`, numeric(1), 4L), c(3, 2))
  }
  expect_equal(
    histogram_stat(c(0, 1 + 1e-10, 2), c(0, 1, 2), "sum"),
    c(1 + 1e-10, 2)
  )
  expect_error(setup_HistogramConfig(
    bin_stat = "mean",
    normalization = "density"
  ))
  expect_error(setup_HistogramConfig(bin_stat = "sum", density = TRUE))
  expect_error(setup_HistogramConfig(bar_mode = "stack", density = TRUE))
  expect_error(setup_HistogramConfig(bar_mode = "group", mode = "ridge"))
  cfg <- setup_HistogramConfig(
    x = "value",
    group = "group",
    bar_mode = "stack",
    bin_stat = "sum",
    bin_edges = c(-2, 0, 2)
  )
  data <- data.frame(value = rep(x, 2), group = rep(c("A", "B"), each = 5))
  opt <- to_list(compile(cfg, data))
  expect_lt(opt[["yAxis"]][["min"]], -6)
  expect_gt(opt[["yAxis"]][["max"]], 6)
  expect_equal(
    to_list(compile(
      read_chart_config(write_chart_config(cfg, tempfile(fileext = ".json"))),
      data
    )),
    opt
  )
})
