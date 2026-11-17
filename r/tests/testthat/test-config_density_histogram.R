# test-config_density_histogram.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# DensityConfig and HistogramConfig: the "one bound column plus an optional
# grouping column" binding, which is the third distinct shape after scatter's
# two single columns and bar's multi-column series.

dh_data <- function() {
  set.seed(1)
  data.frame(v = rnorm(100), g = rep(c("a", "b"), 50))
}


# %% construction ----

test_that("each config carries its own type constant", {
  expect_identical(DensityConfig()@type, "density")
  expect_identical(HistogramConfig()@type, "histogram")
})

test_that("neither setup function has a mandatory argument", {
  for (fn in list(setup_DensityConfig, setup_HistogramConfig)) {
    no_default <- vapply(
      formals(fn),
      function(f) identical(f, quote(expr = )),
      logical(1L)
    )
    expect_false(any(no_default))
  }
})

test_that("semantics are validated", {
  expect_error(DensityConfig(n = 1L))
  expect_error(HistogramConfig(breaks = "bogus"))
  expect_identical(setup_HistogramConfig(breaks = "FD")@breaks, "FD")
})

test_that("setup coerces n to an integer", {
  expect_identical(setup_DensityConfig(n = 256)@n, 256L)
})


# %% compile ----

test_that("density compiles to the same option as the vector path", {
  d <- dh_data()
  expect_identical(
    raw_option(compile(setup_DensityConfig(x = "v"), data = d)),
    raw_option(density_option(x = d[["v"]], xlab = "v"))
  )
})

test_that("histogram compiles to the same option as the vector path", {
  d <- dh_data()
  expect_identical(
    raw_option(compile(setup_HistogramConfig(x = "v"), data = d)),
    raw_option(histogram_option(x = d[["v"]], xlab = "v"))
  )
})

test_that("a grouping column splits the chart into series", {
  d <- dh_data()
  opt <- raw_option(compile(
    setup_DensityConfig(x = "v", group = "g"),
    data = d
  ))
  expect_length(opt[["series"]], length(unique(d[["g"]])))
})

test_that("compile errors when x is unset", {
  expect_error(compile(setup_DensityConfig(), data = dh_data()), "needs `x`")
  expect_error(compile(setup_HistogramConfig(), data = dh_data()), "needs `x`")
})


# %% resolve ----

test_that("the x label comes from the bound column", {
  r <- resolve(setup_DensityConfig(x = "v"), data = dh_data())
  expect_identical(r@xlab, "v")
  expect_identical(r@origin[["xlab"]], "derived")
})

test_that("a histogram's y label says what a bar's height is", {
  d <- dh_data()
  label <- function(...) resolve(setup_HistogramConfig(x = "v", ...), d)@ylab
  expect_identical(label(), "Count")
  expect_identical(label(normalization = "percent"), "Percent")
  expect_identical(label(normalization = "count_density"), "Count density")
  expect_identical(label(bin_stat = "mean"), "Mean v")
  expect_identical(label(bin_stat = "sum"), "Sum of v")
  expect_identical(label(ylab = "Penguins"), "Penguins")
  expect_identical(
    resolve(setup_HistogramConfig(x = "v"), d)@origin[["ylab"]],
    "derived"
  )
  expect_identical(
    raw_option(draw_histogram(d[["v"]], normalization = "density"))[[
      "yAxis"
    ]][["name"]],
    "Density"
  )
  expect_identical(
    raw_option(compile(setup_HistogramConfig(x = "v", bin_stat = "max"), d))[[
      "yAxis"
    ]][["name"]],
    "Maximum v"
  )
})

test_that("a density's y label says the height is a density", {
  d <- dh_data()
  expect_identical(
    resolve(setup_DensityConfig(x = "v"), data = d)@ylab,
    "Density"
  )
  expect_identical(
    raw_option(compile(setup_DensityConfig(x = "v"), data = d))[["yAxis"]][[
      "name"
    ]],
    "Density"
  )
  expect_identical(
    raw_option(draw_density(d[["v"]]))[["yAxis"]][["name"]],
    "Density"
  )
  expect_null(raw_option(draw_density(d[["v"]], ylab = ""))[["yAxis"]][[
    "name"
  ]])
})


# %% round trip ----

test_that("both configs round-trip and draw the same", {
  d <- dh_data()
  configs <- list(
    setup_DensityConfig(x = "v", group = "g", bw = "SJ", n = 256L, title = "D"),
    setup_HistogramConfig(x = "v", group = "g", breaks = "FD", title = "H")
  )
  for (cfg in configs) {
    path <- tempfile(fileext = ".json")
    write_chart_config(cfg, path)
    expect_identical(read_chart_config(path), cfg)
    expect_identical(
      compile(read_chart_config(path), data = d),
      compile(cfg, data = d)
    )
  }
})

test_that("both types are registered and reachable by type", {
  for (type in c("density", "histogram")) {
    expect_true(type %in% names(chart_registry()))
  }
  path <- tempfile(fileext = ".json")
  writeLines('{"type": "histogram", "x": "v", "breaks": "Scott"}', path)
  cfg <- read_chart_config(path)
  expect_s3_class(cfg, "rtemis.draw::HistogramConfig")
  expect_identical(cfg@breaks, "Scott")
})
