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
    compile(setup_DensityConfig(x = "v"), data = d),
    density_option(x = d[["v"]], xlab = "v")
  )
})

test_that("histogram compiles to the same option as the vector path", {
  d <- dh_data()
  expect_identical(
    compile(setup_HistogramConfig(x = "v"), data = d),
    histogram_option(x = d[["v"]], xlab = "v")
  )
})

test_that("a grouping column splits the chart into series", {
  d <- dh_data()
  opt <- compile(setup_DensityConfig(x = "v", group = "g"), data = d)
  expect_length(opt@series, length(unique(d[["g"]])))
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

test_that("no y label is invented", {
  # The y axis shows an estimated density or a bin count, which has no name in
  # the data. A constant like "Density" would be a default, not a derivation --
  # and it would make the config path disagree with the vector path.
  d <- dh_data()
  for (cfg in list(
    setup_DensityConfig(x = "v"),
    setup_HistogramConfig(x = "v")
  )) {
    expect_null(resolve(cfg, data = d)@ylab)
  }
  expect_null(compile(setup_DensityConfig(x = "v"), data = d)@y_axis@name)
  expect_null(draw_density(d[["v"]])[["x"]][["option"]][["yAxis"]][["name"]])
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
