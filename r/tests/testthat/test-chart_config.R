# test-chart_config.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The config layer: ChartConfig / ScatterConfig, setup_ScatterConfig(),
# compile(), and the draw() method that ties them together.

# %% ChartConfig ----

test_that("ChartConfig is abstract", {
  expect_error(ChartConfig(), "abstract")
})


# %% ScatterConfig construction ----

test_that("ScatterConfig carries a constant type discriminator", {
  cfg <- ScatterConfig()
  expect_s3_class(cfg, "rtemis.draw::ScatterConfig")
  expect_identical(cfg@type, "scatter")
})

test_that("the type discriminator is not settable", {
  cfg <- ScatterConfig()
  expect_error(cfg@type <- "bar")
})

test_that("ScatterConfig defaults match the documented values", {
  cfg <- ScatterConfig()
  expect_true(cfg@se)
  expect_identical(cfg@n_fit, 200L)
  expect_identical(cfg@fit_alpha, 0.25)
  # Everything else is unset, which is what lets an authored config be a subset.
  expect_null(cfg@x)
  expect_null(cfg@y)
  expect_null(cfg@fit)
  expect_null(cfg@palette)
  expect_null(cfg@dat_path)
  expect_null(cfg@margin_left)
})

test_that("ScatterConfig validates its properties", {
  expect_error(ScatterConfig(fit = "bogus"))
  expect_error(ScatterConfig(fit_alpha = 2))
  expect_error(ScatterConfig(fit_alpha = -1))
  expect_error(ScatterConfig(n_fit = 1L))
  expect_error(ScatterConfig(margin_left = -1L))
  # An axis limit is a pair, so a scalar is not one.
  expect_error(ScatterConfig(xlim = 0))
})

test_that("ScatterConfig accepts valid values", {
  cfg <- ScatterConfig(
    x = "wt",
    y = "mpg",
    fit = "gam",
    xlim = c(0, 10),
    palette = c("#111111", "#222222"),
    margin_left = 48L
  )
  expect_identical(cfg@fit, "gam")
  expect_identical(cfg@xlim, c(0, 10))
  expect_length(cfg@palette, 2L)
  expect_identical(cfg@margin_left, 48L)
})


# %% setup_ScatterConfig ----

test_that("setup_ScatterConfig has no mandatory argument", {
  # The premise the published schema rests on: a config can require nothing
  # precisely because no argument is obligatory.
  formals_ <- formals(setup_ScatterConfig)
  no_default <- vapply(
    formals_,
    function(f) identical(f, quote(expr = )),
    logical(1L)
  )
  expect_false(any(no_default))
  expect_s3_class(setup_ScatterConfig(), "rtemis.draw::ScatterConfig")
})

test_that("setup_ScatterConfig passes values through and coerces n_fit", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", n_fit = 50)
  expect_identical(cfg@x, "wt")
  expect_identical(cfg@y, "mpg")
  # `50` is a double at the call site; the property is an integer.
  expect_identical(cfg@n_fit, 50L)
})


# %% compile ----

test_that("compile() returns an EChartsOption", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  expect_s3_class(compile(cfg, data = mtcars), "rtemis.draw::EChartsOption")
})

test_that("compile() and draw_scatter() produce the same option", {
  # The one-implementation guarantee: both entry points go through
  # scatter_option(), so a config and the equivalent vector call agree exactly.
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm", title = "Cars")
  from_config <- draw(cfg, data = mtcars)[["x"]][["option"]]
  from_vectors <- draw_scatter(
    mtcars[["wt"]],
    mtcars[["mpg"]],
    fit = "glm",
    title = "Cars",
    xlab = "wt",
    ylab = "mpg"
  )[["x"]][["option"]]
  expect_identical(from_config, from_vectors)
})

test_that("axis labels default to the bound column names", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  opt <- compile(cfg, data = mtcars)
  expect_identical(opt@x_axis@name, "wt")
  expect_identical(opt@y_axis@name, "mpg")
})

test_that("explicit axis labels win over the column names", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", xlab = "Weight")
  opt <- compile(cfg, data = mtcars)
  expect_identical(opt@x_axis@name, "Weight")
})

test_that("compile() groups points when `group` names a column", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", group = "cyl")
  opt <- compile(cfg, data = mtcars)
  # One series per level of the grouping column.
  expect_length(opt@series, length(unique(mtcars[["cyl"]])))
})

test_that("compile() errors when x or y is unset", {
  expect_error(
    compile(setup_ScatterConfig(y = "mpg"), data = mtcars),
    "needs both"
  )
})

test_that("compile() names the missing column and what is available", {
  cfg <- setup_ScatterConfig(x = "nope", y = "mpg")
  expect_error(compile(cfg, data = mtcars), "nope")
  expect_error(compile(cfg, data = mtcars), "Available")
})


# %% data resolution ----

test_that("a config with no data and no dat_path errors informatively", {
  expect_error(
    compile(setup_ScatterConfig(x = "wt", y = "mpg")),
    "No data"
  )
})

test_that("dat_path is read when no data is supplied", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(mtcars, path, row.names = FALSE)
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", dat_path = path)
  expect_s3_class(compile(cfg), "rtemis.draw::EChartsOption")
})

test_that("a missing dat_path errors", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", dat_path = "no/such.csv")
  expect_error(compile(cfg), "does not exist")
})

test_that("supplied data takes precedence over dat_path", {
  # `dat_path` is never read when the caller hands over data, so a bad path is
  # harmless in that case.
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", dat_path = "no/such.csv")
  expect_s3_class(compile(cfg, data = mtcars), "rtemis.draw::EChartsOption")
})


# %% margins ----

test_that("per-side margin scalars assemble into a margins vector", {
  cfg <- setup_ScatterConfig(
    x = "wt",
    y = "mpg",
    margin_left = 60L,
    margin_top = 10L
  )
  opt <- compile(cfg, data = mtcars)
  expect_identical(opt@grid@left, 60)
  expect_identical(opt@grid@top, 10)
  # Sides not named stay unset, so the chart's own layout still applies to them.
  expect_null(opt@grid@right)
  expect_null(opt@grid@bottom)
})

test_that("unset margins fall back to the package default", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  opt <- compile(cfg, data = mtcars)
  expect_identical(opt@grid@left, DEFAULT_MARGINS[["left"]])
  expect_identical(opt@grid@right, DEFAULT_MARGINS[["right"]])
})


# %% draw ----

test_that("draw() on a config returns an htmlwidget", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  w <- draw(cfg, data = mtcars)
  expect_s3_class(w, "htmlwidget")
})

test_that("draw() passes render targets through without touching the config", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  w <- draw(cfg, data = mtcars, width = 640, height = 480)
  expect_identical(w[["width"]], 640)
  expect_identical(w[["height"]], 480)
  # The config is unchanged: render targets are arguments, not properties.
  expect_false("width" %in% names(S7::props(cfg)))
})


# %% resolve ----

test_that("resolve fills labels from the bound column names", {
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_identical(r@xlab, "wt")
  expect_identical(r@ylab, "mpg")
})

test_that("resolve fills axis limits from the data", {
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_length(r@xlim, 2L)
  expect_true(r@xlim[[1L]] <= min(mtcars[["wt"]]))
  expect_true(r@xlim[[2L]] >= max(mtcars[["wt"]]))
})

test_that("resolve marks what it filled as derived", {
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_identical(r@origin[["ylab"]], "derived")
  expect_identical(r@origin[["xlim"]], "derived")
  # What the author set keeps its origin.
  expect_identical(r@origin[["x"]], "user")
})

test_that("resolve never overwrites what the author set", {
  cfg <- setup_ScatterConfig(
    x = "wt",
    y = "mpg",
    xlab = "Weight",
    xlim = c(0, 6)
  )
  r <- resolve(cfg, data = mtcars)
  expect_identical(r@xlab, "Weight")
  expect_identical(r@xlim, c(0, 6))
  expect_identical(r@origin[["xlab"]], "user")
})

test_that("no names means no labels", {
  # Labels come from column names. A config that names nothing has nothing to
  # derive from, and inventing a name would be putting data shape into the
  # config.
  r <- resolve(setup_ScatterConfig(), data = mtcars)
  expect_null(r@xlab)
  expect_null(r@ylab)
  expect_null(r@xlim)
})

test_that("color is left for the interface to supply", {
  # The palette belongs to the interface; baking one into the document would
  # stop another interface applying its own.
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_null(r@palette)
  expect_identical(r@origin[["palette"]], "default")
})

test_that("resolve is idempotent", {
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_identical(resolve(r, data = mtcars), r)
})

test_that("a resolved config draws the same chart as the partial one", {
  # resolve() only makes explicit what the builder would have derived, so it
  # must not change what is rendered.
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm")
  expect_identical(
    compile(resolve(cfg, data = mtcars), data = mtcars),
    compile(cfg, data = mtcars)
  )
})

test_that("a resolved config round-trips through JSON and draws the same", {
  cfg <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  path <- tempfile(fileext = ".json")
  write_chart_config(cfg, path)
  expect_identical(
    compile(read_chart_config(path), data = mtcars),
    compile(cfg, data = mtcars)
  )
})


# %% pad ----

test_that("pad defaults to base R's 4% and widens the derived limits", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  expect_identical(cfg@pad, 0.04)
  r <- resolve(cfg, data = mtcars)
  span <- diff(range(mtcars[["wt"]]))
  expect_equal(r@xlim[[1L]], min(mtcars[["wt"]]) - 0.04 * span)
  expect_equal(r@xlim[[2L]], max(mtcars[["wt"]]) + 0.04 * span)
})

test_that("pad = 0 puts the limits exactly on the data range", {
  r <- resolve(setup_ScatterConfig(x = "wt", y = "mpg", pad = 0), data = mtcars)
  expect_equal(r@xlim, range(mtcars[["wt"]]))
})

test_that("a larger pad widens the limits", {
  wide <- resolve(
    setup_ScatterConfig(x = "wt", y = "mpg", pad = 0.2),
    data = mtcars
  )
  narrow <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_lt(wide@xlim[[1L]], narrow@xlim[[1L]])
  expect_gt(wide@xlim[[2L]], narrow@xlim[[2L]])
})

test_that("pad is ignored when limits are given", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", xlim = c(0, 6), pad = 0.5)
  expect_identical(resolve(cfg, data = mtcars)@xlim, c(0, 6))
})

test_that("a negative pad is rejected", {
  expect_error(ScatterConfig(pad = -0.1))
})

test_that("draw_scatter takes pad and it reaches the axis", {
  tight <- draw_scatter(mtcars[["wt"]], mtcars[["mpg"]], pad = 0)
  expect_equal(
    tight[["x"]][["option"]][["xAxis"]][["min"]],
    min(mtcars[["wt"]])
  )
})

test_that("pad round-trips through JSON", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", pad = 0.1)
  path <- tempfile(fileext = ".json")
  write_chart_config(cfg, path)
  expect_identical(read_chart_config(path)@pad, 0.1)
})


# %% animation ----

test_that("animation is left to ECharts by default", {
  # NULL means "say nothing", so ECharts applies its own default rather than
  # rtemis.draw pinning one.
  expect_null(draw_scatter(1:5, 1:5)[["x"]][["option"]][["animation"]])
  expect_null(
    draw(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)[["x"]][[
      "option"
    ]][["animation"]]
  )
})

test_that("animation can be turned off through every ECharts entry point", {
  off <- function(w) w[["x"]][["option"]][["animation"]]
  expect_false(off(draw(
    EChartsOption(series = ScatterSeries(data = list(c(1, 2)))),
    animation = FALSE
  )))
  expect_false(off(draw(
    setup_ScatterConfig(x = "wt", y = "mpg"),
    data = mtcars,
    animation = FALSE
  )))
})

test_that("animation is a render target, not a config property", {
  # Whether a chart animates depends on where it is drawn -- an IDE pane versus
  # a web canvas redrawing many points on every interaction -- so it belongs to
  # the interface and is never written into a document.
  expect_false("animation" %in% names(ScatterConfig@properties))
  path <- tempfile(fileext = ".json")
  write_chart_config(setup_ScatterConfig(x = "wt"), path)
  expect_false(grepl("animation", paste(readLines(path), collapse = "")))
})


# %% the compile() contract ----

# `compile()` materializes the data and resolves the config before dispatching,
# so that a method cannot forget either step. These test the invariant rather
# than any one method, because it is the invariant that keeps a chart type added
# later from quietly skipping resolution.

test_that("compile() hands its method a resolved config and materialized data", {
  # A throwaway chart type that records what the generic passed it, so this
  # tests the generic's contract rather than any one chart's method.
  Probe <- S7::new_class(
    "Probe",
    parent = ChartConfig,
    properties = list(
      type = prop_chart_type("probe"),
      xlab = prop_string(NULL, nullable = TRUE, description = "X label.")
    )
  )
  S7::method(resolve, Probe) <- function(config, data = NULL, ...) {
    config_derive(config, list(xlab = if (!is.null(data)) names(data)[[1L]]))
  }
  seen <- NULL
  S7::method(compile, Probe) <- function(config, data = NULL, ...) {
    seen <<- list(xlab = config@xlab, rows = nrow(data))
    NULL
  }

  path <- tempfile(fileext = ".csv")
  utils::write.csv(mtcars, path, row.names = FALSE)
  compile(Probe(dat_path = path, origin = c(xlab = "default")))

  # Resolved: the method saw a value only resolve() produces.
  expect_identical(seen[["xlab"]], names(mtcars)[[1L]])
  # Materialized: the method saw the table, not the path it came from.
  expect_identical(seen[["rows"]], nrow(mtcars))
})

test_that("every chart type resolves before it compiles", {
  # A config whose derived values are visible in the compiled option: if
  # resolution were skipped, the axis labels would be absent.
  config <- setup_ScatterConfig(x = "wt", y = "mpg")
  option <- compile(config, data = mtcars)
  expect_identical(option@x_axis@name, "wt")
  expect_identical(option@y_axis@name, "mpg")
})

test_that("compile() requires data", {
  expect_error(
    compile(setup_ScatterConfig(x = "wt", y = "mpg")),
    class = "rtemis_null_input"
  )
})


# %% resolve() without data ----

test_that("resolve() derives what it can with no data at all", {
  # Labels come from the column names, which the config already states; limits
  # come from the values, which it does not have. Neither is an error.
  config <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"))
  expect_identical(config@xlab, "wt")
  expect_identical(config@ylab, "mpg")
  expect_null(config@xlim)
  expect_null(config@ylim)
})

test_that("resolve() is idempotent", {
  # compile() resolves up front and a shared builder may resolve again, so
  # resolving twice has to be the same as resolving once.
  once <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  twice <- resolve(once, data = mtcars)
  expect_identical(S7::props(once), S7::props(twice))
})

test_that("resolve() never overwrites a value the author set", {
  config <- resolve(
    setup_ScatterConfig(x = "wt", y = "mpg", xlab = "Weight", xlim = c(0, 10)),
    data = mtcars
  )
  expect_identical(config@xlab, "Weight")
  expect_identical(config@xlim, c(0, 10))
  expect_identical(config@origin[["xlab"]], "user")
  expect_identical(config@origin[["ylab"]], "derived")
})

test_that("resolve() marks derived values as derived", {
  config <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
  expect_identical(config@origin[["xlim"]], "derived")
  expect_identical(config@origin[["x"]], "user")
  expect_identical(config@origin[["se"]], "default")
})

test_that("an origin map admits only the three documented values", {
  expect_error(
    ScatterConfig(origin = c(x = "guessed")),
    "must be one of 'user', 'default', 'derived'"
  )
  expect_no_error(ScatterConfig(origin = c(x = "derived")))
})


# %% square and equal_axes on a config ----

test_that("a config resolves the common interval into the document", {
  # The reconciled limits are part of the document, not something the builder
  # applies on the way past: reading the document back has to draw the same
  # chart, which it cannot do if the interval lives only in the renderer.
  data <- data.frame(true = c(1, 2, 3, 4), predicted = c(1.1, 1.9, 3.4, 3.7))
  config <- resolve(
    setup_ScatterConfig(
      x = "true",
      y = "predicted",
      square = TRUE,
      equal_axes = TRUE
    ),
    data = data
  )
  expect_identical(config@xlim, config@ylim)
  expect_identical(config@origin[["xlim"]], "derived")
})

test_that("a config without the flags derives per-axis limits as before", {
  data <- data.frame(true = c(1, 2, 3, 4), predicted = c(1.1, 1.9, 3.4, 3.7))
  config <- resolve(
    setup_ScatterConfig(x = "true", y = "predicted"),
    data = data
  )
  expect_false(identical(config@xlim, config@ylim))
})

test_that("drawing a config carries its aspect hint to the widget", {
  data <- data.frame(true = c(1, 2, 3, 4), predicted = c(1.1, 1.9, 3.4, 3.7))
  widget <- draw(
    setup_ScatterConfig(
      x = "true",
      y = "predicted",
      square = TRUE,
      equal_axes = TRUE
    ),
    data = data
  )
  expect_equal(widget[["x"]][["aspect"]][["ratio"]], 1)
})

test_that("a chart type with no render hints sends none", {
  widget <- draw(
    setup_BarConfig(x = "k", y = "v"),
    data = data.frame(k = c("a", "b"), v = c(1, 2))
  )
  expect_null(widget[["x"]][["aspect"]])
})

test_that("square and equal_axes survive a round trip through JSON", {
  path <- tempfile(fileext = ".json")
  write_chart_config(
    setup_ScatterConfig(x = "a", y = "b", square = TRUE, equal_axes = TRUE),
    path
  )
  config <- read_chart_config(path)
  expect_true(config@square)
  expect_true(config@equal_axes)
  unlink(path)
})

test_that("a config on a non-ECharts backend still draws", {
  # `meta` is the ECharts binding's channel and the Sigma method has no formal
  # for it, so an empty one must not be forwarded.
  m <- matrix(
    c(0, 1, 1, 1, 0, 1, 1, 1, 0),
    nrow = 3L,
    dimnames = list(letters[1:3], letters[1:3])
  )
  expect_s3_class(draw(setup_NetworkConfig(), data = m), "htmlwidget")
  expect_s3_class(
    draw(
      setup_ChoroplethConfig(location = "st", value = "v"),
      data = data.frame(st = c("CA", "TX"), v = c(1, 2))
    ),
    "htmlwidget"
  )
})


# %% provenance shape ----

test_that("an origin naming a property the chart lacks is rejected", {
  # The published schema states `origin` as a closed object, so the class holds
  # to the same shape rather than letting a stray key survive to the far end of
  # a round trip.
  expect_error(
    ScatterConfig(origin = c(nope = "user")),
    "does not have|not have"
  )
})

test_that("a writer with a key beyond name and version is rejected", {
  expect_error(
    ScatterConfig(writer = c(name = "a", version = "1", extra = "x")),
    "name` and `version"
  )
  expect_no_error(ScatterConfig(writer = c(name = "a", version = "1")))
})
