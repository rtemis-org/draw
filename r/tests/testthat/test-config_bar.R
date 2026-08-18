# test-config_bar.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# BarConfig, the second chart type. Its job in the sequence is to test what
# ScatterConfig could not: a **multi-series** binding, where `y` names one
# column per series rather than exactly one column.

bar_data <- function() {
  data.frame(g = c("a", "b", "c"), u = c(3, 1, 4), v = c(1, 5, 9))
}


# %% construction ----

test_that("BarConfig carries its own type constant", {
  expect_identical(BarConfig()@type, "bar")
})

test_that("y accepts several column names", {
  cfg <- setup_BarConfig(x = "g", y = c("u", "v"))
  expect_identical(cfg@y, c("u", "v"))
})

test_that("setup_BarConfig has no mandatory argument", {
  no_default <- vapply(
    formals(setup_BarConfig),
    function(f) identical(f, quote(expr = )),
    logical(1L)
  )
  expect_false(any(no_default))
})

test_that("title is inherited from ChartConfig, not redeclared", {
  # It is the one property every chart type has, so it belongs on the base.
  expect_true("title" %in% names(BarConfig@properties))
  expect_true("title" %in% names(ChartConfig@properties))
  expect_identical(setup_BarConfig(title = "T")@title, "T")
})


# %% compile ----

test_that("one bound column draws one series and matches the vector path", {
  d <- bar_data()
  from_config <- compile(setup_BarConfig(x = "g", y = "u"), data = d)
  from_vectors <- bar_option(x = d[["g"]], y = d[["u"]], xlab = "g", ylab = "u")
  expect_identical(from_config, from_vectors)
})

test_that("several bound columns draw one series each, named by column", {
  d <- bar_data()
  opt <- compile(
    setup_BarConfig(x = "g", y = c("u", "v"), stack = TRUE),
    data = d
  )
  expect_length(opt@series, 2L)
  expect_identical(
    opt,
    bar_option(
      x = d[["g"]],
      y = list(u = d[["u"]], v = d[["v"]]),
      stack = TRUE,
      xlab = "g"
    )
  )
})

test_that("compile errors when x or y is unset", {
  expect_error(
    compile(setup_BarConfig(y = "u"), data = bar_data()),
    "needs both"
  )
  expect_error(
    compile(setup_BarConfig(x = "g"), data = bar_data()),
    "needs both"
  )
})


# %% resolve ----

test_that("the category label comes from x and the value label from y", {
  r <- resolve(setup_BarConfig(x = "g", y = "u"), data = bar_data())
  expect_identical(r@xlab, "g")
  expect_identical(r@ylab, "u")
})

test_that("several value columns derive no value label", {
  # No single name describes them, and the legend already names each -- the
  # same "no name, no label" rule as an unbound axis.
  r <- resolve(setup_BarConfig(x = "g", y = c("u", "v")), data = bar_data())
  expect_identical(r@xlab, "g")
  expect_null(r@ylab)
})

test_that("horizontal swaps which axis gets which label", {
  r <- resolve(
    setup_BarConfig(x = "g", y = "u", horizontal = TRUE),
    data = bar_data()
  )
  expect_identical(r@xlab, "u")
  expect_identical(r@ylab, "g")
})

test_that("resolve marks derived labels and leaves authored ones alone", {
  r <- resolve(
    setup_BarConfig(x = "g", y = "u", xlab = "Group"),
    data = bar_data()
  )
  expect_identical(r@xlab, "Group")
  expect_identical(r@origin[["xlab"]], "user")
  expect_identical(r@origin[["ylab"]], "derived")
})


# %% round trip ----

test_that("a multi-series config round-trips and draws the same", {
  d <- bar_data()
  cfg <- setup_BarConfig(
    x = "g",
    y = c("u", "v"),
    stack = TRUE,
    palette = c("#111111", "#222222"),
    title = "Two series"
  )
  path <- tempfile(fileext = ".json")
  write_chart_config(cfg, path)
  expect_identical(read_chart_config(path), cfg)
  expect_identical(
    compile(read_chart_config(path), data = d),
    compile(cfg, data = d)
  )
})

test_that("a multi-column binding stays a JSON array", {
  # A one-element `y` must not unbox to a bare string, or a reader would see a
  # different shape than the schema declares.
  path <- tempfile(fileext = ".json")
  write_chart_config(setup_BarConfig(x = "g", y = "u"), path)
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(is.list(parsed[["y"]]))
  expect_length(parsed[["y"]], 1L)
})


# %% registry and schema ----

test_that("bar is registered and reachable by type", {
  expect_true("bar" %in% names(chart_registry()))
  path <- tempfile(fileext = ".json")
  writeLines('{"type": "bar", "x": "g", "y": ["u"]}', path)
  expect_s3_class(read_chart_config(path), "rtemis.draw::BarConfig")
})

test_that("the bar schema declares y as an array of strings", {
  s <- chart_schema(
    BarConfig,
    id = "https://schema.rtemis.org/chart/bar/v1/schema.json",
    title = "rtemis BarConfig",
    description = "Bar chart."
  )
  y <- s[["properties"]][["y"]]
  expect_true("array" %in% y[["type"]])
  expect_identical(y[["items"]][["type"]], "string")
  expect_identical(s[["properties"]][["type"]][["const"]], "bar")
})
