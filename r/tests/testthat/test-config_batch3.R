# test-config_batch3.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# NetworkConfig and ChoroplethConfig: the two configs that compile to a backend
# other than ECharts, and the last binding shape -- a chart whose bound data is
# a *structure* (an adjacency matrix) rather than columns in a table.
#
# There is no GraphConfig or MapConfig: `draw_graph()` and `draw_map()` take
# already-built models, which are rendering IR. The chart types are the network
# and the choropleth, and building the model is what their compile() does.

adj <- function() {
  matrix(
    c(0, 1, 1, 1, 0, 1, 1, 1, 0),
    nrow = 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
}
states <- function() {
  data.frame(st = c("CA", "TX", "NY"), v = c(1, 2, 3), stringsAsFactors = FALSE)
}


# %% construction ----

test_that("each config carries its own type constant", {
  expect_identical(NetworkConfig()@type, "network")
  expect_identical(ChoroplethConfig()@type, "choropleth")
})

test_that("neither setup function has a mandatory argument", {
  for (fn in list(setup_NetworkConfig, setup_ChoroplethConfig)) {
    no_default <- vapply(
      formals(fn),
      function(f) identical(f, quote(expr = )),
      logical(1L)
    )
    expect_false(any(no_default))
  }
})

test_that("a network config declares no column bindings", {
  # Its bound data is the adjacency matrix itself, so there is nothing to name.
  props <- names(NetworkConfig@properties)
  expect_false(any(c("x", "y", "group") %in% props))
})


# %% compile ----

test_that("network compiles to the same Sigma option as the vector path", {
  expect_identical(
    compile(setup_NetworkConfig(), data = adj()),
    graph_option(model = graph_from_matrix(adj()))
  )
})

test_that("network compiles to a SigmaOption, not an EChartsOption", {
  # compile() is a generic precisely so the backend can be a fact about the
  # chart type rather than about the config layer.
  expect_s3_class(
    compile(setup_NetworkConfig(), data = adj()),
    "rtemis.draw::SigmaOption"
  )
  expect_s3_class(
    compile(
      setup_ChoroplethConfig(
        location = "st",
        value = "v",
        resolution = "state"
      ),
      data = states()
    ),
    "rtemis.draw::MapLibreOption"
  )
})

test_that("choropleth compiles to the same MapLibre option as the vector path", {
  expect_identical(
    compile(
      setup_ChoroplethConfig(
        location = "st",
        value = "v",
        resolution = "state"
      ),
      data = states()
    ),
    map_option(
      model = map_from_data_frame(states(), "st", "v", "state", NULL, "v")
    )
  )
})

test_that("network accepts an edge list as well as a matrix", {
  edges <- data.frame(
    source = c("a", "b"),
    target = c("b", "c"),
    stringsAsFactors = FALSE
  )
  expect_s3_class(
    compile(setup_NetworkConfig(), data = edges),
    "rtemis.draw::SigmaOption"
  )
})

test_that("network takes an optional node table alongside the matrix", {
  nodes <- data.frame(id = letters[1:3], group = c("g1", "g1", "g2"))
  edges <- data.frame(source = c("a", "b"), target = c("b", "c"))
  opt <- compile(
    setup_NetworkConfig(),
    data = list(adjacency = edges, nodes = nodes)
  )
  expect_s3_class(opt, "rtemis.draw::SigmaOption")
})

test_that("network rejects data that is neither a matrix nor an edge list", {
  expect_error(
    compile(setup_NetworkConfig(), data = 1:5),
    "square numeric matrix"
  )
})

test_that("choropleth errors when a binding is unset", {
  expect_error(
    compile(setup_ChoroplethConfig(value = "v"), data = states()),
    "needs both"
  )
})


# %% resolve ----

test_that("the choropleth value label is derived from the bound column", {
  r <- resolve(
    setup_ChoroplethConfig(location = "st", value = "v"),
    data = states()
  )
  expect_identical(r@value_label, "v")
  expect_identical(r@origin[["value_label"]], "derived")
})

test_that("an authored value label is kept", {
  r <- resolve(
    setup_ChoroplethConfig(location = "st", value = "v", value_label = "Score"),
    data = states()
  )
  expect_identical(r@value_label, "Score")
  expect_identical(r@origin[["value_label"]], "user")
})


# %% round trip ----

test_that("both configs round-trip and draw the same", {
  cases <- list(
    list(
      cfg = setup_NetworkConfig(layout = "force", color_by_group = TRUE),
      data = adj()
    ),
    list(
      cfg = setup_ChoroplethConfig(
        location = "st",
        value = "v",
        resolution = "state",
        colormap = "viridis"
      ),
      data = states()
    )
  )
  for (case in cases) {
    path <- tempfile(fileext = ".json")
    write_chart_config(case[["cfg"]], path)
    back <- read_chart_config(path)
    expect_identical(back, case[["cfg"]])
    expect_identical(
      compile(back, data = case[["data"]]),
      compile(case[["cfg"]], data = case[["data"]])
    )
  }
})


# %% declared enums ----

test_that("map enums are rejected at construction, not at compile time", {
  # The option class validates these deep inside compile(); declaring them on
  # the config means a bad value fails where it was set.
  expect_error(ChoroplethConfig(colormap = "reds"))
  expect_error(ChoroplethConfig(classification = "bogus"))
  expect_error(ChoroplethConfig(legend_position = "middle"))
  expect_identical(setup_ChoroplethConfig(colormap = "magma")@colormap, "magma")
})
