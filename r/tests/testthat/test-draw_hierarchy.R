# test-draw_hierarchy.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

places <- data.frame(
  region = c("Europe", "Europe", "Asia", "Europe"),
  country = c("France", "Spain", "Japan", "France"),
  n = c(3, 2, 5, 1)
)

# -- TreemapConfig -------------------------------------------------------------

test_that("setup_TreemapConfig sets defaults and validates", {
  config <- setup_TreemapConfig(levels = c("region", "country"), value = "n")
  expect_true(S7::S7_inherits(config, TreemapConfig))
  expect_equal(config@type, "treemap")
  expect_equal(config@levels, c("region", "country"))
  expect_true(config@show_labels)
  expect_equal(config@border_width, 1)
  expect_error(setup_TreemapConfig(border_width = -1))
  expect_error(setup_TreemapConfig(show_labels = "yes"))
})

test_that("a TreemapConfig compiles to a treemap build request", {
  config <- setup_TreemapConfig(levels = c("region", "country"), value = "n")
  request <- compile(config, data = places)
  expect_true(S7::S7_inherits(request, BuildRequest))
  expect_equal(request@type, "treemap")
  data <- jsonlite::fromJSON(request@data)
  expect_setequal(names(data[["columns"]]), c("region", "country", "n"))
  expect_equal(request@config[["levels"]], c("region", "country"))
  expect_error(
    compile(setup_TreemapConfig(), data = places),
    "needs `levels`",
    class = "rtemis_null_input"
  )
  expect_error(
    compile(setup_TreemapConfig(levels = "continent"), data = places),
    "not in the data",
    class = "rtemis_value_error"
  )
})

test_that("draw_treemap nests and sums the rows in the built option", {
  w <- draw_treemap(places, levels = c("region", "country"), value = "n")
  expect_s3_class(w, "htmlwidget")
  series <- raw_option(w)[["series"]][[1L]]
  expect_equal(series[["type"]], "treemap")
  europe <- series[["data"]][[1L]]
  expect_equal(europe[["name"]], "Europe")
  expect_equal(
    vapply(europe[["children"]], `[[`, numeric(1L), "value"),
    c(4, 2)
  )
  counts <- raw_option(draw_treemap(places, levels = "region"))
  expect_equal(
    vapply(counts[["series"]][[1L]][["data"]], `[[`, numeric(1L), "value"),
    c(3, 1)
  )
})

test_that("a treemap row ending at an inner node stops the build", {
  ragged <- data.frame(
    region = c("Europe", "Europe"),
    country = c("France", NA),
    n = c(1, 2)
  )
  w <- draw_treemap(ragged, levels = c("region", "country"), value = "n")
  skip_if_no_node()
  expect_error(built_option(w), "Node 'Europe' has children")
})

# -- SunburstConfig ------------------------------------------------------------

test_that("setup_SunburstConfig sets defaults and validates", {
  config <- setup_SunburstConfig(levels = "region")
  expect_equal(config@type, "sunburst")
  expect_identical(config@label_depth, 2L)
  expect_equal(config@label_rotate, "radial")
  expect_equal(c(config@inner_radius, config@radius), c("18%", "85%"))
  expect_error(setup_SunburstConfig(label_rotate = "diagonal"))
  expect_error(setup_SunburstConfig(min_angle = 400))
  expect_error(setup_SunburstConfig(label_depth = -1L))
})

test_that("draw_sunburst draws one ring per level with labels to the depth", {
  w <- draw_sunburst(
    places,
    levels = c("region", "country"),
    value = "n",
    label_depth = 1L,
    label_rotate = "tangential"
  )
  series <- raw_option(w)[["series"]][[1L]]
  expect_equal(series[["type"]], "sunburst")
  expect_equal(unlist(series[["radius"]]), c("18%", "85%"))
  shown <- vapply(
    series[["levels"]],
    function(level) level[["label"]][["show"]],
    logical(1L)
  )
  expect_equal(shown, c(FALSE, TRUE, FALSE))
  expect_equal(series[["label"]][["rotate"]], "tangential")
})

# -- RadarConfig ---------------------------------------------------------------

teams <- data.frame(
  team = c("b", "a", "b"),
  speed = c(4, 2, 6),
  power = c(1, 3, 5)
)

test_that("setup_RadarConfig sets defaults and validates", {
  config <- setup_RadarConfig(indicators = c("speed", "power"), group = "team")
  expect_equal(config@type, "radar")
  expect_equal(config@aggregate, "none")
  expect_equal(config@area_opacity, 0.12)
  expect_error(setup_RadarConfig(aggregate = "max"))
  expect_error(setup_RadarConfig(area_opacity = 2))
  expect_error(
    compile(setup_RadarConfig(indicators = "speed"), data = teams),
    "at least two `indicators`",
    class = "rtemis_null_input"
  )
  expect_error(
    compile(
      setup_RadarConfig(indicators = c("speed", "range")),
      data = teams
    ),
    "not in the data"
  )
})

test_that("draw_radar combines a group's rows by the aggregate", {
  w <- draw_radar(
    teams,
    indicators = c("speed", "power"),
    group = "team",
    aggregate = "mean"
  )
  option <- raw_option(w)
  data <- option[["series"]][[1L]][["data"]]
  expect_equal(vapply(data, `[[`, character(1L), "name"), c("b", "a"))
  expect_equal(unlist(data[[1L]][["value"]]), c(5, 3))
  expect_equal(unlist(option[["legend"]][["data"]]), c("b", "a"))
  w_none <- draw_radar(teams, indicators = c("speed", "power"), group = "team")
  skip_if_no_node()
  expect_error(built_option(w_none), "both belong to series 'b'")
})

# -- FunnelConfig --------------------------------------------------------------

test_that("setup_FunnelConfig sets defaults and validates", {
  config <- setup_FunnelConfig(values = "n", labels = "step")
  expect_equal(config@type, "funnel")
  expect_equal(config@sort, "descending")
  expect_equal(config@label_format, "name")
  expect_error(setup_FunnelConfig(sort = "random"))
  expect_error(setup_FunnelConfig(label_format = "percent"))
  expect_error(
    compile(setup_FunnelConfig(values = "n"), data = places),
    "needs both `values` and `labels`",
    class = "rtemis_null_input"
  )
})

test_that("draw_funnel sums the stages of a label", {
  w <- draw_funnel(
    values = c(60, 30, 40, 10),
    labels = c("Visit", "Cart", "Visit", "Buy"),
    sort = "none",
    label_format = "name_value"
  )
  series <- raw_option(w)[["series"]][[1L]]
  expect_equal(series[["type"]], "funnel")
  expect_equal(series[["sort"]], "none")
  expect_equal(
    vapply(series[["data"]], `[[`, numeric(1L), "value"),
    c(100, 30, 10)
  )
  expect_error(draw_funnel(c(1, -1)), "finite nonnegative values")
})

# -- registry and export -------------------------------------------------------

test_that("the four chart types are registered and export to SVG", {
  expect_true(all(
    c("treemap", "sunburst", "radar", "funnel") %in% names(chart_registry())
  ))
  skip_if_no_node()
  widgets <- list(
    draw_treemap(places, levels = c("region", "country"), value = "n"),
    draw_sunburst(places, levels = c("region", "country"), value = "n"),
    draw_radar(teams, indicators = c("speed", "power"), group = "team", aggregate = "sum"),
    draw_funnel(c(100, 30, 10), labels = c("Visit", "Cart", "Buy"))
  )
  for (w in widgets) {
    path <- tempfile(fileext = ".svg")
    save_drawing(w, path, width = 500, height = 400)
    svg <- paste(readLines(path, warn = FALSE), collapse = "")
    expect_match(svg, "<svg", fixed = TRUE)
    expect_false(grepl("NaN", svg, fixed = TRUE))
    unlink(path)
  }
})
