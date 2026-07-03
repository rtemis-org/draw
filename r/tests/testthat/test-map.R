# test-map.R
# Tests for MapRow, MapModel, MapLibreOption, the model builder
# (map_from_data_frame), the geometry loader, draw_map(), draw_choropleth(),
# and draw() dispatch to the MapLibre backend.

# -- MapRow ---------------------------------------------------------------------

test_that("MapRow serializes location/value and optional extras", {
  r <- MapRow(location = "USA", value = 25.5, extras = list(region = "NA"))
  expect_true(S7::S7_inherits(r, MapRow))
  out <- to_list(r)
  expect_equal(out[["location"]], "USA")
  expect_equal(out[["value"]], 25.5)
  expect_equal(out[["extras"]][["region"]], "NA")
})

test_that("MapRow drops extras when NULL", {
  out <- to_list(MapRow(location = "CAN", value = 2.1))
  expect_equal(names(out), c("location", "value"))
})

test_that("MapRow rejects a non-scalar value and unnamed extras", {
  expect_error(MapRow(location = "USA", value = c(1, 2)))
  expect_error(MapRow(location = "USA", value = 1, extras = list(1, 2)))
})

# -- MapModel -------------------------------------------------------------------

test_that("MapModel assembles rows + resolution + labels", {
  m <- MapModel(
    rows = list(
      MapRow(location = "USA", value = 1),
      MapRow(location = "CAN", value = 2)
    ),
    resolution = "country",
    value_label = "GDP",
    tooltip_fields = "region"
  )
  out <- to_list(m)
  expect_named(out, c("rows", "resolution", "valueLabel", "tooltipFields"))
  expect_length(out[["rows"]], 2L)
  expect_null(names(out[["rows"]]))
  expect_equal(out[["valueLabel"]], "GDP")
  # tooltipFields must serialize as a JSON array (unnamed list)
  expect_type(out[["tooltipFields"]], "list")
  expect_null(names(out[["tooltipFields"]]))
})

test_that("MapModel tooltipFields is an empty array by default", {
  out <- to_list(MapModel(rows = list(MapRow(location = "USA", value = 1))))
  expect_length(out[["tooltipFields"]], 0L)
})

test_that("MapModel rejects an invalid resolution and non-MapRow rows", {
  expect_error(MapModel(resolution = "planet"))
  expect_error(MapModel(rows = list(list(location = "USA", value = 1))))
})

# -- MapLibreOption -------------------------------------------------------------

test_that("MapLibreOption applies sensible style defaults", {
  m <- MapModel(rows = list(MapRow(location = "USA", value = 1)))
  opt <- MapLibreOption(model = m)
  expect_true(S7::S7_inherits(opt, MapLibreOption))
  expect_equal(opt@classification, "quantile")
  expect_equal(opt@color_scheme, "blues")
  expect_equal(opt@num_classes, 5)
  expect_equal(opt@opacity, 1)
  expect_true(opt@show_boundaries)
  expect_equal(opt@legend_position, "bottom-right")
})

test_that("MapLibreOption to_list produces the {model, style} payload shape", {
  m <- MapModel(rows = list(MapRow(location = "USA", value = 1)))
  out <- to_list(MapLibreOption(
    model = m,
    classification = "jenks",
    color_scheme = "viridis",
    num_classes = 7
  ))
  expect_named(out, c("model", "style"))
  expect_equal(out[["style"]][["classification"]], "jenks")
  expect_equal(out[["style"]][["colorScheme"]], "viridis")
  expect_equal(out[["style"]][["numClasses"]], 7)
  expect_true(all(
    c("showBoundaries", "outlineWidth", "legendPosition", "reportPosition") %in%
      names(out[["style"]])
  ))
  expect_false("title" %in% names(out))
})

test_that("MapLibreOption validates enums and ranges", {
  m <- MapModel(rows = list(MapRow(location = "USA", value = 1)))
  expect_error(MapLibreOption(model = m, classification = "kmeans"))
  expect_error(MapLibreOption(model = m, color_scheme = "rainbow"))
  expect_error(MapLibreOption(model = m, num_classes = 1))
  expect_error(MapLibreOption(model = m, num_classes = 99))
  expect_error(MapLibreOption(model = m, legend_position = "middle"))
})

test_that("MapLibreOption accepts a plain list model but rejects one without rows", {
  expect_no_error(MapLibreOption(
    model = list(rows = list(), resolution = "country")
  ))
  expect_error(MapLibreOption(model = list(resolution = "country")), "rows")
  expect_error(MapLibreOption(model = 42), "MapModel")
})

# -- map_from_data_frame --------------------------------------------------------

test_that("map_from_data_frame builds rows with extras from tooltip columns", {
  df <- data.frame(
    iso = c("USA", "CAN"),
    gdp = c(25.5, 2.1),
    region = c("NA", "NA"),
    stringsAsFactors = FALSE
  )
  m <- map_from_data_frame(
    df,
    location = "iso",
    value = "gdp",
    resolution = "country",
    tooltip = "region"
  )
  out <- to_list(m)
  expect_length(out[["rows"]], 2L)
  expect_equal(out[["rows"]][[1L]][["location"]], "USA")
  expect_equal(out[["rows"]][[1L]][["extras"]][["region"]], "NA")
  expect_equal(out[["tooltipFields"]][[1L]], "region")
  # value_label defaults to the value column name
  expect_equal(out[["valueLabel"]], "gdp")
})

test_that("map_from_data_frame errors on missing columns", {
  df <- data.frame(iso = "USA", gdp = 1)
  expect_error(
    map_from_data_frame(df, "iso", "nope", "country"),
    "not found"
  )
})

# -- geometry loader ------------------------------------------------------------

test_that("load_map_geometry returns embedded topojson + metadata per resolution", {
  g <- load_map_geometry("country")
  expect_equal(g[["object"]], "countries")
  expect_equal(g[["center"]], c(0, 20))
  expect_gt(nchar(g[["topojson"]]), 1000)
  # the string is valid JSON describing a TopoJSON
  topo <- jsonlite::fromJSON(g[["topojson"]], simplifyVector = FALSE)
  expect_equal(topo[["type"]], "Topology")
  expect_true("countries" %in% names(topo[["objects"]]))

  expect_equal(load_map_geometry("state")[["object"]], "states")
  expect_equal(load_map_geometry("county")[["object"]], "counties")
})

# -- draw() dispatch + widget ---------------------------------------------------

test_that("draw() dispatches a MapLibreOption to the rtemis-map backend", {
  m <- MapModel(rows = list(MapRow(location = "USA", value = 1)))
  w <- draw(MapLibreOption(model = m))
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "rtemis.draw")
  # Map payload shape: model/style/geo, not the ECharts shape (option)
  expect_false(is.null(w$x$model))
  expect_false(is.null(w$x$style))
  expect_false(is.null(w$x$geo))
  expect_null(w$x$option)
  # geometry embedded for the model's resolution
  expect_equal(w$x$geo$object, "countries")
  expect_gt(nchar(w$x$geo$topojson), 1000)
  # theme resolved uniformly by draw()
  expect_true(w$x$autoTheme)
})

test_that("draw_choropleth returns a configured htmlwidget", {
  df <- data.frame(
    iso = c("USA", "CAN", "MEX", "BRA", "FRA"),
    gdp = c(25.5, 2.1, 1.4, 1.9, 2.9),
    region = c("N", "N", "N", "S", "E"),
    stringsAsFactors = FALSE
  )
  w <- draw_choropleth(
    df,
    location = "iso",
    value = "gdp",
    tooltip = "region",
    color_scheme = "viridis",
    classification = "jenks"
  )
  expect_s3_class(w, "htmlwidget")
  expect_length(w$x$model$rows, 5L)
  expect_equal(w$x$style$colorScheme, "viridis")
  expect_equal(w$x$style$classification, "jenks")
  expect_equal(w$x$model$tooltipFields[[1L]], "region")
})

test_that("draw_choropleth embeds the US geometry for state / county resolutions", {
  ws <- draw_choropleth(
    data.frame(st = c("CA", "TX"), v = c(1, 2)),
    location = "st",
    value = "v",
    resolution = "state"
  )
  expect_equal(ws$x$geo$object, "states")

  wc <- draw_choropleth(
    data.frame(fips = "06075", v = 1),
    location = "fips",
    value = "v",
    resolution = "county"
  )
  expect_equal(wc$x$geo$object, "counties")
})

test_that("draw_choropleth honours an explicit theme and NA (no theme)", {
  df <- data.frame(iso = "USA", gdp = 1)
  wt <- draw_choropleth(df, "iso", "gdp", theme = theme_dark())
  expect_false(is.null(wt$x$theme))
  expect_null(wt$x$autoTheme)

  wna <- draw_choropleth(df, "iso", "gdp", theme = NA)
  expect_null(wna$x$theme)
  expect_null(wna$x$autoTheme)
})

test_that("draw_choropleth warns and ignores filename (no static export yet)", {
  df <- data.frame(iso = "USA", gdp = 1)
  expect_message(
    draw_choropleth(df, "iso", "gdp", filename = "map.png"),
    "not yet supported"
  )
})

test_that("draw_choropleth rejects an invalid resolution / scheme", {
  df <- data.frame(iso = "USA", gdp = 1)
  expect_error(draw_choropleth(df, "iso", "gdp", resolution = "planet"))
  expect_error(draw_choropleth(df, "iso", "gdp", color_scheme = "rainbow"))
})
