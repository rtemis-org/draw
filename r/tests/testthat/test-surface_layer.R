test_that("surface grids sort both axes with predictions and serialize holes", {
  z <- outer(c(2, 0, 1), c(3, 1, 2), function(x, y) 10 * x + y)
  layer <- SurfaceLayer(x = c(2, 0, 1), y = c(3, 1, 2), z = z)
  series <- to_list(layer)
  expect_equal(series[["dataShape"]], list(3L, 3L))
  expect_equal(series[["data"]][[1]], list(0, 1, 1))
  expect_equal(series[["data"]][[2]], list(1, 1, 11))
  expect_equal(series[["data"]][[4]], list(0, 2, 2))
  expect_equal(series[["shading"]], "color")
  expect_false(series[["wireframe"]][["show"]])
  expect_equal(strip_js(series), series)
  decoded <- jsonlite::fromJSON(
    jsonlite::toJSON(series, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_equal(decoded, series)
  z[1, 1] <- NA
  hole <- to_list(SurfaceLayer(x = c(2, 0, 1), y = c(3, 1, 2), z = z))
  expect_null(hole[["data"]][[9]][[3]])
  expect_error(SurfaceLayer(x = c(1, 1)))
  expect_error(SurfaceLayer(y = c(0, Inf)))
  expect_error(SurfaceLayer(z = matrix(1:6, 3)))
  expect_error(SurfaceLayer(z = matrix(NA_real_, 2, 2)))
  expect_error(SurfaceLayer(z = matrix(Inf, 2, 2)))
  expect_error(SurfaceLayer(opacity = 1.1))
  expect_error(SurfaceLayer(), "numeric z matrix")
})

test_that("surface layers expand ranges explicitly and survive panel composition", {
  plot <- draw_scatter3d(c(0, 1), c(0, 1), c(0, 1))
  layer <- SurfaceLayer(x = c(-2, 2), y = c(-1, 3), z = matrix(-3:0, 2))
  expect_error(draw_add_surface(plot, layer = layer, expand = FALSE), "within")
  near <- SurfaceLayer(x = c(0, 1 + 1e-9), z = matrix(0, 2, 2))
  expect_error(draw_add_surface(plot, layer = near, expand = FALSE), "within")
  inside <- SurfaceLayer(z = matrix(c(0, 1, 0, 1), 2))
  bounded <- draw_add_surface(plot, layer = inside, expand = FALSE)
  expect_equal(
    bounded[["x"]][["option"]][["xAxis3D"]],
    plot[["x"]][["option"]][["xAxis3D"]]
  )
  expect_error(draw_add_surface(plot, layer = layer, opacity = .5), "alone")
  expect_error(draw_add_surface(draw_scatter(1:3, 1:3), layer = layer), "3D")
  expect_error(
    draw_add_surface(plot, layer = layer, expand = NA),
    "TRUE or FALSE"
  )
  result <- draw_add_surface(plot, layer = layer)
  option <- result[["x"]][["option"]]
  expect_equal(
    c(option[["xAxis3D"]][["min"]], option[["xAxis3D"]][["max"]]),
    c(-2, 2)
  )
  expect_equal(
    c(option[["zAxis3D"]][["min"]], option[["zAxis3D"]][["max"]]),
    c(-3, 1)
  )
  expect_true(option[["legend"]][["show"]])
  expect_equal(option[["legend"]][["data"]], list("Observations", "Surface"))
  expect_length(option[["series"]], 2L)
  combined <- draw_add_surface(
    result,
    0:1,
    0:1,
    matrix(0, 2, 2),
    name = "Second"
  )
  expect_equal(
    combined[["x"]][["option"]][["legend"]][["data"]],
    list("Observations", "Surface", "Second")
  )
  expect_equal(plot[["x"]][["option"]][["xAxis3D"]][["min"]], 0)
  panel <- draw_panels(list(result, draw_bar(c(A = 1))))
  expect_true(
    "echarts-gl" %in% vapply(panel[["dependencies"]], `[[`, "", "name")
  )
})

test_that("3D paths preserve group, sorting, gaps and linked point colors", {
  x <- c(3, 1, NA, 4, 2)
  plot <- draw_scatter3d(
    x,
    c(30, 10, 0, 40, 20),
    c(6, 2, 0, 8, 4),
    mode = "both",
    order = "x"
  )
  series <- plot[["x"]][["option"]][["series"]]
  expect_equal(
    vapply(series, `[[`, "", "type"),
    c("scatter3D", "line3D", "line3D")
  )
  expect_equal(series[[2]][["data"]], list(list(1, 10, 2), list(3, 30, 6)))
  expect_equal(series[[3]][["data"]], list(list(2, 20, 4), list(4, 40, 8)))
  expect_length(unique(vapply(series, `[[`, "", "name")), 1L)
  grouped <- draw_scatter3d(
    1:6,
    1:6,
    1:6,
    group = rep(c("B", "A"), 3),
    mode = "lines"
  )
  expect_equal(
    vapply(grouped[["x"]][["option"]][["series"]], `[[`, "", "name"),
    c("B", "A")
  )
  expect_error(
    draw_scatter3d(c(1, NA, 2), 1:3, 1:3, mode = "lines"),
    "consecutive"
  )
  expect_error(setup_Scatter3DConfig(mode = "surface"))
  expect_error(setup_Scatter3DConfig(line_width = 0))
  cfg <- setup_Scatter3DConfig(
    x = "x",
    y = "y",
    z = "z",
    mode = "both",
    order = "x",
    line_width = 3
  )
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  write_chart_config(cfg, file)
  restored <- read_chart_config(file)
  expect_equal(restored@mode, "both")
  data <- data.frame(x = x, y = 1:5, z = 1:5)
  expect_equal(to_list(compile(cfg, data)), to_list(compile(restored, data)))
})

test_that("3D vector visibility splits crossing geometry and preserves topology", {
  skip_if_no_node()
  plot <- draw_scatter3d(
    c(0, 1, 0, 1),
    c(0, 0, 1, 1),
    c(0, 1, 1, 0),
    mode = "both"
  )
  plot <- draw_add_surface(plot, c(1, 0), c(1, 0), matrix(c(3, 2, 1, 0), 2))
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input))
  jsonlite::write_json(
    list(
      option = strip_js(plot[["x"]][["option"]]),
      module = system.file(
        "htmlwidgets/lib/draw/scatter3d.js",
        package = "rtemis.draw"
      )
    ),
    input,
    auto_unbox = TRUE,
    digits = NA
  )
  result <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "surface_geometry.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "3D surface visibility passed")
})
