test_that("3D coordinates, bounds, groups and camera have a portable contract", {
  cfg <- setup_Scatter3DConfig(
    x = "Sepal.Length",
    y = "Sepal.Width",
    z = "Petal.Length",
    group = "Species"
  )
  opt <- to_list(compile(cfg, iris))
  expect_equal(
    opt[["series"]][[1]][["data"]][[1]],
    as.list(unname(as.numeric(iris[1, 1:3])))
  )
  expect_equal(opt[["grid3D"]][["viewControl"]][["projection"]], "perspective")
  expect_equal(opt[["xAxis3D"]][["min"]], min(iris[[1]]))
  expect_equal(opt[["xAxis3D"]][["max"]], max(iris[[1]]))
  expect_null(to_list(EChartsOption())[["grid3D"]])
  expect_error(setup_Scatter3DConfig(alpha = 91))
  expect_error(setup_Scatter3DConfig(point_size = 0))
  expect_error(draw_scatter3d(1:3, 1:2, 1:3))
  expect_error(draw_scatter3d(c(1, Inf), 1:2, 1:2))
  w <- draw_scatter3d(c(1, NA, 3), c(1, 2, 3), c(1, 2, 3))
  expect_length(w[["x"]][["option"]][["series"]][[1]][["data"]], 2)
  expect_true(any(vapply(
    w[["dependencies"]],
    function(d) d[["name"]] == "echarts-gl",
    logical(1)
  )))
  expect_length(
    draw_scatter3d(1, 2, 3)[["x"]][["option"]][["series"]][[1]][["data"]],
    1
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(cfg, path)
  expect_equal(to_list(compile(read_chart_config(path), iris)), opt)
})


test_that("3D matrix labels and panel dependencies survive composition", {
  w <- draw_scatter3d(iris[1:3])
  expect_equal(w[["x"]][["option"]][["zAxis3D"]][["name"]], "Petal.Length")
  p <- draw_panels(list(w, draw_bar(c(A = 2, B = 3))))
  names <- vapply(p[["dependencies"]], function(d) d[["name"]], character(1))
  expect_equal(sum(names == "echarts-gl"), 1L)
})

test_that("3D opacity defaults to the point count and paths alone to opaque", {
  cfg <- setup_Scatter3DConfig(
    x = "Sepal.Length",
    y = "Sepal.Width",
    z = "Petal.Length",
    group = "Species"
  )
  expect_null(cfg@opacity)
  expected <- rtemis.draw:::auto_alpha(150)
  opt <- to_list(compile(cfg, iris))
  expect_equal(opt[["series"]][[1]][["itemStyle"]][["opacity"]], expected)
  # resolve() records the same value, so the resolved document draws the same.
  r <- resolve(cfg, iris)
  expect_equal(r@opacity, expected)
  expect_identical(r@origin[["opacity"]], "derived")
  expect_equal(to_list(compile(r, iris)), opt)
  # Incomplete rows are not counted.
  w <- draw_scatter3d(c(1, NA, 3), c(1, 2, 3), c(1, 2, 3))
  expect_equal(
    w$x$option$series[[1]]$itemStyle$opacity,
    rtemis.draw:::auto_alpha(2)
  )
  w <- draw_scatter3d(1:4, 1:4, 1:4, mode = "both")
  opacity <- vapply(
    w$x$option$series,
    function(s) (s$itemStyle %||% s$lineStyle)$opacity,
    numeric(1)
  )
  expect_equal(opacity, rep(rtemis.draw:::auto_alpha(4), 2))
  w <- draw_scatter3d(1:4, 1:4, 1:4, mode = "lines")
  expect_equal(w$x$option$series[[1]]$lineStyle$opacity, 1)
  expect_equal(
    resolve(
      setup_Scatter3DConfig(
        x = "a",
        y = "b",
        z = "c",
        mode = "lines"
      ),
      data.frame(a = 1:3, b = 1:3, c = 1:3)
    )@opacity,
    1
  )
  # An explicit value wins.
  w <- draw_scatter3d(1:4, 1:4, 1:4, opacity = 0.3)
  expect_equal(w$x$option$series[[1]]$itemStyle$opacity, 0.3)
  expect_error(setup_Scatter3DConfig(opacity = 2))
})


test_that("3D hover guides follow themes and preserve authored colors", {
  skip_if_no_node()
  plot <- draw_scatter3d(iris[1:3], group = iris[["Species"]])
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
  output <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "scatter3d_theme.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(
    paste(output, collapse = "\n"),
    "3D hover guides follow the active theme"
  )
})


test_that("3D camera settings validate and survive configuration round trips", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  for (projection in c("perspective", "orthographic")) {
    cfg <- setup_Scatter3DConfig(
      x = "Sepal.Length",
      y = "Sepal.Width",
      z = "Petal.Length",
      projection = projection,
      distance = 250,
      view_size = 220
    )
    write_chart_config(cfg, path)
    restored <- read_chart_config(path)
    expect_equal(restored@projection, projection)
    expect_equal(restored@distance, 250)
    opt <- to_list(compile(restored, iris))
    expect_equal(opt[["grid3D"]][["viewControl"]][["projection"]], projection)
    expect_equal(opt[["grid3D"]][["rtemisDistance"]], 250)
    expect_equal(opt[["grid3D"]][["viewControl"]][["orthographicSize"]], 219)
    expect_equal(opt, to_list(compile(cfg, iris)))
  }
  expect_equal(setup_Scatter3DConfig()@distance, 250)
  expect_equal(Scatter3DConfig()@distance, 250)
  schema <- chart_schema(
    Scatter3DConfig,
    id = "https://schema.rtemis.org/chart/scatter3d/v1/schema.json",
    title = "rtemis Scatter3DConfig",
    description = "Three-dimensional scatter chart."
  )
  expect_setequal(
    unlist(schema[["properties"]][["projection"]][["enum"]]),
    c("perspective", "orthographic")
  )
  expect_equal(schema[["properties"]][["distance"]][["minimum"]], 100)
  expect_equal(schema[["properties"]][["distance"]][["maximum"]], 400)
  expect_error(setup_Scatter3DConfig(projection = "oblique"))
  expect_error(setup_Scatter3DConfig(distance = 99))
  expect_error(setup_Scatter3DConfig(distance = 401))
  expect_error(setup_Scatter3DConfig(distance = Inf))
})

test_that("3D projection matches camera matrices for both camera modes", {
  skip_if_no_node()
  plot <- draw_scatter3d(iris[1:3], group = iris[["Species"]])
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
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "scatter3d_projection.js")),
      shQuote(input)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
test_that("3D inputs require complete coordinates and finite axis spans", {
  expect_error(
    draw_scatter3d(NA_real_, NA_real_, NA_real_),
    "complete 3D observation",
    class = "rtemis_null_input"
  )
  expect_error(
    draw_scatter3d(c(-1e308, 1e308), 1:2, 1:2),
    "finite nonzero range"
  )
  expect_error(draw_scatter3d(matrix(1:4, 2L)), "three coordinate columns")
  expect_error(draw_scatter3d(matrix(1:6, 2L), y = 1:2), "columns alone")
  expect_identical(
    render_meta(setup_Scatter3DConfig(), NULL),
    legend_meta("top", "outside")
  )
})
