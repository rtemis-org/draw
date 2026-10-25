# test-zoom.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# %% helpers ----
zoom_types <- function(option) {
  vapply(option[["dataZoom"]], function(z) z[["type"]], character(1))
}

inside_zooms <- function(option) {
  Filter(function(z) identical(z[["type"]], "inside"), option[["dataZoom"]])
}


# %% resolve_zoom ----
test_that("resolve_zoom returns NULL for FALSE and NULL", {
  expect_null(resolve_zoom(FALSE))
  expect_null(resolve_zoom(NULL))
})

test_that("resolve_zoom x and y presets pair a slider with a Shift+wheel inside zoom", {
  zoom <- resolve_zoom(TRUE, axis = "x")
  expect_identical(
    vapply(zoom, function(z) z@type, character(1)),
    c("slider", "inside")
  )
  expect_equal(zoom[[1L]]@x_axis_index, 0)
  expect_identical(zoom[[2L]]@zoom_on_mouse_wheel, "shift")
  expect_true(zoom[[2L]]@move_on_mouse_move)
  expect_null(zoom[[2L]]@filter_mode)
  zoom <- resolve_zoom(TRUE, axis = "y", filter_mode = "none")
  expect_equal(zoom[[1L]]@y_axis_index, 0)
  expect_null(zoom[[1L]]@x_axis_index)
  expect_identical(zoom[[2L]]@filter_mode, "none")
})

test_that("resolve_zoom xy preset gives one inside zoom per axis", {
  zoom <- resolve_zoom(TRUE, axis = "xy", filter_mode = "none")
  expect_length(zoom, 2L)
  expect_identical(
    vapply(zoom, function(z) z@type, character(1)),
    c("inside", "inside")
  )
  expect_equal(zoom[[1L]]@x_axis_index, 0)
  expect_equal(zoom[[2L]]@y_axis_index, 0)
  for (z in zoom) {
    expect_identical(z@zoom_on_mouse_wheel, "shift")
    expect_identical(z@filter_mode, "none")
  }
})

test_that("resolve_zoom wraps a DataZoom and rejects other input", {
  dz <- DataZoom(type = "inside")
  expect_identical(resolve_zoom(dz), list(dz))
  expect_error(resolve_zoom("yes"), class = "rtemis_type_error")
  expect_error(resolve_zoom(TRUE, axis = "z"))
})


# %% zoom_toolbox ----
test_that("zoom_toolbox carries box-zoom and reset, with filterMode only when set", {
  tb <- zoom_toolbox()
  expect_named(tb[["feature"]], c("dataZoom", "restore"))
  expect_null(tb[["feature"]][["dataZoom"]][["filterMode"]])
  expect_identical(
    zoom_toolbox("none")[["feature"]][["dataZoom"]][["filterMode"]],
    "none"
  )
})


# %% reserve_slider_room ----
test_that("reserve_slider_room grows the right margin for a vertical slider", {
  zoom <- resolve_zoom(TRUE, axis = "y")
  placed <- reserve_slider_room(zoom, Grid(right = 36), axis = "y")
  expect_equal(placed[["data_zoom"]][[1L]]@right, 43)
  expect_equal(placed[["data_zoom"]][[1L]]@width, 30)
  expect_equal(placed[["grid"]]@right, 81)
  expect_null(placed[["grid"]]@bottom)
})

test_that("reserve_slider_room leaves a percentage margin unchanged", {
  zoom <- resolve_zoom(TRUE, axis = "y")
  grid <- Grid(right = "10%")
  placed <- reserve_slider_room(zoom, grid, axis = "y")
  expect_identical(placed[["grid"]], grid)
  expect_null(placed[["data_zoom"]][[1L]]@right)
})


# %% zoom_layout ----
test_that("zoom_layout adds a toolbox for xy and leaves the grid unchanged", {
  grid <- Grid(bottom = 36)
  out <- zoom_layout(TRUE, axis = "xy", grid = grid)
  expect_identical(out[["grid"]], grid)
  expect_false(is.null(out[["toolbox"]][["feature"]][["dataZoom"]]))
  out <- zoom_layout(list(DataZoom(type = "inside")), axis = "xy", grid = grid)
  expect_null(out[["toolbox"]])
  expect_null(zoom_layout(FALSE)[["data_zoom"]])
})


# %% draw_scatter ----
test_that("draw_scatter zoom = TRUE adds inside zooms on both axes and a toolbox", {
  opt <- draw_scatter(mtcars$wt, mtcars$mpg, zoom = TRUE)$x$option
  expect_identical(zoom_types(opt), c("inside", "inside"))
  for (z in opt[["dataZoom"]]) {
    expect_identical(z[["zoomOnMouseWheel"]], "shift")
    expect_identical(z[["filterMode"]], "none")
  }
  expect_false(is.null(opt[["toolbox"]][["feature"]][["dataZoom"]]))
  opt <- draw_scatter(mtcars$wt, mtcars$mpg)$x$option
  expect_null(opt[["dataZoom"]])
  expect_null(opt[["toolbox"]])
})

test_that("ScatterConfig carries zoom to the option", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", zoom = TRUE)
  expect_true(cfg@zoom)
  opt <- draw(cfg, data = mtcars)$x$option
  expect_identical(zoom_types(opt), c("inside", "inside"))
  expect_false(setup_ScatterConfig()@zoom)
})


# %% draw_bar ----
test_that("draw_bar zoom targets the category axis in either orientation", {
  opt <- draw_bar(c("A", "B", "C"), c(3, 7, 2), zoom = TRUE)$x$option
  expect_identical(zoom_types(opt), c("slider", "inside"))
  expect_equal(opt[["dataZoom"]][[1L]][["xAxisIndex"]], 0)
  expect_identical(opt[["dataZoom"]][[2L]][["zoomOnMouseWheel"]], "shift")
  opt <- draw_bar(
    c("A", "B", "C"),
    c(3, 7, 2),
    horizontal = TRUE,
    zoom = TRUE
  )$x$option
  slider <- opt[["dataZoom"]][[1L]]
  expect_equal(slider[["yAxisIndex"]], 0)
  expect_null(slider[["xAxisIndex"]])
  expect_equal(slider[["right"]], DEFAULT_MARGINS[["right"]] + 7)
  expect_equal(opt[["grid"]][["right"]], DEFAULT_MARGINS[["right"]] + 45)
  expect_null(draw_bar(c("A", "B"), c(1, 2))$x$option[["dataZoom"]])
})

test_that("BarConfig carries zoom to the option", {
  data <- data.frame(g = c("a", "b"), v = c(1, 2))
  cfg <- setup_BarConfig(x = "g", y = "v", zoom = TRUE)
  opt <- draw(cfg, data = data)$x$option
  expect_identical(zoom_types(opt), c("slider", "inside"))
})


# %% draw_histogram, draw_density ----
test_that("draw_histogram and draw_density zoom the x-axis without filtering", {
  for (opt in list(
    draw_histogram(iris$Sepal.Length, zoom = TRUE)$x$option,
    draw_density(iris$Sepal.Length, zoom = TRUE)$x$option
  )) {
    expect_identical(zoom_types(opt), c("slider", "inside"))
    expect_equal(opt[["dataZoom"]][[1L]][["xAxisIndex"]], 0)
    for (z in opt[["dataZoom"]]) {
      expect_identical(z[["filterMode"]], "none")
    }
    expect_identical(inside_zooms(opt)[[1L]][["zoomOnMouseWheel"]], "shift")
  }
  expect_null(draw_density(iris$Sepal.Length)$x$option[["dataZoom"]])
})

test_that("ridge distributions zoom every row together without a slider", {
  opt <- draw_density(
    split(iris$Sepal.Length, iris$Species),
    mode = "ridge",
    zoom = TRUE
  )$x$option
  expect_identical(zoom_types(opt), "inside")
  expect_identical(opt[["dataZoom"]][[1L]][["xAxisIndex"]], "all")
  expect_identical(opt[["dataZoom"]][[1L]][["zoomOnMouseWheel"]], "shift")
})

test_that("DensityConfig and HistogramConfig carry zoom to the option", {
  opt <- draw(
    setup_DensityConfig(x = "mpg", zoom = TRUE),
    data = mtcars
  )$x$option
  expect_identical(zoom_types(opt), c("slider", "inside"))
  opt <- draw(
    setup_HistogramConfig(x = "mpg", zoom = TRUE),
    data = mtcars
  )$x$option
  expect_identical(zoom_types(opt), c("slider", "inside"))
})


# %% Shift+wheel across existing presets ----
test_that("line, time series, and Gantt presets zoom on Shift+wheel", {
  opt <- draw_line(1:5, 1:5, zoom = TRUE)$x$option
  expect_identical(inside_zooms(opt)[[1L]][["zoomOnMouseWheel"]], "shift")
  tasks <- data.frame(
    label = c("a", "b"),
    start = c(0, 1),
    end = c(1, 3)
  )
  opt <- draw_gantt(tasks)$x$option
  expect_identical(zoom_types(opt), c("inside", "inside"))
  for (z in opt[["dataZoom"]]) {
    expect_identical(z[["zoomOnMouseWheel"]], "shift")
    expect_identical(z[["filterMode"]], "none")
  }
  expect_identical(
    opt[["toolbox"]][["feature"]][["dataZoom"]][["filterMode"]],
    "none"
  )
})


# %% schemas ----
test_that("chart schemas publish zoom as a boolean", {
  for (cls in list(ScatterConfig, BarConfig, DensityConfig, HistogramConfig)) {
    s <- chart_schema(
      cls,
      id = "https://example.org/s.json",
      title = "t",
      description = "d"
    )
    expect_identical(s[["properties"]][["zoom"]][["type"]], "boolean")
  }
})
