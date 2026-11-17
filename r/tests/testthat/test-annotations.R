test_that("annotations validate and materialize native geometry", {
  layer <- AnnotationLayer(
    hline = 0,
    vline = 2,
    x = 1,
    y = 3,
    text = "Peak",
    bands = list(c(1, 2))
  )
  out <- to_list(layer)
  expect_equal(
    out[["markLine"]][["data"]],
    list(list(yAxis = 0), list(xAxis = 2))
  )
  expect_equal(out[["data"]][[1]][["label"]][["formatter"]], "{b}")
  expect_equal(
    out[["markArea"]][["data"]][[1]],
    list(list(xAxis = 1), list(xAxis = 2))
  )
  expect_identical(strip_js(out), out)
  expect_error(AnnotationLayer(x = 1, y = 2))
  expect_error(AnnotationLayer(bands = list(c(2, 1))))
  expect_error(AnnotationLayer(hline = Inf))
  expect_error(AnnotationLayer(x = NA_real_, y = 1, text = "bad"))
  w <- draw_scatter(1:5, 5:1)
  layers <- draw_annotate(w, layer = layer)[["x"]][["build"]][["layers"]]
  expect_length(layers, 1L)
  expect_identical(layers[[1L]][["series"]], list(out))
  expect_error(draw_annotate(w, layer = layer, color = "red"))
  expect_error(
    draw_annotate(w, hline = 1, x_axis = 3L),
    class = "rtemis_value_error"
  )
  expect_error(draw_annotate(draw_pie(c(1, 2)), hline = 1))
})
test_that("annotations follow every build of a built chart", {
  skip_if_no_node()
  w <- draw_annotate(draw_line(1:5, c(2, 3, 1, 5, 4)), hline = 3)
  for (dark in c(FALSE, TRUE)) {
    series <- built_option(w, dark = dark, simplify = FALSE)[["series"]]
    expect_length(series, 2L)
    expect_identical(series[[2L]][["markLine"]][["data"]][[1L]][["yAxis"]], 3L)
  }
})
test_that("annotations require axes from the same grid", {
  plot <- draw(EChartsOption(
    x_axis = Axis(type = "value"),
    y_axis = Axis(type = "value"),
    series = ScatterSeries(data = list(c(1, 2)))
  ))
  plot[["x"]][["option"]][["xAxis"]][["gridIndex"]] <- 1L
  expect_error(
    draw_annotate(plot, hline = 3),
    "same grid",
    class = "rtemis_value_error"
  )
})
