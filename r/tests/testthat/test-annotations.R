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
  expect_length(
    draw_annotate(w, layer = layer)[["x"]][["option"]][["series"]],
    2
  )
  expect_error(draw_annotate(w, layer = layer, color = "red"))
  expect_error(draw_annotate(w, x_axis = 3L))
  expect_error(draw_annotate(draw_pie(c(1, 2)), hline = 1))
})
