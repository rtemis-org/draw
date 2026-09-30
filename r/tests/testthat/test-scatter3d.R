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
  expect_equal(opt[["grid3D"]][["viewControl"]][["projection"]], "orthographic")
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
