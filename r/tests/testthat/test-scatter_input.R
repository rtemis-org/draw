test_that("scatter inputs align lists and retain per-point metadata", {
  skip_if_no_node()
  w <- draw_scatter(list(A = 1:3, B = 4:5), list(B = 8:9, A = 5:7))
  expect_equal(
    built_option(w, simplify = FALSE)[["series"]][[2]][["data"]],
    list(list(4L, 8L), list(5L, 9L))
  )
  w <- draw_scatter(
    c(1, NA, 3),
    c(3, 4, 5),
    group = c("A", "B", "A"),
    size = c(2, 4, 8),
    hover = c("One", "Missing", "Three")
  )
  points <- built_option(w, simplify = FALSE)[["series"]][[1]][["data"]]
  # A symbol's area is proportional to its size value, the largest 30 px across.
  expect_equal(
    vapply(points, function(p) p[["symbolSize"]], numeric(1)),
    c(15, 30)
  )
  expect_equal(vapply(points, `[[`, "", "name"), c("One", "Three"))
  expect_equal(unlist(points[[2]][["value"]]), c(3, 5, 8))
  expect_error(draw_scatter(1:3, 1:2))
  expect_error(draw_scatter(1:3, 1:3, size = 1:2))
  expect_error(draw_scatter(1:3, 1:3, hover = "one"))
  expect_error(draw_scatter(1:3, 1:3, color = 1:2))
  expect_error(draw_scatter(list(1:3), list(1:3), size = 2))
  data <- data.frame(
    x = 1:5,
    y = c(1, 3, 2, 5, 4),
    text = letters[1:5],
    size = 1:5
  )
  cfg <- setup_ScatterConfig(
    x = "x",
    y = "y",
    size = "size",
    hover = "text",
    fit = "glm",
    fit_name = "Linear",
    rug = TRUE
  )
  opt <- built_option(compile(cfg, data), simplify = FALSE)
  expect_equal(opt[["series"]][[1]][["name"]], "Linear")
  expect_equal(tail(opt[["series"]], 1)[[1]][["renderItem"]], "rtemis.rug.v1")
  expect_equal(
    built_option(
      compile(
        read_chart_config(write_chart_config(cfg, tempfile(fileext = ".json"))),
        data
      ),
      simplify = FALSE
    ),
    opt
  )
})
test_that("scatter inputs require a complete observation after filtering", {
  expect_error(
    draw_scatter(c(1, NA), c(NA, 2)),
    "complete scatter observation",
    class = "rtemis_null_input"
  )
})
test_that("a color vector drops the rows it lacks and maps to a color scale", {
  skip_if_no_node()
  w <- draw_scatter(1:4, c(2, 4, 3, 5), color = c(-1, NA, 0.5, 2))
  option <- built_option(w, simplify = FALSE)
  expect_length(option[["series"]][[1]][["data"]], 3L)
  expect_identical(option[["visualMap"]][["dimension"]], 2L)
  expect_equal(option[["visualMap"]][["min"]], -1)
  diverging <- built_option(
    draw_scatter(1:4, 1:4, color = c(-1, 0, 1, 2), color_scale = "diverging")
  )
  expect_equal(diverging[["visualMap"]][["min"]], -2)
  expect_equal(diverging[["visualMap"]][["max"]], 2)
  expect_length(diverging[["visualMap"]][["inRange"]][["color"]], 3L)
  expect_error(
    setup_ScatterConfig(x = "a", y = "b", color = "c", group = "d"),
    "color or group"
  )
})
