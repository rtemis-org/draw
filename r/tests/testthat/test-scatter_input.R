test_that("scatter inputs align lists and retain per-point metadata", {
  w <- draw_scatter(list(A = 1:3, B = 4:5), list(B = 8:9, A = 5:7))
  expect_equal(
    w[["x"]][["option"]][["series"]][[2]][["data"]],
    list(c(4, 8), c(5, 9))
  )
  w <- draw_scatter(
    c(1, NA, 3),
    c(3, 4, 5),
    group = c("A", "B", "A"),
    size = c(2, 4, 8),
    hover = c("One", "Missing", "Three")
  )
  points <- w[["x"]][["option"]][["series"]][[1]][["data"]]
  expect_equal(vapply(points, `[[`, numeric(1), "symbolSize"), c(2, 8))
  expect_equal(vapply(points, `[[`, "", "name"), c("One", "Three"))
  expect_equal(points[[2]][["value"]], c(3, 5))
  expect_error(draw_scatter(1:3, 1:2))
  expect_error(draw_scatter(1:3, 1:3, size = 1:2))
  expect_error(draw_scatter(1:3, 1:3, hover = "one"))
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
  opt <- to_list(compile(cfg, data))
  expect_equal(opt[["series"]][[1]][["name"]], "Linear")
  expect_equal(tail(opt[["series"]], 1)[[1]][["renderItem"]], "rtemis.rug.v1")
  expect_equal(
    to_list(compile(
      read_chart_config(write_chart_config(cfg, tempfile(fileext = ".json"))),
      data
    )),
    opt
  )
})
