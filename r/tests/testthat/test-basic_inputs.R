test_that("bar matrices, tables and sorting preserve category-series identity", {
  mat <- matrix(
    c(2, 1, 4, 3),
    2,
    dimnames = list(c("A", "B"), c("First", "Second"))
  )
  actual <- draw_bar(mat, order = "increasing")[["x"]][["option"]]
  expect_equal(actual[["xAxis"]][["data"]], c("B", "A"))
  expect_equal(actual[["series"]][[1]][["data"]], c(1, 2))
  expect_equal(actual[["series"]][[2]][["data"]], c(3, 4))
  counts <- table(c("A", "B", "B"))
  expect_equal(
    draw_bar(names(counts), counts)[["x"]][["option"]][["series"]][[1]][[
      "data"
    ]],
    c(1L, 2L)
  )
  expect_equal(
    draw_bar(counts)[["x"]][["option"]][["series"]][[1]][["data"]],
    c(1L, 2L)
  )
  expect_equal(bar_input(c("A", "B"), c(2, 2), "decreasing")[["index"]], 1:2)
  expect_error(draw_bar(c("A", "B"), 1))
  expect_error(draw_bar(c("A", "A"), 1:2))
  expect_error(draw_bar(c("A", "B"), c(1, Inf)))
})

test_that("pie tables and label choices preserve actual slice values", {
  tab <- data.frame(label = c("A", "B"), value = c(2, 3))
  w <- draw_pie(tab, label_format = "name_percent", percent_digits = 2L)
  s <- w[["x"]][["option"]][["series"]][[1]]
  expect_equal(
    s[["data"]],
    list(list(value = 2, name = "A"), list(value = 3, name = "B"))
  )
  expect_equal(s[["label"]][["formatter"]], "{b}: {d}%")
  expect_equal(s[["percentPrecision"]], 2L)
  expect_error(draw_pie(c(1, -1)))
  expect_error(draw_pie(c(0, 0)))
  expect_error(draw_pie(1:3, c("A", "B")))
  expect_error(setup_PieConfig(label_format = "bad"))
})
