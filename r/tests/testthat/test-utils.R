# test-utils.R
# Tests for internal utility helpers

test_that("snake_to_camel converts snake_case names to camelCase", {
  expect_equal(rtemis.draw:::snake_to_camel("border_width"), "borderWidth")
  expect_equal(rtemis.draw:::snake_to_camel("font_size"), "fontSize")
  expect_equal(rtemis.draw:::snake_to_camel("color"), "color")
})

test_that("calc_limits applies symmetric padding to the data range", {
  expect_equal(
    rtemis.draw:::calc_limits(c(0, 10), pad = 0.1),
    c(-1, 11)
  )
})

test_that("calc_limits handles constant data", {
  expect_equal(
    rtemis.draw:::calc_limits(c(5, 5, 5), pad = 0.04),
    c(4.98, 5.02)
  )
})

test_that("color_with_alpha converts colors to rgba strings", {
  expect_equal(
    rtemis.draw:::color_with_alpha("#00b2b2", 0.5),
    "rgba(0, 178, 178, 0.5)"
  )
})
