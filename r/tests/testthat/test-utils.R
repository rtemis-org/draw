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

# -- parse_margins / resolve_margins -------------------------------------------

test_that("parse_margins(NULL) returns all-NULL sides", {
  m <- rtemis.draw:::parse_margins(NULL)
  expect_equal(names(m), c("top", "right", "bottom", "left"))
  expect_true(all(vapply(m, is.null, logical(1))))
})

test_that("parse_margins accepts named numeric vector", {
  m <- rtemis.draw:::parse_margins(c(left = 80, right = 20))
  expect_equal(m$left, 80)
  expect_equal(m$right, 20)
  expect_null(m$top)
  expect_null(m$bottom)
})

test_that("parse_margins accepts named list with mixed types", {
  m <- rtemis.draw:::parse_margins(list(left = 80, right = "10%"))
  expect_equal(m$left, 80)
  expect_equal(m$right, "10%")
})

test_that("parse_margins rejects unrecognised side names", {
  expect_error(
    rtemis.draw:::parse_margins(c(foo = 10)),
    "unrecognised"
  )
  expect_error(
    rtemis.draw:::parse_margins(c(left = 10, bogus = 5)),
    "bogus"
  )
})

test_that("parse_margins rejects unnamed input", {
  expect_error(rtemis.draw:::parse_margins(c(10, 20)), "named")
  expect_error(rtemis.draw:::parse_margins(list(10, 20)), "named")
})

test_that("parse_margins rejects duplicate names", {
  expect_error(
    rtemis.draw:::parse_margins(c(left = 10, left = 20)),
    "duplicate"
  )
})

test_that("parse_margins rejects non-scalar per-side values", {
  expect_error(
    rtemis.draw:::parse_margins(list(left = c(1, 2))),
    "left"
  )
})

test_that("parse_margins treats NA entry as unspecified", {
  m <- rtemis.draw:::parse_margins(c(left = 80, right = NA_real_))
  expect_equal(m$left, 80)
  expect_null(m$right)
})

test_that("resolve_margins(NULL) returns NULL", {
  expect_null(rtemis.draw:::resolve_margins(NULL))
})

test_that("resolve_margins returns a Grid with only specified sides", {
  g <- rtemis.draw:::resolve_margins(c(left = 80, top = 40))
  expect_true(S7::S7_inherits(g, Grid))
  expect_equal(g@left, 80)
  expect_equal(g@top, 40)
  expect_null(g@right)
  expect_null(g@bottom)
})
