# test-styles.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

test_that("style classes omit unset fields", {
  for (constructor in list(LineStyle, AreaStyle, ItemStyle, TextStyle)) {
    expect_identical(to_list(constructor()), list())
  }
})

test_that("area origins serialize numeric and named baselines", {
  for (origin in list("auto", "start", "end", 0, -2)) {
    expect_identical(to_list(AreaStyle(origin = origin)), list(origin = origin))
  }
  for (bad in list("middle", TRUE, c(1, 2))) {
    expect_error(AreaStyle(origin = bad), "must be")
  }
})

test_that("item border radii serialize supported values", {
  for (radius in list(2, c(1, 2, 3, 4), "50%")) {
    expect_identical(
      to_list(ItemStyle(border_radius = radius)),
      list(borderRadius = radius)
    )
  }
  for (bad in list(TRUE, list(1), c("1%", "2%"))) {
    expect_error(ItemStyle(border_radius = bad), "must be")
  }
})

test_that("text styles preserve weights, padding and nested rich text", {
  for (weight in list(400, "normal", "bold", "bolder", "lighter")) {
    expect_identical(
      to_list(TextStyle(font_weight = weight)),
      list(fontWeight = weight)
    )
  }
  style <- TextStyle(
    font_size = "12px",
    border_radius = c(1, 2, 3, 4),
    padding = c(2, 4),
    rich = list(emphasis = TextStyle(font_weight = "bold"))
  )
  expect_identical(
    to_list(style),
    list(
      fontSize = "12px",
      borderRadius = c(1, 2, 3, 4),
      padding = c(2, 4),
      rich = list(emphasis = list(fontWeight = "bold"))
    )
  )
  expect_error(TextStyle(font_weight = "heavy"), "must be")
  expect_error(TextStyle(border_radius = "2px"), "numeric vector")
  expect_error(TextStyle(padding = "2px"), "numeric vector")
})
