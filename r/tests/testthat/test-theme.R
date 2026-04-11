# test-theme.R
# Tests for Theme S7 class and built-in themes

test_that("Theme creates with defaults", {
  th <- Theme()
  expect_true(S7::S7_inherits(th, Theme))
  expect_equal(to_list(th), list())
})

test_that("Theme to_list() converts names", {
  th <- Theme(
    color = c("#5470c6", "#91cc75"),
    background_color = "#fff",
    text_style = TextStyle(font_size = 14, color = "#333")
  )
  out <- to_list(th)
  expect_equal(out$color, c("#5470c6", "#91cc75"))
  expect_equal(out$backgroundColor, "#fff")
  expect_equal(out$textStyle$fontSize, 14)
  expect_equal(out$textStyle$color, "#333")
})

test_that("Theme axis overrides", {
  th <- Theme(
    category_axis = list(
      axisLine = list(lineStyle = list(color = "#999")),
      splitLine = list(show = FALSE)
    ),
    value_axis = list(
      splitLine = list(lineStyle = list(type = "dashed"))
    )
  )
  out <- to_list(th)
  expect_equal(out$categoryAxis$axisLine$lineStyle$color, "#999")
  expect_equal(out$categoryAxis$splitLine$show, FALSE)
  expect_equal(out$valueAxis$splitLine$lineStyle$type, "dashed")
})

test_that("Theme component overrides", {
  th <- Theme(
    title = list(textStyle = list(fontSize = 20)),
    tooltip = list(backgroundColor = "#000"),
    legend = list(textStyle = list(color = "#666"))
  )
  out <- to_list(th)
  expect_equal(out$title$textStyle$fontSize, 20)
  expect_equal(out$tooltip$backgroundColor, "#000")
  expect_equal(out$legend$textStyle$color, "#666")
})

test_that("Theme series defaults", {
  th <- Theme(
    line = list(symbol = "circle", smooth = TRUE),
    bar = list(barMaxWidth = 40)
  )
  out <- to_list(th)
  expect_equal(out$line$symbol, "circle")
  expect_equal(out$line$smooth, TRUE)
  expect_equal(out$bar$barMaxWidth, 40)
})

test_that("Theme color validates", {
  expect_error(Theme(color = 123))
})

# -- Built-in themes -----------------------------------------------------------

test_that("theme_light returns valid Theme", {
  th <- theme_light()
  expect_true(S7::S7_inherits(th, Theme))
  out <- to_list(th)
  expect_true(length(out$color) >= 5)
  expect_null(out$backgroundColor)
  expect_equal(out$textStyle$fontFamily, "sans-serif")
  expect_equal(out$textStyle$fontSize, 12)
})

test_that("theme_dark returns valid Theme", {
  th <- theme_dark()
  expect_true(S7::S7_inherits(th, Theme))
  out <- to_list(th)
  expect_equal(out$backgroundColor, "#181818")
  expect_equal(out$textStyle$color, "rgba(255, 255, 255, 0.7)")
  expect_equal(out$categoryAxis$splitLine$show, FALSE)
})

# -- textStyle propagation --------------------------------------------------------

test_that("Theme textStyle propagates to component defaults", {
  th <- Theme(text_style = TextStyle(font_size = 24, font_family = "Helvetica"))
  out <- to_list(th)
  # Global textStyle cascades to title, legend, tooltip
  expect_equal(out$title$textStyle$fontSize, 24)
  expect_equal(out$title$textStyle$fontFamily, "Helvetica")
  expect_equal(out$legend$textStyle$fontSize, 24)
  expect_equal(out$tooltip$textStyle$fontSize, 24)
  # Also to subtitle
  expect_equal(out$title$subtextStyle$fontSize, 24)
})

test_that("Theme component textStyle overrides propagated values", {
  th <- Theme(
    text_style = TextStyle(font_size = 24, color = "#333"),
    title = list(textStyle = list(fontSize = 36))
  )
  out <- to_list(th)
  # Component-level fontSize wins over global
  expect_equal(out$title$textStyle$fontSize, 36)
  # Global color still propagated (not overridden at component level)
  expect_equal(out$title$textStyle$color, "#333")
})
