# test-label.R
# Tests for LabelOption and LabelLine S7 classes

# -- LabelOption ---------------------------------------------------------------

test_that("LabelOption creates with defaults (all NULL)", {
  lab <- LabelOption()
  expect_true(S7::S7_inherits(lab, LabelOption))
  expect_null(lab@show)
  expect_null(lab@position)
  expect_null(lab@distance)
  expect_null(lab@rotate)
  expect_null(lab@offset)
  expect_null(lab@formatter)
  expect_null(lab@silent)
  expect_null(lab@precision)
  expect_null(lab@value_animation)
  expect_null(lab@min_margin)
  expect_null(lab@text_style)
})

test_that("LabelOption to_list() drops NULLs", {
  lab <- LabelOption()
  expect_equal(to_list(lab), list())
})

test_that("LabelOption to_list() converts names and values", {
  lab <- LabelOption(
    show = TRUE,
    position = "top",
    distance = 5,
    rotate = 45,
    value_animation = TRUE,
    min_margin = 2
  )
  out <- to_list(lab)
  expect_equal(out$show, TRUE)
  expect_equal(out$position, "top")
  expect_equal(out$distance, 5)
  expect_equal(out$rotate, 45)
  expect_equal(out$valueAnimation, TRUE)
  expect_equal(out$minMargin, 2)
  # camelCase keys
  expect_true("valueAnimation" %in% names(out))
  expect_true("minMargin" %in% names(out))
})

test_that("LabelOption flattens text_style into same level", {
  lab <- LabelOption(
    show = TRUE,
    position = "inside",
    text_style = TextStyle(
      color = "#fff",
      font_size = 14,
      font_weight = "bold"
    )
  )
  out <- to_list(lab)
  # text_style fields should be flattened

  expect_equal(out$color, "#fff")
  expect_equal(out$fontSize, 14)
  expect_equal(out$fontWeight, "bold")
  # textStyle key should NOT exist

  expect_null(out$textStyle)
  # label-level fields still present

  expect_equal(out$show, TRUE)
  expect_equal(out$position, "inside")
})

test_that("LabelOption position accepts valid strings", {
  valid_positions <- c(
    "top",
    "left",
    "right",
    "bottom",
    "inside",
    "insideLeft",
    "insideRight",
    "insideTop",
    "insideBottom",
    "insideTopLeft",
    "insideBottomLeft",
    "insideTopRight",
    "insideBottomRight",
    "outside"
  )
  for (pos in valid_positions) {
    lab <- LabelOption(position = pos)
    expect_equal(lab@position, pos)
  }
})

test_that("LabelOption position accepts numeric array", {
  lab <- LabelOption(position = c(10, 20))
  expect_equal(lab@position, c(10, 20))
})

test_that("LabelOption position rejects invalid strings", {
  expect_error(LabelOption(position = "invalid"))
})

test_that("LabelOption offset accepts numeric vector", {
  lab <- LabelOption(offset = c(5, -10))
  expect_equal(lab@offset, c(5, -10))
})

test_that("LabelOption offset rejects non-numeric", {
  expect_error(LabelOption(offset = "bad"))
})

test_that("LabelOption precision accepts number and 'auto'", {
  lab1 <- LabelOption(precision = 2)
  expect_equal(lab1@precision, 2)
  lab2 <- LabelOption(precision = "auto")
  expect_equal(lab2@precision, "auto")
})

test_that("LabelOption precision rejects invalid values", {
  expect_error(LabelOption(precision = "bad"))
  expect_error(LabelOption(precision = c(1, 2)))
})

test_that("LabelOption formatter accepts string and function", {
  lab1 <- LabelOption(formatter = "{b}: {c}")
  expect_equal(lab1@formatter, "{b}: {c}")
  fn <- function(params) paste(params$name)
  lab2 <- LabelOption(formatter = fn)
  expect_true(is.function(lab2@formatter))
})

# -- LabelLine -----------------------------------------------------------------

test_that("LabelLine creates with defaults (all NULL)", {
  ll <- LabelLine()
  expect_true(S7::S7_inherits(ll, LabelLine))
  expect_null(ll@show)
  expect_null(ll@show_above)
  expect_null(ll@length)
  expect_null(ll@length2)
  expect_null(ll@smooth)
  expect_null(ll@min_turn_angle)
  expect_null(ll@line_style)
})

test_that("LabelLine to_list() drops NULLs", {
  ll <- LabelLine()
  expect_equal(to_list(ll), list())
})

test_that("LabelLine to_list() converts names", {
  ll <- LabelLine(
    show = TRUE,
    show_above = FALSE,
    length = 20,
    length2 = 10,
    smooth = 0.5,
    min_turn_angle = 30
  )
  out <- to_list(ll)
  expect_equal(out$show, TRUE)
  expect_equal(out$showAbove, FALSE)
  expect_equal(out$length, 20)
  expect_equal(out$length2, 10)
  expect_equal(out$smooth, 0.5)
  expect_equal(out$minTurnAngle, 30)
})

test_that("LabelLine smooth accepts logical and numeric", {
  ll1 <- LabelLine(smooth = TRUE)
  expect_equal(ll1@smooth, TRUE)
  ll2 <- LabelLine(smooth = 0.3)
  expect_equal(ll2@smooth, 0.3)
})

test_that("LabelLine smooth rejects invalid", {
  expect_error(LabelLine(smooth = "bad"))
})

test_that("LabelLine line_style accepts LineStyle", {
  ll <- LabelLine(
    show = TRUE,
    line_style = LineStyle(color = "#333", width = 2)
  )
  out <- to_list(ll)
  expect_equal(out$lineStyle$color, "#333")
  expect_equal(out$lineStyle$width, 2)
})

test_that("LabelLine line_style rejects non-LineStyle", {
  expect_error(LabelLine(line_style = "bad"))
})
