# test-utils.R
# Tests for internal utility helpers

test_that("rtemis_welcome returns the formatted welcome string silently", {
  expect_silent(
    welcome <- rtemis.draw:::rtemis_welcome("example.pkg", "1.2.3")
  )
  expect_identical(
    welcome,
    paste(".:example.pkg 1.2.3 \U0001F58C", utils::sessionInfo()[["platform"]])
  )
})

test_that("rtemis_welcome accepts package_version objects", {
  expect_identical(
    rtemis.draw:::rtemis_welcome("rtemis.draw", package_version("0.5.3.9000")),
    paste(
      ".:rtemis.draw 0.5.3.9000 \U0001F58C",
      utils::sessionInfo()[["platform"]]
    )
  )
})

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

test_that("lighten interpolates toward white in RGB space", {
  # black lightened 10% => 10% of 255 per channel
  expect_equal(rtemis.draw:::lighten("#000000", 0.1), "#1A1A1A")
  # amount = 0 is identity, amount = 1 is white
  expect_equal(rtemis.draw:::lighten("#3366CC", 0), "#3366CC")
  expect_equal(rtemis.draw:::lighten("#3366CC", 1), "#FFFFFF")
  # white is a fixed point
  expect_equal(rtemis.draw:::lighten("#FFFFFF", 0.5), "#FFFFFF")
})

test_that("lighten accepts named colors and is vectorized", {
  expect_equal(
    rtemis.draw:::lighten(c("red", "#000000"), 0.5),
    c("#FF8080", "#808080")
  )
})

test_that("lighten preserves names of the input vector", {
  expect_equal(
    rtemis.draw:::lighten(c(a = "#000000", b = "#FFFFFF"), 0.5),
    c(a = "#808080", b = "#FFFFFF")
  )
  # unnamed input stays unnamed
  expect_null(names(rtemis.draw:::lighten("#000000", 0.5)))
})

test_that("lighten preserves the alpha channel", {
  # opaque input => 6-digit hex, no alpha suffix
  expect_equal(rtemis.draw:::lighten("#000000", 0.5), "#808080")
  # input with alpha keeps its alpha unchanged
  expect_equal(rtemis.draw:::lighten("#00000080", 0.5), "#80808080")
})

test_that("lighten rejects invalid amount", {
  expect_error(rtemis.draw:::lighten("#000000", -0.1), "amount")
  expect_error(rtemis.draw:::lighten("#000000", 1.5), "amount")
  expect_error(rtemis.draw:::lighten("#000000", "a"), "amount")
  expect_error(rtemis.draw:::lighten("#000000", c(0.1, 0.2)), "amount")
})

test_that("auto_alpha falls with the log of the point count", {
  auto_alpha <- rtemis.draw:::auto_alpha
  expect_equal(auto_alpha(10), 0.75)
  expect_equal(auto_alpha(1000), 0.45)
  expect_equal(auto_alpha(1e4), 0.3)
  # Clamped at both ends; an empty count does not reach log10(0).
  expect_equal(auto_alpha(0), 0.9)
  expect_equal(auto_alpha(1), 0.9)
  expect_equal(auto_alpha(1e7), 0.15)
  expect_true(auto_alpha(150) > auto_alpha(151))
})

test_that("numeric and logical property factories enforce scalar bounds", {
  ScalarProperties <- S7::new_class(
    "ScalarProperties",
    properties = list(
      size = nonneg_numeric_default(2),
      opacity = prob_default(.5),
      visible = logical_default(TRUE),
      mode = map_enum_default(c("first", "second"), "first")
    )
  )
  value <- ScalarProperties()
  expect_identical(value@size, 2)
  expect_identical(value@opacity, .5)
  expect_true(value@visible)
  expect_identical(value@mode, "first")
  expect_equal(ScalarProperties(size = 0)@size, 0)
  for (opacity in c(0, 1)) {
    expect_equal(ScalarProperties(opacity = opacity)@opacity, opacity)
  }
  expect_false(ScalarProperties(visible = FALSE)@visible)
  expect_identical(ScalarProperties(mode = "second")@mode, "second")
  for (bad in list(-1, NA_real_, numeric(), c(1, 2))) {
    expect_error(ScalarProperties(size = bad), "non-negative")
  }
  for (bad in list(-.1, 1.1, NA_real_, numeric(), c(0, 1))) {
    expect_error(ScalarProperties(opacity = bad), "single number")
  }
  for (bad in list(NA, logical(), c(TRUE, FALSE))) {
    expect_error(ScalarProperties(visible = bad), "single logical")
  }
  for (bad in list("other", NA_character_, character(), c("first", "second"))) {
    expect_error(ScalarProperties(mode = bad), "must be one of")
  }
})

test_that("nullable property factories validate values and omit unset fields", {
  NullableProperties <- S7::new_class(
    "NullableProperties",
    properties = list(
      size = numeric_or_null_property(),
      position = numeric_or_string_property(),
      color = color_palette_property(),
      offset = symbol_offset_property(),
      style = class_or_null_property(LineStyle)
    )
  )
  expect_identical(props_to_list(NullableProperties()), list())
  value <- NullableProperties(
    size = 2,
    position = "10%",
    color = c("red", "blue"),
    offset = c("-50%", "50%"),
    style = LineStyle(width = 3)
  )
  expect_identical(
    props_to_list(value),
    list(
      size = 2,
      position = "10%",
      color = c("red", "blue"),
      offset = c("-50%", "50%"),
      style = list(width = 3)
    )
  )
  expect_equal(NullableProperties(position = 10, offset = 2)@position, 10)
  expect_equal(NullableProperties(offset = c(1, 2))@offset, c(1, 2))
  for (bad in list("2", c(1, 2))) {
    expect_error(NullableProperties(size = bad), "single number")
  }
  expect_error(NullableProperties(position = TRUE), "number, string")
  expect_error(NullableProperties(position = c(1, 2)), "single value")
  expect_error(NullableProperties(color = 1:2), "character vector")
  expect_error(NullableProperties(offset = TRUE), "percentage string")
  for (bad in list(numeric(), 1:3)) {
    expect_error(NullableProperties(offset = bad), "length-2")
  }
  expect_error(NullableProperties(style = TextStyle()), "LineStyle")
})

test_that("property serialization applies renames and preserves nested list values", {
  NestedProperties <- S7::new_class(
    "NestedProperties",
    properties = list(
      line_style = S7::class_any,
      entries = S7::class_list
    )
  )
  value <- NestedProperties(
    line_style = list(primary = LineStyle(width = 2), label = "primary"),
    entries = list(LineStyle(color = "red"), "plain")
  )
  expect_identical(
    props_to_list(value, rename = c(line_style = "stroke_style")),
    list(
      strokeStyle = list(primary = list(width = 2), label = "primary"),
      entries = list(list(color = "red"), "plain")
    )
  )
})

test_that("margins reject non-vector inputs and draw rejects unnamed arguments", {
  expect_error(
    parse_margins(environment()),
    "named numeric vector",
    class = "rtemis_type_error"
  )
  expect_error(
    check_dots_empty(1),
    "1 unnamed argument",
    class = "rtemis_value_error"
  )
  expect_error(
    check_dots_empty(1, 2),
    "2 unnamed arguments",
    class = "rtemis_value_error"
  )
})
