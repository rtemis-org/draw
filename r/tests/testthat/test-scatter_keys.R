# The keys of a scatter chart's color and size encodings, placed by the
# @rtemis/draw key layout. spec: draw/shared-builders#keys

test_that("a size column adds a size key titled by size_title", {
  skip_if_no_node()
  w <- draw_scatter(
    1:4,
    c(2, 3, 1, 4),
    size = c(10, 20, 30, 40),
    size_title = "Count"
  )
  expect_identical(w[["x"]][["build"]][["config"]][["size_title"]], "Count")
  key <- built_option(w, simplify = FALSE)[["graphic"]][["elements"]][[1L]]
  expect_identical(key[["id"]], "rtemis-size-key")
  expect_identical(key[["info"]][["sizeKey"]][["title"]], "Count")
  values <- vapply(
    key[["info"]][["sizeKey"]][["items"]],
    function(item) item[["value"]],
    numeric(1)
  )
  expect_equal(values, c(40, 30, 20))
})

test_that("a color scale is titled by color_title and makes fits gray", {
  skip_if_no_node()
  option <- built_option(
    draw_scatter(
      1:6,
      c(2, 3, 1, 5, 4, 6),
      color = c(0.41, 0.45, 0.5, 0.55, 0.6, 0.65),
      color_title = "Ratio",
      fit = "glm"
    ),
    simplify = FALSE
  )
  expect_identical(option[["visualMap"]][["text"]][[1L]], "Ratio")
  expect_identical(option[["visualMap"]][["precision"]], 2L)
  fit <- option[["series"]][[length(option[["series"]])]]
  expect_identical(fit[["lineStyle"]][["color"]], "#808080")
})

test_that("color takes one y column", {
  expect_error(
    setup_ScatterConfig(x = "a", y = c("b", "c"), color = "d"),
    "one y column"
  )
})

test_that("SVG export draws both keys beside the plot", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(
    draw_scatter(
      1:20,
      sin(1:20),
      size = 1:20,
      color = cos(1:20),
      size_title = "Weight",
      color_title = "Phase",
      legend_position = "right"
    ),
    path,
    width = 600,
    height = 400
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(svg, ">Weight<", fixed = TRUE)
  expect_match(svg, ">Phase<", fixed = TRUE)
  # The size key's circles are drawn.
  expect_gte(lengths(regmatches(svg, gregexpr("<circle", svg))), 3L)
})
