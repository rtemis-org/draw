# test-widget_sizing.R
# Widget dimensions in viewers, documents, and SVG export

test_that("unset dimensions fill the host and set dimensions are respected", {
  filled <- draw_bar(c(A = 1, B = 2))
  expect_null(filled[["width"]])
  expect_null(filled[["height"]])
  expect_true(filled[["sizingPolicy"]][["viewer"]][["fill"]])
  expect_true(filled[["sizingPolicy"]][["browser"]][["fill"]])

  tall <- draw_bar(c(A = 1, B = 2), height = 300)
  expect_identical(tall[["width"]], "100%")
  expect_identical(tall[["height"]], 300)

  fixed <- draw_bar(c(A = 1, B = 2), width = 700, height = 300)
  expect_identical(fixed[["width"]], 700)
  expect_false(fixed[["sizingPolicy"]][["viewer"]][["fill"]])
})

test_that("default panels fill viewers and span document columns", {
  plots <- list(draw_bar(c(A = 1)), draw_bar(c(B = 2)), draw_bar(c(C = 3)))
  w <- draw_panels(plots, ncol = 2, gap = 20, padding = 8)
  expect_null(w[["width"]])
  expect_null(w[["height"]])
  expect_true(w[["sizingPolicy"]][["viewer"]][["fill"]])
  expect_identical(w[["x"]][["exportSize"]], list(width = 900, height = 756))
  sizing <- getFromNamespace("resolveSizing", "htmlwidgets")(
    w,
    w[["sizingPolicy"]],
    standalone = FALSE,
    knitrOptions = list(out.width.px = 672, out.height.px = 480)
  )
  expect_identical(sizing[["width"]], "100%")
  expect_identical(sizing[["height"]], 756)
})

test_that("panels keep a stated dimension and derive the other", {
  plots <- list(draw_bar(c(A = 1)), draw_bar(c(B = 2)))
  numeric_width <- draw_panels(plots, width = 700)
  expect_identical(numeric_width[["width"]], 700)
  expect_identical(numeric_width[["height"]], 732)
  expect_identical(numeric_width[["x"]][["exportSize"]][["width"]], 700)
  relative <- draw_panels(plots, width = "100%")
  expect_identical(relative[["height"]], 732)
  expect_identical(relative[["x"]][["exportSize"]][["width"]], 900)
  stated <- draw_panels(plots, height = 400)
  expect_identical(stated[["width"]], "100%")
  expect_identical(stated[["height"]], 400)
})

test_that("container-following charts export at their declared size", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  svg_size <- function(widget, ...) {
    save_drawing(widget, path, ...)
    text <- paste(readLines(path, warn = FALSE), collapse = " ")
    regmatches(text, regexpr('width="[0-9.]+" height="[0-9.]+"', text))
  }
  panels <- draw_panels(list(draw_bar(c(A = 1)), draw_bar(c(B = 2))))
  expect_identical(svg_size(panels), 'width="900" height="732"')
  expect_identical(
    svg_size(panels, width = 640, height = 360),
    'width="640" height="360"'
  )
  expect_identical(svg_size(draw_bar(c(A = 1))), 'width="800" height="600"')
  expect_identical(
    svg_size(draw_bar(c(A = 1), width = 640, height = 360)),
    'width="640" height="360"'
  )
})

test_that("curve charts fill the host and keep their square export size", {
  y <- rep(c("no", "yes"), each = 4L)
  p <- c(.1, .3, .4, .8, .2, .6, .7, .9)
  default <- draw_roc(y, p)
  expect_null(default[["width"]])
  expect_null(default[["height"]])
  grid <- default[["x"]][["option"]][["grid"]]
  expect_identical(
    default[["x"]][["exportSize"]],
    list(
      width = 480 + grid[["left"]] + grid[["right"]],
      height = 480 + grid[["top"]] + grid[["bottom"]]
    )
  )
  narrow <- draw_roc(y, p, width = 500)
  expect_identical(narrow[["width"]], 500)
  expect_identical(
    narrow[["height"]],
    default[["x"]][["exportSize"]][["height"]]
  )
  short <- draw_roc(y, p, height = 400)
  expect_identical(short[["height"]], 400)
  expect_identical(short[["width"]], default[["x"]][["exportSize"]][["width"]])
})

test_that("the binding sizes charts from the element and the host height", {
  skip_if_no_node()
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "widget_sizing.js")),
      shQuote(system.file(
        "htmlwidgets/rtemis-draw.js",
        package = "rtemis.draw"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})
