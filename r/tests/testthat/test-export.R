# test-export.R
# Tests for save_drawing(). The SVG path itself needs a `node` binary, so the
# node-free tests here cover the guards and, importantly, that a rejected
# export leaves nothing behind at the caller's path.

test_that("save_drawing rejects a non-widget", {
  expect_error(save_drawing(list(), tempfile(fileext = ".svg")), "htmlwidget")
})


test_that("save_drawing requires an extension", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  expect_error(save_drawing(w, file.path(tempdir(), "chart")), "extension")
})


test_that("an unsupported format errors without creating the file", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".png")
  expect_error(save_drawing(w, path), "only supports")
  expect_false(file.exists(path))
})


test_that("a failed SVG export leaves no file at the destination", {
  skip_if(
    nzchar(Sys.which("node")),
    "node is installed, so the export succeeds"
  )
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".svg")
  expect_error(save_drawing(w, path), "Node.js")
  expect_false(file.exists(path))
})


test_that("save_drawing writes the SVG and returns its path invisibly", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  expect_invisible(save_drawing(w, path))
  expect_true(file.exists(path))
  expect_match(readLines(path, n = 1L, warn = FALSE), "svg")
})


test_that("strip_js drops JS()-wrapped values at any depth", {
  x <- list(
    a = 1,
    formatter = htmlwidgets::JS("function (p) { return p.value; }"),
    nested = list(b = 2, cb = htmlwidgets::JS("function () {}"))
  )
  out <- strip_js(x)
  expect_equal(out[["a"]], 1)
  expect_null(out[["formatter"]])
  expect_equal(out[["nested"]][["b"]], 2)
  expect_null(out[["nested"]][["cb"]])
})
