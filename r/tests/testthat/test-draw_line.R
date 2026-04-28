test_that("draw_line line_style: NULL produces no lineStyle in series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = NULL)
  opt <- w$x$option
  ls <- opt$series[[1]]$lineStyle
  expect_null(ls)
})

test_that("draw_line line_style: single value applied to single series", {
  w <- draw_line(1:3, c(1, 2, 3), line_style = "dashed")
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "dashed")
})

test_that("draw_line line_style: per-series values applied correctly", {
  y <- list(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
  w <- draw_line(1:3, y, line_style = c("solid", "dashed", "dotted"))
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
  expect_equal(opt$series[[3]]$lineStyle$type, "dotted")
})

test_that("draw_line line_style: recycled when fewer values than series", {
  y <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  w <- draw_line(1:3, y, line_style = c("solid", "dashed"))
  opt <- w$x$option
  expect_equal(opt$series[[1]]$lineStyle$type, "solid")
  expect_equal(opt$series[[2]]$lineStyle$type, "dashed")
  expect_equal(opt$series[[3]]$lineStyle$type, "solid")
  expect_equal(opt$series[[4]]$lineStyle$type, "dashed")
})

test_that("draw_line line_style: invalid value errors", {
  expect_error(
    draw_line(1:3, 1:3, line_style = "bold"),
    "line_style"
  )
})

test_that("draw_line line_style: all valid values accepted", {
  for (ls in c("solid", "dashed", "dotted")) {
    expect_no_error(draw_line(1:3, 1:3, line_style = ls))
  }
})
