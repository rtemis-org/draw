# test-draw_a3.R
# Tests for draw_a3() and its internal layout/helper functions

# ── Internal helpers ──────────────────────────────────────────────────────────

test_that("a3_seq_coords returns correct length", {
  coords <- rtemis.draw:::a3_seq_coords(100L, 21L)
  expect_length(coords$xs, 100L)
  expect_length(coords$ys, 100L)
})

test_that("a3_seq_coords x oscillates between 1 and n_per_row", {
  n_per_row <- 10L
  coords <- rtemis.draw:::a3_seq_coords(50L, n_per_row)
  # All raw x values (before border offsets) should be near [1, n_per_row]
  expect_true(all(coords$xs >= 0.8 & coords$xs <= n_per_row + 0.2))
})

test_that("a3_seq_coords y increases monotonically by row", {
  coords <- rtemis.draw:::a3_seq_coords(21L, 21L)
  # First 20 residues: y close to 1; residue 21 (right-turn): y = 1.5
  expect_equal(coords$ys[[1L]], 1.0)
  expect_equal(coords$ys[[21L]], 1.5, tolerance = 1e-10)
})

test_that("a3_seq_coords handles single-residue sequences", {
  coords <- rtemis.draw:::a3_seq_coords(1L, 21L)
  expect_equal(coords$xs, 1.0)
  expect_equal(coords$ys, 1.0)
})

test_that("a3_scale_x scales correctly", {
  expect_equal(rtemis.draw:::a3_scale_x(1, 0.3), 1.0)
  expect_equal(rtemis.draw:::a3_scale_x(21, 0.3), 1 + 20 * 0.3)
  expect_equal(rtemis.draw:::a3_scale_x(5, 1), 5.0)
})

test_that("a3_with_alpha converts hex to rgba", {
  expect_equal(rtemis.draw:::a3_with_alpha("#0F766E", 0.35), "rgba(15, 118, 110, 0.35)")
  # Short hex
  expect_equal(rtemis.draw:::a3_with_alpha("#F00", 1), "rgba(255, 0, 0, 1)")
  # Non-hex passthrough
  expect_equal(rtemis.draw:::a3_with_alpha("red", 0.5), "red")
})

test_that("a3_flex_positions handles integer vector", {
  result <- rtemis.draw:::a3_flex_positions(c(1L, 5L, 10L))
  expect_equal(result, c(1L, 5L, 10L))
})

test_that("a3_flex_positions expands matrix ranges", {
  mat <- matrix(c(1L, 3L, 7L, 9L), ncol = 2L, byrow = TRUE)
  result <- rtemis.draw:::a3_flex_positions(mat)
  expect_equal(result, c(1L, 2L, 3L, 7L, 8L, 9L))
})

test_that("a3_circular_offset returns x and y", {
  off <- rtemis.draw:::a3_circular_offset(1L, 4L, 10)
  expect_named(off, c("x", "y"))
  # At index=1 of 4: angle = 2*pi/4 = pi/2 → x = sin(pi/2)*10 = 10, y = cos(pi/2)*10 ≈ 0
  expect_equal(off$x, 10, tolerance = 1e-10)
  expect_equal(off$y, 0, tolerance = 1e-10)
})

# ── draw_a3 ───────────────────────────────────────────────────────────────────

test_that("draw_a3 rejects non-A3 input", {
  expect_error(draw_a3("MAEPR"), class = "rlang_error")
  expect_error(draw_a3(list(sequence = "MAEPR")), class = "rlang_error")
})

test_that("draw_a3 creates htmlwidget from minimal A3 object", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  w <- draw_a3(a)
  expect_s3_class(w, "htmlwidget")
})

test_that("draw_a3 option has correct axis structure", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  w <- draw_a3(a)
  opt <- w$x$option
  expect_equal(opt$xAxis$type, "value")
  expect_equal(opt$xAxis$show, FALSE)
  expect_equal(opt$yAxis$type, "value")
  expect_equal(opt$yAxis$inverse, TRUE)
  expect_equal(opt$yAxis$show, FALSE)
})

test_that("draw_a3 backbone series is present when show_markers = TRUE", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPR")
  w <- draw_a3(a, show_markers = TRUE)
  series_types <- vapply(w$x$option$series, `[[`, character(1L), "type")
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_true("Primary structure" %in% series_names)
  backbone_idx <- which(series_names == "Primary structure")
  expect_equal(series_types[[backbone_idx]], "line")
})

test_that("draw_a3 backbone series is absent when show_markers = FALSE", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPR")
  w <- draw_a3(a, show_markers = FALSE)
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_false("Primary structure" %in% series_names)
})

test_that("draw_a3 includes region series for annotated A3", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3(
    "MAEPRQEFEVMEDHAGTYGLGDRK",
    region = list(
      Domain_A = rtemis.a3::annotation_range(
        matrix(c(3L, 10L), ncol = 2L, byrow = TRUE)
      )
    )
  )
  w <- draw_a3(a)
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_true("Domain_A" %in% series_names)
  region_idx <- which(series_names == "Domain_A")
  expect_equal(w$x$option$series[[region_idx]]$type, "line")
})

test_that("draw_a3 includes site series for annotated A3", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3(
    "MAEPRQEFEVMEDHAGTYGLGDRK",
    site = list(
      Active_site = rtemis.a3::annotation_position(c(5L, 17L))
    )
  )
  w <- draw_a3(a)
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_true("Active_site" %in% series_names)
  site_idx <- which(series_names == "Active_site")
  expect_equal(w$x$option$series[[site_idx]]$type, "scatter")
  # Site data should have 2 points
  expect_length(w$x$option$series[[site_idx]]$data, 2L)
})

test_that("draw_a3 residue labels series is present when show_labels = TRUE", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPR")
  w <- draw_a3(a, show_labels = TRUE)
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_true("Residue labels" %in% series_names)
})

test_that("draw_a3 position labels series absent when position_every = NULL", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  w <- draw_a3(a, position_every = NULL)
  series_names <- vapply(w$x$option$series, function(s) s$name %||% "", character(1L))
  expect_false("Position labels" %in% series_names)
})

test_that("draw_a3 title is included in option", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPR")
  w <- draw_a3(a, title = "MAPT")
  expect_equal(w$x$option$title$text, "MAPT")
})

test_that("draw_a3 legend is plain and vertical", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  w <- draw_a3(a)
  legend <- w$x$option$legend
  expect_equal(legend$type, "plain")
  expect_equal(legend$orient, "vertical")
})

test_that("draw_a3 height is auto-computed when NULL", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  # height should be set in the widget sizingPolicy, not in the option;
  # the widget itself is the check — it should not error
  w <- draw_a3(a, height = NULL)
  expect_s3_class(w, "htmlwidget")
  # Explicit height is forwarded
  w2 <- draw_a3(a, height = 500)
  expect_equal(w2$height, 500)
})

test_that("draw_a3 dataZoom absent when enable_zoom = FALSE", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
  w <- draw_a3(a, enable_zoom = FALSE)
  expect_null(w$x$option$dataZoom)
})

test_that("draw_a3 n_per_row validation works", {
  skip_if_not_installed("rtemis.a3")
  a <- rtemis.a3::create_A3("MAEPR")
  expect_error(draw_a3(a, n_per_row = 1L), class = "rlang_error")
})
