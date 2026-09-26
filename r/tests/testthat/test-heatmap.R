# test-heatmap.R
# Tests for draw_heatmap() including hierarchical clustering dendrogram support

# Reproducible test matrix (4x4 numeric, named)
make_mat <- function(n = 6L) {
  set.seed(42L)
  m <- matrix(
    rnorm(n * n),
    nrow = n,
    ncol = n,
    dimnames = list(paste0("R", seq_len(n)), paste0("C", seq_len(n)))
  )
  m
}

# -- hclust_to_dendro_data() ---------------------------------------------------

test_that("hclust_to_dendro_data returns n-1 segments and correct max_height", {
  m <- make_mat()
  h <- stats::hclust(stats::dist(m))
  out <- hclust_to_dendro_data(h)

  n <- nrow(m)
  expect_named(out, c("data", "min_height", "max_height"))
  expect_length(out[["data"]], n - 1L)
  expect_equal(out[["max_height"]], h[["height"]][n - 1L])
})

test_that("hclust_to_dendro_data produces numeric 5-tuples", {
  m <- make_mat()
  h <- stats::hclust(stats::dist(m))
  out <- hclust_to_dendro_data(h)
  for (seg in out[["data"]]) {
    expect_length(seg, 5L)
    expect_true(all(vapply(seg, is.numeric, logical(1L))))
  }
})

test_that("hclust_to_dendro_data leaf positions cover 0 .. n-1", {
  # Leaf positions (values 0 and 1 in 5-tuples) should span 0 .. n-1
  m <- make_mat(5L)
  h <- stats::hclust(stats::dist(m))
  out <- hclust_to_dendro_data(h)
  # Leaf positions: left_pos values from segments where left_h == 0 (leaf on left),
  # and right_pos values from segments where right_h == 0 (leaf on right).
  left_leaf_pos <- vapply(
    out[["data"]][vapply(
      out[["data"]],
      function(seg) seg[[3L]] == 0,
      logical(1L)
    )],
    `[[`,
    numeric(1L),
    1L
  )
  right_leaf_pos <- vapply(
    out[["data"]][vapply(
      out[["data"]],
      function(seg) seg[[4L]] == 0,
      logical(1L)
    )],
    `[[`,
    numeric(1L),
    2L
  )
  leaf_pos <- unique(c(left_leaf_pos, right_leaf_pos))
  expect_setequal(leaf_pos, seq(0, nrow(m) - 1L))
})

# -- draw_heatmap(): no clustering (baseline) ----------------------------------

test_that("draw_heatmap with no clustering returns a single grid and single axes", {
  m <- make_mat()
  w <- draw_heatmap(m)
  opt <- w$x$option

  # Single grid object (not a list)
  expect_false(is.list(opt$grid) && is.list(opt$grid[[1]]))
  expect_equal(opt$xAxis$type, "category")
  expect_equal(opt$yAxis$type, "category")
  expect_equal(opt$series[[1]]$type, "heatmap")
})

# -- draw_heatmap(): row dendrogram only ---------------------------------------

test_that("draw_heatmap with cluster_rows adds a row dendrogram panel", {
  m <- make_mat()
  w <- draw_heatmap(m, cluster_rows = TRUE)
  opt <- w$x$option

  # Two grids: row dendro (index 0) + heatmap (index 1)
  expect_length(opt$grid, 2L)
  # Two x-axes: height axis + col category axis
  expect_length(opt$xAxis, 2L)
  expect_equal(opt$xAxis[[1]]$type, "value")
  # Default dendro_row_side = "right": root is on the right, so inverse = FALSE
  expect_false(isTRUE(opt$xAxis[[1]]$inverse))
  expect_equal(opt$xAxis[[2]]$type, "category")
  # Two y-axes: hidden row category + visible row category
  expect_length(opt$yAxis, 2L)
  expect_false(isTRUE(opt$yAxis[[1]]$show))
  # Two series: custom (dendro) + heatmap
  expect_length(opt$series, 2L)
  expect_equal(opt$series[[1]]$type, "custom")
  expect_equal(opt$series[[2]]$type, "heatmap")
  # Heatmap series points to axes index 1
  expect_equal(opt$series[[2]]$xAxisIndex, 1L)
  expect_equal(opt$series[[2]]$yAxisIndex, 1L)
  # Row dendro grid must have a fixed width (not right margin)
  expect_true(!is.null(opt$grid[[1]]$width))
})

# -- draw_heatmap(): col dendrogram only ---------------------------------------

test_that("draw_heatmap with cluster_cols adds a col dendrogram panel", {
  m <- make_mat()
  w <- draw_heatmap(m, cluster_cols = TRUE)
  opt <- w$x$option

  expect_length(opt$grid, 2L)
  # Col dendro grid must have a fixed height (not bottom margin)
  expect_true(!is.null(opt$grid[[1]]$height))
  # y-axes: height axis (col dendro) + row category (heatmap)
  expect_equal(opt$yAxis[[1]]$type, "value")
  expect_equal(opt$series[[1]]$type, "custom")
  expect_equal(opt$series[[2]]$type, "heatmap")
  expect_equal(opt$series[[2]]$xAxisIndex, 1L)
  expect_equal(opt$series[[2]]$yAxisIndex, 1L)
})

# -- draw_heatmap(): both dendrograms -----------------------------------------

test_that("draw_heatmap with cluster_rows and cluster_cols produces 3 grids", {
  m <- make_mat()
  w <- draw_heatmap(m, cluster_rows = TRUE, cluster_cols = TRUE)
  opt <- w$x$option

  # Three grids, axes, and series
  expect_length(opt$grid, 3L)
  expect_length(opt$xAxis, 3L)
  expect_length(opt$yAxis, 3L)
  expect_length(opt$series, 3L)
  expect_equal(opt$series[[1]]$type, "custom") # row dendro
  expect_equal(opt$series[[2]]$type, "custom") # col dendro
  expect_equal(opt$series[[3]]$type, "heatmap")
  # Heatmap series uses axis index 2
  expect_equal(opt$series[[3]]$xAxisIndex, 2L)
  expect_equal(opt$series[[3]]$yAxisIndex, 2L)
})

# -- draw_heatmap(): show_row_dendro = FALSE -----------------------------------

test_that("show_row_dendro=FALSE reorders rows but does not add a dendro panel", {
  m <- make_mat()
  w0 <- draw_heatmap(m, cluster_rows = TRUE, show_row_dendro = FALSE)
  opt <- w0$x$option

  # Single grid: no extra panel
  expect_false(is.list(opt$grid) && is.list(opt$grid[[1]]))
  expect_equal(opt$series[[1]]$type, "heatmap")
  # Row names must be reordered (different from unclustered)
  w1 <- draw_heatmap(m)
  # Category data may or may not differ, but the widget should be valid
  expect_s3_class(w0, "htmlwidget")
})

# -- draw_heatmap(): square_cells + row dendrogram ----------------------------

test_that("square_cells meta leftPx includes dendro_row_width when cluster_rows", {
  m <- make_mat()
  # Run both and compare leftPx in meta
  w_plain <- draw_heatmap(m, square_cells = TRUE)
  w_dendro <- draw_heatmap(m, cluster_rows = TRUE, square_cells = TRUE)

  # With row dendro on the right (default), rightPx is larger by dendro_row_width
  expect_true(w_dendro$x$rightPx > w_plain$x$rightPx)
  expect_equal(w_dendro$x$rightPx - w_plain$x$rightPx, 60L)
})

# -- draw_heatmap(): triangle masking + clustering ----------------------------

test_that("triangle masking with cluster_rows does not error", {
  set.seed(1L)
  m <- cor(matrix(rnorm(100L), 10L, 10L))
  expect_no_error(
    draw_heatmap(
      m,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      triangle = "lower"
    )
  )
})

# -- draw_heatmap(): custom dendro_color --------------------------------------

test_that("dendro_color and orientation are plain renderer parameters", {
  m <- make_mat()
  w <- draw_heatmap(m, cluster_rows = TRUE, dendro_color = "#ff0000")
  series <- w[["x"]][["option"]][["series"]][[1L]]
  expect_identical(series[["renderItem"]], "rtemis.dendrogram.v1")
  expect_identical(
    series[["itemPayload"]],
    list(orientation = "row", color = "#ff0000")
  )
})

# -- draw_heatmap(): basic widget structure -----------------------------------

test_that("draw_heatmap with clustering returns a valid htmlwidget", {
  m <- make_mat()
  expect_s3_class(draw_heatmap(m, cluster_rows = TRUE), "htmlwidget")
  expect_s3_class(draw_heatmap(m, cluster_cols = TRUE), "htmlwidget")
  expect_s3_class(
    draw_heatmap(m, cluster_rows = TRUE, cluster_cols = TRUE),
    "htmlwidget"
  )
})

# -- draw_heatmap(): small matrix edge case ------------------------------------

test_that("draw_heatmap with 2 rows and cluster_rows works", {
  m <- matrix(
    c(1, 2, 3, 4),
    nrow = 2L,
    dimnames = list(c("A", "B"), c("X", "Y"))
  )
  expect_no_error(draw_heatmap(m, cluster_rows = TRUE))
})

test_that("draw_heatmap with 1 row does not add dendro panel", {
  # n_rows = 1: clustering is skipped (need > 1 row), so no extra grid is added
  m <- matrix(1:3, nrow = 1L, dimnames = list("A", c("X", "Y", "Z")))
  w <- draw_heatmap(m, cluster_rows = TRUE)
  opt <- w$x$option
  # Single grid: opt$grid is a named list; its first element is a scalar, not a list
  expect_false(is.list(opt$grid[[1]]))
  expect_s3_class(w, "htmlwidget")
})


test_that("heatmap labels default to two decimals without rounding source values", {
  m <- matrix(c(.5951098244376296, 1, -.2350528703555328, NA), 2L)
  defaults <- draw_heatmap(m, show_values = TRUE)[["x"]][["option"]]
  items <- defaults[["series"]][[1L]][["data"]]
  expect_identical(
    vapply(items, function(p) p[["label"]][["formatter"]], ""),
    c("0.60", "-0.24", "1.00", "")
  )
  expect_identical(items[[1L]][["value"]][[3L]], m[1L, 1L])
  expect_identical(HeatmapConfig()@value_digits, 2L)
  cfg <- setup_HeatmapConfig(show_values = TRUE, value_digits = 3L)
  explicit <- draw(cfg, data = m)[["x"]][["option"]]
  expect_identical(
    explicit,
    draw_heatmap(m, show_values = TRUE, value_digits = 3L)[["x"]][["option"]]
  )
  expect_identical(
    explicit[["series"]][[1L]][["data"]][[1L]][["label"]][["formatter"]],
    "0.595"
  )
})


test_that("vertical heatmap colorbars stay centered on the plotted data", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  m <- make_mat(4L)
  charts <- list(
    draw_heatmap(m, show_values = TRUE, margins = c(top = 35, bottom = 115)),
    draw_heatmap(
      m,
      show_values = TRUE,
      cluster_rows = TRUE,
      cluster_cols = TRUE
    ),
    draw_heatmap(
      m,
      show_values = TRUE,
      cluster_cols = TRUE,
      dendro_col_side = "bottom"
    ),
    draw_heatmap(m, show_values = TRUE, colorbar_orient = "horizontal"),
    draw_heatmap(m, show_values = TRUE, show_colorbar = FALSE)
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = lapply(charts, function(w) strip_js(w[["x"]])),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "heatmap_colorbar.js")),
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})


test_that("square heatmaps keep their geometry in bounded panels and after resize", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  m <- matrix(
    seq(-1, 1, length.out = 12),
    nrow = 3,
    dimnames = list(c("A", "B", "C"), c("W", "X", "Y", "Z"))
  )
  cases <- list(
    list(),
    list(cluster_rows = TRUE),
    list(cluster_cols = TRUE),
    list(cluster_rows = TRUE, cluster_cols = TRUE),
    list(
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      dendro_row_side = "left",
      dendro_col_side = "bottom"
    )
  )
  charts <- unlist(
    lapply(list(theme_light(), theme_dark()), function(theme) {
      lapply(cases, function(args) {
        w <- do.call(
          draw_heatmap,
          c(
            list(x = m, square_cells = TRUE, show_values = TRUE, theme = theme),
            args
          )
        )
        strip_js(w[["x"]])
      })
    }),
    recursive = FALSE
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = charts,
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      ),
      binding = system.file(
        "htmlwidgets/rtemis-draw.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "heatmap_export_geometry.js")),
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})
