test_that("supplied trees align identities, notes and tracks through one permutation", {
  x <- matrix(
    c(9, 1, 7, 2, 8, 0, 6, 3, 7, 1, 4, 4),
    4,
    dimnames = list(LETTERS[1:4], letters[1:3])
  )
  tree <- hclust(dist(x[c(3, 1, 4, 2), ]))
  notes <- matrix(paste0("note", seq_along(x)), 4, dimnames = dimnames(x))
  colors <- matrix(
    c("red", "blue", "green", "black"),
    4,
    dimnames = list(rownames(x), "class")
  )
  record <- list(
    values = x,
    row_tree = tree,
    cell_notes = notes[4:1, 3:1],
    row_colors = colors[4:1, , drop = FALSE]
  )
  normalized <- heatmap_input(record)
  expect_equal(normalized[["cell_notes"]], notes)
  expected_order <- match(tree[["labels"]][tree[["order"]]], rownames(x))
  expect_equal(normalized[["row_tree"]][["order"]], expected_order)
  dendro <- heatmap_tree(as.dendrogram(tree), rownames(x))
  expect_equal(dendro[["order"]], expected_order)
  plain <- unclass(tree)
  expect_equal(heatmap_tree(plain, rownames(x))[["order"]], expected_order)
  config <- setup_HeatmapConfig(show_notes = TRUE, row_cut = 2L)
  opt <- to_list(compile(config, data = record))
  expect_equal(
    opt[["yAxis"]][[2]][["data"]],
    as.list(rownames(x)[expected_order])
  )
  cells <- opt[["series"]][[2]][["data"]]
  expect_equal(
    vapply(cells, function(cell) cell[["value"]][[3]], numeric(1)),
    as.numeric(t(x[expected_order, ]))
  )
  expect_equal(
    vapply(cells, `[[`, character(1), "note"),
    as.character(t(notes[expected_order, ]))
  )
  expect_equal(cells[[1]][["name"]], notes[expected_order[[1]], 1])
  expect_equal(cells[[1]][["label"]][["formatter"]], "{b}")
  expect_equal(
    opt[["series"]][[3]][["itemPayload"]][["colors"]],
    lapply(expected_order, function(i) list(normalized[["row_colors"]][i, 1]))
  )
  expect_equal(opt[["visualMap"]][["seriesIndex"]], 1L)
  expect_equal(strip_js(opt)[["series"]], opt[["series"]])
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_chart_config(config, path)
  expect_equal(read_chart_config(path)@row_cut, 2L)
  # Supplied trees take precedence over the clustering switch.
  other <- to_list(heatmap_option(x = x, row_tree = tree, cluster_rows = TRUE)[[
    "option"
  ]])
  expect_equal(other[["yAxis"]][[2]][["data"]], opt[["yAxis"]][[2]][["data"]])
})

test_that("heatmap annotation validation rejects ambiguity and malformed trees", {
  x <- matrix(1:12, 4, dimnames = list(LETTERS[1:4], letters[1:3]))
  tree <- hclust(dist(x))
  for (bad in list(
    matrix(numeric(), 0, 2),
    matrix(NA_real_, 2, 2),
    matrix(Inf, 2, 2),
    matrix(1i, 2, 2)
  )) {
    expect_error(heatmap_input(bad), "numeric matrix")
  }
  expect_error(heatmap_input(x, cell_notes = x), "character cell_notes")
  expect_error(heatmap_input(x, row_colors = c("red", "blue")), "color tracks")
  expect_error(
    heatmap_input(x, row_colors = rep("not-a-color", 4)),
    "valid R colors"
  )
  expect_error(heatmap_input(list(values = x, other = 1)), "record")
  expect_error(
    heatmap_input(list(values = x, row_tree = tree), row_tree = tree),
    "once"
  )
  expect_error(
    heatmap_input(x, row_tree = hclust(dist(matrix(1:9, 3)))),
    "tree"
  )
  expect_error(
    heatmap_tree(within(unclass(tree), labels <- letters[1:4]), rownames(x)),
    "labels"
  )
  expect_error(
    heatmap_tree(within(unclass(tree), order <- rev(order)), rownames(x)),
    "valid"
  )
  expect_error(
    heatmap_tree(within(unclass(tree), merge[1, 1] <- 1), rownames(x)),
    "valid"
  )
  expect_error(
    heatmap_tree(within(unclass(tree), height[1] <- Inf), rownames(x)),
    "valid"
  )
  expect_error(
    heatmap_tree(
      within(unclass(tree), merge[1, 1] <- merge[1, 2]),
      rownames(x)
    ),
    "valid"
  )
  expect_error(heatmap_option(x = x, show_notes = TRUE), "show_notes")
  expect_error(
    heatmap_option(
      x = x,
      show_values = TRUE,
      show_notes = TRUE,
      cell_notes = matrix("a", 4, 3)
    ),
    "choose"
  )
  expect_error(heatmap_option(x = x, row_cut = 2), "tree cut")
  expect_error(heatmap_option(x = x, row_tree = tree, row_cut = 5), "tree cut")
  expect_error(
    heatmap_option(x = x, row_tree = tree, triangle = "lower"),
    "full matrix"
  )
  expect_error(heatmap_option(x = x, row_names = "A"), "display label")
  expect_error(setup_HeatmapConfig(row_cut = 0))
  expect_error(setup_HeatmapConfig(show_notes = NA))
})

test_that("empty-row slots and column slots remain aligned with dendrogram leaves", {
  x <- matrix(c(1, NA, 9, 3, 2, NA, 8, 4, NA, NA, NA, NA), 4)
  built <- to_list(heatmap_option(
    x = x,
    cluster_rows = TRUE,
    cluster_cols = TRUE
  )[["option"]])
  segments <- built[["series"]][[1]][["data"]]
  leaves <- unlist(lapply(segments, function(seg) {
    c(if (seg[[3]] == 0) seg[[1]], if (seg[[4]] == 0) seg[[2]])
  }))
  expect_setequal(leaves, c(0, 2, 3))
  expect_length(built[["yAxis"]][[3]][["data"]], 4L)
  column <- built[["series"]][[2]][["data"]][[1]]
  expect_equal(unlist(column[1:2]), c(0, 1))
  # Triangle masking subsets annotations before clustering and preserves cells.
  square <- matrix(1:16, 4)
  notes <- matrix(paste0("n", 1:16), 4)
  opt <- to_list(heatmap_option(
    x = square,
    triangle = "lower",
    cell_notes = notes,
    row_colors = rep("red", 4),
    col_colors = rep("blue", 4)
  )[["option"]])
  expect_equal(opt[["series"]][[1]][["data"]][[1]][["note"]], "n2")
  expect_length(opt[["series"]][[2]][["data"]], 3L)
  expect_length(opt[["series"]][[3]][["data"]], 3L)
})

test_that("branch colors are stable in displayed cluster order and respect cuts", {
  tree <- hclust(dist(c(0, 1, 9, 10)))
  colors <- heatmap_branch_colors(tree, 2L, "gray")
  expect_length(unique(unlist(colors)), 3L)
  expect_equal(colors[[3]], "gray")
  expect_equal(heatmap_branch_colors(tree, 4L, "gray"), rep(list("gray"), 3))
  expect_length(unique(unlist(heatmap_branch_colors(tree, 1L, "gray"))), 1L)
  expect_error(heatmap_branch_colors(tree, 1.5, "gray"), "tree cut")
  zero <- hclust(dist(rep(1, 3)))
  expect_gt(hclust_to_dendro_data(zero)[["max_height"]], 0)
})

test_that("annotated heatmaps preserve native vector geometry after resizing", {
  skip_if_no_node()
  x <- matrix(c(1, 2, 8, 9, 3, 2, 7, 10, 5, 4, 8, 7), 4)
  charts <- list()
  for (theme in list(theme_light(), theme_dark())) {
    for (side in c("top", "bottom")) {
      w <- draw_heatmap(
        x,
        row_tree = hclust(dist(x)),
        col_tree = hclust(dist(t(x))),
        cell_notes = matrix("note {b}", 4, 3),
        show_notes = TRUE,
        row_colors = cbind(c("red", "blue", "red", "blue"), rep("black", 4)),
        col_colors = c("green", "green", "orange"),
        row_cut = 2L,
        col_cut = 2L,
        dendro_col_side = side,
        square_cells = FALSE,
        theme = theme
      )
      charts[[length(charts) + 1L]] <- list(
        option = strip_js(w[["x"]][["option"]]),
        theme = to_list(theme)
      )
    }
  }
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input))
  jsonlite::write_json(
    list(
      charts = charts,
      panels = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      ),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      renderers = system.file(
        "htmlwidgets/lib/draw/renderers.js",
        package = "rtemis.draw"
      )
    ),
    input,
    auto_unbox = TRUE,
    digits = NA
  )
  result <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "heatmap_annotations.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "8 annotated heatmap geometry cases passed"
  )
})
