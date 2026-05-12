# test-series_sankey.R
# Tests for SankeyNodeItem, SankeyEdgeItem, SankeyLevelOption,
# SankeySeries, and draw_sankey()

# -- SankeyNodeItem -------------------------------------------------------------

test_that("SankeyNodeItem creates with defaults", {
  n <- SankeyNodeItem()
  expect_true(S7::S7_inherits(n, SankeyNodeItem))
  out <- to_list(n)
  expect_equal(length(out), 0L)
})

test_that("SankeyNodeItem to_list() converts names correctly", {
  n <- SankeyNodeItem(
    name = "A",
    value = 10,
    depth = 1L,
    draggable = FALSE,
    local_x = 0.1,
    local_y = 0.5
  )
  out <- to_list(n)
  expect_equal(out[["name"]], "A")
  expect_equal(out[["value"]], 10)
  expect_equal(out[["depth"]], 1L)
  expect_equal(out[["draggable"]], FALSE)
  expect_equal(out[["localX"]], 0.1)
  expect_equal(out[["localY"]], 0.5)
})

test_that("SankeyNodeItem focus_node_adjacency validation", {
  expect_equal(
    SankeyNodeItem(focus_node_adjacency = TRUE)@focus_node_adjacency,
    TRUE
  )
  expect_equal(
    SankeyNodeItem(focus_node_adjacency = "inEdges")@focus_node_adjacency,
    "inEdges"
  )
  expect_equal(
    SankeyNodeItem(focus_node_adjacency = "outEdges")@focus_node_adjacency,
    "outEdges"
  )
  expect_equal(
    SankeyNodeItem(focus_node_adjacency = "allEdges")@focus_node_adjacency,
    "allEdges"
  )
  expect_error(SankeyNodeItem(focus_node_adjacency = "bad"))
  expect_error(SankeyNodeItem(focus_node_adjacency = 1))
})

test_that("SankeyNodeItem depth validates non-negative integer scalar", {
  expect_equal(SankeyNodeItem(depth = 0L)@depth, 0L)
  expect_equal(SankeyNodeItem(depth = 2L)@depth, 2L)
  expect_error(SankeyNodeItem(depth = -1L))
  expect_error(SankeyNodeItem(depth = 1.5))
})

test_that("SankeyNodeItem NULL fields are dropped", {
  n <- SankeyNodeItem(name = "X")
  out <- to_list(n)
  expect_true("name" %in% names(out))
  expect_false("value" %in% names(out))
  expect_false("depth" %in% names(out))
})

# -- SankeyEdgeItem -------------------------------------------------------------

test_that("SankeyEdgeItem creates with defaults", {
  e <- SankeyEdgeItem()
  expect_true(S7::S7_inherits(e, SankeyEdgeItem))
  out <- to_list(e)
  expect_equal(length(out), 0L)
})

test_that("SankeyEdgeItem to_list() converts names correctly", {
  e <- SankeyEdgeItem(source = "A", target = "B", value = 5)
  out <- to_list(e)
  expect_equal(out[["source"]], "A")
  expect_equal(out[["target"]], "B")
  expect_equal(out[["value"]], 5)
})

test_that("SankeyEdgeItem accepts plain list for line_style (curveness)", {
  e <- SankeyEdgeItem(
    source = "A",
    target = "B",
    value = 3,
    line_style = list(curveness = 0.5, opacity = 0.3)
  )
  out <- to_list(e)
  expect_equal(out[["lineStyle"]][["curveness"]], 0.5)
  expect_equal(out[["lineStyle"]][["opacity"]], 0.3)
})

test_that("SankeyEdgeItem accepts LineStyle S7 object for line_style", {
  e <- SankeyEdgeItem(
    source = "X",
    target = "Y",
    value = 2,
    line_style = LineStyle(opacity = 0.4)
  )
  out <- to_list(e)
  expect_equal(out[["lineStyle"]][["opacity"]], 0.4)
})

# -- SankeyLevelOption ----------------------------------------------------------

test_that("SankeyLevelOption creates with depth", {
  lv <- SankeyLevelOption(depth = 0L)
  expect_true(S7::S7_inherits(lv, SankeyLevelOption))
  out <- to_list(lv)
  expect_equal(out[["depth"]], 0L)
})

test_that("SankeyLevelOption depth is required", {
  expect_error(SankeyLevelOption())
})

test_that("SankeyLevelOption depth rejects negative", {
  expect_error(SankeyLevelOption(depth = -1))
})

test_that("SankeyLevelOption depth rejects non-integer and negative", {
  expect_error(SankeyLevelOption(depth = 1.5))
  expect_error(SankeyLevelOption(depth = -1L))
})

test_that("SankeyLevelOption to_list() includes item_style", {
  lv <- SankeyLevelOption(
    depth = 1L,
    item_style = ItemStyle(color = "#f00")
  )
  out <- to_list(lv)
  expect_equal(out[["depth"]], 1L)
  expect_equal(out[["itemStyle"]][["color"]], "#f00")
})

# -- SankeySeries ---------------------------------------------------------------

test_that("SankeySeries creates with defaults", {
  s <- SankeySeries()
  expect_true(S7::S7_inherits(s, SankeySeries))
  out <- to_list(s)
  expect_equal(out[["type"]], "sankey")
  expect_equal(length(out), 1L)
})

test_that("SankeySeries to_list() sets type = 'sankey'", {
  s <- SankeySeries(name = "flow")
  out <- to_list(s)
  expect_equal(out[["type"]], "sankey")
  expect_equal(out[["name"]], "flow")
})

test_that("SankeySeries data and links serialize as unnamed arrays", {
  s <- SankeySeries(
    data = list(
      list(name = "A"),
      list(name = "B"),
      list(name = "C")
    ),
    links = list(
      list(source = "A", target = "B", value = 5),
      list(source = "A", target = "C", value = 3)
    )
  )
  out <- to_list(s)
  expect_null(names(out[["data"]]))
  expect_null(names(out[["links"]]))
  expect_equal(length(out[["data"]]), 3L)
  expect_equal(length(out[["links"]]), 2L)
  expect_equal(out[["data"]][[1L]][["name"]], "A")
  expect_equal(out[["links"]][[1L]][["source"]], "A")
  expect_equal(out[["links"]][[2L]][["value"]], 3)
})

test_that("SankeySeries data accepts SankeyNodeItem objects", {
  s <- SankeySeries(
    data = list(SankeyNodeItem(name = "X", depth = 0L)),
    links = list(list(source = "X", target = "Y", value = 1))
  )
  out <- to_list(s)
  expect_equal(out[["data"]][[1L]][["name"]], "X")
  expect_equal(out[["data"]][[1L]][["depth"]], 0L)
})

test_that("SankeySeries orient validation", {
  expect_equal(SankeySeries(orient = "vertical")@orient, "vertical")
  expect_equal(SankeySeries(orient = "horizontal")@orient, "horizontal")
  expect_error(SankeySeries(orient = "diagonal"))
})

test_that("SankeySeries sort validation", {
  expect_equal(SankeySeries(sort = "desc")@sort, "desc")
  expect_null(SankeySeries(sort = NULL)@sort)
  expect_error(SankeySeries(sort = "asc"))
})

test_that("SankeySeries roam validation", {
  expect_equal(SankeySeries(roam = TRUE)@roam, TRUE)
  expect_equal(SankeySeries(roam = "move")@roam, "move")
  expect_equal(SankeySeries(roam = "scale")@roam, "scale")
  expect_error(SankeySeries(roam = "zoom"))
})

test_that("SankeySeries levels serialize as unnamed array", {
  s <- SankeySeries(
    levels = list(
      SankeyLevelOption(depth = 0L, item_style = ItemStyle(color = "#f00")),
      SankeyLevelOption(depth = 1L)
    )
  )
  out <- to_list(s)
  expect_null(names(out[["levels"]]))
  expect_equal(length(out[["levels"]]), 2L)
  expect_equal(out[["levels"]][[1L]][["depth"]], 0)
  expect_equal(out[["levels"]][[1L]][["itemStyle"]][["color"]], "#f00")
})

test_that("SankeySeries line_style accepts plain list with curveness", {
  s <- SankeySeries(line_style = list(curveness = 0.5, opacity = 0.2))
  out <- to_list(s)
  expect_equal(out[["lineStyle"]][["curveness"]], 0.5)
})

test_that("SankeySeries NULL fields are dropped", {
  s <- SankeySeries(orient = "horizontal")
  out <- to_list(s)
  expect_true("orient" %in% names(out))
  expect_false("name" %in% names(out))
  expect_false("data" %in% names(out))
})

# -- draw_sankey ----------------------------------------------------------------

test_that("draw_sankey returns an htmlwidget", {
  links <- data.frame(
    source = c("A", "A", "B"),
    target = c("B", "C", "C"),
    value = c(5, 3, 4),
    stringsAsFactors = FALSE
  )
  w <- draw_sankey(links)
  expect_true(inherits(w, "htmlwidget"))
})

test_that("draw_sankey derives nodes from links automatically", {
  links <- data.frame(
    source = c("X", "Y"),
    target = c("Y", "Z"),
    value = c(10, 6),
    stringsAsFactors = FALSE
  )
  w <- draw_sankey(links)
  opt <- w[["x"]][["option"]]
  node_names <- vapply(opt[["series"]][[1L]][["data"]], `[[`, "", "name")
  expect_setequal(node_names, c("X", "Y", "Z"))
})

test_that("draw_sankey rejects non-tabular links", {
  expect_error(draw_sankey(list(source = "A", target = "B", value = 1)))
})

test_that("draw_sankey rejects links missing required columns", {
  bad <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
  expect_error(draw_sankey(bad), regexp = "source")
})

test_that("draw_sankey handles factor source/target columns", {
  links <- data.frame(
    source = factor(c("A", "B"), levels = c("A", "B")),
    target = factor(c("B", "C"), levels = c("B", "C")),
    value = c(5, 3)
  )
  w <- draw_sankey(links)
  opt <- w[["x"]][["option"]]
  node_names <- vapply(opt[["series"]][[1L]][["data"]], `[[`, "", "name")
  expect_setequal(node_names, c("A", "B", "C"))
  expect_type(node_names, "character")
})

test_that("draw_sankey respects orient argument", {
  links <- data.frame(
    source = "A",
    target = "B",
    value = 1,
    stringsAsFactors = FALSE
  )
  w <- draw_sankey(links, orient = "vertical")
  series <- w[["x"]][["option"]][["series"]][[1L]]
  expect_equal(series[["orient"]], "vertical")
})
