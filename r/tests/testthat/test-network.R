# test-network.R
# Tests for GraphNode, GraphEdge, GraphModel, the model builders
# (graph_from_matrix / graph_from_edge_list), draw_graph(), and draw_network().

# -- GraphNode ------------------------------------------------------------------

test_that("GraphNode requires an id and serializes camelCase-free fields", {
  n <- GraphNode(id = "a", label = "Node A", value = 3, group = "g1")
  expect_true(S7::S7_inherits(n, GraphNode))
  out <- to_list(n)
  expect_equal(out[["id"]], "a")
  expect_equal(out[["label"]], "Node A")
  expect_equal(out[["value"]], 3)
  expect_equal(out[["group"]], "g1")
})

test_that("GraphNode drops NULL fields", {
  out <- to_list(GraphNode(id = "x"))
  expect_equal(names(out), "id")
})

test_that("GraphNode rejects a missing / non-scalar id", {
  expect_error(GraphNode())
  expect_error(GraphNode(id = c("a", "b")))
})

# -- GraphEdge ------------------------------------------------------------------

test_that("GraphEdge serializes source/target/weight/sign", {
  e <- GraphEdge(source = "a", target = "b", weight = 0.8, sign = -1)
  out <- to_list(e)
  expect_equal(out[["source"]], "a")
  expect_equal(out[["target"]], "b")
  expect_equal(out[["weight"]], 0.8)
  expect_equal(out[["sign"]], -1)
})

test_that("GraphEdge rejects an invalid sign", {
  expect_error(GraphEdge(source = "a", target = "b", sign = 2))
  expect_error(GraphEdge(source = "a", target = "b", sign = 0))
})

# -- GraphModel -----------------------------------------------------------------

test_that("GraphModel assembles nodes, edges, directed", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a"), GraphNode(id = "b")),
    edges = list(GraphEdge(source = "a", target = "b")),
    directed = TRUE
  )
  out <- to_list(m)
  expect_length(out[["nodes"]], 2L)
  expect_length(out[["edges"]], 1L)
  expect_true(out[["directed"]])
  # nodes/edges must serialize as unnamed arrays
  expect_null(names(out[["nodes"]]))
  expect_equal(out[["nodes"]][[1L]][["id"]], "a")
})

test_that("GraphModel rejects non-GraphNode nodes", {
  expect_error(GraphModel(nodes = list(list(id = "a"))))
})

# -- graph_from_matrix ----------------------------------------------------------

test_that("graph_from_matrix builds an undirected graph from the upper triangle", {
  m <- matrix(c(0, 1, 2, 1, 0, 0, 2, 0, 0), nrow = 3)
  rownames(m) <- colnames(m) <- c("a", "b", "c")
  g <- graph_from_matrix(m)
  out <- to_list(g)
  expect_false(out[["directed"]])
  expect_length(out[["nodes"]], 3L)
  # upper triangle non-zero entries: (a,b)=1, (a,c)=2 -> 2 edges
  expect_length(out[["edges"]], 2L)
  # node value defaults to weighted degree: a connects to b(1)+c(2)=3
  vals <- vapply(out[["nodes"]], function(n) n[["value"]], numeric(1))
  names(vals) <- vapply(out[["nodes"]], function(n) n[["id"]], character(1))
  expect_equal(vals[["a"]], 3)
})

test_that("graph_from_matrix encodes sign and respects threshold", {
  m <- matrix(c(0, -0.9, 0.2, -0.9, 0, 0, 0.2, 0, 0), nrow = 3)
  rownames(m) <- colnames(m) <- c("a", "b", "c")
  g <- graph_from_matrix(m, threshold = 0.5)
  out <- to_list(g)
  # only |w| >= 0.5 kept -> the (a,b) = -0.9 edge
  expect_length(out[["edges"]], 1L)
  expect_equal(out[["edges"]][[1L]][["sign"]], -1)
  expect_equal(out[["edges"]][[1L]][["weight"]], 0.9)
})

test_that("graph_from_matrix directed reads all off-diagonal entries", {
  m <- matrix(c(0, 1, 0, 2, 0, 3, 0, 0, 0), nrow = 3)
  g <- graph_from_matrix(m, directed = TRUE)
  out <- to_list(g)
  expect_true(out[["directed"]])
  # off-diagonal non-zero: [2,1]=1, [1,2]=2, [3,2]=3 -> 3 directed edges
  expect_length(out[["edges"]], 3L)
})

test_that("graph_from_matrix rejects a non-square matrix", {
  expect_error(graph_from_matrix(matrix(1:6, nrow = 2)), "square")
})

# -- graph_from_edge_list -------------------------------------------------------

test_that("graph_from_edge_list reads named columns", {
  edges <- data.frame(
    source = c("a", "a", "b"),
    target = c("b", "c", "c"),
    weight = c(1, 2, 0.5),
    stringsAsFactors = FALSE
  )
  g <- graph_from_edge_list(edges)
  out <- to_list(g)
  expect_length(out[["edges"]], 3L)
  # three distinct node ids discovered from the edges
  expect_length(out[["nodes"]], 3L)
})

test_that("graph_from_edge_list falls back to positional columns", {
  edges <- data.frame(from = c("a"), to = c("b"), stringsAsFactors = FALSE)
  g <- graph_from_edge_list(edges)
  out <- to_list(g)
  expect_equal(out[["edges"]][[1L]][["source"]], "a")
  expect_equal(out[["edges"]][[1L]][["target"]], "b")
})

test_that("graph_from_edge_list adds edge-referenced nodes missing from node table", {
  edges <- data.frame(
    source = "a",
    target = "z",
    stringsAsFactors = FALSE
  )
  nodes <- data.frame(id = "a", group = "g1", stringsAsFactors = FALSE)
  g <- graph_from_edge_list(edges, nodes = nodes)
  out <- to_list(g)
  ids <- vapply(out[["nodes"]], function(n) n[["id"]], character(1))
  expect_setequal(ids, c("a", "z"))
})

# -- draw_graph / draw_network --------------------------------------------------

test_that("draw_network returns an htmlwidget for matrix input", {
  m <- matrix(c(0, 0.8, 0.8, 0), nrow = 2)
  rownames(m) <- colnames(m) <- c("a", "b")
  w <- draw_network(m)
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "rtemis.draw")
  # payload carries the model + style for the JS binding
  expect_length(w$x$model$nodes, 2L)
  expect_equal(w$x$style$layout, "force")
})

test_that("draw_network returns an htmlwidget for edge-list input", {
  edges <- data.frame(
    source = c("a", "b"),
    target = c("b", "c"),
    stringsAsFactors = FALSE
  )
  w <- draw_network(edges, layout = "circular", color_by_group = TRUE)
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$style$layout, "circular")
  expect_true(w$x$style$colorByGroup)
})

test_that("draw_network rejects unsupported input types", {
  expect_error(draw_network(1:10), "matrix")
  expect_error(draw_network("nope"), "matrix")
})

test_that("draw_graph auto-theme payload carries light and dark themes", {
  m <- matrix(c(0, 1, 1, 0), nrow = 2)
  w <- draw_graph(graph_from_matrix(m))
  expect_true(w$x$autoTheme)
  expect_false(is.null(w$x$theme))
  expect_false(is.null(w$x$themeDark))
})

# -- SigmaOption ----------------------------------------------------------------

test_that("SigmaOption applies sensible style defaults", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a"), GraphNode(id = "b")),
    edges = list(GraphEdge(source = "a", target = "b")),
    directed = FALSE
  )
  opt <- SigmaOption(model = m)
  expect_true(S7::S7_inherits(opt, SigmaOption))
  expect_equal(opt@layout, "force")
  expect_equal(opt@node_size, 10)
  expect_equal(opt@node_opacity, 0.95)
  expect_true(opt@scale_by_degree)
  expect_false(opt@color_by_group)
  expect_equal(opt@node_color, rtemis_colors[[1L]])
  expect_equal(opt@negative_color, "#ff9e1f")
})

test_that("SigmaOption to_list produces the {model, style} payload shape", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a"), GraphNode(id = "b")),
    edges = list(GraphEdge(source = "a", target = "b", weight = 0.5)),
    directed = FALSE
  )
  out <- to_list(SigmaOption(model = m, layout = "circular", node_size = 20))
  expect_named(out, c("model", "style"))
  expect_length(out$model$nodes, 2L)
  # style keys are camelCase for the JS binding
  expect_equal(out$style$layout, "circular")
  expect_equal(out$style$nodeSize, 20)
  expect_true("scaleByDegree" %in% names(out$style))
  # title omitted when NULL
  expect_false("title" %in% names(out))
})

test_that("SigmaOption serializes a title when present", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a")),
    edges = list(),
    directed = FALSE
  )
  out <- to_list(SigmaOption(model = m, title = "My Network"))
  expect_equal(out$title, "My Network")
})

test_that("SigmaOption accepts a plain list model but rejects a model without nodes", {
  expect_no_error(SigmaOption(model = list(nodes = list(), edges = list())))
  expect_error(SigmaOption(model = list(edges = list())), "nodes")
  expect_error(SigmaOption(model = 42), "GraphModel")
})

test_that("SigmaOption rejects an invalid layout", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a")),
    edges = list(),
    directed = FALSE
  )
  expect_error(SigmaOption(model = m, layout = "spiral"))
})

# -- draw() generic dispatch ----------------------------------------------------

test_that("draw() dispatches a SigmaOption to the rtemis-graph backend", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a"), GraphNode(id = "b")),
    edges = list(GraphEdge(source = "a", target = "b")),
    directed = FALSE
  )
  w <- draw(SigmaOption(model = m))
  expect_s3_class(w, "htmlwidget")
  # Sigma payload shape (model/style), not the ECharts shape (option)
  expect_false(is.null(w$x$model))
  expect_false(is.null(w$x$style))
  expect_null(w$x$option)
  # theme resolved uniformly by draw(), not carried on the option
  expect_true(w$x$autoTheme)
})

test_that("draw(SigmaOption) honours an explicit theme and NA (no theme)", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a")),
    edges = list(),
    directed = FALSE
  )
  wt <- draw(SigmaOption(model = m), theme = theme_dark())
  expect_false(is.null(wt$x$theme))
  expect_null(wt$x$autoTheme)

  wna <- draw(SigmaOption(model = m), theme = NA)
  expect_null(wna$x$theme)
  expect_null(wna$x$autoTheme)
})

test_that("draw(SigmaOption) warns and ignores filename (no static export yet)", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a")),
    edges = list(),
    directed = FALSE
  )
  expect_message(
    draw(SigmaOption(model = m), filename = "net.svg"),
    "not yet supported"
  )
})

test_that("draw() still renders an EChartsOption and a bare list", {
  we <- draw(EChartsOption(series = LineSeries(data = list(1, 2, 3))))
  expect_s3_class(we, "htmlwidget")
  expect_false(is.null(we$x$option))
  expect_equal(we$x$renderer, "canvas")

  wl <- draw(list(series = list(list(type = "line", data = list(1, 2, 3)))))
  expect_s3_class(wl, "htmlwidget")
  expect_false(is.null(wl$x$option))
})

test_that("graph_from_edge_list assigns 0 (not NA) to isolated nodes", {
  # "iso" appears in the node table but in no edge: its weighted degree is
  # absent from the degree vector, which must coalesce to 0 rather than NA.
  edges <- data.frame(source = "a", target = "b", weight = 2)
  nodes <- data.frame(id = c("a", "b", "iso"))
  m <- graph_from_edge_list(edges, nodes = nodes)
  vals <- vapply(m@nodes, function(n) n@value, numeric(1L))
  expect_false(anyNA(vals))
  iso <- Filter(function(n) n@id == "iso", m@nodes)[[1L]]
  expect_equal(iso@value, 0)
})

test_that("SigmaOption strips names from color scalars when serializing", {
  m <- GraphModel(
    nodes = list(GraphNode(id = "a")),
    edges = list(),
    directed = FALSE
  )
  opt <- SigmaOption(model = m, node_color = c(primary = "#6CA3A0"))
  lst <- to_list(opt)
  expect_null(names(lst$style$nodeColor))
  expect_identical(lst$style$nodeColor, "#6CA3A0")
})
