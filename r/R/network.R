# network.R
# Network / graph plots rendered with Sigma.js (not ECharts).
#
# This is the first non-ECharts rendering surface in rtemis.draw. It mirrors
# rtemislive's GraphCanvas (~/Code/live/src/components/chart/GraphCanvas.tsx),
# which consumes a renderer-agnostic graph model (nodes + edges + directed) and
# manages the sigma instance lifecycle. The S7 classes below are the R analog of
# the TypeScript interfaces GraphNode / GraphEdge / GraphModel in
# ~/Code/live/src/lib/types.ts.
#
# Pipeline:
#   draw_network(x)               # matrix or edge-list -> GraphModel (S7)
#     -> graph_from_matrix() / graph_from_edge_list()
#     -> draw_graph(model)        # GraphModel -> htmlwidget ("rtemis-graph")
#     -> rtemis-graph.js          # graphology + sigma render in the browser

# -- GraphNode ------------------------------------------------------------------

#' Graph Node
#'
#' One node (vertex) in a network. The R analog of the `GraphNode` TypeScript
#' interface in rtemislive (`~/Code/live/src/lib/types.ts`).
#'
#' @param id Character: Unique node identifier. Edges reference nodes by this id.
#' @param label Optional Character: Display label. Defaults to `id` in the
#'   renderer when absent.
#' @param value Optional Numeric: Drives node size when `scale_by_degree` is
#'   enabled (e.g. weighted degree).
#' @param group Optional Character: Categorical key for node color.
#' @export
GraphNode <- S7::new_class(
  "GraphNode",
  properties = list(
    id = character_scalar,
    label = optional_character_scalar,
    value = numeric_or_null_property(),
    group = optional_character_scalar
  )
)

S7::method(to_list, GraphNode) <- function(x, ...) {
  props_to_list(x)
}

# -- GraphEdge ------------------------------------------------------------------

#' Graph Edge
#'
#' One edge (link) in a network. The R analog of the `GraphEdge` TypeScript
#' interface in rtemislive (`~/Code/live/src/lib/types.ts`).
#'
#' @param source Character: Source node id.
#' @param target Character: Target node id.
#' @param weight Optional Numeric: Edge magnitude; drives thickness. For a
#'   correlation graph, the absolute value of `r`.
#' @param sign Optional Numeric \{-1, 1\}: Sign of the underlying value (e.g.
#'   correlation): `1` positive, `-1` negative. Drives edge color when present.
#' @export
GraphEdge <- S7::new_class(
  "GraphEdge",
  properties = list(
    source = character_scalar,
    target = character_scalar,
    weight = numeric_or_null_property(),
    sign = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (!is.numeric(value) || length(value) != 1L || !value %in% c(-1, 1)) {
          return("must be -1, 1, or NULL")
        }
        NULL
      }
    )
  )
)

S7::method(to_list, GraphEdge) <- function(x, ...) {
  props_to_list(x)
}

# -- GraphModel -----------------------------------------------------------------

#' Graph Model
#'
#' A complete, renderer-agnostic network: a list of [GraphNode] objects, a list
#' of [GraphEdge] objects, and a directedness flag. The R analog of the
#' `GraphModel` TypeScript interface in rtemislive
#' (`~/Code/live/src/lib/types.ts`), consumed by the `rtemis-graph` htmlwidget.
#'
#' Most users build this implicitly through [draw_network()]; the constructor is
#' exported for power users who assemble nodes and edges directly.
#'
#' @param nodes List: List of [GraphNode] objects.
#' @param edges List: List of [GraphEdge] objects.
#' @param directed Logical: Whether edges are directed.
#' @export
GraphModel <- S7::new_class(
  "GraphModel",
  properties = list(
    nodes = S7::new_property(class = S7::class_list, default = list()),
    edges = S7::new_property(class = S7::class_list, default = list()),
    directed = logical_scalar
  ),
  validator = function(self) {
    if (
      length(self@nodes) > 0L &&
        !all(vapply(
          self@nodes,
          function(n) {
            S7::S7_inherits(n, GraphNode)
          },
          logical(1)
        ))
    ) {
      return("`nodes` must be a list of GraphNode objects")
    }
    if (
      length(self@edges) > 0L &&
        !all(vapply(
          self@edges,
          function(e) {
            S7::S7_inherits(e, GraphEdge)
          },
          logical(1)
        ))
    ) {
      return("`edges` must be a list of GraphEdge objects")
    }
    NULL
  }
)

S7::method(to_list, GraphModel) <- function(x, ...) {
  list(
    nodes = unname(lapply(x@nodes, to_list)),
    edges = unname(lapply(x@edges, to_list)),
    directed = x@directed
  )
}

# -- SigmaOption ----------------------------------------------------------------

#' Sigma.js Render Option
#'
#' The complete, validated render spec for a Sigma.js network: a [GraphModel]
#' (the data) plus all visual styling and an optional title. This is the Sigma
#' analog of [EChartsOption] -- the single object [draw()] dispatches on to emit
#' a `rtemis-graph` widget. Theme is *not* a property here; like every backend,
#' theming is supplied to [draw()] and resolved uniformly.
#'
#' Most users never touch this directly -- [draw_network()] and [draw_graph()]
#' build it -- but power users can construct it for full control:
#' `draw(SigmaOption(model = GraphModel(...), layout = "circular"))`.
#'
#' Its [to_list()] produces the `{ model, style, title }` payload consumed by
#' the `rtemis-graph` htmlwidget binding.
#'
#' @param model [GraphModel] or named list: The graph to render (a list must
#'   contain a `nodes` element).
#' @param layout Character \{"force", "circular", "circlepack", "random"\}:
#'   Layout algorithm. `"force"` is ForceAtlas2.
#' @param node_size Numeric \[0, Inf): Base node radius in screen pixels.
#' @param edge_scale Numeric \[0, Inf): Multiplier mapping normalized edge weight
#'   to stroke width.
#' @param node_opacity Numeric \[0, 1\]: Node fill opacity.
#' @param edge_opacity Numeric \[0, 1\]: Edge stroke opacity.
#' @param show_labels Logical: Whether to render node labels.
#' @param scale_by_degree Logical: Scale each node's radius by its `value`
#'   (weighted degree); when `FALSE` all nodes use the base size.
#' @param color_by_group Logical: Color nodes by detected community (Louvain)
#'   instead of a single hue.
#' @param resolution Numeric \[0, Inf): Louvain resolution; higher yields more,
#'   smaller communities.
#' @param blend_edges Logical: Color each edge as the blend of its two endpoint
#'   node colors instead of by sign.
#' @param palette Character: Categorical colors for communities.
#' @param node_color Character: Single node color used when `color_by_group` is
#'   `FALSE`.
#' @param positive_color Character: Edge color for positive-sign edges.
#' @param negative_color Character: Edge color for negative-sign edges.
#' @param title Optional Character: Title shown above the network.
#' @export
SigmaOption <- S7::new_class(
  "SigmaOption",
  properties = list(
    # model: a GraphModel, or a plain list already in {nodes, edges, directed}
    # shape (the contract draw_graph() has always accepted).
    model = S7::new_property(
      class = S7::class_any,
      validator = function(value) {
        if (S7::S7_inherits(value, GraphModel)) {
          return(NULL)
        }
        if (is.list(value) && !is.null(value[["nodes"]])) {
          return(NULL)
        }
        "must be a GraphModel or a list with a `nodes` element"
      }
    ),
    layout = S7::new_property(
      S7::class_character,
      default = "force",
      validator = function(value) {
        if (
          length(value) != 1L ||
            !value %in% c("force", "circular", "circlepack", "random")
        ) {
          return(
            "must be one of \"force\", \"circular\", \"circlepack\", \"random\""
          )
        }
        NULL
      }
    ),
    node_size = nonneg_numeric_default(10),
    edge_scale = nonneg_numeric_default(3),
    node_opacity = prob_default(0.95),
    edge_opacity = prob_default(0.4),
    show_labels = logical_default(TRUE),
    scale_by_degree = logical_default(TRUE),
    color_by_group = logical_default(FALSE),
    resolution = nonneg_numeric_default(1),
    blend_edges = logical_default(FALSE),
    palette = S7::new_property(
      S7::class_character,
      default = quote(as.character(rtemis_colors)),
      validator = function(value) {
        if (length(value) == 0L) {
          return("must be a non-empty character vector of colors")
        }
        NULL
      }
    ),
    node_color = S7::new_property(
      S7::class_character,
      default = quote(rtemis_colors[[1L]])
    ),
    positive_color = S7::new_property(
      S7::class_character,
      default = quote(rtemis_colors[[1L]])
    ),
    negative_color = S7::new_property(
      S7::class_character,
      default = "#ff9e1f"
    ),
    title = optional_character_scalar
  )
)

S7::method(to_list, SigmaOption) <- function(x, ...) {
  model <- x@model
  model_list <- if (S7::S7_inherits(model)) to_list(model) else model
  out <- list(
    model = model_list,
    style = list(
      layout = x@layout,
      nodeSize = x@node_size,
      edgeScale = x@edge_scale,
      nodeOpacity = x@node_opacity,
      edgeOpacity = x@edge_opacity,
      showLabels = x@show_labels,
      scaleByDegree = x@scale_by_degree,
      colorByGroup = x@color_by_group,
      resolution = x@resolution,
      blendEdges = x@blend_edges,
      palette = as.character(x@palette),
      nodeColor = as.character(x@node_color),
      positiveColor = as.character(x@positive_color),
      negativeColor = as.character(x@negative_color)
    )
  )
  if (!is.null(x@title)) {
    out[["title"]] <- x@title
  }
  out
}

# -- draw() method: Sigma.js backend --------------------------------------------

# Sigma.js backend: render the network spec as a `rtemis-graph` widget.
S7::method(draw, SigmaOption) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL,
  filename = NULL,
  ...
) {
  if (!is.null(filename)) {
    warn(
      "Static export of network widgets is not yet supported; ignoring `filename`."
    )
  }
  render_widget(
    "rtemis-graph",
    to_list(option),
    theme = theme,
    width = width,
    height = height,
    elementId = elementId
  )
}

# -- Model builders -------------------------------------------------------------

#' Build a GraphModel from a square weight / adjacency matrix
#'
#' Interprets a square numeric matrix as a weighted graph: entry `[i, j]` is the
#' edge weight between node `i` and node `j`. Node ids come from the matrix
#' dimnames (row names preferred, then column names, else `"1"`, `"2"`, ...).
#' Zero (and `NA`) entries produce no edge. The absolute value drives edge
#' thickness and the sign drives edge color, so a correlation matrix renders
#' directly.
#'
#' @param x Numeric matrix \[square\]: Weight / adjacency matrix.
#' @param directed Logical: When `FALSE` (default) only the upper triangle is
#'   read and one undirected edge is emitted per pair; when `TRUE` every
#'   off-diagonal entry becomes a directed edge.
#' @param self_loops Logical: Whether to keep the diagonal (self-edges).
#' @param threshold Optional Numeric \[0, Inf): Drop edges whose absolute weight
#'   is below this value. `NULL` keeps every non-zero edge.
#' @return [GraphModel].
#' @keywords internal
#' @noRd
graph_from_matrix <- function(
  x,
  directed = FALSE,
  self_loops = FALSE,
  threshold = NULL
) {
  if (!is.matrix(x) || nrow(x) != ncol(x)) {
    abort(
      "Matrix input to `draw_network()` must be square; got ",
      nrow(x),
      "x",
      ncol(x),
      ".",
      class = c("rtemis_dim_error", "rtemis_input_error")
    )
  }
  if (!is.numeric(x)) {
    abort(
      "Matrix input to `draw_network()` must be numeric.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  n <- nrow(x)
  ids <- rownames(x) %||% colnames(x) %||% as.character(seq_len(n))
  ids <- as.character(ids)

  # Accumulate edges and per-node weighted degree (the default node `value`,
  # which drives node size when scale_by_degree is on).
  degree <- stats::setNames(numeric(n), ids)
  edges <- vector("list", 0L)

  for (i in seq_len(n)) {
    # Undirected: only j > i (upper triangle). Directed: all j != i.
    js <- if (directed) seq_len(n) else seq.int(i, n)
    for (j in js) {
      if (i == j && !self_loops) {
        next
      }
      w <- x[i, j]
      if (is.na(w) || w == 0) {
        next
      }
      aw <- abs(w)
      if (!is.null(threshold) && aw < threshold) {
        next
      }
      edges[[length(edges) + 1L]] <- GraphEdge(
        source = ids[i],
        target = ids[j],
        weight = aw,
        sign = if (w > 0) 1 else -1
      )
      degree[i] <- degree[i] + aw
      if (i != j) {
        degree[j] <- degree[j] + aw
      }
    }
  }

  nodes <- lapply(seq_len(n), function(i) {
    GraphNode(id = ids[i], label = ids[i], value = unname(degree[i]))
  })

  GraphModel(nodes = nodes, edges = edges, directed = directed)
}

#' Build a GraphModel from an edge-list (and optional node) data frame
#'
#' @param edges Data frame: One row per edge. Recognized columns: `source` and
#'   `target` (required; the first two columns are used if these names are
#'   absent), `weight` (optional), and `sign` (optional, in \{-1, 1\}).
#' @param nodes Optional Data frame: One row per node. Recognized columns: `id`
#'   (or `name`; the first column if neither is present), `label`, `value`,
#'   `group`. Nodes referenced by edges but absent here are added automatically.
#' @param directed Logical: Whether edges are directed.
#' @return [GraphModel].
#' @keywords internal
#' @noRd
graph_from_edge_list <- function(edges, nodes = NULL, directed = FALSE) {
  if (!is.data.frame(edges)) {
    abort(
      "Edge-list input to `draw_network()` must be a data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (ncol(edges) < 2L) {
    abort(
      "Edge-list data frame must have at least 2 columns (source, target).",
      class = c("rtemis_dim_error", "rtemis_input_error")
    )
  }

  # Resolve source/target columns by name, falling back to position.
  src <- if ("source" %in% names(edges)) edges[["source"]] else edges[[1L]]
  tgt <- if ("target" %in% names(edges)) edges[["target"]] else edges[[2L]]
  src <- as.character(src)
  tgt <- as.character(tgt)
  wgt <- if ("weight" %in% names(edges)) as.numeric(edges[["weight"]]) else NULL
  sgn <- if ("sign" %in% names(edges)) as.numeric(edges[["sign"]]) else NULL

  edge_objs <- lapply(seq_len(nrow(edges)), function(k) {
    GraphEdge(
      source = src[k],
      target = tgt[k],
      weight = if (is.null(wgt)) NULL else wgt[k],
      sign = if (is.null(sgn) || is.na(sgn[k])) NULL else sgn[k]
    )
  })

  # Weighted degree per node id, used as the default `value` for any node not
  # explicitly described in `nodes`.
  edge_ids <- unique(c(src, tgt))
  # Weighted degree, vectorized: each edge contributes its weight to both
  # endpoints. Missing/absent weights count as 1.
  weights <- if (is.null(wgt)) rep(1, length(src)) else abs(wgt)
  weights[is.na(weights)] <- 1
  deg_sums <- vapply(
    split(rep(weights, 2L), c(src, tgt)),
    sum,
    numeric(1L)
  )
  degree <- stats::setNames(numeric(length(edge_ids)), edge_ids)
  degree[names(deg_sums)] <- deg_sums

  # NA-safe degree lookup: isolated nodes (in `nodes` but in no edge) are absent
  # from `degree`, where `degree[id]` returns NA rather than NULL.
  degree_of <- function(id) {
    d <- degree[id]
    if (length(d) == 0L || is.na(d)) 0 else unname(d)
  }

  if (is.null(nodes)) {
    node_objs <- lapply(edge_ids, function(id) {
      GraphNode(id = id, label = id, value = unname(degree[id]))
    })
  } else {
    if (!is.data.frame(nodes)) {
      abort(
        "`nodes` must be a data frame or NULL.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    id_col <- if ("id" %in% names(nodes)) {
      "id"
    } else if ("name" %in% names(nodes)) {
      "name"
    } else {
      names(nodes)[1L]
    }
    node_ids <- as.character(nodes[[id_col]])
    # Extract optional columns once as flat vectors instead of subsetting the
    # data frame per row inside the loop.
    labels <- if ("label" %in% names(nodes)) {
      as.character(nodes[["label"]])
    } else {
      NULL
    }
    values <- if ("value" %in% names(nodes)) {
      as.numeric(nodes[["value"]])
    } else {
      NULL
    }
    groups <- if ("group" %in% names(nodes)) {
      as.character(nodes[["group"]])
    } else {
      NULL
    }
    node_objs <- lapply(seq_len(nrow(nodes)), function(k) {
      id <- node_ids[k]
      GraphNode(
        id = id,
        label = if (!is.null(labels)) labels[k] else id,
        value = if (!is.null(values)) values[k] else degree_of(id),
        group = if (!is.null(groups)) groups[k] else NULL
      )
    })
    # Add any edge-referenced ids missing from the node table.
    missing_ids <- setdiff(edge_ids, node_ids)
    node_objs <- c(
      node_objs,
      lapply(missing_ids, function(id) {
        GraphNode(id = id, label = id, value = unname(degree[id]))
      })
    )
  }

  GraphModel(nodes = node_objs, edges = edge_objs, directed = directed)
}

# -- Widget factory -------------------------------------------------------------

#' Render a GraphModel as a Sigma.js htmlwidget
#'
#' Low-level renderer: takes a [GraphModel] (or a plain list with `nodes`,
#' `edges`, `directed`) and produces an interactive network htmlwidget backed by
#' Sigma.js + graphology. Most users call [draw_network()] instead.
#'
#' @param model [GraphModel] or named list: The graph to render.
#' @param layout Character \{"force", "circular", "circlepack", "random"\}:
#'   Layout algorithm. `"force"` is ForceAtlas2.
#' @param node_size Numeric \[0, Inf): Base node radius in screen pixels.
#' @param edge_scale Numeric \[0, Inf): Multiplier mapping normalized edge weight
#'   to stroke width.
#' @param node_opacity Numeric \[0, 1\]: Node fill opacity.
#' @param edge_opacity Numeric \[0, 1\]: Edge stroke opacity.
#' @param show_labels Logical: Whether to render node labels.
#' @param scale_by_degree Logical: Scale each node's radius by its `value`
#'   (weighted degree); when `FALSE` all nodes use the base size.
#' @param color_by_group Logical: Color nodes by detected community (Louvain)
#'   instead of a single hue.
#' @param resolution Numeric \[0, Inf): Louvain resolution; higher yields more,
#'   smaller communities.
#' @param blend_edges Logical: Color each edge as the blend of its two endpoint
#'   node colors instead of by sign.
#' @param palette Character: Categorical colors for communities.
#' @param node_color Character: Single node color used when `color_by_group` is
#'   `FALSE`.
#' @param positive_color Character: Edge color for positive-sign edges.
#' @param negative_color Character: Edge color for negative-sign edges.
#' @param title Optional Character: Title shown above the network.
#' @param theme Optional [Theme], list, or `NA`: Theme override. `NULL` enables
#'   light/dark auto-detection (matching [draw()]).
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param elementId Optional Character: Explicit element ID.
#' @param filename Optional Character: Currently ignored with a warning (static
#'   export of network widgets is not yet supported); accepted for signature
#'   parity with the other `draw_*` functions.
#' @return htmlwidget.
#' @export
draw_graph <- function(
  model,
  layout = "force",
  node_size = 10,
  edge_scale = 3,
  node_opacity = 0.95,
  edge_opacity = 0.4,
  show_labels = TRUE,
  scale_by_degree = TRUE,
  color_by_group = FALSE,
  resolution = 1,
  blend_edges = FALSE,
  palette = rtemis_colors,
  node_color = rtemis_colors[[1L]],
  positive_color = rtemis_colors[[1L]],
  negative_color = "#ff9e1f",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL,
  filename = NULL
) {
  layout <- match.arg(layout, c("force", "circular", "circlepack", "random"))

  # Assemble the full Sigma render spec, then dispatch through draw(). Theme is
  # resolved uniformly inside draw()/render_widget(), not here.
  option <- SigmaOption(
    model = model,
    layout = layout,
    node_size = node_size,
    edge_scale = edge_scale,
    node_opacity = node_opacity,
    edge_opacity = edge_opacity,
    show_labels = show_labels,
    scale_by_degree = scale_by_degree,
    color_by_group = color_by_group,
    resolution = resolution,
    blend_edges = blend_edges,
    palette = as.character(palette),
    node_color = node_color,
    positive_color = positive_color,
    negative_color = negative_color,
    title = title
  )

  draw(
    option,
    theme = theme,
    width = width,
    height = height,
    elementId = elementId,
    filename = filename
  )
}

# -- Tier 1: draw_network -------------------------------------------------------

#' Draw a Network / Graph
#'
#' Render a network with Sigma.js. Accepts either a square weight / adjacency
#' matrix (e.g. a correlation matrix) or an edge-list data frame, dispatching on
#' the type of `x`. Communities are detected with Louvain and laid out with
#' ForceAtlas2 by default.
#'
#' @param x Numeric matrix or Data frame: A square weight / adjacency matrix, or
#'   an edge-list data frame with `source` / `target` columns (and optional
#'   `weight`, `sign`). For a matrix, dimnames supply node ids and `[i, j]` is
#'   the edge weight (zero / `NA` entries produce no edge); the absolute value
#'   drives thickness and the sign drives color. For an edge list, `source` and
#'   `target` name the endpoints (the first two columns if those names are
#'   absent), with optional `weight` and `sign` columns.
#' @param nodes Optional Data frame: Node attributes (`id`/`name`, `label`,
#'   `value`, `group`). Used only with edge-list input; ignored for matrix
#'   input.
#' @param directed Logical: Whether the graph is directed.
#' @param threshold Optional Numeric \[0, Inf): For matrix input, drop edges
#'   whose absolute weight is below this value.
#' @param self_loops Logical: For matrix input, whether to keep diagonal
#'   self-edges.
#' @inheritParams draw_graph
#' @return htmlwidget.
#' @examples
#' \dontrun{
#' # Correlation matrix
#' draw_network(cor(mtcars), threshold = 0.5)
#'
#' # Edge list
#' edges <- data.frame(
#'   source = c("a", "a", "b"),
#'   target = c("b", "c", "c"),
#'   weight = c(1, 2, 0.5)
#' )
#' draw_network(edges)
#' }
#' @export
draw_network <- function(
  x,
  nodes = NULL,
  directed = FALSE,
  threshold = NULL,
  self_loops = FALSE,
  layout = "force",
  node_size = 10,
  edge_scale = 3,
  node_opacity = 0.95,
  edge_opacity = 0.4,
  show_labels = TRUE,
  scale_by_degree = TRUE,
  color_by_group = FALSE,
  resolution = 1,
  blend_edges = FALSE,
  palette = rtemis_colors,
  node_color = rtemis_colors[[1L]],
  positive_color = rtemis_colors[[1L]],
  negative_color = "#ff9e1f",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL,
  filename = NULL
) {
  if (is.matrix(x)) {
    model <- graph_from_matrix(
      x,
      directed = directed,
      self_loops = self_loops,
      threshold = threshold
    )
  } else if (is.data.frame(x)) {
    model <- graph_from_edge_list(x, nodes = nodes, directed = directed)
  } else {
    abort(
      "`x` must be a square numeric matrix or an edge-list data frame; got ",
      paste(class(x), collapse = "/"),
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }

  draw_graph(
    model,
    layout = layout,
    node_size = node_size,
    edge_scale = edge_scale,
    node_opacity = node_opacity,
    edge_opacity = edge_opacity,
    show_labels = show_labels,
    scale_by_degree = scale_by_degree,
    color_by_group = color_by_group,
    resolution = resolution,
    blend_edges = blend_edges,
    palette = palette,
    node_color = node_color,
    positive_color = positive_color,
    negative_color = negative_color,
    title = title,
    theme = theme,
    width = width,
    height = height,
    elementId = elementId,
    filename = filename
  )
}
