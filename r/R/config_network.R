# config_network.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The network graph's config, compiling to a Sigma option.
#
# The binding is the shape none of the other charts has: **the bound data is the
# adjacency matrix itself**, not columns within a table. So this config has no
# column-name properties at all -- everything it declares is semantics and
# appearance, and `data` carries the whole structure.
#
# An optional node table can be supplied alongside it by passing a named list,
# `list(adjacency = <matrix>, nodes = <data.frame>)`, since one `data` argument
# has to carry both.
#
# There is no separate `GraphConfig`. `draw_graph()` takes an already-built
# `GraphModel`, which is the rendering IR; the *chart type* is the network, and
# building the model is what its `compile()` does.

# %% NetworkConfig ----
#' Network Graph Configuration
#'
#' A serializable description of a network graph. Build one with
#' [setup_NetworkConfig()] rather than calling this constructor directly.
#'
#' The bound data is an adjacency matrix, optionally with a node table:
#' `draw(config, data = m)` or
#' `draw(config, data = list(adjacency = m, nodes = df))`.
#'
#' @param directed Logical: Treat the matrix as directed.
#' @param threshold Optional Numeric: Drop edges weaker than this.
#' @param self_loops Logical: Keep self-edges.
#' @param layout Character: Layout algorithm.
#' @param cluster_resolution Numeric `[0, Inf)`: Community-detection
#'   resolution. Higher values find more, smaller communities.
#' @param scale_by_degree Logical: Size nodes by their degree.
#' @param color_by_group Logical: Color nodes by detected community.
#' @param blend_edges Logical: Blend overlapping edges.
#' @param show_labels Logical: Draw node labels.
#' @param node_size Numeric `[0, Inf)`: Base node size.
#' @param edge_scale Numeric `[0, Inf)`: Edge width multiplier.
#' @param node_opacity,edge_opacity Numeric `[0, 1]`: Opacity of each.
#' @param palette Optional Character: Community colors, overriding the theme
#'   palette. `NULL` uses the theme's.
#' @param node_color Character: Node color when not colored by group.
#' @param positive_color,negative_color Character: Edge colors by sign.
#' @inheritParams ChartConfig
#'
#' @return `NetworkConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_NetworkConfig(layout = "force")@type
NetworkConfig <- new_class(
  name = "NetworkConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("network"),
    # -- semantics ---------------------------------------------------------
    # No data binding: the bound data *is* the adjacency matrix.
    directed = prop_boolean(
      FALSE,
      description = "Treat the matrix as directed."
    ),
    threshold = prop_float(
      NULL,
      nullable = TRUE,
      description = "Drop edges weaker than this."
    ),
    self_loops = prop_boolean(FALSE, description = "Keep self-edges."),
    layout = prop_string("force", description = "Layout algorithm."),
    cluster_resolution = prop_float(
      1,
      min = 0,
      description = paste(
        "Community-detection resolution. Higher values find more, smaller",
        "communities."
      )
    ),
    scale_by_degree = prop_boolean(
      TRUE,
      description = "Size nodes by their degree."
    ),
    color_by_group = prop_boolean(
      FALSE,
      description = "Color nodes by detected community."
    ),
    blend_edges = prop_boolean(
      FALSE,
      description = "Blend overlapping edges."
    ),
    # -- appearance --------------------------------------------------------
    show_labels = prop_boolean(TRUE, description = "Draw node labels."),
    node_size = prop_float(10, min = 0, description = "Base node size."),
    edge_scale = prop_float(3, min = 0, description = "Edge width multiplier."),
    node_opacity = prop_float(
      0.95,
      min = 0,
      max = 1,
      description = "Node opacity."
    ),
    edge_opacity = prop_float(
      0.4,
      min = 0,
      max = 1,
      description = "Edge opacity."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Community colors, overriding the theme palette. NULL uses the theme's."
    ),
    node_color = prop_string(
      "#6CA3A0",
      description = "Node color when not colored by group."
    ),
    positive_color = prop_string(
      "#6CA3A0",
      description = "Color for positive-weight edges."
    ),
    negative_color = prop_string(
      "#ff9e1f",
      description = "Color for negative-weight edges."
    )
  )
) # /rtemis.draw::NetworkConfig


# %% NETWORK_ORIGIN_NAMES ----
NETWORK_ORIGIN_NAMES <- setdiff(
  names(NetworkConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_NetworkConfig ----
#' Set up a Network Graph Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams NetworkConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [NetworkConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' m <- matrix(
#'   c(0, 1, 1, 1, 0, 1, 1, 1, 0),
#'   nrow = 3,
#'   dimnames = list(letters[1:3], letters[1:3])
#' )
#' draw(setup_NetworkConfig(), data = m)
setup_NetworkConfig <- function(
  directed = FALSE,
  threshold = NULL,
  self_loops = FALSE,
  layout = "force",
  cluster_resolution = 1,
  scale_by_degree = TRUE,
  color_by_group = FALSE,
  blend_edges = FALSE,
  show_labels = TRUE,
  node_size = 10,
  edge_scale = 3,
  node_opacity = 0.95,
  edge_opacity = 0.4,
  palette = NULL,
  node_color = rtemis_colors[["teal"]],
  positive_color = rtemis_colors[["teal"]],
  negative_color = "#ff9e1f",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), NETWORK_ORIGIN_NAMES)
  NetworkConfig(
    directed = directed,
    threshold = threshold,
    self_loops = self_loops,
    layout = layout,
    cluster_resolution = cluster_resolution,
    scale_by_degree = scale_by_degree,
    color_by_group = color_by_group,
    blend_edges = blend_edges,
    show_labels = show_labels,
    node_size = node_size,
    edge_scale = edge_scale,
    node_opacity = node_opacity,
    edge_opacity = edge_opacity,
    palette = palette,
    node_color = node_color,
    positive_color = positive_color,
    negative_color = negative_color,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_NetworkConfig


# %% resolve.NetworkConfig ----
# Nothing to derive: no axes, and no column names to take labels from.
method(resolve, NetworkConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.NetworkConfig ----
# `data` is the adjacency matrix, or a list carrying it alongside a node table --
# one argument has to hold both, and a named list is the least surprising way.
method(compile, NetworkConfig) <- function(config, data = NULL, ...) {
  bound <- config_data(config, data)
  nodes <- NULL
  if (
    is.list(bound) && !is.data.frame(bound) && "adjacency" %in% names(bound)
  ) {
    nodes <- bound[["nodes"]]
    bound <- bound[["adjacency"]]
  }
  # Same dispatch `draw_network()` makes: a square matrix or an edge list.
  model <- if (is.matrix(bound)) {
    graph_from_matrix(
      bound,
      directed = config@directed,
      self_loops = config@self_loops,
      threshold = config@threshold
    )
  } else if (is.data.frame(bound)) {
    graph_from_edge_list(bound, nodes = nodes, directed = config@directed)
  } else {
    abort(
      "A NetworkConfig binds a square numeric matrix or an edge-list data ",
      "frame; got ",
      paste(class(bound), collapse = "/"),
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  graph_option(
    model = model,
    layout = config@layout,
    node_size = config@node_size,
    edge_scale = config@edge_scale,
    node_opacity = config@node_opacity,
    edge_opacity = config@edge_opacity,
    show_labels = config@show_labels,
    scale_by_degree = config@scale_by_degree,
    color_by_group = config@color_by_group,
    resolution = config@cluster_resolution,
    blend_edges = config@blend_edges,
    palette = config@palette %||% palette_colors(rtemis_colors),
    node_color = config@node_color,
    positive_color = config@positive_color,
    negative_color = config@negative_color,
    title = config@title
  )
}
