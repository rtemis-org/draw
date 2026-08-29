# series_sankey.R
# Sankey diagram S7 classes: SankeyNodeItem, SankeyEdgeItem,
#                             SankeyLevelOption, SankeySeries
#
# TS sources:
#   SankeySeriesOption:    src/chart/sankey/SankeySeries.ts (line 100)
#   SankeyNodeItemOption:  src/chart/sankey/SankeySeries.ts (line 73)
#   SankeyEdgeItemOption:  src/chart/sankey/SankeySeries.ts (line 88)
#   SankeyLevelOption:     src/chart/sankey/SankeySeries.ts (line 96)

# -- SankeyNodeItem -------------------------------------------------------------

#' Sankey Node Item
#'
#' One node in a Sankey diagram. Node names must match the `source` / `target`
#' strings used in the corresponding [SankeyEdgeItem] objects.
#'
#' Corresponds to `SankeyNodeItemOption` in
#' `src/chart/sankey/SankeySeries.ts` (line 73).
#' ECharts docs:
#' \url{https://echarts.apache.org/en/option.html#series-sankey.data}
#'
#' @param name Optional Character: Node name (must match source/target strings).
#' @param value Optional Numeric: Node value shown in tooltip.
#' @param depth Optional Numeric `[0, Inf)`: Fixed column depth (0 = leftmost).
#' @param draggable Optional Logical: Whether the node can be dragged.
#' @param local_x Optional Numeric: Horizontal position inside the layout rect.
#' @param local_y Optional Numeric: Vertical position inside the layout rect.
#' @param label Optional [LabelOption]: Node label configuration.
#' @param item_style Optional [ItemStyle]: Node fill/border styling.
#' @param focus_node_adjacency Optional Logical or Character
#'   \{"inEdges", "outEdges", "allEdges"\}: Which adjacent elements to highlight
#'   on hover.
#'
#' @return `SankeyNodeItem` object.
#'
#' @export
#'
#' @examples
#' SankeyNodeItem(name = "Source A", value = 10)
SankeyNodeItem <- S7::new_class(
  "SankeyNodeItem",
  properties = list(
    name = optional_character_scalar,
    value = numeric_or_null_property(),
    depth = optional_nonneg_integer_scalar,
    draggable = optional_logical_scalar,
    local_x = numeric_or_null_property(),
    local_y = numeric_or_null_property(),
    label = class_or_null_property(LabelOption),
    item_style = class_or_null_property(ItemStyle),
    focus_node_adjacency = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("inEdges", "outEdges", "allEdges")
        ) {
          return(NULL)
        }
        "must be TRUE/FALSE, 'inEdges', 'outEdges', 'allEdges', or NULL"
      }
    )
  )
)

S7::method(to_list, SankeyNodeItem) <- function(x, ...) {
  props_to_list(x)
}

# -- SankeyEdgeItem -------------------------------------------------------------

#' Sankey Edge Item
#'
#' One directed link (edge) in a Sankey diagram.
#'
#' Corresponds to `SankeyEdgeItemOption` in
#' `src/chart/sankey/SankeySeries.ts` (line 88).
#' ECharts docs:
#' \url{https://echarts.apache.org/en/option.html#series-sankey.links}
#'
#' @param source Optional Character: Source node name.
#' @param target Optional Character: Target node name.
#' @param value Optional Numeric: Flow value (determines link width).
#' @param line_style Optional [LineStyle] or list: Link line styling. Accepts
#'   a [LineStyle] object or a plain named list; a list also allows the
#'   Sankey-specific `curveness` field (e.g. `list(curveness = 0.5,
#'   opacity = 0.3)`).
#' @param edge_label Optional [LabelOption]: Label shown on the link itself.
#' @param focus_node_adjacency Optional Logical or Character
#'   \{"inEdges", "outEdges", "allEdges"\}: Which adjacent elements to highlight
#'   on hover.
#'
#' @return `SankeyEdgeItem` object.
#'
#' @export
#'
#' @examples
#' SankeyEdgeItem(source = "A", target = "B", value = 5)
SankeyEdgeItem <- S7::new_class(
  "SankeyEdgeItem",
  properties = list(
    source = optional_character_scalar,
    target = optional_character_scalar,
    value = numeric_or_null_property(),
    # Accepts LineStyle S7 object or a plain list (allows curveness extension)
    line_style = S7::new_property(class = S7::class_any, default = NULL),
    edge_label = class_or_null_property(LabelOption),
    focus_node_adjacency = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("inEdges", "outEdges", "allEdges")
        ) {
          return(NULL)
        }
        "must be TRUE/FALSE, 'inEdges', 'outEdges', 'allEdges', or NULL"
      }
    )
  )
)

S7::method(to_list, SankeyEdgeItem) <- function(x, ...) {
  out <- props_to_list(x)
  # line_style may be a LineStyle S7 object; props_to_list handles recursion
  # but the rename from snake_case is already applied. Ensure edge_label is
  # serialized under "edgeLabel" (props_to_list does this via snake_to_camel).
  out
}

# -- SankeyLevelOption ----------------------------------------------------------

#' Sankey Level Option
#'
#' Style overrides applied to all nodes at a given column depth. `depth = 0`
#' targets the leftmost column.
#'
#' Corresponds to `SankeyLevelOption` in
#' `src/chart/sankey/SankeySeries.ts` (line 96).
#' ECharts docs:
#' \url{https://echarts.apache.org/en/option.html#series-sankey.levels}
#'
#' @param depth Numeric `[0, Inf)`: Column depth to target (0 = leftmost). Required.
#' @param label Optional [LabelOption]: Node label configuration at this depth.
#' @param item_style Optional [ItemStyle]: Node fill/border styling at this depth.
#' @param line_style Optional [LineStyle] or list: Link styling for edges leaving
#'   this depth. Accepts a [LineStyle] object or a plain list (allows `curveness`).
#'
#' @return `SankeyLevelOption` object.
#'
#' @export
#'
#' @examples
#' SankeyLevelOption(depth = 0L, item_style = ItemStyle(color = "#16a085"))
SankeyLevelOption <- S7::new_class(
  "SankeyLevelOption",
  properties = list(
    depth = nonneg_integer_scalar,
    label = class_or_null_property(LabelOption),
    item_style = class_or_null_property(ItemStyle),
    line_style = S7::new_property(class = S7::class_any, default = NULL)
  )
)

S7::method(to_list, SankeyLevelOption) <- function(x, ...) {
  props_to_list(x)
}

# -- SankeySeries ---------------------------------------------------------------

#' Sankey Series
#'
#' Configuration for a Sankey diagram series. Nodes and directed links are
#' represented as two separate lists. Node names in `data` must match the
#' `source` and `target` strings in `links`.
#'
#' Corresponds to `SankeySeriesOption` in
#' `src/chart/sankey/SankeySeries.ts` (line 100).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-sankey}
#'
#' @param name Optional Character: Series name for legend and tooltip.
#' @param data Optional list: Nodes as a list of [SankeyNodeItem] objects or
#'   plain named lists, each with at least a `name` field.
#' @param links Optional list: Edges as a list of [SankeyEdgeItem] objects or
#'   plain named lists with `source`, `target`, and `value` fields.
#' @param levels Optional list: List of [SankeyLevelOption] for per-depth styling.
#' @param orient Optional Character \{"horizontal", "vertical"\}: Flow direction.
#' @param node_width Optional Numeric `[0, Inf)`: Width of each node rectangle in pixels.
#' @param node_gap Optional Numeric `[0, Inf)`: Vertical gap between nodes in pixels.
#' @param node_align Optional Character \{"justify", "left", "right"\}: Node
#'   alignment within columns.
#' @param draggable Optional Logical: Whether nodes can be dragged interactively.
#' @param layout_iterations Optional Numeric `[0, Inf)`: Number of layout iterations.
#' @param sort Optional Character \{"desc"\} or `NULL`: Column-internal node
#'   ordering. `NULL` preserves the original data order.
#' @param roam Optional Logical or Character \{"move", "scale"\}: Whether to enable
#'   pan/zoom.
#' @param label Optional [LabelOption]: Node label configuration.
#' @param edge_label Optional [LabelOption]: Edge label configuration.
#' @param line_style Optional [LineStyle] or list: Default link line styling.
#'   Accepts a [LineStyle] object or a plain list; a plain list also allows the
#'   Sankey-specific `curveness` field (e.g. `list(curveness = 0.5)`).
#' @param item_style Optional [ItemStyle]: Default node styling.
#' @param color Optional Character: Color palette for nodes as a character vector.
#' @param left Optional Numeric or Character: Series left offset.
#' @param right Optional Numeric or Character: Series right offset.
#' @param top Optional Numeric or Character: Series top offset.
#' @param bottom Optional Numeric or Character: Series bottom offset.
#' @param silent Optional Logical: Whether to disable mouse/touch events.
#' @param z_level Optional Numeric: Canvas layer index.
#' @param z Optional Numeric: Front-back order within the same canvas layer.
#'
#' @return `SankeySeries` object.
#'
#' @export
#'
#' @examples
#' SankeySeries(
#'   data = list(SankeyNodeItem(name = "A"), SankeyNodeItem(name = "B")),
#'   links = list(SankeyEdgeItem(source = "A", target = "B", value = 5))
#' )
SankeySeries <- S7::new_class(
  "SankeySeries",
  properties = list(
    name = optional_character_scalar,
    data = S7::new_property(class = S7::class_any, default = NULL),
    links = S7::new_property(class = S7::class_any, default = NULL),
    levels = S7::new_property(class = S7::class_any, default = NULL),
    orient = enum(c("horizontal", "vertical"), nullable = TRUE),
    node_width = numeric_or_null_property(),
    node_gap = numeric_or_null_property(),
    node_align = enum(c("justify", "left", "right"), nullable = TRUE),
    draggable = optional_logical_scalar,
    layout_iterations = numeric_or_null_property(),
    sort = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.character(value) && length(value) == 1L && value == "desc") {
          return(NULL)
        }
        "must be 'desc' or NULL"
      }
    ),
    roam = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.logical(value) && length(value) == 1L) {
          return(NULL)
        }
        if (
          is.character(value) &&
            length(value) == 1L &&
            value %in% c("move", "scale")
        ) {
          return(NULL)
        }
        "must be TRUE/FALSE, 'move', 'scale', or NULL"
      }
    ),
    label = class_or_null_property(LabelOption),
    edge_label = class_or_null_property(LabelOption),
    # Accepts LineStyle S7 object or plain list (allows curveness extension)
    line_style = S7::new_property(class = S7::class_any, default = NULL),
    item_style = class_or_null_property(ItemStyle),
    color = color_palette_property(),
    left = numeric_or_string_property(),
    right = numeric_or_string_property(),
    top = numeric_or_string_property(),
    bottom = numeric_or_string_property(),
    silent = optional_logical_scalar,
    z_level = numeric_or_null_property(),
    z = numeric_or_null_property()
  )
)

#' @keywords internal
#' @noRd
serialize_sankey_items <- function(items) {
  if (is.null(items)) {
    return(NULL)
  }
  out <- lapply(items, function(item) {
    if (S7::S7_inherits(item)) to_list(item) else item
  })
  unname(out)
}

S7::method(to_list, SankeySeries) <- function(x, ...) {
  out <- props_to_list(x)
  out[["type"]] <- "sankey"

  # Serialize data (nodes) and links as unnamed arrays
  if (!is.null(out[["data"]])) {
    out[["data"]] <- serialize_sankey_items(x@data)
  }
  if (!is.null(out[["links"]])) {
    out[["links"]] <- serialize_sankey_items(x@links)
  }

  # Serialize levels as unnamed array
  if (!is.null(out[["levels"]])) {
    out[["levels"]] <- serialize_sankey_items(x@levels)
  }

  # line_style: S7 object -> list; plain list passed through as-is
  if (!is.null(out[["lineStyle"]]) && S7::S7_inherits(x@line_style)) {
    out[["lineStyle"]] <- to_list(x@line_style)
  }

  out
}
