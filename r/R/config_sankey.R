# config_sankey.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The Sankey diagram's config. Table-bound: it binds a table of links rather
# than one column per role, so the properties name the columns within that
# table, defaulting to `source`, `target`, and `value`.

# %% SankeyConfig ----
#' Sankey Diagram Configuration
#'
#' A serializable description of a Sankey diagram. Build one with
#' [setup_SankeyConfig()] rather than calling this constructor directly.
#'
#' The bound data is a table of links, one row per flow. `source`, `target` and
#' `value` name its columns.
#'
#' @param source Character: Column naming each link's source node.
#' @param target Character: Column naming each link's target node.
#' @param value Character: Column holding each link's magnitude.
#' @param orient Character \{"horizontal", "vertical"\}: Layout direction.
#' @param node_align Optional Character \{"left", "right", "justify"\}: How nodes
#'   are aligned along the flow.
#' @param node_width Optional Numeric `[0, Inf)`: Node thickness in pixels.
#' @param node_gap Optional Numeric `[0, Inf)`: Gap between nodes in pixels.
#' @param palette Optional Character: Node colors, overriding the theme palette
#'   for this chart. `NULL` uses the theme's.
#' @param link_color Character: Link color, `"gradient"` from the source node's
#'   color to the target's, `"source"`, `"target"`, or a color.
#' @param link_opacity Numeric `[0, 1]`: Link opacity.
#' @param label_font_size Numeric `(0, Inf)`: Node label font size in pixels.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Space around the diagram in pixels. Unset sides are derived.
#' @inheritParams ChartConfig
#'
#' @return `SankeyConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_SankeyConfig()@type
SankeyConfig <- new_class(
  name = "SankeyConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("sankey"),
    # -- data binding: columns of the bound link table ----------------------
    # Defaulted rather than nullable: every Sankey has these three roles, so a
    # config always states which column fills each, and a complete document
    # never leaves them unresolved.
    source = prop_string(
      "source",
      description = "Column naming each link's source node."
    ),
    target = prop_string(
      "target",
      description = "Column naming each link's target node."
    ),
    value = prop_string(
      "value",
      description = "Column holding each link's magnitude."
    ),
    # -- semantics ---------------------------------------------------------
    orient = prop_string(
      "horizontal",
      enum = c("horizontal", "vertical"),
      description = "Layout direction."
    ),
    node_align = prop_string(
      NULL,
      enum = c("left", "right", "justify"),
      nullable = TRUE,
      description = "How nodes are aligned along the flow."
    ),
    # -- appearance --------------------------------------------------------
    node_width = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = "Node thickness in pixels."
    ),
    node_gap = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = "Gap between nodes in pixels."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Node colors, overriding the theme palette. Unset uses the theme's."
    ),
    link_color = prop_string(
      "gradient",
      description = paste(
        "Link color: 'gradient' from the source node's color to the",
        "target's, 'source', 'target', or a color."
      )
    ),
    link_opacity = prop_float(
      0.2,
      min = 0,
      max = 1,
      description = "Link opacity."
    ),
    label_font_size = prop_float(
      12,
      exclusive_min = 0,
      description = "Node label font size in pixels."
    ),
    margin_top = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Top margin in pixels."
    ),
    margin_right = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Right margin in pixels."
    ),
    margin_bottom = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Bottom margin in pixels."
    ),
    margin_left = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Left margin in pixels."
    )
  )
) # /rtemis.draw::SankeyConfig


# %% SANKEY_ORIGIN_NAMES ----
SANKEY_ORIGIN_NAMES <- setdiff(
  names(SankeyConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_SankeyConfig ----
#' Set up a Sankey Diagram Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams SankeyConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [SankeyConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' links <- data.frame(
#'   source = c("a", "b"),
#'   target = c("c", "c"),
#'   value = c(1, 2)
#' )
#' draw(setup_SankeyConfig(), data = links)
setup_SankeyConfig <- function(
  source = "source",
  target = "target",
  value = "value",
  orient = "horizontal",
  node_align = NULL,
  node_width = NULL,
  node_gap = NULL,
  palette = NULL,
  link_color = "gradient",
  link_opacity = 0.2,
  label_font_size = 12,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), SANKEY_ORIGIN_NAMES)
  SankeyConfig(
    source = source,
    target = target,
    value = value,
    orient = orient,
    node_align = node_align,
    node_width = node_width,
    node_gap = node_gap,
    palette = palette,
    link_color = link_color,
    link_opacity = link_opacity,
    label_font_size = label_font_size,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_SankeyConfig


# %% resolve.SankeyConfig ----
# Nothing to derive: no axes, and the column bindings are stated rather than
# read off the data.
method(resolve, SankeyConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.SankeyConfig ----
# The option is built in JavaScript by @rtemis/draw, so compiling a Sankey
# config gives the request the widget and SVG export pass to
# `RtemisDraw.builders.sankey`: the bound link columns and the config.
# spec: draw/shared-builders#sankey
method(compile, SankeyConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  columns <- c(config@source, config@target, config@value)
  for (role in c("source", "target", "value")) {
    config_column(data, prop(config, role), role)
  }
  BuildRequest(
    type = "sankey",
    data = tabular_json(data, unique(columns)),
    config = config_members(config),
    appearance = list()
  )
}
