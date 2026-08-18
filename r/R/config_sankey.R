# config_sankey.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The Sankey diagram's config. The first **table-bound** chart: it binds a table
# of links rather than one column per role, so the properties name the columns
# *within* that table.
#
# `sankey_option()` requires the columns to be called `source`, `target` and
# `value`. The config names them instead, defaulting to those, so a table that
# calls them something else can be bound without being renamed first -- which is
# the point of a binding layer.

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
      description = "Node colors, overriding the theme palette. NULL uses the theme's."
    )
  )
) # /rtemis.draw::SankeyConfig


# %% SANKEY_ORIGIN_NAMES ----
SANKEY_ORIGIN_NAMES <- setdiff(
  names(SankeyConfig@properties),
  c("type", "origin", "writer")
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
# The builder expects the conventional column names, so the bound columns are
# renamed into them here. That is what lets a table calling them anything else
# be plotted without the caller reshaping it first.
method(compile, SankeyConfig) <- function(config, data = NULL, ...) {
  dat <- config_data(config, data)
  config <- resolve(config, data = dat)
  links <- data.frame(
    source = config_column(dat, config@source, "source"),
    target = config_column(dat, config@target, "target"),
    value = config_column(dat, config@value, "value"),
    stringsAsFactors = FALSE
  )
  sankey_option(
    links = links,
    orient = config@orient,
    node_width = config@node_width,
    node_gap = config@node_gap,
    node_align = config@node_align,
    title = config@title,
    palette = config@palette
  )
}
