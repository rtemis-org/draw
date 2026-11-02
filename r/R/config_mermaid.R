# config_mermaid.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The Mermaid diagram's config, compiling to a Pele option.
# spec: draw/mermaid-diagrams#config-and-option-layers
#
# The bound data is the Mermaid source text: `draw(config, data = source)`, or
# a `.mmd` / `.mermaid` file named by `dat_path`. The config carries the render
# settings, so one document styles any diagram.

# %% MermaidConfig ----
#' Mermaid Diagram Configuration
#'
#' A serializable description of a Mermaid diagram rendered with Pele. Build
#' one with [setup_MermaidConfig()] rather than calling this constructor
#' directly.
#'
#' The bound data is the Mermaid source: `draw(config, data = source)`, where
#' `source` is a character vector, or a `.mmd` or `.mermaid` file named by
#' `dat_path`. See [draw_mermaid()] for the supported Mermaid syntax.
#'
#' @param palette Optional Character: Colors for categorical entities,
#'   overriding the theme palette. `NULL` uses the theme's.
#' @param node_color Optional Named character: Colors for selected nodes,
#'   named by node id. Each named node gets a border in its color and a fill of
#'   the same color at `fill_alpha` opacity.
#' @param fill_alpha Numeric \[0, 1\]: Fill opacity of the nodes in
#'   `node_color`.
#' @param font_size Numeric (0, Inf): Label font size in pixels.
#' @param padding Optional Numeric \[0, Inf): Space around the diagram in
#'   pixels. `NULL` uses Pele's default.
#' @param auto_direction Logical: Allow left-to-right flowcharts to be drawn
#'   top-to-bottom in widgets narrower than 640 pixels.
#' @param fit Logical: Shrink the diagram to the widget width instead of
#'   drawing it at its natural size.
#' @inheritParams ChartConfig
#'
#' @return `MermaidConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_MermaidConfig(font_size = 16)@type
MermaidConfig <- new_class(
  name = "MermaidConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("mermaid"),
    # No column binding: the bound data is the Mermaid source.
    # -- appearance --------------------------------------------------------
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = paste(
        "Colors for categorical entities, overriding the theme palette.",
        "Unset uses the theme's."
      )
    ),
    node_color = prop_string(
      NULL,
      nullable = TRUE,
      map = TRUE,
      description = paste(
        "Colors for selected nodes, keyed by node id. Each node gets a border",
        "in its color and a fill of the same color at fill_alpha opacity."
      )
    ),
    fill_alpha = prop_float(
      0.1,
      min = 0,
      max = 1,
      description = "Fill opacity of the nodes in node_color."
    ),
    font_size = prop_float(
      14,
      exclusive_min = 0,
      description = "Label font size in pixels."
    ),
    padding = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = paste(
        "Space around the diagram in pixels. Unset uses the renderer's",
        "default."
      )
    ),
    auto_direction = prop_boolean(
      FALSE,
      description = paste(
        "Allow left-to-right flowcharts to be drawn top-to-bottom in narrow",
        "widgets."
      )
    ),
    fit = prop_boolean(
      FALSE,
      description = paste(
        "Shrink the diagram to the widget width instead of drawing it at its",
        "natural size."
      )
    )
  )
) # /rtemis.draw::MermaidConfig


# %% MERMAID_ORIGIN_NAMES ----
MERMAID_ORIGIN_NAMES <- setdiff(
  names(MermaidConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_MermaidConfig ----
#' Set up a Mermaid Diagram Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams MermaidConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [MermaidConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' config <- setup_MermaidConfig(title = "Study design")
#' draw(
#'   config,
#'   data = c(
#'     "flowchart TD",
#'     "  screened[Screened] --> eligible[Eligible]",
#'     "  eligible --> treated[Treatment]",
#'     "  eligible --> control[Control]"
#'   )
#' )
setup_MermaidConfig <- function(
  palette = NULL,
  node_color = NULL,
  fill_alpha = 0.1,
  font_size = 14,
  padding = NULL,
  auto_direction = FALSE,
  fit = FALSE,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), MERMAID_ORIGIN_NAMES)
  MermaidConfig(
    palette = palette,
    node_color = node_color,
    fill_alpha = fill_alpha,
    font_size = font_size,
    padding = padding,
    auto_direction = auto_direction,
    fit = fit,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_MermaidConfig


# %% resolve.MermaidConfig ----
# Nothing to derive: the source has no columns to take labels from.
method(resolve, MermaidConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.MermaidConfig ----
# `data` is the Mermaid source, from `draw(data = )` or the `dat_path` file.
method(compile, MermaidConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  mermaid_option(
    source = data,
    palette = config@palette,
    node_color = config@node_color,
    fill_alpha = config@fill_alpha,
    font_size = config@font_size,
    padding = config@padding,
    auto_direction = config@auto_direction,
    fit = config@fit,
    title = config@title
  )
}
