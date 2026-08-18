# config_a3.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The annotated protein diagram's config. Bound data is an `A3` object from
# rtemis.a3 -- a domain structure, not columns -- so this config declares no
# bindings, only how the diagram is drawn.
#
# Its render hint is the widget `height`, computed from how many residue rows
# the sequence wraps onto. That is a fact about the display surface, so it is
# handed to `draw()` and never written into a document: the same protein laid
# out at a different width needs a different height.

# %% A3Config ----
#' Annotated Protein Diagram Configuration
#'
#' A serializable description of an A3 protein diagram. Build one with
#' [setup_A3Config()] rather than calling this constructor directly.
#'
#' The bound data is an `A3` object, created with `rtemis.a3::create_A3()`.
#'
#' @param n_per_row Integer `[1, Inf)`: Residues per row.
#' @param position_every Integer `[1, Inf)`: Label every nth position.
#' @param ptm_placement Character: How post-translational modifications are
#'   placed relative to the backbone.
#' @param zoom Logical: Enable the zoom control.
#' @param residue_spacing Numeric `[0, Inf)`: Gap between residues.
#' @param marker_size Numeric `[0, Inf)`: Residue marker size.
#' @param font_size Numeric `[0, Inf)`: Residue label font size.
#' @param line_width Numeric `[0, Inf)`: Backbone line width.
#' @param show_markers,show_labels Logical: Whether to draw each.
#' @param region_opacity Numeric `[0, 1]`: Region band opacity.
#' @param residue_fill,residue_stroke Character: Residue marker colors.
#' @param label_color,pos_label_color Character: Label colors.
#' @param variant_color,disease_variant_color Character: Variant marker colors.
#' @inheritParams ChartConfig
#'
#' @return `A3Config` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_A3Config(n_per_row = 30L)@type
A3Config <- new_class(
  name = "A3Config",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("a3"),
    # -- semantics: how the sequence is laid out ----------------------------
    n_per_row = prop_integer(21L, min = 1L, description = "Residues per row."),
    position_every = prop_integer(
      10L,
      min = 1L,
      description = "Label every nth position."
    ),
    ptm_placement = prop_string(
      "radial",
      description = "How modifications are placed relative to the backbone."
    ),
    zoom = prop_boolean(TRUE, description = "Enable the zoom control."),
    # -- appearance --------------------------------------------------------
    residue_spacing = prop_float(
      0.3,
      min = 0,
      description = "Gap between residues."
    ),
    marker_size = prop_float(28, min = 0, description = "Residue marker size."),
    font_size = prop_float(
      18,
      min = 0,
      description = "Residue label font size."
    ),
    line_width = prop_float(2, min = 0, description = "Backbone line width."),
    show_markers = prop_boolean(TRUE, description = "Draw residue markers."),
    show_labels = prop_boolean(TRUE, description = "Draw residue labels."),
    region_opacity = prop_float(
      0.35,
      min = 0,
      max = 1,
      description = "Region band opacity."
    ),
    residue_fill = prop_string(
      "#E7E5E4",
      description = "Residue marker fill."
    ),
    residue_stroke = prop_string(
      "#44403C",
      description = "Residue marker outline."
    ),
    label_color = prop_string("#1C1917", description = "Residue label color."),
    pos_label_color = prop_string(
      "#78716C",
      description = "Position label color."
    ),
    variant_color = prop_string(
      "#FA6E1E",
      description = "Variant marker color."
    ),
    disease_variant_color = prop_string(
      "#E266AE",
      description = "Disease-variant marker color."
    )
  )
) # /rtemis.draw::A3Config


# %% A3_ORIGIN_NAMES ----
A3_ORIGIN_NAMES <- setdiff(
  names(A3Config@properties),
  c("type", "origin", "writer")
)


# %% setup_A3Config ----
#' Set up an Annotated Protein Diagram Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams A3Config
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [A3Config] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_A3Config(n_per_row = 30L)@n_per_row
setup_A3Config <- function(
  n_per_row = 21L,
  position_every = 10L,
  ptm_placement = "radial",
  zoom = TRUE,
  residue_spacing = 0.3,
  marker_size = 28,
  font_size = 18,
  line_width = 2,
  show_markers = TRUE,
  show_labels = TRUE,
  region_opacity = 0.35,
  residue_fill = "#E7E5E4",
  residue_stroke = "#44403C",
  label_color = "#1C1917",
  pos_label_color = "#78716C",
  variant_color = "#FA6E1E",
  disease_variant_color = "#E266AE",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), A3_ORIGIN_NAMES)
  A3Config(
    n_per_row = as.integer(n_per_row),
    position_every = as.integer(position_every),
    ptm_placement = ptm_placement,
    zoom = zoom,
    residue_spacing = residue_spacing,
    marker_size = marker_size,
    font_size = font_size,
    line_width = line_width,
    show_markers = show_markers,
    show_labels = show_labels,
    region_opacity = region_opacity,
    residue_fill = residue_fill,
    residue_stroke = residue_stroke,
    label_color = label_color,
    pos_label_color = pos_label_color,
    variant_color = variant_color,
    disease_variant_color = disease_variant_color,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_A3Config


# %% resolve.A3Config ----
# Nothing to derive: no axes, and the layout is computed by the builder.
method(resolve, A3Config) <- function(config, data = NULL, ...) {
  config
}


# %% a3_built ----
#' Build an A3 diagram's option and render hints together
#'
#' @param config [A3Config]: The chart configuration.
#' @param data Optional `A3` object: The protein to draw.
#' @param width Optional Numeric or Character: Requested widget width, which the
#'   row layout is solved against.
#'
#' @return Named list: `option` and `render`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
a3_built <- function(config, data = NULL, width = NULL) {
  bound <- config_data(config, data)
  config <- resolve(config, data = bound)
  a3_option(
    width = width,
    x = bound,
    n_per_row = config@n_per_row,
    residue_spacing = config@residue_spacing,
    marker_size = config@marker_size,
    font_size = config@font_size,
    line_width = config@line_width,
    show_markers = config@show_markers,
    show_labels = config@show_labels,
    position_every = config@position_every,
    region_opacity = config@region_opacity,
    ptm_placement = config@ptm_placement,
    residue_fill = config@residue_fill,
    residue_stroke = config@residue_stroke,
    label_color = config@label_color,
    pos_label_color = config@pos_label_color,
    variant_color = config@variant_color,
    disease_variant_color = config@disease_variant_color,
    enable_zoom = config@zoom,
    title = config@title,
    grid = NULL,
    height = NULL
  )
} # /rtemis.draw::a3_built


# %% compile.A3Config ----
method(compile, A3Config) <- function(config, data = NULL, ...) {
  a3_built(config, data)[["option"]]
}


# %% draw.A3Config ----
# Overrides the generic method so the derived height reaches the widget. An
# explicit `height` wins: the caller's surface beats the diagram's estimate of
# what it needs.
method(draw, A3Config) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL
) {
  built <- a3_built(option, data, width = width)
  draw(
    built[["option"]],
    theme = theme,
    width = width,
    height = height %||% built[["render"]][["height"]],
    element_id = element_id,
    filename = filename,
    animation = animation,
    ...
  )
}
