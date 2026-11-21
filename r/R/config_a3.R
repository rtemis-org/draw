# config_a3.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The annotated protein diagram's config. Bound data is an `A3` object from
# rtemis.a3 -- a domain structure, not columns -- so this config declares no
# bindings, only how the diagram is drawn.
#
# Its render hints describe preferred glyph sizes and automatic grid/height
# fitting. These surface constraints go to draw(), separately from the portable
# config document. Browser and SVG rendering resolve the available space.

# %% A3Config ----
#' Annotated Protein Diagram Configuration
#'
#' A serializable description of an A3 protein diagram. Build one with
#' [setup_A3Config()] rather than calling this constructor directly.
#'
#' The bound data is an `A3` object, created with `rtemis.a3::create_A3()`.
#'
#' @param n_per_row Integer `[2, Inf)`: Residues per row.
#' @param position_every Optional Integer `[1, Inf)`: Label every nth position.
#'   `NULL` draws no position labels.
#' @param ptm_placement Character \{"radial", "innerRadial", "outerRadial"\}:
#'   How post-translational modifications are placed relative to the backbone.
#' @param zoom Logical: Enable Shift+wheel zoom, and wheel and drag panning,
#'   along the sequence.
#' @param residue_spacing Numeric `(0, Inf)`: Gap between residues.
#' @param marker_size Numeric `[0, Inf)`: Preferred residue marker size;
#'   reduced with related marks when needed to fit the surface.
#' @param font_size Numeric `[0, Inf)`: Residue label font size.
#' @param line_width Numeric `[0, Inf)`: Backbone line width.
#' @param annotation_gap Numeric `[0, Inf)`: Gap in pixels between residue
#'   anchors and processing overlays.
#' @param position_label_gap Numeric `[0, Inf)`: Gap in pixels between residue
#'   anchors and position labels.
#' @param show_markers,show_labels Logical: Whether to draw each.
#' @param region_opacity Numeric `[0, 1]`: Region band opacity.
#' @param residue_fill,residue_stroke Optional Character: Residue marker colors.
#' @param label_color,pos_label_color Optional Character: Label colors.
#' @param variant_color,disease_variant_color Optional Character: Variant marker colors.
#'   `NULL` selects the theme's color, its [A3Theme] (`a3`).
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
    n_per_row = prop_integer(
      21L,
      min = 2L,
      description = "Residues per row."
    ),
    position_every = prop_integer(
      10L,
      min = 1L,
      nullable = TRUE,
      description = "Label every nth position; null draws no position labels."
    ),
    ptm_placement = prop_string(
      "radial",
      enum = c("radial", "innerRadial", "outerRadial"),
      description = "How modifications are placed relative to the backbone."
    ),
    zoom = prop_boolean(
      TRUE,
      description = "Enable Shift+wheel zoom, and wheel and drag panning, along the sequence."
    ),
    # -- appearance --------------------------------------------------------
    residue_spacing = prop_float(
      0.3,
      exclusive_min = 0,
      description = "Gap between residues."
    ),
    marker_size = prop_float(28, min = 0, description = "Residue marker size."),
    font_size = prop_float(
      18,
      min = 0,
      description = "Residue label font size."
    ),
    line_width = prop_float(2, min = 0, description = "Backbone line width."),
    annotation_gap = prop_float(
      16,
      min = 0,
      description = "Gap in pixels between residue anchors and processing overlays."
    ),
    position_label_gap = prop_float(
      16,
      min = 0,
      description = "Gap in pixels between residue anchors and position labels."
    ),
    show_markers = prop_boolean(TRUE, description = "Draw residue markers."),
    show_labels = prop_boolean(TRUE, description = "Draw residue labels."),
    region_opacity = prop_float(
      0.3,
      min = 0,
      max = 1,
      description = "Region band opacity."
    ),
    residue_fill = prop_string(
      NULL,
      nullable = TRUE,
      description = "Residue marker fill."
    ),
    residue_stroke = prop_string(
      NULL,
      nullable = TRUE,
      description = "Residue marker outline."
    ),
    label_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Residue label color."
    ),
    pos_label_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Position label color."
    ),
    variant_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Variant marker color."
    ),
    disease_variant_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Disease-variant marker color."
    )
  )
) # /rtemis.draw::A3Config


# %% A3_ORIGIN_NAMES ----
A3_ORIGIN_NAMES <- setdiff(
  names(A3Config@properties),
  c("type", PROVENANCE_PROPS)
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
  annotation_gap = 16,
  position_label_gap = 16,
  show_markers = TRUE,
  show_labels = TRUE,
  region_opacity = 0.3,
  residue_fill = NULL,
  residue_stroke = NULL,
  label_color = NULL,
  pos_label_color = NULL,
  variant_color = NULL,
  disease_variant_color = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), A3_ORIGIN_NAMES)
  A3Config(
    n_per_row = as.integer(n_per_row),
    position_every = if (is.null(position_every)) {
      NULL
    } else {
      as.integer(position_every)
    },
    ptm_placement = ptm_placement,
    zoom = zoom,
    residue_spacing = residue_spacing,
    marker_size = marker_size,
    font_size = font_size,
    line_width = line_width,
    annotation_gap = annotation_gap,
    position_label_gap = position_label_gap,
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


# %% A3_APPEARANCE ----
# The builder's appearance input from R: the legend gap rtemis.draw's A3
# diagrams have used. The theme is chosen in the widget, which knows whether
# it renders light or dark.
A3_APPEARANCE <- list(legendGap = 40)


# %% compile.A3Config ----
# The option is built in JavaScript by @rtemis/draw, so compiling an A3 config
# gives the request the widget and SVG export pass to `RtemisDraw.builders.a3`.
# spec: draw/shared-builders#r-widget
method(compile, A3Config) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...,
  grid = NULL
) {
  appearance <- A3_APPEARANCE
  if (!is.null(grid)) {
    appearance[["grid"]] <- to_list(grid)
  }
  BuildRequest(
    type = "a3",
    data = a3_document_json(data),
    config = config_members(config),
    appearance = appearance
  )
}


# %% draw.A3Config ----
# Overrides the generic method to pass the surface flags the A3 fit in the
# widget reads: an explicit `height` or `grid` wins over the fitted one.
method(draw, A3Config) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL,
  grid = NULL
) {
  # A config read from a chart record draws with the record's theme unless
  # the caller passes one. spec: draw/chart-appearance#the-theme
  theme <- theme %||% option@theme
  data <- config_data(option, data)
  if (!is.null(grid) && !S7::S7_inherits(grid, Grid)) {
    abort(
      "`grid` must be a Grid object or NULL.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  draw(
    compile(option, data = data, theme = theme, grid = grid),
    theme = theme,
    width = width,
    height = height %||% a3_initial_height(data, option),
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = list(
      a3 = list(autoHeight = is.null(height), autoGrid = is.null(grid))
    ),
    ...
  )
}


# %% a3_initial_height ----
#' Initial widget height of an A3 diagram
#'
#' The container size before the widget renders: the plot body of the wrapped
#' rows plus fixed margins. With `height` unset the widget then sets the exact
#' height that `RtemisDraw.a3SurfaceHints()` gives, and SVG export uses that
#' height too.
#'
#' @param x `A3` object.
#' @param config [A3Config].
#'
#' @return Numeric: Height in pixels.
#'
#' @author EDG
#' @keywords internal
#' @noRd
a3_initial_height <- function(x, config) {
  n <- nchar(x[["sequence"]])
  rows <- max(1, ceiling((n - 1) / (config@n_per_row - 1)))
  72 + config@marker_size * (2 * rows + 1)
}


# %% a3_document_json ----
#' Serialize an A3 object as its A3 JSON document
#'
#' @param x `A3` object from rtemis.a3.
#'
#' @return Character: The A3 JSON document, as `rtemis.a3::write_A3json()`
#'   writes it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
a3_document_json <- function(x) {
  if (!S7::S7_inherits(x) || !grepl("::A3$|^A3$", S7::S7_class(x)@name)) {
    abort(
      "`data` must be an A3 object. ",
      "Create one with `rtemis.a3::create_A3()`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  file <- tempfile(fileext = ".a3.json")
  on.exit(unlink(file), add = TRUE)
  rtemis.a3::write_A3json(x, file)
  paste(readLines(file, warn = FALSE), collapse = "\n")
}
