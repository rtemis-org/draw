# draw_a3.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The A3 diagram's convenience function. Its arguments become an A3Config, and
# the ECharts option is built in the widget by @rtemis/draw's A3 builder
# (config_a3.R, build_request.R).
# spec: draw/shared-builders#r-widget

# %% draw_a3 ----
#' Draw an A3 Amino Acid Sequence Visualization
#'
#' Renders an `A3` object (from `rtemis.a3`) as an interactive ECharts
#' amino-acid sequence diagram. The sequence is wrapped into rows in a
#' meander/serpentine path, with optional site, region, PTM, processing, and
#' variant annotations overlaid as distinct series and collected in an
#' upper-right annotation column. The ECharts option is built in the widget by
#' the `@rtemis/draw` A3 builder, the one rtemislive uses, from the A3 document
#' and an [A3Config]. Legend headings use native rich text and remain
#' available in vector SVG exports through [save_drawing()], which builds the
#' option the same way in Node.js.
#'
#' The annotation column stays at the upper right on every surface. Residue
#' markers, labels, and annotations shrink together to avoid overlap. Row
#' wrapping remains fixed. Browser height follows the layout unless supplied;
#' SVG and panel dimensions remain bounded. A custom `grid` retains its layout.
#'
#' @param x `A3` object from [rtemis.a3::create_A3()] or
#'   [rtemis.a3::read_A3json()].
#' @param n_per_row Integer `[2, Inf)`: Number of residues per wrapped row.
#'   Reduce this for larger residues on narrow surfaces.
#' @param residue_spacing Numeric `(0, Inf)`: Horizontal spacing multiplier.
#'   Values below `1` compress the layout; `1` gives natural hex spacing.
#' @param marker_size Numeric `(0, Inf)`: Preferred residue circle diameter in
#'   pixels; reduced with related marks when needed to fit the drawing surface.
#' @param font_size Numeric `(0, Inf)`: Base font size in pixels for residue
#'   and position labels.
#' @param line_width Numeric `(0, Inf)`: Backbone line width in pixels.
#' @param annotation_gap Numeric `[0, Inf)`: Gap in pixels between residue
#'   anchors and processing overlays.
#' @param position_label_gap Numeric `[0, Inf)`: Gap in pixels between residue
#'   anchors and position labels.
#' @param show_markers Logical: Whether to render the backbone line and
#'   residue circles.
#' @param show_labels Logical: Whether to render single-letter residue labels.
#' @param position_every Optional Integer `[1, Inf)`: Show a numeric position
#'   label every N residues. `NULL` disables position labels.
#' @param region_opacity Numeric `[0, 1]`: Opacity of region band overlays.
#' @param ptm_placement Character `{"radial", "innerRadial", "outerRadial"}`:
#'   Placement of PTM symbols relative to the residue circle.
#'   `"radial"` centers on the circle edge; `"innerRadial"` places inside;
#'   `"outerRadial"` places outside.
#' @param residue_fill Optional Character: Residue circle fill color.
#' @param residue_stroke Optional Character: Residue circle stroke and backbone line color.
#' @param label_color Optional Character: Default residue label text color.
#' @param pos_label_color Optional Character: Position label text color.
#' @param variant_color Optional Character: Label color for variant residues.
#' @param disease_variant_color Optional Character: Label color for disease-associated
#'   variant residues (takes precedence over `variant_color`).
#'   `NULL` color arguments follow the light/dark A3 theme.
#' @param zoom Logical: Enable Shift+wheel zoom, and wheel and drag panning,
#'   along the sequence.
#' @param title Optional Character: Chart title.
#' @param grid Optional [Grid]: Plot-area margins. The fields given replace the
#'   built ones, e.g. `Grid(left = 8, top = 8)`, and the right margin stays as
#'   given instead of following the measured legend width.
#' @param theme Optional [Theme]: Theme override. `NULL` auto-detects
#'   light/dark mode; `NA` uses raw ECharts defaults.
#' @param width Optional Numeric or Character: Widget width.
#' @param height Optional Numeric or Character: Widget height. `NULL` takes
#'   the height of the layout from the builder.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this
#'   file via [save_drawing()].
#' @param enable_zoom Deprecated Logical: Former name of `zoom`; a value sets
#'   `zoom` with a warning.
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' if (requireNamespace("rtemis.a3", quietly = TRUE)) {
#'   a <- rtemis.a3::create_A3(
#'     "MAEPRQEFEVMEDHAGTYGLGDRK",
#'     site = list(
#'       Active_site = rtemis.a3::annotation_position(c(5L, 17L))
#'     ),
#'     region = list(
#'       Domain = rtemis.a3::annotation_range(
#'         matrix(c(3L, 10L, 15L, 22L), ncol = 2, byrow = TRUE)
#'       )
#'     )
#'   )
#'   draw_a3(a)
#' }
draw_a3 <- function(
  x,
  n_per_row = 21L,
  residue_spacing = 0.3,
  marker_size = 28,
  font_size = 18,
  line_width = 2,
  annotation_gap = 16,
  position_label_gap = 16,
  show_markers = TRUE,
  show_labels = TRUE,
  position_every = 10L,
  region_opacity = 0.3,
  ptm_placement = "radial",
  residue_fill = NULL,
  residue_stroke = NULL,
  label_color = NULL,
  pos_label_color = NULL,
  variant_color = NULL,
  disease_variant_color = NULL,
  zoom = TRUE,
  title = NULL,
  grid = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  enable_zoom = NULL
) {
  if (!is.null(enable_zoom)) {
    warn(
      "`enable_zoom` is deprecated; use `zoom`.",
      use_warning = TRUE
    )
    zoom <- enable_zoom
  }
  config <- setup_A3Config(
    n_per_row = n_per_row,
    position_every = position_every,
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
    title = title
  )
  draw(
    config,
    data = x,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    grid = grid
  )
}
