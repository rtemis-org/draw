# config_heatmap.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The heatmap's config. Like the network, its bound data is a **structure** -- a
# numeric matrix -- rather than columns in a table, so it declares no column
# names.
#
# It is also the first config whose `compile()` produces render hints as well as
# an option: the browser needs the cell counts and pixel offsets to size its
# container for square cells. Those go to `draw()` and are never serialized.

# %% HeatmapConfig ----
#' Heatmap Configuration
#'
#' A serializable description of a heatmap. Build one with
#' [setup_HeatmapConfig()] rather than calling this constructor directly.
#'
#' The bound data is a numeric matrix. `row_names` and `col_names` override the
#' matrix's own dimnames rather than naming columns to read.
#'
#' @param row_names,col_names Optional Character: Labels overriding the matrix
#'   dimnames.
#' @param triangle Optional Character \{"lower", "upper"\}: Show only one
#'   triangle, for a symmetric matrix.
#' @param cluster_rows,cluster_cols Logical: Reorder by hierarchical clustering.
#' @param dist_method Character: Distance measure for clustering.
#' @param hclust_method Character: Linkage method for clustering.
#' @param show_row_dendro,show_col_dendro Logical: Draw each dendrogram.
#' @param dendro_row_width,dendro_col_height Numeric `[0, Inf)`: Dendrogram
#'   panel size in pixels.
#' @param dendro_color Optional Character: Dendrogram line color.
#' @param dendro_uniform Logical: Draw dendrograms with uniform branch heights.
#' @param dendro_row_side Character \{"left", "right"\}: Row dendrogram side.
#' @param dendro_col_side Character \{"top", "bottom"\}: Column dendrogram side.
#' @param square_cells Optional Logical: Force square cells, sizing the
#'   container to match.
#' @param colormap Optional Character: Colors defining the continuous scale.
#'   `NULL` derives a theme-aware diverging or sequential scale.
#' @param zlim Optional Numeric: Color-scale limits, length 2.
#' @param show_values Logical: Print each cell's value.
#' @param value_digits Integer `[0, Inf)`: Digits for printed values.
#' @param show_colorbar Logical: Draw the color bar.
#' @param colorbar_orient Character \{"vertical", "horizontal"\}: Color bar
#'   orientation.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Override the auto-computed margins, per side.
#' @inheritParams ChartConfig
#'
#' @return `HeatmapConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_HeatmapConfig(cluster_rows = TRUE)@type
HeatmapConfig <- new_class(
  name = "HeatmapConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("heatmap"),
    # -- data binding: labels for the bound matrix --------------------------
    row_names = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Row labels overriding the matrix dimnames."
    ),
    col_names = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Column labels overriding the matrix dimnames."
    ),
    # -- semantics ---------------------------------------------------------
    triangle = prop_string(
      NULL,
      enum = c("lower", "upper"),
      nullable = TRUE,
      description = "Show only one triangle, for a symmetric matrix."
    ),
    cluster_rows = prop_boolean(
      FALSE,
      description = "Reorder rows by hierarchical clustering."
    ),
    cluster_cols = prop_boolean(
      FALSE,
      description = "Reorder columns by hierarchical clustering."
    ),
    dist_method = prop_string(
      "euclidean",
      description = "Distance measure for clustering."
    ),
    hclust_method = prop_string(
      "complete",
      description = "Linkage method for clustering."
    ),
    square_cells = prop_boolean(
      NULL,
      nullable = TRUE,
      description = "Force square cells, sizing the container to match."
    ),
    # -- appearance --------------------------------------------------------
    show_row_dendro = prop_boolean(
      TRUE,
      description = "Draw the row dendrogram."
    ),
    show_col_dendro = prop_boolean(
      TRUE,
      description = "Draw the column dendrogram."
    ),
    dendro_row_width = prop_float(
      60,
      min = 0,
      description = "Row dendrogram panel width in pixels."
    ),
    dendro_col_height = prop_float(
      60,
      min = 0,
      description = "Column dendrogram panel height in pixels."
    ),
    dendro_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Dendrogram line color."
    ),
    dendro_uniform = prop_boolean(
      FALSE,
      description = "Draw dendrograms with uniform branch heights."
    ),
    dendro_row_side = prop_string(
      "right",
      enum = c("left", "right"),
      description = "Which side the row dendrogram is drawn on."
    ),
    dendro_col_side = prop_string(
      "top",
      enum = c("top", "bottom"),
      description = "Which side the column dendrogram is drawn on."
    ),
    colormap = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = paste(
        "Colors defining the continuous scale. NULL derives a theme-aware",
        "diverging or sequential scale."
      )
    ),
    zlim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Color-scale limits."
    ),
    show_values = prop_boolean(
      FALSE,
      description = "Print each cell's value."
    ),
    value_digits = prop_integer(
      2L,
      min = 0L,
      description = "Digits for printed values."
    ),
    show_colorbar = prop_boolean(TRUE, description = "Draw the color bar."),
    colorbar_orient = prop_string(
      "vertical",
      enum = c("vertical", "horizontal"),
      description = "Color bar orientation."
    ),
    margin_top = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Top margin override in pixels."
    ),
    margin_right = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Right margin override in pixels."
    ),
    margin_bottom = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Bottom margin override in pixels."
    ),
    margin_left = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Left margin override in pixels."
    )
  )
) # /rtemis.draw::HeatmapConfig


# %% HEATMAP_ORIGIN_NAMES ----
HEATMAP_ORIGIN_NAMES <- setdiff(
  names(HeatmapConfig@properties),
  c("type", "origin", "writer")
)


# %% setup_HeatmapConfig ----
#' Set up a Heatmap Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams HeatmapConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [HeatmapConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw(setup_HeatmapConfig(), data = matrix(1:20, nrow = 4))
setup_HeatmapConfig <- function(
  row_names = NULL,
  col_names = NULL,
  triangle = NULL,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  dist_method = "euclidean",
  hclust_method = "complete",
  square_cells = NULL,
  show_row_dendro = TRUE,
  show_col_dendro = TRUE,
  dendro_row_width = 60,
  dendro_col_height = 60,
  dendro_color = NULL,
  dendro_uniform = FALSE,
  dendro_row_side = "right",
  dendro_col_side = "top",
  colormap = NULL,
  zlim = NULL,
  show_values = FALSE,
  value_digits = 2L,
  show_colorbar = TRUE,
  colorbar_orient = "vertical",
  title = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), HEATMAP_ORIGIN_NAMES)
  HeatmapConfig(
    row_names = row_names,
    col_names = col_names,
    triangle = triangle,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    dist_method = dist_method,
    hclust_method = hclust_method,
    square_cells = square_cells,
    show_row_dendro = show_row_dendro,
    show_col_dendro = show_col_dendro,
    dendro_row_width = dendro_row_width,
    dendro_col_height = dendro_col_height,
    dendro_color = dendro_color,
    dendro_uniform = dendro_uniform,
    dendro_row_side = dendro_row_side,
    dendro_col_side = dendro_col_side,
    colormap = colormap,
    zlim = zlim,
    show_values = show_values,
    value_digits = as.integer(value_digits),
    show_colorbar = show_colorbar,
    colorbar_orient = colorbar_orient,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_HeatmapConfig


# %% resolve.HeatmapConfig ----
# Nothing to derive: a heatmap's axes are the matrix dimnames, and its color
# scale is computed by the builder from the theme, which is the interface's.
method(resolve, HeatmapConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.HeatmapConfig ----
# Returns the option only, which is `compile()`'s contract. The render hints the
# builder also produces are recomputed by `draw()` -- see the method below.
method(compile, HeatmapConfig) <- function(config, data = NULL, ...) {
  heatmap_built(config, data)[["option"]]
}


# %% heatmap_built ----
#' Build a heatmap's option and render hints together
#'
#' `compile()` returns an option; a heatmap also derives hints the browser needs
#' to size its container for square cells. Both come from one call, so this
#' shared helper runs it and the two callers take what they need.
#'
#' @param config [HeatmapConfig]: The chart configuration.
#' @param data Optional Matrix: The matrix to plot.
#' @param width,height Optional Numeric or Character: Requested widget size,
#'   which square-cell sizing solves against.
#'
#' @return Named list: `option` and `render`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
heatmap_built <- function(config, data = NULL, width = NULL, height = NULL) {
  bound <- config_data(config, data)
  config <- resolve(config, data = bound)
  heatmap_option(
    width = width,
    height = height,
    x = bound,
    row_names = config@row_names,
    col_names = config@col_names,
    triangle = config@triangle,
    cluster_rows = config@cluster_rows,
    cluster_cols = config@cluster_cols,
    dist_method = config@dist_method,
    hclust_method = config@hclust_method,
    show_row_dendro = config@show_row_dendro,
    show_col_dendro = config@show_col_dendro,
    dendro_row_width = config@dendro_row_width,
    dendro_col_height = config@dendro_col_height,
    dendro_color = config@dendro_color,
    dendro_uniform = config@dendro_uniform,
    dendro_row_side = config@dendro_row_side,
    dendro_col_side = config@dendro_col_side,
    square_cells = config@square_cells,
    colormap = config@colormap,
    zlim = config@zlim,
    show_values = config@show_values,
    value_digits = config@value_digits,
    show_colorbar = config@show_colorbar,
    colorbar_orient = config@colorbar_orient,
    title = config@title,
    margins = config_margins(config)
  )
} # /rtemis.draw::heatmap_built


# %% draw.HeatmapConfig ----
# Overrides the generic `ChartConfig` method because a heatmap's render hints
# are derived from its content and must reach `draw()`. They are computed here
# and passed on; they are never written into a document.
method(draw, HeatmapConfig) <- function(
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
  built <- heatmap_built(option, data, width = width, height = height)
  draw(
    built[["option"]],
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = built[["render"]][["meta"]],
    ...
  )
}
