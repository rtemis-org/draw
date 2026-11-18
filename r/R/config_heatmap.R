# config_heatmap.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The heatmap's config. Like the network, its bound data is a structure -- a
# numeric matrix, or a heatmap record of the matrix and its annotations --
# rather than columns in a table, so it declares no column names. R validates
# the matrix, applies `triangle`, and clusters (`heatmap_data()`); the builder
# draws the rest, and the widget fits square cells from `heatmapHints()`.

# %% HeatmapConfig ----
#' Heatmap Configuration
#'
#' A serializable description of a heatmap. Build one with
#' [setup_HeatmapConfig()] rather than calling this constructor directly.
#'
#' The bound data is a numeric matrix, or a record containing `values` and optional
#' `row_tree`, `col_tree`, `cell_notes`, `row_colors`, and `col_colors` (see
#' [draw_heatmap()]). These annotations remain data, outside the config.
#'
#' For a matrix, `row_names` and `col_names` override the
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
#'   container to match. A filled viewer or browser window keeps its height and
#'   the grid fits within it.
#' @param colormap Optional Character: Colors defining the continuous scale.
#'   `NULL` derives a theme-aware diverging or sequential scale.
#' @param zlim Optional Numeric: Color-scale limits, length 2.
#' @param show_notes Logical: Print supplied cell notes instead of numeric values.
#' @param row_cut,col_cut Optional Integer: Number of clusters for branch coloring,
#'   requiring a computed or supplied tree.
#' @param show_values Logical: Print each cell's value.
#' @param value_digits Integer `[0, Inf)`: Decimal places for printed values,
#'   written as [rtemis.core::ddSci()] writes them.
#' @param show_colorbar Logical: Draw the color bar.
#' @param colorbar_orient Character \{"vertical", "horizontal"\}: Color bar
#'   orientation.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Override the auto-computed margins, per side.
#' @inheritParams draw_line legend_position legend_placement legend_title
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
  properties = c(
    legend_properties("right"),
    list(
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
          "Colors defining the continuous scale. Unset derives a theme-aware",
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
      show_notes = prop_boolean(
        FALSE,
        description = "Print supplied cell notes instead of numeric values."
      ),
      row_cut = prop_integer(
        NULL,
        min = 1L,
        nullable = TRUE,
        description = "Number of clusters for row-tree branch coloring."
      ),
      col_cut = prop_integer(
        NULL,
        min = 1L,
        nullable = TRUE,
        description = "Number of clusters for column-tree branch coloring."
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
  )
) # /rtemis.draw::HeatmapConfig


# %% HEATMAP_ORIGIN_NAMES ----
HEATMAP_ORIGIN_NAMES <- setdiff(
  names(HeatmapConfig@properties),
  c("type", PROVENANCE_PROPS)
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
#' draw(setup_HeatmapConfig(), data = cor(mtcars[, c("mpg", "wt", "hp", "qsec")]))
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
  show_notes = FALSE,
  row_cut = NULL,
  col_cut = NULL,
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
  writer = NULL,
  legend_position = if (colorbar_orient == "horizontal") "bottom" else "right",
  legend_placement = "outside",
  legend_title = NULL
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
    show_notes = show_notes,
    row_cut = row_cut,
    col_cut = col_cut,
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
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
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
# The option is built in JavaScript by @rtemis/draw, so compiling a heatmap
# config gives the request the widget and SVG export pass to
# `RtemisDraw.builders.heatmap`: the prepared matrix, trees, notes, and
# tracks, and the config. spec: draw/shared-builders#heatmap
method(compile, HeatmapConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  heatmap_request(config, data, theme = theme)
}


# %% render_meta.HeatmapConfig ----
# The legend members place the color scale, the heatmap's legend.
method(render_meta, HeatmapConfig) <- function(config, option) {
  list(legendTarget = "visualMap")
}


# %% heatmap_data ----
#' The heatmap builder's data from a matrix or heatmap record
#'
#' Validates the input (`heatmap_input()`), applies the config's `triangle`
#' and clustering, and writes the matrix, its labels, the trees, the cell
#' notes, and the color tracks as @rtemis/draw's `HeatmapData` takes them.
#' `triangle` masks the other triangle and the diagonal and drops rows and
#' columns left empty. Clustering covers the rows (columns) with at least one
#' value, which swap among their positions; a tree's `leaves` name those rows.
#'
#' @param config [HeatmapConfig]: The chart configuration.
#' @param x Numeric matrix or named list: The matrix, or a heatmap record.
#' @param row_tree,col_tree,cell_notes,row_colors,col_colors Optional:
#'   Annotations, as [draw_heatmap()] takes them.
#'
#' @return Named list for `jsonlite::toJSON(auto_unbox = TRUE)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
heatmap_data <- function(
  config,
  x,
  row_tree = NULL,
  col_tree = NULL,
  cell_notes = NULL,
  row_colors = NULL,
  col_colors = NULL
) {
  input <- heatmap_input(
    x,
    row_tree,
    col_tree,
    cell_notes,
    row_colors,
    col_colors
  )
  x <- input[["values"]]
  row_h <- input[["row_tree"]]
  col_h <- input[["col_tree"]]
  notes <- input[["cell_notes"]]
  row_colors <- input[["row_colors"]]
  col_colors <- input[["col_colors"]]
  if (config@show_notes && (is.null(notes) || config@show_values)) {
    abort(
      "Use show_notes with supplied cell_notes, and choose either notes or numeric value labels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  triangle <- config@triangle
  if (!is.null(triangle) && (!is.null(row_h) || !is.null(col_h))) {
    abort(
      "Use a full matrix with supplied trees; triangle masking removes leaves.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  rn <- config@row_names %||% rownames(x) %||% as.character(seq_len(nrow(x)))
  cn <- config@col_names %||% colnames(x) %||% as.character(seq_len(ncol(x)))
  if (
    length(rn) != nrow(x) || length(cn) != ncol(x) || anyNA(rn) || anyNA(cn)
  ) {
    abort(
      "Supply one nonmissing display label per matrix row and column.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(triangle)) {
    mask <- if (triangle == "upper") lower.tri(x) else upper.tri(x)
    x[mask] <- NA
    diag(x) <- NA
    rows_keep <- apply(x, 1L, function(r) any(!is.na(r)))
    cols_keep <- apply(x, 2L, function(c) any(!is.na(c)))
    if (!any(rows_keep) || !any(cols_keep)) {
      abort(
        "Choose a triangle containing at least one finite off-diagonal value.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    x <- x[rows_keep, cols_keep, drop = FALSE]
    if (!is.null(notes)) {
      notes <- notes[rows_keep, cols_keep, drop = FALSE]
    }
    if (!is.null(row_colors)) {
      row_colors <- row_colors[rows_keep, , drop = FALSE]
    }
    if (!is.null(col_colors)) {
      col_colors <- col_colors[cols_keep, , drop = FALSE]
    }
    rn <- rn[rows_keep]
    cn <- cn[cols_keep]
  }
  # A computed tree covers the rows (columns) with a value, by data index.
  cluster <- function(m, cluster_it) {
    if (!cluster_it || nrow(m) < 2L) {
      return(NULL)
    }
    complete <- rowSums(!is.na(m)) > 0L
    if (sum(complete) < 2L) {
      return(NULL)
    }
    d <- stats::dist(m[complete, , drop = FALSE], method = config@dist_method)
    h <- stats::hclust(d, method = config@hclust_method)
    list(
      merge = h[["merge"]],
      height = h[["height"]],
      order = h[["order"]],
      leaves = which(complete) - 1L
    )
  }
  tree_json <- function(h) {
    if (is.null(h)) {
      return(NULL)
    }
    # Lists keep one-element vectors as JSON arrays.
    list(
      merge = lapply(seq_len(nrow(h[["merge"]])), function(i) {
        as.list(as.integer(h[["merge"]][i, ]))
      }),
      height = as.list(as.numeric(h[["height"]])),
      order = as.list(as.integer(h[["order"]])),
      leaves = if (!is.null(h[["leaves"]])) as.list(as.integer(h[["leaves"]]))
    )
  }
  row_h <- row_h %||% cluster(x, config@cluster_rows)
  col_h <- col_h %||% cluster(t(x), config@cluster_cols)
  by_row <- function(m) {
    if (is.null(m)) {
      return(NULL)
    }
    lapply(seq_len(nrow(m)), function(i) as.list(unname(m[i, ])))
  }
  list(
    values = lapply(seq_len(nrow(x)), function(i) {
      lapply(unname(x[i, ]), function(v) if (is.na(v)) NULL else v)
    }),
    row_names = as.list(rn),
    col_names = as.list(cn),
    row_tree = tree_json(row_h),
    col_tree = tree_json(col_h),
    cell_notes = by_row(notes),
    row_colors = by_row(row_colors),
    col_colors = by_row(col_colors)
  )
} # /rtemis.draw::heatmap_data


# %% heatmap_request ----
#' The build request of a heatmap config and its data
#'
#' @inheritParams heatmap_data
#' @param theme Optional [Theme]: Theme whose sign colors and background the
#'   derived color scale takes.
#'
#' @return [BuildRequest] of type `heatmap`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
heatmap_request <- function(config, x, ..., theme = NULL) {
  data <- heatmap_data(config, x, ...)
  appearance <- list()
  if (S7::S7_inherits(theme, Theme)) {
    sign <- theme_sign_colors(theme)
    appearance[["signColors"]] <- list(
      negative = sign[["negative"]],
      positive = sign[["positive"]]
    )
    appearance[["background"]] <- theme@background_color
  }
  BuildRequest(
    type = "heatmap",
    data = as.character(jsonlite::toJSON(
      Filter(Negate(is.null), data),
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = appearance
  )
} # /rtemis.draw::heatmap_request
