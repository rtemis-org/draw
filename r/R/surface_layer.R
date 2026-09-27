# surface_layer.R
# Explicit prediction grids share their topology between ECharts-GL and SVG.

#' Supplied Three-Dimensional Surface
#'
#' A materialized grid of predictions z = f(x, y), independent of the model
#' that produced them. Corresponds to ECharts-GL `SurfaceSeries` in
#' `src/chart/surface/SurfaceSeries.js`. The grid uses flat color shading and
#' no wireframe, matching the rtemislive surface contract. Each grid rectangle
#' is split along ECharts-GL's top-left to bottom-right diagonal. Missing z
#' values remove every adjacent rectangle; they are not interpolated.
#'
#' Coordinates are sorted together with the prediction matrix. SVG export uses
#' the configured orthographic camera and splits intersecting polygons to order
#' visible surfaces, paths and points. Dense or highly intersecting surfaces
#' may require a coarser prediction grid for practical vector output.
#' @param x,y Numeric: Finite, distinct grid coordinates, at least two per axis.
#' @param z Numeric matrix: Predictions, with x along rows and y along columns.
#'   NA removes adjacent cells. At least one complete grid cell is required.
#' @param name Character: Legend label; matching names link legend selection.
#' @param color Character: Surface fill color.
#' @param opacity Numeric `[0, 1]`: Surface opacity.
#' @return SurfaceLayer: Validated prediction grid.
#' @export
#' @examples
#' SurfaceLayer(x = 0:2, y = 0:2, z = outer(0:2, 0:2, `+`))
SurfaceLayer <- new_class(
  "SurfaceLayer",
  package = "rtemis.draw",
  properties = list(
    x = prop_float(c(0, 1), vector = TRUE, min_items = 2L),
    y = prop_float(c(0, 1), vector = TRUE, min_items = 2L),
    z = new_property(class_any, default = NULL),
    name = prop_string("Surface"),
    color = prop_string("#6ca3a0"),
    opacity = prop_float(.35, min = 0, max = 1)
  ),
  validator = function(self) {
    if (anyDuplicated(self@x) || anyDuplicated(self@y)) {
      return("Supply distinct surface coordinates on each axis.")
    }
    z <- self@z
    if (
      !is.matrix(z) ||
        !is.numeric(z) ||
        is.complex(z) ||
        !identical(dim(z), c(length(self@x), length(self@y))) ||
        any(is.infinite(z))
    ) {
      return(
        "Supply a numeric z matrix with one row per x and one column per y, allowing NA but not infinity."
      )
    }
    # Validate after sorting: holes and adjacency refer to the displayed grid.
    z <- z[order(self@x), order(self@y), drop = FALSE]
    rows <- seq_len(nrow(z) - 1L)
    cols <- seq_len(ncol(z) - 1L)
    complete <- is.finite(z[rows, cols, drop = FALSE]) &
      is.finite(z[rows + 1L, cols, drop = FALSE]) &
      is.finite(z[rows, cols + 1L, drop = FALSE]) &
      is.finite(z[rows + 1L, cols + 1L, drop = FALSE])
    if (!any(complete)) {
      return("Supply at least one surface cell with four finite corners.")
    }
    NULL
  }
)

method(to_list, SurfaceLayer) <- function(x) {
  ix <- order(x@x)
  iy <- order(x@y)
  z <- x@z[ix, iy, drop = FALSE]
  # ECharts-GL dataShape is [rows, columns], with x varying fastest.
  data <- unlist(
    lapply(seq_along(iy), function(j) {
      lapply(seq_along(ix), function(i) {
        list(x@x[ix[[i]]], x@y[iy[[j]]], if (is.na(z[i, j])) NULL else z[i, j])
      })
    }),
    recursive = FALSE
  )
  list(
    type = "surface",
    name = x@name,
    shading = "color",
    wireframe = list(show = FALSE),
    dataShape = list(length(iy), length(ix)),
    itemStyle = list(color = x@color, opacity = x@opacity),
    data = data
  )
}

#' Add a Supplied Surface to a Three-Dimensional Drawing
#'
#' Fits are computed explicitly before drawing. Supply predictions from any
#' model on a rectangular grid; fitted R objects and callbacks are not stored
#' in the drawing. The prediction grid may extend the original axis ranges.
#' Add surfaces before composing drawings with [draw_panels()].
#' @param plot htmlwidget: Existing Cartesian ECharts-GL drawing.
#' @inheritParams SurfaceLayer
#' @param layer Optional SurfaceLayer: Complete geometry, supplied alone.
#' @param expand Logical: Extend axis ranges to include surface coordinates.
#'   When false, all finite surface coordinates must already lie within them.
#' @return htmlwidget: Drawing with native interactive and vector surface layers.
#' @export
#' @examples
#' x <- y <- seq(-1, 1, length.out = 5)
#' draw_add_surface(draw_scatter3d(c(-1, 1), c(-1, 1), c(-1, 1)),
#'   x, y, outer(x, y, `+`), name = "Predicted")
draw_add_surface <- function(
  plot,
  x = NULL,
  y = NULL,
  z = NULL,
  name = "Surface",
  color = "#6ca3a0",
  opacity = .35,
  layer = NULL,
  expand = TRUE
) {
  if (is.null(layer)) {
    layer <- SurfaceLayer(
      x = x,
      y = y,
      z = z,
      name = name,
      color = color,
      opacity = opacity
    )
  } else if (
    !S7_inherits(layer, SurfaceLayer) ||
      length(setdiff(
        names(as.list(match.call()))[-1L],
        c("plot", "layer", "expand")
      ))
  ) {
    abort(
      "Supply a SurfaceLayer alone, or supply grid coordinates and surface settings.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !inherits(plot, "rtemis-draw") ||
      is.null(plot[["x"]][["option"]][["grid3D"]])
  ) {
    abort(
      "Add a surface to a 3D Cartesian drawing before composing panels.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  plot[["x"]][["option"]] <- surface_layer_option(
    layer,
    plot[["x"]][["option"]],
    expand
  )
  plot
}

#' Compose a surface with validated Cartesian 3D axes
#' @param option List: Native option from a 3D drawing.
#' @param layer SurfaceLayer: Prediction grid.
#' @param expand Logical: Whether to extend coordinate ranges.
#' @return List: Combined native option.
#' @keywords internal
#' @noRd
surface_layer_option <- new_generic("surface_layer_option", "layer")
method(surface_layer_option, SurfaceLayer) <- function(layer, option, expand) {
  if (!is.logical(expand) || length(expand) != 1L || is.na(expand)) {
    abort(
      "Set expand to TRUE or FALSE.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  coordinates <- list(layer@x, layer@y, layer@z[is.finite(layer@z)])
  for (i in seq_len(3L)) {
    key <- c("xAxis3D", "yAxis3D", "zAxis3D")[[i]]
    axis <- option[[key]]
    limits <- c(axis[["min"]], axis[["max"]])
    if (
      is.null(axis) ||
        !identical(axis[["type"]], "value") ||
        !is.numeric(limits) ||
        length(limits) != 2L ||
        any(!is.finite(limits))
    ) {
      abort(
        "Use one numeric Cartesian 3D axis per dimension with explicit finite limits.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    extent <- range(c(limits, coordinates[[i]]))
    if (!is.finite(diff(extent)) || diff(extent) <= 0) {
      abort(
        "Rescale surface coordinates to a finite nonzero range.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (!expand && any(extent != limits)) {
      abort(
        "Keep surface coordinates within the existing axes, or set expand = TRUE.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    axis[["min"]] <- extent[[1L]]
    axis[["max"]] <- extent[[2L]]
    axis[["interval"]] <- diff(extent) / 4
    option[[key]] <- axis
  }
  option[["series"]] <- c(option[["series"]], list(to_list(layer)))
  # Replace the array directly: modifyList merges named settings, but cannot
  # replace an existing unnamed legend array when another surface is added.
  legend <- option[["legend"]] %||% list()
  legend[["show"]] <- TRUE
  legend[["data"]] <- as.list(unique(vapply(
    option[["series"]],
    function(series) series[["name"]] %||% "",
    character(1L)
  )))
  option[["legend"]] <- legend
  option
}
