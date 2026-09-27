# fit_layer.R
# Materialized prediction geometry keeps model fitting outside the renderer.

#' Supplied Fit Layer
#'
#' Coordinates and optional interval bounds for a fitted curve, independent of
#' the model or language that produced it. Uses LineSeriesOption and
#' CustomSeriesOption in ECharts src/chart/line and src/chart/custom. Rows are
#' sorted together by x. Missing predictions must be removed before construction;
#' use separate layers for disconnected intervals. No fitting is performed here.
#' @param x,y Numeric: Finite curve coordinates with distinct x values.
#' @param lower,upper Optional Numeric: Paired finite interval bounds at every x.
#' @param name Character: Legend label; a matching observation label links selection.
#' @param color Character: Curve and interval color.
#' @param line_width Numeric `(0, Inf)`: Curve width in pixels.
#' @param opacity Numeric `[0, 1]`: Interval opacity.
#' @param x_axis,y_axis Integer `[0, Inf)`: Zero-based axis indices.
#' @return FitLayer: Validated, serializable prediction geometry.
#' @export
#' @examples
#' FitLayer(x = 1:3, y = c(2, 4, 5), lower = c(1, 3, 4), upper = c(3, 5, 6))
FitLayer <- new_class(
  "FitLayer",
  package = "rtemis.draw",
  properties = list(
    x = prop_float(c(0, 1), vector = TRUE, min_items = 2L),
    y = prop_float(c(0, 1), vector = TRUE, min_items = 2L),
    lower = prop_float(NULL, nullable = TRUE, vector = TRUE, min_items = 2L),
    upper = prop_float(NULL, nullable = TRUE, vector = TRUE, min_items = 2L),
    name = prop_string("Fit"),
    color = prop_string("#6ca3a0"),
    line_width = prop_float(2, exclusive_min = 0),
    opacity = prop_float(.2, min = 0, max = 1),
    x_axis = prop_integer(0L, min = 0L),
    y_axis = prop_integer(0L, min = 0L)
  ),
  validator = function(self) {
    if (length(self@x) != length(self@y) || anyDuplicated(self@x)) {
      return("Supply equally sized curve coordinates with distinct x values.")
    }
    if (is.null(self@lower) != is.null(self@upper)) {
      return("Supply both interval bounds or neither.")
    }
    if (
      !is.null(self@lower) &&
        (length(self@lower) != length(self@x) ||
          length(self@upper) != length(self@x) ||
          any(self@lower > self@upper))
    ) {
      return("Supply ordered interval bounds at every curve coordinate.")
    }
    NULL
  }
)
method(to_list, FitLayer) <- function(x) {
  index <- order(x@x)
  curve <- to_list(LineSeries(
    name = x@name,
    data = Map(c, x@x[index], x@y[index]),
    show_symbol = FALSE,
    line_style = LineStyle(color = x@color, width = x@line_width),
    silent = TRUE
  ))
  curve[["xAxisIndex"]] <- x@x_axis
  curve[["yAxisIndex"]] <- x@y_axis
  series <- list(curve)
  if (!is.null(x@lower)) {
    vertices <- c(
      Map(c, x@x[index], x@upper[index]),
      Map(c, rev(x@x[index]), rev(x@lower[index]))
    )
    series <- c(
      list(list(
        type = "custom",
        name = x@name,
        renderItem = "rtemis.ribbon.v1",
        silent = TRUE,
        clip = TRUE,
        z = 0,
        xAxisIndex = x@x_axis,
        yAxisIndex = x@y_axis,
        itemStyle = list(color = x@color),
        itemPayload = list(vertices = vertices, opacity = x@opacity),
        encode = list(x = list(0L, 1L), y = list(2L, 3L)),
        data = list(c(range(x@x), range(c(x@lower, x@upper))))
      )),
      series
    )
  }
  series
}

#' Add a Supplied Fit and Interval to a Drawing
#'
#' Accepts predictions from any model without keeping a fitted R object in the
#' chart. Existing axis limits are preserved, including explicitly requested
#' limits. Annotate child drawings before composing panels. For built-in GLM or
#' GAM estimation, use [draw_scatter()] or [draw_fit()].
#' @param plot htmlwidget: Existing Cartesian ECharts drawing.
#' @inheritParams FitLayer
#' @param layer Optional FitLayer: Complete geometry; do not combine with other settings.
#' @return htmlwidget: Drawing with native vector curve and interval layers.
#' @export
#' @examples
#' draw_add_fit(draw_scatter(1:3, c(2, 4, 5)), 1:3, c(2.1, 3.8, 5.1), name = "Predicted")
draw_add_fit <- function(
  plot,
  x = NULL,
  y = NULL,
  lower = NULL,
  upper = NULL,
  name = "Fit",
  color = "#6ca3a0",
  line_width = 2,
  opacity = .2,
  x_axis = 0L,
  y_axis = 0L,
  layer = NULL
) {
  if (is.null(layer)) {
    layer <- FitLayer(
      x = x,
      y = y,
      lower = lower,
      upper = upper,
      name = name,
      color = color,
      line_width = line_width,
      opacity = opacity,
      x_axis = clean_int(x_axis),
      y_axis = clean_int(y_axis)
    )
  } else if (
    !S7_inherits(layer, FitLayer) ||
      length(setdiff(names(as.list(match.call()))[-1], c("plot", "layer")))
  ) {
    abort(
      "Supply a FitLayer alone, or supply curve coordinates and settings.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  option <- cartesian_layer_option(plot, layer@x_axis, layer@y_axis)
  option[["series"]] <- c(option[["series"]], to_list(layer))
  if (is.null(option[["legend"]])) {
    option[["legend"]] <- list(show = TRUE)
  }
  if (!is.null(option[["legend"]][["data"]])) {
    option[["legend"]][["data"]] <- as.list(unique(c(
      unlist(option[["legend"]][["data"]], use.names = FALSE),
      layer@name
    )))
  }
  plot[["x"]][["option"]] <- option
  plot
}
