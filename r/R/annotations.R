# annotations.R
# Native markers preserve data coordinates and export without callbacks.

#' Cartesian Annotation Layer
#'
#' Portable, materialized annotation geometry. Corresponds to MarkLineOption,
#' MarkAreaOption, and ScatterSeriesOption in ECharts src/component/marker and
#' src/chart/scatter. Coordinates are data values (epoch milliseconds for time
#' axes); categorical coordinates may be category names. Layers are independent
#' of group legend selection.
#' @param hline,vline Numeric: Horizontal and vertical reference positions.
#' @param x,y Numeric or Character: Coordinates for text annotations.
#' @param text Character: One label per coordinate pair.
#' @param bands List: Numeric length-two x intervals to shade.
#' @param color Character: Annotation color.
#' @param line_type Character `{"solid", "dashed", "dotted"}`: Reference line style.
#' @param opacity Numeric `[0, 1]`: Band opacity.
#' @param x_axis,y_axis Integer `[0, Inf)`: Zero-based target axis indices.
#' @return AnnotationLayer: Validated annotation geometry.
#' @export
#' @examples
#' AnnotationLayer(hline = 0, text = "Peak", x = 2, y = 3)
AnnotationLayer <- new_class(
  "AnnotationLayer",
  package = "rtemis.draw",
  properties = list(
    hline = prop_float(NULL, nullable = TRUE, vector = TRUE),
    vline = prop_float(NULL, nullable = TRUE, vector = TRUE),
    x = new_property(class_any, default = numeric()),
    y = new_property(class_any, default = numeric()),
    text = prop_string(NULL, nullable = TRUE, vector = TRUE),
    bands = new_property(class_list, default = list()),
    color = prop_string("#888888"),
    line_type = prop_string("dashed", enum = c("solid", "dashed", "dotted")),
    opacity = prop_float(.15, min = 0, max = 1),
    x_axis = prop_integer(0L, min = 0L),
    y_axis = prop_integer(0L, min = 0L)
  ),
  validator = function(self) {
    if (
      length(self@x) != length(self@text) || length(self@y) != length(self@text)
    ) {
      return("Supply one x/y coordinate pair for each text label.")
    }
    for (values in list(self@x, self@y)) {
      if (
        !(is.numeric(values) || is.character(values)) ||
          anyNA(values) ||
          (is.numeric(values) && any(!is.finite(values)))
      ) {
        return(
          "Supply finite numeric coordinates or nonmissing category names."
        )
      }
    }
    for (band in self@bands) {
      if (
        !is.numeric(band) ||
          length(band) != 2L ||
          any(!is.finite(band)) ||
          band[[1]] >= band[[2]]
      ) {
        return("Supply finite increasing length-two band intervals.")
      }
    }
    NULL
  }
)

method(to_list, AnnotationLayer) <- function(x) {
  markers <- c(
    lapply(x@hline, function(v) list(yAxis = v)),
    lapply(x@vline, function(v) list(xAxis = v))
  )
  # The native label content is materialized, so SVG needs no JS formatter.
  points <- Map(
    function(a, b, label) {
      list(
        name = label,
        value = list(a, b),
        label = list(
          show = TRUE,
          formatter = "{b}",
          color = x@color,
          position = "top"
        )
      )
    },
    x@x,
    x@y,
    x@text
  )
  list(
    type = "scatter",
    name = "",
    xAxisIndex = x@x_axis,
    yAxisIndex = x@y_axis,
    data = points,
    symbolSize = 0,
    silent = TRUE,
    tooltip = list(show = FALSE),
    markLine = list(
      silent = TRUE,
      symbol = "none",
      label = list(show = FALSE),
      lineStyle = list(color = x@color, type = x@line_type),
      data = markers
    ),
    markArea = list(
      silent = TRUE,
      label = list(show = FALSE),
      itemStyle = list(color = x@color, opacity = x@opacity),
      data = lapply(x@bands, function(b) {
        list(list(xAxis = b[[1]]), list(xAxis = b[[2]]))
      })
    )
  )
}

#' Add References, Bands, and Labels to a Cartesian Drawing
#'
#' Adds independent native ECharts layers to an existing drawing. For panels,
#' annotate the children before composition. Annotation coordinates use the
#' existing axes and may extend their automatic ranges; explicit limits remain
#' authoritative. References and bands do not alter data summaries.
#' @param plot htmlwidget: An ECharts drawing.
#' @inheritParams AnnotationLayer
#' @param layer Optional AnnotationLayer: Complete geometry. Do not combine
#'   with annotation convenience arguments.
#' @return htmlwidget: The drawing with an additional vector annotation layer.
#' @export
#' @examples
#' draw_annotate(draw_scatter(1:5, c(2, 3, 1, 5, 4)), hline = 3)
draw_annotate <- function(
  plot,
  hline = NULL,
  vline = NULL,
  x = numeric(),
  y = numeric(),
  text = NULL,
  bands = list(),
  color = "#888888",
  line_type = "dashed",
  opacity = .15,
  x_axis = 0L,
  y_axis = 0L,
  layer = NULL
) {
  if (
    !inherits(plot, "rtemis-draw") ||
      is.null(plot[["x"]][["option"]][["xAxis"]])
  ) {
    abort(
      "Annotate an ECharts cartesian drawing before panel composition.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(layer)) {
    layer <- AnnotationLayer(
      hline = hline,
      vline = vline,
      x = x,
      y = y,
      text = text,
      bands = bands,
      color = color,
      line_type = line_type,
      opacity = opacity,
      x_axis = clean_int(x_axis),
      y_axis = clean_int(y_axis)
    )
  } else {
    if (
      !S7_inherits(layer, AnnotationLayer) ||
        length(setdiff(names(as.list(match.call()))[-1], c("plot", "layer")))
    ) {
      abort(
        "Supply an AnnotationLayer alone, or use the annotation arguments.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  option <- cartesian_layer_option(plot, layer@x_axis, layer@y_axis)
  option[["series"]] <- c(option[["series"]], list(to_list(layer)))
  plot[["x"]][["option"]] <- option
  plot
}


#' Validate the target of a Cartesian layer
#' @param x htmlwidget: Drawing before panel composition.
#' @param x_axis,y_axis Integer: Zero-based axis indices.
#' @return List: The existing ECharts option.
#' @keywords internal
#' @noRd
cartesian_layer_option <- new_generic("cartesian_layer_option", "x")
method(cartesian_layer_option, class_any) <- function(x, x_axis, y_axis) {
  option <- x[["x"]][["option"]]
  if (
    !inherits(x, "rtemis-draw") ||
      is.null(option[["xAxis"]]) ||
      is.null(option[["yAxis"]])
  ) {
    abort(
      "Add layers to a Cartesian drawing before composing panels.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  xa <- option[["xAxis"]]
  ya <- option[["yAxis"]]
  if (!is.null(names(xa))) {
    xa <- list(xa)
  }
  if (!is.null(names(ya))) {
    ya <- list(ya)
  }
  if (x_axis >= length(xa) || y_axis >= length(ya)) {
    abort(
      "Choose existing zero-based annotation axis indices.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    (xa[[x_axis + 1L]][["gridIndex"]] %||% 0L) !=
      (ya[[y_axis + 1L]][["gridIndex"]] %||% 0L)
  ) {
    abort(
      "Choose x and y axes belonging to the same grid.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  option
}
