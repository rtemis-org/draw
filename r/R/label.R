# label.R
# Label-related S7 classes: LabelOption, LabelLine
#
# LabelOption includes all TextStyle fields plus label-specific positioning.
# Rather than inheriting from TextStyle (which S7 doesn't support for
# multi-inheritance), we include a `text_style` property for text styling
# and flatten it in to_list().

# -- LabelOption -----------------------------------------------------------------

#' Label Option
#'
#' Text label configuration for chart elements. Combines label-specific
#' positioning (show, position, distance, rotate) with text styling.
#'
#' Corresponds to `SeriesLabelOption` / `LabelOption` in `src/util/types.ts`
#' (line 1283, 1348).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-bar.label}
#'
#' @param show Whether to show the label.
#' @param position Label position. One of: `"top"`, `"left"`, `"right"`,
#'   `"bottom"`, `"inside"`, `"insideLeft"`, `"insideRight"`, `"insideTop"`,
#'   `"insideBottom"`, `"insideTopLeft"`, `"insideBottomLeft"`,
#'   `"insideTopRight"`, `"insideBottomRight"`, `"outside"`.
#' @param distance Distance from the element.
#' @param rotate Rotation angle in degrees.
#' @param offset Offset `c(x, y)` in pixels.
#' @param formatter Label text formatter. String template (e.g. `"{b}: {c}"`)
#'   or an R function.
#' @param silent Whether the label is silent (no mouse events).
#' @param precision Number precision, or `"auto"`.
#' @param value_animation Whether to animate value changes.
#' @param min_margin Minimum margin between labels.
#' @param text_style A [TextStyle] object for text appearance.
#' @export
LabelOption <- S7::new_class(
  "LabelOption",
  properties = list(
    show = bool_or_null_property(),
    position = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        valid <- c(
          "top", "left", "right", "bottom", "inside",
          "insideLeft", "insideRight", "insideTop", "insideBottom",
          "insideTopLeft", "insideBottomLeft", "insideTopRight",
          "insideBottomRight", "outside"
        )
        if (is.character(value) && length(value) == 1L && value %in% valid) {
          return(NULL)
        }
        # Also accept numeric array [x, y] for absolute positioning
        if (is.numeric(value)) return(NULL)
        paste0("must be one of: ", paste(dQuote(valid), collapse = ", "),
               ", or a numeric position")
      }
    ),
    distance = numeric_or_null_property(),
    rotate = numeric_or_null_property(),
    offset = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value)) return(NULL)
        "must be a numeric vector or NULL"
      }
    ),
    formatter = S7::new_property(class = S7::class_any, default = NULL),
    silent = bool_or_null_property(),
    precision = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        if (is.character(value) && length(value) == 1L && value == "auto") {
          return(NULL)
        }
        "must be a number, 'auto', or NULL"
      }
    ),
    value_animation = bool_or_null_property(),
    min_margin = numeric_or_null_property(),
    # Text styling -- flattened into the same JSON object in to_list()
    text_style = class_or_null_property(TextStyle)
  )
)

S7::method(to_list, LabelOption) <- function(x, ...) {
  # Get label-specific fields
  out <- props_to_list(x)
  # Flatten text_style into the same level (echarts label is a flat object)
  if (!is.null(out$textStyle)) {
    text_fields <- out$textStyle
    out$textStyle <- NULL
    out <- c(out, text_fields)
  }
  out
}

# -- LabelLine -------------------------------------------------------------------

#' Label Line
#'
#' Connecting line between a label and its chart element (e.g. pie label lines).
#'
#' Corresponds to `LabelLineOption` in `src/util/types.ts` (line 1379).
#' ECharts docs: \url{https://echarts.apache.org/en/option.html#series-pie.labelLine}
#'
#' @param show Whether to show the label line.
#' @param show_above Whether the line renders above other elements.
#' @param length Length of the first segment.
#' @param length2 Length of the second segment.
#' @param smooth Smoothness (0-1), or TRUE/FALSE.
#' @param min_turn_angle Minimum angle for turning the line.
#' @param line_style A [LineStyle] object.
#' @export
LabelLine <- S7::new_class(
  "LabelLine",
  properties = list(
    show = bool_or_null_property(),
    show_above = bool_or_null_property(),
    length = numeric_or_null_property(),
    length2 = numeric_or_null_property(),
    smooth = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (is.logical(value) && length(value) == 1L) return(NULL)
        if (is.numeric(value) && length(value) == 1L) return(NULL)
        "must be TRUE/FALSE, a number, or NULL"
      }
    ),
    min_turn_angle = numeric_or_null_property(),
    line_style = class_or_null_property(LineStyle)
  )
)

S7::method(to_list, LabelLine) <- function(x, ...) {
  props_to_list(x)
}
