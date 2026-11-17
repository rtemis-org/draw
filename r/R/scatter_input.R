# scatter_input.R
# Pairwise filtering applies identically to coordinates, groups, sizes, colors,
# and labels.

#' Normalize scatter observations and optional point metadata
#' @inheritParams draw_scatter
#' @return List: Complete aligned coordinate and metadata vectors.
#' @keywords internal
#' @noRd
scatter_input <- new_generic("scatter_input", "x")
method(scatter_input, class_any) <- function(
  x,
  y,
  group,
  size,
  hover,
  color = NULL
) {
  if (is.list(x) || is.list(y)) {
    if (!is.null(size) || !is.null(hover) || !is.null(color)) {
      abort(
        "Supply flattened vectors with group labels when using size, color, or hover metadata.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    pairs <- true_pred_data(x, y, group)
    return(list(
      x = pairs[["true"]],
      y = pairs[["predicted"]],
      group = pairs[["sample"]]
    ))
  }
  if (
    !is.numeric(x) ||
      !is.numeric(y) ||
      is.complex(x) ||
      is.complex(y) ||
      !is.null(dim(x)) ||
      !is.null(dim(y)) ||
      length(x) != length(y) ||
      !length(x) ||
      any(is.infinite(x)) ||
      any(is.infinite(y))
  ) {
    abort(
      "Supply equally sized numeric x/y vectors, allowing NA but not infinity.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  group <- group_values(group, length(x))
  if (!is.null(size)) {
    if (
      !is.numeric(size) ||
        is.complex(size) ||
        !is.null(dim(size)) ||
        !length(size) %in% c(1L, length(x)) ||
        any(is.infinite(size)) ||
        any(size < 0, na.rm = TRUE)
    ) {
      abort(
        "Supply a nonnegative size or one size per observation, allowing NA but not infinity.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    size <- rep_len(size, length(x))
  }
  if (
    !is.null(color) &&
      (!is.numeric(color) ||
        is.complex(color) ||
        !is.null(dim(color)) ||
        length(color) != length(x) ||
        any(is.infinite(color)))
  ) {
    abort(
      "Supply one numeric color value per observation, allowing NA but not infinity.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (
    !is.null(hover) &&
      (!is.character(hover) || length(hover) != length(x) || anyNA(hover))
  ) {
    abort(
      "Supply one nonmissing character hover label per observation.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keep <- !is.na(x) & !is.na(y)
  if (!is.null(group)) {
    keep <- keep & !is.na(group)
  }
  if (!is.null(color)) {
    keep <- keep & !is.na(color)
  }
  if (!is.null(size)) {
    keep <- keep & !is.na(size)
  }
  if (!any(keep)) {
    abort(
      "Supply at least one complete scatter observation.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  list(
    x = x[keep],
    y = y[keep],
    group = if (!is.null(group)) group[keep],
    size = if (!is.null(size)) size[keep],
    hover = if (!is.null(hover)) hover[keep],
    color = if (!is.null(color)) color[keep]
  )
}
