# scatter_input.R
# Pairwise filtering applies identically to coordinates, groups, sizes and labels.

#' Normalize scatter observations and optional point metadata
#' @inheritParams draw_scatter
#' @return List: Complete aligned coordinate and metadata vectors.
#' @keywords internal
#' @noRd
scatter_input <- new_generic("scatter_input", "x")
method(scatter_input, class_any) <- function(x, y, group, size, hover) {
  if (is.list(x) || is.list(y)) {
    if (!is.null(size) || !is.null(hover)) {
      abort(
        "Supply flattened vectors with group labels when using size or hover metadata.",
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
        any(!is.finite(size)) ||
        any(size < 0)
    ) {
      abort(
        "Supply a nonnegative finite size or one size per observation.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    size <- rep_len(size, length(x))
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
    hover = if (!is.null(hover)) hover[keep]
  )
}

#' Materialize native point records
#' @inheritParams draw_scatter
#' @return List: Coordinate vectors or native records with size and label.
#' @keywords internal
#' @noRd
scatter_points <- new_generic("scatter_points", "x")
method(scatter_points, class_numeric) <- function(x, y, size, hover) {
  lapply(seq_along(x), function(i) {
    value <- c(x[[i]], y[[i]])
    if (is.null(size) && is.null(hover)) {
      return(value)
    }
    Filter(
      Negate(is.null),
      list(
        value = value,
        symbolSize = if (!is.null(size)) size[[i]],
        name = if (!is.null(hover)) hover[[i]]
      )
    )
  })
}
