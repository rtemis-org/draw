# boxplot_layers.R
# spec: draw/first-cran-release#distribution-layers

#' Compute a width-normalized Gaussian violin profile
#' @param x Numeric: Finite observed values, without missing entries.
#' @inheritParams draw_boxplot
#' @return List: Evaluation positions and relative half-widths, or a crossbar.
#' @keywords internal
#' @noRd
boxplot_density <- new_generic("boxplot_density", "x")
method(boxplot_density, class_numeric) <- function(
  x,
  bandwidth = NULL,
  adjust = 1,
  density_points = 128L
) {
  if (!length(x)) {
    return(list())
  }
  if (length(unique(x)) == 1L) {
    return(list(position = as.list(x[[1L]]), width = list(1)))
  }
  bw <- (bandwidth %||% stats::bw.nrd0(x)) * adjust
  if (!is.finite(bw) || bw <= 0) {
    abort(
      "Choose a finite positive density bandwidth and adjustment.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  estimate <- stats::density(
    x,
    bw = bw,
    kernel = "gaussian",
    n = density_points,
    from = min(x),
    to = max(x)
  )
  if (any(!is.finite(estimate[["y"]])) || max(estimate[["y"]]) <= 0) {
    abort(
      "Rescale the observations or choose a suitable density bandwidth.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  list(
    position = as.list(estimate[["x"]]),
    width = as.list(estimate[["y"]] / max(estimate[["y"]]))
  )
}

#' Validate comparison records for the boxplot builder
#'
#' The builder resolves each record's endpoints to boxes and places it; this
#' checks the record shape and writes the records as the builder takes them.
#' @param x Optional Data frame: Comparison records: `from`, `to`, and `label`,
#'   plus `from_group` and `to_group` when the chart has several series, and an
#'   optional numeric `position`.
#' @param multi Logical: Whether the chart has several series.
#' @return List of records, or `NULL`.
#' @keywords internal
#' @noRd
boxplot_comparisons <- new_generic("boxplot_comparisons", "x")
method(boxplot_comparisons, class_any) <- function(x, multi) {
  if (is.null(x)) {
    return(NULL)
  }
  required <- c("from", "to", "label", if (multi) c("from_group", "to_group"))
  if (
    !is.data.frame(x) || !all(required %in% names(x)) || anyDuplicated(names(x))
  ) {
    abort(
      "Supply a comparison data frame with unique columns: ",
      paste(required, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  for (field in required) {
    if (
      !(is.character(x[[field]]) || is.factor(x[[field]])) ||
        anyNA(x[[field]]) ||
        any(!nzchar(as.character(x[[field]])))
    ) {
      abort(
        "Supply nonmissing, nonempty comparison names and labels.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (!multi && any(c("from_group", "to_group") %in% names(x))) {
    abort(
      "Omit comparison group columns for a single series; use category names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  position <- x[["position"]]
  if (
    !is.null(position) &&
      (!is.numeric(position) ||
        is.complex(position) ||
        !is.null(dim(position)) ||
        any(!is.finite(position)))
  ) {
    abort(
      "Supply finite numeric comparison positions or omit the position column.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  lapply(seq_len(nrow(x)), function(i) {
    record <- list(
      from = jsonlite::unbox(as.character(x[["from"]][[i]])),
      to = jsonlite::unbox(as.character(x[["to"]][[i]])),
      label = jsonlite::unbox(as.character(x[["label"]][[i]]))
    )
    if (multi) {
      record[["from_group"]] <- jsonlite::unbox(as.character(x[["from_group"]][[
        i
      ]]))
      record[["to_group"]] <- jsonlite::unbox(as.character(x[["to_group"]][[
        i
      ]]))
    }
    if (!is.null(position)) {
      record[["position"]] <- jsonlite::unbox(position[[i]])
    }
    record
  })
}
