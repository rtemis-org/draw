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

#' Resolve comparison records to nonempty boxplot cells
#' @param x Optional Data frame: Comparison records.
#' @param cells List: Observations organized by series and category.
#' @param categories Character: Category names in plotting order.
#' @param groups Optional Character: Series group names.
#' @param limits Numeric: Visible value range.
#' @return List: Bracket data and the extended value range.
#' @keywords internal
#' @noRd
boxplot_comparisons <- new_generic("boxplot_comparisons", "x")
method(boxplot_comparisons, class_any) <- function(
  x,
  cells,
  categories,
  groups,
  limits
) {
  if (is.null(x)) {
    return(list(data = list(), limits = limits))
  }
  required <- c(
    "from",
    "to",
    "label",
    if (!is.null(groups)) c("from_group", "to_group")
  )
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
  if (anyDuplicated(categories)) {
    abort(
      "Use unique category labels when supplying comparisons.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.null(groups) && any(c("from_group", "to_group") %in% names(x))) {
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
  span <- diff(limits)
  if (span == 0) {
    span <- max(abs(limits), 1)
  }
  position <- position %||% (limits[[2L]] + seq_len(nrow(x)) * span * .12)
  records <- vector("list", nrow(x))
  for (i in seq_len(nrow(x))) {
    category <- match(
      c(as.character(x[["from"]][[i]]), as.character(x[["to"]][[i]])),
      categories
    )
    series <- if (is.null(groups)) {
      c(1L, 1L)
    } else {
      match(
        c(
          as.character(x[["from_group"]][[i]]),
          as.character(x[["to_group"]][[i]])
        ),
        groups
      )
    }
    if (anyNA(c(category, series))) {
      abort(
        "Use existing category and group names in comparison endpoints.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (category[[1L]] == category[[2L]] && series[[1L]] == series[[2L]]) {
      abort(
        "Choose two distinct comparison endpoints.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    for (end in seq_len(2L)) {
      if (
        !any(is.finite(cells[[series[[end]]]][[category[[end]]]][["values"]]))
      ) {
        abort(
          "Choose comparison endpoints with available observations.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
    records[[i]] <- list(
      category[[1L]] - 1L,
      category[[2L]] - 1L,
      position[[i]],
      series[[1L]] - 1L,
      series[[2L]] - 1L
    )
    records[[i]][[6L]] <- as.character(x[["label"]][[i]])
  }
  # Leave room above the text, including explicitly positioned brackets.
  list(
    data = records,
    limits = if (length(position)) {
      range(c(limits, position - span * .04, position + span * .12))
    } else {
      limits
    }
  )
}

#' Build native vector distribution layers from validated observations
#' @param x List: Cells organized by series and category.
#' @param categories Character: Category names in plotting order.
#' @param groups Optional Character: Series group names.
#' @param limits Numeric: Visible value range.
#' @inheritParams draw_boxplot
#' @return List: Custom series and the complete visible value range.
#' @keywords internal
#' @noRd
boxplot_layers <- new_generic("boxplot_layers", "x")
method(boxplot_layers, class_list) <- function(
  x,
  categories,
  groups,
  horizontal,
  geometry,
  bandwidth,
  adjust,
  density_points,
  paired,
  pair_alpha,
  pair_width,
  fill_alpha,
  comparisons,
  limits
) {
  series <- list()
  anchors <- as.list(seq_along(x) - 1L)
  for (s in seq_along(x)) {
    cells <- x[[s]]
    payload <- list(
      horizontal = horizontal,
      boxSeries = anchors,
      boxIndex = s - 1L
    )
    name <- if (is.null(groups)) NULL else groups[[s]]
    if (geometry != "box") {
      profiles <- lapply(cells, function(cell) {
        boxplot_density(
          cell[["values"]][is.finite(cell[["values"]])],
          bandwidth,
          adjust,
          density_points
        )
      })
      payload[["profiles"]] <- profiles
      payload[["fillAlpha"]] <- fill_alpha
      data <- lapply(seq_along(cells), function(j) {
        cell <- cells[[j]]
        values <- cell[["values"]][is.finite(cell[["values"]])]
        bounds <- if (length(values)) range(values) else c(NA_real_, NA_real_)
        list(
          value = as.list(c(j - 1L, bounds)),
          itemStyle = list(color = cell[["color"]])
        )
      })
      series[[length(series) + 1L]] <- list(
        type = "custom",
        name = name,
        renderItem = "rtemis.violin.v1",
        clip = TRUE,
        z = 1,
        dimensions = list("Category", "Minimum", "Maximum"),
        encode = if (horizontal) {
          list(x = list(1L, 2L), y = 0L)
        } else {
          list(x = 0L, y = list(1L, 2L))
        },
        itemPayload = payload,
        data = data
      )
    }
    if (paired) {
      pairs <- list()
      for (j in seq_len(max(0L, length(cells) - 1L))) {
        from <- cells[[j]]
        to <- cells[[j + 1L]]
        matched <- match(from[["ids"]], to[["ids"]])
        available <- which(
          !is.na(matched) &
            is.finite(from[["values"]]) &
            is.finite(to[["values"]][matched])
        )
        for (i in available) {
          k <- matched[[i]]
          pairs[[length(pairs) + 1L]] <- list(
            j - 1L,
            from[["values"]][[i]],
            from[["offsets"]][[i]],
            j,
            to[["values"]][[k]],
            to[["offsets"]][[k]],
            from[["ids"]][[i]]
          )
        }
      }
      series[[length(series) + 1L]] <- list(
        type = "custom",
        name = name,
        renderItem = "rtemis.boxplot_pairs.v1",
        silent = TRUE,
        clip = TRUE,
        z = 2,
        dimensions = list(
          "From",
          "From value",
          "From offset",
          "To",
          "To value",
          "To offset",
          "Observation"
        ),
        encode = if (horizontal) {
          list(x = list(1L, 4L), y = list(0L, 3L))
        } else {
          list(x = list(0L, 3L), y = list(1L, 4L))
        },
        itemPayload = c(
          payload[c("horizontal", "boxSeries", "boxIndex")],
          list(alpha = pair_alpha, width = pair_width)
        ),
        itemStyle = list(
          color = if (is.null(groups)) "#888888" else cells[[1L]][["color"]]
        ),
        data = pairs
      )
    }
  }
  brackets <- boxplot_comparisons(comparisons, x, categories, groups, limits)
  if (length(brackets[["data"]])) {
    series[[length(series) + 1L]] <- list(
      type = "custom",
      renderItem = "rtemis.boxplot_brackets.v1",
      silent = TRUE,
      clip = TRUE,
      z = 4,
      dimensions = list(
        "From",
        "To",
        "Position",
        "From series",
        "To series",
        "Label"
      ),
      encode = if (horizontal) {
        list(x = 2L, y = list(0L, 1L))
      } else {
        list(x = list(0L, 1L), y = 2L)
      },
      itemPayload = list(horizontal = horizontal, boxSeries = anchors),
      data = brackets[["data"]]
    )
  }
  list(series = series, limits = brackets[["limits"]])
}
