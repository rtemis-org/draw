# heatmap_input.R
# Matrix annotations are data, and share every subset and permutation of values.

#' Validate and align a supplied hierarchical tree
#' @param tree Optional hclust, dendrogram or List: Merge, height, order and labels.
#' @param labels Character: Original matrix identities on the clustered axis.
#' @return Optional hclust: Tree with leaf indices aligned to matrix identities.
#' @keywords internal
#' @noRd
heatmap_tree <- new_generic("heatmap_tree", "tree")
method(heatmap_tree, class_any) <- function(tree, labels) {
  if (is.null(tree)) {
    return(NULL)
  }
  if (inherits(tree, "dendrogram")) {
    tree <- stats::as.hclust(tree)
  }
  n <- length(labels)
  bad <- function() {
    abort(
      "Supply a valid hierarchical tree with one leaf per matrix identity.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.list(tree) || n < 2L) {
    bad()
  }
  merge <- tree[["merge"]]
  heights <- tree[["height"]]
  order <- tree[["order"]]
  if (
    !is.matrix(merge) ||
      !is.numeric(merge) ||
      !identical(dim(merge), c(n - 1L, 2L)) ||
      any(!is.finite(merge)) ||
      any(merge != trunc(merge)) ||
      !is.numeric(heights) ||
      length(heights) != n - 1L ||
      any(!is.finite(heights)) ||
      any(heights < 0) ||
      !is.numeric(order) ||
      !identical(sort(as.integer(order)), seq_len(n)) ||
      anyNA(order) ||
      any(order != trunc(order))
  ) {
    bad()
  }
  # Every leaf and every non-root node must have exactly one parent. Positive
  # references may only name earlier merges; this also rules out cycles.
  if (
    any(merge == 0) ||
      any(merge < -n) ||
      any(merge > row(merge) - 1L) ||
      !identical(sort(as.integer(merge[merge < 0])), seq.int(-n, -1L)) ||
      !identical(sort(as.integer(merge[merge > 0])), seq_len(n - 2L))
  ) {
    bad()
  }
  traversal <- vector("list", n - 1L)
  for (i in seq_len(n - 1L)) {
    traversal[[i]] <- unlist(
      lapply(merge[i, ], function(j) {
        if (j < 0) -j else traversal[[j]]
      }),
      use.names = FALSE
    )
  }
  if (!identical(as.integer(order), as.integer(traversal[[n - 1L]]))) {
    bad()
  }
  names <- tree[["labels"]]
  if (!is.null(names)) {
    if (
      length(names) != n ||
        anyNA(names) ||
        anyDuplicated(names) ||
        anyDuplicated(labels) ||
        !setequal(names, labels)
    ) {
      abort(
        "Match tree labels to the original matrix dimnames, or use an unlabeled positional tree.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    indices <- match(names, labels)
    merge[merge < 0] <- -indices[-merge[merge < 0]]
    order <- indices[order]
  }
  structure(
    list(
      merge = merge,
      height = heights,
      order = as.integer(order),
      labels = labels,
      method = tree[["method"]] %||% "supplied"
    ),
    class = "hclust"
  )
}

#' Normalize a heatmap matrix and aligned annotations
#' @inheritParams draw_heatmap
#' @return List: Matrix, trees, notes and color-track matrices in input order.
#' @keywords internal
#' @noRd
heatmap_input <- new_generic("heatmap_input", "x")
method(heatmap_input, class_any) <- function(
  x,
  row_tree = NULL,
  col_tree = NULL,
  cell_notes = NULL,
  row_colors = NULL,
  col_colors = NULL
) {
  extras <- list(
    row_tree = row_tree,
    col_tree = col_tree,
    cell_notes = cell_notes,
    row_colors = row_colors,
    col_colors = col_colors
  )
  if (is.list(x) && !is.data.frame(x)) {
    allowed <- c("values", names(extras))
    if (
      is.null(names(x)) ||
        anyDuplicated(names(x)) ||
        any(!names(x) %in% allowed) ||
        is.null(x[["values"]])
    ) {
      abort(
        "Supply a matrix or a heatmap record containing values and named annotations.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    for (name in names(extras)) {
      if (!is.null(x[[name]]) && !is.null(extras[[name]])) {
        abort(
          "Supply each heatmap annotation once, in the record or as an argument.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      extras[[name]] <- extras[[name]] %||% x[[name]]
    }
    x <- x[["values"]]
  }
  x <- as.matrix(x)
  if (
    !is.numeric(x) ||
      is.complex(x) ||
      any(dim(x) == 0L) ||
      any(is.infinite(x)) ||
      !any(is.finite(x))
  ) {
    abort(
      "Supply a nonempty numeric matrix with finite values or NA and at least one finite cell.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  identities <- list(
    rownames(x) %||% as.character(seq_len(nrow(x))),
    colnames(x) %||% as.character(seq_len(ncol(x)))
  )
  # Named annotations align by identity; unnamed annotations are positional.
  align <- function(value, expected, axis) {
    labels <- dimnames(value)[[axis]]
    if (is.null(labels)) {
      return(seq_along(expected))
    }
    if (
      anyNA(labels) ||
        anyDuplicated(labels) ||
        anyDuplicated(expected) ||
        !setequal(labels, expected)
    ) {
      abort(
        "Match annotation dimnames to the original matrix identities.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    match(expected, labels)
  }
  notes <- extras[["cell_notes"]]
  if (!is.null(notes)) {
    notes <- as.matrix(notes)
    if (!is.character(notes) || !identical(dim(notes), dim(x))) {
      abort(
        "Supply a character cell_notes matrix with the same dimensions as values.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    notes <- notes[
      align(notes, identities[[1L]], 1L),
      align(notes, identities[[2L]], 2L),
      drop = FALSE
    ]
    notes[is.na(notes)] <- ""
  }
  colors <- lapply(seq_len(2L), function(axis) {
    value <- extras[[c("row_colors", "col_colors")[[axis]]]]
    if (is.null(value)) {
      return(NULL)
    }
    if (is.null(dim(value))) {
      value <- matrix(value, ncol = 1L)
    }
    value <- as.matrix(value)
    if (
      !is.character(value) ||
        nrow(value) != length(identities[[axis]]) ||
        ncol(value) < 1L ||
        anyNA(value)
    ) {
      abort(
        "Supply character color tracks with one row per matrix identity and one column per track.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    value <- value[align(value, identities[[axis]], 1L), , drop = FALSE]
    rgba <- tryCatch(
      grDevices::col2rgb(value, alpha = TRUE),
      error = function(e) NULL
    )
    if (is.null(rgba)) {
      abort(
        "Supply valid R colors for every heatmap color-track cell.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    value[] <- grDevices::rgb(
      rgba[1L, ],
      rgba[2L, ],
      rgba[3L, ],
      rgba[4L, ],
      maxColorValue = 255
    )
    value
  })
  list(
    values = x,
    row_tree = heatmap_tree(extras[["row_tree"]], identities[[1L]]),
    col_tree = heatmap_tree(extras[["col_tree"]], identities[[2L]]),
    cell_notes = notes,
    row_colors = colors[[1L]],
    col_colors = colors[[2L]]
  )
}

#' Materialize colors for branches below a tree cut
#' @param tree Optional hclust: Computed or supplied tree.
#' @param k Optional Integer: Number of clusters.
#' @param color Character: Color for branches connecting different clusters.
#' @return Optional List: One color per merge.
#' @keywords internal
#' @noRd
heatmap_branch_colors <- new_generic("heatmap_branch_colors", "tree")
method(heatmap_branch_colors, class_any) <- function(tree, k, color) {
  if (is.null(k)) {
    return(NULL)
  }
  if (
    is.null(tree) ||
      length(k) != 1L ||
      !is.numeric(k) ||
      is.na(k) ||
      k < 1 ||
      k > length(tree[["order"]]) ||
      k != trunc(k)
  ) {
    abort(
      "Supply a tree cut between one and the number of clustered leaves, with clustering or a supplied tree.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  clusters <- stats::cutree(tree, k = k)
  # Assign colors by displayed cluster order, independent of input permutation.
  clusters <- match(clusters, unique(clusters[tree[["order"]]]))
  palette <- grDevices::hcl.colors(k, palette = "Dark 3")
  groups <- integer(nrow(tree[["merge"]]))
  colors <- rep(color, length(groups))
  for (i in seq_along(groups)) {
    children <- vapply(
      tree[["merge"]][i, ],
      function(j) {
        if (j < 0L) clusters[[-j]] else groups[[j]]
      },
      integer(1L)
    )
    if (children[[1L]] != 0L && children[[1L]] == children[[2L]]) {
      groups[[i]] <- children[[1L]]
      colors[[i]] <- palette[[groups[[i]]]]
    }
  }
  as.list(colors)
}
