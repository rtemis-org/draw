# draw_significance.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Prepare significance data without changing the testing family
#'
#' Adjustment consumes the raw p-value column once, including its missing slots
#' in the declared family size. Display filtering happens only afterward.
#' spec: draw/first-cran-release#massglm-significance-views
#'
#' @param config [SignificanceConfig]: Statistical and display settings.
#' @param data Data frame or named list: Bound estimate, p-value, and label columns.
#' @return List containing materialized `data`, `zero_cap`, and `threshold`.
#' @keywords internal
#' @noRd
significance_data <- new_generic("significance_data", "config")

method(significance_data, SignificanceConfig) <- function(config, data) {
  estimate <- config_column(data, config@estimate, "estimate")
  p <- config_column(data, config@p_value, "p_value")
  values <- list(estimate = estimate, p_value = p)
  for (name in names(values)) {
    value <- values[[name]]
    if (is.logical(value) && all(is.na(value))) {
      value <- as.numeric(value)
    }
    if (
      !is.numeric(value) ||
        is.complex(value) ||
        !is.null(dim(value)) ||
        !length(value) ||
        any(is.infinite(value))
    ) {
      abort(
        "Supply a nonempty numeric `",
        name,
        "` vector containing finite values or NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    values[[name]] <- value
  }
  estimate <- values[["estimate"]]
  p <- values[["p_value"]]
  if (length(estimate) != length(p) || any(p < 0 | p > 1, na.rm = TRUE)) {
    abort(
      "Supply equally sized estimates and p-values, with p-values in [0, 1] or NA.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  labels <- config_column(data, config@label, "label")
  if (is.null(labels)) {
    labels <- paste("Outcome", seq_along(p))
  }
  if (is.factor(labels)) {
    labels <- as.character(labels)
  }
  if (
    !is.character(labels) ||
      !is.null(dim(labels)) ||
      length(labels) != length(p) ||
      anyNA(labels) ||
      any(!nzchar(labels))
  ) {
    abort(
      "Supply one nonempty character label per outcome, or set `label = NULL`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  display_group <- group_values(
    config_column(data, config@group, "group"),
    length(p)
  )
  if (
    !is.null(display_group) &&
      any(!is.na(display_group) & !nzchar(as.character(display_group)))
  ) {
    abort(
      "Supply nonempty display-group labels or NA.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  n_tests <- config@n_tests %||% length(p)
  if (n_tests < length(p)) {
    abort(
      "`n_tests` must include every supplied row, including missing p-values.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Force the full family size explicitly: p.adjust's default n is evaluated
  # after its internal NA filtering. Missing outcomes remain unknown here.
  adjusted <- stats::p.adjust(p, method = config@p_adjust_method, n = n_tests)
  transformed <- switch(
    config@p_transform,
    neg_log10 = -log10(adjusted),
    identity = adjusted,
    one_minus = 1 - adjusted
  )
  threshold <- switch(
    config@p_transform,
    neg_log10 = -log10(config@p_thresh),
    identity = config@p_thresh,
    one_minus = 1 - config@p_thresh
  )
  keep <- !is.na(estimate) & !is.na(p)
  if (!is.null(display_group)) {
    keep <- keep & !is.na(display_group)
  }
  if (!any(keep)) {
    abort(
      "Supply at least one complete estimate/p-value pair with a nonmissing display group when grouping.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  capped <- !is.na(adjusted) & adjusted == 0 & config@p_transform == "neg_log10"
  cap <- NULL
  if (any(capped & keep)) {
    highest <- max(c(0, threshold, transformed[keep & is.finite(transformed)]))
    cap <- config@zero_cap %||% (ceiling(highest) + 1)
    if (cap <= highest) {
      abort(
        "Set `zero_cap` above all finite displayed heights and the significance threshold.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    transformed[capped] <- cap
  }
  list(
    data = data.frame(
      row = seq_along(p),
      label = labels,
      estimate = estimate,
      p_value = p,
      p_adjusted = adjusted,
      value = transformed,
      display_group = display_group %||% rep(NA_character_, length(p)),
      keep = keep,
      capped = capped,
      stringsAsFactors = FALSE
    ),
    zero_cap = cap,
    threshold = threshold,
    n_tests = n_tests
  )
}


#' Build the request of a significance chart
#'
#' Validates and adjusts the family (`significance_data()`), reports omitted
#' outcomes, and hands each hypothesis's label, estimate, and raw and adjusted
#' p-values to `@rtemis/draw`'s significance builder, which transforms, caps,
#' groups, labels, and draws them.
#' spec: draw/shared-builders#significance
#'
#' @inheritParams significance_data
#' @param theme Optional [Theme], list, or `NA`: Supplies the sign colors the
#'   config leaves unset.
#' @return [BuildRequest] of type `significance`.
#' @keywords internal
#' @noRd
significance_request <- new_generic("significance_request", "config")

method(significance_request, SignificanceConfig) <- function(
  config,
  data,
  theme = NULL
) {
  prepared <- significance_data(config, data)
  d <- prepared[["data"]]
  # A user-supplied range must not silently discard the capped marks.
  capped <- d[["keep"]] & d[["capped"]]
  if (
    any(capped) &&
      !is.null(config@ylim) &&
      (prepared[["zero_cap"]] < config@ylim[[1L]] ||
        prepared[["zero_cap"]] > config@ylim[[2L]])
  ) {
    abort(
      "Expand `ylim` to include `zero_cap`, or change the display transform.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (any(!d[["keep"]])) {
    msg(
      sum(!d[["keep"]]),
      "outcome(s) omitted;",
      prepared[["n_tests"]],
      "tests in family"
    )
  }
  na_null <- function(x) lapply(x, function(v) if (is.na(v)) NULL else v)
  document <- list(
    label = I(d[["label"]]),
    estimate = na_null(d[["estimate"]]),
    p_value = na_null(d[["p_value"]]),
    p_adjusted = na_null(d[["p_adjusted"]])
  )
  if (!is.null(config@group)) {
    document[["group"]] <- na_null(as.character(d[["display_group"]]))
    document[["groups"]] <- I(group_levels(d[["display_group"]][d[["keep"]]]))
  }
  sign <- theme_sign_colors(theme)
  BuildRequest(
    type = "significance",
    data = as.character(jsonlite::toJSON(
      document,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list(
      signColors = list(
        negative = sign[["negative"]],
        neutral = sign[["neutral"]],
        positive = sign[["positive"]]
      )
    )
  )
}


#' Align vectors for a significance chart
#' @param x Numeric vector: Estimates, with NA for unavailable values.
#' @param pvals Numeric vector: Raw p-values in `[0, 1]`, or NA.
#' @param xnames Optional Character vector: Outcome labels.
#' @param group Optional vector: One display-group label per outcome.
#' @return Data frame with estimate, p_value, label, and optional group columns.
#' @keywords internal
#' @noRd
significance_input <- new_generic("significance_input", "x")
method(significance_input, class_any) <- function(
  x,
  pvals,
  xnames = NULL,
  group = NULL
) {
  if (length(x) != length(pvals) || !length(x)) {
    abort(
      "Supply equally sized, nonempty estimate and p-value vectors.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  xnames <- xnames %||%
    names(x) %||%
    names(pvals) %||%
    paste("Outcome", seq_along(x))
  if (length(xnames) != length(x)) {
    abort(
      "Supply one outcome label per estimate.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Validate before data.frame() can recycle or expand a matrix/list input.
  data <- list(estimate = x, p_value = pvals, label = xnames)
  if (
    any(vapply(
      data,
      function(value) !is.atomic(value) || !is.null(dim(value)),
      logical(1)
    ))
  ) {
    abort(
      "Supply vectors, not matrices or lists, for estimates, p-values, and labels.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  data[["group"]] <- group_values(group, length(x))
  as.data.frame(data, stringsAsFactors = FALSE)
}


#' Draw a Volcano Plot
#'
#' Plot signed estimates and raw p-values through [SignificanceConfig].
#' The Manhattan view places outcomes in input order; it does not use genomic
#' coordinates. Missing pairs have no marks but remain in the testing family.
#' @inheritSection SignificanceConfig Statistical semantics
#' @param x Numeric vector: Signed estimates; NA values have no plotted mark.
#' @param pvals Numeric vector: Raw p-values in `[0, 1]`, or NA.
#' @param xnames Optional Character vector: One outcome label per estimate.
#' @param group Optional vector: One display-group label per outcome, ordered by
#'   factor level, or by first appearance for other vectors; unset uses
#'   significance and direction. Missing groups omit marks but retain the testing family.
#' @inheritParams SignificanceConfig
#' @param ... Additional named settings for [setup_SignificanceConfig()], such as
#'   `reference`, `zero_cap`, `n_tests`, colors, axis limits, or margins.
#' @param zoom Logical: Enable Shift+wheel zoom and drag panning; see
#'   [setup_SignificanceConfig()].
#' @param theme Optional [Theme]: Widget/export theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_volcano(c(-2, 0.1, 3), c(0.001, 0.6, 0.002),
#'   xnames = c("A", "B", "C"), p_adjust_method = "holm")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_volcano <- function(
  x,
  pvals,
  xnames = NULL,
  p_adjust_method = "holm",
  p_transform = "neg_log10",
  p_thresh = 0.05,
  x_thresh = 0,
  annotate_n = 7L,
  ...,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  group = NULL,
  zoom = FALSE
) {
  data <- significance_input(x, pvals, xnames, group)
  config <- setup_SignificanceConfig(
    view = "volcano",
    label = "label",
    group = if (!is.null(group)) "group" else NULL,
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    p_thresh = p_thresh,
    x_thresh = x_thresh,
    annotate_n = annotate_n,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    zoom = zoom,
    ...
  )
  draw(
    config,
    data = data,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}


#' Draw a Categorical Manhattan Plot
#'
#' Plot signed estimates and raw p-values through [SignificanceConfig].
#' The Manhattan view places outcomes in input order; it does not use genomic
#' coordinates. Missing pairs have no marks but remain in the testing family.
#' @inheritSection SignificanceConfig Statistical semantics
#' @param x Numeric vector: Signed estimates; NA values have no plotted mark.
#' @param pvals Numeric vector: Raw p-values in `[0, 1]`, or NA.
#' @param xnames Optional Character vector: One outcome label per estimate.
#' @param group Optional vector: One display-group label per outcome, ordered by
#'   factor level, or by first appearance for other vectors; unset uses
#'   significance and direction. Missing groups omit marks but retain the testing family.
#' @inheritParams SignificanceConfig
#' @param ... Additional named settings for [setup_SignificanceConfig()], such as
#'   `reference`, `zero_cap`, `n_tests`, colors, axis limits, or margins.
#' @param zoom Logical: Enable Shift+wheel zoom and drag panning; see
#'   [setup_SignificanceConfig()].
#' @param theme Optional [Theme]: Widget/export theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_manhattan(c(-2, 0.1, 3), c(0.001, 0.6, 0.002),
#'   xnames = c("A", "B", "C"), p_adjust_method = "holm")
#' @inheritParams draw_line legend_position legend_placement legend_title
draw_manhattan <- function(
  x,
  pvals,
  xnames = NULL,
  p_adjust_method = "holm",
  p_transform = "neg_log10",
  p_thresh = 0.05,
  x_thresh = 0,
  annotate_n = 7L,
  ...,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  group = NULL,
  zoom = FALSE
) {
  data <- significance_input(x, pvals, xnames, group)
  config <- setup_SignificanceConfig(
    view = "manhattan",
    label = "label",
    group = if (!is.null(group)) "group" else NULL,
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    p_thresh = p_thresh,
    x_thresh = x_thresh,
    annotate_n = annotate_n,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
    zoom = zoom,
    ...
  )
  draw(
    config,
    data = data,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}

#' Draw One-Minus-P-Value Bars
#'
#' A convenience view of [SignificanceConfig]. Adjustment uses the full supplied
#' testing family, including missing slots; missing p-values are not plotted.
#' The reference is at one minus the significance threshold.
#' @param x Numeric: P-values in `[0, 1]` or NA.
#' @param xnames Optional Character: Feature labels, otherwise names of x.
#' @param p_adjust_method Character: A method from [stats::p.adjust.methods].
#' @param pval_hline Numeric (0, 1): Significance threshold.
#' @param ... Additional arguments passed to [draw_manhattan()].
#' @return htmlwidget: Bars showing one minus adjusted p-values.
#' @export
#' @examples
#' draw_pvals(c(A = .01, B = .2, C = .04), p_adjust_method = "holm")
draw_pvals <- function(
  x,
  xnames = names(x),
  p_adjust_method = "none",
  pval_hline = .05,
  ...
) {
  draw_manhattan(
    rep(0, length(x)),
    x,
    xnames = xnames,
    p_adjust_method = p_adjust_method,
    p_transform = "one_minus",
    p_thresh = pval_hline,
    annotate_n = 0L,
    ...
  )
}
