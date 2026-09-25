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
  if (!any(keep)) {
    abort(
      "No outcomes have both an estimate and a p-value; supply at least one complete pair.",
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
  group <- rep(2L, length(p))
  significant <- keep & adjusted < config@p_thresh
  group[significant & estimate < config@x_thresh] <- 1L
  group[significant & estimate > config@x_thresh] <- 3L
  annotate <- rep(FALSE, length(p))
  for (side in c(1L, 3L)) {
    at <- which(keep & group == side)
    selected <- head(at[order(p[at], at)], config@annotate_n)
    annotate[selected] <- TRUE
  }
  list(
    data = data.frame(
      row = seq_along(p),
      label = labels,
      estimate = estimate,
      p_value = p,
      p_adjusted = adjusted,
      value = transformed,
      group = group,
      keep = keep,
      capped = capped,
      annotate = annotate,
      stringsAsFactors = FALSE
    ),
    zero_cap = cap,
    threshold = threshold,
    n_tests = n_tests
  )
}


#' Compile materialized significance marks to ECharts
#'
#' Uses native scatter/bar data, labels, and reference lines. No JavaScript
#' callback is needed for visible marks or tooltip values.
#'
#' @inheritParams significance_data
#' @return An [EChartsOption] with portable series data and configuration.
#' @keywords internal
#' @noRd
significance_option <- new_generic("significance_option", "config")

method(significance_option, SignificanceConfig) <- function(config, data) {
  prepared <- significance_data(config, data)
  d <- prepared[["data"]]
  keep <- d[["keep"]]
  volcano <- config@view == "volcano"
  colors <- c(
    config@negative_color,
    config@neutral_color,
    config@positive_color
  )
  groups <- c("Significant low", "Other", "Significant high")
  if (config@x_thresh == 0) {
    groups <- c("Significant negative", "Other", "Significant positive")
  }
  present <- sort(unique(d[["group"]][keep]))
  series <- lapply(present, function(group) {
    at <- which(keep & d[["group"]] == group)
    marks <- lapply(at, function(i) {
      item <- list(
        name = d[["label"]][[i]],
        value = list(
          if (volcano) d[["estimate"]][[i]] else i - 1L,
          d[["value"]][[i]],
          d[["estimate"]][[i]],
          d[["p_value"]][[i]],
          d[["p_adjusted"]][[i]]
        )
      )
      if (volcano) {
        item[["symbol"]] <- if (d[["capped"]][[i]]) "triangle" else "circle"
        if (d[["annotate"]][[i]]) {
          item[["label"]] <- to_list(LabelOption(
            show = TRUE,
            position = "top",
            formatter = "{b}",
            text_style = TextStyle(font_size = 11)
          ))
        }
      }
      item
    })
    s <- if (volcano) {
      to_list(ScatterSeries(
        name = groups[[group]],
        data = marks,
        symbol_size = config@point_size,
        item_style = ItemStyle(color = colors[[group]], opacity = config@alpha)
      ))
    } else {
      to_list(BarSeries(
        name = groups[[group]],
        data = marks,
        item_style = ItemStyle(color = colors[[group]], opacity = config@alpha)
      ))
    }
    # Native dimensions/encode preserve raw values for hover without a callback.
    s[["dimensions"]] <- as.list(c(
      "Position",
      "Display value",
      "Estimate",
      "Raw p-value",
      "Adjusted p-value"
    ))
    s[["encode"]] <- list(x = 0L, y = 1L, tooltip = as.list(2:4))
    if (volcano) {
      s[["labelLayout"]] <- list(moveOverlap = "shiftY", hideOverlap = FALSE)
      s[["labelLine"]] <- list(
        show = TRUE,
        lineStyle = list(color = colors[[group]])
      )
    } else {
      # Disjoint significance series share one category position. Each outcome
      # has exactly one bar; disabling grouping prevents horizontal offsets.
      s[["barGap"]] <- "-100%"
    }
    s
  })
  if (config@reference) {
    refs <- list(list(
      yAxis = prepared[["threshold"]],
      label = list(
        formatter = paste0("p < ", format(config@p_thresh, trim = TRUE)),
        position = "insideEndTop"
      )
    ))
    if (volcano) {
      refs <- c(
        refs,
        list(list(xAxis = config@x_thresh, label = list(show = FALSE)))
      )
    }
    # References belong to an unnamed series so legend filtering cannot hide
    # the statistical thresholds together with one significance group.
    reference_series <- to_list(ScatterSeries(data = list(), silent = TRUE))
    reference_series[["markLine"]] <- list(
      silent = TRUE,
      animation = FALSE,
      symbol = "none",
      lineStyle = list(color = "#888888", type = "dashed", width = 1),
      data = refs
    )
    series <- c(series, list(reference_series))
  }
  # A separate vector triangle identifies capped bars without changing their
  # significance color or adding a spurious legend entry.
  capped_at <- which(keep & d[["capped"]])
  if (!volcano && length(capped_at)) {
    series <- c(
      series,
      lapply(sort(unique(d[["group"]][capped_at])), function(group) {
        at <- capped_at[d[["group"]][capped_at] == group]
        to_list(ScatterSeries(
          # Sharing the group's name makes its cap markers follow the same
          # legend toggle as its bars.
          name = groups[[group]],
          data = lapply(at, function(i) {
            list(value = list(i - 1L, d[["value"]][[i]]))
          }),
          item_style = ItemStyle(
            color = colors[[group]],
            opacity = config@alpha
          ),
          symbol = "triangle",
          symbol_size = config@point_size,
          silent = TRUE,
          z = 5L
        ))
      })
    )
  }
  notes <- character()
  if (length(capped_at)) {
    notes <- c(
      notes,
      paste0(
        "p = 0 shown at ",
        format(prepared[["zero_cap"]], trim = TRUE),
        " (triangles)"
      )
    )
  }
  if (any(!keep)) {
    notes <- c(
      notes,
      paste(
        sum(!keep),
        "outcome(s) omitted;",
        prepared[["n_tests"]],
        "tests in family"
      )
    )
  }
  x_range <- if (volcano) {
    config@xlim %||%
      calc_limits(
        c(
          d[["estimate"]][keep],
          if (config@reference) config@x_thresh
        ),
        0.12
      )
  } else {
    NULL
  }
  y_range <- config@ylim %||%
    calc_limits(
      c(
        0,
        d[["value"]][keep],
        if (config@reference) prepared[["threshold"]]
      ),
      0.12
    )
  # A user-supplied range must not silently discard the explicitly capped marks.
  if (
    length(capped_at) &&
      (prepared[["zero_cap"]] < y_range[[1L]] ||
        prepared[["zero_cap"]] > y_range[[2L]])
  ) {
    abort(
      "Expand `ylim` to include `zero_cap`, or change the display transform.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  EChartsOption(
    title = if (!is.null(config@title) || length(notes)) {
      Title(
        text = config@title,
        subtext = if (length(notes)) paste(notes, collapse = "; "),
        left = "center"
      )
    } else {
      NULL
    },
    legend = if (config@legend) {
      Legend(data = as.list(groups[present]), bottom = 0L, item_gap = 20)
    } else {
      NULL
    },
    tooltip = Tooltip(trigger = "item"),
    x_axis = Axis(
      type = if (volcano) "value" else "category",
      name = config@xlab,
      name_location = "middle",
      scale = volcano,
      data = if (!volcano) as.list(d[["label"]]) else NULL,
      axis_label = if (volcano) no_corner_axis_label() else NULL,
      split_line = if (volcano) no_corner_split_line() else NULL,
      min = if (volcano) x_range[[1L]] else NULL,
      max = if (volcano) x_range[[2L]] else NULL
    ),
    y_axis = Axis(
      type = "value",
      name = config@ylab,
      name_location = "middle",
      axis_label = no_corner_axis_label(),
      split_line = no_corner_split_line(),
      min = y_range[[1L]],
      max = y_range[[2L]]
    ),
    grid = Grid(
      left = config@margin_left %||% 60L,
      right = config@margin_right %||% 36L,
      top = config@margin_top %||% if (length(notes)) 70L else 42L,
      bottom = config@margin_bottom %||% if (config@legend) 68L else 40L
    ),
    series = series
  )
}


#' Align vectors for a significance chart
#' @param x Numeric vector: Estimates, with NA for unavailable values.
#' @param pvals Numeric vector: Raw p-values in `[0, 1]`, or NA.
#' @param xnames Optional Character vector: Outcome labels.
#' @return Data frame with estimate, p_value, and label columns.
#' @keywords internal
#' @noRd
significance_input <- new_generic("significance_input", "x")
method(significance_input, class_any) <- function(x, pvals, xnames = NULL) {
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
#' @inheritParams SignificanceConfig
#' @param ... Additional named settings for [setup_SignificanceConfig()], such as
#'   `reference`, `zero_cap`, `n_tests`, colors, axis limits, or margins.
#' @param theme Optional [Theme]: Widget/export theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_volcano(c(-2, 0.1, 3), c(0.001, 0.6, 0.002),
#'   xnames = c("A", "B", "C"), p_adjust_method = "holm")
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
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  data <- significance_input(x, pvals, xnames)
  config <- setup_SignificanceConfig(
    view = "volcano",
    label = "label",
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    p_thresh = p_thresh,
    x_thresh = x_thresh,
    annotate_n = annotate_n,
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
#' @inheritParams SignificanceConfig
#' @param ... Additional named settings for [setup_SignificanceConfig()], such as
#'   `reference`, `zero_cap`, `n_tests`, colors, axis limits, or margins.
#' @param theme Optional [Theme]: Widget/export theme.
#' @param width,height Optional Numeric or Character: Widget dimensions.
#' @param element_id Optional Character: HTML element identifier.
#' @param filename Optional Character: Static output path, currently SVG.
#' @return An ECharts htmlwidget.
#' @export
#' @examples
#' draw_manhattan(c(-2, 0.1, 3), c(0.001, 0.6, 0.002),
#'   xnames = c("A", "B", "C"), p_adjust_method = "holm")
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
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  data <- significance_input(x, pvals, xnames)
  config <- setup_SignificanceConfig(
    view = "manhattan",
    label = "label",
    p_adjust_method = p_adjust_method,
    p_transform = p_transform,
    p_thresh = p_thresh,
    x_thresh = x_thresh,
    annotate_n = annotate_n,
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
