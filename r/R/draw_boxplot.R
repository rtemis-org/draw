# draw_boxplot.R
# spec: draw/first-cran-release#metric-distributions

#' Validate boxplot observations without computing quantiles
#' @param x Numeric: Finite or missing observations.
#' @inheritParams draw_boxplot
#' @return Numeric: Validated observations, retaining missing values.
#' @keywords internal
#' @noRd
boxplot_values <- new_generic("boxplot_values", "x")
method(boxplot_values, class_any) <- function(x, na_rm = TRUE) {
  if (is.logical(x) && all(is.na(x))) {
    x <- as.numeric(x)
  }
  if (
    !is.numeric(x) || is.complex(x) || !is.null(dim(x)) || any(is.infinite(x))
  ) {
    abort(
      "Supply numeric boxplot values that are finite or NA.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!na_rm && anyNA(x)) {
    abort(
      "Remove missing boxplot values or set `na_rm = TRUE`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  x
}

#' Summarize one box without replacing unavailable observations
#' @param x Numeric: Observations, optionally missing.
#' @inheritParams draw_boxplot
#' @return List with five plotting statistics, retained row indices, outlier
#'   flags, and missing count.
#' @keywords internal
#' @noRd
boxplot_summary <- new_generic("boxplot_summary", "x")
method(boxplot_summary, class_any) <- function(
  x,
  quartiles = "linear",
  whisker = 1.5,
  na_rm = TRUE
) {
  x <- boxplot_values(x, na_rm)
  at <- which(!is.na(x))
  values <- x[at]
  if (!length(values)) {
    return(list(
      stats = rep(NA_real_, 5),
      at = at,
      outlier = logical(),
      missing = sum(is.na(x))
    ))
  }
  q <- if (quartiles == "hinges") {
    stats::fivenum(values)[2:4]
  } else {
    as.numeric(stats::quantile(values, c(.25, .5, .75), type = 7))
  }
  # Whiskers end at observed values inside the fences. When a fence lies
  # inside an interpolated quartile (e.g. a very small multiplier), include the
  # quartile itself so the five plotting statistics remain ordered.
  inside <- if (whisker == 0) {
    rep(TRUE, length(values))
  } else {
    iqr <- q[[3]] - q[[1]]
    values >= q[[1]] - whisker * iqr & values <= q[[3]] + whisker * iqr
  }
  ends <- range(c(values[inside], q[c(1, 3)]))
  list(
    stats = c(ends[[1]], q, ends[[2]]),
    at = at,
    outlier = !inside,
    missing = sum(is.na(x))
  )
}

#' Deterministic, bounded offsets without changing R's random state
#' @param x Integer: Positive observation indices.
#' @return Numeric: Base-two radical-inverse positions centered on zero.
#' @keywords internal
#' @noRd
boxplot_offsets <- new_generic("boxplot_offsets", "x")
method(boxplot_offsets, class_numeric) <- function(x) {
  value <- numeric(length(x))
  weight <- .5
  while (any(x > 0)) {
    value <- value + (x %% 2) * weight
    x <- floor(x / 2)
    weight <- weight / 2
  }
  value - .5
}

#' Build box summaries and observation overlays
#' @inheritParams draw_boxplot
#' @return EChartsOption: Native boxes and named vector point renderers.
#' @keywords internal
#' @noRd
boxplot_option <- new_generic("boxplot_option", "x")
method(boxplot_option, class_any) <- function(
  x,
  labels = NULL,
  group = NULL,
  horizontal = FALSE,
  palette = NULL,
  fill_alpha = .25,
  na_rm = TRUE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margins = DEFAULT_MARGINS,
  verbosity = 1L,
  observation = NULL,
  quartiles = "linear",
  whisker = 1.5,
  boxpoints = "none",
  point_size = 5,
  point_alpha = .6,
  point_spread = .5
) {
  # Validate the vector entry point through the same property declarations
  # used by JSON configs; no second set of statistical or style defaults.
  setup_BoxplotConfig(
    horizontal = horizontal,
    palette = palette,
    fill_alpha = fill_alpha,
    na_rm = na_rm,
    quartiles = quartiles,
    whisker = whisker,
    boxpoints = boxpoints,
    point_size = point_size,
    point_alpha = point_alpha,
    point_spread = point_spread,
    labels = labels,
    xlab = xlab,
    ylab = ylab,
    title = title
  )
  check_integer_scalar(verbosity)
  if (verbosity < 0L) {
    abort(
      "Set `verbosity` to a nonnegative integer.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.list(x)) {
    x <- list(x)
  }
  if (!length(x)) {
    abort(
      "Supply at least one numeric vector for a boxplot.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  # Reject invalid values before filtering missing groups, including Inf in
  # otherwise excluded rows. Group order follows first appearance.
  x <- lapply(x, boxplot_values, na_rm = na_rm)
  sizes <- lengths(x)
  group <- group_values(group, sizes)
  if (
    !is.null(observation) &&
      (!is.atomic(observation) ||
        !is.null(dim(observation)) ||
        anyNA(observation) ||
        any(sizes != length(observation)))
  ) {
    abort(
      "Supply one nonmissing observation identifier per row of every boxplot column.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  variable_names <- names(x)
  if (
    is.null(variable_names) ||
      anyNA(variable_names) ||
      any(!nzchar(variable_names))
  ) {
    variable_names <- paste("Variable", seq_along(x))
  }
  grouped <- !is.null(group)
  multi <- grouped && length(x) > 1L
  levels <- if (grouped) unique(as.character(group[!is.na(group)])) else NULL
  categories <- if (grouped && !multi) levels else labels %||% variable_names
  if (
    length(categories) != if (grouped && !multi) length(levels) else length(x)
  ) {
    abort(
      "Supply one label per boxplot column.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (!length(categories)) {
    abort(
      "Supply at least one nonmissing boxplot group.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  colors <- unname(palette_colors(
    palette %||% if (grouped) rtemis_colors else rtemis_colors[[1L]]
  ))
  colors <- rep_len(colors, if (grouped) length(levels) else length(x))
  nseries <- if (multi) length(levels) else 1L
  boxes <- vector("list", nseries)
  overlays <- vector("list", nseries)
  missing <- 0L
  available <- 0L
  visible_limits <- c(Inf, -Inf)
  for (s in seq_len(nseries)) {
    box_data <- vector("list", length(categories))
    point_data <- list()
    for (j in seq_along(categories)) {
      column <- if (grouped && !multi) 1L else j
      g <- if (multi) s else j
      rows <- if (grouped) {
        which(!is.na(group) & as.character(group) == levels[[g]])
      } else {
        seq_along(x[[column]])
      }
      values <- x[[column]][rows]
      summary <- boxplot_summary(values, quartiles, whisker, na_rm)
      missing <- missing + summary[["missing"]]
      available <- available + length(summary[["at"]])
      color <- colors[[if (grouped) g else j]]
      style <- list(
        color = color_with_alpha(color, fill_alpha),
        borderColor = color
      )
      box_data[[j]] <- if (
        grouped && !multi || !grouped && length(unique(colors)) > 1L
      ) {
        list(
          name = categories[[j]],
          value = summary[["stats"]],
          itemStyle = style
        )
      } else {
        summary[["stats"]]
      }
      take <- if (boxpoints == "all") {
        seq_along(summary[["at"]])
      } else if (boxpoints == "outliers") {
        which(summary[["outlier"]])
      } else {
        integer()
      }
      at <- summary[["at"]][take]
      visible <- c(summary[["stats"]], values[at])
      visible <- visible[is.finite(visible)]
      if (length(visible)) {
        visible_limits <- c(
          min(visible_limits[[1]], visible),
          max(visible_limits[[2]], visible)
        )
      }

      # Offsets use original row indices, so missing values and changing point
      # mode do not move the surviving observations. Never consume RNG state.
      offsets <- boxplot_offsets(as.numeric(rows[at])) * point_spread
      for (k in seq_along(at)) {
        id <- if (is.null(observation)) {
          as.character(rows[at[[k]]])
        } else {
          as.character(observation[rows[at[[k]]]])
        }
        point_data[[length(point_data) + 1L]] <- list(
          value = list(j - 1L, values[[at[[k]]]], id, offsets[[k]]),
          itemStyle = list(color = color, opacity = point_alpha)
        )
      }
    }
    color <- colors[[if (multi) s else 1L]]
    boxes[[s]] <- BoxplotSeries(
      name = if (multi) levels[[s]] else NULL,
      data = box_data,
      item_style = ItemStyle(
        color = color_with_alpha(color, fill_alpha),
        border_color = color
      )
    )
    if (length(point_data)) {
      overlays[[s]] <- list(
        type = "custom",
        name = if (multi) levels[[s]] else NULL,
        renderItem = "rtemis.boxplot_points.v1",
        clip = TRUE,
        z = 3,
        dimensions = list("Category", "Value", "Observation", "Offset"),
        encode = if (horizontal) {
          list(x = 1L, y = 0L, tooltip = list(1L, 2L))
        } else {
          list(x = 0L, y = 1L, tooltip = list(1L, 2L))
        },
        itemPayload = list(
          horizontal = horizontal,
          boxSeries = as.list(seq_len(nseries) - 1L),
          boxIndex = s - 1L,
          pointSize = point_size,
          pointAlpha = point_alpha
        ),
        data = point_data
      )
    }
  }
  if (!available) {
    abort(
      "Supply at least one available boxplot value; missing groups are not zeros.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (missing) {
    msg(
      "Removed",
      missing,
      "NA",
      ngettext(missing, "value", "values"),
      "from data",
      verbosity = verbosity
    )
  }
  value_name <- if (grouped && !multi && !is.null(names(x))) {
    names(x)[[1L]]
  } else {
    NULL
  }
  categories <- if (length(categories) == 1L) {
    as.list(categories)
  } else {
    categories
  }
  # Native automatic thinning can hide the identity of a whole box, even with
  # only three short category names at phone widths. Keep those names visible.
  x_axis <- if (horizontal) {
    Axis(type = "value", scale = TRUE, name = xlab %||% value_name)
  } else {
    Axis(
      type = "category",
      data = categories,
      name = xlab,
      axis_label = AxisLabel(interval = 0)
    )
  }
  y_axis <- if (horizontal) {
    Axis(
      type = "category",
      data = categories,
      name = ylab,
      axis_label = AxisLabel(interval = 0)
    )
  } else {
    Axis(type = "value", scale = TRUE, name = ylab %||% value_name)
  }
  # Center names as for bars and lines, keeping them outside the plot area.
  if (!is.null(x_axis@name)) {
    x_axis@name_location <- "middle"
  }
  if (!is.null(y_axis@name)) {
    y_axis@name_location <- "middle"
  }
  # Give endpoint markers room without changing their data coordinates.
  # Include only drawn values; hidden outliers do not shrink a box-only plot.
  limits <- visible_limits
  if (boxpoints != "none") {
    limits <- calc_limits(visible_limits, pad = .05)
    if (horizontal) {
      x_axis@min <- limits[[1L]]
      x_axis@max <- limits[[2L]]
      x_axis@axis_label <- AxisLabel(
        show_min_label = FALSE,
        show_max_label = FALSE
      )
    } else {
      y_axis@min <- limits[[1L]]
      y_axis@max <- limits[[2L]]
      y_axis@axis_label <- AxisLabel(
        show_min_label = FALSE,
        show_max_label = FALSE
      )
    }
  }
  # ECharts otherwise places the category-axis baseline at the range edge
  # when zero is absent. Reuse the line/scatter rule: emphasize only zero.
  if (horizontal) {
    y_axis@axis_line <- axis_line_for_orthogonal(limits)
  } else {
    x_axis@axis_line <- axis_line_for_orthogonal(limits)
  }
  EChartsOption(
    title = if (!is.null(title)) Title(text = title) else NULL,
    tooltip = Tooltip(trigger = "item"),
    legend = if (multi) Legend(data = as.list(levels)) else NULL,
    x_axis = x_axis,
    y_axis = y_axis,
    grid = resolve_margins(margins),
    series = c(boxes, Filter(Negate(is.null), overlays))
  )
}

#' Draw a Boxplot with Observation Overlays
#'
#' Draw one box per numeric vector, optionally split by groups. Statistics use
#' linear-interpolation quartiles (`stats::quantile(type = 7)`) or Tukey hinges
#' (`stats::fivenum()`). Whiskers extend to observations within the quartiles
#' plus/minus `whisker` times their difference, including the quartiles
#' themselves. `whisker = 0` uses the full observed range. Values strictly
#' outside the fences are outliers; they are never removed from the quartiles.
#'
#' All-NA and empty boxes retain their category without a mark; entirely
#' unavailable input is an error. Missing values are reported in the console
#' according to `verbosity` and never replaced with zero. Missing group
#' assignments are excluded. Axis titles are centered. The category-axis
#' baseline is shown only when the plotted value range includes zero.
#' Every category label is shown so boxes remain identifiable in narrow plots.
#' Use a horizontal layout or a larger figure for many or long category names.
#' With point overlays, padded range endpoints are unlabeled; interior ticks
#' retain the backend's numeric formatting and plotted values are unchanged.
#' Points retain their exact value coordinate. Their perpendicular offsets use
#' a deterministic base-two sequence in input row order within `point_spread`
#' times the box width. Zero spread centers every point; coincident points can
#' overlap. No random state is read or changed. Browser and SVG share the same
#' named point renderer, including grouped-box offsets and legend filtering.
#'
#' @param x Numeric or List: Numeric vectors, one per box; named lists supply
#'   category labels. With `group`, every vector must match its length.
#' @param labels Optional Character: One category label per vector. A single
#'   grouped vector uses group labels instead.
#' @param group Optional Atomic vector or single-column data frame: Group
#'   identities in first-appearance order. The column name does not add a legend
#'   or heading; both input forms produce the same plot.
#' @param observation Optional Atomic vector: Observation identifiers in tooltips,
#'   aligned to every input vector. Unset uses original row numbers.
#' @param horizontal Logical: Draw horizontal boxes.
#' @param quartiles Character \{"linear", "hinges"\}: Quartile convention.
#' @param whisker Numeric `[0, Inf)`: Finite IQR fence multiplier;
#'   zero uses full-range whiskers.
#' @param boxpoints Character \{"none", "all", "outliers"\}: Values to overlay.
#' @param point_size Numeric `(0, Inf)`: Finite point diameter in pixels.
#' @param point_alpha Numeric `[0, 1]`: Point opacity.
#' @param point_spread Numeric `[0, 1]`: Fraction of box width occupied by offsets.
#' @param na_rm Logical: Remove missing values. FALSE rejects missing input.
#' @param palette Optional Character: Box colors, recycled across boxes or groups.
#' @param fill_alpha Numeric `[0, 1]`: Box fill opacity.
#' @param xlab,ylab Optional Character: Axis labels.
#' @param title Optional Character: Chart title.
#' @param verbosity Integer `[0, Inf)`: Verbosity for missing-value messages.
#' @inheritParams draw_line
#' @return htmlwidget: ECharts boxes with optional vector point overlays.
#' @export
#' @examples
#' draw_boxplot(list(Training = c(.8, .9, .85), Test = c(.7, .8, .75)),
#'   boxpoints = "all", observation = c("Fold1", "Fold2", "Fold3"))
#' draw_boxplot(iris["Sepal.Length"], group = iris["Species"])
draw_boxplot <- function(
  x,
  labels = NULL,
  group = NULL,
  horizontal = FALSE,
  palette = NULL,
  fill_alpha = .25,
  na_rm = TRUE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  verbosity = 1L,
  element_id = NULL,
  filename = NULL,
  observation = NULL,
  quartiles = "linear",
  whisker = 1.5,
  boxpoints = "none",
  point_size = 5,
  point_alpha = .6,
  point_spread = .5
) {
  opt <- boxplot_option(
    x = x,
    labels = labels,
    group = group,
    horizontal = horizontal,
    palette = palette,
    fill_alpha = fill_alpha,
    na_rm = na_rm,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margins = margins,
    verbosity = verbosity,
    observation = observation,
    quartiles = quartiles,
    whisker = whisker,
    boxpoints = boxpoints,
    point_size = point_size,
    point_alpha = point_alpha,
    point_spread = point_spread
  )
  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
