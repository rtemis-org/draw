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
  # inside an interpolated quartile (e.g. a multiplier near zero), include the
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

#' Summarize boxplot columns into an @rtemis/draw build request
#'
#' Computes what the boxplot builder draws, one cell per column, or per column
#' and group level: the five plotting statistics, the count, the observed
#' range, the drawn points, the violin profile, and, for `paired`, every value
#' with its identifier. Orders the categories as `order` says.
#' spec: draw/shared-builders#boxplot
#' @inheritParams draw_boxplot
#' @return [BuildRequest] of type `"boxplot"`.
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
  point_spread = .5,
  mean_marker = FALSE,
  geometry = "box",
  bandwidth = NULL,
  adjust = 1,
  density_points = 128L,
  paired = FALSE,
  pair_alpha = .35,
  pair_width = 1,
  comparisons = NULL,
  order = "input",
  transform = "none",
  zoom = FALSE,
  group_layout = "auto",
  color_by_group = FALSE,
  ylim = NULL,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL
) {
  check_zoom_flag(zoom)
  margin <- pixel_margins(margins)
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
  # otherwise excluded rows. Group order follows `group_levels()`.
  x <- lapply(x, boxplot_values, na_rm = na_rm)
  x <- lapply(x, boxplot_transform, transform = transform)
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
  if (paired && is.null(observation)) {
    abort(
      "Supply explicit `observation` identifiers when `paired = TRUE`.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  variables <- names(x)
  if (is.null(variables) || anyNA(variables) || any(!nzchar(variables))) {
    variables <- paste("Variable", seq_along(x))
  }
  names(x) <- variables
  if (!is.null(labels) && length(labels) != length(x)) {
    abort(
      "Supply one label per boxplot column.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  names(variables) <- labels %||% variables
  grouped <- !is.null(group)
  levels <- if (grouped) group_levels(group) else NULL
  if (grouped && !length(levels)) {
    abort(
      "Supply at least one nonmissing boxplot group.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  layout <- if (!grouped) {
    "none"
  } else if (group_layout == "auto") {
    if (length(x) == 1L) "x-axis" else "legend"
  } else {
    group_layout
  }
  rows_of <- function(variable, level) {
    if (is.null(level)) {
      seq_along(x[[variable]])
    } else {
      which(!is.na(group) & as.character(group) == level)
    }
  }
  # `order` sorts the categories by their values pooled across series.
  if (order != "input") {
    if (layout == "x-axis") {
      samples <- lapply(levels, function(level) {
        unlist(lapply(variables, function(v) x[[v]][rows_of(v, level)]))
      })
      names(samples) <- levels
      levels <- names(distribution_order(
        lapply(samples, function(v) v[!is.na(v)]),
        order
      ))
    } else {
      samples <- lapply(variables, function(v) x[[v]])
      names(samples) <- variables
      kept <- names(distribution_order(
        lapply(samples, function(v) v[!is.na(v)]),
        order
      ))
      variables <- variables[match(kept, variables)]
    }
  }
  missing <- 0L
  cells <- list()
  for (variable in variables) {
    for (level in if (grouped) levels else list(NULL)) {
      rows <- rows_of(variable, level)
      values <- x[[variable]][rows]
      summary <- boxplot_summary(values, quartiles, whisker, na_rm)
      missing <- missing + summary[["missing"]]
      at <- summary[["at"]]
      ids <- if (is.null(observation)) {
        as.character(rows)
      } else {
        as.character(observation[rows])
      }
      if (paired && anyDuplicated(ids[at])) {
        abort(
          "Use unique observation identifiers within each boxplot category and group.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      take <- switch(
        boxpoints,
        all = seq_along(at),
        outliers = which(summary[["outlier"]]),
        integer()
      )
      observations <- function(k) {
        lapply(k, function(i) {
          list(
            value = jsonlite::unbox(values[[at[[i]]]]),
            id = jsonlite::unbox(ids[[at[[i]]]]),
            index = jsonlite::unbox(rows[[at[[i]]]])
          )
        })
      }
      cell <- list(
        variable = jsonlite::unbox(variable),
        level = if (!is.null(level)) jsonlite::unbox(level),
        stats = if (length(at)) summary[["stats"]],
        n = jsonlite::unbox(length(at)),
        range = if (length(at)) range(values[at]),
        points = observations(take)
      )
      if (mean_marker && length(at)) {
        cell[["mean"]] <- jsonlite::unbox(mean(values[at]))
      }
      if (paired) {
        cell[["observations"]] <- observations(seq_along(at))
      }
      if (geometry != "box") {
        profile <- boxplot_density(
          values[at],
          bandwidth,
          adjust,
          density_points
        )
        cell[["profile"]] <- if (length(profile)) {
          list(
            position = unlist(profile[["position"]]),
            width = unlist(profile[["width"]])
          )
        }
      }
      cells[[length(cells) + 1L]] <- cell
    }
  }
  if (!any(vapply(cells, function(cell) cell[["n"]] > 0L, logical(1)))) {
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
  # One column whose group levels are the categories names the value axis
  # after the column; in the legend layout the column is itself the category.
  value_name <- if (layout == "x-axis" && length(x) == 1L) {
    names(variables)[[1L]]
  }
  config <- setup_BoxplotConfig(
    x = unname(variables),
    labels = if (!is.null(labels)) names(variables),
    horizontal = horizontal,
    palette = palette,
    fill_alpha = fill_alpha,
    na_rm = na_rm,
    order = order,
    transform = transform,
    geometry = geometry,
    bandwidth = bandwidth,
    adjust = adjust,
    density_points = density_points,
    paired = paired,
    pair_alpha = pair_alpha,
    pair_width = pair_width,
    quartiles = quartiles,
    whisker = whisker,
    boxpoints = boxpoints,
    point_size = point_size,
    point_alpha = point_alpha,
    point_spread = point_spread,
    mean_marker = mean_marker,
    zoom = zoom,
    group_layout = group_layout,
    color_by_group = color_by_group,
    ylim = ylim,
    xlab = xlab %||% if (horizontal) value_name,
    ylab = ylab %||% if (!horizontal) value_name,
    title = title,
    margin_top = margin[["top"]],
    margin_right = margin[["right"]],
    margin_bottom = margin[["bottom"]],
    margin_left = margin[["left"]],
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  data <- list(
    variables = I(unname(variables)),
    levels = if (grouped) I(levels),
    cells = cells,
    comparisons = boxplot_comparisons(
      comparisons,
      layout == "legend" || (layout == "x-axis" && length(x) > 1L)
    )
  )
  BuildRequest(
    type = "boxplot",
    data = as.character(jsonlite::toJSON(
      data,
      auto_unbox = FALSE,
      na = "null",
      null = "null",
      digits = NA
    )),
    config = config_members(config),
    appearance = list()
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
#' Violin densities use a Gaussian kernel with `stats::bw.nrd0()` unless a
#' numeric bandwidth is supplied, multiplied by `adjust`. They are evaluated
#' over the observed range and each is scaled to the same maximum width;
#' widths do not encode sample size or comparable density across groups.
#' Singleton and constant samples appear as crossbars, without an invented
#' density. `geometry = "both"` overlays a narrow box on each violin.
#'
#' Paired lines join matching explicit observation IDs in adjacent categories
#' only, within groups for multiple grouped vectors. A missing measurement
#' leaves a gap. IDs must be unique within each category/group. Lines meet
#' visible observation points at their deterministic offsets.
#'
#' Comparison records annotate supplied results; no statistical test or
#' multiplicity adjustment is performed. `from` and `to` name categories;
#' `label` supplies the displayed text. Multiple grouped vectors also require
#' `from_group` and `to_group`. Optional finite `position` values place brackets
#' on the value axis; otherwise brackets stack above the observed geometry.
#' References must identify distinct, nonempty cells. Brackets disappear when
#' either endpoint's group is hidden. Long labels may require a larger figure.
#'
#' @param x Numeric or List: Numeric vectors, one per box; named lists supply
#'   category labels. With `group`, every vector must match its length.
#' @param labels Optional Character: One category label per vector. A single
#'   grouped vector uses group labels instead.
#' @param group Optional Atomic vector or single-column data frame: Group
#'   identities, ordered by factor level, or by first appearance for other
#'   vectors. The column name does not add a legend or heading; both input forms
#'   produce the same plot.
#' @param observation Optional Atomic vector: Observation identifiers in tooltips,
#'   aligned to every input vector. Required for pairing; otherwise unset uses
#'   original row numbers.
#' @param geometry Character \{"box", "violin", "both"\}: Distribution geometry.
#' @param bandwidth Optional Numeric `(0, Inf)`: Finite Gaussian kernel bandwidth.
#' @param adjust Numeric `(0, Inf)`: Finite bandwidth multiplier.
#' @param density_points Integer `[16, 4096]`: Density evaluation grid size.
#' @param paired Logical: Connect explicit IDs across adjacent categories.
#' @param pair_alpha Numeric `[0, 1]`: Paired line opacity.
#' @param pair_width Numeric `(0, Inf)`: Finite paired line width in pixels.
#' @param comparisons Optional Data frame: Category endpoints and comparison labels.
#' @param horizontal Logical: Draw horizontal boxes.
#' @param quartiles Character \{"linear", "hinges"\}: Quartile convention.
#' @param whisker Numeric `[0, Inf)`: Finite IQR fence multiplier;
#'   zero uses full-range whiskers.
#' @param boxpoints Character \{"none", "all", "outliers"\}: Values to overlay.
#' @param point_size Numeric `(0, Inf)`: Finite point diameter in pixels.
#' @param point_alpha Numeric `[0, 1]`: Point opacity.
#' @param point_spread Numeric `[0, 1]`: Fraction of box width occupied by offsets.
#' @param mean_marker Logical: Mark each box's mean with a diamond.
#' @param na_rm Logical: Remove missing values. FALSE rejects missing input.
#' @param palette Optional Character: Box colors, recycled across boxes or groups.
#' @param fill_alpha Numeric `[0, 1]`: Box fill opacity.
#' @param xlab,ylab Optional Character: Axis labels. `""` draws no label.
#' @param title Optional Character: Chart title.
#' @param verbosity Integer `[0, Inf)`: Verbosity for missing-value messages.
#' @param order Character `{"input", "mean", "median"}`: Keep input order or sort
#'   categories by decreasing mean/median after transformation; empty boxes last.
#' @param transform Character `{"none", "scale", "minmax"}`: Transform each input
#'   column before grouping. Scale subtracts the mean and divides by the sample
#'   standard deviation; minmax maps the available range to zero through one.
#'   Constant columns become zero and missing values remain missing.
#' @param zoom Logical: Enable zoom on the category axis (the y-axis when
#'   `horizontal = TRUE`). `TRUE` adds a slider, Shift+wheel zoom, and drag
#'   panning; double-click resets the view.
#' @param group_layout Character \{"auto", "x-axis", "legend"\}: Where group
#'   levels go. `"x-axis"` makes them the categories, with one series per
#'   column; `"legend"` makes the columns the categories, with one series per
#'   level. `"auto"` uses `"x-axis"` for one column and `"legend"` for several.
#' @param color_by_group Logical: Color each box of a one-series chart by its
#'   category. A one-series chart otherwise takes one color, since the axis
#'   already names each box.
#' @param ylim Optional Numeric \[length 2\]: Value axis limits. `NULL` fits
#'   the drawn marks.
#' @inheritParams draw_line
#' @return htmlwidget: ECharts boxes with optional vector point overlays.
#' @export
#' @examples
#' draw_boxplot(list(Training = c(.8, .9, .85), Test = c(.7, .8, .75)),
#'   boxpoints = "all", observation = c("Fold1", "Fold2", "Fold3"))
#' draw_boxplot(iris["Sepal.Length"], group = iris["Species"])
#' @inheritParams draw_line legend_position legend_placement legend_title
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
  point_spread = .5,
  mean_marker = FALSE,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  geometry = "box",
  bandwidth = NULL,
  adjust = 1,
  density_points = 128L,
  paired = FALSE,
  pair_alpha = .35,
  pair_width = 1,
  comparisons = NULL,
  order = "input",
  transform = "none",
  zoom = FALSE,
  group_layout = "auto",
  color_by_group = FALSE,
  ylim = NULL
) {
  request <- boxplot_option(
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
    order = order,
    transform = transform,
    geometry = geometry,
    bandwidth = bandwidth,
    adjust = adjust,
    density_points = density_points,
    paired = paired,
    pair_alpha = pair_alpha,
    pair_width = pair_width,
    quartiles = quartiles,
    whisker = whisker,
    boxpoints = boxpoints,
    point_size = point_size,
    point_alpha = point_alpha,
    point_spread = point_spread,
    mean_marker = mean_marker,
    comparisons = comparisons,
    zoom = zoom,
    group_layout = group_layout,
    color_by_group = color_by_group,
    ylim = ylim,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title
  )
  draw(
    request,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta(legend_position, legend_placement, legend_title)
  )
}

#' Draw Violin Distributions
#'
#' A convenience entry point for [draw_boxplot()] with violin geometry.
#' Smoothing, grouping, observation overlays, paired lines, comparisons, and
#' vector export follow the same contract.
#' @inheritParams draw_boxplot
#' @param show_box Logical: Overlay a narrow box on each violin.
#' @param ... Arguments passed to [draw_boxplot()], except `geometry`.
#' @return htmlwidget: Vector violin distributions.
#' @export
#' @examples
#' draw_violin(iris["Sepal.Length"], group = iris[["Species"]], show_box = TRUE)
draw_violin <- function(x, ..., show_box = FALSE) {
  check_logical_scalar(show_box)
  draw_boxplot(x, ..., geometry = if (show_box) "both" else "violin")
}

#' Transform one distribution while preserving missing observation slots
#' @param x Numeric: Validated observations.
#' @param transform Character: None, sample-standardization, or minmax.
#' @return Numeric: Transformed values with the same observation order.
#' @keywords internal
#' @noRd
boxplot_transform <- new_generic("boxplot_transform", "x")
method(boxplot_transform, class_numeric) <- function(x, transform) {
  if (transform == "none" || all(is.na(x))) {
    return(x)
  }
  at <- which(!is.na(x))
  values <- x[at]
  offset <- if (transform == "scale") mean(values) else min(values)
  divisor <- if (transform == "scale") {
    stats::sd(values)
  } else {
    diff(range(values))
  }
  if (length(values) > 1L && (!is.finite(offset) || !is.finite(divisor))) {
    abort(
      "Rescale values before applying the boxplot transformation.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  x[at] <- if (length(values) == 1L || divisor == 0) {
    0
  } else {
    (values - offset) / divisor
  }
  if (any(is.infinite(x))) {
    abort(
      "Rescale the values before applying the boxplot transformation.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  x
}
