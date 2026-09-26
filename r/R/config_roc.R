# config_roc.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' ROC Plot Configuration
#'
#' Portable bindings for long ROC records. Each split/class/fold identifies one
#' curve. The reserved fold label `aggregate` identifies pooled predictions.
#' Compiles to `LineSeriesOption` in `src/chart/line/LineSeries.ts`,
#' `GridOption` in `src/coord/cartesian/GridModel.ts`, and `LegendOption` in
#' `src/component/legend/LegendModel.ts`.
#'
#' @section Statistical semantics:
#' Vertices are ordered by FPR then TPR and joined without smoothing. Curves
#' must be monotone and include (0, 0) and (1, 1). Supplied AUC is authoritative
#' and may describe a higher-resolution curve. Without an AUC binding, the
#' supplied vertices define trapezoidal AUC. An undefined curve is represented
#' by missing FPR, TPR, and AUC and is omitted with a visible count.
#' Per-resample legends report the unweighted mean and sample SD of defined
#' fold AUCs, with available/total curve counts. SD is NA for one defined fold.
#' Pooled AUC is never inferred from per-fold AUCs.
#' The legend is inset in the chosen corner of the plotting area. Curve identity
#' and AUC share a line; long labels wrap to the available width. All entries
#' remain present in static SVG output, without a scrolling legend.
#' Hover describes the selected curve, including its resample identifier.
#' Numeric tooltip labels use `digits`; coordinates and AUCs retain their full
#' precision in the compiled data.
#'
#' @param fpr Character: Column containing false positive rates.
#' @param tpr Character: Column containing true positive rates.
#' @param auc Optional Character: Optional supplied AUC column; unset integrates the supplied vertices.
#' @param class_label Optional Character: Optional positive-class label column.
#' @param split Optional Character: Optional sample or model label column.
#' @param fold Optional Character: Optional resample column; aggregate identifies pooled curves.
#' @param omitted Optional Character: Optional omitted-observation count column, constant within each curve.
#' @param variant Character: Draw pooled curves or separate resample curves.
#' @param digits Integer: Decimal places for AUC labels and tooltip values.
#' @param diagonal Logical: Show an independent chance diagonal.
#' @param diagonal_color Character: Chance-line color.
#' @param palette Optional Character: Group colors; unset uses the chart theme.
#' @param legend Logical: Show group labels and AUC summaries.
#' @param legend_position Character \{"bottom-right", "top-right", "top-left", "bottom-left"\}: Corner inside the plotting area for the legend.
#' @param square Logical: Keep the plotting grid square.
#' @param line_width Numeric: Curve stroke width in pixels.
#' @param fold_opacity Numeric: Opacity of individual resample curves.
#' @param xlab Character: Horizontal axis label.
#' @param ylab Character: Vertical axis label.
#' @inheritParams ChartConfig
#' @return A `ROCConfig` object.
#' @export
#' @examples
#' draw(setup_ROCConfig(), data = data.frame(fpr = c(0, 0, 1), tpr = c(0, 1, 1)))
ROCConfig <- new_class(
  name = "ROCConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("roc"),
    fpr = prop_string(
      "fpr",
      description = "Column containing false positive rates."
    ),
    tpr = prop_string(
      "tpr",
      description = "Column containing true positive rates."
    ),
    auc = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional supplied AUC column; unset integrates the supplied vertices."
    ),
    class_label = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional positive-class label column."
    ),
    split = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional sample or model label column."
    ),
    fold = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional resample column; aggregate identifies pooled curves."
    ),
    omitted = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional omitted-observation count column, constant within each curve."
    ),
    variant = prop_string(
      "aggregate",
      enum = c("aggregate", "per_resample"),
      description = "Draw pooled curves or separate resample curves."
    ),
    digits = prop_integer(
      3L,
      min = 0L,
      max = 8L,
      description = "Decimal places for AUC labels and tooltip values."
    ),
    diagonal = prop_boolean(
      TRUE,
      description = "Show an independent chance diagonal."
    ),
    diagonal_color = prop_string("#888888", description = "Chance-line color."),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Group colors; unset uses the chart theme."
    ),
    legend = prop_boolean(
      TRUE,
      description = "Show group labels and AUC summaries."
    ),
    legend_position = prop_string(
      "bottom-right",
      enum = c("bottom-right", "top-right", "top-left", "bottom-left"),
      description = "Corner inside the plotting area for the legend."
    ),
    square = prop_boolean(TRUE, description = "Keep the plotting grid square."),
    line_width = prop_float(
      2,
      exclusive_min = 0,
      description = "Curve stroke width in pixels."
    ),
    fold_opacity = prop_float(
      0.45,
      min = 0,
      max = 1,
      description = "Opacity of individual resample curves."
    ),
    xlab = prop_string(
      "False positive rate",
      description = "Horizontal axis label."
    ),
    ylab = prop_string(
      "True positive rate",
      description = "Vertical axis label."
    )
  ),
  validator = function(self) {
    if (!is.finite(self@line_width)) "@line_width must be finite" else NULL
  }
)

ROC_ORIGIN_NAMES <- setdiff(
  names(ROCConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a ROC Plot Configuration
#' @inheritParams ROCConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [ROCConfig] object.
#' @export
#' @examples
#' setup_ROCConfig(class_label = "class", split = "split", fold = "fold", auc = "auc")
setup_ROCConfig <- function(
  fpr = "fpr",
  tpr = "tpr",
  auc = NULL,
  class_label = NULL,
  split = NULL,
  fold = NULL,
  omitted = NULL,
  variant = "aggregate",
  digits = 3L,
  diagonal = TRUE,
  diagonal_color = "#888888",
  palette = NULL,
  legend = TRUE,
  legend_position = "bottom-right",
  square = TRUE,
  line_width = 2,
  fold_opacity = 0.45,
  xlab = "False positive rate",
  ylab = "True positive rate",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), ROC_ORIGIN_NAMES)
  ROCConfig(
    fpr = fpr,
    tpr = tpr,
    auc = auc,
    class_label = class_label,
    split = split,
    fold = fold,
    omitted = omitted,
    variant = variant,
    digits = clean_int(digits),
    diagonal = diagonal,
    diagonal_color = diagonal_color,
    palette = palette,
    legend = legend,
    legend_position = legend_position,
    square = square,
    line_width = line_width,
    fold_opacity = fold_opacity,
    xlab = xlab,
    ylab = ylab,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, ROCConfig) <- function(config, data = NULL, ...) config
method(to_list, ROCConfig) <- function(x) chart_config_to_list(x)
method(compile, ROCConfig) <- function(config, data = NULL, ...) {
  check_dots_empty(...)
  roc_option(config, data)
}

method(render_meta, ROCConfig) <- function(config, option) {
  meta <- list(legendPosition = config@legend_position)
  if (!config@square) {
    return(meta)
  }
  grid <- option@grid
  meta[["aspect"]] <- list(
    ratio = 1,
    leftPx = grid[["left"]],
    rightPx = grid[["right"]],
    topPx = grid[["top"]],
    botPx = grid[["bottom"]]
  )
  meta
}

#' Render a ROC config with an inset legend
#' @inheritParams draw
#' @param data Optional Data frame: Long ROC records.
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
method(draw, ROCConfig) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL
) {
  built <- compile(option, data)
  grid <- built@grid
  width <- width %||% (480 + grid[["left"]] + grid[["right"]])
  height <- height %||% (480 + grid[["top"]] + grid[["bottom"]])
  draw(
    built,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    meta = render_meta(option, built),
    ...
  )
}
