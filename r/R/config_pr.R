# config_pr.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Precision-Recall Plot Configuration
#'
#' Portable bindings for long precision-recall records. Each split/class/fold
#' identifies one curve. The reserved fold label `aggregate` identifies pooled
#' predictions. Compiles to `LineSeriesOption` in
#' `src/chart/line/LineSeries.ts`, `GridOption` in
#' `src/coord/cartesian/GridModel.ts`, and `LegendOption` in
#' `src/component/legend/LegendModel.ts`.
#'
#' @section Statistical semantics:
#' Vertices are ordered by recall, then by decreasing precision, which is
#' threshold order. Curves must include recall 0 and recall 1. Consecutive
#' vertices are joined by steps: precision holds its value at each vertex back
#' to the previous recall, so the area under the drawn curve is the average
#' precision, \eqn{\sum_k (R_k - R_{k-1}) P_k}. Precision is not linear in recall
#' between two points of the curve, so a straight segment misstates the
#' attainable values (Davis and Goadrich, 2006).
#'
#' Supplied average precision is authoritative and may describe a
#' higher-resolution curve. Without an `ap` binding, the supplied vertices
#' define it. An undefined curve is represented by missing recall, precision,
#' and average precision and is reported in the console when omitted.
#' Per-resample legends report the unweighted mean and sample SD of average
#' precision over defined folds. SD is NA for one defined fold. Pooled average
#' precision is never inferred from per-fold values.
#'
#' The chance level of a curve is the prevalence of its positive class, the
#' expected precision of a classifier that scores independently of the label
#' (Saito and Rehmsmeier, 2015). Prevalence differs by class and sample, so each
#' curve group draws its own dotted baseline, in the group color, at the mean
#' prevalence of its defined folds. The baseline toggles with its curves in the
#' legend.
#'
#' The legend uses the shared position and placement controls. Hover describes
#' the selected curve, including its resample identifier. Numeric tooltip labels
#' use `digits`; coordinates and summaries retain their full precision in the
#' compiled data.
#'
#' @references
#' Davis J, Goadrich M (2006). The relationship between precision-recall and ROC
#' curves. Proceedings of the 23rd International Conference on Machine Learning,
#' 233-240. \doi{10.1145/1143844.1143874}
#'
#' Saito T, Rehmsmeier M (2015). The precision-recall plot is more informative
#' than the ROC plot when evaluating binary classifiers on imbalanced datasets.
#' PLoS ONE, 10(3), e0118432. \doi{10.1371/journal.pone.0118432}
#'
#' @param recall Character: Column containing recall values.
#' @param precision Character: Column containing precision values.
#' @param ap Optional Character: Optional supplied average precision column; unset computes it from the supplied vertices.
#' @param prevalence Optional Character: Optional positive-class prevalence column, constant within each curve.
#' @param class_label Optional Character: Optional positive-class label column.
#' @param split Optional Character: Optional sample or model label column.
#' @param fold Optional Character: Optional resample column; aggregate identifies pooled curves.
#' @param omitted Optional Character: Optional omitted-observation count column, constant within each curve.
#' @param variant Character: Draw pooled curves or separate resample curves.
#' @param digits Integer: Decimal places for average precision labels and tooltip values.
#' @param baseline Logical: Show the prevalence baseline of each curve group.
#' @param palette Optional Character: Group colors; unset uses the chart theme.
#' @param legend Logical: Show group labels and average precision summaries.
#' @inheritParams draw_line
#' @param square Logical: Keep the plotting grid square.
#' @param line_width Numeric: Curve stroke width in pixels.
#' @param fold_opacity Numeric: Opacity of individual resample curves.
#' @param xlab Character: Horizontal axis label.
#' @param ylab Character: Vertical axis label.
#' @inheritParams ChartConfig
#' @return A `PRConfig` object.
#' @export
#' @examples
#' draw(
#'   setup_PRConfig(),
#'   data = data.frame(recall = c(0, 0.5, 1), precision = c(1, 1, 0.5))
#' )
PRConfig <- new_class(
  name = "PRConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("pr"),
      recall = prop_string(
        "recall",
        description = "Column containing recall values."
      ),
      precision = prop_string(
        "precision",
        description = "Column containing precision values."
      ),
      ap = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional supplied average precision column; unset computes it from the supplied vertices."
      ),
      prevalence = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional positive-class prevalence column, constant within each curve."
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
        description = "Decimal places for average precision labels and tooltip values."
      ),
      baseline = prop_boolean(
        TRUE,
        description = "Show the prevalence baseline of each curve group."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Group colors; unset uses the chart theme."
      ),
      legend = prop_boolean(
        TRUE,
        description = "Show group labels and average precision summaries."
      ),
      square = prop_boolean(
        TRUE,
        description = "Keep the plotting grid square."
      ),
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
        "Recall",
        description = "Horizontal axis label."
      ),
      ylab = prop_string(
        "Precision",
        description = "Vertical axis label."
      )
    )
  ),
  validator = function(self) {
    if (!is.finite(self@line_width)) "@line_width must be finite" else NULL
  }
)

PR_ORIGIN_NAMES <- setdiff(
  names(PRConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a Precision-Recall Plot Configuration
#' @inheritParams PRConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [PRConfig] object.
#' @export
#' @examples
#' setup_PRConfig(class_label = "class", split = "split", fold = "fold", ap = "ap")
setup_PRConfig <- function(
  recall = "recall",
  precision = "precision",
  ap = NULL,
  prevalence = NULL,
  class_label = NULL,
  split = NULL,
  fold = NULL,
  omitted = NULL,
  variant = "aggregate",
  digits = 3L,
  baseline = TRUE,
  palette = NULL,
  legend = TRUE,
  legend_position = "top",
  legend_placement = "outside",
  legend_title = NULL,
  square = TRUE,
  line_width = 2,
  fold_opacity = 0.45,
  xlab = "Recall",
  ylab = "Precision",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), PR_ORIGIN_NAMES)
  PRConfig(
    recall = recall,
    precision = precision,
    ap = ap,
    prevalence = prevalence,
    class_label = class_label,
    split = split,
    fold = fold,
    omitted = omitted,
    variant = variant,
    digits = clean_int(digits),
    baseline = baseline,
    palette = palette,
    legend = legend,
    legend_position = legend_position,
    legend_placement = legend_placement,
    legend_title = legend_title,
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

method(resolve, PRConfig) <- function(config, data = NULL, ...) config
method(to_list, PRConfig) <- function(x) chart_config_to_list(x)
method(compile, PRConfig) <- function(config, data = NULL, theme = NULL, ...) {
  check_dots_empty(...)
  pr_option(config, data)
}
method(render_meta, PRConfig) <- square_render_meta
method(draw, PRConfig) <- draw_curve_config
