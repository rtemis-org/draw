# config_significance.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Significance Plot Configuration
#'
#' One portable specification for volcano plots and categorical significance
#' bars. The latter is the MassGLM Manhattan view, not a genomic-position plot.
#' Compiles to `ScatterSeriesOption` / `BarSeriesOption` in ECharts
#' `src/chart/scatter/ScatterSeries.ts` and `src/chart/bar/BarSeries.ts`, with
#' `MarkLineOption` from `src/component/marker/MarkLineModel.ts`.
#' `to_list()` serializes the semantic configuration using schema keys;
#' `compile()` produces the ECharts option.
#'
#' @section Statistical semantics:
#' P-values are adjusted once over the full supplied family before rows with
#' missing estimates or p-values are omitted from the marks. Missing hypotheses
#' still count in `n_tests`; they do not become observed p-values of one.
#' Raw and adjusted values remain in the compiled data. Function-valued
#' transforms are not accepted by this portable configuration.
#'
#' Under `neg_log10`, zero p-values have infinite mathematical height. They
#' retain their zero value but are displayed at `zero_cap`, identified by
#' triangles and a caption. No positive p-value is clamped. For identity and
#' one-minus transforms, zeros are finite and no cap is used.
#'
#' @param view Character {"volcano", "manhattan"}: Scatter or categorical bar view.
#' @param estimate Character: Column containing coefficients or other signed effects.
#' @param p_value Character: Column containing unadjusted p-values.
#' @param label Optional Character: Outcome-label column. Unset generates row labels.
#' @param p_adjust_method Character: Method from [stats::p.adjust.methods]. Adjustment precedes transformation.
#' @param n_tests Optional Integer [1, Inf): Hypothesis-family size, at least the number of supplied rows. Unset counts all rows, including missing p-values.
#' @param p_transform Character {"neg_log10", "identity", "one_minus"}: Named display transform of adjusted p-values.
#' @param p_thresh Numeric (0, 1]: Significance threshold on adjusted p-values; the comparison is strict.
#' @param x_thresh Numeric: Effect threshold separating low and high significant results.
#' @param zero_cap Optional Numeric (0, Inf): Finite display height for zero p-values under neg_log10; must exceed all finite heights and the significance reference. Unset computes a cap one unit above their rounded-up maximum.
#' @param annotate_n Integer [0, Inf): Maximum significant feature labels per side in the volcano view, ranked by raw p-value with input-order ties.
#' @param reference Logical: Show the transformed significance threshold and, for volcanoes, the effect threshold.
#' @param legend Logical: Show the nonempty significance groups.
#' @param negative_color Character: Color for significant effects below x_thresh.
#' @param neutral_color Character: Color for other results, including effects equal to x_thresh.
#' @param positive_color Character: Color for significant effects above x_thresh.
#' @param alpha Numeric `[0, 1]`: Mark opacity.
#' @param point_size Numeric (0, Inf): Scatter symbol size in pixels.
#' @param xlab Optional Character: X-axis label.
#' @param ylab Optional Character: Y-axis label.
#' @param xlim Optional Numeric: Exactly two increasing finite volcano x-axis limits. Not used by the categorical view.
#' @param ylim Optional Numeric: Exactly two increasing finite y-axis limits.
#' @param margin_top Optional Integer [0, Inf): Top plot margin in pixels.
#' @param margin_right Optional Integer [0, Inf): Right plot margin in pixels.
#' @param margin_bottom Optional Integer [0, Inf): Bottom plot margin in pixels.
#' @param margin_left Optional Integer [0, Inf): Left plot margin in pixels.
#' @inheritParams ChartConfig
#' @return A `SignificanceConfig` object.
#' @export
#' @examples
#' cfg <- setup_SignificanceConfig(estimate = "effect", p_value = "p")
#' draw(cfg, data = data.frame(effect = c(-2, 0.1, 3), p = c(0.001, 0.6, 0.002)))
SignificanceConfig <- new_class(
  name = "SignificanceConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("significance"),
    view = prop_string(
      "volcano",
      enum = c("volcano", "manhattan"),
      description = "Scatter or categorical bar view."
    ),
    estimate = prop_string(
      "estimate",
      description = "Column containing coefficients or other signed effects."
    ),
    p_value = prop_string(
      "p_value",
      description = "Column containing unadjusted p-values."
    ),
    label = prop_string(
      NULL,
      nullable = TRUE,
      description = "Outcome-label column. Unset generates row labels."
    ),
    p_adjust_method = prop_string(
      "holm",
      enum = stats::p.adjust.methods,
      description = "Multiple-testing adjustment method. Adjustment precedes transformation."
    ),
    n_tests = prop_integer(
      NULL,
      nullable = TRUE,
      min = 1L,
      description = "Hypothesis-family size, at least the number of supplied rows. Unset counts all rows, including missing p-values."
    ),
    p_transform = prop_string(
      "neg_log10",
      enum = c("neg_log10", "identity", "one_minus"),
      description = "Named display transform of adjusted p-values."
    ),
    p_thresh = prop_float(
      0.05,
      exclusive_min = 0,
      max = 1,
      description = "Significance threshold on adjusted p-values; the comparison is strict."
    ),
    x_thresh = prop_float(
      0,
      description = "Effect threshold separating low and high significant results."
    ),
    zero_cap = prop_float(
      NULL,
      nullable = TRUE,
      exclusive_min = 0,
      description = "Finite display height for zero p-values under neg_log10; must exceed all finite heights and the significance reference. Unset computes a cap one unit above their rounded-up maximum."
    ),
    annotate_n = prop_integer(
      7L,
      min = 0L,
      description = "Maximum significant feature labels per side in the volcano view, ranked by raw p-value with input-order ties."
    ),
    reference = prop_boolean(
      TRUE,
      description = "Show the transformed significance threshold and, for volcanoes, the effect threshold."
    ),
    legend = prop_boolean(
      TRUE,
      description = "Show the nonempty significance groups."
    ),
    negative_color = prop_string(
      "#BE2E5F",
      description = "Color for significant effects below x_thresh."
    ),
    neutral_color = prop_string(
      "#808080",
      description = "Color for other results, including effects equal to x_thresh."
    ),
    positive_color = prop_string(
      "#0F6A66",
      description = "Color for significant effects above x_thresh."
    ),
    alpha = prop_float(0.8, min = 0, max = 1, description = "Mark opacity."),
    point_size = prop_float(
      8,
      exclusive_min = 0,
      description = "Scatter symbol size in pixels."
    ),
    xlab = prop_string(NULL, nullable = TRUE, description = "X-axis label."),
    ylab = prop_string(NULL, nullable = TRUE, description = "Y-axis label."),
    xlim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Exactly two increasing finite volcano x-axis limits. Not used by the categorical view."
    ),
    ylim = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      min_items = 2L,
      description = "Exactly two increasing finite y-axis limits."
    ),
    margin_top = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Top plot margin in pixels."
    ),
    margin_right = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Right plot margin in pixels."
    ),
    margin_bottom = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Bottom plot margin in pixels."
    ),
    margin_left = prop_integer(
      NULL,
      nullable = TRUE,
      min = 0L,
      description = "Left plot margin in pixels."
    )
  ),
  validator = function(self) {
    errors <- character()
    for (name in c("p_thresh", "x_thresh", "zero_cap", "alpha", "point_size")) {
      value <- prop(self, name)
      if (!is.null(value) && any(!is.finite(value))) {
        errors <- c(errors, paste0("@", name, " must be finite"))
      }
    }
    for (name in c("xlim", "ylim")) {
      value <- prop(self, name)
      if (
        !is.null(value) &&
          (length(value) != 2L ||
            any(!is.finite(value)) ||
            value[[1L]] >= value[[2L]])
      ) {
        errors <- c(
          errors,
          paste0("@", name, " must contain two increasing finite limits")
        )
      }
    }
    if (self@view == "manhattan" && !is.null(self@xlim)) {
      errors <- c(errors, "@xlim is only available for the volcano view")
    }
    if (self@p_transform != "neg_log10" && !is.null(self@zero_cap)) {
      errors <- c(errors, "@zero_cap is only available with neg_log10")
    }
    if (length(errors)) errors else NULL
  }
)

SIGNIFICANCE_ORIGIN_NAMES <- setdiff(
  names(SignificanceConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a Significance Plot Configuration
#'
#' Build a validated configuration with provenance for authored settings.
#' @inheritParams SignificanceConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [SignificanceConfig] object.
#' @export
#' @examples
#' setup_SignificanceConfig(view = "manhattan", p_adjust_method = "BH")
setup_SignificanceConfig <- function(
  view = "volcano",
  estimate = "estimate",
  p_value = "p_value",
  label = NULL,
  p_adjust_method = "holm",
  n_tests = NULL,
  p_transform = "neg_log10",
  p_thresh = 0.05,
  x_thresh = 0,
  zero_cap = NULL,
  annotate_n = 7L,
  reference = TRUE,
  legend = TRUE,
  negative_color = "#BE2E5F",
  neutral_color = "#808080",
  positive_color = "#0F6A66",
  alpha = 0.8,
  point_size = 8,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), SIGNIFICANCE_ORIGIN_NAMES)
  SignificanceConfig(
    view = view,
    estimate = estimate,
    p_value = p_value,
    label = label,
    p_adjust_method = p_adjust_method,
    n_tests = if (is.null(n_tests)) NULL else clean_int(n_tests),
    p_transform = p_transform,
    p_thresh = p_thresh,
    x_thresh = x_thresh,
    zero_cap = zero_cap,
    annotate_n = clean_int(annotate_n),
    reference = reference,
    legend = legend,
    negative_color = negative_color,
    neutral_color = neutral_color,
    positive_color = positive_color,
    alpha = alpha,
    point_size = point_size,
    xlab = xlab,
    ylab = ylab,
    xlim = xlim,
    ylim = ylim,
    margin_top = if (is.null(margin_top)) NULL else clean_int(margin_top),
    margin_right = if (is.null(margin_right)) NULL else clean_int(margin_right),
    margin_bottom = if (is.null(margin_bottom)) {
      NULL
    } else {
      clean_int(margin_bottom)
    },
    margin_left = if (is.null(margin_left)) NULL else clean_int(margin_left),
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

# Resolve inexpensive metadata here. Data adjustment and mark preparation run
# once in compile(), which always starts from the unadjusted p-value column.
method(resolve, SignificanceConfig) <- function(config, data = NULL, ...) {
  label <- switch(
    config@p_transform,
    neg_log10 = "-log10(p-value)",
    identity = "p-value",
    one_minus = "1 - p-value"
  )
  if (config@p_adjust_method != "none") {
    label <- paste0(label, " (", config@p_adjust_method, "-adjusted)")
  }
  config_derive(
    config,
    list(
      n_tests = if (!is.null(data)) {
        as.integer(length(config_column(data, config@p_value, "p_value")))
      } else {
        NULL
      },
      xlab = if (config@view == "volcano") "Coefficient" else "Outcome",
      ylab = label
    )
  )
}

method(compile, SignificanceConfig) <- function(config, data = NULL, ...) {
  check_dots_empty(...)
  significance_option(config, data)
}

# Serialize the semantic configuration; compile() produces the ECharts option.
method(to_list, SignificanceConfig) <- function(x) {
  chart_config_to_list(x)
}
