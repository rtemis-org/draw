# config_confusion.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Confusion Plot Configuration
#'
#' Portable confusion counts with reference rows and predicted columns.
#' Data consists of long records with class labels and a frequency, optionally
#' grouped into panels. Missing class labels represent omitted pairs; absent
#' class combinations have zero frequency. All panels use the same class order.
#' Compiles to `HeatmapSeriesOption` in `src/chart/heatmap/HeatmapSeries.ts`,
#' `GridOption` in `src/coord/cartesian/GridModel.ts`, and continuous visual maps.
#' `to_list()` emits semantic config keys; `compile()` emits ECharts options.
#' `draw()` fits square count cells with aligned marginal summaries to the
#' available canvas. Missing-pair omissions are reported in the console.
#' The cell counts show included observations without a separate sample-size label. Color fades and
#' marginal backgrounds follow the active theme unless explicitly overridden.
#'
#' @section Statistical semantics:
#' Color intensity is the fraction within each reference row; labels show raw
#' counts. Sensitivity, specificity, PPV, and NPV are one-versus-rest rates for
#' each named class. Balanced accuracy is mean class recall, including every
#' declared class. Zero denominators produce NA, never zero; consequently
#' balanced accuracy is NA if any declared class has no reference observations.
#' Summaries of pooled resamples describe pooled predictions, not mean fold
#' performance. Repeated observations are counted each time they appear.
#'
#' @param reference Character: Column containing reference class labels.
#' @param predicted Character: Column containing predicted class labels.
#' @param count Character: Column containing nonnegative integer frequencies.
#' @param panel Optional Character: Optional panel-label column; unset draws one matrix.
#' @param classes Optional Character vector: Ordered class labels shared by every panel; unset preserves first appearance.
#' @param show_metrics Logical: Show per-class rates, accuracy, and balanced accuracy.
#' @param digits Integer: Decimal places for rates in cells and hover text.
#' @param ncol Integer: Maximum number of panels per row.
#' @param correct_color Character: Six-digit hex color at unit fraction for correct predictions.
#' @param incorrect_color Character: Six-digit hex color at unit fraction for incorrect predictions.
#' @param low_color Optional Character: Six-digit hex color at zero row fraction;
#'   unset uses the active theme background.
#' @param summary_color Optional Character: Six-digit hex background for metric
#'   cells; unset uses a faint neutral tint of the active theme background.
#' @param font_size Numeric: Cell-label font size in pixels.
#' @param xlab Character: Predicted-class axis label.
#' @param ylab Character: Reference-class axis label.
#' @inheritParams ChartConfig
#' @return A `ConfusionConfig` object.
#' @export
#' @examples
#' cfg <- setup_ConfusionConfig()
#' draw(cfg, data = data.frame(reference = c("yes", "no"),
#'   predicted = c("yes", "no"), n = c(8, 12)))
ConfusionConfig <- new_class(
  name = "ConfusionConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("confusion"),
    reference = prop_string(
      "reference",
      description = "Column containing reference class labels."
    ),
    predicted = prop_string(
      "predicted",
      description = "Column containing predicted class labels."
    ),
    count = prop_string(
      "n",
      description = "Column containing nonnegative integer frequencies."
    ),
    panel = prop_string(
      NULL,
      nullable = TRUE,
      description = "Optional panel-label column; unset draws one matrix."
    ),
    classes = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      unique_items = TRUE,
      description = "Ordered class labels shared by every panel; unset preserves first appearance."
    ),
    show_metrics = prop_boolean(
      TRUE,
      description = "Show per-class rates, accuracy, and balanced accuracy."
    ),
    digits = prop_integer(
      2L,
      min = 0L,
      max = 8L,
      description = "Decimal places for rates in cells and hover text."
    ),
    ncol = prop_integer(
      2L,
      min = 1L,
      description = "Maximum number of panels per row."
    ),
    correct_color = prop_string(
      "#0F6A66",
      description = "Six-digit hex color at unit fraction for correct predictions."
    ),
    incorrect_color = prop_string(
      "#BE2E5F",
      description = "Six-digit hex color at unit fraction for incorrect predictions."
    ),
    low_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Six-digit hex color at zero row fraction; unset uses the active theme background."
    ),
    summary_color = prop_string(
      NULL,
      nullable = TRUE,
      description = "Six-digit hex background for metric cells; unset uses a faint neutral theme tint."
    ),
    font_size = prop_float(
      12,
      exclusive_min = 0,
      description = "Cell-label font size in pixels."
    ),
    xlab = prop_string(
      "Predicted",
      description = "Predicted-class axis label."
    ),
    ylab = prop_string("Reference", description = "Reference-class axis label.")
  ),
  validator = function(self) {
    errors <- character()
    for (name in c(
      "correct_color",
      "incorrect_color",
      "low_color",
      "summary_color"
    )) {
      if (
        !is.null(prop(self, name)) &&
          !grepl("^#[0-9A-Fa-f]{6}$", prop(self, name))
      ) {
        errors <- c(errors, paste0("@", name, " must be a six-digit hex color"))
      }
    }
    if (!is.finite(self@font_size)) {
      errors <- c(errors, "@font_size must be finite")
    }
    if (length(errors)) errors else NULL
  }
)

CONFUSION_ORIGIN_NAMES <- setdiff(
  names(ConfusionConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a Confusion Plot Configuration
#' @inheritParams ConfusionConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [ConfusionConfig] object.
#' @export
#' @examples
#' setup_ConfusionConfig(panel = "split", classes = c("yes", "no"))
setup_ConfusionConfig <- function(
  reference = "reference",
  predicted = "predicted",
  count = "n",
  panel = NULL,
  classes = NULL,
  show_metrics = TRUE,
  digits = 2L,
  ncol = 2L,
  correct_color = "#0F6A66",
  incorrect_color = "#BE2E5F",
  low_color = NULL,
  summary_color = NULL,
  font_size = 12,
  xlab = "Predicted",
  ylab = "Reference",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), CONFUSION_ORIGIN_NAMES)
  ConfusionConfig(
    reference = reference,
    predicted = predicted,
    count = count,
    panel = panel,
    classes = classes,
    show_metrics = show_metrics,
    digits = clean_int(digits),
    ncol = clean_int(ncol),
    correct_color = correct_color,
    incorrect_color = incorrect_color,
    low_color = low_color,
    summary_color = summary_color,
    font_size = font_size,
    xlab = xlab,
    ylab = ylab,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, ConfusionConfig) <- function(config, data = NULL, ...) {
  if (is.null(data)) {
    return(config)
  }
  reference <- config_column(data, config@reference, "reference")
  predicted <- config_column(data, config@predicted, "predicted")
  labels <- unique(c(as.character(reference), as.character(predicted)))
  labels <- labels[!is.na(labels)]
  if (!length(labels) && is.null(config@classes)) {
    abort(
      "Supply at least one class label, or set `classes` explicitly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  config_derive(config, list(classes = labels))
}

method(compile, ConfusionConfig) <- function(config, data = NULL, ...) {
  check_dots_empty(...)
  confusion_option(config, data)
}

method(to_list, ConfusionConfig) <- function(x) chart_config_to_list(x)


#' Carry theme and square-cell constraints to the shared render layout
#' @inheritParams render_meta
#' @return List of portable confusion rendering hints.
#' @keywords internal
#' @noRd
method(render_meta, ConfusionConfig) <- function(config, option) {
  list(
    confusion = drop_nulls(list(
      ncol = config@ncol,
      metrics = config@show_metrics,
      fontSize = config@font_size,
      lowColor = config@low_color,
      summaryColor = config@summary_color
    ))
  )
}
