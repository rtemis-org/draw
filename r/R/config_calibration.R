# config_calibration.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

#' Calibration Plot Configuration
#'
#' Bind binary observation records to a reliability diagram. Each row contains
#' an observed outcome (zero or one) and its predicted probability, optionally
#' grouped by sample or model. Configuration carries column names, not data.
#'
#' Compiles to `LineSeriesOption` in `src/chart/line/LineSeries.ts` and
#' `ScatterSeriesOption` in `src/chart/scatter/ScatterSeries.ts`.
#' ECharts docs: <https://echarts.apache.org/en/option.html#series-line>
#'
#' @section Statistical semantics:
#' Bins are computed independently within each group. Equidistant bins partition
#' the unit interval. Quantile bins use linear interpolation at positions
#' `(n - 1) * p + 1` in the sorted probabilities (R quantile type 7).
#' Repeated boundaries are collapsed without splitting tied scores. Intervals
#' include their lower boundary and exclude their upper boundary, except that
#' the final interval includes its upper boundary. Constant probabilities form
#' one bin. Empty bins are omitted; lines join the remaining bin means.
#' Each point is the mean predicted probability and mean observed outcome in
#' its bin. The Brier score is the mean squared probability error across all
#' complete observations, before binning. It is not an average of bin errors.
#' Missing pairs are removed together when `na_rm` is true; invalid finite
#' ranges and infinite values are always rejected. A group with no complete
#' observations is rejected. The probability rug uses those same complete
#' observations and sits just inside the lower plotting edge.
#' Both axes span zero to one. Display precision does not round plotted values.
#' A one-bin curve remains visible as a point even in lines-only mode.
#'
#' @param observed Character: Column containing binary outcomes, zero or one.
#' @param probability Character: Column containing predicted probabilities.
#' @param group Optional Character: Column identifying samples or models.
#' @param n_bins Integer `[1, Inf)`: Requested number of bins per group.
#' @param bin_method Character \{"quantile", "equidistant"\}: Binning rule.
#' @param na_rm Logical: Remove incomplete observation/probability pairs.
#' @param mode Character \{"lines", "markers", "lines+markers"\}: Curve display.
#' @param show_brier Logical: Include each sample's Brier score in its legend label.
#' @param rug Logical: Show the distribution of individual probabilities.
#' @param rug_size Numeric `(0, Inf)`: Rug tick height in pixels.
#' @param rug_opacity Numeric `[0, 1]`: Rug tick opacity.
#' @param point_size Numeric `(0, Inf)`: Calibration marker diameter in pixels.
#' @inheritParams ROCConfig
#' @inheritParams ChartConfig
#' @return A `CalibrationConfig` object.
#' @export
#' @examples
#' config <- setup_CalibrationConfig(n_bins = 2L, bin_method = "equidistant")
#' draw(config, data = data.frame(observed = c(0, 1), probability = c(.2, .8)))
CalibrationConfig <- new_class(
  name = "CalibrationConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("calibration"),
      observed = prop_string(
        "observed",
        description = "Column containing binary outcomes, zero or one."
      ),
      probability = prop_string(
        "probability",
        description = "Column containing predicted probabilities."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Column identifying samples or models."
      ),
      n_bins = prop_integer(
        10L,
        min = 1L,
        description = "Requested number of bins per group."
      ),
      bin_method = prop_string(
        "quantile",
        enum = c("quantile", "equidistant"),
        description = "Binning rule: type-7 quantiles with tied boundaries collapsed, or equal-width intervals."
      ),
      na_rm = prop_boolean(
        TRUE,
        description = "Remove incomplete observation and probability pairs."
      ),
      mode = prop_string(
        "lines+markers",
        enum = c("lines", "markers", "lines+markers"),
        description = "Curve display; a single bin always retains a visible marker."
      ),
      show_brier = prop_boolean(
        TRUE,
        description = "Include each sample's Brier score in its legend label."
      ),
      rug = prop_boolean(
        TRUE,
        description = "Show individual probabilities along the lower plotting edge."
      ),
      rug_size = prop_float(
        8,
        exclusive_min = 0,
        description = "Rug tick height in pixels."
      ),
      rug_opacity = prop_float(
        .3,
        min = 0,
        max = 1,
        description = "Rug tick opacity."
      ),
      point_size = prop_float(
        7,
        exclusive_min = 0,
        description = "Calibration marker diameter in pixels."
      ),
      line_width = prop_float(
        2,
        exclusive_min = 0,
        description = "Curve stroke width in pixels."
      ),
      digits = prop_integer(
        3L,
        min = 0L,
        max = 8L,
        description = "Decimal places for displayed probabilities and Brier scores."
      ),
      diagonal = prop_boolean(
        TRUE,
        description = "Show the perfect-calibration diagonal."
      ),
      diagonal_color = prop_string(
        "#888888",
        description = "Perfect-calibration line color."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Group colors; unset uses the chart theme."
      ),
      legend = prop_boolean(
        TRUE,
        description = "Show group labels and optional Brier scores."
      ),
      square = prop_boolean(
        TRUE,
        description = "Keep the plotting grid square."
      ),
      xlab = prop_string(
        "Mean predicted probability",
        description = "Horizontal axis label."
      ),
      ylab = prop_string(
        "Observed proportion",
        description = "Vertical axis label."
      )
    )
  ),
  validator = function(self) {
    if (any(!is.finite(c(self@rug_size, self@point_size, self@line_width)))) {
      "Use finite rug, point, and line sizes."
    } else {
      NULL
    }
  }
)

CALIBRATION_ORIGIN_NAMES <- setdiff(
  names(CalibrationConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a Calibration Plot Configuration
#' @inheritParams CalibrationConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [CalibrationConfig] object.
#' @export
#' @examples
#' setup_CalibrationConfig(group = "model", n_bins = 5L)
setup_CalibrationConfig <- function(
  observed = "observed",
  probability = "probability",
  group = NULL,
  n_bins = 10L,
  bin_method = "quantile",
  na_rm = TRUE,
  mode = "lines+markers",
  show_brier = TRUE,
  rug = TRUE,
  rug_size = 8,
  rug_opacity = .3,
  point_size = 7,
  line_width = 2,
  digits = 3L,
  diagonal = TRUE,
  diagonal_color = "#888888",
  palette = NULL,
  legend = TRUE,
  legend_position = "top",
  legend_placement = "outside",
  square = TRUE,
  xlab = "Mean predicted probability",
  ylab = "Observed proportion",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), CALIBRATION_ORIGIN_NAMES)
  CalibrationConfig(
    observed = observed,
    probability = probability,
    group = group,
    n_bins = clean_int(n_bins),
    bin_method = bin_method,
    na_rm = na_rm,
    mode = mode,
    show_brier = show_brier,
    rug = rug,
    rug_size = rug_size,
    rug_opacity = rug_opacity,
    point_size = point_size,
    line_width = line_width,
    digits = clean_int(digits),
    diagonal = diagonal,
    diagonal_color = diagonal_color,
    palette = palette,
    legend = legend,
    legend_position = legend_position,
    legend_placement = legend_placement,
    square = square,
    xlab = xlab,
    ylab = ylab,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, CalibrationConfig) <- function(config, data = NULL, ...) config
method(to_list, CalibrationConfig) <- function(x) chart_config_to_list(x)
method(compile, CalibrationConfig) <- function(config, data = NULL, ...) {
  check_dots_empty(...)
  calibration_option(config, data)
}
method(render_meta, CalibrationConfig) <- function(config, option) {
  if (!config@square) {
    return(list())
  }
  grid <- option@grid
  list(
    aspect = list(
      ratio = 1,
      leftPx = grid[["left"]],
      rightPx = grid[["right"]],
      topPx = grid[["top"]],
      botPx = grid[["bottom"]]
    )
  )
}
