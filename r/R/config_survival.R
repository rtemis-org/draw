# config_survival.R
# ::rtemis.draw::

#' Survival Curve Configuration
#'
#' Draw precomputed survival estimates, without fitting a model in the renderer.
#' Corresponds to `LineSeriesOption` in `src/chart/line/LineSeries.ts` and
#' `ScatterSeriesOption` in `src/chart/scatter/ScatterSeries.ts`.
#' ECharts docs: <https://echarts.apache.org/en/option.html#series-line.step>
#'
#' @section Records and statistical semantics:
#' Supply a curve data frame, or a list with `curves` and optional `risk` data
#' frames. Each curve row contains time and survival probability, with optional
#' group, pointwise lower/upper confidence bounds, censor counts, and risk counts.
#' Include the initial time/probability explicitly: the renderer never invents
#' an origin. Records are sorted within groups; times must be unique and finite,
#' probabilities nonincreasing in `[0, 1]`. Curves are right-continuous and stop at
#' their last recorded time. Missing confidence bounds produce gaps, not zeros.
#' A censor tick represents one or more censored observations at that time;
#' its tooltip retains the supplied count. Counts may be fractional for weights.
#' Median survival is the first crossing of 0.5; a plateau at 0.5 uses its
#' midpoint, including the last follow-up time if the plateau is terminal.
#' Comparisons to 0.5 use tolerance `sqrt(.Machine$double.eps)`, as in survival.
#' Landmarks outside a group's observed time range are omitted without
#' extrapolation. Display precision never rounds plotted coordinates.
#'
#' Risk records have fixed columns `time`, `group`, and `n_risk`, and state the
#' risk count immediately before the specified time. They must cover the same
#' unique times in each group. Unknown counts use NA and display as `NA`.
#' The renderer does not interpolate or infer risk sets from the curves.
#' All layers belonging to a group share its legend toggle, including risk rows.
#'
#' @param time Character: Time column in the curve records.
#' @param survival Character: Survival probability column.
#' @param group Optional Character: Curve group column.
#' @param lower,upper Optional Character: Pointwise confidence bound columns.
#' @param n_censor Optional Character: Censor count column.
#' @param n_risk Optional Character: Risk count column at recorded times.
#' @param show_ci Logical: Draw available pointwise confidence bands.
#' @param show_censors Logical: Draw censor ticks.
#' @param show_median Logical: Draw median survival droplines when estimable.
#' @param landmarks Optional Numeric vector: Finite times for survival labels.
#' @param risk_table Logical: Draw supplied risk records beneath the curves.
#' @param ci_opacity Numeric `[0, 1]`: Confidence band opacity.
#' @param censor_size Numeric `(0, Inf)`: Censor tick height in pixels.
#' @inheritParams ROCConfig
#' @inheritParams ChartConfig
#' @return A `SurvivalConfig` object.
#' @export
#' @examples
#' cfg <- setup_SurvivalConfig()
#' draw(cfg, data = data.frame(time = c(0, 1, 2), survival = c(1, .8, .5)))
SurvivalConfig <- new_class(
  name = "SurvivalConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = c(
    legend_properties(),
    list(
      type = prop_chart_type("survival"),
      time = prop_string(
        "time",
        description = "Time column in the curve records."
      ),
      survival = prop_string(
        "survival",
        description = "Survival probability column."
      ),
      group = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional curve group column."
      ),
      lower = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional lower confidence bound column."
      ),
      upper = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional upper confidence bound column."
      ),
      n_censor = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional censor count column."
      ),
      n_risk = prop_string(
        NULL,
        nullable = TRUE,
        description = "Optional risk count column at recorded times."
      ),
      show_ci = prop_boolean(
        TRUE,
        description = "Draw available pointwise confidence bands."
      ),
      show_censors = prop_boolean(
        TRUE,
        description = "Draw ticks at recorded censoring times."
      ),
      show_median = prop_boolean(
        FALSE,
        description = "Draw the median survival dropline when estimable."
      ),
      landmarks = prop_float(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Optional times for labeled survival landmarks."
      ),
      risk_table = prop_boolean(
        FALSE,
        description = "Draw supplied risk records beneath the curve."
      ),
      ci_opacity = prop_float(
        .15,
        min = 0,
        max = 1,
        description = "Confidence band opacity."
      ),
      line_width = prop_float(
        2,
        exclusive_min = 0,
        description = "Survival curve stroke width in pixels."
      ),
      censor_size = prop_float(
        8,
        exclusive_min = 0,
        description = "Censor tick height in pixels."
      ),
      digits = prop_integer(
        3L,
        min = 0L,
        max = 8L,
        description = "Decimal places for displayed probabilities."
      ),
      palette = prop_string(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        description = "Group colors; unset uses the chart theme."
      ),
      legend = prop_boolean(TRUE, description = "Show curve labels."),
      xlab = prop_string("Time", description = "Horizontal axis label."),
      ylab = prop_string(
        "Survival probability",
        description = "Vertical axis label."
      )
    )
  ),
  validator = config_validator(list(
    list(
      schema = list(
        `if` = list(
          required = list("lower"),
          properties = list(lower = list(not = list(type = "null")))
        ),
        then = list(
          properties = list(upper = list(not = list(type = "null")))
        )
      ),
      message = "Bind both lower and upper confidence columns, or neither."
    ),
    list(
      schema = list(
        `if` = list(
          required = list("upper"),
          properties = list(upper = list(not = list(type = "null")))
        ),
        then = list(
          properties = list(lower = list(not = list(type = "null")))
        )
      ),
      message = "Bind both lower and upper confidence columns, or neither."
    )
  ))
)

SURVIVAL_ORIGIN_NAMES <- setdiff(
  names(SurvivalConfig@properties),
  c("type", PROVENANCE_PROPS)
)

#' Set Up a Survival Curve Configuration
#' @inheritParams SurvivalConfig
#' @param origin Optional Named character: Per-property provenance.
#' @param writer Optional Named character: Interface name and version.
#' @return A [SurvivalConfig] object.
#' @export
#' @examples
#' setup_SurvivalConfig(group = "treatment", show_median = TRUE)
setup_SurvivalConfig <- function(
  time = "time",
  survival = "survival",
  group = NULL,
  lower = NULL,
  upper = NULL,
  n_censor = NULL,
  n_risk = NULL,
  show_ci = TRUE,
  show_censors = TRUE,
  show_median = FALSE,
  landmarks = NULL,
  risk_table = FALSE,
  ci_opacity = .15,
  line_width = 2,
  censor_size = 8,
  digits = 3L,
  palette = NULL,
  legend = TRUE,
  xlab = "Time",
  ylab = "Survival probability",
  legend_position = "top",
  legend_placement = "outside",
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), SURVIVAL_ORIGIN_NAMES)
  SurvivalConfig(
    time = time,
    survival = survival,
    group = group,
    lower = lower,
    upper = upper,
    n_censor = n_censor,
    n_risk = n_risk,
    show_ci = show_ci,
    show_censors = show_censors,
    show_median = show_median,
    landmarks = landmarks,
    risk_table = risk_table,
    ci_opacity = ci_opacity,
    line_width = line_width,
    censor_size = censor_size,
    digits = clean_int(digits),
    palette = palette,
    legend = legend,
    xlab = xlab,
    ylab = ylab,
    legend_position = legend_position,
    legend_placement = legend_placement,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, SurvivalConfig) <- function(config, data = NULL, ...) config
method(to_list, SurvivalConfig) <- function(x) chart_config_to_list(x)
method(compile, SurvivalConfig) <- function(config, data = NULL, ...) {
  check_dots_empty(...)
  survival_option(config, data)
}
