/**
 * ECharts option builders shared by rtemis.draw and rtemislive. Each builder
 * takes a chart's data and its `chart/<type>/v1` config and returns the
 * ECharts option.
 *
 * spec: draw/shared-builders#package
 *
 * @module
 */

import { buildA3Option } from "./charts/a3/index.ts";
import { buildBarOption } from "./charts/bar/index.ts";
import { buildBoxplotOption } from "./charts/boxplot/index.ts";
import { buildConfusionOption } from "./charts/confusion/index.ts";
import { buildPrOption, buildRocOption, curveAspect } from "./charts/curves/index.ts";
import { buildDensityOption } from "./charts/density/index.ts";
import { buildHistogramOption } from "./charts/histogram/index.ts";
import { buildLineOption, lineAspect } from "./charts/line/index.ts";
import { buildScatterOption, scatterAspect } from "./charts/scatter/index.ts";

export type {
  A3Appearance,
  A3ChartConfig,
  A3EChartsOption,
  A3PtmPlacement,
  A3SurfaceHints,
} from "./charts/a3/index.ts";
export { A3_CHART_CONFIG_MEMBERS, a3SurfaceHints, buildA3Option } from "./charts/a3/index.ts";
export type { BarAppearance, BarChartConfig } from "./charts/bar/index.ts";
export { BAR_CHART_CONFIG_MEMBERS, buildBarOption } from "./charts/bar/index.ts";
export type {
  BoxplotAppearance,
  BoxplotCell,
  BoxplotChartConfig,
  BoxplotComparison,
  BoxplotData,
  BoxplotObservation,
} from "./charts/boxplot/index.ts";
export {
  BOXPLOT_CHART_CONFIG_MEMBERS,
  buildBoxplotOption,
  radicalInverse,
} from "./charts/boxplot/index.ts";
export type {
  ConfusionAppearance,
  ConfusionChartConfig,
  ConfusionData,
  ConfusionLayout,
  ConfusionPanel,
  ConfusionPayload,
  ConfusionRates,
} from "./charts/confusion/index.ts";
export {
  buildConfusionOption,
  CONFUSION_CHART_CONFIG_MEMBERS,
  confusionHeight,
  confusionLayout,
  confusionRates,
  fitConfusion,
} from "./charts/confusion/index.ts";
export type {
  Curve,
  CurveAppearance,
  CurveData,
  CurveGroup,
  PrChartConfig,
  RocChartConfig,
} from "./charts/curves/index.ts";
export {
  buildPrOption,
  buildRocOption,
  curveAspect,
  curveLabel,
  curveLegendTitle,
  PR_CHART_CONFIG_MEMBERS,
  ROC_CHART_CONFIG_MEMBERS,
} from "./charts/curves/index.ts";
export type {
  BandwidthRule,
  DensityAppearance,
  DensityChartConfig,
  DensityData,
  DensityKernel,
  DensityOptions,
  DensitySample,
  DensitySummary,
} from "./charts/density/index.ts";
export {
  buildDensityOption,
  DENSITY_CHART_CONFIG_MEMBERS,
  DENSITY_VALUE_LABEL,
  kernelDensity,
  ruleBandwidth,
} from "./charts/density/index.ts";
export type {
  BreaksRule,
  HistogramAppearance,
  HistogramBinStat,
  HistogramChartConfig,
  HistogramData,
  HistogramNormalization,
  HistogramSample,
  HistogramSummary,
} from "./charts/histogram/index.ts";
export {
  buildHistogramOption,
  classCount,
  HISTOGRAM_CHART_CONFIG_MEMBERS,
  histogramEdges,
  histogramValueLabel,
  pretty,
} from "./charts/histogram/index.ts";
export type {
  AspectHint,
  LineAppearance,
  LineChartConfig,
  LineDash,
  LineData,
} from "./charts/line/index.ts";
export {
  buildLineOption,
  LINE_CHART_CONFIG_MEMBERS,
  lineAspect,
} from "./charts/line/index.ts";
export type { ScatterAppearance, ScatterChartConfig, ScatterData } from "./charts/scatter/index.ts";
export {
  autoAlpha,
  buildScatterOption,
  DEFAULT_POINT_SIZE,
  MAX_SYMBOL_SIZE,
  SCATTER_CHART_CONFIG_MEMBERS,
  scatterAspect,
} from "./charts/scatter/index.ts";
export type { KeyContext, KeyPlacement, KeyReserve, SizeKeySpec } from "./layout/keys.ts";
export {
  fitKeys,
  fitLegend,
  keyPlacement,
  keyState,
  SIZE_KEY_ID,
  sizeKeyValues,
} from "./layout/keys.ts";
export type { FitCurve, FitMembers } from "./shared/fit.ts";
export { FIT_MEMBERS, fitLabel } from "./shared/fit.ts";
export { ddSci, formatNumber, SIGNIFICANT_DIGITS } from "./shared/format.ts";
export type { MeasureMembers } from "./shared/labels.ts";
export { valueLabel } from "./shared/labels.ts";
export type { Layer } from "./shared/layers.ts";
export { appendLayers } from "./shared/layers.ts";
export type { MarginMembers } from "./shared/layout.ts";
export { DEFAULT_MARGIN, ZOOM_SLIDER_ID } from "./shared/layout.ts";
export type { LegendMembers } from "./shared/legend.ts";
export type { AxisLimits } from "./shared/limits.ts";
export { calcLimits, calcLimitsFromZero, DEFAULT_PAD } from "./shared/limits.ts";
export { RENDERER_NAMES, registerRenderers } from "./shared/renderers.ts";
export type { ColumnarSource, ColumnNames, ColumnType, TabularData } from "./shared/source.ts";
export { timeLabel, timeMs } from "./shared/source.ts";
export type { ColorScheme, ThemeTokens } from "./theme/tokens.ts";
export {
  DEFAULT_FONT_FAMILY,
  darkThemeTokens,
  fontStack,
  lightThemeTokens,
  RTEMIS_PALETTE,
  SIGN_COLORS,
  themeFontSizes,
  themeOption,
} from "./theme/tokens.ts";

/** The builder of each chart type, by `chart/<type>/v1` type name. */
export const builders = Object.freeze({
  a3: buildA3Option,
  bar: buildBarOption,
  boxplot: buildBoxplotOption,
  confusion: buildConfusionOption,
  density: buildDensityOption,
  histogram: buildHistogramOption,
  line: buildLineOption,
  pr: buildPrOption,
  roc: buildRocOption,
  scatter: buildScatterOption,
});

/**
 * The fixed-aspect plotting box of a built option, by chart type, for a host
 * that sizes its surface before drawing.
 */
export const aspectHints = Object.freeze({
  line: lineAspect,
  pr: curveAspect,
  roc: curveAspect,
  scatter: scatterAspect,
});

/** A chart type with a builder in this package. */
export type ChartType = keyof typeof builders;
