export type {
  HistogramAppearance,
  HistogramBinStat,
  HistogramChartConfig,
  HistogramData,
  HistogramNormalization,
  HistogramSample,
} from "./config.ts";
export {
  buildHistogramOption,
  HISTOGRAM_CHART_CONFIG_MEMBERS,
  histogramValueLabel,
} from "./config.ts";
export type { BreaksRule, HistogramSummary } from "./edges.ts";
export { classCount, histogramEdges, pretty } from "./edges.ts";
