/**
 * The fit phase: the layout that depends on the surface and on measured
 * content, computed after `setOption` and after every resize, in the browser
 * and in Node. `fit()` is the one entry point hosts call; it runs its steps in
 * a fixed order, each a no-op unless the option or the context calls for it,
 * and each derived from the authored option, so repeated passes do not
 * accumulate. spec: draw/chart-appearance#build-and-fit
 */
import { fitHeatmap, type HeatmapHints } from "../charts/heatmap/config.ts";
import { fitKeys, fitLegend, type KeyPlacement, keyState, placeColorScale } from "./keys.ts";

/** The ECharts module, for text measurement. */
export type FitECharts = Parameters<typeof fitKeys>[0];

/** The ECharts instance a fit measures and updates. */
export type FitChart = Parameters<typeof fitKeys>[1];

/** The build-time layout hints of a chart, by the step that reads them. */
export interface FitHints {
  /** A heatmap's cell counts and margins, from `heatmapHints()`. */
  readonly heatmap?: HeatmapHints;
}

/** What `fit()` needs from its host. */
export interface FitContext {
  /** The chart type of the build request (`chart/<type>/v1`); unset for an option no builder made. */
  readonly type?: string | null;
  /** The option the host set, before any fit: every step starts from it. */
  readonly option: Record<string, unknown>;
  /** The build-time layout hints. */
  readonly hints?: FitHints;
  /** Where the keys of a scatter chart sit; unset, right of the plot. */
  readonly placement?: KeyPlacement | null;
}

/** What a fit leaves: `state` changes when a later setOption or resize moves what the fit placed. */
export interface FitResult {
  readonly state: string;
}

/**
 * Events after which a host fits again: a legend toggle or scroll changes a
 * centered legend's width, and zoom and restore change the axis extents.
 */
export const FIT_EVENTS = Object.freeze([
  "legendselectchanged",
  "legendscroll",
  "datazoom",
  "restore",
] as const);

/** Chart types whose builder lays out their own legend and grid. */
const OWN_LAYOUT = new Set(["a3"]);

/**
 * Fits a drawn chart to its surface: square heatmap cells, the scatter
 * chart's color scale and size key or any other chart's vertical color
 * scale, and the legend row aligned with the plot area. Call after
 * `setOption` and after a resize, outside ECharts' own render pass.
 */
export function fit(echarts: FitECharts, chart: FitChart, context: FitContext): FitResult {
  const { option, type } = context;
  if (!(type && OWN_LAYOUT.has(type))) {
    const heatmap = context.hints?.heatmap;
    if (heatmap) fitHeatmap(chart, option, heatmap);
    if (type === "scatter") {
      fitKeys(echarts, chart, {
        option,
        ...(context.placement ? { placement: context.placement } : {}),
      });
    } else {
      placeColorScale(chart, { option });
    }
    fitLegend(chart, { option });
  }
  return { state: fitState(chart) };
}

/**
 * The geometry a fit places, as a string: equal before and after a pass
 * means nothing moved it since. Hosts compare it to skip a redundant fit.
 */
export function fitState(chart: FitChart): string {
  return keyState(chart);
}
