/**
 * The fit phase: the layout that depends on the surface and on measured
 * content, computed after `setOption` and after every resize, in the browser
 * and in Node. `fit()` is the one entry point hosts call; it runs its steps in
 * a fixed order, each a no-op unless the option or the context calls for it,
 * and each derived from the authored option, so repeated passes do not
 * accumulate. spec: draw/chart-appearance#build-and-fit
 */
import { type A3FitHints, fitA3 } from "../charts/a3/fit.ts";
import { fitGantt } from "../charts/gantt/fit.ts";
import { fitHeatmap, type HeatmapHints } from "../charts/heatmap/config.ts";
import type { AspectHint } from "../charts/line/config.ts";
import { fitPieLabels } from "../charts/pie/fit.ts";
import { fitSankey } from "../charts/sankey/fit.ts";
import { fitAspectAxes } from "./aspect.ts";
import { fitEndpointLabels } from "./axes.ts";
import {
  fitKeys,
  fitLegend,
  type KeyPlacement,
  keyPlacement,
  keyState,
  placeColorScale,
} from "./keys.ts";
import { layoutLegend } from "./legend.ts";
import { centerColorScales, layoutColorScale } from "./scales.ts";

/** The ECharts module, for text measurement. */
export type FitECharts = Parameters<typeof fitKeys>[0];

/** The ECharts instance a fit measures and updates. */
export type FitChart = Parameters<typeof fitKeys>[1];

/** The build-time layout hints of a chart, by the step that reads them. */
export interface FitHints {
  /** A heatmap's cell counts and margins, from `heatmapHints()`. */
  readonly heatmap?: Partial<HeatmapHints> | null;
  /** The fixed-aspect plotting box, from `aspectHints`. */
  readonly aspect?: AspectHint | null;
  /** An A3 diagram's glyph sizes, from `a3SurfaceHints()`, with the host's grid and height flags. */
  readonly a3?: A3FitHints | null;
}

/** A chart's legend members, as its config sets them. */
export interface FitLegendMembers {
  /** An edge or a corner of the plotting area. */
  readonly position?: string | null;
  /** `outside` or `inside` the plotting area. */
  readonly placement?: string | null;
  readonly title?: string | null;
  /** The title sits above a horizontal legend's items rather than before its first row. */
  readonly titleAbove?: boolean | null;
  /** `visualMap` when the members place the color scale rather than a categorical legend. */
  readonly target?: string | null;
}

/** What `fit()` needs from its host. */
export interface FitContext {
  /** The chart type of the build request (`chart/<type>/v1`); unset for an option no builder made. */
  readonly type?: string | null;
  /** The option the host set, before any fit: every step starts from it. */
  readonly option: Record<string, unknown>;
  /** The build-time layout hints. */
  readonly hints?: FitHints;
  /**
   * The legend members. With them, the legend and the color scale take the
   * members' position and placement; without them, the legend stays where
   * the option puts it and its row aligns with the plot area.
   */
  readonly members?: FitLegendMembers | null;
  /** Where the keys of a scatter chart sit, for a host without legend members; unset, right of the plot. */
  readonly placement?: KeyPlacement | null;
  /** The surface's height is fixed; an unbounded surface takes an A3 diagram's height. */
  readonly bounded?: boolean;
}

/**
 * What a fit leaves: `state` changes when a later setOption or resize moves
 * what the fit placed, and `height` is the height the chart took on an
 * unbounded surface.
 */
export interface FitResult {
  readonly state: string;
  readonly height?: number;
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

/** Chart types whose legend and grid only their own fit lays out. */
const OWN_LAYOUT = new Set(["a3"]);

/**
 * Fits a drawn chart to its surface. With legend members: the Gantt chart's
 * labels and legend, an A3 diagram's rail and glyph scale, the fixed-aspect
 * box, square heatmap cells, Sankey labels, the categorical legend at the
 * members' position, edge-aligned pie labels, the scatter chart's keys or the
 * color scale, and value axis end labels. Without them: square heatmap
 * cells, the scatter chart's keys at `placement` or any other chart's
 * vertical color scale, and the legend row aligned with the plot area. Call
 * after `setOption` and after a resize, outside ECharts' own render pass.
 * Without members, an A3 diagram's hints run the A3 fit and nothing else.
 */
export function fit(echarts: FitECharts, chart: FitChart, context: FitContext): FitResult {
  if (context.members) return fitWithMembers(echarts, chart, context, context.members);
  const { option, type } = context;
  const a3 = context.hints?.a3;
  if (a3) {
    // biome-ignore lint/suspicious/noExplicitAny: the A3 fit reads ECharts' untyped model API.
    fitA3(chart as any, { option, hints: a3, bounded: context.bounded ?? true });
  } else if (!(type && OWN_LAYOUT.has(type))) {
    const heatmap = context.hints?.heatmap;
    if (heatmap) fitHeatmap(chart, option, heatmap as HeatmapHints);
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

function fitWithMembers(
  echarts: FitECharts,
  chart: FitChart,
  context: FitContext,
  members: FitLegendMembers,
): FitResult {
  // biome-ignore lint/suspicious/noExplicitAny: the steps read ECharts' untyped model API.
  const instance = chart as any;
  const { option, type } = context;
  const hints = context.hints ?? {};
  const bounded = context.bounded ?? true;
  fitGantt(echarts, instance, { option, legendPosition: members.position ?? null });
  let height: number | undefined;
  if (hints.a3) {
    fitA3(instance, { option, hints: hints.a3, bounded });
    if (!bounded && hints.a3.autoHeight) height = instance.getHeight();
  }
  if (hints.aspect) fitAspectAxes(instance, hints.aspect);
  if (hints.heatmap?.squareCells) fitSquareCells(instance, option, hints.heatmap as HeatmapHints);
  fitSankey(echarts, instance, { option });
  // The end labels change the axis labels' extent, and with it the plotting
  // area the legend aligns with, so they are set before the legend is laid
  // out and again after it changes the axis lengths.
  fitEndpointLabels(instance, { option });
  layoutLegend(echarts, instance, {
    option,
    position: members.position ?? null,
    placement: members.placement ?? null,
    title: members.title ?? null,
    titleAbove: members.titleAbove ?? false,
    target: members.target ?? null,
    aspect: hints.aspect ?? null,
  });
  fitPieLabels(echarts, instance, { option });
  if (type === "scatter") {
    let hasLegend = false;
    instance.getModel().eachComponent("legend", (model: FitModel) => {
      if (model.get("show") && model.getData().length) hasLegend = true;
    });
    const aspect = hints.aspect;
    fitKeys(echarts, chart, {
      option,
      placement: keyPlacement(members.position, members.placement, hasLegend),
      ...(aspect ? { afterReserve: () => fitAspectAxes(instance, aspect) } : {}),
    });
  } else if (members.target === "visualMap") {
    layoutColorScale(echarts, instance, {
      option,
      position: members.position ?? null,
      placement: members.placement ?? null,
      title: members.title ?? null,
      heatmap: hints.heatmap ?? null,
    });
  } else {
    centerColorScales(instance, { option });
  }
  fitEndpointLabels(instance, { option });
  return { state: fitState(chart), ...(height === undefined ? {} : { height }) };
}

interface FitModel {
  get(key: string): unknown;
  getData(): unknown[];
}

/**
 * Square heatmap cells, which fail on hints or a surface that leave no room
 * for the cells: a host with legend members exports what it draws, where a
 * chart without room for its cells would be wrong. Hints without square cells
 * leave the chart as it is.
 */
export function fitSquareCells(
  chart: FitChart & { getWidth(): number; getHeight(): number },
  option: Record<string, unknown>,
  hints: HeatmapHints,
): void {
  if (!hints.squareCells) return;
  const { nRows, nCols, leftPx, rightPx, topPx, botPx } = hints;
  if (
    ![nRows, nCols].every((v) => Number.isInteger(v) && v > 0) ||
    ![leftPx, rightPx, topPx, botPx].every((v) => Number.isFinite(v) && v >= 0)
  ) {
    throw new Error("Supply positive heatmap dimensions and nonnegative pixel margins.");
  }
  const cell = Math.min(
    (chart.getWidth() - leftPx - rightPx) / nCols,
    (chart.getHeight() - topPx - botPx) / nRows,
  );
  if (cell <= 0) {
    throw new Error("Increase figure dimensions to leave room for heatmap cells and margins.");
  }
  fitHeatmap(chart, option, hints);
}

/**
 * The geometry a fit places, as a string: equal before and after a pass
 * means nothing moved it since. Hosts compare it to skip a redundant fit.
 */
export function fitState(chart: FitChart): string {
  return keyState(chart);
}
