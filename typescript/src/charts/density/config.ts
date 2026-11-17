import { formatNumber } from "../../shared/format.ts";
import { axisZoom, cartesianGrid, type MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { ridgeOption } from "../../shared/ridge.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import type { BandwidthRule, DensityKernel } from "./kde.ts";

/**
 * An instance of the `chart/density/v1` schema, written by rtemis.draw's
 * `DensityConfig` class. Every member is optional. The members that decide
 * the curves (`x`, `group`, `order`, `na_rm`, `n`, `bw`, `bandwidth`,
 * `kernel`, `adjust`) are applied by the host, which computes
 * {@link DensityData}.
 * spec: draw/shared-builders#density
 */
export interface DensityChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "density";
  /** Columns to estimate; each is a sample, split by `group`. */
  readonly x?: ColumnNames | null;
  /** Column whose levels split each `x` column into samples. */
  readonly group?: string | null;
  /** Samples overlaid, or one row each on a shared x range. Default `"overlap"`. */
  readonly mode?: "overlap" | "ridge";
  /** Sample order: input, or decreasing mean or median. Default `"input"`. */
  readonly order?: "input" | "mean" | "median";
  /** Zoom and pan the x axis. */
  readonly zoom?: boolean;
  /** Points at which each density is estimated. */
  readonly n?: number;
  /** Bandwidth rule, used when `bandwidth` is unset. */
  readonly bw?: BandwidthRule;
  /** Bandwidth in measurement units. */
  readonly bandwidth?: number | null;
  /** Smoothing kernel. */
  readonly kernel?: DensityKernel;
  /** Bandwidth multiplier. */
  readonly adjust?: number;
  /** Fill opacity under each curve. Default 0.25. */
  readonly fill_alpha?: number;
  /** Drop missing values before estimating. */
  readonly na_rm?: boolean;
  /** Series colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Bottom axis name; unset or empty draws none. */
  readonly xlab?: string | null;
  /** Left axis name; unset or empty draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** Every member of {@link DensityChartConfig}, checked against `chart/density/v1`. */
export const DENSITY_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "group",
  "mode",
  "order",
  "zoom",
  "n",
  "bw",
  "bandwidth",
  "kernel",
  "adjust",
  "fill_alpha",
  "na_rm",
  "palette",
  "xlab",
  "ylab",
  "title",
  "margin_top",
  "margin_right",
  "margin_bottom",
  "margin_left",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof DensityChartConfig)[]);

type MissingMember = Exclude<
  keyof DensityChartConfig,
  (typeof DENSITY_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/** The density curve of one sample: one `x` column, or one column within one group level. */
export interface DensitySample {
  /** The sample's name; unset for the only sample of an ungrouped column. */
  readonly name?: string | null;
  /** Number of values estimated from. */
  readonly n: number;
  /** Evaluation points and the density there; empty for an empty sample. */
  readonly x: readonly number[];
  readonly y: readonly number[];
}

/**
 * A density chart's data: the curve of each sample, in drawing order,
 * computed by the host as the config's estimation members say. rtemis.draw
 * estimates with `stats::density()`, rtemislive through `kernelDensity()`
 * from bins computed in DuckDB.
 */
export interface DensityData {
  readonly samples: readonly DensitySample[];
}

/** What is not chart config: the theme palette and animation. */
export interface DensityAppearance {
  /** The theme's categorical palette; unset uses the theme's. */
  readonly palette?: readonly string[];
  /** Animate the first render; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** The value axis name a host derives for a density chart when the author sets none. */
export const DENSITY_VALUE_LABEL = "Density";

/**
 * Build the ECharts option of a density chart from host-computed curves and
 * its `chart/density/v1` config: one line with a filled area per sample on a
 * numeric x axis, overlaid, or in ridge rows on a shared x range (`mode`).
 */
export function buildDensityOption(
  data: DensityData,
  config: DensityChartConfig,
  appearance: DensityAppearance = {},
): Record<string, unknown> {
  const samples = data.samples;
  if (samples.length === 0) {
    throw new Error("A density chart needs `x` set to one or more columns to draw.");
  }
  for (const s of samples) {
    if (s.x.length !== s.y.length) {
      throw new Error(`Sample '${s.name ?? "x"}' needs one density per evaluation point.`);
    }
  }
  const names = samples.map((s, i) => s.name ?? `Series ${i + 1}`);
  const series = samples.map((s, i) => ({
    type: "line",
    name: names[i],
    showSymbol: false,
    areaStyle: { opacity: config.fill_alpha ?? 0.25 },
    data: s.x.map((x, j) => [x, s.y[j]]),
  }));
  const xs = samples.flatMap((s) => s.x);
  const ys = samples.flatMap((s) => s.y);
  const palette = asArray(config.palette);
  const colors = palette.length > 0 ? palette : appearance.palette;
  const xlab = config.xlab || null;
  const ylab = config.ylab || null;
  const common = {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: { trigger: "axis", valueFormatter: formatNumber },
    ...(colors && colors.length > 0 ? { color: [...colors] } : {}),
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };

  if ((config.mode ?? "overlap") === "ridge" && samples.length > 1) {
    return ridgeOption({
      common,
      series,
      names,
      x: [Math.min(...xs), Math.max(...xs)],
      y: [Math.min(0, ...ys), Math.max(0, ...ys)],
      xlab,
      zoom: config.zoom ?? false,
    });
  }

  const xAxis: Record<string, unknown> = {
    type: "value",
    scale: true,
    ...(xlab ? { name: xlab, nameLocation: "middle" } : {}),
  };
  const yAxis: Record<string, unknown> = {
    type: "value",
    min: 0,
    ...(ylab ? { name: ylab, nameLocation: "middle" } : {}),
  };
  const grid = cartesianGrid(config, Boolean(xlab || ylab));
  const dataZoom = config.zoom ? axisZoom("x", grid, { filterMode: "none" }) : undefined;
  return {
    ...common,
    ...(samples.length > 1 ? { legend: { data: names } } : {}),
    xAxis,
    yAxis,
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    series,
  };
}
