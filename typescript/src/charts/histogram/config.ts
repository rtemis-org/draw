import { formatNumber } from "../../shared/format.ts";
import { axisZoom, cartesianGrid, type MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { ridgeOption } from "../../shared/ridge.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import type { BreaksRule } from "./edges.ts";
import { HISTOGRAM, type HistogramPayload } from "./renderers.ts";

/** A height normalization of the bins of one sample. */
export type HistogramNormalization =
  | "count"
  | "probability"
  | "percent"
  | "density"
  | "count_density";

/** A statistic of the x values in each bin. */
export type HistogramBinStat = "count" | "sum" | "mean" | "min" | "max";

/**
 * An instance of the `chart/histogram/v1` schema, written by rtemis.draw's
 * `HistogramConfig` class. Every member is optional. The members that decide
 * the bins and the curves (`x`, `group`, `breaks`, `bins`, `bin_edges`,
 * `bin_stat`, `order`, `na_rm`, and the density members `n`, `bw`,
 * `bandwidth`, `kernel`, `adjust`) are applied by the host, which computes
 * {@link HistogramData}.
 * spec: draw/shared-builders#histogram
 */
export interface HistogramChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "histogram";
  /** Columns to bin; each is a sample, split by `group`. */
  readonly x?: ColumnNames | null;
  /** Column whose levels split each `x` column into samples. */
  readonly group?: string | null;
  /** Class-count rule of the edges. Default `"Sturges"`. */
  readonly breaks?: BreaksRule;
  /** Suggested bin count, rounded to `pretty()` edges. */
  readonly bins?: number | null;
  /** Bin edges, strictly increasing, spanning every value. */
  readonly bin_edges?: readonly number[] | null;
  /** Height of a bin within its sample. Default `"count"`. */
  readonly normalization?: HistogramNormalization;
  /** Statistic of the x values in each bin. Default `"count"`. */
  readonly bin_stat?: HistogramBinStat;
  /** Groups overlaid, dodged within each bin, or stacked. Default `"overlay"`. */
  readonly bar_mode?: "overlay" | "group" | "stack";
  /** Samples overlaid, or one row each on a shared x range. Default `"overlap"`. */
  readonly mode?: "overlap" | "ridge";
  /** Sample order: input, or decreasing mean or median. Default `"input"`. */
  readonly order?: "input" | "mean" | "median";
  /** Overlay each sample's kernel density on the bins' scale. */
  readonly density?: boolean;
  /** Points at which the density is estimated. */
  readonly n?: number;
  /** Bandwidth selector. */
  readonly bw?: "nrd0" | "nrd" | "ucv" | "bcv" | "SJ" | "SJ-ste" | "SJ-dpi";
  /** Bandwidth in measurement units. */
  readonly bandwidth?: number | null;
  /** Density smoothing kernel. */
  readonly kernel?:
    | "gaussian"
    | "epanechnikov"
    | "rectangular"
    | "triangular"
    | "biweight"
    | "cosine"
    | "optcosine";
  /** Bandwidth multiplier. */
  readonly adjust?: number;
  /** Drop missing values before binning. */
  readonly na_rm?: boolean;
  /** Bar fill opacity; unset, 0.5 where bars of several samples overlap and 0.75 otherwise. */
  readonly fill_alpha?: number | null;
  /** Bar border opacity; 0 draws no border. Default 1. */
  readonly border_alpha?: number;
  /** Zoom and pan the x axis. */
  readonly zoom?: boolean;
  /** Series colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Bottom axis name; unset or empty draws none. */
  readonly xlab?: string | null;
  /** Left axis name; unset or empty draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** Every member of {@link HistogramChartConfig}, checked against `chart/histogram/v1`. */
export const HISTOGRAM_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "group",
  "breaks",
  "bins",
  "bin_edges",
  "normalization",
  "bin_stat",
  "bar_mode",
  "mode",
  "order",
  "density",
  "n",
  "bw",
  "bandwidth",
  "kernel",
  "adjust",
  "na_rm",
  "fill_alpha",
  "border_alpha",
  "zoom",
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
] as const satisfies readonly (keyof HistogramChartConfig)[]);

type MissingMember = Exclude<
  keyof HistogramChartConfig,
  (typeof HISTOGRAM_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/** The bins of one sample: one `x` column, or one column within one group level. */
export interface HistogramSample {
  /** The sample's name; unset for the only sample of an ungrouped column. */
  readonly name?: string | null;
  /** Number of values binned. */
  readonly n: number;
  /** Values in each bin, `(lower, upper]` with the lowest edge included. */
  readonly counts: readonly number[];
  /** `bin_stat` of the values in each bin, 0 for an empty bin; unset for a count. */
  readonly values?: readonly number[] | null;
  /** Kernel density in density units, for `density`. */
  readonly density?: { readonly x: readonly number[]; readonly y: readonly number[] } | null;
}

/**
 * A histogram's data: one set of edges and the bins of each sample, in
 * drawing order, computed by the host as the config's binning members say.
 * rtemis.draw bins with `graphics::hist()`, rtemislive in DuckDB.
 */
export interface HistogramData {
  readonly edges: readonly number[];
  readonly samples: readonly HistogramSample[];
}

/** What is not chart config: the theme palette and animation. */
export interface HistogramAppearance {
  /** The theme's categorical palette; unset uses the theme's. */
  readonly palette?: readonly string[];
  /** Animate the first render; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** The name of a bin's height in the tooltip, by normalization and statistic. */
function heightName(config: HistogramChartConfig): string {
  const stat = config.bin_stat ?? "count";
  if (stat !== "count") return stat;
  switch (config.normalization ?? "count") {
    case "count":
      return "Height";
    case "probability":
      return "Probability";
    case "percent":
      return "Percent";
    case "density":
      return "Density";
    case "count_density":
      return "Count density";
  }
}

/**
 * The value axis name a host derives for a histogram when the author sets
 * none, which says what a bar's height is: the normalization ("Count",
 * "Probability", "Percent", "Density", "Count density"), or the statistic of
 * the x values in a bin, with the column when `x` is one column ("Sum of X",
 * "Mean X", "Minimum X", "Maximum X"). `name` maps a column name to its
 * display text. rtemis.draw's `resolve()` derives the same text.
 */
export function histogramValueLabel(
  config: HistogramChartConfig,
  name: (column: string) => string = (column) => column,
): string {
  const xs = asArray(config.x);
  const only = xs.length === 1 ? name(xs[0] as string) : null;
  switch (config.bin_stat ?? "count") {
    case "sum":
      return only === null ? "Sum" : `Sum of ${only}`;
    case "mean":
      return only === null ? "Mean" : `Mean ${only}`;
    case "min":
      return only === null ? "Minimum" : `Minimum ${only}`;
    case "max":
      return only === null ? "Maximum" : `Maximum ${only}`;
    default: {
      const height = heightName(config);
      return height === "Height" ? "Count" : height;
    }
  }
}

/** Bar heights of one sample on a normalization scale; an empty sample is all zeros. */
function normalize(
  counts: readonly number[],
  widths: readonly number[],
  n: number,
  normalization: HistogramNormalization,
): number[] {
  if (n === 0) return counts.map(() => 0);
  const scale: Record<HistogramNormalization, (c: number, w: number) => number> = {
    count: (c) => c,
    probability: (c) => c / n,
    percent: (c) => (100 * c) / n,
    density: (c, w) => c / (n * w),
    count_density: (c, w) => c / w,
  };
  return counts.map((c, i) => scale[normalization](c, widths[i] as number));
}

/**
 * Build the ECharts option of a histogram from host-computed bins and its
 * `chart/histogram/v1` config. Each bin is a rectangle between its edges on a
 * numeric x axis, drawn by the `rtemis.histogram.v1` renderer that
 * {@link registerRenderers} registers; `bar_mode` overlays, dodges, or stacks
 * the samples, and `mode` ridge gives each sample a row on a shared x range.
 * With `density`, each sample's kernel density is drawn on the bins' scale.
 */
export function buildHistogramOption(
  data: HistogramData,
  config: HistogramChartConfig,
  appearance: HistogramAppearance = {},
): Record<string, unknown> {
  const edges = data.edges;
  if (edges.length < 2 || edges.some((e, i) => i > 0 && !(e > (edges[i - 1] as number)))) {
    throw new Error("Histogram `edges` must be at least two strictly increasing values.");
  }
  const samples = data.samples;
  if (samples.length === 0) {
    throw new Error("A histogram needs `x` set to one or more columns to draw.");
  }
  const nBins = edges.length - 1;
  for (const s of samples) {
    if (s.counts.length !== nBins || (s.values && s.values.length !== nBins)) {
      throw new Error(
        `Each sample needs one count per bin (${nBins}); '${s.name ?? "x"}' has ${s.counts.length}.`,
      );
    }
  }
  const normalization = config.normalization ?? "count";
  const binStat = config.bin_stat ?? "count";
  const barMode = config.bar_mode ?? "overlay";
  const ridge = (config.mode ?? "overlap") === "ridge";
  const density = config.density ?? false;
  if (binStat !== "count" && (normalization !== "count" || density)) {
    throw new Error(
      'A `bin_stat` other than count takes `normalization = "count"` and no `density`.',
    );
  }
  if (barMode !== "overlay" && (ridge || density)) {
    throw new Error('A `bar_mode` other than overlay takes `mode = "overlap"` and no `density`.');
  }
  const widths = edges.slice(1).map((e, i) => e - (edges[i] as number));
  const first = widths[0] as number;
  const equalWidth = Math.max(...widths.map((w) => Math.abs(w / first - 1))) < 1e-7;
  if (
    binStat === "count" &&
    !equalWidth &&
    normalization !== "density" &&
    normalization !== "count_density"
  ) {
    throw new Error('Use `normalization = "density"` or `"count_density"` for unequal-width bins.');
  }

  const names = samples.map((s, i) => s.name ?? `Series ${i + 1}`);
  const nonempty = samples.filter((s) => s.n > 0).length;
  const fillAlpha =
    config.fill_alpha ?? (barMode === "overlay" && !ridge && nonempty > 1 ? 0.5 : 0.75);
  const borderAlpha = config.border_alpha ?? 1;
  const height = heightName(config);
  const heights = samples.map((s) =>
    binStat === "count" ? normalize(s.counts, widths, s.n, normalization) : [...(s.values ?? [])],
  );

  const series: Record<string, unknown>[] = [];
  const barIndices: number[] = [];
  samples.forEach((s, i) => {
    const name = names[i] as string;
    barIndices.push(series.length);
    series.push({
      type: "custom",
      name,
      renderItem: HISTOGRAM,
      clip: true,
      z: 2,
      dimensions: ["Lower", "Upper", height, "Count"],
      encode: {
        x: [0, 1],
        y: 2,
        tooltip: normalization === "count" && binStat === "count" ? [0, 1, 3] : [0, 1, 2, 3],
      },
      data: s.counts.map((c, b) => [edges[b], edges[b + 1], heights[i]?.[b] ?? 0, c]),
    });
    if (density) {
      // Counts and probabilities per bin take a bin width; density scales do not.
      const multiplier =
        normalization === "count"
          ? s.n * first
          : normalization === "probability"
            ? first
            : normalization === "percent"
              ? 100 * first
              : normalization === "density"
                ? 1
                : s.n;
      // An empty sample keeps its curve series, empty, beside its bars.
      const curve = s.density ?? { x: [], y: [] };
      series.push({
        type: "line",
        name,
        // Transparent point targets give the curve an item tooltip, which an
        // unmarked line does not; the SVG export draws them invisibly.
        showSymbol: true,
        symbol: "circle",
        symbolSize: 8,
        itemStyle: { opacity: 0 },
        lineStyle: { opacity: 1 },
        emphasis: { itemStyle: { opacity: 1 } },
        z: 3,
        data: curve.x.map((x, j) => [x, (curve.y[j] as number) * multiplier]),
      });
    }
  });
  const payload: HistogramPayload = {
    fillAlpha,
    borderAlpha,
    mode: barMode,
    seriesIndices: barIndices,
    heights,
  };
  for (const i of barIndices) (series[i] as Record<string, unknown>).itemPayload = payload;

  // The y range holds every bar, every stack, and every curve.
  const curveHeights = series
    .filter((s) => s.type === "line")
    .flatMap((s) => (s.data as number[][]).map((d) => d[1] as number));
  const totals =
    barMode === "stack"
      ? edges
          .slice(1)
          .flatMap((_, b) => [
            heights.reduce((sum, h) => sum + Math.max(h[b] ?? 0, 0), 0),
            heights.reduce((sum, h) => sum + Math.min(h[b] ?? 0, 0), 0),
          ])
      : heights.flat();
  const low = Math.min(0, ...totals, ...curveHeights);
  const high = Math.max(0, ...totals, ...curveHeights);

  const palette = asArray(config.palette);
  const colors = palette.length > 0 ? palette : appearance.palette;
  const xlab = config.xlab || null;
  const ylab = config.ylab || null;
  const common = {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: { trigger: "item", valueFormatter: formatNumber },
    // A custom bar hovered during an update transition leaves the y axis at
    // an intermediate scale (ECharts 6.1.0), so updates apply at once.
    animationDurationUpdate: 0,
    ...(colors && colors.length > 0 ? { color: [...colors] } : {}),
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };

  if (ridge && samples.length > 1) {
    const curveX = series
      .filter((s) => s.type === "line")
      .flatMap((s) => (s.data as number[][]).map((d) => d[0] as number));
    return ridgeOption({
      common,
      series,
      names,
      x: [
        Math.min(edges[0] as number, ...curveX),
        Math.max(edges[edges.length - 1] as number, ...curveX),
      ],
      y: [low, high],
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
    min: low < 0 ? low * 1.05 : 0,
    ...(barMode === "stack" && high > 0 ? { max: high * 1.05 } : {}),
    ...(ylab ? { name: ylab, nameLocation: "middle" } : {}),
  };
  const grid = cartesianGrid(config, Boolean(xlab || ylab));
  const dataZoom = config.zoom ? axisZoom("x", grid, { filterMode: "none" }) : undefined;
  return {
    ...common,
    ...(samples.length > 1 ? { legend: { data: names, itemStyle: { opacity: 1 } } } : {}),
    xAxis,
    yAxis,
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    series,
  };
}
