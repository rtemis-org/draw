import { tooltipValue } from "../../shared/format.ts";
import { axisZoom, cartesianGrid, type MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { type AxisLimits, calcLimits } from "../../shared/limits.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import { RTEMIS_PALETTE, SIGN_COLORS } from "../../theme/tokens.ts";
import {
  BOXPLOT_BRACKETS,
  BOXPLOT_MEAN,
  BOXPLOT_PAIRS,
  BOXPLOT_POINTS,
  BOXPLOT_VIOLIN,
} from "./renderers.ts";

/**
 * An instance of the `chart/boxplot/v1` schema, written by rtemis.draw's
 * `BoxplotConfig` class. Every member is optional. The members that decide
 * the statistics (`order`, `transform`, `quartiles`, `whisker`, `na_rm`,
 * `bandwidth`, `adjust`, `density_points`, `comparisons`, `observation`) are
 * applied by the host, which computes {@link BoxplotData}.
 * spec: draw/shared-builders#boxplot
 */
export interface BoxplotChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "boxplot";
  /** Columns to summarize, one box each. */
  readonly x?: ColumnNames | null;
  /** Column that splits each box by level. */
  readonly group?: string | null;
  /** Column identifying observations, for point tooltips and pairing. */
  readonly observation?: string | null;
  /** Category order, applied by the host. Default `"input"`. */
  readonly order?: "input" | "mean" | "median";
  /** Transform of each column before summarizing, applied by the host. Default `"none"`. */
  readonly transform?: "none" | "scale" | "minmax";
  /** Distribution geometry. Default `"box"`. */
  readonly geometry?: "box" | "violin" | "both";
  /** Gaussian density bandwidth, used by the host. */
  readonly bandwidth?: number | null;
  /** Density bandwidth multiplier, used by the host. Default `1`. */
  readonly adjust?: number;
  /** Density evaluation points, used by the host. Default `128`. */
  readonly density_points?: number;
  /** Join matching observations in adjacent categories. Default `false`. */
  readonly paired?: boolean;
  /** Paired line opacity, `[0, 1]`. Default `0.35`. */
  readonly pair_alpha?: number;
  /** Paired line width in pixels. Default `1`. */
  readonly pair_width?: number;
  /** Data entry holding comparison records, read by the host. */
  readonly comparisons?: string | null;
  /** Quartile convention, applied by the host. Default `"linear"`. */
  readonly quartiles?: "linear" | "hinges";
  /** IQR multiplier for the whisker fences, applied by the host. Default `1.5`. */
  readonly whisker?: number;
  /** Observed values drawn over the boxes. Default `"none"`. */
  readonly boxpoints?: "none" | "all" | "outliers";
  /** Point diameter in pixels. Default `5`. */
  readonly point_size?: number;
  /** Point opacity, `[0, 1]`. Default `0.6`. */
  readonly point_alpha?: number;
  /** Fraction of the box width point offsets occupy, `[0, 1]`. Default `0.5`. */
  readonly point_spread?: number;
  /** Mark each box's mean with a diamond, from the host's `mean`. Default `false`. */
  readonly mean_marker?: boolean;
  /** Draw the boxes horizontally. Default `false`. */
  readonly horizontal?: boolean;
  /** A slider, Shift+wheel zoom, and drag panning on the category axis. Default `false`. */
  readonly zoom?: boolean;
  /** Drop missing values before summarizing, applied by the host. Default `true`. */
  readonly na_rm?: boolean;
  /** Labels of the `x` columns; unset uses the column names. */
  readonly labels?: ColumnNames | null;
  /** Box colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Box fill opacity, `[0, 1]`. Default `0.25`. */
  readonly fill_alpha?: number;
  /**
   * Where the group levels go: `"x-axis"` makes them the categories, with one
   * series per column; `"legend"` makes the columns the categories, with one
   * series per level. `"auto"`: x-axis for one column, legend for several.
   */
  readonly group_layout?: "auto" | "x-axis" | "legend";
  /** Color each box of a one-series chart by its category. Default `false`. */
  readonly color_by_group?: boolean;
  /** Value axis limits, `[min, max]`. */
  readonly ylim?: readonly [number, number] | null;
  /** Bottom axis name; unset draws none. */
  readonly xlab?: string | null;
  /** Left axis name; unset draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** Every member of {@link BoxplotChartConfig}, checked against `chart/boxplot/v1`. */
export const BOXPLOT_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "group",
  "observation",
  "order",
  "transform",
  "geometry",
  "bandwidth",
  "adjust",
  "density_points",
  "paired",
  "pair_alpha",
  "pair_width",
  "comparisons",
  "quartiles",
  "whisker",
  "boxpoints",
  "point_size",
  "point_alpha",
  "point_spread",
  "mean_marker",
  "horizontal",
  "zoom",
  "na_rm",
  "labels",
  "palette",
  "fill_alpha",
  "group_layout",
  "color_by_group",
  "ylim",
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
] as const satisfies readonly (keyof BoxplotChartConfig)[]);

type MissingMember = Exclude<
  keyof BoxplotChartConfig,
  (typeof BOXPLOT_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/** One observed value: its value, identifier, and 1-based row position in its column. */
export interface BoxplotObservation {
  readonly value: number;
  readonly id: string;
  readonly index: number;
}

/** The summary of one column, or of one column within one group level. */
export interface BoxplotCell {
  /** The bound column. */
  readonly variable: string;
  /** The group level; unset without `group`. */
  readonly level?: string | null;
  /** Lower whisker end, lower quartile, median, upper quartile, upper whisker end; `null` without values. */
  readonly stats: readonly [number, number, number, number, number] | null;
  /** Number of values summarized. */
  readonly n: number;
  /** Smallest and largest value; `null` without values. */
  readonly range?: readonly [number, number] | null;
  /** Mean of the values, for `mean_marker`; `null` without values. */
  readonly mean?: number | null;
  /** The values drawn as points (`boxpoints` all or outliers). */
  readonly points?: readonly BoxplotObservation[];
  /** Every value with its identifier, for `paired`. */
  readonly observations?: readonly BoxplotObservation[];
  /** Violin profile: positions on the value axis and widths relative to the widest. */
  readonly profile?: {
    readonly position: readonly number[];
    readonly width: readonly number[];
  } | null;
}

/** A comparison bracket between two boxes, with the text it carries. */
export interface BoxplotComparison {
  readonly from: string;
  readonly to: string;
  readonly label: string;
  readonly from_group?: string | null;
  readonly to_group?: string | null;
  readonly position?: number | null;
}

/**
 * A boxplot's data: summaries the host computed from the bound columns, as
 * the config's statistics members say. rtemis.draw computes them in R,
 * rtemislive in DuckDB. `variables` and `levels` are in drawing order.
 */
export interface BoxplotData {
  readonly variables: readonly string[];
  readonly levels?: readonly string[] | null;
  readonly cells: readonly BoxplotCell[];
  readonly comparisons?: readonly BoxplotComparison[] | null;
}

/** What is not chart config: the theme palette and animation. */
export interface BoxplotAppearance {
  /** The theme's categorical palette; unset uses rtemis.draw's. */
  readonly palette?: readonly string[];
  /** Animate the first render and updates; unset leaves the ECharts default. */
  readonly animation?: boolean;
  /** The theme's neutral color (`neutral_color`); unset is rtemis.draw's, `SIGN_COLORS.neutral`. */
  readonly neutral?: string;
}

interface BoxSeries {
  readonly name: string | null;
  readonly cells: readonly (BoxplotCell | null)[];
}

/**
 * Build the ECharts option of a boxplot from host-computed summaries and its
 * `chart/boxplot/v1` config. Without `group`, the columns are the categories
 * of one series. With `group`, `group_layout` places the levels: on the
 * category axis with one series per column, or in the legend with one series
 * per level over the columns. Points, mean markers, violins, paired lines,
 * and comparison brackets are custom series drawn by the renderers
 * {@link registerRenderers} registers.
 */
export function buildBoxplotOption(
  data: BoxplotData,
  config: BoxplotChartConfig,
  appearance: BoxplotAppearance = {},
): Record<string, unknown> {
  const variables = data.variables;
  if (variables.length === 0) {
    throw new Error("A boxplot needs `x` set to one or more columns to draw.");
  }
  const labels = asArray(config.labels);
  if (labels.length > 0 && labels.length !== variables.length) {
    throw new Error(
      `\`labels\` must have one label per \`x\` column (${variables.length}); it has ${labels.length}.`,
    );
  }
  const name = (variable: string) => labels[variables.indexOf(variable)] ?? variable;
  const levels = data.levels && data.levels.length > 0 ? data.levels : null;
  const layout = !levels
    ? "none"
    : (config.group_layout ?? "auto") === "auto"
      ? variables.length === 1
        ? "x-axis"
        : "legend"
      : config.group_layout;
  const cell = (variable: string, level: string | null) =>
    data.cells.find((c) => c.variable === variable && (c.level ?? null) === level) ?? null;

  let categories: string[];
  let series: BoxSeries[];
  if (!levels) {
    categories = variables.map(name);
    series = [{ name: null, cells: variables.map((v) => cell(v, null)) }];
  } else if (layout === "x-axis") {
    categories = [...levels];
    series = variables.map((v) => ({
      name: variables.length > 1 ? name(v) : null,
      cells: levels.map((l) => cell(v, l)),
    }));
  } else {
    categories = variables.map(name);
    series = levels.map((l) => ({ name: l, cells: variables.map((v) => cell(v, l)) }));
  }
  if (new Set(categories).size !== categories.length) {
    throw new Error("Use unique category labels for a boxplot.");
  }
  if (!data.cells.some((c) => c.n > 0)) {
    throw new Error("Supply at least one available boxplot value; missing groups are not zeros.");
  }

  const memberPalette = asArray(config.palette);
  const neutral = appearance.neutral ?? SIGN_COLORS.neutral;
  const palette = memberPalette.length > 0 ? memberPalette : (appearance.palette ?? RTEMIS_PALETTE);
  const single = series.length === 1;
  // One series is one color, unless its boxes are colored by category: on
  // request, or when the palette member lists several colors.
  const perBox = single && (config.color_by_group === true || memberPalette.length > 1);
  const colorOf = (s: number, j: number) =>
    (perBox ? palette[j % palette.length] : palette[s % palette.length]) as string;
  const fillAlpha = config.fill_alpha ?? 0.25;
  const geometry = config.geometry ?? "box";
  const boxpoints = config.boxpoints ?? "none";
  const horizontal = config.horizontal ?? false;
  const spread = config.point_spread ?? 0.5;
  const boxSeries = series.map((_, s) => s);

  const visible: number[] = [];
  const boxes = series.map((s, si) => {
    const values = s.cells.map((c, j) => {
      const stats = c?.stats ?? [null, null, null, null, null];
      for (const v of c?.stats ?? []) visible.push(v);
      if (!perBox) return stats;
      const color = colorOf(si, j);
      return {
        name: categories[j],
        value: stats,
        itemStyle: {
          color: withAlpha(color, fillAlpha),
          borderColor: color,
          ...(geometry === "violin" ? { opacity: 0 } : {}),
        },
      };
    });
    const color = colorOf(si, 0);
    return {
      type: "boxplot",
      ...(s.name === null ? {} : { name: s.name }),
      data: values,
      ...(geometry === "both" ? { boxWidth: [7, 14] } : {}),
      ...(geometry === "violin" ? { silent: true } : {}),
      itemStyle: {
        color: withAlpha(color, fillAlpha),
        borderColor: color,
        ...(geometry === "violin" ? { opacity: 0 } : {}),
      },
    };
  });

  const encodeOne = horizontal ? { x: 1, y: 0 } : { x: 0, y: 1 };
  const overlays: Record<string, unknown>[] = [];
  const offsetOf = (index: number) => radicalInverse(index) * spread;
  series.forEach((s, si) => {
    if (boxpoints === "none") return;
    const points = s.cells.flatMap((c, j) =>
      (c?.points ?? []).map((p) => {
        visible.push(p.value);
        return {
          value: [j, p.value, p.id, offsetOf(p.index)],
          itemStyle: { color: colorOf(si, j), opacity: config.point_alpha ?? 0.6 },
        };
      }),
    );
    if (points.length === 0) return;
    overlays.push({
      type: "custom",
      ...(s.name === null ? {} : { name: s.name }),
      renderItem: BOXPLOT_POINTS,
      clip: true,
      z: 3,
      dimensions: ["Category", "Value", "Observation", "Offset"],
      encode: { ...encodeOne, tooltip: [1, 2] },
      itemPayload: {
        horizontal,
        boxSeries,
        boxIndex: si,
        pointSize: config.point_size ?? 5,
        pointAlpha: config.point_alpha ?? 0.6,
      },
      data: points,
    });
  });

  if (config.mean_marker) {
    series.forEach((s, si) => {
      const means = s.cells.flatMap((c, j) =>
        typeof c?.mean === "number"
          ? [{ value: [j, c.mean], itemStyle: { color: colorOf(si, j) } }]
          : [],
      );
      if (means.length === 0) return;
      overlays.push({
        type: "custom",
        ...(s.name === null ? {} : { name: s.name }),
        renderItem: BOXPLOT_MEAN,
        clip: true,
        z: 4,
        dimensions: ["Category", "Mean"],
        encode: { ...encodeOne, tooltip: [1] },
        itemPayload: { horizontal, boxSeries, boxIndex: si, size: 9 },
        data: means,
      });
    });
  }

  const layers: Record<string, unknown>[] = [];
  const rangeOf = (c: BoxplotCell | null) => c?.range ?? null;
  series.forEach((s, si) => {
    const payload = { horizontal, boxSeries, boxIndex: si };
    if (geometry !== "box") {
      layers.push({
        type: "custom",
        ...(s.name === null ? {} : { name: s.name }),
        renderItem: BOXPLOT_VIOLIN,
        clip: true,
        z: 1,
        dimensions: ["Category", "Minimum", "Maximum"],
        encode: horizontal ? { x: [1, 2], y: 0 } : { x: 0, y: [1, 2] },
        itemPayload: {
          ...payload,
          profiles: s.cells.map((c) => c?.profile ?? {}),
          fillAlpha,
        },
        data: s.cells.map((c, j) => {
          const range = rangeOf(c);
          return {
            value: [j, range?.[0] ?? null, range?.[1] ?? null],
            itemStyle: { color: colorOf(si, j) },
          };
        }),
      });
    }
    if (config.paired) {
      const pairs: unknown[][] = [];
      s.cells.forEach((from, j) => {
        const to = s.cells[j + 1];
        if (!from || !to) return;
        const shown = (c: BoxplotCell) => new Set((c.points ?? []).map((p) => p.id));
        const fromShown = shown(from);
        const toShown = shown(to);
        const target = new Map((to.observations ?? []).map((o) => [o.id, o]));
        for (const a of from.observations ?? []) {
          const b = target.get(a.id);
          if (!b) continue;
          pairs.push([
            j,
            a.value,
            fromShown.has(a.id) ? offsetOf(a.index) : 0,
            j + 1,
            b.value,
            toShown.has(b.id) ? offsetOf(b.index) : 0,
            a.id,
          ]);
        }
      });
      layers.push({
        type: "custom",
        ...(s.name === null ? {} : { name: s.name }),
        renderItem: BOXPLOT_PAIRS,
        silent: true,
        clip: true,
        z: 2,
        dimensions: [
          "From",
          "From value",
          "From offset",
          "To",
          "To value",
          "To offset",
          "Observation",
        ],
        encode: horizontal ? { x: [1, 4], y: [0, 3] } : { x: [0, 3], y: [1, 4] },
        itemPayload: {
          ...payload,
          alpha: config.pair_alpha ?? 0.35,
          width: config.pair_width ?? 1,
        },
        itemStyle: { color: s.name === null ? neutral : colorOf(si, 0) },
        data: pairs,
      });
    }
  });

  // Violins and paired lines span every value, not only the box statistics.
  if (geometry !== "box" || config.paired) {
    for (const s of series)
      for (const c of s.cells) for (const v of rangeOf(c) ?? []) visible.push(v);
  }
  let limits = calcLimits(visible, 0) ?? ([0, 1] as AxisLimits);
  const comparisons = data.comparisons ?? [];
  if (comparisons.length > 0) {
    const resolved = bracketRecords(comparisons, categories, series, limits);
    limits = resolved.limits;
    layers.push({
      type: "custom",
      renderItem: BOXPLOT_BRACKETS,
      // The renderer draws the brackets and their labels in the series color.
      itemStyle: { color: neutral },
      silent: true,
      clip: true,
      z: 4,
      dimensions: ["From", "To", "Position", "From series", "To series", "Label"],
      encode: horizontal ? { x: 2, y: [0, 1] } : { x: [0, 1], y: 2 },
      itemPayload: { horizontal, boxSeries },
      data: resolved.records,
    });
  }
  // Drawn marks beyond the box statistics get room at the axis ends.
  const padded =
    boxpoints !== "none" || geometry !== "box" || config.paired || comparisons.length > 0;
  const valueLimits = config.ylim ?? (padded ? calcLimits(limits, 0.05) : null);
  const axisLimits = valueLimits ?? limits;

  const categoryAxis: Record<string, unknown> = {
    type: "category",
    data: categories,
    // Every box keeps its label; ECharts would otherwise thin them.
    axisLabel: { interval: 0 },
    // A horizontal chart lists its categories top to bottom, in reading order.
    ...(horizontal ? { inverse: true } : {}),
    axisLine: { show: axisLimits[0] <= 0 && axisLimits[1] >= 0, onZero: true },
  };
  const valueAxis: Record<string, unknown> = {
    type: "value",
    scale: true,
    ...(valueLimits ? { min: valueLimits[0], max: valueLimits[1] } : {}),
  };
  const xAxis = horizontal ? valueAxis : categoryAxis;
  const yAxis = horizontal ? categoryAxis : valueAxis;
  if (config.xlab) Object.assign(xAxis, { name: config.xlab });
  if (config.ylab) Object.assign(yAxis, { name: config.ylab });

  const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
  const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : undefined;
  const named = series.filter((s) => s.name !== null);

  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    ...(named.length > 0
      ? {
          legend: {
            data: named.map((s) => s.name),
            ...(geometry === "violin" ? { itemStyle: { opacity: 1 } } : {}),
          },
        }
      : {}),
    xAxis,
    yAxis,
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    series: [...boxes, ...overlays, ...layers],
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };
}

// Paired lines of one series take a neutral gray, so they read as links
// rather than as a group.

/**
 * Deterministic offsets in `[-0.5, 0.5)`: the base-two radical inverse of a
 * 1-based row position, so points keep their places when others are removed
 * and no random state is read.
 */
export function radicalInverse(index: number): number {
  let x = index;
  let value = 0;
  let weight = 0.5;
  while (x > 0) {
    value += (x % 2) * weight;
    x = Math.floor(x / 2);
    weight /= 2;
  }
  return value - 0.5;
}

/** A `#rrggbb` color with an alpha, as `rgba()`; any other color as it is. */
function withAlpha(color: string, alpha: number): string {
  const m = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(color);
  if (!m) return color;
  const [r, g, b] = [m[1], m[2], m[3]].map((h) => Number.parseInt(h as string, 16));
  return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

/**
 * Comparison brackets resolved to category and series indices, positioned
 * above the drawn marks unless a record gives its position, and the value
 * range extended to hold them and their labels.
 */
function bracketRecords(
  comparisons: readonly BoxplotComparison[],
  categories: readonly string[],
  series: readonly BoxSeries[],
  limits: AxisLimits,
): { records: unknown[][]; limits: AxisLimits } {
  const grouped = series.length > 1;
  const span =
    limits[1] - limits[0] === 0 ? Math.max(Math.abs(limits[1]), 1) : limits[1] - limits[0];
  const positions: number[] = [];
  const records = comparisons.map((c, i) => {
    if (!grouped && (c.from_group || c.to_group)) {
      throw new Error("Omit comparison group columns for a single series; use category names.");
    }
    const from = categories.indexOf(c.from);
    const to = categories.indexOf(c.to);
    const seriesIndex = (g: string | null | undefined) =>
      grouped ? series.findIndex((s) => s.name === g) : 0;
    const fromSeries = seriesIndex(c.from_group);
    const toSeries = seriesIndex(c.to_group);
    if (from < 0 || to < 0 || fromSeries < 0 || toSeries < 0) {
      throw new Error("Use existing category and group names in comparison endpoints.");
    }
    if (from === to && fromSeries === toSeries) {
      throw new Error("Choose two distinct comparison endpoints.");
    }
    for (const [cat, s] of [
      [from, fromSeries],
      [to, toSeries],
    ] as const) {
      if (!((series[s]?.cells[cat]?.n ?? 0) > 0)) {
        throw new Error("Choose comparison endpoints with available observations.");
      }
    }
    const position = c.position ?? limits[1] + (i + 1) * span * 0.12;
    positions.push(position);
    return [from, to, position, fromSeries, toSeries, c.label];
  });
  const extended = calcLimits(
    [...limits, ...positions.map((p) => p - span * 0.04), ...positions.map((p) => p + span * 0.12)],
    0,
  );
  return { records, limits: extended ?? limits };
}
