import { tooltipValue } from "../../shared/format.ts";
import { axisZoom, cartesianGrid } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import {
  asArray,
  type ColumnNames,
  column,
  distinct,
  isTime,
  type TabularData,
  textColumn,
  timeMs,
} from "../../shared/source.ts";
import { SIGN_COLORS } from "../../theme/tokens.ts";
import { AXIS_LABELS } from "./renderers.ts";

/** The host's input to the time series builder beyond the data and config. */
export interface TimeSeriesAppearance {
  /** The theme's neutral color (`neutral_color`); unset is rtemis.draw's, `SIGN_COLORS.neutral`. */
  readonly neutral?: string;
}

/** A rolling summary of a time series. */
export type RollFn = "none" | "mean" | "median" | "min" | "max" | "sum";

/**
 * An instance of the `chart/timeseries/v1` schema, written by rtemis.draw's
 * `TimeSeriesConfig` class. Every member is optional. `x`, `y`, `group`,
 * `x2`, `y2`, `zt`, and `shade_bin` name columns.
 * spec: draw/shared-builders#time-series
 */
export interface TimeSeriesChartConfig extends LegendMembers {
  readonly type?: "timeseries";
  /** Time column: numbers, or a column the data declares `"time"`. */
  readonly x?: string | null;
  /** Value columns; each is one series on the left axis. */
  readonly y?: ColumnNames | null;
  /** Column splitting every `y` and `y2` column into one series per value. */
  readonly group?: string | null;
  /** Time column of the right-axis series; unset uses `x`. */
  readonly x2?: string | null;
  /** Value columns on the right axis. */
  readonly y2?: ColumnNames | null;
  /** Column of zeitgeber labels, which replace the time axis labels. */
  readonly zt?: string | null;
  /** Column of zero/one flags; runs of ones are shaded. */
  readonly shade_bin?: string | null;
  /** Every how many times a zeitgeber label is written; phase zero always is. Default 1. */
  readonly show_zt_every?: number;
  /** Observations per rolling window. Default 7. */
  readonly window?: number;
  /** Rolling summary. Default `"none"`. */
  readonly roll_fn?: RollFn;
  /** Window alignment. Default `"center"`. */
  readonly align?: "center" | "left" | "right";
  /** Fewest values present a window needs; unset is a majority, and the whole window for `"sum"`. */
  readonly min_obs?: number | null;
  /** Rolling line width in pixels. Default 2. */
  readonly roll_width?: number;
  /** Rolling line color; unset uses the series color. */
  readonly roll_color?: string | null;
  /** Raw observation opacity. Default 0.5. */
  readonly raw_opacity?: number;
  /** Draw raw observations as points; `false` draws lines. Default `true`. */
  readonly points?: boolean;
  /** A slider, Shift+wheel zoom, and drag panning on the time axis. Default `false`. */
  readonly zoom?: boolean;
  readonly xlab?: string | null;
  readonly ylab?: string | null;
  readonly y2lab?: string | null;
  /** Series colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  readonly title?: string | null;
}

/** Every member of {@link TimeSeriesChartConfig}, checked against `chart/timeseries/v1`. */
export const TIMESERIES_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "x",
  "y",
  "group",
  "xlab",
  "ylab",
  "palette",
  "zoom",
  "x2",
  "zt",
  "shade_bin",
  "show_zt_every",
  "y2",
  "y2lab",
  "window",
  "roll_fn",
  "align",
  "min_obs",
  "roll_width",
  "roll_color",
  "raw_opacity",
  "points",
] as const satisfies readonly (keyof TimeSeriesChartConfig)[]);

type Missing = Exclude<
  keyof TimeSeriesChartConfig,
  (typeof TIMESERIES_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** The rolling-window members of a config. */
export type RollingMembers = Pick<
  TimeSeriesChartConfig,
  "window" | "roll_fn" | "align" | "min_obs"
>;

const SUMMARIES: Record<Exclude<RollFn, "none">, (v: number[]) => number> = {
  mean: (v) => v.reduce((a, b) => a + b, 0) / v.length,
  median: (v) => {
    const s = [...v].sort((a, b) => a - b);
    const m = s.length >> 1;
    return s.length % 2 ? (s[m] as number) : ((s[m - 1] as number) + (s[m] as number)) / 2;
  },
  min: (v) => Math.min(...v),
  max: (v) => Math.max(...v),
  sum: (v) => v.reduce((a, b) => a + b, 0),
};

/**
 * The rolling summary of `values` in their order: at each position, the
 * summary of the window's values that are present, or `null` when fewer than
 * `min_obs` are (unset: a majority of the window, `window` / 2 + 1 rounded
 * down, and the whole window for `"sum"`). A window counts observations, and
 * a position beyond either end is missing. A centered window of even width
 * holds one more following value than preceding, as data.table's centered
 * alignment does. With `roll_fn` `"none"`, every position is `null`.
 */
export function rollingValues(
  values: readonly (number | null)[],
  config: RollingMembers,
): (number | null)[] {
  const fn = config.roll_fn ?? "none";
  if (fn === "none") return values.map(() => null);
  const width = config.window ?? 7;
  const required = config.min_obs ?? (fn === "sum" ? width : Math.floor(width / 2) + 1);
  const align = config.align ?? "center";
  const offset = align === "right" ? width - 1 : align === "left" ? 0 : Math.floor((width - 1) / 2);
  return values.map((_, i) => {
    const start = Math.max(i - offset, 0);
    const end = Math.min(i - offset + width - 1, values.length - 1);
    const present: number[] = [];
    for (let k = start; k <= end; k++) {
      const v = values[k];
      if (typeof v === "number") present.push(v);
    }
    return present.length < required ? null : SUMMARIES[fn](present);
  });
}

/** One sorted series: its name, axis, times, and values. */
interface Sample {
  readonly name: string;
  readonly axis: 0 | 1;
  readonly x: number[];
  readonly y: (number | null)[];
}

/**
 * The time series chart's samples: per `y` column, then per `y2` column, the
 * rows with a time (`x`, or `x2` for `y2` when set), sorted by time; with
 * `group`, one sample per column and level, in level order. A sample is named
 * after its column, a `y2` column that is also a `y` column followed by
 * " (right)"; with `group`, after its level when only one column is bound, and
 * "column / level" otherwise. A missing value stays missing, a gap in the
 * series.
 */
function samples(
  data: TabularData,
  config: TimeSeriesChartConfig,
): {
  primary: Sample[];
  secondary: Sample[];
  time: boolean;
} {
  const x = config.x;
  const yColumns = asArray(config.y);
  if (!x || yColumns.length === 0) {
    throw new Error("A time series chart needs both `x` and `y` set to draw.");
  }
  const times = (name: string, member: string) => {
    const time = isTime(data, name);
    const values = column(data.columns, name, member).map((v, i) => {
      const t = time ? timeMs(v) : typeof v === "number" && !Number.isNaN(v) ? v : null;
      if (t !== null && !Number.isFinite(t)) {
        throw new Error(
          `Column '${name}' (config member '${member}') must hold finite times; row ${i + 1} holds '${String(v)}'.`,
        );
      }
      return t;
    });
    return { time, values };
  };
  const numbers = (name: string, member: string) =>
    column(data.columns, name, member).map((v, i) => {
      if (v === null || v === undefined || (typeof v === "number" && Number.isNaN(v))) return null;
      if (typeof v !== "number" || !Number.isFinite(v)) {
        throw new Error(
          `Column '${name}' (config member '${member}') must hold finite numbers or missing values; row ${i + 1} holds '${String(v)}'.`,
        );
      }
      return v;
    });
  const sample = (
    name: string,
    axis: 0 | 1,
    xs: (number | null)[],
    ys: (number | null)[],
    keep: (i: number) => boolean,
  ): Sample => {
    const rows = xs.flatMap((t, i) => (t !== null && keep(i) ? [i] : []));
    rows.sort((a, b) => (xs[a] as number) - (xs[b] as number));
    return { name, axis, x: rows.map((i) => xs[i] as number), y: rows.map((i) => ys[i] ?? null) };
  };

  const primaryX = times(x, "x");
  const y2Columns = asArray(config.y2);
  if (config.x2 && y2Columns.length === 0) {
    throw new Error("Supply `y2` with `x2`, or unset both.");
  }
  const secondaryX = config.x2 ? times(config.x2, "x2") : primaryX;
  if (secondaryX.time !== primaryX.time) {
    throw new Error("Use numeric time throughout, or dates and date-times throughout.");
  }
  const bound = [
    ...yColumns.map((name) => ({ name, label: name, axis: 0 as const, member: "y" })),
    ...y2Columns.map((name) => ({
      name,
      label: yColumns.includes(name) ? `${name} (right)` : name,
      axis: 1 as const,
      member: "y2",
    })),
  ];
  const split = (axis: 0 | 1) => {
    const xs = axis === 0 ? primaryX.values : secondaryX.values;
    const parts = bound.filter((b) => b.axis === axis);
    if (!config.group) {
      return parts.map((b) => sample(b.label, axis, xs, numbers(b.name, b.member), () => true));
    }
    const groups = textColumn(data, config.group, "group");
    const raw = column(data.columns, config.group, "group");
    const present = (i: number) => raw[i] !== null && raw[i] !== undefined;
    const levels = distinct(
      groups.filter((_, i) => present(i)),
      data.levels?.[config.group],
    );
    return parts.flatMap((b) => {
      const ys = numbers(b.name, b.member);
      return levels.map((level) =>
        sample(
          bound.length === 1 ? level : `${b.label} / ${level}`,
          axis,
          xs,
          ys,
          (i) => present(i) && groups[i] === level,
        ),
      );
    });
  };
  const primary = split(0);
  const secondary = split(1);
  if (![...primary, ...secondary].some((s) => s.y.some((v) => v !== null))) {
    throw new Error("A time series chart needs at least one time with a value.");
  }
  return { primary, secondary, time: primaryX.time };
}

/** Rows with a time, in time order: their indices. */
function sortedRows(xs: readonly (number | null)[]): number[] {
  const rows = xs.flatMap((t, i) => (t === null ? [] : [i]));
  return rows.sort((a, b) => (xs[a] as number) - (xs[b] as number));
}

/**
 * The ECharts option of a time series chart: per series, its raw
 * observations (points, or lines with `points` false) at `raw_opacity`, and
 * with `roll_fn` its rolling summary as a line under the same name, so one
 * legend entry toggles both; `y2` series on a right axis; with `group`, every
 * value column splits by group. The axis tooltip writes each series'
 * observation and summary on one row. Zeitgeber labels
 * (`zt`) replace the time axis labels at their own times, every
 * `show_zt_every`-th and every phase zero; `shade_bin` shades each run of
 * ones. The time axis is a time axis, in UTC, when `x` is a time column.
 * spec: draw/shared-builders#time-series
 */
export function buildTimeSeriesOption(
  data: TabularData,
  config: TimeSeriesChartConfig,
  appearance: TimeSeriesAppearance = {},
): Record<string, unknown> {
  const neutral = appearance.neutral ?? SIGN_COLORS.neutral;
  const { primary, secondary, time } = samples(data, config);
  const points = config.points ?? true;
  const rawOpacity = config.raw_opacity ?? 0.5;
  const pair = (xs: readonly number[], ys: readonly (number | null)[]) =>
    xs.map((t, i) => [t, ys[i] ?? null]);
  const series: Record<string, unknown>[] = [];
  const rolling = config.roll_fn ?? "none";
  // Series indices of the rolling lines, which the tooltip writes beside
  // their observations.
  const rolledIndices = new Set<number>();
  for (const s of [...primary, ...secondary]) {
    const raw = {
      type: "line",
      name: s.name,
      data: pair(s.x, s.y),
      showSymbol: points,
      // Filled, where ECharts' default `emptyCircle` fills with the canvas
      // background.
      symbol: "circle",
      symbolSize: 5,
      lineStyle: { width: points ? 0 : 1, opacity: rawOpacity },
      itemStyle: { opacity: rawOpacity },
      yAxisIndex: s.axis,
    };
    series.push(raw);
    if (rolling !== "none") {
      const rolled = rollingValues(s.y, config);
      if (rolled.some((v) => v !== null && !Number.isFinite(v))) {
        throw new Error("Rescale values to keep rolling summaries finite.");
      }
      rolledIndices.add(series.length);
      series.push({
        ...raw,
        data: pair(s.x, rolled),
        showSymbol: false,
        lineStyle: {
          width: config.roll_width ?? 2,
          ...(config.roll_color ? { color: config.roll_color } : {}),
        },
        itemStyle: { opacity: 1 },
      });
    }
  }

  const xAxis: Record<string, unknown> = {
    type: time ? "time" : "value",
    scale: true,
    ...(config.xlab ? { name: config.xlab } : {}),
  };
  if (config.zt || config.shade_bin) {
    if (config.x2) {
      throw new Error("Zeitgeber labels and shading follow `x`: unset `x2`.");
    }
  }
  const xs = time
    ? column(data.columns, config.x as string, "x").map(timeMs)
    : column(data.columns, config.x as string, "x").map((v) =>
        typeof v === "number" && Number.isFinite(v) ? v : null,
      );
  const order = sortedRows(xs);
  if (config.zt) {
    const every = config.show_zt_every ?? 1;
    const labels = column(data.columns, config.zt, "zt");
    labels.forEach((v, i) => {
      if (typeof v !== "number" || !Number.isFinite(v)) {
        throw new Error(
          `Column '${config.zt}' (config member 'zt') must hold a finite number in every row; row ${i + 1} holds '${String(v)}'.`,
        );
      }
    });
    const keep = order.filter((row, k) => k % every === 0 || labels[row] === 0);
    xAxis.axisLabel = { show: false };
    series.push({
      type: "custom",
      name: "",
      renderItem: AXIS_LABELS,
      // The renderer draws the labels in the series color.
      itemStyle: { color: neutral },
      silent: true,
      clip: false,
      dimensions: [
        { name: "time", type: "float" },
        { name: "text", type: "ordinal" },
      ],
      encode: { x: 0, y: -1, tooltip: -1 },
      data: keep.map((row) => [xs[row], String(labels[row])]),
    });
  }
  if (config.shade_bin) {
    const flags = column(data.columns, config.shade_bin, "shade_bin");
    flags.forEach((v, i) => {
      if (v !== 0 && v !== 1) {
        throw new Error(
          `Column '${config.shade_bin}' (config member 'shade_bin') must hold 0 or 1 in every row; row ${i + 1} holds '${String(v)}'.`,
        );
      }
    });
    const runs: { on: boolean; start: number }[] = [];
    order.forEach((row, k) => {
      const on = flags[row] === 1;
      if (runs[runs.length - 1]?.on !== on) runs.push({ on, start: k });
    });
    // A run spans its first and last observation.
    const area = runs.flatMap((run, r) => {
      if (!run.on) return [];
      const next = runs[r + 1];
      const end = next ? next.start - 1 : order.length - 1;
      return [
        [
          { xAxis: xs[order[run.start] as number], itemStyle: { color: neutral, opacity: 0.15 } },
          { xAxis: xs[order[end] as number] },
        ],
      ];
    });
    if (area.length > 0 && series[0]) {
      series[0].markArea = { silent: true, label: { show: false }, data: area };
    }
  }

  const yAxis: Record<string, unknown>[] = [
    {
      type: "value",
      scale: true,
      ...(config.ylab ? { name: config.ylab } : {}),
    },
  ];
  if (secondary.length > 0) {
    yAxis.push({
      type: "value",
      scale: true,
      position: "right",
      ...(config.y2lab ? { name: config.y2lab } : {}),
      splitLine: { show: false },
    });
  }
  const grid = cartesianGrid({}, Boolean(config.xlab || config.ylab || config.y2lab));
  const dataZoom = config.zoom ? axisZoom("x", grid) : undefined;
  const palette = asArray(config.palette);
  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    ...(time ? { useUTC: true } : {}),
    ...(palette.length > 0 ? { color: [...palette] } : {}),
    tooltip: {
      trigger: "axis",
      formatter: (params: unknown) => axisTooltip(params, rolledIndices, rolling),
    },
    legend: {
      data: [...primary, ...secondary].map((s) => s.name),
      itemStyle: { opacity: 1 },
    },
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    xAxis,
    // One left axis is an object, as in the other builders; with `y2`, the
    // two axes are an array.
    yAxis: yAxis.length === 1 ? yAxis[0] : yAxis,
    series,
  };
}

/** One series' entry in an axis tooltip, as ECharts passes it. */
interface TooltipParam {
  readonly seriesName?: string;
  readonly seriesIndex?: number;
  readonly value?: unknown;
  readonly marker?: string;
  readonly axisValueLabel?: string;
}

const escapeHtml = (text: string) => text.replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);

/**
 * The axis tooltip of a time series chart: the time, then one row per series
 * name with its observation and, with a rolling summary, the summary beside
 * it ("mean 0.512"), so a series and its rolling line read as one row.
 */
export function axisTooltip(
  params: unknown,
  rolledIndices: ReadonlySet<number>,
  rolling: RollFn,
): string {
  const list = [params].flat() as TooltipParam[];
  if (list.length === 0) return "";
  const rows = new Map<string, { marker: string; raw?: number; roll?: number }>();
  for (const p of list) {
    const y = Array.isArray(p.value) ? p.value[1] : p.value;
    if (typeof y !== "number" || !Number.isFinite(y)) continue;
    const name = p.seriesName ?? "";
    const row = rows.get(name) ?? { marker: p.marker ?? "" };
    if (rolledIndices.has(p.seriesIndex ?? -1)) row.roll = y;
    else row.raw = y;
    rows.set(name, row);
  }
  const value = (v: number | undefined) => (v === undefined ? "–" : String(tooltipValue(v)));
  const lines = [...rows].map(([name, row]) => {
    const summary =
      row.roll === undefined
        ? ""
        : ` <span style="opacity:0.7">${rolling} ${value(row.roll)}</span>`;
    return `${row.marker}${escapeHtml(name)}&nbsp;&nbsp;<b>${value(row.raw)}</b>${summary}`;
  });
  return [escapeHtml(list[0]?.axisValueLabel ?? ""), ...lines].join("<br/>");
}
