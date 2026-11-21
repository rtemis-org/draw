import {
  FIT_MEMBERS,
  type FitCurve,
  type FitMembers,
  type FitTarget,
  fitSeries,
} from "../../shared/fit.ts";
import { tooltipValue } from "../../shared/format.ts";
import { axisZoom, cartesianGrid, type MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import {
  type AxisLimits,
  calcLimits,
  calcLimitsFromZero,
  DEFAULT_PAD,
  equalAxisLimits,
} from "../../shared/limits.ts";
import {
  asArray,
  type ColumnNames,
  column,
  distinct,
  isTime,
  numberOrNull,
  type TabularData,
  textColumn,
  timeMs,
} from "../../shared/source.ts";
import { RTEMIS_PALETTE } from "../../theme/tokens.ts";

/** A dash pattern of a line. */
export type LineDash = "solid" | "dashed" | "dotted";

/**
 * An instance of the `chart/line/v1` schema, written by rtemis.draw's
 * `LineConfig` class. Every member is optional. spec: draw/shared-builders#line
 */
export interface LineChartConfig extends MarginMembers, LegendMembers, FitMembers {
  readonly type?: "line";
  /**
   * Column along the horizontal axis. A numeric column gets a value axis, a
   * time column a time axis, and any other a category axis.
   */
  readonly x?: string | null;
  /** Columns to plot, one line each. */
  readonly y?: ColumnNames | null;
  /** Column whose values split each `y` column into one line per group. */
  readonly group?: string | null;
  /**
   * How the `y` values of rows sharing an x and group combine into one point.
   * The host applies it to the data before building (rtemislive in DuckDB,
   * rtemis.draw in R); the builder requires one row per x in each line.
   * Default `"none"`.
   */
  readonly aggregate?: "none" | "sum" | "mean" | "median";
  /** Column whose contiguous runs shade vertical background bands. */
  readonly blocks?: string | null;
  /** Draw smoothed lines. Default `false`. */
  readonly smooth?: boolean;
  /** Fill the area under each line; unset y limits then hold zero. Default `false`. */
  readonly area?: boolean;
  /**
   * Show a marker at each value. Default `false`: a point with no neighbor to
   * join keeps its marker, so no value goes undrawn.
   */
  readonly points?: boolean;
  /**
   * Stack the lines, each on the sum of those before it. Requires `area`,
   * since the bands are what show each line's share. Default `false`.
   */
  readonly stack?: boolean;
  /** A slider, Shift+wheel zoom, and drag panning on the x axis. Default `false`. */
  readonly zoom?: boolean;
  /** Line colors, one per line, recycled; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Dash pattern of each line, recycled; unset draws solid lines. */
  readonly line_style?: LineDash | readonly LineDash[] | null;
  /** Band colors, one per level of `blocks`; `null` or `"transparent"` draws no band. */
  readonly block_color?: string | readonly (string | null)[] | null;
  /** Band opacity, `[0, 1]`. Default `0.2`. */
  readonly block_opacity?: number;
  /** Draw the plotting box square. The host sizes the box from {@link lineAspect}. */
  readonly square?: boolean;
  /** Give one data unit the same length on both axes. Requires a numeric x. */
  readonly equal_axes?: boolean;
  /** Fraction of the data range each axis extends by when its limits are unset. Default `0.04`. */
  readonly pad?: number;
  /** X axis limits, `[min, max]`; on a time axis, epoch milliseconds. Unset pads the data range. */
  readonly xlim?: readonly [number, number] | null;
  /** Y axis limits, `[min, max]`. Unset pads the data range. */
  readonly ylim?: readonly [number, number] | null;
  /** X axis name; unset draws none. */
  readonly xlab?: string | null;
  /** Y axis name; unset draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/**
 * A line chart's data: the bound columns, and the curves the host fitted when
 * `fit` is set. spec: draw/shared-builders#fits
 */
export interface LineData extends TabularData {
  /** One curve per line, or one per `y` column when pooled; unset draws no fit. */
  readonly fits?: readonly FitCurve[];
}

/** What is not chart config: the theme palette and animation. */
export interface LineAppearance {
  /** The theme's categorical palette, which colors the fits; unset uses rtemis.draw's. */
  readonly palette?: readonly string[];
  /** Animate the first render and updates; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** Every member of {@link LineChartConfig}, checked against `chart/line/v1`. */
export const LINE_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "y",
  "group",
  "aggregate",
  "blocks",
  "smooth",
  "area",
  "points",
  "stack",
  "zoom",
  "palette",
  "line_style",
  "block_color",
  "block_opacity",
  "square",
  "equal_axes",
  "pad",
  "xlim",
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
  ...FIT_MEMBERS,
] as const satisfies readonly (keyof LineChartConfig)[]);

type MissingMember = Exclude<keyof LineChartConfig, (typeof LINE_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

type XType = "value" | "time" | "category";

interface LineSeries {
  readonly name: string;
  /** The `y` column and the group level the line draws. */
  readonly column: string;
  readonly group: string | null;
  /** Row indices of the series, in row order. */
  readonly rows: readonly number[];
  readonly y: readonly (number | null)[];
}

/**
 * Build the ECharts option of a line chart from tabular data and its
 * `chart/line/v1` config. One line per `y` column, or per `y` column and
 * group, through the rows in data order, each line holding one row per x.
 * A stacked line has no point where a line below it has none, since the sum
 * there is unknown. Unset axis limits extend the data
 * range by `pad` on each side; a stacked chart's y range is that of the sums,
 * and a filled area's y range holds zero, unpadded on that side.
 */
export function buildLineOption(
  data: LineData,
  config: LineChartConfig,
  appearance: LineAppearance = {},
): Record<string, unknown> {
  const yColumns = asArray(config.y);
  if (!config.x || yColumns.length === 0) {
    throw new Error("A line chart needs both `x` and `y` set to draw.");
  }
  const xType = xAxisType(data, config.x);
  if (config.group && config.blocks) {
    throw new Error(
      "`blocks` shades runs along `x`, and with `group` each x repeats once per group: bind one of the two.",
    );
  }
  if (config.equal_axes && xType !== "value") {
    throw new Error(
      `\`equal_axes\` compares the two axes' scales, which needs a numeric x. This chart's x is ${xType === "time" ? "a date or time" : "categorical"}.`,
    );
  }
  if (config.xlim && xType === "category") {
    throw new Error("`xlim` only applies when `x` is numeric or a date.");
  }
  if (config.stack && config.equal_axes) {
    throw new Error(
      "A stacked y axis shows sums, so `equal_axes` cannot apply: unset one of the two.",
    );
  }
  if (config.fit && (config.aggregate ?? "none") !== "none") {
    throw new Error(
      "A fit models observations, and with `aggregate` each point is a summary of several: set `aggregate` to none, or unset `fit`.",
    );
  }
  if (config.fit && xType !== "value") {
    throw new Error(
      "A fit needs a numeric x; this chart's x is " +
        (xType === "time" ? "a date or time." : "categorical."),
    );
  }
  if (config.fit && config.area) {
    throw new Error("A fit is drawn over lines, and `area` fills them: unset one of the two.");
  }
  if (config.stack && !config.area) {
    throw new Error(
      "`stack` draws each line on the sum of those below it, which only filled bands show: set `area` too.",
    );
  }

  const rawX = column(data.columns, config.x, "x");
  const xNum = xType === "category" ? null : rawX.map(xType === "time" ? timeMs : numberOrNull);
  const xText = xType === "category" ? textColumn(data, config.x, "x") : null;
  const categories = xText ? distinct(xText, data.levels?.[config.x]) : null;
  const xKey = (row: number) => (xNum ? xNum[row] : xText?.[row]) ?? null;
  const series = lineSeries(data, config, yColumns, xKey);
  for (const s of series) requireOneRowPerX(s, xKey, xText !== null);
  if (config.stack) stackOnKnownSums(series, xKey);

  const pad = config.pad ?? DEFAULT_PAD;
  const yValues = series.flatMap((s) => s.y);
  const limits = equalAxisLimits(
    xNum,
    yValues,
    config.xlim ?? null,
    config.ylim ?? null,
    pad,
    config.square ?? false,
    config.equal_axes ?? false,
  );
  const xLim = xNum ? (limits.xlim ?? calcLimits(xNum, pad)) : null;
  // A filled area is read by its height from zero, so its axis holds zero.
  const yDerive = config.area ? calcLimitsFromZero : calcLimits;
  const yLim = limits.ylim ?? yDerive(config.stack ? stackedValues(series, xKey) : yValues, pad);

  const memberPalette = asArray(config.palette);
  // A fit takes its line's color, so with a fit every line's color is explicit.
  const palette =
    memberPalette.length > 0 || !config.fit
      ? memberPalette
      : (appearance.palette ?? RTEMIS_PALETTE);
  const targets: FitTarget[] = series.map((s, i) => ({
    y: s.column,
    group: s.group,
    name: series.length === 1 ? null : s.name,
    color: palette.length > 0 ? (palette[i % palette.length] as string) : "",
  }));
  const fits = config.fit ? fitSeries(data.fits ?? [], targets, config, "dashed") : null;
  const dashes = asArray(config.line_style);
  const at = categories ? new Map(categories.map((c, i) => [c, i])) : null;
  const inOrder =
    xText !== null &&
    categories !== null &&
    !config.group &&
    xText.length === categories.length &&
    xText.every((x, i) => x === categories[i]);
  const builtSeries: Record<string, unknown>[] = series.map((s, i) => {
    const points = s.rows.map((row, j) => {
      const y = s.y[j] ?? null;
      if (inOrder) return y;
      if (at && xText) return [at.get(xText[row] as string) ?? null, y];
      return [xNum?.[row] ?? null, y];
    });
    // Without markers a point with no neighbor draws nothing, so it keeps its
    // marker and every other point is marked "none".
    const isolated = config.points ? [] : isolatedPoints(s.y);
    const marked = isolated.length > 0;
    const values = marked
      ? points.map((value, j) => (isolated.includes(j) ? value : { value, symbol: "none" }))
      : points;
    const color = palette.length > 0 ? palette[i % palette.length] : undefined;
    const dash = dashes.length > 0 ? dashes[i % dashes.length] : undefined;
    const name = fits?.names.get(targets[i] as FitTarget) ?? (series.length === 1 ? null : s.name);
    return {
      type: "line",
      symbol: "circle",
      ...(name === null ? {} : { name }),
      data: values,
      ...(config.smooth ? { smooth: true } : {}),
      ...(config.points || marked ? {} : { showSymbol: false }),
      ...(config.stack ? { stack: "total" } : {}),
      ...(config.area ? { areaStyle: {} } : {}),
      ...(color === undefined ? {} : { itemStyle: { color } }),
      ...(dash === undefined ? {} : { lineStyle: { type: dash } }),
    };
  });

  if (config.blocks) {
    const markArea = blockMarkArea(data, config, config.blocks, xNum, xText);
    if (markArea && builtSeries[0]) builtSeries[0].markArea = markArea;
  }

  const xAxis: Record<string, unknown> = {
    type: xType,
    ...(config.xlab ? { name: config.xlab } : {}),
    ...(categories ? { data: categories } : {}),
    ...(xType === "value" ? { scale: true } : {}),
    ...(xLim ? { min: xLim[0], max: xLim[1] } : {}),
    ...(xType === "category" ? {} : { splitLine: NO_CORNER_LINES }),
    ...(xType === "time" ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {}),
    ...(yLim ? { axisLine: zeroAxisLine(yLim) } : {}),
  };
  const yAxis: Record<string, unknown> = {
    type: "value",
    ...(config.ylab ? { name: config.ylab } : {}),
    scale: true,
    ...(yLim ? { min: yLim[0], max: yLim[1] } : {}),
    splitLine: NO_CORNER_LINES,
    ...(xLim ? { axisLine: zeroAxisLine(xLim) } : {}),
  };

  const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
  const dataZoom = config.zoom ? axisZoom("x", grid) : undefined;
  const legendNames = [
    ...new Set(
      [...builtSeries, ...(fits?.series ?? [])]
        .map((s) => s.name)
        .filter((n): n is string => typeof n === "string"),
    ),
  ];

  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: { trigger: "axis", valueFormatter: tooltipValue },
    ...(legendNames.length > 0 ? { legend: { data: legendNames } } : {}),
    xAxis,
    yAxis,
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    ...(xType === "time" ? { useUTC: true } : {}),
    series: [...builtSeries, ...(fits?.series ?? [])],
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };
}

// The first and last split lines sit on the padded limits, not on ticks.
const NO_CORNER_LINES = Object.freeze({ showMinLine: false, showMaxLine: false });

/**
 * The axis line of one axis, drawn at zero of the other axis when the other
 * axis's range holds zero; ECharts otherwise draws it at the plot edge.
 */
function zeroAxisLine(otherLimits: AxisLimits): Record<string, unknown> {
  return { show: otherLimits[0] <= 0 && otherLimits[1] >= 0, onZero: true };
}

function xAxisType(data: TabularData, name: string): XType {
  if (isTime(data, name)) return "time";
  const values = column(data.columns, name, "x");
  if (!Array.isArray(data.columns[name])) return "value";
  const present = values.filter((v) => v !== null && v !== undefined);
  return present.length > 0 && present.every((v) => typeof v === "number") ? "value" : "category";
}

/** The lines: one per `y` column, or per group and `y` column, groups in level order. */
function lineSeries(
  data: TabularData,
  config: LineChartConfig,
  yColumns: readonly string[],
  xKey: (row: number) => unknown,
): LineSeries[] {
  const ys = new Map(yColumns.map((y) => [y, column(data.columns, y, "y").map(numberOrNull)]));
  // A row without an x has no place on the axis.
  const allRows = (ys.get(yColumns[0] as string) ?? [])
    .map((_, i) => i)
    .filter((row) => xKey(row) !== null);
  const select = (y: string, rows: readonly number[]) => {
    const values = ys.get(y) ?? [];
    return rows.map((row) => values[row] ?? null);
  };
  if (!config.group) {
    return yColumns.map((y) => ({
      name: y,
      column: y,
      group: null,
      rows: allRows,
      y: select(y, allRows),
    }));
  }
  const raw = column(data.columns, config.group, "group");
  const labels = textColumn(data, config.group, "group");
  const missing = (v: unknown) => v === null || v === undefined || Number.isNaN(v);
  const present = labels.filter((_, row) => !missing(raw[row]));
  const groups = distinct(present, data.levels?.[config.group]);
  const multiY = yColumns.length > 1;
  return groups.flatMap((g) => {
    const rows = allRows.filter((row) => !missing(raw[row]) && labels[row] === g);
    return yColumns.map((y) => ({
      name: groups.length > 1 ? (multiY ? `${y} / ${g}` : g) : y,
      column: y,
      group: g,
      rows,
      y: select(y, rows),
    }));
  });
}

/**
 * Stop when a line holds two rows at one x: a line joins one value per x, and
 * two would draw a spike whose order is arbitrary.
 */
function requireOneRowPerX(
  line: LineSeries,
  xKey: (row: number) => unknown,
  category: boolean,
): void {
  const seen = new Set<unknown>();
  for (const row of line.rows) {
    const key = xKey(row);
    if (seen.has(key)) {
      const shown = category ? `'${String(key)}'` : String(key);
      throw new Error(
        `Several rows hold x = ${shown} in line '${line.name}'. Combine them before building, as the \`aggregate\` member says (sum, mean, or median).`,
      );
    }
    seen.add(key);
  }
}

/**
 * Remove each stacked point whose base is unknown: a line's value at an x
 * stacks on the lines below it, and where one of them has no value there the
 * sum is unknown. ECharts would stack it on the next line down, drawing a
 * total that omits the missing value.
 */
function stackOnKnownSums(series: LineSeries[], xKey: (row: number) => unknown): void {
  const known: Set<unknown>[] = [];
  series.forEach((s, i) => {
    const below = known.slice(0, i);
    const y = s.y.map((value, j) =>
      value !== null && below.every((keys) => keys.has(xKey(s.rows[j] as number))) ? value : null,
    );
    series[i] = { ...s, y };
    known.push(new Set(s.rows.filter((_, j) => y[j] !== null).map((row) => xKey(row))));
  });
}

/**
 * The values a stacked y axis spans: each line's running sum over the lines
 * below it at the same x, with positive values and negative values stacked
 * separately, as ECharts stacks them.
 */
function stackedValues(
  series: readonly LineSeries[],
  xKey: (row: number) => unknown,
): (number | null)[] {
  const tops = { positive: new Map<unknown, number>(), negative: new Map<unknown, number>() };
  const out: (number | null)[] = [];
  for (const s of series) {
    s.rows.forEach((row, j) => {
      const y = s.y[j];
      if (y === null || y === undefined) return;
      const key = xKey(row);
      const side = y >= 0 ? "positive" : "negative";
      const value = (tops[side].get(key) ?? 0) + y;
      tops[side].set(key, value);
      out.push(value);
    });
  }
  return out;
}

/** Positions of the values with no non-missing neighbor in data order. */
function isolatedPoints(y: readonly (number | null)[]): number[] {
  const present = (j: number) => y[j] !== null && y[j] !== undefined;
  return y.flatMap((_, j) => (present(j) && !present(j - 1) && !present(j + 1) ? [j] : []));
}

/**
 * Vertical bands over the contiguous runs of `blocks` along the rows, one
 * color per level: the column's levels that occur, or its sorted distinct
 * values. On a numeric or time x each band extends to the start of the next
 * drawn band, so adjacent bands meet; on a category axis a band covers whole
 * categories.
 */
function blockMarkArea(
  data: TabularData,
  config: LineChartConfig,
  blocks: string,
  xNum: readonly (number | null)[] | null,
  xText: readonly string[] | null,
): Record<string, unknown> | null {
  const colors = asArray(config.block_color);
  if (colors.length === 0) {
    throw new Error("`block_color` must be set when `blocks` is.");
  }
  const raw = column(data.columns, blocks, "blocks");
  const missing = (v: unknown) => v === null || v === undefined || Number.isNaN(v);
  const keys = raw.map((v) => (missing(v) ? null : String(v)));
  const present = new Set(keys.filter((k): k is string => k !== null));
  const declared = data.levels?.[blocks];
  const levels = declared
    ? declared.filter((level) => present.has(level))
    : sortedLevels(raw.filter((v) => !missing(v)));
  if (colors.length !== levels.length) {
    throw new Error(
      `\`block_color\` must have length ${levels.length} (one per level of \`blocks\`); it has ${colors.length}.`,
    );
  }
  const colorOf = new Map(levels.map((level, i) => [level, colors[i] ?? null]));
  const drawn = (key: string | null) => {
    if (key === null) return false;
    const color = colorOf.get(key);
    return typeof color === "string" && color !== "transparent";
  };
  const position = (row: number) => (xNum ? xNum[row] : xText?.[row]) ?? null;

  const runs: { key: string | null; start: number; end: number }[] = [];
  keys.forEach((key, row) => {
    const last = runs[runs.length - 1];
    if (last && last.key === key) last.end = row;
    else runs.push({ key, start: row, end: row });
  });
  const pairs = runs.flatMap((run, i) => {
    if (!drawn(run.key)) return [];
    const next = runs[i + 1];
    const end = xNum && next && drawn(next.key) ? next.start : run.end;
    return [
      [
        {
          xAxis: position(run.start),
          name: run.key,
          itemStyle: {
            color: colorOf.get(run.key as string),
            opacity: config.block_opacity ?? 0.2,
          },
        },
        { xAxis: position(end) },
      ],
    ];
  });
  return pairs.length > 0 ? { data: pairs, silent: true } : null;
}

/** Distinct values in ascending order, numbers by value and text by collation. */
function sortedLevels(values: readonly unknown[]): string[] {
  const unique = [...new Set(values)];
  if (unique.every((v) => typeof v === "number")) {
    return (unique as number[]).sort((a, b) => a - b).map(String);
  }
  return unique.map(String).sort((a, b) => a.localeCompare(b));
}

/** A fixed-aspect plotting box's ratio and the pixel margins around it. */
export interface AspectHint {
  /** Height over width of the plotting box. */
  readonly ratio: number;
  readonly leftPx: number;
  readonly rightPx: number;
  readonly topPx: number;
  readonly botPx: number;
}

/**
 * The plotting box a host fits for a line chart with `square` or `equal_axes`,
 * read from its built option, or `null` without either. `square` asks for a
 * ratio of 1; `equal_axes` for the y span over the x span, so one data unit
 * has the same length on both axes. The host solves the box from its width.
 */
export function lineAspect(
  option: Record<string, unknown>,
  config: Pick<LineChartConfig, "square" | "equal_axes">,
): AspectHint | null {
  const square = config.square ?? false;
  const equal = config.equal_axes ?? false;
  if (!square && !equal) return null;
  const request = square ? "square" : "equal_axes";
  const span = (axis: unknown, which: string): AxisLimits => {
    const a = (axis ?? {}) as { type?: string; min?: unknown; max?: unknown };
    if (typeof a.min !== "number" || typeof a.max !== "number") {
      throw new Error(
        `\`${request}\` needs a numeric ${which} axis with known limits, and this chart's is ${a.type === "category" ? "categorical" : "unbounded"}. Plot numeric values, or set \`${which}lim\`.`,
      );
    }
    return [a.min, a.max];
  };
  const x = span(option.xAxis, "x");
  const y = span(option.yAxis, "y");
  const grid = (option.grid ?? {}) as Record<string, unknown>;
  const px = (side: string): number => {
    const value = grid[side];
    if (typeof value !== "number" || !Number.isFinite(value)) {
      throw new Error(
        `\`square\` and \`equal_axes\` need all four margins as pixel numbers, and \`${side}\` is ${value === undefined ? "unset" : `'${String(value)}'`}. Set the four \`margin_*\` members.`,
      );
    }
    return value;
  };
  return {
    ratio: equal ? (y[1] - y[0]) / (x[1] - x[0]) : 1,
    leftPx: px("left"),
    rightPx: px("right"),
    topPx: px("top"),
    botPx: px("bottom"),
  };
}
