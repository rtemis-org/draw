import {
  COLOR_SCALE_LENGTH,
  KEY_COLUMN_WIDTH,
  type SizeKeySpec,
  sizeKeyElement,
  sizeKeyValues,
} from "../../layout/keys.ts";
import {
  FIT_MEMBERS,
  type FitCurve,
  type FitMembers,
  type FitTarget,
  fitSeries,
} from "../../shared/fit.ts";
import { formatNumber } from "../../shared/format.ts";
import { cartesianGrid, type MarginMembers, xyZoom } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { type AxisLimits, calcLimits, DEFAULT_PAD, equalAxisLimits } from "../../shared/limits.ts";
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
import { RTEMIS_PALETTE, SIGN_COLORS } from "../../theme/tokens.ts";
import { RUG } from "./renderers.ts";

/**
 * An instance of the `chart/scatter/v1` schema, written by rtemis.draw's
 * `ScatterConfig` class. Every member is optional.
 * spec: draw/shared-builders#scatter
 */
export interface ScatterChartConfig extends MarginMembers, LegendMembers, FitMembers {
  readonly type?: "scatter";
  /** Column along the horizontal axis: numeric, or a time column. */
  readonly x?: string | null;
  /** Numeric columns along the vertical axis, one series each. */
  readonly y?: ColumnNames | null;
  /** Column whose values split each `y` column into one series per group. */
  readonly group?: string | null;
  /**
   * Nonnegative numeric column that sets each symbol's area in proportion to
   * its value, the largest value at five times `point_size` across.
   */
  readonly size?: string | null;
  /** Numeric column mapped to a continuous color scale; excludes `group`. */
  readonly color?: string | null;
  /**
   * The color scale of `color`: `"sequential"` from the lowest to the highest
   * value, or `"diverging"` centered on zero. Default `"sequential"`.
   */
  readonly color_scale?: "sequential" | "diverging";
  /** Colors of the color scale, low to high; unset uses the theme's sign colors. */
  readonly colormap?: ColumnNames | null;
  /** Title of the color scale; unset uses the `color` column's name. */
  readonly color_title?: string | null;
  /** Title of the size key; unset uses the `size` column's name. */
  readonly size_title?: string | null;
  /** Column of literal tooltip labels, one per row. */
  readonly hover?: string | null;
  /** Marks along both axes at each point. Default `false`. */
  readonly rug?: boolean;
  /** The identity line within the axis limits. Default `false`. */
  readonly diagonal?: boolean;
  /** Identity-line color; unset uses a neutral gray. */
  readonly diagonal_color?: string | null;
  /** Shift+wheel zoom, drag panning, and box-zoom controls on both axes. Default `false`. */
  readonly zoom?: boolean;
  /**
   * Symbol diameter in pixels of points without `size`; with `size`, the
   * largest value's symbol is five times it across. Default `6`.
   */
  readonly point_size?: number;
  /** Point opacity, `[0, 1]`; unset sets it from the number of points ({@link autoAlpha}). */
  readonly point_alpha?: number | null;
  /** Series colors, recycled; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Draw the plotting box square. The host sizes the box from the aspect hint. */
  readonly square?: boolean;
  /** Give one data unit the same length on both axes. */
  readonly equal_axes?: boolean;
  /** Fraction of the data range each axis extends by when its limits are unset. Default `0.04`. */
  readonly pad?: number;
  /** X axis limits, `[min, max]`; on a time axis, epoch milliseconds. */
  readonly xlim?: readonly [number, number] | null;
  /** Y axis limits, `[min, max]`. */
  readonly ylim?: readonly [number, number] | null;
  /** X axis name; unset draws none. */
  readonly xlab?: string | null;
  /** Y axis name; unset draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** A scatter chart's data: the bound columns, and the curves the host fitted. */
export interface ScatterData extends TabularData {
  /** One curve per series, or one per `y` column when pooled; unset draws no fit. */
  readonly fits?: readonly FitCurve[];
}

/** What is not chart config: the theme palette and animation. */
export interface ScatterAppearance {
  /** The theme's categorical palette; unset uses rtemis.draw's. */
  readonly palette?: readonly string[];
  /** Animate the first render and updates; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** Every member of {@link ScatterChartConfig}, checked against `chart/scatter/v1`. */
export const SCATTER_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "y",
  "group",
  "size",
  "color",
  "color_scale",
  "colormap",
  "color_title",
  "size_title",
  "hover",
  "rug",
  "diagonal",
  "diagonal_color",
  "zoom",
  "point_size",
  "point_alpha",
  "palette",
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
] as const satisfies readonly (keyof ScatterChartConfig)[]);

type MissingMember = Exclude<
  keyof ScatterChartConfig,
  (typeof SCATTER_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/** Diameter in pixels of the symbol of the largest `size` value at the default `point_size`. */
export const MAX_SYMBOL_SIZE = 30;

/** Default symbol diameter in pixels of points without `size`. */
export const DEFAULT_POINT_SIZE = 6;

/**
 * Point opacity from the number of points drawn, a guard against
 * overplotting: 0.75 at 10 points, 0.45 at 1,000, and 0.3 at 10,000, within
 * `[0.15, 0.9]`. rtemis.draw's `auto_alpha()` is the same rule.
 */
export function autoAlpha(n: number): number {
  return Math.min(0.9, Math.max(0.15, 0.9 - 0.15 * Math.log10(Math.max(n, 1))));
}

interface PointSeries extends FitTarget {
  /** Row indices drawn, in row order. */
  readonly rows: readonly number[];
}

/**
 * Build the ECharts option of a scatter chart from tabular data and its
 * `chart/scatter/v1` config. One series per `y` column, or per `y` column and
 * group; a row is drawn when every bound column has a value. Fits are the
 * host's curves in `data.fits`, drawn in the color and under the legend entry
 * of their series. Unset axis limits extend the range of the points by `pad`
 * on each side.
 */
export function buildScatterOption(
  data: ScatterData,
  config: ScatterChartConfig,
  appearance: ScatterAppearance = {},
): Record<string, unknown> {
  const yColumns = asArray(config.y);
  if (!config.x || yColumns.length === 0) {
    throw new Error("A scatter chart needs both `x` and `y` set to draw.");
  }
  if (config.color && config.group) {
    throw new Error("`color` and `group` both set the color of a point: bind one of the two.");
  }
  if (config.color && yColumns.length > 1) {
    throw new Error(
      "With `color`, a point's color shows its `color` value, so points of several `y` columns cannot be told apart: bind one `y` column.",
    );
  }
  const time = isTime(data, config.x);
  const xValues = time
    ? column(data.columns, config.x, "x").map(timeMs)
    : numberColumn(data, config.x, "x");
  if (config.equal_axes && time) {
    throw new Error("`equal_axes` compares the two axes' scales, which needs a numeric x.");
  }
  const ys = new Map(yColumns.map((y) => [y, numberColumn(data, y, "y")]));
  const size = config.size ? numberColumn(data, config.size, "size") : null;
  if (size?.some((v) => v !== null && v < 0)) {
    throw new Error(
      `Column '${config.size}' (config member 'size') has negative values; a symbol's area shows a nonnegative quantity.`,
    );
  }
  const colorValues = config.color ? numberColumn(data, config.color, "color") : null;
  const hover = config.hover ? column(data.columns, config.hover, "hover") : null;

  const memberPalette = asArray(config.palette);
  const palette = memberPalette.length > 0 ? memberPalette : (appearance.palette ?? RTEMIS_PALETTE);
  // With a color scale, a point's color is a value: the fits, the rug, and
  // the size key take the neutral color, which belongs to no value.
  const series = pointSeries(data, config, yColumns, xValues, ys, [size, colorValues], palette).map(
    (s) => (colorValues ? { ...s, color: SIGN_COLORS.neutral } : s),
  );
  if (series.every((s) => s.rows.length === 0)) {
    throw new Error("A scatter chart needs at least one row with every bound column set.");
  }

  const pad = config.pad ?? DEFAULT_PAD;
  const drawnX = series.flatMap((s) => s.rows.map((row) => xValues[row]));
  const drawnY = series.flatMap((s) => s.rows.map((row) => ys.get(s.y)?.[row] ?? null));
  const limits = equalAxisLimits(
    drawnX,
    drawnY,
    config.xlim ?? null,
    config.ylim ?? null,
    pad,
    config.square ?? false,
    config.equal_axes ?? false,
  );
  const xLim = limits.xlim ?? calcLimits(drawnX, pad);
  const yLim = limits.ylim ?? calcLimits(drawnY, pad);

  const fits = config.fit ? fitSeries(data.fits ?? [], series, config) : null;
  const nameOf = (s: PointSeries) => fits?.names.get(s) ?? s.name;
  const alpha = config.point_alpha ?? autoAlpha(drawnX.length);
  const maxSize = size ? Math.max(0, ...series.flatMap((s) => s.rows.map((r) => size[r] ?? 0))) : 0;
  // `point_size` scales the mapped symbols too: the largest value draws at
  // MAX_SYMBOL_SIZE / DEFAULT_POINT_SIZE (5) times it, 30 pixels at 6.
  const largestSymbol =
    (MAX_SYMBOL_SIZE * (config.point_size ?? DEFAULT_POINT_SIZE)) / DEFAULT_POINT_SIZE;
  const symbolSize = (row: number) => {
    const v = size?.[row] ?? 0;
    return maxSize > 0 ? largestSymbol * Math.sqrt(v / maxSize) : 0;
  };
  const points: Record<string, unknown>[] = series.map((s) => {
    const y = ys.get(s.y) ?? [];
    const values = s.rows.map((row) => {
      // The color value is dimension 2, which the color scale reads; the
      // size value follows, for the tooltip.
      const value = [
        xValues[row],
        y[row],
        ...(colorValues ? [colorValues[row]] : []),
        ...(size ? [size[row]] : []),
      ];
      if (!size && !hover) return value;
      return {
        value,
        ...(size ? { symbolSize: symbolSize(row) } : {}),
        ...(hover ? { name: String(hover[row]) } : {}),
      };
    });
    const name = nameOf(s);
    return {
      type: "scatter",
      ...(name === null ? {} : { name }),
      data: values,
      ...(size ? {} : { symbolSize: config.point_size ?? DEFAULT_POINT_SIZE }),
      itemStyle: { ...(colorValues ? {} : { color: s.color }), opacity: alpha },
    };
  });

  const rugs = config.rug
    ? series.map((s) => {
        const y = ys.get(s.y) ?? [];
        const name = nameOf(s);
        return {
          type: "custom",
          ...(name === null ? {} : { name }),
          renderItem: RUG,
          silent: true,
          clip: true,
          itemStyle: { color: s.color },
          data: s.rows.map((row) => [xValues[row], y[row]]),
          encode: { x: 0, y: 1 },
        };
      })
    : [];
  const diagonal = config.diagonal && xLim && yLim ? diagonalSeries(xLim, yLim, config) : [];

  const legendNames = [
    ...new Set(
      [...points, ...(fits?.series ?? [])]
        .map((s) => s.name)
        .filter((n): n is string => typeof n === "string"),
    ),
  ];

  const xName = config.xlab ?? null;
  const yName = config.ylab ?? null;
  const xAxis: Record<string, unknown> = {
    type: time ? "time" : "value",
    ...(xName ? { name: xName, nameLocation: "middle" } : {}),
    ...(time ? {} : { scale: true }),
    ...(xLim ? { min: xLim[0], max: xLim[1] } : {}),
    splitLine: NO_CORNER_LINES,
    ...(time ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {}),
    ...(yLim ? { axisLine: zeroAxisLine(yLim) } : {}),
  };
  const yAxis: Record<string, unknown> = {
    type: "value",
    ...(yName ? { name: yName, nameLocation: "middle" } : {}),
    scale: true,
    ...(yLim ? { min: yLim[0], max: yLim[1] } : {}),
    splitLine: NO_CORNER_LINES,
    ...(xLim && !time ? { axisLine: zeroAxisLine(xLim) } : {}),
  };

  const grid = cartesianGrid(config, Boolean(xName || yName));
  // The keys stand in a column right of the plot: the color scale, then the
  // size key. A host that measures (`fitKeys()`) places them again.
  const sizeKey: SizeKeySpec | null =
    size && maxSize > 0
      ? {
          title: config.size_title ?? (config.size as string),
          items: sizeKeyValues(
            Math.min(...series.flatMap((s) => s.rows.map((r) => size[r] ?? 0))),
            maxSize,
          ).map((value) => ({ value, diameter: largestSymbol * Math.sqrt(value / maxSize) })),
        }
      : null;
  if ((colorValues || sizeKey) && typeof config.margin_right !== "number") {
    grid.right = Math.max(Number(grid.right ?? 0), KEY_COLUMN_WIDTH);
  }
  const keysTop = typeof grid.top === "number" ? grid.top : 36;
  const zoom = config.zoom ? xyZoom(!(config.square || config.equal_axes)) : null;

  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: {
      trigger: "item",
      formatter: pointTooltip(config, series.length > 1, xName ?? config.x, yColumns, time),
    },
    ...(legendNames.length > 0 ? { legend: { data: legendNames, itemStyle: { opacity: 1 } } } : {}),
    xAxis,
    yAxis,
    grid,
    ...(zoom ? zoom : {}),
    ...(colorValues ? { visualMap: colorScale(config, colorValues, points.length, keysTop) } : {}),
    ...(sizeKey
      ? {
          graphic: {
            elements: [
              sizeKeyElement(sizeKey, "vertical", {
                right: 10,
                top: keysTop + (colorValues ? COLOR_SCALE_LENGTH + 60 : 0),
              }),
            ],
          },
        }
      : {}),
    ...(time ? { useUTC: true } : {}),
    series: [...points, ...(fits?.series ?? []), ...rugs, ...diagonal],
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };
}

// The first and last split lines sit on the padded limits, not on ticks.
const NO_CORNER_LINES = Object.freeze({ showMinLine: false, showMaxLine: false });

/** The axis line of one axis, at zero of the other when its range holds zero. */
function zeroAxisLine(otherLimits: AxisLimits): Record<string, unknown> {
  return { show: otherLimits[0] <= 0 && otherLimits[1] >= 0, onZero: true };
}

/** A numeric column, with missing and non-finite cells as `null`. */
function numberColumn(data: TabularData, name: string, member: string): (number | null)[] {
  const values = column(data.columns, name, member);
  const out = values.map(numberOrNull);
  const text = values.findIndex((v) => v !== null && v !== undefined && typeof v !== "number");
  if (text >= 0) {
    throw new Error(
      `Column '${name}' (config member '${member}') must be numeric; row ${text + 1} holds '${String(values[text])}'.`,
    );
  }
  return out;
}

/**
 * The point series: one per `y` column, or per group and `y` column, groups
 * in level order. A row is drawn in a series when its x, its y, its group,
 * and the `extra` encoded values are all present.
 */
function pointSeries(
  data: TabularData,
  config: ScatterChartConfig,
  yColumns: readonly string[],
  x: readonly (number | null)[],
  ys: ReadonlyMap<string, readonly (number | null)[]>,
  extra: readonly (readonly (number | null)[] | null)[],
  palette: readonly string[],
): PointSeries[] {
  const complete = (row: number) =>
    x[row] !== null && extra.every((values) => values === null || values[row] !== null);
  const rowsOf = (y: string, candidates: readonly number[]) =>
    candidates.filter((row) => complete(row) && (ys.get(y)?.[row] ?? null) !== null);
  const all = x.map((_, row) => row);
  const color = (i: number) => palette[i % palette.length] as string;
  if (!config.group) {
    return yColumns.map((y, i) => ({
      y,
      group: null,
      name: yColumns.length > 1 ? y : null,
      color: color(i),
      rows: rowsOf(y, all),
    }));
  }
  const raw = column(data.columns, config.group, "group");
  const labels = textColumn(data, config.group, "group");
  const missing = (v: unknown) => v === null || v === undefined || Number.isNaN(v);
  const present = labels.filter((_, row) => !missing(raw[row]));
  const groups = distinct(present, data.levels?.[config.group]);
  const multiY = yColumns.length > 1;
  return groups.flatMap((g, gi) => {
    const rows = all.filter((row) => !missing(raw[row]) && labels[row] === g);
    return yColumns.map((y, yi) => ({
      y,
      group: g,
      name: multiY ? `${y} / ${g}` : g,
      color: color(gi * yColumns.length + yi),
      rows: rowsOf(y, rows),
    }));
  });
}

/** The identity segment within both axes' limits, dashed, or nothing where they do not overlap. */
function diagonalSeries(
  xLim: AxisLimits,
  yLim: AxisLimits,
  config: ScatterChartConfig,
): Record<string, unknown>[] {
  const lo = Math.max(xLim[0], yLim[0]);
  const hi = Math.min(xLim[1], yLim[1]);
  if (!(lo < hi)) return [];
  return [
    {
      type: "line",
      symbol: "circle",
      data: [
        [lo, lo],
        [hi, hi],
      ],
      showSymbol: false,
      silent: true,
      legendHoverLink: false,
      lineStyle: { color: config.diagonal_color ?? SIGN_COLORS.neutral, type: "dashed" },
      z: 1,
    },
  ];
}

/**
 * The continuous color scale of `color`: from the lowest to the highest value
 * (`sequential`), or symmetric about zero (`diverging`), so zero takes the
 * middle color. The scale applies to the point series alone.
 */
function colorScale(
  config: ScatterChartConfig,
  values: readonly (number | null)[],
  pointSeriesCount: number,
  top: number,
): Record<string, unknown> {
  const finite = values.filter((v): v is number => v !== null);
  const diverging = config.color_scale === "diverging";
  let min = Math.min(...finite);
  let max = Math.max(...finite);
  if (diverging) {
    const m = Math.max(Math.abs(min), Math.abs(max)) || 1;
    min = -m;
    max = m;
  } else if (min === max) {
    max = min + 1;
  }
  const given = asArray(config.colormap);
  if (given.length === 1) {
    throw new Error("`colormap` needs at least two colors, low to high.");
  }
  const colors =
    given.length > 0
      ? given
      : diverging
        ? [SIGN_COLORS.negative, SIGN_COLORS.neutral, SIGN_COLORS.positive]
        : [SIGN_COLORS.neutral, SIGN_COLORS.positive];
  return {
    type: "continuous",
    dimension: 2,
    seriesIndex: Array.from({ length: pointSeriesCount }, (_, i) => i),
    min,
    max,
    // ECharts labels the ends with `precision` decimals (default 0, which
    // reads 0.4 to 0.65 as 0 to 1): two significant digits of the span.
    precision: Math.max(0, 1 - Math.floor(Math.log10(max - min))),
    calculable: true,
    orient: "vertical",
    itemHeight: COLOR_SCALE_LENGTH,
    right: 10,
    top,
    inRange: { color: colors },
    text: [config.color_title ?? config.color, ""],
  };
}

/**
 * The tooltip of a point: its hover label, its series when there are several,
 * and each bound value under its column name, numbers by `formatNumber()`.
 */
function pointTooltip(
  config: ScatterChartConfig,
  named: boolean,
  xName: string,
  yColumns: readonly string[],
  time: boolean,
): (params: unknown) => string {
  const html = (s: unknown) =>
    String(s).replace(
      /[&<>"]/g,
      (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c] as string,
    );
  return (params: unknown) => {
    const p = params as {
      marker?: string;
      seriesName?: string;
      name?: string;
      value?: unknown;
    };
    const value = (Array.isArray(p.value) ? p.value : []) as unknown[];
    const x =
      time && typeof value[0] === "number"
        ? new Date(value[0]).toISOString().slice(0, 19).replace("T", " ")
        : formatNumber(value[0]);
    const yName = yColumns.length === 1 ? (config.ylab ?? yColumns[0]) : (config.ylab ?? "y");
    const lines = [
      ...(config.hover && p.name ? [`<b>${html(p.name)}</b>`] : []),
      ...(named && p.seriesName ? [`${p.marker ?? ""}${html(p.seriesName)}`] : []),
      `${html(xName)}: ${html(x)}`,
      `${html(yName)}: ${html(formatNumber(value[1]))}`,
      ...(config.color ? [`${html(config.color)}: ${html(formatNumber(value[2]))}`] : []),
      ...(config.size
        ? [`${html(config.size)}: ${html(formatNumber(value[config.color ? 3 : 2]))}`]
        : []),
    ];
    return lines.join("<br/>");
  };
}

/**
 * The fixed-aspect plotting box of a scatter chart reads as a line chart's:
 * both are cartesian charts on two value axes.
 */
export { lineAspect as scatterAspect } from "../line/config.ts";
