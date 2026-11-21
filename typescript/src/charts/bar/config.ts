import { tooltipValue } from "../../shared/format.ts";
import { axisZoom, cartesianGrid, type MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import {
  asArray,
  type ColumnNames,
  column,
  distinct,
  numberOrNull,
  type TabularData,
  textColumn,
} from "../../shared/source.ts";

/**
 * An instance of the `chart/bar/v1` schema, written by rtemis.draw's
 * `BarConfig` class. Every member is optional. spec: draw/shared-builders#bar
 */
export interface BarChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "bar";
  /** Column holding the categories. */
  readonly x?: string | null;
  /**
   * Columns holding the values, one series each. Unset, each bar is the
   * number of rows of its category and group.
   */
  readonly y?: ColumnNames | null;
  /** Column whose values split each value column into one series per group. */
  readonly group?: string | null;
  /**
   * How the `y` values of rows sharing a category and group combine into one
   * bar. The host applies it to the data before building (rtemislive in
   * DuckDB, rtemis.draw in R); the builder requires one row per category and
   * group when it is set, and a `y`. Default `"none"`: one bar per row.
   */
  readonly aggregate?: "none" | "sum" | "mean" | "median";
  /** Category order by total value across series. Default `"input"`. */
  readonly order?: "input" | "increasing" | "decreasing";
  /** Stack the series instead of grouping them. Default `false`. */
  readonly stack?: boolean;
  /** Draw the bars horizontally. Default `false`. */
  readonly horizontal?: boolean;
  /** A slider, Shift+wheel zoom, and drag panning on the category axis. Default `false`. */
  readonly zoom?: boolean;
  /** Bar thickness in pixels; unset uses automatic category spacing. */
  readonly bar_width?: number | null;
  /** Series colors, or bar colors for one series; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Value axis limits, `[min, max]`; unset fits the data. */
  readonly ylim?: readonly [number, number] | null;
  /** Bottom axis name; unset draws none. */
  readonly xlab?: string | null;
  /** Left axis name; unset draws none. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** What is not chart config: animation. */
export interface BarAppearance {
  /** Animate the first render and updates; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** Every member of {@link BarChartConfig}, checked against `chart/bar/v1`. */
export const BAR_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "x",
  "y",
  "group",
  "aggregate",
  "order",
  "stack",
  "horizontal",
  "zoom",
  "bar_width",
  "palette",
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
] as const satisfies readonly (keyof BarChartConfig)[]);

type MissingMember = Exclude<keyof BarChartConfig, (typeof BAR_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

interface BarSeries {
  readonly name: string;
  readonly data: (number | null)[];
}

/**
 * Build the ECharts option of a bar chart from tabular data and its
 * `chart/bar/v1` config. One bar per row and series, each category once;
 * with `group`, the categories are the distinct `x` values and each group adds a series per
 * value column, empty where a category lacks the group. With `y` unset, each
 * bar is the number of rows of its category and group: the sum of the data's
 * `frequency` column, or one per row. Distinct categories and groups follow
 * their column's levels, then order of appearance.
 */
export function buildBarOption(
  data: TabularData,
  config: BarChartConfig,
  appearance: BarAppearance = {},
): Record<string, unknown> {
  const source = data.columns;
  const yColumns = asArray(config.y);
  if (!config.x) {
    throw new Error("A bar chart needs `x` set to draw.");
  }
  const xs = textColumn(data, config.x, "x");
  const aggregate = config.aggregate ?? "none";
  const counting = yColumns.length === 0;
  if (counting && aggregate !== "none") {
    throw new Error(
      "`aggregate` combines the values of `y`, and `y` is unset: each bar counts the rows of its category.",
    );
  }
  const frequency = data.frequency ? column(source, data.frequency, "frequency") : null;
  const valuesOf = (y: string | null): (number | null)[] =>
    y === null
      ? xs.map((_, row) => (frequency ? numberOrNull(frequency[row]) : 1))
      : column(source, y, "y").map(numberOrNull);
  const measures: (string | null)[] = counting ? [null] : [...yColumns];
  const groupValues = config.group ? textColumn(data, config.group, "group") : null;
  // A bar is one category and group: two rows for one bar would draw two
  // bars under one label, or keep one of them.
  if (!counting) requireOneRowPer(xs, groupValues ?? undefined);

  let categories: string[];
  let series: BarSeries[];
  if (groupValues || counting) {
    categories = distinct(xs, data.levels?.[config.x]);
    const groups = groupValues
      ? distinct(groupValues, data.levels?.[config.group as string])
      : [null];
    const at = new Map(categories.map((c, i) => [c, i]));
    series = groups.flatMap((g) =>
      measures.map((y) => {
        const values = valuesOf(y);
        const cells: (number | null)[] = categories.map(() => null);
        xs.forEach((x, row) => {
          const i = at.get(x);
          if (i === undefined || (groupValues && groupValues[row] !== g)) return;
          const value = values[row] ?? null;
          cells[i] = counting ? (cells[i] ?? 0) + (value ?? 0) : value;
        });
        const name =
          g !== null && groups.length > 1
            ? measures.length > 1
              ? `${y} / ${g}`
              : g
            : (y ?? "Count");
        return { name, data: cells };
      }),
    );
  } else {
    categories = xs;
    series = measures.map((y) => ({ name: y as string, data: valuesOf(y) }));
  }

  const permutation = order(categories, series, config.order ?? "input");
  categories = permutation.map((i) => categories[i] as string);
  series = series.map((s) => ({ ...s, data: permutation.map((i) => s.data[i] ?? null) }));

  const palette = asArray(config.palette);
  const stack = config.stack ? "total" : undefined;
  const barWidth = typeof config.bar_width === "number" ? config.bar_width : undefined;
  const builtSeries = series.map((s, i) => {
    const single = series.length === 1;
    const perBar = single && palette.length > 1;
    const color = perBar ? undefined : palette.length > 0 ? palette[i % palette.length] : undefined;
    return {
      type: "bar",
      ...(single ? {} : { name: s.name }),
      data: perBar
        ? s.data.map((value, j) => ({
            value,
            itemStyle: { color: palette[(permutation[j] ?? j) % palette.length] },
          }))
        : s.data,
      ...(stack ? { stack } : {}),
      ...(barWidth === undefined ? {} : { barWidth }),
      ...(color === undefined ? {} : { itemStyle: { color } }),
    };
  });

  const horizontal = config.horizontal ?? false;
  // A horizontal chart lists its categories top to bottom, in reading order.
  const category: Record<string, unknown> = {
    type: "category",
    data: categories,
    axisPointer: { type: "none" },
    ...(horizontal ? { inverse: true } : {}),
  };
  const value: Record<string, unknown> = {
    type: "value",
    ...(config.ylim ? { min: config.ylim[0], max: config.ylim[1] } : {}),
  };
  const xAxis = horizontal ? value : category;
  const yAxis = horizontal ? category : value;
  if (config.xlab) Object.assign(xAxis, { name: config.xlab });
  if (config.ylab) Object.assign(yAxis, { name: config.ylab });

  const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
  const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : undefined;

  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: { trigger: "axis", valueFormatter: tooltipValue },
    ...(series.length > 1 ? { legend: { data: series.map((s) => s.name) } } : {}),
    xAxis,
    yAxis,
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    series: builtSeries,
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };
}

/** The category permutation for an order: input order, or stable by total across series. */
function order(
  categories: readonly string[],
  series: readonly BarSeries[],
  how: "input" | "increasing" | "decreasing",
): number[] {
  const index = categories.map((_, i) => i);
  if (how === "input") return index;
  const total = (i: number) => series.reduce((sum, s) => sum + (s.data[i] ?? 0), 0);
  const sign = how === "increasing" ? 1 : -1;
  return index.sort((a, b) => sign * (total(a) - total(b)) || a - b);
}

/**
 * Stop when two rows share a category (and group): with `group` or
 * `aggregate` set, a bar is one category and group, combined by the host.
 */
function requireOneRowPer(xs: readonly string[], groups?: readonly string[]): void {
  const seen = new Set<string>();
  xs.forEach((x, row) => {
    const key = groups ? `${x}\u0000${groups[row]}` : x;
    if (seen.has(key)) {
      const where = groups ? `category '${x}' in group '${groups[row]}'` : `category '${x}'`;
      throw new Error(
        `Several rows hold ${where}. Combine them before building, as the \`aggregate\` member says (sum, mean, or median).`,
      );
    }
    seen.add(key);
  });
}
