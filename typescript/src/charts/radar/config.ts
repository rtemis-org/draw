import { tooltipValue } from "../../shared/format.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import {
  asArray,
  type ColumnNames,
  column,
  distinct,
  type TabularData,
  textColumn,
} from "../../shared/source.ts";

/**
 * An instance of the `chart/radar/v1` schema, written by rtemis.draw's
 * `RadarConfig` class. Every member is optional; `indicators` names at least
 * two numeric columns and is required to draw.
 * spec: draw/shared-builders#radar
 */
export interface RadarChartConfig extends LegendMembers {
  readonly type?: "radar";
  readonly title?: string | null;
  /** Numeric columns, one axis each, clockwise from the top. */
  readonly indicators?: ColumnNames | null;
  /** Column whose values name the series; unset draws one series of every row. */
  readonly group?: string | null;
  /**
   * How the values of rows of one series combine. A series is one polygon,
   * so with `"none"` two rows in one series are an error. Default `"none"`.
   */
  readonly aggregate?: "none" | "sum" | "mean" | "median";
  /** Fill opacity of each polygon. Default 0.12. */
  readonly area_opacity?: number;
  /** Polygon line width in pixels. Default 2. */
  readonly line_width?: number;
  /** Series colors; unset uses the host's palette, then the theme's. */
  readonly palette?: ColumnNames | null;
}

/** Every member of {@link RadarChartConfig}, checked against `chart/radar/v1`. */
export const RADAR_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "indicators",
  "group",
  "aggregate",
  "area_opacity",
  "line_width",
  "palette",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof RadarChartConfig)[]);

type Missing = Exclude<keyof RadarChartConfig, (typeof RADAR_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the builder needs. */
export interface RadarAppearance {
  /** The host's categorical palette; unset leaves series colors to the theme. */
  readonly palette?: readonly string[];
}

/** One polygon of a radar chart: its name and its value on each indicator. */
export interface RadarSeries {
  readonly name: string;
  readonly values: readonly number[];
}

/** The name of the series of every row, when the config sets no group. */
export const RADAR_ALL_ROWS = "All rows";

const summary = (values: readonly number[], how: "sum" | "mean" | "median"): number => {
  if (how === "sum") return values.reduce((a, b) => a + b, 0);
  if (how === "mean") return values.reduce((a, b) => a + b, 0) / values.length;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1
    ? (sorted[mid] as number)
    : ((sorted[mid - 1] as number) + (sorted[mid] as number)) / 2;
};

/**
 * The polygons of a radar chart: one per value of `group`, in its level
 * order, otherwise in order of first appearance, or one of every row; each
 * the value of its row on every indicator, or, with `aggregate`, the
 * summary of its rows' finite values. Fewer than two indicators, a non-numeric
 * indicator, two rows in one series without an aggregate, a missing value
 * without an aggregate, and a series with no finite value on an indicator stop
 * with an error naming the cause.
 */
export function radarSeries(data: TabularData, config: RadarChartConfig): RadarSeries[] {
  const indicators = asArray(config.indicators);
  if (indicators.length < 2) {
    throw new Error("A radar chart needs at least two columns in the config member 'indicators'.");
  }
  const columns = indicators.map((name) => column(data.columns, name, "indicators"));
  const n = columns[0]?.length ?? 0;
  const groupName = config.group;
  const groups = groupName ? textColumn(data, groupName, "group") : null;
  const names = groups
    ? distinct(groups, data.levels?.[groupName as string])
    : n > 0
      ? [RADAR_ALL_ROWS]
      : [];
  const rowsOf = new Map<string, number[]>(names.map((name) => [name, []]));
  for (let i = 0; i < n; i++) rowsOf.get(groups ? (groups[i] as string) : RADAR_ALL_ROWS)?.push(i);
  const aggregate = config.aggregate ?? "none";
  return names.map((name) => {
    const rows = rowsOf.get(name) as number[];
    if (aggregate === "none" && rows.length > 1) {
      throw new Error(
        `Rows ${(rows[0] as number) + 1} and ${(rows[1] as number) + 1} both belong to series '${name}', which is one polygon. Set the config member 'aggregate' to sum, mean, or median to combine them${groupName ? "" : ", or set 'group'"}.`,
      );
    }
    const values = indicators.map((indicator, j) => {
      const finite: number[] = [];
      for (const i of rows) {
        const v = (columns[j] as readonly unknown[])[i];
        if (typeof v === "number" && Number.isFinite(v)) finite.push(v);
        else if (aggregate === "none") {
          throw new Error(
            `Column '${indicator}' (config member 'indicators') must hold a finite number; row ${i + 1} holds '${String(v)}'.`,
          );
        }
      }
      if (finite.length === 0) {
        throw new Error(
          `Series '${name}' has no finite value in column '${indicator}' (config member 'indicators').`,
        );
      }
      return aggregate === "none" ? (finite[0] as number) : summary(finite, aggregate);
    });
    return { name, values };
  });
}

/**
 * The ECharts option of a radar chart: one axis per indicator, from 0, or
 * from its smallest value when negative, to its largest value, the radar
 * lowered below a legend or title; one polygon
 * per series, filled at `area_opacity` and stroked at `line_width`, colored
 * from the palette; a legend listing the series when there are several.
 * spec: draw/shared-builders#radar
 */
export function buildRadarOption(
  data: TabularData,
  config: RadarChartConfig,
  appearance: RadarAppearance = {},
): Record<string, unknown> {
  const series = radarSeries(data, config);
  const indicators = asArray(config.indicators);
  const own = asArray(config.palette);
  const palette = own.length > 0 ? own : (appearance.palette ?? []);
  const color = (i: number) => (palette.length > 0 ? palette[i % palette.length] : undefined);
  const indicator = indicators.map((name, j) => {
    const values = series.map((s) => s.values[j] as number);
    const max = Math.max(...values);
    const min = Math.min(...values);
    return { name, max: max > 0 ? max : 1, ...(min < 0 ? { min } : { min: 0 }) };
  });
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    ...(series.length > 1 ? { legend: { data: series.map((s) => s.name) } } : {}),
    // A legend or title above the chart lowers and narrows the radar so the
    // top indicator's name stays clear of them.
    radar:
      series.length > 1 || config.title
        ? {
            center: ["50%", "56%"],
            radius: "62%",
            indicator,
            splitArea: { areaStyle: { opacity: 0.25 } },
          }
        : { radius: "68%", indicator, splitArea: { areaStyle: { opacity: 0.25 } } },
    series: [
      {
        type: "radar",
        data: series.map((s, i) => {
          const c = color(i);
          return {
            name: s.name,
            value: [...s.values],
            areaStyle: { opacity: config.area_opacity ?? 0.12 },
            lineStyle: { ...(c ? { color: c } : {}), width: config.line_width ?? 2 },
            ...(c ? { itemStyle: { color: c } } : {}),
          };
        }),
      },
    ],
  };
}
