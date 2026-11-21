import { xyZoom } from "../../shared/layout.ts";
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
import { GANTT_BAR, type GanttPayload } from "./renderers.ts";

/**
 * An instance of the `chart/gantt/v1` schema, written by rtemis.draw's
 * `GanttConfig` class. Every member is optional. `label`, `start`, `end`,
 * `group`, `tooltip`, and `border` name columns of the task table.
 * spec: draw/shared-builders#gantt
 */
export interface GanttChartConfig extends LegendMembers {
  readonly type?: "gantt";
  /** Column naming each task's row. Default `"label"`. */
  readonly label?: string;
  /** Column holding each task's start. Default `"start"`. */
  readonly start?: string;
  /** Column holding each task's end. Default `"end"`. */
  readonly end?: string;
  /** Column whose values color the bars, one legend entry per value. */
  readonly group?: string | null;
  /** Column holding each bar's tooltip text; unset shows the task's label. */
  readonly tooltip?: string | null;
  /** Logical column; a true value outlines the bar. */
  readonly border?: string | null;
  /** Time axis type: `"value"` for numeric offsets, `"time"` for timestamps. Default `"value"`. */
  readonly axis_type?: "value" | "time";
  /** Shift+wheel zoom, drag panning, and box-zoom controls on both axes. Default `true`. */
  readonly zoom?: boolean;
  /** A guide line that follows the pointer and labels its time. Default `true`. */
  readonly guides?: boolean;
  /** Bar thickness as a fraction of one row. Default 0.6. */
  readonly bar_height?: number;
  /** Bar corner radius in pixels. Default 0. */
  readonly bar_radius?: number;
  /** Outline color of bars that `border` flags. Default `"#E53935"`. */
  readonly border_color?: string;
  /** Outline width in pixels. Default 1.5. */
  readonly border_width?: number;
  /** Bar colors by group; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Time axis name; unset draws none. */
  readonly xlab?: string | null;
  readonly title?: string | null;
}

/** Every member of {@link GanttChartConfig}, checked against `chart/gantt/v1`. */
export const GANTT_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "label",
  "start",
  "end",
  "group",
  "tooltip",
  "border",
  "axis_type",
  "zoom",
  "guides",
  "bar_height",
  "bar_radius",
  "border_color",
  "border_width",
  "palette",
  "xlab",
] as const satisfies readonly (keyof GanttChartConfig)[]);

type Missing = Exclude<keyof GanttChartConfig, (typeof GANTT_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the builder needs. */
export interface GanttAppearance {
  /** The theme's categorical palette; unset leaves each group's color to the chart theme. */
  readonly palette?: readonly string[];
}

/** Pixels per task row of {@link ganttHeight}. */
export const GANTT_ROW_PX = 20;

/**
 * The height in pixels of a Gantt chart of `rows` task rows: `rowPx` per row
 * and 96 pixels for the legend, toolbox, and time axis, at least 220.
 */
export function ganttHeight(rows: number, rowPx: number = GANTT_ROW_PX): number {
  return Math.max(rows * rowPx + 96, 220);
}

/**
 * The ECharts option of a Gantt chart from a task table: one bar per row of
 * the table, from its start to its end on the time axis, on the row of its
 * label. Repeated labels share a row, and rows run top to bottom in order of
 * first appearance. With `group`, each group is one series in the legend, in
 * level order; `border` outlines the bars it flags. Hovering a bar shows its
 * tooltip text.
 * spec: draw/shared-builders#gantt
 */
export function buildGanttOption(
  data: TabularData,
  config: GanttChartConfig,
  appearance: GanttAppearance = {},
): Record<string, unknown> {
  const labelColumn = config.label ?? "label";
  const labels = textColumn(data, labelColumn, "label");
  if (labels.length === 0) throw new Error("A Gantt chart needs at least one task.");
  const bound = (member: "start" | "end") => {
    const name = config[member] ?? member;
    const time = isTime(data, name);
    return column(data.columns, name, member).map((v, i) => {
      const value = time ? timeMs(v) : typeof v === "number" && Number.isFinite(v) ? v : null;
      if (value === null) {
        throw new Error(
          `Column '${name}' (config member '${member}') must hold a number or time in every row; row ${i + 1} holds '${String(v)}'.`,
        );
      }
      return value;
    });
  };
  const starts = bound("start");
  const ends = bound("end");
  const tips = config.tooltip ? textColumn(data, config.tooltip, "tooltip") : labels;
  const flags = config.border
    ? column(data.columns, config.border, "border").map((v) => (v === true || v === 1 ? 1 : 0))
    : null;

  const rows = distinct(labels);
  const rowOf = new Map(rows.map((r, i) => [r, i]));
  const groups = config.group ? textColumn(data, config.group, "group") : null;
  const levels = groups ? distinct(groups, data.levels?.[config.group as string]) : [""];

  const palette = asArray(config.palette);
  const colors = palette.length > 0 ? palette : (appearance.palette ?? []);
  const payload: GanttPayload = {
    barHeight: config.bar_height ?? 0.6,
    barRadius: config.bar_radius ?? 0,
    borderColor: config.border_color ?? "#E53935",
    borderWidth: config.border_width ?? 1.5,
  };
  const series = levels.map((level, k) => {
    const color = colors.length > 0 ? colors[k % colors.length] : undefined;
    const items: Record<string, unknown>[] = [];
    labels.forEach((label, i) => {
      if (groups && groups[i] !== level) return;
      const value = [rowOf.get(label) as number, starts[i], ends[i]];
      items.push({ value: flags ? [...value, flags[i]] : value, name: tips[i] });
    });
    return {
      type: "custom",
      ...(groups ? { name: level } : {}),
      renderItem: GANTT_BAR,
      itemPayload: payload,
      encode: { x: [1, 2], y: 0 },
      clip: true,
      ...(color ? { itemStyle: { color } } : {}),
      data: items,
    };
  });

  const zoom = config.zoom ?? true;
  const time = config.axis_type === "time";
  const controls = zoom ? xyZoom(true) : null;
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    ...(time ? { useUTC: true } : {}),
    tooltip: { trigger: "item", confine: true, formatter: "{b}" },
    legend: { show: groups !== null, data: groups ? levels : [] },
    ...(controls ? { toolbox: controls.toolbox } : {}),
    grid: {
      left: 8,
      right: 16,
      // The toolbox icons take a row above the plot, ending about 40 pixels
      // from the top.
      top: Math.max(config.title ? 40 : 12, zoom ? 48 : 0),
      bottom: 8,
      outerBoundsMode: "same",
      outerBoundsContain: config.xlab ? "all" : "axisLabel",
    },
    xAxis: {
      type: time ? "time" : "value",
      ...(config.xlab ? { name: config.xlab, nameGap: 28 } : {}),
      ...((config.guides ?? true)
        ? {
            axisPointer: {
              show: true,
              type: "line",
              snap: false,
              label: { show: true },
            },
          }
        : {}),
    },
    yAxis: {
      type: "category",
      // Leading spaces indent a label; the SVG renderer collapses them unless
      // they are no-break spaces.
      data: rows.map((r) => r.replace(/^ +/, (m) => "\u00A0".repeat(m.length))),
      inverse: true,
      boundaryGap: true,
      // Every row is labeled; `ganttHeight()` gives each row its line.
      axisLabel: { interval: 0 },
    },
    ...(controls ? { dataZoom: controls.dataZoom } : {}),
    series,
  };
}
