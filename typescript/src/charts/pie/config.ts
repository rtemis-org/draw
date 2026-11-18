import { formatNumber, tooltipValue } from "../../shared/format.ts";
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
 * An instance of the `chart/pie/v1` schema, written by rtemis.draw's
 * `PieConfig` class. Every member is optional; `values` and `labels` name
 * columns of the data and are required to draw.
 * spec: draw/shared-builders#pie
 */
export interface PieChartConfig extends LegendMembers {
  readonly type?: "pie";
  /** Column holding the slice values. */
  readonly values?: string | null;
  /** Column holding the slice labels. */
  readonly labels?: string | null;
  /**
   * Slice label content: the label (`"name"`), the value, the percentage, or
   * the label with either. Default `"name"`.
   */
  readonly label_format?: "name" | "value" | "percent" | "name_value" | "name_percent";
  /** Decimal places of a percentage. Default 1. */
  readonly percent_digits?: number;
  /** Whether each slice carries its label. Default `true`. */
  readonly show_labels?: boolean;
  /** Draw a Nightingale rose chart, sizing slices by radius or by area; unset draws a pie. */
  readonly rose_type?: "radius" | "area" | null;
  /** Outer radius, as a CSS length or percentage. Default `"75%"`. */
  readonly radius?: string;
  /** Inner radius of a donut, as a CSS length or percentage; unset draws a full pie. */
  readonly inner_radius?: string | null;
  /** Slice colors in slice order; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  readonly title?: string | null;
}

/** Every member of {@link PieChartConfig}, checked against `chart/pie/v1`. */
export const PIE_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "values",
  "labels",
  "label_format",
  "percent_digits",
  "show_labels",
  "rose_type",
  "radius",
  "inner_radius",
  "palette",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof PieChartConfig)[]);

type Missing = Exclude<keyof PieChartConfig, (typeof PIE_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the builder needs. */
export interface PieAppearance {
  /** The host's categorical palette; unset leaves slice colors to the theme. */
  readonly palette?: readonly string[];
}

/** One slice of a pie: the sum of the rows with its label. */
export interface PieSlice {
  readonly name: string;
  readonly value: number;
}

type PieLabelParams = { name?: string; value?: unknown };

/**
 * The label of each `label_format`: an ECharts template, or, for a value, a
 * function writing it with {@link formatNumber}.
 */
const LABEL_TEMPLATES = {
  name: "{b}",
  value: (p: PieLabelParams) => String(formatNumber(p.value)),
  percent: "{d}%",
  name_value: (p: PieLabelParams) => `${p.name ?? ""}: ${formatNumber(p.value)}`,
  name_percent: "{b}: {d}%",
} as const;

const isMissing = (v: unknown) =>
  v === null || v === undefined || (typeof v === "number" && Number.isNaN(v));

/** The slices of a pie: {@link labeledSums} of its labels and values. */
export function pieSlices(data: TabularData, config: PieChartConfig): PieSlice[] {
  return labeledSums(data, config, "A pie chart");
}

/**
 * The sum of the values of each label, for a chart of labeled parts (a pie or
 * a funnel): one entry per label in the label column's level order, otherwise
 * in order of first appearance. A missing label, a value that is missing, not
 * finite, or negative, and a total of zero stop with an error naming the
 * cause; `chart` begins the errors that name the chart ("A pie chart").
 */
export function labeledSums(
  data: TabularData,
  config: { readonly values?: string | null; readonly labels?: string | null },
  chart: string,
): PieSlice[] {
  const valueName = config.values;
  const labelName = config.labels;
  if (!valueName || !labelName) {
    throw new Error(`${chart} needs the config members 'values' and 'labels' set.`);
  }
  const raw = column(data.columns, labelName, "labels");
  const labels = textColumn(data, labelName, "labels");
  const values = column(data.columns, valueName, "values");
  const sums = new Map<string, number>();
  labels.forEach((label, i) => {
    if (isMissing(raw[i])) {
      throw new Error(
        `Column '${labelName}' (config member 'labels') must hold a label in every row; row ${i + 1} is missing.`,
      );
    }
    const v = values[i];
    if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
      throw new Error(
        `Column '${valueName}' (config member 'values') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`,
      );
    }
    sums.set(label, (sums.get(label) ?? 0) + v);
  });
  const names = distinct([...sums.keys()], data.levels?.[labelName]);
  const slices = names.map((name) => ({ name, value: sums.get(name) as number }));
  if (!slices.some((s) => s.value > 0)) {
    throw new Error(`${chart} needs a positive total; column '${valueName}' sums to 0.`);
  }
  return slices;
}

/**
 * The ECharts option of a pie chart from a table of labels and values: one
 * slice per label, the rows of a label summed, colored from the palette in
 * slice order; labels aligned to the chart's edges, in `label_format`, with
 * percentages to `percent_digits` places; a legend listing the slices when
 * there are several.
 * spec: draw/shared-builders#pie
 */
export function buildPieOption(
  data: TabularData,
  config: PieChartConfig,
  appearance: PieAppearance = {},
): Record<string, unknown> {
  const slices = pieSlices(data, config);
  const memberPalette = asArray(config.palette);
  const palette = memberPalette.length > 0 ? memberPalette : (appearance.palette ?? []);
  // A theme's series item style outranks the option's color list, so each
  // slice carries its own color when a palette is given.
  const items = slices.map((slice, i) =>
    palette.length > 0
      ? { ...slice, itemStyle: { color: palette[i % palette.length] } }
      : { ...slice },
  );
  const outer = config.radius ?? "75%";
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    ...(slices.length > 1 ? { legend: { data: slices.map((s) => s.name) } } : {}),
    series: [
      {
        type: "pie",
        data: items,
        radius: config.inner_radius ? [config.inner_radius, outer] : outer,
        ...(config.rose_type ? { roseType: config.rose_type } : {}),
        percentPrecision: config.percent_digits ?? 1,
        avoidLabelOverlap: true,
        // Edge alignment keeps outside labels within the chart at narrow widths.
        label: {
          show: config.show_labels !== false,
          alignTo: "edge",
          edgeDistance: 8,
          formatter: LABEL_TEMPLATES[config.label_format ?? "name"],
        },
      },
    ],
  };
}
