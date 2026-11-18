import { formatNumber, tooltipValue } from "../../shared/format.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { asArray, type ColumnNames, type TabularData } from "../../shared/source.ts";
import { labeledSums, type PieSlice } from "../pie/config.ts";

/**
 * An instance of the `chart/funnel/v1` schema, written by rtemis.draw's
 * `FunnelConfig` class. Every member is optional; `values` and `labels` name
 * columns of the data and are required to draw.
 * spec: draw/shared-builders#funnel
 */
export interface FunnelChartConfig extends LegendMembers {
  readonly type?: "funnel";
  readonly title?: string | null;
  /** Column holding the stage values. */
  readonly values?: string | null;
  /** Column holding the stage labels. */
  readonly labels?: string | null;
  /** Stage order: by value, largest or smallest first, or the data's order. Default `"descending"`. */
  readonly sort?: "descending" | "ascending" | "none";
  /** Whether each stage carries its label. Default `true`. */
  readonly show_labels?: boolean;
  /** Stage label content: the label, the value, or both. Default `"name"`. */
  readonly label_format?: "name" | "value" | "name_value";
  /** Stage colors in stage order; unset uses the host's palette, then the theme's. */
  readonly palette?: ColumnNames | null;
}

/** Every member of {@link FunnelChartConfig}, checked against `chart/funnel/v1`. */
export const FUNNEL_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "values",
  "labels",
  "sort",
  "show_labels",
  "label_format",
  "palette",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof FunnelChartConfig)[]);

type Missing = Exclude<keyof FunnelChartConfig, (typeof FUNNEL_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the builder needs. */
export interface FunnelAppearance {
  /** The host's categorical palette; unset leaves stage colors to the theme. */
  readonly palette?: readonly string[];
}

type FunnelLabelParams = { name?: string; value?: unknown };

const LABEL_TEMPLATES = {
  name: "{b}",
  value: (p: FunnelLabelParams) => String(formatNumber(p.value)),
  name_value: (p: FunnelLabelParams) => `${p.name ?? ""}: ${formatNumber(p.value)}`,
} as const;

/** The stages of a funnel: the sum of the values of each label, as {@link labeledSums} gives them. */
export function funnelStages(data: TabularData, config: FunnelChartConfig): PieSlice[] {
  return labeledSums(data, config, "A funnel");
}

/**
 * The ECharts option of a funnel from a table of labels and values: one
 * stage per label, the rows of a label summed, ordered by `sort`, each
 * stage colored from the palette in stage order and labeled inside by
 * `label_format`; a legend listing the stages when there are several.
 * spec: draw/shared-builders#funnel
 */
export function buildFunnelOption(
  data: TabularData,
  config: FunnelChartConfig,
  appearance: FunnelAppearance = {},
): Record<string, unknown> {
  const stages = funnelStages(data, config);
  const own = asArray(config.palette);
  const palette = own.length > 0 ? own : (appearance.palette ?? []);
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    ...(stages.length > 1 ? { legend: { data: stages.map((s) => s.name) } } : {}),
    series: [
      {
        type: "funnel",
        sort: config.sort ?? "descending",
        left: "10%",
        width: "80%",
        top: config.title ? 70 : 50,
        bottom: 20,
        minSize: "0%",
        maxSize: "100%",
        label: {
          show: config.show_labels !== false,
          position: "inside",
          formatter: LABEL_TEMPLATES[config.label_format ?? "name"],
        },
        data: stages.map((s, i) =>
          palette.length > 0
            ? { ...s, itemStyle: { color: palette[i % palette.length] } }
            : { ...s },
        ),
      },
    ],
  };
}
