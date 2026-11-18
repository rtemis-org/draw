import { tooltipValue } from "../../shared/format.ts";
import { type HierarchyData, type HierarchyMembers, hierarchyOf } from "../../shared/hierarchy.ts";
import { asArray, type ColumnNames, type TabularData } from "../../shared/source.ts";
import { darkThemeTokens, lightThemeTokens } from "../../theme/tokens.ts";

/**
 * An instance of the `chart/treemap/v1` schema, written by rtemis.draw's
 * `TreemapConfig` class. Every member is optional; `levels` names the
 * columns of the tree's levels and is required to draw a table.
 * spec: draw/shared-builders#treemap
 */
export interface TreemapChartConfig extends HierarchyMembers {
  readonly type?: "treemap";
  readonly title?: string | null;
  /** Label each tile with its name. Default `true`. */
  readonly show_labels?: boolean;
  /** Width of the border between tiles, in pixels. Default 1. */
  readonly border_width?: number;
  /** Colors of the top-level nodes; unset uses the host's palette, then the theme's. */
  readonly palette?: ColumnNames | null;
}

/** Every member of {@link TreemapChartConfig}, checked against `chart/treemap/v1`. */
export const TREEMAP_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "levels",
  "value",
  "show_labels",
  "border_width",
  "palette",
] as const satisfies readonly (keyof TreemapChartConfig)[]);

type Missing = Exclude<keyof TreemapChartConfig, (typeof TREEMAP_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the treemap and sunburst builders need. */
export interface HierarchyAppearance {
  /** The host's categorical palette; unset leaves node colors to the theme. */
  readonly palette?: readonly string[];
  /** The chart background, the color of the borders between nodes; unset uses the scheme's theme background. */
  readonly background?: string;
  /** The dark scheme, whose theme background the borders take when `background` is unset. */
  readonly dark?: boolean;
}

/** The border color of a tree chart: the background, else the light or dark theme's background. */
export function hierarchyBorder(appearance: HierarchyAppearance): string {
  return (
    appearance.background ??
    (appearance.dark ? darkThemeTokens().bg_color : lightThemeTokens().bg_color)
  );
}

/** The palette of a tree chart: the config's, else the host's; empty leaves colors to the theme. */
export function hierarchyPalette(
  config: { readonly palette?: ColumnNames | null },
  appearance: HierarchyAppearance,
): readonly string[] {
  const own = asArray(config.palette);
  return own.length > 0 ? own : (appearance.palette ?? []);
}

/**
 * The ECharts option of a treemap from a table, its rows nested by
 * `levels` and summed by `value`, or from a tree the host built: tiles sized
 * by value, top-level nodes colored from the palette, separated by
 * `border_width` borders in the background color, labeled with their names
 * when `show_labels` is set; no zoom, drill-down, or breadcrumb.
 * spec: draw/shared-builders#treemap
 */
export function buildTreemapOption(
  data: TabularData | HierarchyData,
  config: TreemapChartConfig,
  appearance: HierarchyAppearance = {},
): Record<string, unknown> {
  const tree = hierarchyOf(data, config, "A treemap");
  const palette = hierarchyPalette(config, appearance);
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    ...(palette.length > 0 ? { color: [...palette] } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    series: [
      {
        type: "treemap",
        // ECharts reserves room below the tiles for a breadcrumb, which the
        // treemap does not draw.
        top: config.title ? 40 : 8,
        bottom: 8,
        left: 8,
        right: 8,
        roam: false,
        nodeClick: false,
        breadcrumb: { show: false },
        label: { show: config.show_labels !== false, formatter: "{b}" },
        upperLabel: { show: false },
        // The border alone separates siblings; a gap would add a second band
        // in the parent's color.
        itemStyle: {
          borderColor: hierarchyBorder(appearance),
          borderWidth: config.border_width ?? 1,
          gapWidth: 0,
        },
        data: tree,
      },
    ],
  };
}
