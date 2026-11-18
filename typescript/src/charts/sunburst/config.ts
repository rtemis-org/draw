import { tooltipValue } from "../../shared/format.ts";
import {
  type HierarchyData,
  type HierarchyMembers,
  hierarchyDepth,
  hierarchyOf,
} from "../../shared/hierarchy.ts";
import type { ColumnNames, TabularData } from "../../shared/source.ts";
import { type HierarchyAppearance, hierarchyBorder, hierarchyPalette } from "../treemap/config.ts";

/**
 * An instance of the `chart/sunburst/v1` schema, written by rtemis.draw's
 * `SunburstConfig` class. Every member is optional; `levels` names the
 * columns of the tree's levels and is required to draw a table.
 * spec: draw/shared-builders#sunburst
 */
export interface SunburstChartConfig extends HierarchyMembers {
  readonly type?: "sunburst";
  readonly title?: string | null;
  /** Rings that carry labels, from the center out; 0 labels none. Default 2. */
  readonly label_depth?: number;
  /** Smallest angle, in degrees, of a sector that carries a label. Default 0. */
  readonly min_angle?: number;
  /** Label direction. Default `"radial"`. */
  readonly label_rotate?: "radial" | "tangential" | "horizontal";
  /** Outer radius, as a CSS length or percentage. Default `"85%"`. */
  readonly radius?: string;
  /** Radius of the empty center, as a CSS length or percentage. Default `"18%"`. */
  readonly inner_radius?: string;
  /** Width of the border between sectors, in pixels. Default 1. */
  readonly border_width?: number;
  /** Colors of the innermost ring; unset uses the host's palette, then the theme's. */
  readonly palette?: ColumnNames | null;
}

/** Every member of {@link SunburstChartConfig}, checked against `chart/sunburst/v1`. */
export const SUNBURST_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "levels",
  "value",
  "label_depth",
  "min_angle",
  "label_rotate",
  "radius",
  "inner_radius",
  "border_width",
  "palette",
] as const satisfies readonly (keyof SunburstChartConfig)[]);

type Missing = Exclude<keyof SunburstChartConfig, (typeof SUNBURST_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * The ECharts option of a sunburst from a table, its rows nested by `levels`
 * and summed by `value`, or from a tree the host built: one ring per level
 * between `inner_radius` and `radius`, the innermost ring colored from the
 * palette, sectors separated by `border_width` borders in the background
 * color, and labels on the `label_depth` innermost rings, turned by
 * `label_rotate`, on sectors of at least `min_angle` degrees.
 * spec: draw/shared-builders#sunburst
 */
export function buildSunburstOption(
  data: TabularData | HierarchyData,
  config: SunburstChartConfig,
  appearance: HierarchyAppearance = {},
): Record<string, unknown> {
  const tree = hierarchyOf(data, config, "A sunburst");
  const palette = hierarchyPalette(config, appearance);
  const labelDepth = config.label_depth ?? 2;
  const minAngle = config.min_angle ?? 0;
  const rotate = config.label_rotate === "horizontal" ? 0 : (config.label_rotate ?? "radial");
  // levels[0] is the hidden center and levels[i] the i-th ring, so a ring
  // carries labels when i <= label_depth.
  const levels = Array.from({ length: hierarchyDepth(tree) + 1 }, (_, i) => ({
    label: { show: i > 0 && i <= labelDepth, rotate, minAngle },
  }));
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    ...(palette.length > 0 ? { color: [...palette] } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    series: [
      {
        type: "sunburst",
        radius: [config.inner_radius ?? "18%", config.radius ?? "85%"],
        ...(config.title ? { center: ["50%", "54%"] } : {}),
        nodeClick: false,
        data: tree,
        label: { rotate, minAngle },
        itemStyle: {
          borderColor: hierarchyBorder(appearance),
          borderWidth: config.border_width ?? 1,
        },
        levels,
      },
    ],
  };
}
