import { tooltipValue } from "../../shared/format.ts";
import type { MarginMembers } from "../../shared/layout.ts";
import {
  asArray,
  type ColumnNames,
  column,
  type TabularData,
  textColumn,
} from "../../shared/source.ts";
import { RTEMIS_PALETTE } from "../../theme/tokens.ts";

/**
 * An instance of the `chart/sankey/v1` schema, written by rtemis.draw's
 * `SankeyConfig` class. Every member is optional. `source`, `target`, and
 * `value` name columns of the link table.
 * spec: draw/shared-builders#sankey
 */
export interface SankeyChartConfig extends MarginMembers {
  readonly type?: "sankey";
  /** Column naming each link's source node. Default `"source"`. */
  readonly source?: string;
  /** Column naming each link's target node. Default `"target"`. */
  readonly target?: string;
  /** Column holding each link's magnitude. Default `"value"`. */
  readonly value?: string;
  /** Layout direction. Default `"horizontal"`. */
  readonly orient?: "horizontal" | "vertical";
  /** How nodes align along the flow; unset is ECharts' `"justify"`. */
  readonly node_align?: "left" | "right" | "justify" | null;
  /** Node thickness in pixels; unset is ECharts' 20. */
  readonly node_width?: number | null;
  /** Gap between nodes in pixels; unset is ECharts' 8. */
  readonly node_gap?: number | null;
  /** Node colors in order of first appearance; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /**
   * Link color: `"gradient"` from the source node's color to the target's,
   * `"source"`, `"target"`, or a color. Default `"gradient"`.
   */
  readonly link_color?: string;
  /** Link opacity. Default 0.2. */
  readonly link_opacity?: number;
  /** Node label font size in pixels. Default 12. */
  readonly label_font_size?: number;
  readonly title?: string | null;
}

/** Every member of {@link SankeyChartConfig}, checked against `chart/sankey/v1`. */
export const SANKEY_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "source",
  "target",
  "value",
  "orient",
  "node_align",
  "node_width",
  "node_gap",
  "palette",
  "link_color",
  "link_opacity",
  "label_font_size",
  "margin_top",
  "margin_right",
  "margin_bottom",
  "margin_left",
] as const satisfies readonly (keyof SankeyChartConfig)[]);

type Missing = Exclude<keyof SankeyChartConfig, (typeof SANKEY_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** Theme values the builder needs. */
export interface SankeyAppearance {
  /** The theme's categorical palette; unset is {@link RTEMIS_PALETTE}. */
  readonly palette?: readonly string[];
}

/** One link of a Sankey diagram: the sum of the rows with its source and target. */
export interface SankeyLink {
  readonly source: string;
  readonly target: string;
  readonly value: number;
}

/**
 * The links of a link table: one per source and target pair, its value the
 * sum of the pair's rows, in order of first appearance. A value that is
 * missing, not finite, or negative stops with an error naming the row, and so
 * does a cycle, naming its nodes.
 */
export function sankeyLinks(data: TabularData, config: SankeyChartConfig): SankeyLink[] {
  const sourceName = config.source ?? "source";
  const valueName = config.value ?? "value";
  const sources = textColumn(data, sourceName, "source");
  const targets = textColumn(data, config.target ?? "target", "target");
  const values = column(data.columns, valueName, "value");
  const sums = new Map<string, SankeyLink>();
  sources.forEach((source, i) => {
    const target = targets[i] as string;
    const v = values[i];
    if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
      throw new Error(
        `Column '${valueName}' (config member 'value') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`,
      );
    }
    const key = JSON.stringify([source, target]);
    const link = sums.get(key);
    sums.set(key, { source, target, value: (link?.value ?? 0) + v });
  });
  const links = [...sums.values()];
  const cycle = findCycle(links);
  if (cycle) {
    throw new Error(`A Sankey diagram's links must not form a cycle; ${cycle.join(" → ")} is one.`);
  }
  return links;
}

/** A cycle among the links as its nodes, the first repeated at the end, or `null`. */
function findCycle(links: readonly SankeyLink[]): string[] | null {
  const next = new Map<string, string[]>();
  for (const { source, target } of links) next.set(source, [...(next.get(source) ?? []), target]);
  const state = new Map<string, "open" | "done">();
  const path: string[] = [];
  const visit = (node: string): string[] | null => {
    state.set(node, "open");
    path.push(node);
    for (const to of next.get(node) ?? []) {
      if (state.get(to) === "open") return [...path.slice(path.indexOf(to)), to];
      if (!state.has(to)) {
        const found = visit(to);
        if (found) return found;
      }
    }
    path.pop();
    state.set(node, "done");
    return null;
  };
  for (const node of next.keys()) {
    if (!state.has(node)) {
      const found = visit(node);
      if (found) return found;
    }
  }
  return null;
}

/**
 * The ECharts option of a Sankey diagram from a link table: nodes in order of
 * first appearance, every source before the targets that are only targets,
 * each colored from the palette; links summed by pair, colored as
 * `link_color` says, with curveness 0.5; hovering a node highlights its
 * adjacent links. A horizontal diagram's right margin holds the longest label
 * of a node that is only a target.
 * spec: draw/shared-builders#sankey
 */
export function buildSankeyOption(
  data: TabularData,
  config: SankeyChartConfig,
  appearance: SankeyAppearance = {},
): Record<string, unknown> {
  const links = sankeyLinks(data, config);
  if (links.length === 0) throw new Error("A Sankey diagram needs at least one link.");
  const names = [...new Set([...links.map((l) => l.source), ...links.map((l) => l.target)])];
  const memberPalette = asArray(config.palette);
  const palette = memberPalette.length > 0 ? memberPalette : (appearance.palette ?? RTEMIS_PALETTE);
  const fontSize = config.label_font_size ?? 12;
  const vertical = config.orient === "vertical";
  // A theme's series item style outranks the option's color list, so each
  // node carries its own color.
  const nodes = names.map((name, i) => ({
    name,
    itemStyle: { color: palette[i % palette.length] },
  }));
  const sources = new Set(links.map((l) => l.source));
  const longest = Math.max(0, ...names.filter((n) => !sources.has(n)).map((n) => n.length));
  // 0.52 em per character approximates the system UI faces the charts use.
  const auto = vertical
    ? { left: 12, right: 12, top: 48, bottom: 24 }
    : {
        left: 12,
        right: Math.max(24, Math.min(Math.ceil(longest * 0.52 * fontSize) + 24, 540)),
        top: 48,
        bottom: 24,
      };
  const box = Object.fromEntries(
    (["top", "right", "bottom", "left"] as const).map((side) => {
      const set = config[`margin_${side}`];
      return [side, typeof set === "number" ? set : auto[side]];
    }),
  );
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    tooltip: { trigger: "item", valueFormatter: tooltipValue },
    series: [
      {
        type: "sankey",
        data: nodes,
        links,
        orient: vertical ? "vertical" : "horizontal",
        ...(config.node_align ? { nodeAlign: config.node_align } : {}),
        ...(typeof config.node_width === "number" ? { nodeWidth: config.node_width } : {}),
        ...(typeof config.node_gap === "number" ? { nodeGap: config.node_gap } : {}),
        emphasis: { focus: "adjacency" },
        lineStyle: {
          color: config.link_color ?? "gradient",
          opacity: config.link_opacity ?? 0.2,
          curveness: 0.5,
        },
        label: { fontSize },
        ...box,
      },
    ],
  };
}
