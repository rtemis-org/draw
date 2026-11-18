import { describe, expect, it } from "vitest";
import { buildSankeyOption, sankeyLinks } from "../src/charts/sankey/config.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = Record<string, unknown> & {
  data: { name: string; itemStyle: { color: string } }[];
  links: { source: string; target: string; value: number }[];
};
const seriesOf = (option: Record<string, unknown>) => (option.series as Series[])[0] as Series;

const flows: TabularData = {
  columns: {
    source: ["a", "a", "b", "a"],
    target: ["b", "c", "c", "b"],
    value: [1, 2, 3, 4],
  },
};

describe("sankeyLinks", () => {
  it("sums repeated pairs in order of first appearance", () => {
    expect(sankeyLinks(flows, {})).toEqual([
      { source: "a", target: "b", value: 5 },
      { source: "a", target: "c", value: 2 },
      { source: "b", target: "c", value: 3 },
    ]);
  });

  it("stops on a missing, infinite, or negative value, naming the row", () => {
    for (const bad of [null, Number.NaN, Number.POSITIVE_INFINITY, -1]) {
      expect(() =>
        sankeyLinks({ columns: { source: ["a", "b"], target: ["b", "c"], value: [1, bad] } }, {}),
      ).toThrow(
        `Column 'value' (config member 'value') must hold a finite, nonnegative number in every row; row 2 holds '${String(bad)}'.`,
      );
    }
  });

  it("stops on a cycle, naming its nodes", () => {
    expect(() =>
      sankeyLinks(
        { columns: { source: ["a", "b", "c"], target: ["b", "c", "a"], value: [1, 1, 1] } },
        {},
      ),
    ).toThrow("a → b → c → a is one.");
    expect(() =>
      sankeyLinks({ columns: { source: ["a"], target: ["a"], value: [1] } }, {}),
    ).toThrow("a → a is one.");
  });

  it("binds renamed columns", () => {
    const renamed = { columns: { from: ["x"], to: ["y"], n: [2] } };
    expect(sankeyLinks(renamed, { source: "from", target: "to", value: "n" })).toEqual([
      { source: "x", target: "y", value: 2 },
    ]);
    expect(() => sankeyLinks(renamed, {})).toThrow(
      "Column 'source' (config member 'source') is not in the data.",
    );
  });
});

describe("buildSankeyOption", () => {
  it("colors each node and draws gradient links with hover focus", () => {
    const series = seriesOf(buildSankeyOption(flows, {}, { palette: ["#111", "#222"] }));
    expect(series.data).toEqual([
      { name: "a", itemStyle: { color: "#111" } },
      { name: "b", itemStyle: { color: "#222" } },
      { name: "c", itemStyle: { color: "#111" } },
    ]);
    expect(series.lineStyle).toEqual({ color: "gradient", opacity: 0.2, curveness: 0.5 });
    expect(series.emphasis).toEqual({ focus: "adjacency" });
    expect(series.orient).toBe("horizontal");
  });

  it("applies the members", () => {
    const option = buildSankeyOption(flows, {
      orient: "vertical",
      node_align: "left",
      node_width: 12,
      node_gap: 4,
      palette: ["#333"],
      link_color: "source",
      link_opacity: 0.45,
      label_font_size: 14,
      margin_top: 60,
      title: "Flows",
    });
    const series = seriesOf(option);
    expect(series).toMatchObject({
      orient: "vertical",
      nodeAlign: "left",
      nodeWidth: 12,
      nodeGap: 4,
      lineStyle: { color: "source", opacity: 0.45 },
      label: { fontSize: 14 },
      top: 60,
      right: 12,
      bottom: 24,
      left: 12,
    });
    expect(series.data.every((n) => n.itemStyle.color === "#333")).toBe(true);
    expect(option.title).toEqual({ text: "Flows", left: "center" });
  });

  it("holds the longest target-only label in a horizontal diagram's right margin", () => {
    const long = { columns: { source: ["a"], target: ["a much longer target"], value: [1] } };
    expect(seriesOf(buildSankeyOption(long, {})).right).toBe(Math.ceil(20 * 0.52 * 12) + 24);
    expect(seriesOf(buildSankeyOption(flows, {})).right).toBe(Math.ceil(1 * 0.52 * 12) + 24);
  });
});
