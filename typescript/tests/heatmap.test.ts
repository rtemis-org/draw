// biome-ignore-all lint/suspicious/noExplicitAny: the tests read nested fields of built options, which are untyped JSON.
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  branchColors,
  buildHeatmapOption,
  fitHeatmap,
  type HeatmapData,
  type HeatmapTree,
  heatmapHints,
  heatmapMargins,
} from "../src/charts/heatmap/config.ts";
import { colorRamp, dark3, divergingRamp } from "../src/shared/color.ts";

// R's heatmap options before the builder, with the data rtemis.draw's
// heatmap_data() sends for the same input: written by R into the fixture.
interface ParityCase {
  name: string;
  data: HeatmapData;
  config: Record<string, unknown>;
  option: Record<string, unknown>;
  hints: Record<string, unknown>;
}
const parity = JSON.parse(
  readFileSync(new URL("./fixtures/heatmap/r-parity.json", import.meta.url), "utf8"),
) as ParityCase[];

const channels = (hex: string) => [1, 3, 5].map((i) => Number.parseInt(hex.slice(i, i + 2), 16));
// R's colorRampPalette() computes with fused multiply-adds on arm64, so a
// channel can differ by one.
const sameColors = (a: readonly string[], b: readonly string[]) =>
  a.length === b.length &&
  a.every((x, i) =>
    channels(x).every((v, c) => Math.abs(v - (channels(b[i] as string)[c] as number)) <= 1),
  );

// Numbers rounded to 10 decimals, which R and JavaScript agree on.
const rounded = (value: unknown): unknown =>
  typeof value === "number"
    ? Math.round(value * 1e10) / 1e10
    : Array.isArray(value)
      ? value.map(rounded)
      : value && typeof value === "object"
        ? Object.fromEntries(Object.entries(value).map(([k, v]) => [k, rounded(v)]))
        : value;

describe("buildHeatmapOption against rtemis.draw's heatmap_option()", () => {
  for (const c of parity) {
    it(c.name, () => {
      const option = JSON.parse(JSON.stringify(buildHeatmapOption(c.data, c.config))) as Record<
        string,
        any
      >;
      const expected = structuredClone(c.option) as Record<string, any>;
      expect(sameColors(option.visualMap.inRange.color, expected.visualMap.inRange.color)).toBe(
        true,
      );
      delete option.visualMap.inRange;
      delete expected.visualMap.inRange;
      // The builder's heatmap series turns animation and hover emphasis off,
      // which R's option left at ECharts' defaults.
      const heatmap = [option.series].flat().find((s: any) => s.type === "heatmap");
      expect(heatmap).toMatchObject({ animation: false, emphasis: { disabled: true } });
      delete heatmap.animation;
      delete heatmap.emphasis;
      for (const key of ["grid", "xAxis", "yAxis", "visualMap", "series", "title"]) {
        expect(rounded(option[key]), key).toEqual(rounded(expected[key]));
      }
      expect(heatmapHints(c.data, c.config as never)).toMatchObject(c.hints);
    });
  }
});

// hclust(dist(c(0, 1, 9, 10))): leaves 1 and 2 join, then 3 and 4, then both.
const tree: HeatmapTree = {
  merge: [
    [-1, -2],
    [-3, -4],
    [1, 2],
  ],
  height: [1, 1, 10],
  order: [1, 2, 3, 4],
};

describe("branchColors", () => {
  it("colors the merges inside each cluster, in displayed order", () => {
    const colors = branchColors(tree, 2, "gray") as string[];
    expect(colors.slice(0, 2)).toEqual(dark3(2));
    expect(colors[2]).toBe("gray");
    expect(branchColors(tree, 4, "gray")).toEqual(["gray", "gray", "gray"]);
    expect(new Set(branchColors(tree, 1, "gray")).size).toBe(1);
    expect(branchColors(tree, null, "gray")).toBeNull();
    expect(() => branchColors(tree, 1.5, "gray")).toThrow("whole number of clusters");
  });
});

describe("dendrograms", () => {
  const data: HeatmapData = {
    values: [
      [1, 2],
      [3, 4],
      [5, 6],
      [7, 8],
    ],
    row_tree: tree,
  };

  it("draws one U shape per merge at the leaves' positions and merge heights", () => {
    const option = buildHeatmapOption(data, {}) as Record<string, any>;
    const dendro = option.series[0];
    expect(dendro.renderItem).toBe("rtemis.dendrogram.v1");
    expect(dendro.data).toEqual([
      [0, 1, 0, 0, 1],
      [2, 3, 0, 0, 1],
      [0.5, 2.5, 1, 1, 10],
    ]);
    const uniform = buildHeatmapOption(data, { dendro_uniform: true }) as Record<string, any>;
    expect(uniform.series[0].data.map((d: number[]) => d[4])).toEqual([1, 2, 3]);
  });

  it("keeps leaves the tree leaves out in place", () => {
    // Row 1 has no values and stays at position 1; the tree swaps rows 0, 2, and 3.
    const partial: HeatmapData = {
      values: [[1], [null], [3], [2]],
      row_names: ["a", "b", "c", "d"],
      row_tree: {
        merge: [
          [-1, -3],
          [-2, 1],
        ],
        height: [1, 2],
        order: [2, 1, 3],
        leaves: [0, 2, 3],
      },
    };
    const option = buildHeatmapOption(partial, {}) as Record<string, any>;
    expect(option.yAxis[1].data).toEqual(["c", "b", "a", "d"]);
  });

  it("gives a tree of equal heights a visible panel", () => {
    const flat = buildHeatmapOption(
      { ...data, row_tree: { ...tree, height: [0, 0, 0] } },
      {},
    ) as Record<string, any>;
    expect(flat.xAxis[0].max).toBeGreaterThan(0);
  });

  it("stops on a cut without a tree", () => {
    expect(() => buildHeatmapOption({ values: [[1]] }, { row_cut: 2 })).toThrow("row tree");
  });
});

describe("color scales", () => {
  it("derive from the sign colors and the background, light or dark", () => {
    const option = (dark: boolean) =>
      (buildHeatmapOption({ values: [[-1, 1]] }, {}, { dark }) as Record<string, any>).visualMap
        .inRange.color;
    expect(option(false)[50]).toBe("#FFFFFF");
    expect(option(true)[50]).toBe("#181818");
    expect(option(false)[0]).toBe("#2D83D3");
    expect(option(false)[100]).toBe("#C55E28");
  });

  it("match R's grDevices", () => {
    expect(dark3(4)).toEqual(["#E16A86", "#909800", "#00AD9A", "#9183E6"]);
    expect(dark3(7)).toEqual([
      "#E16A86",
      "#C18500",
      "#799D00",
      "#00AB6E",
      "#00A9BE",
      "#6C8EE6",
      "#D169D0",
    ]);
    expect(
      sameColors(colorRamp(["#2D83D3", "#ffffff"], 5), [
        "#2D83D3",
        "#61A2DE",
        "#96C1E9",
        "#CAE0F3",
        "#FFFFFF",
      ]),
    ).toBe(true);
    // R: diverging_palette("#2D83D3", "#ffffff", "#C55E28", c(-0.74, 1))[c(1, 26, 51, 76, 101)]
    const d = divergingRamp("#2D83D3", "#ffffff", "#C55E28", [-0.74, 1]);
    expect(
      sameColors(
        [0, 25, 50, 75, 100].map((i) => d[i] as string),
        ["#2D83D3", "#A7CBEC", "#F7EBE4", "#DEA486", "#C55E28"],
      ),
    ).toBe(true);
  });
});

describe("cells", () => {
  it("write values with ddSci and notes on request", () => {
    const option = buildHeatmapOption(
      { values: [[0.5951, null]], cell_notes: [["n1", "n2"]] },
      { show_values: true },
    ) as Record<string, any>;
    expect(option.series[0].data).toEqual([
      { value: [0, 0, 0.5951], name: "n1", note: "n1", label: { formatter: "0.60" } },
      { value: [1, 0, null], name: "n2", note: "n2", label: { formatter: "" } },
    ]);
    expect(() => buildHeatmapOption({ values: [[1]] }, { show_notes: true })).toThrow("cell notes");
    const tip = option.tooltip.formatter({
      value: [0, 0, 0.5951],
      data: { note: "<b>", label: { formatter: "0.60" } },
    });
    expect(tip).toBe("1 × 1: 0.60<br>&#60;b&#62;");
  });
});

describe("heatmapMargins and fitHeatmap", () => {
  const data: HeatmapData = {
    values: [
      [1, 2, 3],
      [4, 5, 6],
    ],
    row_names: ["alpha", "b"],
    col_names: ["x", "y", "z"],
  };

  it("derive the margins from the labels, and take each margin member", () => {
    expect(heatmapMargins(data, {})).toEqual({ left: 20 + 5 * 8, right: 90, top: 10, bottom: 36 });
    expect(heatmapMargins(data, { margin_left: 12, title: "T" })).toMatchObject({
      left: 12,
      top: 40,
    });
  });

  it("square the cells, centered, and shift the color scale with them", () => {
    const option = buildHeatmapOption(data, { square_cells: true });
    const hints = heatmapHints(data, { square_cells: true });
    let update: any = null;
    fitHeatmap(
      { getWidth: () => 1000, getHeight: () => 400, setOption: (o) => (update = o) },
      option,
      hints,
    );
    // 354 pixels of height over 2 rows: 177-pixel cells, 531 pixels wide.
    const width = 1000 - hints.leftPx - hints.rightPx;
    const dx = (width - 3 * 177) / 2;
    expect(update.grid[0].left).toBeCloseTo(hints.leftPx + dx);
    expect(update.grid[0].right).toBeCloseTo(hints.rightPx + dx);
    expect(update.grid[0].top).toBeCloseTo(hints.topPx);
    expect(update.visualMap[0].right).toBeCloseTo(dx);
    update = null;
    fitHeatmap(
      { getWidth: () => 1000, getHeight: () => 400, setOption: (o) => (update = o) },
      option,
      {
        ...hints,
        squareCells: false,
      },
    );
    expect(update).toBeNull();
  });
});
