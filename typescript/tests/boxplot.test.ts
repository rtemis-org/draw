import { describe, expect, it } from "vitest";
import {
  type BoxplotData,
  buildBoxplotOption,
  radicalInverse,
} from "../src/charts/boxplot/config.ts";
import { RENDERER_NAMES, registerRenderers } from "../src/shared/renderers.ts";

type Series = {
  type: string;
  name?: string;
  data: unknown[];
  renderItem?: string;
  itemStyle?: { color: string; borderColor: string };
  itemPayload?: Record<string, unknown>;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];
const stats = (low: number) => [low, low + 1, low + 2, low + 3, low + 4] as const;

const grouped: BoxplotData = {
  variables: ["u", "w"],
  levels: ["a", "b"],
  cells: [
    { variable: "u", level: "a", stats: stats(1), n: 4, range: [0, 6] },
    { variable: "u", level: "b", stats: stats(2), n: 4, range: [1, 7] },
    { variable: "w", level: "a", stats: stats(3), n: 4, range: [2, 8] },
    { variable: "w", level: "b", stats: null, n: 0, range: null },
  ],
};

describe("buildBoxplotOption", () => {
  it("puts the columns on the axis without a group, in one color", () => {
    const option = buildBoxplotOption(
      {
        variables: ["u", "w"],
        cells: grouped.cells.slice(0, 1).map((c) => ({ ...c, level: null })),
      },
      { labels: ["U", "W"] },
    );
    expect((option.xAxis as { data: string[] }).data).toEqual(["U", "W"]);
    const [box] = seriesOf(option);
    expect(box?.data).toEqual([stats(1), [null, null, null, null, null]]);
    expect(box?.itemStyle?.borderColor).toBe("#6CA3A0");
    expect(option).not.toHaveProperty("legend");
  });

  it("places group levels by group_layout", () => {
    const legend = buildBoxplotOption(grouped, {});
    expect((legend.xAxis as { data: string[] }).data).toEqual(["u", "w"]);
    expect(seriesOf(legend).map((s) => s.name)).toEqual(["a", "b"]);
    expect(seriesOf(legend)[1]?.data).toEqual([stats(2), [null, null, null, null, null]]);
    const xAxis = buildBoxplotOption(grouped, { group_layout: "x-axis" });
    expect((xAxis.xAxis as { data: string[] }).data).toEqual(["a", "b"]);
    expect(seriesOf(xAxis).map((s) => s.name)).toEqual(["u", "w"]);
  });

  it("colors the boxes of one series by category only on request", () => {
    const one: BoxplotData = { ...grouped, variables: ["u"], cells: grouped.cells.slice(0, 2) };
    const plain = buildBoxplotOption(one, {});
    expect(seriesOf(plain)[0]?.data[0]).toEqual(stats(1));
    const colored = buildBoxplotOption(one, { color_by_group: true });
    const items = seriesOf(colored)[0]?.data as { itemStyle: { borderColor: string } }[];
    expect(items.map((d) => d.itemStyle.borderColor)).toEqual(["#6CA3A0", "#F08904"]);
  });

  it("draws points at deterministic offsets and pads the value axis", () => {
    const option = buildBoxplotOption(
      {
        variables: ["u"],
        cells: [
          {
            variable: "u",
            stats: stats(1),
            n: 3,
            range: [1, 20],
            points: [{ value: 20, id: "r3", index: 3 }],
          },
        ],
      },
      { boxpoints: "outliers", point_spread: 0.5 },
    );
    const points = seriesOf(option)[1];
    expect(points?.renderItem).toBe("rtemis.boxplot_points.v1");
    expect(points?.data).toEqual([
      {
        value: [0, 20, "r3", radicalInverse(3) * 0.5],
        itemStyle: { color: "#6CA3A0", opacity: 0.6 },
      },
    ]);
    expect((option.yAxis as { max: number }).max).toBeGreaterThan(20);
  });

  it("marks each box's mean on request, under its series' name", () => {
    const withMeans: BoxplotData = {
      ...grouped,
      cells: grouped.cells.map((c, i) => ({ ...c, mean: c.stats ? 2.5 + i : null })),
    };
    expect(seriesOf(buildBoxplotOption(withMeans, {})).map((s) => s.renderItem)).not.toContain(
      "rtemis.boxplot_mean.v1",
    );
    const means = seriesOf(buildBoxplotOption(withMeans, { mean_marker: true })).filter(
      (s) => s.renderItem === "rtemis.boxplot_mean.v1",
    );
    expect(means.map((s) => s.name)).toEqual(["a", "b"]);
    expect(means[0]?.data).toEqual([
      { value: [0, 2.5], itemStyle: { color: "#6CA3A0" } },
      { value: [1, 4.5], itemStyle: { color: "#6CA3A0" } },
    ]);
    // An empty box has no mean.
    expect(means[1]?.data).toHaveLength(1);
    expect(means[0]?.itemPayload).toMatchObject({ boxIndex: 0, size: 9 });
  });

  it("lists horizontal categories top to bottom and honors ylim", () => {
    const option = buildBoxplotOption(grouped, { horizontal: true, ylim: [0, 10] });
    expect(option.yAxis).toMatchObject({ type: "category", inverse: true });
    expect(option.xAxis).toMatchObject({ type: "value", min: 0, max: 10 });
  });

  it("rejects unknown comparison endpoints and empty boxes", () => {
    const base = {
      variables: ["u", "w"],
      cells: grouped.cells
        .slice(0, 2)
        .map((c, i) => ({ ...c, variable: i ? "w" : "u", level: null })),
    };
    expect(() =>
      buildBoxplotOption({ ...base, comparisons: [{ from: "u", to: "z", label: "p" }] }, {}),
    ).toThrow("existing category");
    expect(() =>
      buildBoxplotOption({ ...base, comparisons: [{ from: "u", to: "u", label: "p" }] }, {}),
    ).toThrow("distinct");
    expect(() =>
      buildBoxplotOption({ variables: ["u"], cells: [{ variable: "u", stats: null, n: 0 }] }, {}),
    ).toThrow("available");
  });
});

describe("radicalInverse", () => {
  it("spreads row positions without randomness", () => {
    expect([1, 2, 3, 4].map(radicalInverse)).toEqual([0, -0.25, 0.25, -0.375]);
  });
});

describe("registerRenderers", () => {
  it("registers each named renderer", () => {
    const names: string[] = [];
    registerRenderers({ registerCustomSeries: (name: string) => void names.push(name) });
    expect(names).toEqual([...RENDERER_NAMES]);
    expect(names).toContain("rtemis.violin.v1");
    expect(names).toContain("rtemis.boxplot_mean.v1");
  });
});
