import { describe, expect, it } from "vitest";
import { buildPieOption, pieSlices } from "../src/charts/pie/config.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = Record<string, unknown> & {
  data: { name: string; value: number; itemStyle?: { color: string } }[];
  label: Record<string, unknown>;
};
const seriesOf = (option: Record<string, unknown>) => (option.series as Series[])[0] as Series;

const counts: TabularData = {
  columns: { kind: ["b", "a", "b", "c"], n: [1, 2, 3, 4] },
};
const bound = { values: "n", labels: "kind" } as const;

describe("pieSlices", () => {
  it("sums the rows of each label in order of first appearance", () => {
    expect(pieSlices(counts, bound)).toEqual([
      { name: "b", value: 4 },
      { name: "a", value: 2 },
      { name: "c", value: 4 },
    ]);
  });

  it("orders the slices by the label column's levels", () => {
    const data = { ...counts, levels: { kind: ["c", "b", "a"] } };
    expect(pieSlices(data, bound).map((s) => s.name)).toEqual(["c", "b", "a"]);
  });

  it("reads a typed array of values", () => {
    const data = { columns: { kind: ["a", "b"], n: Float64Array.from([1, 2]) } };
    expect(pieSlices(data, bound)).toEqual([
      { name: "a", value: 1 },
      { name: "b", value: 2 },
    ]);
  });

  it("stops on a missing, infinite, or negative value, naming the row", () => {
    for (const bad of [null, Number.NaN, Number.POSITIVE_INFINITY, -1]) {
      expect(() => pieSlices({ columns: { kind: ["a", "b"], n: [1, bad] } }, bound)).toThrow(
        `Column 'n' (config member 'values') must hold a finite, nonnegative number in every row; row 2 holds '${String(bad)}'.`,
      );
    }
  });

  it("stops on a missing label, naming the row", () => {
    expect(() => pieSlices({ columns: { kind: ["a", null], n: [1, 2] } }, bound)).toThrow(
      "Column 'kind' (config member 'labels') must hold a label in every row; row 2 is missing.",
    );
  });

  it("stops on a zero total and on unbound columns", () => {
    expect(() => pieSlices({ columns: { kind: ["a", "b"], n: [0, 0] } }, bound)).toThrow(
      "A pie chart needs a positive total; column 'n' sums to 0.",
    );
    expect(() => pieSlices(counts, { values: "n" })).toThrow(
      "A pie chart needs the config members 'values' and 'labels' set.",
    );
    expect(() => pieSlices(counts, { values: "m", labels: "kind" })).toThrow(
      "Column 'm' (config member 'values') is not in the data.",
    );
  });
});

describe("buildPieOption", () => {
  it("draws one slice per label with edge-aligned name labels", () => {
    const option = buildPieOption(counts, bound);
    const series = seriesOf(option);
    expect(series.type).toBe("pie");
    expect(series.radius).toBe("75%");
    expect(series.percentPrecision).toBe(1);
    expect(series.roseType).toBeUndefined();
    expect(series.label).toEqual({
      show: true,
      alignTo: "edge",
      edgeDistance: 8,
      formatter: "{b}",
    });
    expect(series.data.map((d) => d.itemStyle)).toEqual([undefined, undefined, undefined]);
    expect(option.legend).toEqual({ data: ["b", "a", "c"] });
    expect(option.title).toBeUndefined();
    expect((option.tooltip as Record<string, unknown>).trigger).toBe("item");
  });

  it("maps the label, radius, rose, and title members", () => {
    const option = buildPieOption(counts, {
      ...bound,
      label_format: "name_percent",
      percent_digits: 2,
      show_labels: false,
      rose_type: "area",
      radius: "70%",
      inner_radius: "40%",
      title: "Kinds",
    });
    const series = seriesOf(option);
    expect(series.label.formatter).toBe("{b}: {d}%");
    expect(series.label.show).toBe(false);
    expect(series.percentPrecision).toBe(2);
    expect(series.roseType).toBe("area");
    expect(series.radius).toEqual(["40%", "70%"]);
    expect(option.title).toEqual({ text: "Kinds", left: "center" });
  });

  it("colors the slices from the palette member, then the host's", () => {
    const own = seriesOf(buildPieOption(counts, { ...bound, palette: ["#111111", "#222222"] }));
    expect(own.data.map((d) => d.itemStyle?.color)).toEqual(["#111111", "#222222", "#111111"]);
    const host = seriesOf(buildPieOption(counts, bound, { palette: ["#333333"] }));
    expect(host.data.map((d) => d.itemStyle?.color)).toEqual(["#333333", "#333333", "#333333"]);
  });

  it("leaves out the legend of a single slice", () => {
    const option = buildPieOption({ columns: { kind: ["a"], n: [1] } }, bound);
    expect(option.legend).toBeUndefined();
  });

  it("builds JSON apart from the tooltip's value formatter", () => {
    const option = buildPieOption(counts, bound);
    const { tooltip: _tooltip, ...rest } = option;
    expect(JSON.parse(JSON.stringify(rest))).toEqual(rest);
  });
});
