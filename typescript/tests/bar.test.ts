import { describe, expect, it } from "vitest";
import { buildBarOption } from "../src/charts/bar/config.ts";
import { valueLabel } from "../src/shared/labels.ts";
import { ZOOM_SLIDER_ID } from "../src/shared/layout.ts";

const source = {
  columns: {
    g: ["A", "B", "C"],
    u: [3, 7, 2],
    v: [1, null, 5],
  },
};

type Series = { name?: string; data: unknown[]; itemStyle?: { color: string }; stack?: string };
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

describe("buildBarOption", () => {
  it("draws one unnamed series without a legend", () => {
    const option = buildBarOption(source, { x: "g", y: "u" });
    expect(seriesOf(option)).toEqual([{ type: "bar", data: [3, 7, 2] }]);
    expect(option).not.toHaveProperty("legend");
    expect(option.xAxis).toMatchObject({ type: "category", data: ["A", "B", "C"] });
    expect(option.yAxis).toEqual({ type: "value" });
    expect(option.tooltip).toMatchObject({ trigger: "axis" });
    expect(
      (option.tooltip as { valueFormatter: (v: number) => string }).valueFormatter(2.345),
    ).toBe("2.35");
    expect(option.grid).toMatchObject({ top: 36, right: 36, bottom: 36, left: 36 });
  });

  it("draws named series with a legend, missing values as null", () => {
    const option = buildBarOption(source, { x: "g", y: ["u", "v"], stack: true });
    const series = seriesOf(option);
    expect(series.map((s) => s.name)).toEqual(["u", "v"]);
    expect(series[1]?.data).toEqual([1, null, 5]);
    expect(series.every((s) => s.stack === "total")).toBe(true);
    expect(option.legend).toEqual({ data: ["u", "v"] });
  });

  it("orders categories by total, stably", () => {
    const option = buildBarOption(
      { columns: { g: ["A", "B", "C", "D"], u: [2, 5, 2, 1] } },
      { x: "g", y: "u", order: "decreasing", palette: ["#111", "#222", "#333", "#444"] },
    );
    expect((option.xAxis as { data: string[] }).data).toEqual(["B", "A", "C", "D"]);
    const data = seriesOf(option)[0]?.data as { value: number; itemStyle: { color: string } }[];
    expect(data.map((d) => d.value)).toEqual([5, 2, 2, 1]);
    expect(data.map((d) => d.itemStyle.color)).toEqual(["#222", "#111", "#333", "#444"]);
  });

  it("splits value columns by group over distinct categories", () => {
    const grouped = {
      g: ["A", "B", "A", "C"],
      sex: ["F", "F", "M", "M"],
      u: [1, 2, 3, 4],
    };
    const option = buildBarOption({ columns: grouped }, { x: "g", y: "u", group: "sex" });
    expect((option.xAxis as { data: string[] }).data).toEqual(["A", "B", "C"]);
    expect(seriesOf(option).map((s) => [s.name, s.data])).toEqual([
      ["F", [1, 2, null]],
      ["M", [3, null, 4]],
    ]);
    const two = buildBarOption(
      { columns: { ...grouped, w: [5, 6, 7, 8] } },
      { x: "g", y: ["u", "w"], group: "sex" },
    );
    expect(seriesOf(two).map((s) => s.name)).toEqual(["u / F", "w / F", "u / M", "w / M"]);
    const leveled = buildBarOption(
      { columns: grouped, levels: { sex: ["M", "F"], g: ["C", "B", "A"] } },
      { x: "g", y: "u", group: "sex" },
    );
    expect((leveled.xAxis as { data: string[] }).data).toEqual(["C", "B", "A"]);
    expect(seriesOf(leveled).map((s) => s.name)).toEqual(["M", "F"]);
  });

  it("colors series from the palette, recycled", () => {
    const series = seriesOf(buildBarOption(source, { x: "g", y: ["u", "v"], palette: "#abc" }));
    expect(series.map((s) => s.itemStyle?.color)).toEqual(["#abc", "#abc"]);
    expect(
      seriesOf(buildBarOption(source, { x: "g", y: "u", palette: "#abc" }))[0]?.itemStyle,
    ).toEqual({
      color: "#abc",
    });
  });

  it("names axes, swaps them when horizontal, and applies ylim to the value axis", () => {
    const option = buildBarOption(source, {
      x: "g",
      y: "u",
      horizontal: true,
      xlab: "Count",
      ylab: "Group",
      ylim: [0, 10],
    });
    expect(option.xAxis).toMatchObject({ type: "value", name: "Count", min: 0, max: 10 });
    expect(option.yAxis).toMatchObject({ type: "category", name: "Group" });
    expect(option.grid).toMatchObject({ outerBoundsContain: "all" });
  });

  it("fixes only the margins set, and reserves a slider band for zoom", () => {
    const option = buildBarOption(source, { x: "g", y: "u", margin_left: 80, zoom: true });
    expect(option.grid).toEqual({
      left: 80,
      bottom: 80 + 7 + 30 + 8,
      outerBoundsMode: "same",
      outerBoundsContain: "axisLabel",
    });
    const [slider, inside] = option.dataZoom as Record<string, unknown>[];
    expect(slider).toMatchObject({ type: "slider", id: ZOOM_SLIDER_ID, bottom: 87, height: 30 });
    expect(inside).toMatchObject({ type: "inside", zoomOnMouseWheel: "shift" });
  });

  it("reads typed-array columns, with NaN as missing", () => {
    const option = buildBarOption(
      { columns: { g: Float64Array.from([1, 2]), u: Float64Array.from([3, Number.NaN]) } },
      { x: "g", y: "u" },
    );
    expect((option.xAxis as { data: string[] }).data).toEqual(["1", "2"]);
    expect(seriesOf(option)[0]?.data).toEqual([3, null]);
  });

  it("requires one row per category and group", () => {
    const repeated = { columns: { g: ["A", "A"], sex: ["F", "F"], u: [1, 2] } };
    expect(() => buildBarOption(repeated, { x: "g", y: "u", group: "sex" })).toThrow(
      /category 'A' in group 'F'.*aggregate/,
    );
    expect(() => buildBarOption(repeated, { x: "g", y: "u", aggregate: "mean" })).toThrow(
      /category 'A'/,
    );
    expect(() => buildBarOption(repeated, { x: "g", y: "u" })).toThrow(/category 'A'/);
  });

  it("names a missing column and its member", () => {
    expect(() => buildBarOption(source, { x: "g", y: "w" })).toThrow(/'w'.*'y'/);
    expect(() => buildBarOption(source, { y: "u" })).toThrow("`x` set");
  });
});

describe("bar counts", () => {
  const columns = { g: ["A", "B", "A", "C", "A"], s: ["F", "M", "M", "F", "F"] };

  it("counts the rows of each category with y unset", () => {
    const option = buildBarOption({ columns, levels: { g: ["C", "B", "A"] } }, { x: "g" });
    expect((option.xAxis as { data: string[] }).data).toEqual(["C", "B", "A"]);
    expect(seriesOf(option)).toEqual([{ type: "bar", data: [1, 1, 3] }]);
  });

  it("counts per group, and sums a frequency column of combined rows", () => {
    const grouped = buildBarOption({ columns }, { x: "g", group: "s" });
    expect(seriesOf(grouped).map((s) => [s.name, s.data])).toEqual([
      ["F", [2, null, 1]],
      ["M", [1, 1, null]],
    ]);
    const combined = buildBarOption(
      { columns: { g: ["A", "B", "A"], n: [4, 2, 3] }, frequency: "n" },
      { x: "g" },
    );
    expect(seriesOf(combined)[0]?.data).toEqual([7, 2]);
  });

  it("rejects an aggregate without y", () => {
    expect(() => buildBarOption({ columns }, { x: "g", aggregate: "mean" })).toThrow(
      "`y` is unset",
    );
  });

  it("names the value axis by its measure", () => {
    expect(valueLabel({})).toBe("Count");
    expect(valueLabel({ y: "age" })).toBe("age");
    expect(valueLabel({ y: "age", aggregate: "mean" })).toBe("Mean age");
    expect(valueLabel({ y: ["age"], aggregate: "median" })).toBe("Median age");
    expect(valueLabel({ y: "age", aggregate: "sum" }, (c) => c.toUpperCase())).toBe("Sum of AGE");
    expect(valueLabel({ y: ["a", "b"], aggregate: "mean" })).toBe("Mean");
    expect(valueLabel({ y: ["a", "b"] })).toBeNull();
  });
});

describe("horizontal bars", () => {
  it("list categories top to bottom in their order", () => {
    const option = buildBarOption(
      { columns: { g: ["A", "B"], u: [1, 2] } },
      { x: "g", y: "u", horizontal: true },
    );
    expect(option.yAxis).toMatchObject({ type: "category", data: ["A", "B"], inverse: true });
    expect(option.xAxis).toEqual({ type: "value" });
  });
});
