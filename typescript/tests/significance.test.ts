import { describe, expect, it } from "vitest";
import {
  buildSignificanceOption,
  type SignificanceData,
  significanceAxisNames,
} from "../src/charts/significance/config.ts";
import { applyRepelLabels } from "../src/shared/repel.ts";
import { SIGN_COLORS } from "../src/theme/tokens.ts";

// Holm over six hypotheses, as the host computes it.
const data: SignificanceData = {
  label: ["A", "B", "C", "D", "E", "F"],
  estimate: [-2, -1, 0, 1, 2, 3],
  p_value: [0.001, 0.002, 0.003, 0.05, 0.004, 0.005],
  p_adjusted: [0.006, 0.01, 0.012, 0.05, 0.012, 0.012],
};

type Item = { name: string; value: unknown[]; symbol?: string; label?: unknown };
type Series = {
  type: string;
  name?: string;
  data: Item[];
  itemStyle?: { color?: string };
  markLine?: { data: Record<string, unknown>[] };
  rtemisRepelLabels?: boolean;
  labelLayout?: unknown;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];
const names = (s: Series | undefined) => s?.data.map((d) => d.name);

describe("buildSignificanceOption", () => {
  it("groups by significance and sign, a tie at x_thresh being positive", () => {
    const series = seriesOf(buildSignificanceOption(data, {}));
    expect(series.slice(0, 3).map((s) => s.name)).toEqual([
      "Significant negative",
      "Not significant",
      "Significant positive",
    ]);
    expect(series.slice(0, 3).map(names)).toEqual([["A", "B"], ["D"], ["C", "E", "F"]]);
    expect(series.slice(0, 3).map((s) => s.itemStyle?.color)).toEqual([
      SIGN_COLORS.negative,
      SIGN_COLORS.neutral,
      SIGN_COLORS.positive,
    ]);
    // Coordinates, then the tooltip's text.
    expect(series[0]?.data[0]?.value).toEqual([-2, -Math.log10(0.006), "-2", "0.00100", "0.00600"]);
  });

  it("labels the points of each side with the smallest raw p-values", () => {
    const series = seriesOf(buildSignificanceOption(data, { annotate_n: 1 }));
    const labeled = series.flatMap((s) => s.data.filter((d) => d.label).map((d) => d.name));
    expect(labeled).toEqual(["A", "C"]);
    expect(series[0]?.rtemisRepelLabels).toBe(true);
    const bars = seriesOf(buildSignificanceOption(data, { view: "manhattan" }));
    expect(bars.flatMap((s) => s.data.filter((d) => d.label))).toEqual([]);
  });

  it("names the axes after the coefficient and the adjustment", () => {
    const option = buildSignificanceOption(data, { xlab: "Coefficient of x1" });
    expect(option.xAxis).toMatchObject({ name: "Coefficient of x1" });
    expect(option.yAxis).toMatchObject({ name: "-log10(Holm-adjusted p)" });
    expect(significanceAxisNames({ view: "manhattan", p_adjust_method: "none" })).toEqual({
      x: "Outcome",
      y: "-log10(p)",
    });
    expect(significanceAxisNames({ p_adjust_method: "BH", p_transform: "one_minus" }).y).toBe(
      "1 - BH-adjusted p",
    );
  });

  it("draws a zero p-value at a marked cap with a caption", () => {
    const zero: SignificanceData = { ...data, p_adjusted: [0, 0.01, 0.012, 0.05, 0.012, 0.012] };
    const option = buildSignificanceOption(zero, {});
    const [negative] = seriesOf(option);
    // One unit above the rounded-up highest finite height.
    expect(negative?.data[0]?.value[1]).toBe(Math.ceil(-Math.log10(0.01)) + 1);
    expect(negative?.data[0]?.symbol).toBe("triangle");
    expect(option.title).toMatchObject({ subtext: "p = 0 shown at 3 (triangles)" });
    expect(() => buildSignificanceOption(zero, { zero_cap: 1 })).toThrow("zero_cap");
    expect(() => buildSignificanceOption(zero, { ylim: [0, 2] })).toThrow("ylim");
    const bars = seriesOf(buildSignificanceOption(zero, { view: "manhattan" }));
    expect(bars.at(-1)).toMatchObject({
      type: "scatter",
      name: "Significant negative",
      symbol: "triangle",
    });
  });

  it("omits an incomplete hypothesis from the marks, keeping its position", () => {
    const gap: SignificanceData = {
      ...data,
      estimate: [-2, null, 0, 1, 2, 3],
    };
    const option = buildSignificanceOption(gap, { view: "manhattan" });
    expect((option.xAxis as { data: string[] }).data).toHaveLength(6);
    const marks = seriesOf(option).flatMap((s) => s.data.map((d) => d.value[0]));
    expect(marks).not.toContain(1);
  });

  it("colors display groups from the palette in group order", () => {
    const grouped: SignificanceData = {
      ...data,
      group: ["b", "b", "a", "a", null, "a"],
      groups: ["b", "a"],
    };
    const option = buildSignificanceOption(grouped, {
      group: "g",
      palette: ["#111111", "#222222"],
    });
    const series = seriesOf(option);
    expect(series.slice(0, 2).map((s) => [s.name, s.itemStyle?.color])).toEqual([
      ["b", "#111111"],
      ["a", "#222222"],
    ]);
    expect(option.legend).toMatchObject({ data: ["b", "a"] });
  });

  it("keeps the thresholds out of the legend and leaves many outcomes unlabeled", () => {
    const series = seriesOf(buildSignificanceOption(data, {}));
    const reference = series.at(-1);
    expect(reference?.name).toBeUndefined();
    expect(reference?.markLine?.data).toEqual([
      { yAxis: -Math.log10(0.05), label: { formatter: "p < 0.05", position: "end" } },
      { xAxis: 0, label: { show: false } },
    ]);
    const many: SignificanceData = {
      label: Array.from({ length: 41 }, (_, i) => `y${i}`),
      estimate: Array.from({ length: 41 }, () => 1),
      p_value: Array.from({ length: 41 }, () => 0.5),
      p_adjusted: Array.from({ length: 41 }, () => 1),
    };
    const bars = buildSignificanceOption(many, { view: "manhattan" });
    expect(bars.xAxis).toMatchObject({ axisLabel: { show: false } });
  });
});

describe("applyRepelLabels", () => {
  it("installs one shared layout on the flagged series", () => {
    const option = applyRepelLabels(buildSignificanceOption(data, {}));
    const layouts = seriesOf(option)
      .filter((s) => s.rtemisRepelLabels)
      .map((s) => s.labelLayout);
    expect(layouts.length).toBeGreaterThan(1);
    expect(typeof layouts[0]).toBe("function");
    expect(new Set(layouts).size).toBe(1);
  });

  it("moves a label off another label and off a labeled point", () => {
    const series: Record<string, unknown> = { rtemisRepelLabels: true };
    applyRepelLabels({ series: [series] });
    const layout = series.labelLayout as (p: unknown) => { x: number; y: number };
    const rect = { x: 100, y: 100, width: 8, height: 8 };
    const labelRect = { x: 90, y: 84, width: 30, height: 14 };
    const first = layout({ seriesIndex: 0, dataIndex: 0, rect, labelRect });
    const second = layout({ seriesIndex: 0, dataIndex: 1, rect, labelRect });
    expect(first).toMatchObject({ x: 90, y: 84 });
    expect(second).not.toMatchObject({ x: 90, y: 84 });
  });
});
