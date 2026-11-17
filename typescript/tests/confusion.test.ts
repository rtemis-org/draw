import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import {
  buildConfusionOption,
  type ConfusionData,
  confusionLayout,
  confusionRates,
} from "../src/charts/confusion/config.ts";
import { confusionHeight, fitConfusion } from "../src/charts/confusion/fit.ts";
import { SIGN_COLORS } from "../src/theme/tokens.ts";

// counts[r][c]: reference class r predicted as class c.
const counts = [
  [40, 10, 2],
  [5, 30, 4],
  [3, 6, 50],
];
const data: ConfusionData = { classes: ["a", "b", "c"], panels: [{ name: "Test", counts }] };

type Series = {
  type: string;
  data: { value: unknown[]; label: { formatter: string } }[];
  xAxisIndex: number;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

describe("confusionRates", () => {
  it("computes R's per-class rates, accuracy, and balanced accuracy", () => {
    const r = confusionRates(counts);
    expect(r.total).toBe(150);
    expect(r.sensitivity[0]).toBeCloseTo(40 / 52, 12);
    expect(r.specificity[0]).toBeCloseTo((150 - 52 - 48 + 40) / (150 - 52), 12);
    expect(r.ppv[1]).toBeCloseTo(30 / 46, 12);
    expect(r.npv[2]).toBeCloseTo((150 - 59 - 56 + 50) / (150 - 56), 12);
    expect(r.accuracy).toBeCloseTo(120 / 150, 12);
    expect(r.balancedAccuracy).toBeCloseTo((40 / 52 + 30 / 39 + 50 / 59) / 3, 12);
  });

  it("leaves a rate without a denominator unknown", () => {
    const r = confusionRates([
      [3, 0],
      [0, 0],
    ]);
    expect(r.sensitivity).toEqual([1, null]);
    expect(r.fraction[1]).toEqual([null, null]);
    expect(r.balancedAccuracy).toBeNull();
  });
});

describe("buildConfusionOption", () => {
  it("draws counts on the diagonal and off it, with Predicted above the matrix", () => {
    const option = buildConfusionOption(data, {});
    const [hits, misses] = seriesOf(option);
    expect(hits?.data.map((d) => d.label.formatter)).toEqual(["40", "30", "50"]);
    // Off-diagonal cells column by column.
    expect(misses?.data.map((d) => d.label.formatter)).toEqual(["5", "3", "10", "6", "2", "4"]);
    expect(hits?.data[0]?.value.slice(4, 8)).toEqual(["a", "a", 40 / 52, "0.77"]);
    const [x] = option.xAxis as Record<string, unknown>[];
    expect(x).toMatchObject({ name: "Predicted", position: "top", data: ["a", "b", "c"] });
    expect((option.yAxis as Record<string, unknown>[])[0]).toMatchObject({
      name: "Reference",
      inverse: true,
    });
  });

  it("fades toward the theme's sign colors unless the config sets colors", () => {
    const maps = buildConfusionOption(data, {}).visualMap as { inRange: { color: string[] } }[];
    expect(maps[0]?.inRange.color).toEqual(["#FFFFFF", SIGN_COLORS.negative]);
    expect(maps[1]?.inRange.color).toEqual(["#FFFFFF", SIGN_COLORS.positive]);
    const set = buildConfusionOption(data, { correct_color: "#0F6A66" }).visualMap as {
      inRange: { color: string[] };
    }[];
    expect(set[0]?.inRange.color[1]).toBe("#0F6A66");
    const themed = buildConfusionOption(
      data,
      {},
      {
        signColors: { negative: "#111111", positive: "#222222" },
      },
    ).visualMap as { inRange: { color: string[] } }[];
    expect(themed[1]?.inRange.color[1]).toBe("#222222");
  });

  it("adds metric strips by default, at `digits` decimals", () => {
    const option = buildConfusionOption(data, { digits: 3 });
    const series = seriesOf(option);
    expect(series).toHaveLength(5);
    expect(option.grid).toHaveLength(4);
    expect(series[2]?.data[0]?.label.formatter).toBe((40 / 52).toFixed(3));
    expect(series[4]?.data.map((d) => d.label.formatter)).toEqual([
      `Accuracy\n${(120 / 150).toFixed(3)}`,
      `BA\n${((40 / 52 + 30 / 39 + 50 / 59) / 3).toFixed(3)}`,
    ]);
    const bare = buildConfusionOption(data, { show_metrics: false });
    expect(seriesOf(bare)).toHaveLength(2);
    expect(bare.grid).toHaveLength(1);
  });

  it("titles each of several panels, `ncol` per row", () => {
    const two: ConfusionData = {
      classes: data.classes,
      panels: [
        { name: "Training", counts },
        { name: "Test", counts },
      ],
    };
    const option = buildConfusionOption(two, { title: "Model" });
    const titles = option.title as Record<string, unknown>[];
    expect(titles.map((t) => t.text)).toEqual(["Model", "Training", "Test"]);
    expect(titles[2]).toMatchObject({ left: "75%", top: "7%" });
    expect(seriesOf(option)[5]?.xAxisIndex).toBe(4);
    expect(confusionLayout({ ncol: 1, show_metrics: false })).toEqual({
      metrics: false,
      ncol: 1,
      fontSize: 12,
    });
  });

  it("stops on a count matrix of the wrong size", () => {
    expect(() =>
      buildConfusionOption({ classes: ["a", "b"], panels: [{ name: "x", counts: [[1]] }] }, {}),
    ).toThrow("2 by 2");
  });
});

describe("fitConfusion", () => {
  const echarts = loadECharts();
  it.skipIf(!echarts)("fits square cells and aligned strips to the canvas and theme", () => {
    const option = buildConfusionOption(data, {});
    const payload = { option, confusion: confusionLayout({}) };
    const theme = { backgroundColor: "#181818", textStyle: { color: "#dddddd" } };
    const height = confusionHeight(echarts, payload, theme, 600);
    fitConfusion(echarts, payload, theme, 600, height);
    const grids = option.grid as { left: number; top: number; width: number; height: number }[];
    const [matrix, right, bottom] = grids;
    expect(matrix?.width).toBeCloseTo(matrix?.height as number, 6);
    expect(right?.top).toBe(matrix?.top);
    expect(right?.height).toBe(matrix?.height);
    expect(bottom?.left).toBe(matrix?.left);
    expect(bottom?.width).toBe(matrix?.width);
    // The low end of each fill is the theme background.
    const maps = option.visualMap as { inRange: { color: string[] } }[];
    expect(maps[0]?.inRange.color[0]).toBe("#181818");
    const chart = echarts.init(null, null, { renderer: "svg", ssr: true, width: 600, height });
    chart.setOption(option);
    expect(chart.renderToSVGString()).toContain(">40<");
    chart.dispose();
  });
});

// biome-ignore lint/suspicious/noExplicitAny: the bundled ECharts module carries no types here.
function loadECharts(): any {
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  return existsSync(path) ? createRequire(import.meta.url)(path) : null;
}
