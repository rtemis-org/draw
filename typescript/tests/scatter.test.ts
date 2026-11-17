import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import { buildLineOption } from "../src/charts/line/config.ts";
import { autoAlpha, buildScatterOption, scatterAspect } from "../src/charts/scatter/config.ts";
import { scatterRenderers } from "../src/charts/scatter/renderers.ts";
import {
  fitKeys,
  keyPlacement,
  keyState,
  SIZE_KEY_ID,
  type SizeKeySpec,
  sizeKeyValues,
} from "../src/layout/keys.ts";
import { fitLabel } from "../src/shared/fit.ts";
import { appendLayers } from "../src/shared/layers.ts";
import { SIGN_COLORS } from "../src/theme/tokens.ts";

type Series = {
  type: string;
  name?: string;
  data: unknown[];
  symbolSize?: number;
  itemStyle?: { color?: string; opacity?: number };
  lineStyle?: { color?: string; type?: string; opacity?: number };
  areaStyle?: { color?: string; opacity?: number };
  renderItem?: string;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

const grouped = {
  columns: {
    x: [1, 2, 3, 4, 5, 6],
    y: [2, 3, 3, 5, 4, 6],
    g: ["b", "a", "b", "a", "b", "a"],
  },
  levels: { g: ["a", "b"] },
};
const curve = (group: string | null, lift: number) => ({
  y: "y",
  group,
  x: [1, 6],
  fitted: [1 + lift, 6 + lift],
  lower: [0 + lift, 5 + lift],
  upper: [2 + lift, 7 + lift],
  rsq: 0.5,
});

describe("buildScatterOption", () => {
  it("draws one series of [x, y] points and pads both axes", () => {
    const option = buildScatterOption({ columns: { x: [0, 10], y: [0, 100] } }, { x: "x", y: "y" });
    const [points] = seriesOf(option);
    expect(points).toMatchObject({ type: "scatter", symbolSize: 6 });
    expect(points?.data).toEqual([
      [0, 0],
      [10, 100],
    ]);
    expect(points?.itemStyle).toEqual({ color: "#6CA3A0", opacity: autoAlpha(2) });
    expect(option.xAxis).toMatchObject({ type: "value", min: -0.4, max: 10.4 });
    expect(option.yAxis).toMatchObject({ min: -4, max: 104 });
    expect(option).not.toHaveProperty("legend");
  });

  it("splits groups in level order and drops rows missing a bound value", () => {
    const option = buildScatterOption(
      { ...grouped, columns: { ...grouped.columns, y: [2, null, 3, 5, 4, 6] } },
      { x: "x", y: "y", group: "g", palette: ["#111111", "#222222"] },
    );
    const series = seriesOf(option);
    expect(series.map((s) => s.name)).toEqual(["a", "b"]);
    expect(series[0]?.data).toEqual([
      [4, 5],
      [6, 6],
    ]);
    expect(series[0]?.itemStyle?.color).toBe("#111111");
    expect(option.legend).toEqual({ data: ["a", "b"], itemStyle: { opacity: 1 } });
  });

  it("draws each fit beneath its group's legend entry and color", () => {
    const option = buildScatterOption(
      { ...grouped, fits: [curve("a", 0), curve("b", 1)] },
      { x: "x", y: "y", group: "g", fit: "glm", rsq: true, fit_alpha: 0.3 },
    );
    const series = seriesOf(option);
    expect(series.map((s) => s.type)).toEqual([
      "scatter",
      "scatter",
      "line",
      "line",
      "line",
      "line",
    ]);
    const names = series.map((s) => s.name);
    expect(names[0]).toBe("a (R² = 0.500)");
    expect(new Set(names).size).toBe(2);
    const [band, line] = [series[2], series[3]];
    expect(band?.data).toEqual([
      [1, 2],
      [6, 7],
      [6, 5],
      [1, 0],
    ]);
    expect(band?.areaStyle).toEqual({ color: series[0]?.itemStyle?.color, opacity: 0.3 });
    expect(line?.lineStyle).toEqual({ color: series[0]?.itemStyle?.color });
  });

  it("omits the interval with `se` false and draws nothing without `fit`", () => {
    const data = { ...grouped, fits: [curve("a", 0), curve("b", 1)] };
    const noBand = buildScatterOption(data, { x: "x", y: "y", group: "g", fit: "glm", se: false });
    expect(seriesOf(noBand)).toHaveLength(4);
    const noFit = buildScatterOption(data, { x: "x", y: "y", group: "g" });
    expect(seriesOf(noFit)).toHaveLength(2);
  });

  it("draws a pooled fit in gray under its own entry", () => {
    const option = buildScatterOption(
      { ...grouped, fits: [curve(null, 0)] },
      { x: "x", y: "y", group: "g", fit: "glm", fit_per_group: false },
    );
    const fit = seriesOf(option).at(-1);
    expect(fit?.name).toBe("Fit");
    expect(fit?.lineStyle?.color).toBe("#808080");
    expect((option.legend as { data: string[] }).data).toEqual(["a", "b", "Fit"]);
  });

  it("rejects a fit that matches no series", () => {
    expect(() =>
      buildScatterOption(
        { ...grouped, fits: [curve("z", 0)] },
        { x: "x", y: "y", group: "g", fit: "glm" },
      ),
    ).toThrow(/matches no series/);
  });

  it("sizes symbols by area and maps color on a scale", () => {
    const option = buildScatterOption(
      { columns: { x: [1, 2, 3], y: [1, 2, 3], n: [1, 4, 16], c: [-2, 0, 1] } },
      { x: "x", y: "y", size: "n", color: "c", color_scale: "diverging" },
    );
    const data = seriesOf(option)[0]?.data as { value: number[]; symbolSize: number }[];
    expect(data.map((d) => d.symbolSize)).toEqual([7.5, 15, 30]);
    // `point_size` scales the mapped symbols, keeping areas proportional.
    const doubled = buildScatterOption(
      { columns: { x: [1, 2, 3], y: [1, 2, 3], n: [1, 4, 16] } },
      { x: "x", y: "y", size: "n", point_size: 12 },
    );
    expect(
      ((seriesOf(doubled)[0]?.data ?? []) as { symbolSize: number }[]).map((d) => d.symbolSize),
    ).toEqual([15, 30, 60]);
    expect(data[0]?.value).toEqual([1, 1, -2, 1]);
    expect(option.visualMap).toMatchObject({
      dimension: 2,
      min: -2,
      max: 2,
      inRange: { color: [SIGN_COLORS.negative, SIGN_COLORS.neutral, SIGN_COLORS.positive] },
    });
    expect((option.grid as { right: number }).right).toBe(100);
    // The ends are labeled to two significant digits of the span.
    expect((option.visualMap as { precision: number }).precision).toBe(1);
    const narrow = buildScatterOption(
      { columns: { x: [1, 2], y: [1, 2], c: [0.402, 0.652] } },
      { x: "x", y: "y", color: "c" },
    );
    expect((narrow.visualMap as { precision: number }).precision).toBe(2);
    expect(() =>
      buildScatterOption({ columns: { x: [1], y: [1], n: [-1] } }, { x: "x", y: "y", size: "n" }),
    ).toThrow(/nonnegative/);
  });

  it("rejects color with group, text x, and one colormap color", () => {
    expect(() => buildScatterOption(grouped, { x: "x", y: "y", group: "g", color: "x" })).toThrow(
      /bind one/,
    );
    expect(() => buildScatterOption({ columns: { x: ["a"], y: [1] } }, { x: "x", y: "y" })).toThrow(
      /must be numeric/,
    );
    expect(() =>
      buildScatterOption(
        { columns: { x: [1, 2], y: [1, 2], c: [1, 2] } },
        { x: "x", y: "y", color: "c", colormap: ["#000000"] },
      ),
    ).toThrow(/at least two colors/);
  });

  it("puts square, equally scaled axes on one interval with an identity line", () => {
    const option = buildScatterOption(
      { columns: { x: [0, 10], y: [0, 1] } },
      {
        x: "x",
        y: "y",
        square: true,
        equal_axes: true,
        diagonal: true,
        zoom: true,
        margin_top: 10,
        margin_right: 20,
        margin_bottom: 30,
        margin_left: 40,
      },
    );
    expect(option.xAxis).toMatchObject({ min: -0.4, max: 10.4 });
    expect(option.yAxis).toMatchObject({ min: -0.4, max: 10.4 });
    expect(seriesOf(option).at(-1)?.data).toEqual([
      [-0.4, -0.4],
      [10.4, 10.4],
    ]);
    expect(scatterAspect(option, { square: true, equal_axes: true })).toEqual({
      ratio: 1,
      topPx: 10,
      rightPx: 20,
      botPx: 30,
      leftPx: 40,
    });
    // Box zoom sets each axis independently, so an equally scaled plot omits it.
    expect(Object.keys((option.toolbox as { feature: object }).feature)).toEqual(["restore"]);
    expect(option.dataZoom).toHaveLength(2);
  });

  it("adds a rug per series, drawn by a registered renderer", () => {
    const option = buildScatterOption(
      { columns: { x: [1, 2], y: [3, 4] } },
      { x: "x", y: "y", rug: true },
    );
    const rug = seriesOf(option).at(-1);
    expect(rug).toMatchObject({ type: "custom", renderItem: "rtemis.rug.v1" });
    expect(Object.keys(scatterRenderers)).toEqual(["rtemis.rug.v1"]);
  });

  it("formats the tooltip from hover labels and bound columns", () => {
    const option = buildScatterOption(
      { columns: { x: [1.23456], y: [2], h: ["One"] } },
      { x: "x", y: "y", hover: "h", xlab: "Weight" },
    );
    const formatter = (option.tooltip as { formatter: (p: unknown) => string }).formatter;
    expect(formatter({ name: "One", value: [1.23456, 2] })).toBe(
      "<b>One</b><br/>Weight: 1.23<br/>y: 2",
    );
  });
});

describe("fits on a line chart", () => {
  it("draws dashed fits in each line's color and rejects an aggregate", () => {
    const data = {
      columns: { x: [1, 2, 3], y: [1, 3, 2] },
      fits: [{ y: "y", x: [1, 3], fitted: [1.5, 2.5] }],
    };
    const option = buildLineOption(data, { x: "x", y: "y", fit: "glm" });
    const fit = seriesOf(option).at(-1);
    expect(fit?.lineStyle).toEqual({ color: "#6CA3A0", type: "dashed" });
    expect(seriesOf(option)[0]?.itemStyle?.color).toBe("#6CA3A0");
    expect(() => buildLineOption(data, { x: "x", y: "y", fit: "glm", aggregate: "mean" })).toThrow(
      /aggregate/,
    );
  });
});

describe("fitLabel", () => {
  it("joins fit_name and appends R²", () => {
    expect(fitLabel("a", { fit_name: "Linear" }, 0.25)).toBe("a - Linear");
    expect(fitLabel(null, { rsq: true }, null)).toBe("Fit (R² = NA)");
    expect(fitLabel("a", {}, 0.25)).toBe("a");
  });
});

describe("appendLayers", () => {
  it("appends layer series and joins a listed legend", () => {
    const option = { series: [{ type: "scatter" }], legend: { data: ["a"] } };
    const out = appendLayers(option, [
      { series: [{ type: "line", name: "Model" }], legend: "Model" },
    ]);
    expect(out.series).toHaveLength(2);
    expect(out.legend).toEqual({ data: ["a", "Model"] });
    expect(appendLayers(option, null)).toBe(option);
    const shown = appendLayers({ series: [] }, [{ series: [], legend: "Model" }]);
    expect(shown.legend).toEqual({ show: true });
  });
});

describe("keys", () => {
  it("picks round size key values within the range, largest first", () => {
    expect(sizeKeyValues(0.37, 0.65)).toEqual([0.6, 0.5, 0.4]);
    expect(sizeKeyValues(2700, 6300)).toEqual([6000, 5000, 4000]);
    expect(sizeKeyValues(5, 5)).toEqual([5]);
  });

  it("draws a size key and gray fits with a color scale, and rejects several y with color", () => {
    const data = {
      columns: { x: [1, 2, 3], y: [1, 2, 3], n: [1, 4, 16], c: [0, 1, 2], z: [3, 2, 1] },
      fits: [{ y: "y", x: [1, 3], fitted: [1, 3] }],
    };
    const option = buildScatterOption(data, {
      x: "x",
      y: "y",
      size: "n",
      color: "c",
      fit: "glm",
      size_title: "Count",
    });
    const key = (option.graphic as { elements: { id: string; info: { sizeKey: SizeKeySpec } }[] })
      .elements[0];
    expect(key?.id).toBe(SIZE_KEY_ID);
    expect(key?.info.sizeKey.title).toBe("Count");
    expect(key?.info.sizeKey.items[0]).toEqual({ value: 15, diameter: 30 * Math.sqrt(15 / 16) });
    expect(seriesOf(option).at(-1)?.lineStyle?.color).toBe(SIGN_COLORS.neutral);
    expect(() => buildScatterOption(data, { x: "x", y: ["y", "z"], color: "c" })).toThrow(
      /one `y` column/,
    );
  });

  it("places the keys from measured geometry beside the plot, on each side", () => {
    const echarts = loadECharts();
    if (!echarts) return;
    const option = buildScatterOption(
      { columns: { x: [1, 2, 3], y: [1, 2, 3], n: [1, 4, 16], c: [0, 1, 2] } },
      { x: "x", y: "y", size: "n", color: "c", margin_right: 36 },
      { animation: false },
    );
    for (const side of ["right", "left", "top", "bottom"] as const) {
      const chart = echarts.init(null, null, {
        renderer: "svg",
        ssr: true,
        width: 600,
        height: 400,
      });
      chart.setOption(option);
      const reserve = fitKeys(echarts, chart, { option, placement: { side, inside: false } });
      expect(reserve?.side).toBe(side);
      expect(reserve?.extra).toBeGreaterThan(30);
      const grid = chart.getOption().grid[0];
      expect(grid[side]).toBeCloseTo((side === "right" ? 36 : 36) + (reserve?.extra ?? 0));
      // A second pass starts from the authored margin and does not accumulate.
      expect(fitKeys(echarts, chart, { option, placement: { side, inside: false } })?.extra).toBe(
        reserve?.extra,
      );
      expect(chart.getOption().grid[0][side]).toBeCloseTo(grid[side]);
      chart.dispose();
    }
  });

  it("reports a key state that a later setOption changes and a refit restores", () => {
    const echarts = loadECharts();
    if (!echarts) return;
    const option = buildScatterOption(
      { columns: { x: [1, 2, 3], y: [1, 2, 3], n: [1, 4, 16], c: [0, 1, 2] } },
      { x: "x", y: "y", size: "n", color: "c", margin_right: 36 },
      { animation: false },
    );
    const chart = echarts.init(null, null, { renderer: "svg", ssr: true, width: 600, height: 400 });
    chart.setOption(option);
    fitKeys(echarts, chart, { option });
    const fitted = keyState(chart);
    fitKeys(echarts, chart, { option });
    expect(keyState(chart)).toBe(fitted);
    // A host merging the built option again puts the keys back in their static places.
    const { series: _series, ...cosmetic } = option;
    chart.setOption(cosmetic);
    expect(keyState(chart)).not.toBe(fitted);
    fitKeys(echarts, chart, { option });
    expect(keyState(chart)).toBe(fitted);
    chart.dispose();
  });

  it("maps a legend position to the keys' side", () => {
    expect(keyPlacement("top", "outside", false)).toEqual({ side: "top", inside: false });
    expect(keyPlacement("bottom-right", "inside", false)).toEqual({ side: "right", inside: true });
    expect(keyPlacement("right", "outside", true)).toEqual({ side: "left", inside: false });
    expect(keyPlacement("top", "outside", true)).toEqual({ side: "right", inside: false });
  });
});

// The ECharts build rtemis.draw bundles, for the layout tests; they are
// skipped without it.
// biome-ignore lint/suspicious/noExplicitAny: ECharts is untyped here.
function loadECharts(): any {
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  return existsSync(path) ? createRequire(import.meta.url)(path) : null;
}
