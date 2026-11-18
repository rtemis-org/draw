import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import { buildHeatmapOption } from "../src/charts/heatmap/config.ts";
import { buildScatterOption } from "../src/charts/scatter/config.ts";
import { buildSpectrogramOption } from "../src/charts/spectrogram/config.ts";
import { fitKeys, KEY_GAP, placeColorScale } from "../src/layout/keys.ts";
import { registerRenderers } from "../src/shared/renderers.ts";

// biome-ignore lint/suspicious/noExplicitAny: the R package's ECharts bundle carries no types.
function loadECharts(): any {
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  return existsSync(path) ? createRequire(import.meta.url)(path) : null;
}

// The bar's left edge, and the right edge of the rightmost plotting area.
// biome-ignore lint/suspicious/noExplicitAny: an instance of the untyped ECharts bundle.
function measure(chart: any) {
  const model = chart.getModel();
  let right = Number.NEGATIVE_INFINITY;
  for (let i = 0; model.getComponent("grid", i); i++) {
    const rect = model.getComponent("grid", i).coordinateSystem.getRect();
    right = Math.max(right, rect.x + rect.width);
  }
  const view = chart.getViewOfComponentModel(model.getComponent("visualMap"));
  let bar = Number.POSITIVE_INFINITY;
  // biome-ignore lint/suspicious/noExplicitAny: an element of the untyped ECharts bundle.
  view.group.traverse((el: any) => {
    if (!el.isGroup && el.style?.fill?.colorStops) {
      const r = el.getBoundingRect().clone();
      r.applyTransform(el.getComputedTransform());
      bar = Math.min(bar, r.x);
    }
  });
  return { right, bar };
}

describe("placeColorScale", () => {
  const echarts = loadECharts();
  if (echarts) registerRenderers(echarts);
  const cases: [string, Record<string, unknown>][] = [
    [
      "spectrogram",
      buildSpectrogramOption(
        {
          time: [0, 1, 2],
          frequency: [0, 10],
          values: [
            [1, 2, 3],
            [4, 5, 6],
          ],
        },
        { db: false },
      ),
    ],
    [
      "clustered heatmap",
      buildHeatmapOption(
        {
          values: [
            [1, 0.2, -0.4],
            [0.2, 1, 0.1],
            [-0.4, 0.1, 1],
          ],
          row_tree: {
            merge: [
              [-1, -2],
              [-3, 1],
            ],
            height: [0.5, 1],
            order: [1, 2, 3],
          },
        },
        {},
      ),
    ],
  ];
  for (const [name, option] of cases) {
    it.skipIf(!echarts)(`leaves ${KEY_GAP} px between the ${name}'s plot and its scale`, () => {
      const chart = echarts.init(null, null, {
        renderer: "svg",
        ssr: true,
        width: 700,
        height: 420,
      });
      chart.setOption({ ...option, animation: false });
      expect(placeColorScale(chart)).toBe(true);
      const { right, bar } = measure(chart);
      expect(bar - right).toBeCloseTo(KEY_GAP, 0);
      // A second pass leaves it in place.
      placeColorScale(chart);
      expect(measure(chart).bar).toBeCloseTo(bar, 6);
      chart.dispose();
    });
  }

  it.skipIf(!echarts)("draws the scale's title above its bar, aligned with it", () => {
    const option = buildSpectrogramOption(
      {
        time: [0, 1, 2],
        frequency: [0, 10],
        values: [
          [1, 2, 3],
          [4, 5, 6],
        ],
      },
      { db: false },
    );
    const authored = {
      ...option,
      animation: false,
      visualMap: { ...(option.visualMap as object), text: ["decibels", ""] },
    };
    const chart = echarts.init(null, null, { renderer: "svg", ssr: true, width: 700, height: 420 });
    chart.setOption(authored);
    placeColorScale(chart, { option: authored });
    const { right, bar } = measure(chart);
    const graphic = chart.getOption().graphic[0].elements[0];
    expect(graphic.style.text).toBe("decibels");
    expect(graphic.left).toBeCloseTo(right + KEY_GAP, 6);
    expect(bar).toBeCloseTo(graphic.left, 0);
    expect(chart.getOption().visualMap[0].text).toEqual(["", ""]);
    chart.dispose();
  });

  it.skipIf(!echarts)(`leaves ${KEY_GAP} px between a scatter chart's plot and its scale`, () => {
    const option = buildScatterOption(
      { columns: { x: [1, 2, 3], y: [1, 2, 3], c: [0, 1, 2] } },
      { x: "x", y: "y", color: "c" },
      { animation: false },
    );
    const chart = echarts.init(null, null, { renderer: "svg", ssr: true, width: 600, height: 400 });
    chart.setOption(option);
    fitKeys(echarts, chart, { option, placement: { side: "right", inside: false } });
    const { right, bar } = measure(chart);
    expect(bar - right).toBeCloseTo(KEY_GAP, 0);
    chart.dispose();
  });
});
