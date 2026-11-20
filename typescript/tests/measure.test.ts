import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import { buildBarOption } from "../src/charts/bar/config.ts";
import { buildPieOption } from "../src/charts/pie/config.ts";
import { buildSpectrogramOption } from "../src/charts/spectrogram/config.ts";
import { KEY_GAP, placeColorScale } from "../src/layout/keys.ts";
import { measureLayout } from "../src/layout/measure.ts";
import { registerRenderers } from "../src/shared/renderers.ts";

// biome-ignore lint/suspicious/noExplicitAny: the R package's ECharts bundle carries no types.
function loadECharts(): any {
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  return existsSync(path) ? createRequire(import.meta.url)(path) : null;
}

const echarts = loadECharts();
if (echarts) registerRenderers(echarts);

function render(option: Record<string, unknown>) {
  const chart = echarts.init(null, null, { renderer: "svg", ssr: true, width: 600, height: 400 });
  chart.setOption({ ...option, animation: false });
  return chart;
}

const inside = (inner: { x: number; y: number; width: number; height: number }) =>
  inner.x >= 0 && inner.y >= 0 && inner.x + inner.width <= 600 && inner.y + inner.height <= 400;

describe("measureLayout", () => {
  it.skipIf(!echarts)("measures a grid, a legend, and a title", () => {
    const option = buildBarOption(
      { columns: { g: ["A", "B", "A", "B"], u: [3, 7, 2, 5], s: ["f", "f", "m", "m"] } },
      { x: "g", y: "u", group: "s", title: "Counts" },
    );
    const chart = render(option);
    const layout = measureLayout(chart);
    expect(layout.pane).toEqual({ width: 600, height: 400 });
    expect(layout.plots).toHaveLength(1);
    expect(layout.plotsFrom).toBe("grid");
    const [plot] = layout.plots;
    expect(plot && inside(plot)).toBe(true);
    expect(layout.legend).not.toBeNull();
    expect(layout.title).not.toBeNull();
    // The title sits above the plot, and the legend outside it.
    const bottom = (r: { y: number; height: number } | null | undefined) =>
      (r?.y ?? 0) + (r?.height ?? 0);
    expect(layout.title?.y ?? Number.POSITIVE_INFINITY).toBeLessThan(plot?.y ?? 0);
    const legend = layout.legend;
    expect(legend !== null && (bottom(legend) <= (plot?.y ?? 0) || legend.y >= bottom(plot))).toBe(
      true,
    );
    expect(layout.colorScale).toBeNull();
    chart.dispose();
  });

  it.skipIf(!echarts)("measures the drawn series of a chart without a grid", () => {
    const chart = render(
      buildPieOption(
        { columns: { kind: ["a", "b", "c"], n: [1, 2, 3] } },
        {
          values: "n",
          labels: "kind",
        },
      ),
    );
    const layout = measureLayout(chart);
    expect(layout.plots).toHaveLength(1);
    expect(layout.plotsFrom).toBe("series");
    expect(layout.plots[0]?.width).toBeGreaterThan(100);
    chart.dispose();
  });

  it.skipIf(!echarts)("measures a color scale and its bar", () => {
    const chart = render(
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
    );
    placeColorScale(chart);
    const layout = measureLayout(chart);
    const plot = layout.plots[0];
    const bar = layout.colorScaleBar;
    expect(plot && bar).toBeTruthy();
    if (plot && bar) expect(bar.x - (plot.x + plot.width)).toBeCloseTo(KEY_GAP, 0);
    expect(layout.colorScale?.width).toBeGreaterThan(bar?.width ?? 0);
    // The scale's transparent, borderless background paints nothing and
    // is left out, so the scale starts at its bar's handles, clear of the plot.
    if (plot) expect(layout.colorScale?.x).toBeGreaterThan(plot.x + plot.width);
    chart.dispose();
  });
});
