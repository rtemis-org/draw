import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import { buildBarOption } from "../src/charts/bar/config.ts";
import { buildScatterOption } from "../src/charts/scatter/config.ts";
import { FIT_EVENTS, fit as fitChart, fitState } from "../src/layout/fit.ts";
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

const bar = buildBarOption(
  { columns: { g: ["A", "B", "A", "B"], u: [3, 7, 2, 5], s: ["f", "f", "m", "m"] } },
  { x: "g", y: "u", group: "s" },
);

describe.skipIf(!echarts)("fit", () => {
  it("returns the state it leaves, and a second pass leaves it unchanged", () => {
    const chart = render(bar);
    const first = fitChart(echarts, chart, { type: "bar", option: bar });
    expect(first.state).toBe(fitState(chart));
    expect(fitChart(echarts, chart, { type: "bar", option: bar }).state).toBe(first.state);
    chart.dispose();
  });

  it("aligns a centered legend row with the plot area", () => {
    const chart = render(bar);
    fitChart(echarts, chart, { type: "bar", option: bar });
    const grid = chart.getModel().getComponent("grid").coordinateSystem.getRect();
    const legend = chart.getModel().getComponent("legend");
    const box = chart.getViewOfComponentModel(legend).group.getBoundingRect();
    const left = chart.getOption().legend[0].left as number;
    expect(left + box.width / 2).toBeCloseTo(grid.x + grid.width / 2, 0);
    chart.dispose();
  });

  it("places a scatter chart's keys on the side the host asks for", () => {
    const option = buildScatterOption(
      { columns: { x: [1, 2, 3, 4], y: [2, 1, 4, 3], c: [0, 1, 2, 3] } },
      { x: "x", y: "y", color: "c" },
    );
    const right = render(option);
    fitChart(echarts, right, { type: "scatter", option });
    const left = render(option);
    fitChart(echarts, left, {
      type: "scatter",
      option,
      placement: { side: "left", inside: false },
    });
    const scaleLeft = (chart: typeof right) => chart.getOption().visualMap[0].left as number;
    expect(scaleLeft(left)).toBeLessThan(scaleLeft(right));
    right.dispose();
    left.dispose();
  });

  it("leaves a chart that lays out its own legend as built", () => {
    const chart = render(bar);
    const before = JSON.stringify(chart.getOption().legend);
    fitChart(echarts, chart, { type: "a3", option: bar });
    expect(JSON.stringify(chart.getOption().legend)).toBe(before);
    chart.dispose();
  });

  it("names the events after which a host fits again", () => {
    expect(FIT_EVENTS).toEqual(["legendselectchanged", "legendscroll", "datazoom", "restore"]);
  });
});
