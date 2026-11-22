import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { A3 } from "@rtemis/a3";
import { describe, expect, it } from "vitest";
import { a3SurfaceHints, buildA3Option } from "../src/charts/a3/config.ts";
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

  it("aligns the legend with the plot area with legend members too", () => {
    for (const placement of ["outside", "inside"]) {
      const chart = render(bar);
      fitChart(echarts, chart, {
        type: "bar",
        option: bar,
        members: { position: "top", placement },
      });
      const grid = chart.getModel().getComponent("grid").coordinateSystem.getRect();
      const legend = chart.getModel().getComponent("legend");
      const group = chart.getViewOfComponentModel(legend).group;
      const box = group.getBoundingRect().clone();
      box.applyTransform(group.getComputedTransform());
      expect(box.x + box.width / 2).toBeCloseTo(grid.x + grid.width / 2, 0);
      if (placement === "outside") expect(box.y + box.height).toBeLessThanOrEqual(grid.y);
      chart.dispose();
    }
  });

  it("fits an A3 diagram without legend members: the rail sets the right margin", () => {
    const doc = new A3(
      JSON.parse(readFileSync(new URL("./fixtures/mapt.a3.json", import.meta.url), "utf8")),
    ).toData();
    const option = buildA3Option(doc, { n_per_row: 21 }) as unknown as Record<string, unknown>;
    const chart = render(option);
    const hints = { ...a3SurfaceHints(doc, { n_per_row: 21 }), autoGrid: true, legendGap: 24 };
    fitChart(echarts, chart, { type: "a3", option, hints: { a3: hints } });
    const legend = chart.getModel().getComponent("legend");
    const rail = chart.getViewOfComponentModel(legend).group.getBoundingRect().width;
    const legendRight = (option.legend as { right: number }).right;
    expect(chart.getOption().grid[0].right).toBeCloseTo(legendRight + rail + 24, 5);
    chart.dispose();
  });

  it("returns no height for a chart without an A3 surface", () => {
    const chart = render(bar);
    expect(fitChart(echarts, chart, { type: "bar", option: bar, members: {} }).height).toBe(
      undefined,
    );
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
