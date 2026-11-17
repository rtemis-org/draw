import { describe, expect, it } from "vitest";
import {
  buildHistogramOption,
  type HistogramData,
  histogramValueLabel,
} from "../src/charts/histogram/config.ts";
import { HISTOGRAM, histogramRenderers } from "../src/charts/histogram/renderers.ts";
import { formatNumber } from "../src/shared/format.ts";
import { RENDERER_NAMES } from "../src/shared/renderers.ts";

type Series = {
  type: string;
  name: string;
  data: number[][];
  dimensions?: string[];
  encode?: Record<string, unknown>;
  itemPayload?: Record<string, unknown>;
  xAxisIndex?: number;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];
const heightsOf = (s: Series | undefined) => (s?.data ?? []).map((d) => d[2]);

const one: HistogramData = {
  edges: [0, 2, 4, 6],
  samples: [{ name: "x", n: 10, counts: [2, 5, 3] }],
};
const two: HistogramData = {
  edges: [0, 2, 4, 6],
  samples: [
    { name: "a", n: 10, counts: [2, 5, 3] },
    { name: "b", n: 4, counts: [0, 1, 3] },
  ],
};

describe("buildHistogramOption", () => {
  it("draws each bin between its edges with its count", () => {
    const option = buildHistogramOption(one, {});
    const [bars] = seriesOf(option);
    expect(bars?.type).toBe("custom");
    expect(bars?.data).toEqual([
      [0, 2, 2, 2],
      [2, 4, 5, 5],
      [4, 6, 3, 3],
    ]);
    expect(bars?.dimensions).toEqual(["Lower", "Upper", "Height", "Count"]);
    expect(bars?.encode).toEqual({ x: [0, 1], y: 2, tooltip: [0, 1, 3] });
    expect(option.xAxis).toMatchObject({ type: "value", scale: true });
    expect(option.yAxis).toMatchObject({ type: "value", min: 0 });
    expect(option.legend).toBeUndefined();
    expect(option.animationDurationUpdate).toBe(0);
    expect(option.tooltip).toEqual({ trigger: "item", valueFormatter: formatNumber });
    expect(RENDERER_NAMES).toContain(HISTOGRAM);
  });

  it("normalizes each sample's heights", () => {
    const height = (normalization: "probability" | "percent" | "density" | "count_density") =>
      heightsOf(seriesOf(buildHistogramOption(one, { normalization }))[0]);
    expect(height("probability")).toEqual([0.2, 0.5, 0.3]);
    expect(height("percent")).toEqual([20, 50, 30]);
    expect(height("density")).toEqual([0.1, 0.25, 0.15]);
    expect(height("count_density")).toEqual([1, 2.5, 1.5]);
    const [bars] = seriesOf(buildHistogramOption(one, { normalization: "density" }));
    expect(bars?.dimensions?.[2]).toBe("Density");
    expect(bars?.encode?.tooltip).toEqual([0, 1, 2, 3]);
  });

  it("takes unequal widths only on a density scale", () => {
    const unequal: HistogramData = {
      edges: [0, 1, 4],
      samples: [{ name: "x", n: 4, counts: [1, 3] }],
    };
    expect(() => buildHistogramOption(unequal, {})).toThrow("unequal-width bins");
    expect(
      heightsOf(seriesOf(buildHistogramOption(unequal, { normalization: "density" }))[0]),
    ).toEqual([0.25, 0.25]);
  });

  it("draws a bin statistic from the host's values", () => {
    const data: HistogramData = {
      edges: [0, 2, 4],
      samples: [{ name: "x", n: 3, counts: [1, 2], values: [1.5, 6.5] }],
    };
    const [bars] = seriesOf(buildHistogramOption(data, { bin_stat: "sum" }));
    expect(heightsOf(bars)).toEqual([1.5, 6.5]);
    expect(bars?.dimensions?.[2]).toBe("sum");
    expect(() => buildHistogramOption(data, { bin_stat: "sum", normalization: "percent" })).toThrow(
      "bin_stat",
    );
  });

  it("overlays several samples more translucently, with a legend", () => {
    const option = buildHistogramOption(two, {});
    const [a, b] = seriesOf(option);
    expect(a?.itemPayload).toMatchObject({ fillAlpha: 0.5, borderAlpha: 1, mode: "overlay" });
    expect(b?.itemPayload).toBe(a?.itemPayload);
    expect(option.legend).toEqual({ data: ["a", "b"], itemStyle: { opacity: 1 } });
    const dodged = seriesOf(buildHistogramOption(two, { bar_mode: "group" }));
    expect(dodged[0]?.itemPayload).toMatchObject({ fillAlpha: 0.75, mode: "group" });
    expect(seriesOf(buildHistogramOption(two, { fill_alpha: 0.3 }))[0]?.itemPayload).toMatchObject({
      fillAlpha: 0.3,
    });
  });

  it("pads a stacked y axis to the tallest stack", () => {
    const option = buildHistogramOption(two, { bar_mode: "stack" });
    expect(option.yAxis).toMatchObject({ min: 0, max: 6 * 1.05 });
    expect(seriesOf(option)[0]?.itemPayload).toMatchObject({
      seriesIndices: [0, 1],
      heights: [
        [2, 5, 3],
        [0, 1, 3],
      ],
    });
  });

  it("draws each sample's density on the bins' scale", () => {
    const data: HistogramData = {
      edges: [0, 2, 4],
      samples: [
        { name: "x", n: 10, counts: [4, 6], density: { x: [0, 2, 4], y: [0.1, 0.2, 0.1] } },
      ],
    };
    const [, curve] = seriesOf(buildHistogramOption(data, { density: true }));
    expect(curve?.type).toBe("line");
    expect(curve?.name).toBe("x");
    // A count scale multiplies a density by the sample size and the bin width.
    expect(curve?.data).toEqual([
      [0, 2],
      [2, 4],
      [4, 2],
    ]);
    const [, onDensity] = seriesOf(
      buildHistogramOption(data, { density: true, normalization: "density" }),
    );
    expect(onDensity?.data[1]).toEqual([2, 0.2]);
    expect(() => buildHistogramOption(data, { density: true, bar_mode: "stack" })).toThrow(
      "bar_mode",
    );
  });

  it("gives each sample a row in ridge mode", () => {
    const option = buildHistogramOption(two, { mode: "ridge", xlab: "Value" });
    expect(option.legend).toEqual({ show: false });
    expect(option.grid).toHaveLength(2);
    expect((option.grid as Record<string, unknown>[])[0]).toMatchObject({
      outerBoundsMode: "none",
    });
    const xAxes = option.xAxis as Record<string, unknown>[];
    expect(xAxes.map((a) => a.show)).toEqual([false, true]);
    expect(xAxes[1]).toMatchObject({ name: "Value", min: 0, max: 6 });
    const yAxes = option.yAxis as Record<string, unknown>[];
    expect(yAxes.map((a) => a.name)).toEqual(["a", "b"]);
    expect(yAxes[0]).toMatchObject({ min: 0, max: 5 * 1.05 });
    expect(seriesOf(option).map((s) => s.xAxisIndex)).toEqual([0, 1]);
  });

  it("names the axes it is given and zooms x without filtering bins", () => {
    const option = buildHistogramOption(one, { xlab: "Age", ylab: "", zoom: true });
    expect(option.xAxis).toMatchObject({ name: "Age", nameLocation: "middle" });
    expect((option.yAxis as Record<string, unknown>).name).toBeUndefined();
    const zoom = option.dataZoom as Record<string, unknown>[];
    expect(zoom.map((z) => z.filterMode)).toEqual(["none", "none"]);
  });

  it("stops on edges that do not increase or counts of the wrong length", () => {
    expect(() => buildHistogramOption({ edges: [0, 0], samples: one.samples }, {})).toThrow(
      "strictly increasing",
    );
    expect(() =>
      buildHistogramOption({ edges: [0, 1], samples: [{ name: "x", n: 1, counts: [1, 0] }] }, {}),
    ).toThrow("one count per bin (1)");
  });
});

describe("histogramValueLabel", () => {
  it("names the height by normalization or statistic", () => {
    expect(histogramValueLabel({})).toBe("Count");
    expect(histogramValueLabel({ normalization: "count_density" })).toBe("Count density");
    expect(histogramValueLabel({ normalization: "percent" })).toBe("Percent");
    expect(histogramValueLabel({ bin_stat: "mean" })).toBe("Mean");
    expect(histogramValueLabel({ bin_stat: "sum", x: ["len"] })).toBe("Sum of len");
    expect(histogramValueLabel({ bin_stat: "max", x: "len" }, (c) => c.toUpperCase())).toBe(
      "Maximum LEN",
    );
  });
});

describe("rtemis.histogram.v1", () => {
  const render = histogramRenderers[HISTOGRAM] as (params: unknown, api: unknown) => unknown;
  const api = (active: number[], values: number[]) => ({
    currentSeriesIndices: () => active,
    coord: ([x, y]: number[]) => [(x as number) * 10, 100 - (y as number) * 10],
    value: (i: number) => values[i] as number,
    visual: () => "#123456",
  });
  const payload = (mode: string) => ({
    fillAlpha: 0.5,
    borderAlpha: 1,
    mode,
    seriesIndices: [0, 1],
    heights: [
      [2, 5],
      [3, 1],
    ],
  });

  it("draws nothing for an empty bin", () => {
    expect(
      render(
        { itemPayload: payload("overlay"), seriesIndex: 0, dataIndex: 0 },
        api([0], [0, 2, 0, 0]),
      ),
    ).toBeUndefined();
  });

  it("dodges the shown series within a bin", () => {
    const rect = render(
      { itemPayload: payload("group"), seriesIndex: 1, dataIndex: 0 },
      api([0, 1], [0, 2, 3, 3]),
    ) as { shape: Record<string, number> };
    expect(rect.shape).toEqual({ x: 10, y: 70, width: 10, height: 30 });
  });

  it("stacks on the shown series below, and skips a hidden one", () => {
    const stacked = render(
      { itemPayload: payload("stack"), seriesIndex: 1, dataIndex: 0 },
      api([0, 1], [0, 2, 3, 3]),
    ) as { shape: Record<string, number> };
    expect(stacked.shape).toEqual({ x: 0, y: 50, width: 20, height: 30 });
    const alone = render(
      { itemPayload: payload("stack"), seriesIndex: 1, dataIndex: 0 },
      api([1], [0, 2, 3, 3]),
    ) as { shape: Record<string, number> };
    expect(alone.shape).toEqual({ x: 0, y: 70, width: 20, height: 30 });
    expect(
      render(
        { itemPayload: payload("stack"), seriesIndex: 0, dataIndex: 0 },
        api([1], [0, 2, 2, 2]),
      ),
    ).toBeUndefined();
  });
});
