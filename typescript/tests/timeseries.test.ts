import { describe, expect, it } from "vitest";
import {
  axisTooltip,
  buildTimeSeriesOption,
  rollingValues,
} from "../src/charts/timeseries/config.ts";
import { AXIS_LABELS, timeSeriesRenderers } from "../src/charts/timeseries/renderers.ts";
import { placeZoomSlider, ZOOM_SLIDER_ID } from "../src/shared/layout.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = Record<string, unknown> & { name: string; data: unknown[] };
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

describe("rollingValues", () => {
  it("summarizes the values present when a majority is there, as rtemis.draw does", () => {
    const v = [1, 2, 3, 4, 5, 6, 7, 8];
    expect(rollingValues(v, { window: 4, roll_fn: "mean", align: "left" })).toEqual([
      2.5,
      3.5,
      4.5,
      5.5,
      6.5,
      7,
      null,
      null,
    ]);
    expect(rollingValues(v, { window: 4, roll_fn: "mean", align: "right" })).toEqual([
      null,
      null,
      2,
      2.5,
      3.5,
      4.5,
      5.5,
      6.5,
    ]);
    expect(rollingValues(v, { window: 4, roll_fn: "mean", align: "center" })).toEqual([
      2,
      2.5,
      3.5,
      4.5,
      5.5,
      6.5,
      7,
      null,
    ]);
  });

  it("needs the whole window for a sum, and min_obs where set", () => {
    expect(rollingValues([1, 2, 6], { window: 3, roll_fn: "sum", align: "right" })).toEqual([
      null,
      null,
      9,
    ]);
    expect(rollingValues([1, null, 6], { window: 3, roll_fn: "sum", align: "right" })).toEqual([
      null,
      null,
      null,
    ]);
    expect(rollingValues([1, null, 6], { window: 3, roll_fn: "median", align: "right" })).toEqual([
      null,
      null,
      3.5,
    ]);
    expect(
      rollingValues([1, null, null], { window: 3, roll_fn: "mean", align: "right", min_obs: 1 }),
    ).toEqual([1, 1, 1]);
    expect(rollingValues([1, 2], { roll_fn: "none" })).toEqual([null, null]);
  });
});

describe("buildTimeSeriesOption", () => {
  const data: TabularData = {
    columns: { t: [3, 1, 2], a: [3, 1, 2], b: [6, 2, 4] },
  };

  it("sorts each series by time and pairs raw and rolled lines under one name", () => {
    const option = buildTimeSeriesOption(data, {
      x: "t",
      y: ["a", "b"],
      window: 3,
      roll_fn: "sum",
      zoom: true,
    });
    const series = seriesOf(option);
    expect(series.map((s) => s.name)).toEqual(["a", "a", "b", "b"]);
    expect(series[0]?.data).toEqual([
      [1, 1],
      [2, 2],
      [3, 3],
    ]);
    expect(series[1]?.data).toEqual([
      [1, null],
      [2, 6],
      [3, null],
    ]);
    expect(series[3]?.data[1]).toEqual([2, 12]);
    expect(series[1]?.showSymbol).toBe(false);
    expect(option.legend).toEqual({ data: ["a", "b"], itemStyle: { opacity: 1 } });
    expect(option.dataZoom).toHaveLength(2);
  });

  it("splits one y column by group in level order", () => {
    const grouped = { columns: { t: [3, 1, 2], v: [6, 2, 4], g: ["B", "A", "B"] } };
    const series = seriesOf(buildTimeSeriesOption(grouped, { x: "t", y: "v", group: "g" }));
    expect(series.map((s) => s.name)).toEqual(["B", "A"]);
    expect(series[0]?.data).toEqual([
      [2, 4],
      [3, 6],
    ]);
    expect(series[0]?.symbol).toBe("circle");
  });

  it("splits every y and y2 column by group, named column / level", () => {
    const grouped = {
      columns: { t: [1, 2, 3, 4], v: [1, 2, 3, 4], w: [5, 6, 7, 8], g: ["F", "M", "F", "M"] },
    };
    const option = buildTimeSeriesOption(grouped, { x: "t", y: "v", y2: "w", group: "g" });
    const series = seriesOf(option);
    expect(series.map((s) => [s.name, s.yAxisIndex])).toEqual([
      ["v / F", 0],
      ["v / M", 0],
      ["w / F", 1],
      ["w / M", 1],
    ]);
    expect(series[3]?.data).toEqual([
      [2, 6],
      [4, 8],
    ]);
    expect((option.legend as { data: string[] }).data).toEqual([
      "v / F",
      "v / M",
      "w / F",
      "w / M",
    ]);
  });

  it("draws y2 on a right axis with its own times", () => {
    const dual = {
      columns: {
        t: [1, 2, 3, null],
        v: [2, 4, 6, null],
        t2: [null, null, null, 0.5],
        w: [null, null, null, 100],
      },
    };
    const option = buildTimeSeriesOption(dual, { x: "t", y: "v", x2: "t2", y2: "w", y2lab: "W" });
    const series = seriesOf(option);
    expect(series[1]).toMatchObject({ name: "w", yAxisIndex: 1, data: [[0.5, 100]] });
    expect(option.yAxis).toHaveLength(2);
    expect((option.yAxis as Record<string, unknown>[])[1]).toMatchObject({
      position: "right",
      name: "W",
    });
  });

  it("places times in UTC on a time axis and stops on mixed time types", () => {
    const day = 86_400_000;
    const option = buildTimeSeriesOption(
      { columns: { t: [0, day, 2 * day], v: [1, 2, 3] }, types: { t: "time" } },
      { x: "t", y: "v" },
    );
    expect((option.xAxis as Record<string, unknown>).type).toBe("time");
    expect(option.useUTC).toBe(true);
    expect(() =>
      buildTimeSeriesOption(
        { columns: { t: [0, day], v: [1, 2], t2: [1, 2], w: [1, 2] }, types: { t: "time" } },
        { x: "t", y: "v", x2: "t2", y2: "w" },
      ),
    ).toThrow("throughout");
  });

  it("labels zeitgeber times and shades runs of ones", () => {
    const zt = {
      columns: { t: [0, 2, 8, 24], v: [1, 3, 2, 4], phase: [0, 2, 8, 0], night: [0, 1, 1, 0] },
    };
    const option = buildTimeSeriesOption(zt, {
      x: "t",
      y: "v",
      zt: "phase",
      show_zt_every: 3,
      shade_bin: "night",
    });
    const series = seriesOf(option);
    const labels = series[series.length - 1] as Series;
    expect(labels.renderItem).toBe(AXIS_LABELS);
    expect(labels.itemStyle).toEqual({ color: "#808080" });
    expect(labels.data).toEqual([
      [0, "0"],
      [24, "0"],
    ]);
    expect((option.xAxis as Record<string, unknown>).axisLabel).toEqual({ show: false });
    expect((series[0] as Series).markArea).toEqual({
      silent: true,
      label: { show: false },
      data: [[{ xAxis: 2, itemStyle: { color: "#808080", opacity: 0.15 } }, { xAxis: 8 }]],
    });
    expect(() =>
      buildTimeSeriesOption(
        { columns: { ...zt.columns, night: [0, 2, 1, 0] } },
        { x: "t", y: "v", shade_bin: "night" },
      ),
    ).toThrow("0 or 1");
  });

  it("reports what it cannot draw", () => {
    expect(() => buildTimeSeriesOption(data, { x: "t" })).toThrow("both `x` and `y`");
    expect(() =>
      buildTimeSeriesOption({ columns: { t: [1, 2], v: [null, null] } }, { x: "t", y: "v" }),
    ).toThrow("at least one time with a value");
    expect(() =>
      buildTimeSeriesOption({ columns: { t: [1, 2], v: [1, Infinity] } }, { x: "t", y: "v" }),
    ).toThrow("row 2");
    expect(() =>
      buildTimeSeriesOption({ columns: { t: [1, 2], v: [1, 2] } }, { x: "t", y: "v", x2: "t" }),
    ).toThrow("Supply `y2`");
    expect(() =>
      buildTimeSeriesOption(
        { columns: { t: [1, 2, 3], v: [1e308, 1e308, 1e308] } },
        { x: "t", y: "v", roll_fn: "sum", window: 2 },
      ),
    ).toThrow("rolling summaries finite");
  });
});

describe("the zeitgeber label renderer", () => {
  it("writes the label under its time inside the plot only", () => {
    const render = timeSeriesRenderers[AXIS_LABELS] as (
      p: unknown,
      a: unknown,
    ) => { style: Record<string, unknown> } | undefined;
    const api = (t: number) => ({
      coord: ([x]: number[]) => [x as number, 0],
      value: (d: number) => (d === 0 ? t : "6"),
      font: () => "12px sans-serif",
      visual: () => "#808080",
    });
    const params = { coordSys: { x: 10, y: 0, width: 100, height: 50 } };
    expect(render(params, api(40))?.style).toMatchObject({
      x: 40,
      y: 60,
      text: "6",
      fill: "#808080",
    });
    expect(render(params, api(200))).toBeUndefined();
  });
});

describe("axisTooltip", () => {
  it("writes each series' observation and rolling summary on one row", () => {
    const html = axisTooltip(
      [
        {
          seriesName: "F",
          seriesIndex: 0,
          value: [1, 0.51111],
          marker: "<i></i>",
          axisValueLabel: "2012-05-11",
        },
        { seriesName: "F", seriesIndex: 1, value: [1, 0.5124], marker: "<i></i>" },
        { seriesName: "<b>", seriesIndex: 2, value: [1, null], marker: "" },
        { seriesName: "<b>", seriesIndex: 3, value: [1, 2], marker: "" },
      ],
      new Set([1, 3]),
      "mean",
    );
    expect(html).toBe(
      '2012-05-11<br/><i></i>F&nbsp;&nbsp;<b>0.511</b> <span style="opacity:0.7">mean 0.512</span><br/>&#60;b&#62;&nbsp;&nbsp;<b>–</b> <span style="opacity:0.7">mean 2</span>',
    );
  });
});

describe("placeZoomSlider", () => {
  it("places the slider above a margin the host set after the build", () => {
    const option = buildTimeSeriesOption(
      { columns: { t: [1, 2, 3], v: [1, 2, 3] } },
      { x: "t", y: "v", zoom: true },
    );
    (option.grid as Record<string, unknown>).bottom = 60;
    placeZoomSlider(option);
    const slider = (option.dataZoom as Record<string, unknown>[]).find(
      (z) => z.id === ZOOM_SLIDER_ID,
    );
    expect(slider?.bottom).toBe(67);
    expect((option.grid as Record<string, unknown>).bottom).toBe(60 + 7 + 30 + 8);
  });
});
