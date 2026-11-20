import { describe, expect, it } from "vitest";
import {
  buildSurvivalOption,
  layoutSurvivalRisk,
  type SurvivalData,
  survivalMedian,
} from "../src/charts/survival/config.ts";
import { survivalRenderers } from "../src/charts/survival/renderers.ts";

type Series = {
  type: string;
  name?: string;
  data: unknown[];
  renderItem?: string;
  itemPayload?: Record<string, unknown>;
  markLine?: { data: { xAxis: number }[] };
  label?: unknown;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

const curve = {
  name: "A",
  time: [0, 2, 5],
  survival: [1, 0.6, 0.2],
  lower: [1, 0.4, null],
  upper: [1, 0.8, null],
  n_censor: [0, 2, 1],
  n_risk: [10, 8, 1],
};
const data: SurvivalData = { curves: [curve] };

describe("survivalMedian", () => {
  it("follows R's crossings and plateau midpoints", () => {
    expect(survivalMedian({ time: [0, 1, 3], survival: [1, 0.7, 0.4] })).toBe(3);
    expect(survivalMedian({ time: [0, 1, 2, 4], survival: [1, 0.5, 0.5, 0.2] })).toBe(2.5);
    expect(survivalMedian({ time: [0, 1, 2, 5], survival: [1, 0.5, 0.5, 0.5] })).toBe(3);
    expect(survivalMedian({ time: [0, 1, 3], survival: [1, 0.7, 0.6] })).toBeNull();
  });
});

describe("buildSurvivalOption", () => {
  it("draws right-continuous steps with hover text, the band, censor ticks, and the median", () => {
    const series = seriesOf(buildSurvivalOption(data, {}));
    expect(series.map((s) => s.type)).toEqual([
      "line",
      "line",
      "line",
      "scatter",
      "line",
      "scatter",
    ]);
    expect(series[0]).toMatchObject({ step: "end", name: "A" });
    expect(series[0]?.data[1]).toEqual([2, 0.6, "0.600", "0.400", "0.800", "8", "2"]);
    // The band's width up to the missing interval, then a gap.
    expect(series[2]?.data).toEqual([
      [0, 0],
      [2, 0.4],
      [5, 0.4],
      [5, null],
    ]);
    expect(series[3]?.data).toHaveLength(2);
    expect(series[4]?.data).toEqual([
      [0, 0.5],
      [5, 0.5],
      [5, 0],
    ]);
    expect(series[5]?.data).toEqual([{ value: [5, 0], name: "Median 5" }]);
  });

  it("labels landmark survival as a percent within each curve's range", () => {
    const series = seriesOf(
      buildSurvivalOption(data, { show_median: false, landmarks: [-1, 1, 3, 9] }),
    );
    const marks = series.find((s) => s.type === "scatter" && s.label);
    expect(marks?.data).toEqual([
      { value: [1, 1], name: "100%" },
      { value: [3, 0.6], name: "60%" },
    ]);
    expect(series.at(-1)?.markLine?.data).toEqual([{ xAxis: 1 }, { xAxis: 3 }]);
  });

  it("draws one risk row per curve under its name, and labels the risk times on the axis", () => {
    const option = buildSurvivalOption(
      {
        curves: [curve, { ...curve, name: "B" }],
        risk: {
          time: [0, 1, 5],
          counts: [
            [10, null, 1],
            [9, 7, 2.5],
          ],
        },
      },
      { risk_table: true, palette: ["#111111", "#222222"] },
    );
    const rows = seriesOf(option).filter((s) => s.renderItem === "rtemis.survival_risk.v1");
    expect(rows.map((s) => s.name)).toEqual(["A", "B", undefined]);
    expect(rows[0]?.itemPayload?.labels).toEqual(["10", "NA", "1"]);
    expect(rows[1]?.itemPayload?.labels).toEqual(["9", "7", "2.5"]);
    expect(rows[2]?.data).toEqual([[0, -1]]);
    expect(option.xAxis).toMatchObject({ axisLabel: { customValues: [0, 1, 5] } });
    // Under the axis name (gap 30, 12 px), rows of 1.6 × the 12 px labels.
    const offset = 30 + Math.ceil(12 * 1.3) + 12;
    expect(rows[0]?.itemPayload).toMatchObject({ offset, lineHeight: 19, fontSize: 12 });
    expect((option.grid as { bottom: number }).bottom).toBe(offset + 2 * 19 + 10);
    expect(() => buildSurvivalOption(data, { risk_table: true })).toThrow("risk records");
  });

  it("draws no legend for one unnamed curve", () => {
    const option = buildSurvivalOption({ curves: [{ ...curve, name: null }] }, {});
    expect(option.legend).toMatchObject({ show: false, data: ["Survival"] });
    expect(buildSurvivalOption(data, {}).legend).toMatchObject({ show: true });
  });
});

describe("the risk strip renderer", () => {
  const payload = {
    offset: 54,
    lineHeight: 19,
    rows: 2,
    fontSize: 12,
    labels: ["10", "8"],
    caption: "At risk",
    captionOffset: 41,
    textColor: "#333333",
  };
  const api = (time: number, row: number) => ({
    coord: ([t]: number[]) => [100 + 10 * (t as number), 300],
    value: (d: number) => (d === 0 ? time : row),
    visual: () => "#ff0000",
  });
  const render = survivalRenderers["rtemis.survival_risk.v1"] as (
    p: unknown,
    a: unknown,
  ) => {
    x?: number;
    rotation?: number;
    style: Record<string, unknown>;
  };
  it("puts each count under its time, in its curve's color", () => {
    const el = render({ itemPayload: payload, dataIndex: 1 }, api(5, 1));
    expect(el.style).toMatchObject({
      x: 150,
      y: 300 + 54 + 1.5 * 19,
      text: "8",
      fill: "#ff0000",
    });
  });
  it("draws the caption rotated beside the rows, left of the plot's edge", () => {
    const el = render({ itemPayload: payload, dataIndex: 0 }, api(0, -1));
    expect(el).toMatchObject({ x: 100 - 41, rotation: Math.PI / 2 });
    expect(el.style).toMatchObject({ text: "At risk", fill: "#333333" });
  });
});

describe("layoutSurvivalRisk", () => {
  it("follows a restyled time axis", () => {
    const option = buildSurvivalOption(
      { curves: [curve], risk: { time: [0, 5], counts: [[10, 1]] } },
      { risk_table: true },
    );
    const x = option.xAxis as Record<string, unknown>;
    x.nameGap = 40;
    x.nameTextStyle = { fontSize: 16, textMargin: [10, 3, 10, 3] };
    x.axisLabel = { ...(x.axisLabel as object), fontSize: 15 };
    const rows = seriesOf(layoutSurvivalRisk(option)).filter((s) => s.renderItem);
    const offset = 40 + 10 + Math.ceil(16 * 1.3) + 10 + 12;
    expect(rows[0]?.itemPayload).toMatchObject({ offset, lineHeight: 24, fontSize: 15 });
    expect((option.grid as { bottom: number }).bottom).toBe(offset + 24 + 10);
    expect(option.xAxis).toMatchObject({ axisLabel: { showMinLabel: true, showMaxLabel: true } });
  });

  it("places the caption where the y axis name sits", () => {
    const captionOffset = (yAxis: Record<string, unknown> | null) => {
      const option = buildSurvivalOption(
        { curves: [curve], risk: { time: [0, 5], counts: [[10, 1]] } },
        { risk_table: true },
      );
      if (yAxis) Object.assign(option.yAxis as object, yAxis);
      const rows = seriesOf(layoutSurvivalRisk(option)).filter((s) => s.renderItem);
      return (rows[0]?.itemPayload as { captionOffset: number } | undefined)?.captionOffset;
    };
    // The builder's centered name, its 12-pixel text 45 pixels left of the
    // plot: its center 51 pixels left.
    expect(captionOffset(null)).toBe(51);
    expect(captionOffset({ nameGap: 60 })).toBe(66);
    expect(captionOffset({ nameGap: 80, nameTextStyle: { fontSize: 16 } })).toBe(88);
    // A smaller gap: ECharts moves the name clear of the tick labels.
    expect(captionOffset({ nameGap: 20 })).toBe(51);
    // A name at an end of the axis leaves the caption in its place.
    expect(captionOffset({ nameGap: 60, nameLocation: "end" })).toBe(51);
  });
});
