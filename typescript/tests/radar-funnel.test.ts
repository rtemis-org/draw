import { describe, expect, it } from "vitest";
import { buildFunnelOption, funnelStages } from "../src/charts/funnel/config.ts";
import { buildRadarOption, RADAR_ALL_ROWS, radarSeries } from "../src/charts/radar/config.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = Record<string, unknown> & { data: Record<string, unknown>[] };
const seriesOf = (option: Record<string, unknown>) => (option.series as Series[])[0] as Series;

const teams: TabularData = {
  columns: {
    team: ["b", "a", "b"],
    speed: [4, 2, 6],
    power: [1, 3, Number.NaN],
  },
};

describe("radarSeries", () => {
  it("draws one series per row of each group", () => {
    const data = {
      columns: { team: ["b", "a"], speed: [4, 2], power: [1, 3] },
      levels: { team: ["a", "b"] },
    };
    expect(radarSeries(data, { indicators: ["speed", "power"], group: "team" })).toEqual([
      { name: "a", values: [2, 3] },
      { name: "b", values: [4, 1] },
    ]);
  });

  it("combines the rows of a series by the aggregate, leaving out missing values", () => {
    const config = { indicators: ["speed", "power"], group: "team" } as const;
    expect(radarSeries(teams, { ...config, aggregate: "mean" })).toEqual([
      { name: "b", values: [5, 1] },
      { name: "a", values: [2, 3] },
    ]);
    expect(radarSeries(teams, { ...config, aggregate: "sum" })[0]?.values).toEqual([10, 1]);
    expect(radarSeries(teams, { indicators: ["speed", "power"], aggregate: "median" })).toEqual([
      { name: RADAR_ALL_ROWS, values: [4, 2] },
    ]);
  });

  it("stops on ambiguous rows, missing values, and too few indicators", () => {
    expect(() => radarSeries(teams, { indicators: ["speed", "power"], group: "team" })).toThrow(
      "Rows 1 and 3 both belong to series 'b', which is one polygon. Set the config member 'aggregate' to sum, mean, or median to combine them.",
    );
    expect(() => radarSeries(teams, { indicators: ["speed", "power"] })).toThrow(
      "Set the config member 'aggregate' to sum, mean, or median to combine them, or set 'group'.",
    );
    const one = { columns: { speed: [1], power: [null] } };
    expect(() => radarSeries(one, { indicators: ["speed", "power"] })).toThrow(
      "Column 'power' (config member 'indicators') must hold a finite number; row 1 holds 'null'.",
    );
    expect(() => radarSeries(one, { indicators: ["speed", "power"], aggregate: "mean" })).toThrow(
      "Series 'All rows' has no finite value in column 'power' (config member 'indicators').",
    );
    expect(() => radarSeries(one, { indicators: "speed" })).toThrow(
      "A radar chart needs at least two columns in the config member 'indicators'.",
    );
  });
});

describe("buildRadarOption", () => {
  const data = { columns: { team: ["a", "b"], speed: [2, 4], power: [-1, 3] } };
  const config = { indicators: ["speed", "power"], group: "team" } as const;

  it("scales each axis to its values and lists the series", () => {
    const option = buildRadarOption(data, config);
    expect((option.radar as { indicator: unknown }).indicator).toEqual([
      { name: "speed", max: 4, min: 0 },
      { name: "power", max: 3, min: -1 },
    ]);
    expect(option.legend).toEqual({ data: ["a", "b"] });
    expect(option.radar).toMatchObject({ center: ["50%", "56%"], radius: "62%" });
    const series = seriesOf(option);
    expect(series.data[0]).toEqual({
      name: "a",
      value: [2, -1],
      areaStyle: { opacity: 0.12 },
      lineStyle: { width: 2 },
    });
  });

  it("colors the series from the palette and maps the look", () => {
    const option = buildRadarOption(
      data,
      { ...config, area_opacity: 0.3, line_width: 1, title: "R" },
      { palette: ["#111111", "#222222"] },
    );
    expect(seriesOf(option).data[1]).toMatchObject({
      areaStyle: { opacity: 0.3 },
      lineStyle: { color: "#222222", width: 1 },
      itemStyle: { color: "#222222" },
    });
    expect(option.title).toEqual({ text: "R", left: "center" });
  });

  it("leaves out the legend of one series", () => {
    const option = buildRadarOption({ columns: { s: [1], p: [2] } }, { indicators: ["s", "p"] });
    expect(option.legend).toBeUndefined();
    expect(option.radar).toMatchObject({ radius: "68%" });
  });
});

describe("funnel", () => {
  const steps: TabularData = {
    columns: { step: ["visit", "cart", "visit", "buy"], n: [60, 30, 40, 10] },
  };
  const bound = { values: "n", labels: "step" } as const;

  it("sums the rows of each stage", () => {
    expect(funnelStages(steps, bound)).toEqual([
      { name: "visit", value: 100 },
      { name: "cart", value: 30 },
      { name: "buy", value: 10 },
    ]);
    expect(() => funnelStages(steps, { values: "n" })).toThrow(
      "A funnel needs the config members 'values' and 'labels' set.",
    );
  });

  it("draws sorted stages with inside labels and a legend", () => {
    const option = buildFunnelOption(steps, bound);
    const series = seriesOf(option);
    expect(series).toMatchObject({
      type: "funnel",
      sort: "descending",
      label: { show: true, position: "inside", formatter: "{b}" },
    });
    expect(option.legend).toEqual({ data: ["visit", "cart", "buy"] });
  });

  it("maps sort, labels, palette, and title", () => {
    const option = buildFunnelOption(
      steps,
      { ...bound, sort: "none", label_format: "name_value", show_labels: false, title: "F" },
      { palette: ["#333333"] },
    );
    const series = seriesOf(option);
    expect(series.sort).toBe("none");
    expect((series.label as { show: boolean }).show).toBe(false);
    const formatter = (series.label as { formatter: (p: unknown) => string }).formatter;
    expect(formatter({ name: "visit", value: 1234.5678 })).toBe("visit: 1235");
    expect(series.data.map((d) => (d.itemStyle as { color: string }).color)).toEqual([
      "#333333",
      "#333333",
      "#333333",
    ]);
    expect(series.top).toBe(70);
  });
});
