import { describe, expect, it } from "vitest";
import { buildDensityOption, type DensityData } from "../src/charts/density/config.ts";
import { tooltipValue } from "../src/shared/format.ts";

type Series = {
  type: string;
  name: string;
  data: number[][];
  areaStyle?: { opacity: number };
  showSymbol?: boolean;
  xAxisIndex?: number;
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

const two: DensityData = {
  samples: [
    { name: "a", n: 10, x: [0, 1, 2], y: [0.1, 0.5, 0.1] },
    { name: "b", n: 5, x: [1, 2, 3], y: [0.2, 0.3, 0.2] },
  ],
};

describe("buildDensityOption", () => {
  it("draws each sample's curve with a filled area on a numeric axis", () => {
    const option = buildDensityOption({ samples: [two.samples[0] as never] }, {});
    const [curve] = seriesOf(option);
    expect(curve).toMatchObject({ type: "line", name: "a", showSymbol: false });
    expect(curve?.areaStyle).toEqual({ opacity: 0.25 });
    expect(curve?.data).toEqual([
      [0, 0.1],
      [1, 0.5],
      [2, 0.1],
    ]);
    expect(option.xAxis).toMatchObject({ type: "value", scale: true });
    expect(option.yAxis).toMatchObject({ type: "value", min: 0 });
    expect(option.tooltip).toEqual({ trigger: "axis", valueFormatter: tooltipValue });
    expect(option.legend).toBeUndefined();
  });

  it("overlays several samples with a legend, and takes the fill opacity and names", () => {
    const option = buildDensityOption(two, { fill_alpha: 0.6, xlab: "len", ylab: "Density" });
    expect(option.legend).toEqual({ data: ["a", "b"] });
    expect(seriesOf(option).map((s) => s.areaStyle?.opacity)).toEqual([0.6, 0.6]);
    expect(option.xAxis).toMatchObject({ name: "len", nameLocation: "middle" });
    expect(option.yAxis).toMatchObject({ name: "Density" });
  });

  it("gives each sample a row in ridge mode, on the curves' shared x range", () => {
    const option = buildDensityOption(two, { mode: "ridge", zoom: true });
    expect(option.legend).toEqual({ show: false });
    const xAxes = option.xAxis as Record<string, unknown>[];
    expect(xAxes[0]).toMatchObject({ min: 0, max: 3, show: false });
    const yAxes = option.yAxis as Record<string, unknown>[];
    expect(yAxes.map((a) => a.name)).toEqual(["a", "b"]);
    expect(yAxes[1]).toMatchObject({ min: 0, max: 0.5 * 1.05 });
    expect(seriesOf(option).map((s) => s.xAxisIndex)).toEqual([0, 1]);
    expect(option.dataZoom).toEqual([
      expect.objectContaining({ type: "inside", xAxisIndex: [0, 1], filterMode: "none" }),
    ]);
  });

  it("zooms x without filtering, and stops on a curve of mismatched lengths", () => {
    const zoom = buildDensityOption(two, { zoom: true }).dataZoom as Record<string, unknown>[];
    expect(zoom.map((z) => z.filterMode)).toEqual(["none", "none"]);
    expect(() => buildDensityOption({ samples: [{ name: "a", n: 1, x: [0], y: [] }] }, {})).toThrow(
      "one density per evaluation point",
    );
    expect(() => buildDensityOption({ samples: [] }, {})).toThrow("`x` set");
  });
});
