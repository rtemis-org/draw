import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  buildScatter3DOption,
  type Scatter3DChartConfig,
  type Surface3D,
} from "../src/charts/scatter3d/config.ts";
import { appendLayers } from "../src/shared/layers.ts";
import type { TabularData } from "../src/shared/source.ts";

// rtemis.draw's scatter3d_option() output for each case, written before the
// builder replaced it, with the surfaces its glm fits computed.
const parity = JSON.parse(
  readFileSync(join(__dirname, "fixtures/scatter3d/r-parity.json"), "utf8"),
) as {
  data: TabularData;
  cases: {
    name: string;
    config: Scatter3DChartConfig;
    surfaces: Surface3D[] | null;
    option: Record<string, unknown>;
  }[];
};

type Series = Record<string, unknown> & { data: unknown[][] };

// jsonlite writes 15 significant digits; numbers compare to 12.
const rounded = (v: unknown): unknown =>
  typeof v === "number"
    ? Number(v.toPrecision(12))
    : Array.isArray(v)
      ? v.map(rounded)
      : v && typeof v === "object"
        ? Object.fromEntries(Object.entries(v).map(([k, x]) => [k, rounded(x)]))
        : v;
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

describe("buildScatter3DOption", () => {
  for (const c of parity.cases) {
    it(`reproduces rtemis.draw's option: ${c.name}`, () => {
      const option = buildScatter3DOption(
        { ...parity.data, ...(c.surfaces ? { surfaces: c.surfaces } : {}) },
        c.config,
      );
      expect(rounded(option)).toEqual(rounded(c.option));
    });
  }

  const xyz = { x: "x", y: "y", z: "z" } as const;

  it("breaks paths at incomplete rows and draws runs of two or more", () => {
    const data = {
      columns: { x: [1, 2, null, 4, 5, 6], y: [1, 2, 3, 4, 5, 6], z: [1, 2, 3, 4, null, 6] },
    };
    const series = seriesOf(buildScatter3DOption(data, { ...xyz, mode: "lines" }));
    expect(series.map((s) => s.data.length)).toEqual([2]);
    expect(series[0]?.lineStyle).toEqual({ width: 2, opacity: 1 });
  });

  it("reads typed arrays and leaves out rows missing the group", () => {
    const data = {
      columns: {
        x: Float64Array.from([1, 2, Number.NaN, 4]),
        y: [1, 2, 3, 4],
        z: [1, 2, 3, 4],
        g: ["a", null, "a", "b"],
      },
    };
    const option = buildScatter3DOption(data, { ...xyz, group: "g" });
    expect(seriesOf(option).map((s) => [s.name, s.data.length])).toEqual([
      ["a", 1],
      ["b", 1],
    ]);
    expect(option.legend).toEqual({ data: ["a", "b"] });
  });

  it("widens a single value and takes the host palette", () => {
    const option = buildScatter3DOption({ columns: { x: [2], y: [0], z: [100] } }, xyz, {
      palette: ["#123456"],
    });
    expect(option.xAxis3D).toMatchObject({ min: 1, max: 3 });
    expect(option.zAxis3D).toMatchObject({ min: 95, max: 105 });
    expect(option.color).toEqual(["#123456"]);
    expect(option.legend).toEqual({ show: false });
  });

  it("stops on text or infinity, no complete row, and unbound axes", () => {
    expect(() =>
      buildScatter3DOption({ columns: { x: [1, "a"], y: [1, 2], z: [1, 2] } }, xyz),
    ).toThrow(
      "Column 'x' (config member 'x') must hold finite numbers or missing values; row 2 holds 'a'.",
    );
    expect(() =>
      buildScatter3DOption(
        { columns: { x: [1, Number.POSITIVE_INFINITY], y: [1, 2], z: [1, 2] } },
        xyz,
      ),
    ).toThrow("row 2 holds 'Infinity'");
    expect(() => buildScatter3DOption({ columns: { x: [null], y: [1], z: [1] } }, xyz)).toThrow(
      "at least one complete observation",
    );
    expect(() =>
      buildScatter3DOption({ columns: { x: [1, 2], y: [1, 2], z: [1, 2] } }, { x: "x", y: "y" }),
    ).toThrow("'x', 'y', and 'z' set");
    expect(() =>
      buildScatter3DOption({ columns: { x: [-1e308, 1e308], y: [1, 2], z: [1, 2] } }, xyz),
    ).toThrow("finite nonzero range");
    expect(() =>
      buildScatter3DOption(
        { columns: { x: [1, null, 3], y: [1, 2, 3], z: [1, 2, 3] } },
        { ...xyz, mode: "lines" },
      ),
    ).toThrow("two consecutive complete observations");
  });
});

describe("surfaces", () => {
  it("lists a surface not named after a group in the legend", () => {
    const surface = {
      name: "Fit",
      x: [0, 1],
      y: [0, 1],
      z: [
        [0, 1],
        [1, 2],
      ],
    };
    const option = buildScatter3DOption(
      { columns: { x: [0, 1], y: [0, 1], z: [0, 1], g: ["a", "b"] }, surfaces: [surface] },
      { x: "x", y: "y", z: "z", group: "g", fit_alpha: 0.2 },
    );
    expect(option.legend).toEqual({ data: ["a", "b", "Fit"] });
    const series = seriesOf(option).at(-1) as Series;
    expect(series).toMatchObject({
      type: "surface",
      dataShape: [2, 2],
      itemStyle: { opacity: 0.2 },
    });
    expect(series.data).toEqual([
      [0, 0, 0],
      [1, 0, 1],
      [0, 1, 1],
      [1, 1, 2],
    ]);
    expect(option.zAxis3D).toMatchObject({ min: 0, max: 2 });
  });
});

describe("surface layers", () => {
  const option = buildScatter3DOption(
    { columns: { x: [0, 1], y: [0, 1], z: [0, 1] } },
    { x: "x", y: "y", z: "z" },
  );
  const surface = {
    type: "surface",
    name: "Predicted",
    data: [
      [0, 0, 0],
      [2, 0, 1],
      [0, 1, 1],
      [2, 1, null],
    ],
  };

  it("widens the 3D axes to a surface layer", () => {
    const out = appendLayers(option, [{ series: [surface], legend: "Predicted" }]);
    expect(out.xAxis3D).toMatchObject({ min: 0, max: 2, interval: 0.5 });
    expect(out.zAxis3D).toMatchObject({ min: 0, max: 1 });
    expect(seriesOf(out)).toHaveLength(2);
    expect(out.legend).toEqual({ show: true });
  });

  it("stops on a surface outside the axes when expand is false", () => {
    expect(() => appendLayers(option, [{ series: [surface], expand: false }])).toThrow(
      "x coordinates span beyond [0, 1]",
    );
  });
});
