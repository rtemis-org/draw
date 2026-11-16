import { describe, expect, it } from "vitest";
import { buildLineOption, lineAspect } from "../src/charts/line/config.ts";
import { formatNumber } from "../src/shared/format.ts";
import { calcLimits, calcLimitsFromZero } from "../src/shared/limits.ts";
import { timeLabel, timeMs } from "../src/shared/source.ts";

type Series = {
  name?: string;
  data: unknown[];
  showSymbol?: boolean;
  stack?: string;
  areaStyle?: object;
  itemStyle?: { color: string };
  lineStyle?: { type: string };
  markArea?: { data: [{ xAxis: unknown; name: string }, { xAxis: unknown }][] };
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];
const axis = (option: Record<string, unknown>, key: "xAxis" | "yAxis") =>
  option[key] as Record<string, unknown>;

describe("buildLineOption", () => {
  it("pairs a numeric x with each value and pads both axes", () => {
    const option = buildLineOption({ columns: { x: [1, 2, 3], y: [2, 4, 3] } }, { x: "x", y: "y" });
    expect(seriesOf(option)).toEqual([
      {
        type: "line",
        showSymbol: false,
        data: [
          [1, 2],
          [2, 4],
          [3, 3],
        ],
      },
    ]);
    expect(axis(option, "xAxis")).toMatchObject({
      type: "value",
      scale: true,
      min: 0.92,
      max: 3.08,
    });
    expect(axis(option, "yAxis")).toMatchObject({ min: 1.92, max: 4.08 });
    expect(axis(option, "xAxis").axisLine).toEqual({ show: false, onZero: true });
    expect(option).not.toHaveProperty("legend");
    expect(option).not.toHaveProperty("useUTC");
  });

  it("lists categories once and gives an ungrouped line its values alone", () => {
    const option = buildLineOption(
      { columns: { q: ["Q1", "Q2", "Q3"], v: [1, 2, 3] } },
      { x: "q", y: "v" },
    );
    expect(axis(option, "xAxis")).toMatchObject({ type: "category", data: ["Q1", "Q2", "Q3"] });
    expect(axis(option, "xAxis")).not.toHaveProperty("min");
    expect(seriesOf(option)[0]?.data).toEqual([1, 2, 3]);
  });

  it("pairs grouped categories with their axis index, groups in level order", () => {
    const option = buildLineOption(
      {
        columns: {
          q: ["Q1", "Q2", "Q1", "Q2", "Q1"],
          v: [1, 2, 3, 4, 5],
          g: ["a", "a", "b", "b", null],
        },
        levels: { g: ["b", "a"] },
      },
      { x: "q", y: "v", group: "g" },
    );
    expect(seriesOf(option).map((s) => [s.name, s.data])).toEqual([
      [
        "b",
        [
          [0, 3],
          [1, 4],
        ],
      ],
      [
        "a",
        [
          [0, 1],
          [1, 2],
        ],
      ],
    ]);
    expect(option.legend).toEqual({ data: ["b", "a"] });
  });

  it("names grouped lines by value column and group when there are several of each", () => {
    const option = buildLineOption(
      { columns: { x: [1, 2, 1, 2], u: [1, 2, 3, 4], w: [5, 6, 7, 8], g: ["a", "a", "b", "b"] } },
      { x: "x", y: ["u", "w"], group: "g" },
    );
    expect(seriesOf(option).map((s) => s.name)).toEqual(["u / a", "w / a", "u / b", "w / b"]);
  });

  it("places a time column on a UTC time axis by its wall-clock reading", () => {
    const option = buildLineOption(
      {
        columns: { t: ["2024-01-01T10:00:00-0800", "2024-01-01T11:00:00-0800"], v: [1, 2] },
        types: { t: "time" },
      },
      { x: "t", y: "v" },
    );
    const ten = Date.UTC(2024, 0, 1, 10);
    expect(option.useUTC).toBe(true);
    expect(axis(option, "xAxis")).toMatchObject({
      type: "time",
      axisLabel: { showMinLabel: false, showMaxLabel: false },
    });
    expect(seriesOf(option)[0]?.data).toEqual([
      [ten, 1],
      [ten + 3_600_000, 2],
    ]);
  });

  it("starts a filled area's y axis at zero, padding only away from zero", () => {
    const build = (u: number[], config = {}) =>
      axis(
        buildLineOption(
          { columns: { x: [1, 2, 3], u } },
          { x: "x", y: "u", area: true, ...config },
        ),
        "yAxis",
      );
    expect(build([20, 30, 40])).toMatchObject({ min: 0, max: 41.6 });
    expect(build([-40, -30, -20])).toMatchObject({ min: -41.6, max: 0 });
    expect(build([-20, 0, 20])).toMatchObject({ min: -21.6, max: 21.6 });
    expect(build([20, 30, 40], { ylim: [10, 50] })).toMatchObject({ min: 10, max: 50 });
    expect(
      axis(
        buildLineOption({ columns: { x: [1, 2, 3], u: [20, 30, 40] } }, { x: "x", y: "u" }),
        "yAxis",
      ),
    ).toMatchObject({ min: 19.2 });
    expect(
      axis(
        buildLineOption(
          { columns: { x: [1, 2, 3], u: [20, 30, 40] } },
          { x: "x", y: "u", area: true },
        ),
        "xAxis",
      ).axisLine,
    ).toEqual({ show: true, onZero: true });
  });

  it("spans a stacked y axis over the sums, positive and negative apart", () => {
    const option = buildLineOption(
      { columns: { x: [1, 2], u: [1, 2], w: [3, -1] } },
      { x: "x", y: ["u", "w"], stack: true, area: true },
    );
    expect(seriesOf(option).every((s) => s.stack === "total" && s.areaStyle)).toBe(true);
    const [min, max] = calcLimits([1, 2, 4, -1]) as [number, number];
    expect(axis(option, "yAxis")).toMatchObject({ min, max });
  });

  it("styles lines from palette, line_style, and points", () => {
    const option = buildLineOption(
      { columns: { x: [1, 2], u: [1, 2], w: [3, 4], z: [5, 6] } },
      {
        x: "x",
        y: ["u", "w", "z"],
        palette: ["#111", "#222"],
        line_style: ["dashed"],
        points: false,
      },
    );
    const series = seriesOf(option);
    expect(series.map((s) => s.itemStyle?.color)).toEqual(["#111", "#222", "#111"]);
    expect(series.every((s) => s.lineStyle?.type === "dashed" && s.showSymbol === false)).toBe(
      true,
    );
  });

  it("shades runs of blocks, extending each band to the next on a numeric x", () => {
    const option = buildLineOption(
      {
        columns: {
          x: [1, 2, 3, 4, 5, 6],
          v: [1, 2, 3, 4, 5, 6],
          b: ["a", "a", "b", null, "c", "c"],
        },
      },
      { x: "x", y: "v", blocks: "b", block_color: ["red", "blue", "transparent"] },
    );
    const bands = seriesOf(option)[0]?.markArea?.data ?? [];
    expect(bands.map(([start, end]) => [start.name, start.xAxis, end.xAxis])).toEqual([
      ["a", 1, 3],
      ["b", 3, 3],
    ]);
  });

  it("keeps a band to its own categories on a category axis", () => {
    const option = buildLineOption(
      { columns: { x: ["p", "q", "r"], v: [1, 2, 3], b: [1, 1, 2] } },
      { x: "x", y: "v", blocks: "b", block_color: ["red", "blue"] },
    );
    const bands = seriesOf(option)[0]?.markArea?.data ?? [];
    expect(bands.map(([start, end]) => [start.xAxis, end.xAxis])).toEqual([
      ["p", "q"],
      ["r", "r"],
    ]);
  });

  it("rejects what rtemis.draw rejects", () => {
    const columns = { x: [1, 2], t: ["a", "b"], v: [1, 2], g: ["a", "b"] };
    expect(() => buildLineOption({ columns }, { x: "x" })).toThrow("both `x` and `y`");
    expect(() => buildLineOption({ columns }, { x: "t", y: "v", xlim: [0, 1] })).toThrow("`xlim`");
    expect(() => buildLineOption({ columns }, { x: "t", y: "v", equal_axes: true })).toThrow(
      "categorical",
    );
    expect(() =>
      buildLineOption({ columns }, { x: "x", y: "v", group: "g", blocks: "g", block_color: ["r"] }),
    ).toThrow("`blocks`");
    expect(() => buildLineOption({ columns }, { x: "x", y: "v", blocks: "g" })).toThrow(
      "`block_color`",
    );
    expect(() =>
      buildLineOption({ columns }, { x: "x", y: "v", blocks: "g", block_color: ["r"] }),
    ).toThrow("length 2");
    expect(() =>
      buildLineOption(
        { columns },
        { x: "x", y: "v", square: true, equal_axes: true, xlim: [0, 1], ylim: [0, 2] },
      ),
    ).toThrow("same interval");
  });
});

describe("lineAspect", () => {
  const columns = { x: [0, 10], y: [0, 1] };
  const margins = { margin_top: 10, margin_right: 20, margin_bottom: 30, margin_left: 40 };

  it("gives no hint without square or equal_axes", () => {
    const option = buildLineOption({ columns }, { x: "x", y: "y" });
    expect(lineAspect(option, {})).toBeNull();
  });

  it("sets the ratio from the two spans, or 1 for a square box", () => {
    const equal = { x: "x", y: "y", equal_axes: true, ...margins };
    expect(lineAspect(buildLineOption({ columns }, equal), equal)).toEqual({
      ratio: 0.1,
      leftPx: 40,
      rightPx: 20,
      topPx: 10,
      botPx: 30,
    });
    const both = { ...equal, square: true };
    const option = buildLineOption({ columns }, both);
    expect(axis(option, "xAxis")).toMatchObject({ min: -0.4, max: 10.4 });
    expect(lineAspect(option, both)?.ratio).toBe(1);
  });

  it("requires numeric limits on both axes", () => {
    const config = { x: "q", y: "y", square: true };
    const option = buildLineOption({ columns: { q: ["a", "b"], y: [0, 1] } }, config);
    expect(() => lineAspect(option, config)).toThrow("categorical");
  });
});

describe("time values", () => {
  it("reads ISO dates and date-times by their wall clock", () => {
    expect(timeMs("2025-09-07")).toBe(Date.UTC(2025, 8, 7));
    expect(timeMs("2025-09-07T10:30:00.250+0200")).toBe(Date.UTC(2025, 8, 7, 10, 30, 0, 250));
    expect(timeMs(5)).toBe(5);
    expect(timeMs(null)).toBeNull();
    expect(timeMs("soon")).toBeNull();
  });

  it("labels a midnight as its date and any other time with its clock", () => {
    expect(timeLabel(Date.UTC(2025, 8, 7))).toBe("2025-09-07");
    expect(timeLabel(Date.UTC(2025, 8, 7, 10, 30))).toBe("2025-09-07 10:30:00");
    expect(timeLabel(Date.UTC(2025, 8, 7, 10, 30, 0, 250))).toBe("2025-09-07 10:30:00.250");
  });
});

describe("formatNumber", () => {
  it("shows integers whole and other numbers to three significant digits", () => {
    expect(formatNumber(3)).toBe("3");
    expect(formatNumber(12345)).toBe("12345");
    expect(formatNumber(2.345)).toBe("2.35");
    expect(formatNumber(0.012345)).toBe("0.0123");
    expect(formatNumber(1234.567)).toBe("1235");
    expect(formatNumber(9.996)).toBe("10.0");
    expect(formatNumber(-0.5)).toBe("-0.500");
    expect(formatNumber(0.000123)).toBe("1.23e-4");
    expect(formatNumber(2.345, 2)).toBe("2.3");
    expect(formatNumber("a")).toBe("a");
    expect(formatNumber(Number.NaN)).toBeNaN();
  });
});

describe("one row per x", () => {
  it("rejects two rows at one x in a line, naming aggregate", () => {
    expect(() =>
      buildLineOption({ columns: { x: [1, 1, 2], u: [5, 7, 1] } }, { x: "x", y: "u" }),
    ).toThrow(/x = 1 in line 'u'.*aggregate/);
    expect(() =>
      buildLineOption(
        { columns: { x: ["a", "a"], u: [5, 7], g: ["p", "p"] } },
        { x: "x", y: "u", group: "g" },
      ),
    ).toThrow(/x = 'a' in line 'u'/);
  });

  it("allows one x per line across groups, and drops rows without an x", () => {
    const option = buildLineOption(
      { columns: { x: [1, 2, 1, null], u: [5, 7, 1, 9], g: ["p", "p", "q", "q"] } },
      { x: "x", y: "u", group: "g" },
    );
    expect(seriesOf(option).map((s) => s.data)).toEqual([
      [
        [1, 5],
        [2, 7],
      ],
      [[1, 1]],
    ]);
  });
});

describe("markers", () => {
  it("draws no markers unless asked, except on a value with no neighbor", () => {
    const plain = buildLineOption({ columns: { x: [1, 2, 3], u: [1, 2, 3] } }, { x: "x", y: "u" });
    expect(seriesOf(plain)[0]?.showSymbol).toBe(false);
    const gap = buildLineOption(
      { columns: { x: [1, 2, 3, 4, 5], u: [1, 2, null, 4, null] } },
      { x: "x", y: "u" },
    );
    const series = seriesOf(gap)[0];
    expect(series?.showSymbol).toBeUndefined();
    expect(series?.data).toEqual([
      { value: [1, 1], symbol: "none" },
      { value: [2, 2], symbol: "none" },
      { value: [3, null], symbol: "none" },
      [4, 4],
      { value: [5, null], symbol: "none" },
    ]);
    const marked = buildLineOption(
      { columns: { x: [1, 2], u: [1, 2] } },
      { x: "x", y: "u", points: true },
    );
    expect(seriesOf(marked)[0]).not.toHaveProperty("showSymbol");
  });
});

describe("stacked lines", () => {
  const columns = { x: [1, 2, 3, 1, 3], u: [5, 7, 1, 2, 3], g: ["a", "a", "a", "b", "b"] };

  it("require area", () => {
    expect(() => buildLineOption({ columns }, { x: "x", y: "u", group: "g", stack: true })).toThrow(
      "set `area` too",
    );
  });

  it("draw no point where a line below has no value, and span the known sums", () => {
    const option = buildLineOption(
      {
        columns: {
          x: [1, 2, 3, 1, 2, 3],
          u: [5, null, 1, 2, 4, 3],
          g: ["a", "a", "a", "b", "b", "b"],
        },
      },
      { x: "x", y: "u", group: "g", stack: true, area: true },
    );
    const [, upper] = seriesOf(option);
    // The middle point has no base; the outer two have no neighbor, so they
    // keep their markers.
    expect(upper?.data).toEqual([[1, 2], { value: [2, null], symbol: "none" }, [3, 3]]);
    expect(axis(option, "yAxis")).toMatchObject({ min: 0 });
    const [, max] = calcLimitsFromZero([5, 1, 7, 4]) as [number, number];
    expect(axis(option, "yAxis").max).toBe(max);
  });
});
