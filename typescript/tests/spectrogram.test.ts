import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  buildSpectrogramOption,
  type SpectrogramChartConfig,
  type SpectrogramData,
  spectrogramColors,
} from "../src/charts/spectrogram/config.ts";
import { COLORMAPS } from "../src/shared/colormaps.ts";

// rtemis.draw's spectrogram_option() output for each case, written before the
// builder replaced it, with the values it drew (after the dB conversion).
const parity = JSON.parse(
  readFileSync(join(__dirname, "fixtures/spectrogram/r-parity.json"), "utf8"),
) as {
  cases: {
    name: string;
    config: SpectrogramChartConfig;
    data: SpectrogramData;
    option: Record<string, unknown>;
  }[];
};

// jsonlite writes 15 significant digits, and R wrote margins as text.
const rounded = (v: unknown): unknown =>
  typeof v === "number"
    ? Number(v.toPrecision(12))
    : Array.isArray(v)
      ? v.map(rounded)
      : v && typeof v === "object"
        ? Object.fromEntries(Object.entries(v).map(([k, x]) => [k, rounded(x)]))
        : v;
const channels = (hex: string) => [1, 3, 5].map((i) => Number.parseInt(hex.slice(i, i + 2), 16));

function comparable(option: Record<string, unknown>) {
  const { tooltip, grid, visualMap, ...rest } = option as Record<string, Record<string, unknown>>;
  const { inRange, ...map } = visualMap as Record<string, unknown>;
  return {
    rest: rounded(rest),
    tooltip: { trigger: (tooltip as Record<string, unknown>).trigger },
    grid: Object.fromEntries(Object.entries(grid ?? {}).map(([k, v]) => [k, Number(v)])),
    map: rounded(map),
    colors: (inRange as { color: string[] }).color.map((c) => c.toUpperCase()),
  };
}

describe("buildSpectrogramOption", () => {
  for (const c of parity.cases) {
    it(`reproduces rtemis.draw's option: ${c.name}`, () => {
      const got = comparable(buildSpectrogramOption(c.data, c.config));
      const want = comparable(c.option);
      expect(got.rest).toEqual(want.rest);
      expect(got.tooltip).toEqual(want.tooltip);
      expect(got.grid).toEqual(want.grid);
      // Values other than dB take one decimal more than a nice interval of
      // the color scale, where R wrote three.
      // The labels sit right of the bar, where R left ECharts to place them.
      const { precision: gotPrecision, align, ...gotMap } = got.map as Record<string, unknown>;
      expect(align).toBe("left");
      const { precision: wantPrecision, ...wantMap } = want.map as Record<string, unknown>;
      expect(gotMap).toEqual(wantMap);
      if ((c.config.db ?? true) === true) expect(gotPrecision).toBe(wantPrecision);
      // Colors agree to one unit per channel: R interpolates viridisLite's
      // maps in Lab with a spline, the builder its 256 colors in RGB.
      expect(got.colors).toHaveLength(want.colors.length);
      got.colors.forEach((color, i) => {
        const a = channels(color);
        const b = channels(want.colors[i] as string);
        a.forEach((x, k) => {
          expect(Math.abs(x - (b[k] as number))).toBeLessThanOrEqual(1);
        });
      });
    });
  }

  const data: SpectrogramData = {
    time: [0, 1],
    frequency: [0, 10],
    values: [
      [1, 2],
      [3, 4],
    ],
  };

  it("names each tooltip line after its axis", () => {
    const option = buildSpectrogramOption(data, { db: false, colorbar_title: "Energy" });
    const formatter = (option.tooltip as { formatter: (p: unknown) => string }).formatter;
    expect(formatter({ value: [1, 0, 2] })).toBe(
      "Time (s): 1.000<br/>Frequency (Hz): 0.0<br/>Energy: 2.0",
    );
    expect(formatter({ value: [1, 0, null] })).toBe("N/A");
  });

  it("rounds values other than dB to a decimal past the scale's nice interval", () => {
    const option = buildSpectrogramOption(
      {
        time: [0, 1],
        frequency: [1, 2],
        values: [
          [-36.2, 44.1],
          [0, 1],
        ],
      },
      { db: false },
    );
    expect((option.visualMap as { precision: number }).precision).toBe(1);
  });

  it("stops on mismatched values, empty ranges, and an unknown colormap", () => {
    expect(() => buildSpectrogramOption({ ...data, values: [[1, 2]] }, {})).toThrow(
      "one row of values",
    );
    expect(() => buildSpectrogramOption(data, { freq_range: [100, 200] })).toThrow(
      "No frequency bins",
    );
    expect(() => buildSpectrogramOption(data, { time_range: [5, 6] })).toThrow("No time frames");
    expect(() => buildSpectrogramOption(data, { colormap: "jet" })).toThrow("Colormap 'jet'");
    expect(() =>
      buildSpectrogramOption(
        {
          ...data,
          values: [
            [null, null],
            [null, null],
          ],
        },
        {},
      ),
    ).toThrow("finite value");
  });
});

describe("spectrogramColors", () => {
  it("takes a named scheme's 256 colors as they are, reversed on request", () => {
    expect(spectrogramColors("magma", 256, false, [0, 1])).toEqual(COLORMAPS.magma);
    expect(spectrogramColors("magma", 256, true, [0, 1])[0]).toBe(COLORMAPS.magma[255]);
  });

  it("centers the diverging scale's midpoint on the background", () => {
    const colors = spectrogramColors("diverging", 5, false, [-1, 3], { background: "#FFFFFF" });
    expect(colors).toHaveLength(5);
    expect(colors[2]).toBe("#FFFFFF");
  });
});
