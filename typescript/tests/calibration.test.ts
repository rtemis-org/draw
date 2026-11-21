import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  buildCalibrationOption,
  type CalibrationChartConfig,
  type CalibrationData,
} from "../src/charts/calibration/config.ts";
import { curveAspect } from "../src/charts/curves/config.ts";
import { SIGN_COLORS } from "../src/theme/tokens.ts";

// rtemis.draw's calibration_option() output for each case, written before the
// builder replaced it, with the bins calibration_data() computed.
const parity = JSON.parse(
  readFileSync(join(__dirname, "fixtures/calibration/r-parity.json"), "utf8"),
) as {
  cases: {
    name: string;
    config: CalibrationChartConfig;
    data: CalibrationData;
    option: Record<string, unknown>;
  }[];
};

// jsonlite writes 15 significant digits; numbers compare to 12.
const rounded = (v: unknown): unknown =>
  typeof v === "number"
    ? Number(v.toPrecision(12))
    : Array.isArray(v)
      ? v.map(rounded)
      : v && typeof v === "object"
        ? Object.fromEntries(Object.entries(v).map(([k, x]) => [k, rounded(x)]))
        : v;

describe("buildCalibrationOption", () => {
  for (const c of parity.cases) {
    it(`reproduces rtemis.draw's option: ${c.name}`, () => {
      // The fixture's title carries textAlign "center", which shifts a
      // centered title left by half its width; the builder centers it with
      // `left` alone, and writes a title only with text.
      // The axis name location, the legend's text, and the diagonal's default
      // color are the theme's.
      const { title, ...want } = c.option as { title: { text?: string } };
      const { textStyle: _text, ...legend } = (want as Record<string, Record<string, unknown>>)
        .legend as Record<string, unknown>;
      (want as Record<string, unknown>).legend = legend;
      if (!c.config.diagonal_color) {
        for (const s of (
          want as { series: { lineStyle?: { color?: string }; itemStyle?: { color?: string } }[] }
        ).series) {
          if (s.lineStyle?.color === "#888888") s.lineStyle.color = SIGN_COLORS.neutral;
          if (s.itemStyle?.color === "#888888") s.itemStyle.color = SIGN_COLORS.neutral;
        }
      }
      for (const axis of ["xAxis", "yAxis"]) {
        const { nameLocation: _location, ...rest } = (
          want as Record<string, Record<string, unknown>>
        )[axis] as Record<string, unknown>;
        (want as Record<string, unknown>)[axis] = rest;
      }
      const expected = title.text ? { ...want, title: { text: title.text, left: "center" } } : want;
      expect(rounded(buildCalibrationOption(c.data, c.config))).toEqual(rounded(expected));
    });
  }

  it("keeps the plotting area square through the curve aspect hint", () => {
    const c = parity.cases[0];
    if (!c) throw new Error("no case");
    const option = buildCalibrationOption(c.data, c.config);
    expect(curveAspect(option, c.config)).toEqual({
      ratio: 1,
      leftPx: 70,
      rightPx: 24,
      topPx: 25,
      botPx: 65,
    });
    expect(curveAspect(option, { square: false })).toBeNull();
  });
});
