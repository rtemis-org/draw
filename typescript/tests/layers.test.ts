import { readdirSync, readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

// A builder writes no path the theme owns, since any value in the option
// defeats the registered theme beneath it. spec: draw/chart-appearance#rule
const THEME_OWNED = [
  /(^|\.)(textStyle|nameTextStyle|subtextStyle|axisTick|nameLocation|itemGap|backgroundColor)(\.|$)/,
  /^color(\.|$)/,
  /^(xAxis|yAxis)\.(axisLine|splitLine)\.lineStyle(\.|$)/,
  /^(xAxis|yAxis)\.splitLine\.show$/,
  /^(xAxis|yAxis)\.axisLabel\.(color|fontSize|fontFamily|fontWeight|margin)$/,
];

// Paths a chart type still writes until the theme carries them: the axis name
// location moves to the theme with rtemis.draw's token `Theme`, and A3 takes
// its fonts and colors from the tokens: milestones T1 and T2 of the
// chart-appearance plan.
const PENDING: Record<string, RegExp | "all"> = {
  a3: "all",
  bar: /(^|\.)nameLocation$/,
  line: /(^|\.)nameLocation$/,
};

function leafPaths(value: unknown, prefix = ""): string[] {
  if (Array.isArray(value)) return value.flatMap((v) => leafPaths(v, prefix));
  if (value !== null && typeof value === "object") {
    return Object.entries(value).flatMap(([k, v]) => leafPaths(v, prefix ? `${prefix}.${k}` : k));
  }
  return [prefix];
}

describe("disjoint layers", () => {
  const dir = new URL("./corpus/", import.meta.url);
  const cases = readdirSync(dir).filter((f) => f.endsWith(".case.json"));
  for (const file of cases) {
    const { type } = JSON.parse(readFileSync(new URL(file, dir), "utf8")) as { type: string };
    const pending = PENDING[type];
    it.skipIf(pending === "all")(`${file} writes no theme-owned path`, () => {
      const option = JSON.parse(
        readFileSync(new URL(file.replace(".case.json", ".option.json"), dir), "utf8"),
      );
      const owned = [...new Set(leafPaths(option))].filter(
        (path) =>
          THEME_OWNED.some((re) => re.test(path)) &&
          !(pending instanceof RegExp && pending.test(path)),
      );
      expect(owned).toEqual([]);
    });
  }
});
