import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  darkThemeTokens,
  fontStack,
  lightThemeTokens,
  type ThemeTokens,
  themeFontSizes,
  themeOption,
} from "../src/theme/tokens.ts";

// rtemis.draw's registered theme objects: to_list(theme_light()) and friends,
// written by R into tests/fixtures/theme/.
const fixture = (name: string) =>
  JSON.parse(
    readFileSync(new URL(`./fixtures/theme/${name}.echarts.json`, import.meta.url), "utf8"),
  ) as Record<string, unknown>;

function asR(tokens: ThemeTokens) {
  const { logAxis, timeAxis, ...rest } = themeOption(tokens);
  return { rest, logAxis, timeAxis };
}

describe("themeOption", () => {
  const cases: [string, ThemeTokens][] = [
    ["light", lightThemeTokens()],
    ["dark", darkThemeTokens()],
    [
      "custom",
      lightThemeTokens({
        base_font_size: 14,
        title_font_size: 20,
        font_family: "Source Sans 3",
        palette: ["#111111", "#222222"],
        grid_color: "#eeeeee",
        axis_color: "#999999",
        tooltip_bg: "#fafafa",
      }),
    ],
    [
      "custom-dark",
      darkThemeTokens({
        base_font_size: 16,
        font_family: "Geist, sans-serif",
        legend_color: "#cccccc",
      }),
    ],
  ];
  for (const [name, tokens] of cases) {
    it(`projects ${name} tokens to rtemis.draw's registered theme`, () => {
      const { rest, logAxis, timeAxis } = asR(tokens);
      expect(rest).toEqual(fixture(name));
      expect(logAxis).toEqual(rest.valueAxis);
      expect(timeAxis).toEqual(rest.valueAxis);
    });
  }
});

describe("fontStack", () => {
  it("quotes names that are not identifiers and appends the system stack", () => {
    expect(fontStack("Source Sans 3")).toBe(
      "'Source Sans 3', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif",
    );
    expect(fontStack("Geist, sans-serif")).toBe("Geist, sans-serif");
    expect(fontStack(null)).toContain("system-ui");
  });
});

describe("themeFontSizes", () => {
  it("rounds the derived title size half to even, as R does", () => {
    expect(themeFontSizes(lightThemeTokens({ base_font_size: 12 })).title).toBe(14);
    expect(themeFontSizes(lightThemeTokens({ base_font_size: 12.5 })).title).toBe(15);
    expect(themeFontSizes(lightThemeTokens({ base_font_size: 10 })).title).toBe(12);
  });
});
