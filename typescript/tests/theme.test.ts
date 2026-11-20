import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { describe, expect, it } from "vitest";
import {
  darkThemeTokens,
  fontStack,
  lightThemeTokens,
  type ThemeTokens,
  themeFontSizes,
  themeObject,
  themeOption,
} from "../src/theme/tokens.ts";

// The expected ECharts theme objects of rtemis.draw's light and dark themes and
// of two custom themes, in tests/fixtures/theme/.
const fixture = (name: string) =>
  JSON.parse(
    readFileSync(new URL(`./fixtures/theme/${name}.echarts.json`, import.meta.url), "utf8"),
  ) as Record<string, unknown>;

function asR(tokens: ThemeTokens) {
  const { logAxis, timeAxis, ...rest } = themeOption(tokens);
  return { rest, logAxis, timeAxis };
}

// rtemis.draw's themes with the axis spacing left to ECharts.
const NO_SPACING = { axis_label_gap: null, axis_name_gap: null, axis_name_location: null } as const;

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

describe("themeOption's weight, gridline, and axis spacing tokens", () => {
  const option = themeOption(
    lightThemeTokens({
      base_font_size: 16,
      axis_label_font_size: 12,
      font_weight: 500,
      title_font_weight: 600,
      grid_color: "#808080",
      grid_opacity: 0.5,
      grid_style: "dashed",
      axis_label_gap: 0.5,
      axis_name_gap: 0.625,
      axis_name_location: "middle",
    }),
  ) as Record<string, Record<string, unknown>>;

  it("projects the font weights", () => {
    expect(option.textStyle).toMatchObject({ fontWeight: 500 });
    expect(option.legend).toMatchObject({ textStyle: { fontWeight: 500 } });
    expect(option.title).toMatchObject({ textStyle: { fontWeight: 600 } });
    expect(option.valueAxis).toMatchObject({ axisLabel: { fontWeight: 500 } });
  });

  it("projects gridline opacity and style", () => {
    expect(option.valueAxis).toMatchObject({
      splitLine: { lineStyle: { color: "#808080", opacity: 0.5, type: "dashed" } },
    });
  });

  it("projects the axis spacing to every axis type, in em of its font size", () => {
    for (const key of ["categoryAxis", "valueAxis", "logAxis", "timeAxis"]) {
      expect(option[key]).toMatchObject({
        nameLocation: "middle",
        nameTextStyle: { textMargin: [10, 3, 10, 3] },
        axisLabel: { margin: 6 },
      });
    }
    // The radar's indicator names and the zoom slider take the text only.
    expect(option.radar?.axisName).not.toHaveProperty("margin");
  });

  it("projects axis line visibility and the color scale text size", () => {
    const hidden = themeOption(
      darkThemeTokens({ axis_line_show: false, color_scale_font_size: 14 }),
    ) as Record<string, Record<string, unknown>>;
    for (const key of ["categoryAxis", "valueAxis", "logAxis", "timeAxis", "radar"]) {
      expect(hidden[key]).toMatchObject({ axisLine: { show: false } });
    }
    // The axis color stays beside the visibility.
    expect(hidden.valueAxis).toMatchObject({
      axisLine: { lineStyle: { color: "rgba(255, 255, 255, 0.3)" } },
    });
    expect(hidden.visualMap).toMatchObject({ textStyle: { fontSize: 14 } });
    const plain = themeOption(lightThemeTokens()) as Record<string, Record<string, unknown>>;
    expect(plain.valueAxis).not.toHaveProperty("axisLine");
    expect(plain.visualMap).toMatchObject({ textStyle: { fontSize: 12 } });
  });

  it("leaves the spacing to ECharts when the tokens are unset", () => {
    const plain = themeOption(lightThemeTokens(NO_SPACING)) as Record<
      string,
      Record<string, unknown>
    >;
    expect(plain.valueAxis).not.toHaveProperty("nameTextStyle");
    expect(plain.valueAxis).not.toHaveProperty("nameLocation");
    expect(plain.valueAxis?.axisLabel).not.toHaveProperty("margin");
  });
});

describe("a registered theme's axis spacing", () => {
  // biome-ignore lint/suspicious/noExplicitAny: the R package's ECharts bundle carries no types.
  let echarts: any = null;
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  if (existsSync(path)) echarts = createRequire(import.meta.url)(path);

  // The x position of the y axis name, for a theme and the y axis option.
  function nameX(theme: Record<string, unknown>, yAxis: Record<string, unknown>): number {
    const id = `spacing-${Math.random()}`;
    echarts.registerTheme(id, theme);
    const chart = echarts.init(null, id, { renderer: "svg", ssr: true, width: 600, height: 400 });
    chart.setOption({
      animation: false,
      grid: { left: 150 },
      xAxis: { type: "category", data: ["a", "b"] },
      yAxis: { type: "value", name: "Value name", nameGap: 5, ...yAxis },
      series: [{ type: "bar", data: [1000000, 2] }],
    });
    const svg: string = chart.renderToSVGString();
    chart.dispose();
    const m = svg.match(/<text[^>]*transform="matrix\(([^)]*)\)"[^>]*>Value name<\/text>/);
    return Number(m?.[1]?.split(",")[4]);
  }

  it.skipIf(!echarts)("places a centered name as the same margin in the option does", () => {
    const theme = themeOption(
      lightThemeTokens({ base_font_size: 16, axis_name_gap: 1.875, axis_name_location: "middle" }),
    );
    // The theme's 30-pixel margin and location, against the option's.
    const fromTheme = nameX(theme, {});
    const fromOption = nameX(themeOption(lightThemeTokens({ base_font_size: 16, ...NO_SPACING })), {
      nameLocation: "middle",
      nameTextStyle: { textMargin: [30, 3, 30, 3] },
    });
    expect(fromTheme).toBeCloseTo(fromOption, 1);
    // ECharts' own margin places the name nearer the tick labels.
    const plain = nameX(themeOption(lightThemeTokens({ base_font_size: 16, ...NO_SPACING })), {
      nameLocation: "middle",
    });
    expect(plain - fromTheme).toBeGreaterThan(10);
  });
});

describe("themeObject and mergePatch", () => {
  it("merges the document's ECharts patch over the projection", () => {
    const obj = themeObject({
      ...lightThemeTokens(),
      echarts: {
        title: { textStyle: { fontSize: 30 } },
        color: ["#000000"],
        dataZoom: null,
      },
    }) as Record<string, Record<string, Record<string, unknown>>>;
    expect(obj.title?.textStyle?.fontSize).toBe(30);
    // The rest of the title's style stays.
    expect(obj.title?.textStyle?.color).toBe("rgba(0, 0, 0, 0.7)");
    expect(obj.color).toEqual(["#000000"]);
    expect(obj).not.toHaveProperty("dataZoom");
  });

  it("is the projection without a patch", () => {
    expect(themeObject(darkThemeTokens())).toEqual(themeOption(darkThemeTokens()));
  });
});

describe("fontStack", () => {
  it("keeps a CSS variable reference as written", () => {
    expect(fontStack("var(--font-geist-sans), ui-sans-serif, sans-serif")).toBe(
      "var(--font-geist-sans), ui-sans-serif, sans-serif",
    );
  });

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
