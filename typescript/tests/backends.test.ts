import { describe, expect, it } from "vitest";
import {
  choroplethTheme,
  DIAGRAM_GRAYS,
  diagramTheme,
  flattenColor,
  networkTheme,
} from "../src/theme/backends.ts";
import { DEFAULT_FONT_FAMILY, darkThemeTokens, lightThemeTokens } from "../src/theme/tokens.ts";

describe("networkTheme and choroplethTheme", () => {
  it("state the light theme's surface, text, font, and scheme", () => {
    const tokens = lightThemeTokens();
    const network = networkTheme(tokens);
    expect(network).toMatchObject({
      background: "#ffffff",
      label_color: "rgba(0, 0, 0, 0.7)",
      font_family: DEFAULT_FONT_FAMILY,
      scheme: "light",
      palette: [...tokens.palette],
      positive_color: tokens.positive_color,
      negative_color: tokens.negative_color,
      tooltip_background: "rgba(255,255,255,0.85)",
    });
    expect(choroplethTheme(tokens)).toEqual({
      scheme: "light",
      background: "#ffffff",
      foreground: "rgba(0, 0, 0, 0.7)",
      font_family: DEFAULT_FONT_FAMILY,
      overlay: {
        background: "rgba(255,255,255,0.85)",
        foreground: "rgba(0, 0, 0, 0.7)",
        muted: "rgba(26,26,26,0.65)",
      },
    });
  });

  it("state the dark scheme rather than derive it from the background", () => {
    const network = networkTheme(darkThemeTokens({ bg_color: "#ffffff" }));
    expect(network.scheme).toBe("dark");
    expect(network.tooltip_background).toBe("rgba(40,40,40,0.85)");
    const map = choroplethTheme(darkThemeTokens({ font_family: "Source Sans 3" }));
    expect(map.font_family).toBe(`'Source Sans 3', ${DEFAULT_FONT_FAMILY}`);
    expect(map.overlay.muted).toBe("rgba(230,230,230,0.7)");
  });
});

describe("diagramTheme", () => {
  it("gives Pele's variables: surface, opaque text, grays, font, series, accent", () => {
    const vars = diagramTheme(
      lightThemeTokens({ palette: ["#6CA3A0", "#F08904", "#BE2E5F"], font_family: "Geist" }),
    );
    expect(vars).toMatchObject({
      bg: "#ffffff",
      fg: "#4d4d4d",
      ...DIAGRAM_GRAYS.light,
      font: `Geist, ${DEFAULT_FONT_FAMILY}`,
      "series-1": "#6CA3A0",
      "series-4": "#6CA3A0",
      "series-8": "#F08904",
      accent: "#6CA3A0",
    });
    const dark = diagramTheme(darkThemeTokens());
    expect(dark).toMatchObject({ bg: "#181818", fg: "#bababa", ...DIAGRAM_GRAYS.dark });
  });

  it("recycles a short palette over the eight series colors", () => {
    const vars = diagramTheme(lightThemeTokens({ palette: ["#aa0000", "#00aa00"] }));
    expect(vars["series-3"]).toBe("#aa0000");
    expect(vars["series-8"]).toBe("#00aa00");
    expect(vars.accent).toBe("#aa0000");
  });
});

describe("flattenColor", () => {
  it("composites an rgba color over the background", () => {
    expect(flattenColor("rgba(0, 0, 0, 0.7)", "#ffffff")).toBe("#4d4d4d");
    expect(flattenColor("rgba(255, 255, 255, 0.7)", "#181818")).toBe("#bababa");
    expect(flattenColor("#123456", "#ffffff")).toBe("#123456");
  });
});
