/**
 * Projections of a theme's tokens to the backends other than ECharts: the
 * Sigma.js network, the MapLibre choropleth, and Pele diagrams. Each reads the
 * stated scheme, colors, and font of the tokens.
 * spec: draw/chart-appearance#the-theme
 */

import type { NetworkAppearance } from "../charts/network/config.ts";
import { fontStack, type ThemeTokens } from "./tokens.ts";

/** The background of panels drawn over a network or a map: tooltips, legends, titles. */
function overlayBackground(dark: boolean): string {
  return dark ? "rgba(40,40,40,0.85)" : "rgba(255,255,255,0.85)";
}

/** What a network widget reads from a theme: the reducers' appearance and its surface. */
export interface NetworkTheme extends NetworkAppearance {
  readonly palette: readonly string[];
  readonly positive_color: string;
  readonly negative_color: string;
  readonly label_color: string;
  readonly font_family: string;
  readonly scheme: "light" | "dark";
  /** The canvas background. */
  readonly background: string;
  /** The hover tooltip's background. */
  readonly tooltip_background: string;
}

/** The network theme of a theme's tokens. */
export function networkTheme(tokens: ThemeTokens): NetworkTheme {
  const dark = tokens.scheme === "dark";
  return {
    palette: [...tokens.palette],
    positive_color: tokens.positive_color,
    negative_color: tokens.negative_color,
    label_color: tokens.fg_color,
    font_family: fontStack(tokens.font_family),
    scheme: tokens.scheme,
    background: tokens.bg_color,
    tooltip_background: overlayBackground(dark),
  };
}

/** What a choropleth widget reads from a theme. */
export interface ChoroplethTheme {
  readonly scheme: "light" | "dark";
  /** The map's background, where no region is drawn. */
  readonly background: string;
  readonly foreground: string;
  readonly font_family: string;
  /** The title, legend, and tooltip panels over the map. */
  readonly overlay: {
    readonly background: string;
    readonly foreground: string;
    readonly muted: string;
  };
}

/** The choropleth theme of a theme's tokens. */
export function choroplethTheme(tokens: ThemeTokens): ChoroplethTheme {
  const dark = tokens.scheme === "dark";
  return {
    scheme: tokens.scheme,
    background: tokens.bg_color,
    foreground: tokens.fg_color,
    font_family: fontStack(tokens.font_family),
    overlay: {
      background: overlayBackground(dark),
      foreground: tokens.fg_color,
      muted: dark ? "rgba(230,230,230,0.7)" : "rgba(26,26,26,0.65)",
    },
  };
}

/**
 * Neutral grays of every uncolored diagram element, by scheme. Nodes sit
 * closer to the background than subgraphs.
 */
export const DIAGRAM_GRAYS = Object.freeze({
  light: Object.freeze({
    muted: "#6e6e6e",
    line: "#5c5c5c",
    surface: "#f3f3f3",
    "surface-alt": "#e6e6e6",
    border: "#8a8a8a",
  }),
  dark: Object.freeze({
    muted: "#999999",
    line: "#a3a3a3",
    surface: "#232323",
    "surface-alt": "#2e2e2e",
    border: "#6a6a6a",
  }),
});

/** Pele cycles its categorical colors over eight variables. */
export const DIAGRAM_SERIES = 8;

function hex6(color: string): string | null {
  let h = color.trim().replace("#", "");
  if (h.length === 3) h = [...h].map((c) => c + c).join("");
  return /^[0-9a-f]{6}$/i.test(h) ? h.toLowerCase() : null;
}

/**
 * An `rgb()` or `rgba()` color composited over a background, as an opaque hex
 * color, so an exported file carries no alpha the background would supply.
 * Any other color is returned as it is.
 */
export function flattenColor(color: string, background: string): string {
  const m = /^rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*(?:,\s*([\d.]+)\s*)?\)$/i.exec(
    color.trim(),
  );
  if (!m) return color;
  const a = m[4] === undefined ? 1 : Number(m[4]);
  const base = hex6(background) ?? "ffffff";
  const channels = [m[1], m[2], m[3]].map((c, i) => {
    const under = Number.parseInt(base.slice(2 * i, 2 * i + 2), 16);
    const v = Math.round(a * Number(c) + (1 - a) * under);
    return Math.max(0, Math.min(255, v)).toString(16).padStart(2, "0");
  });
  return `#${channels.join("")}`;
}

/**
 * Pele's CSS variable values (names without the `--pele-` prefix) for a
 * theme's tokens: background, opaque foreground, the scheme's neutral grays,
 * the font, eight categorical colors cycled from the palette, and the accent,
 * its first color.
 */
export function diagramTheme(tokens: ThemeTokens): Record<string, string> {
  const out: Record<string, string> = {
    bg: tokens.bg_color,
    fg: flattenColor(tokens.fg_color, tokens.bg_color),
    ...DIAGRAM_GRAYS[tokens.scheme],
    font: fontStack(tokens.font_family),
  };
  const palette = tokens.palette;
  if (palette.length > 0) {
    for (let i = 0; i < DIAGRAM_SERIES; i++) {
      out[`series-${i + 1}`] = palette[i % palette.length] as string;
    }
    out.accent = palette[0] as string;
  }
  return out;
}
