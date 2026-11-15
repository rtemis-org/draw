/**
 * Theme tokens: the constants chosen once for a set of charts, independent of
 * the data and the surface, and their projection to the ECharts theme object.
 * The token document is generated from rtemis.draw's `Theme` class.
 * spec: draw/chart-appearance#the-theme
 */

/** Light or dark: which set of neutral control colors the projection uses. */
export type ColorScheme = "light" | "dark";

/** A theme's tokens. Unset text sizes follow `base_font_size`; unset text colors follow `fg_color`. */
export interface ThemeTokens {
  readonly scheme: ColorScheme;
  /** Font family or CSS font stack; unset is the system UI font stack. */
  readonly font_family?: string | null;
  readonly base_font_size: number;
  /** Unset is `base_font_size` × 1.2, rounded half to even. */
  readonly title_font_size?: number | null;
  readonly subtitle_font_size?: number | null;
  readonly axis_label_font_size?: number | null;
  readonly legend_font_size?: number | null;
  readonly tooltip_font_size?: number | null;
  /** Categorical palette. */
  readonly palette: readonly string[];
  readonly bg_color: string;
  readonly fg_color: string;
  readonly title_color?: string | null;
  readonly subtitle_color?: string | null;
  readonly legend_color?: string | null;
  readonly axis_color?: string | null;
  readonly grid_color?: string | null;
  readonly tooltip_bg?: string | null;
  readonly tooltip_border_color?: string | null;
  readonly tooltip_color?: string | null;
  /** Colors of negative values, values of neither sign, and positive values. */
  readonly negative_color: string;
  readonly neutral_color: string;
  readonly positive_color: string;
}

/** rtemis' categorical palette. */
export const RTEMIS_PALETTE: readonly string[] = Object.freeze([
  "#6CA3A0",
  "#F08904",
  "#BE2E5F",
  "#B3CFE8",
  "#0F6A66",
  "#FDB808",
  "#EA384A",
  "#466D96",
  "#526551",
  "#F384FF",
  "#7D0830",
  "#375D86",
  "#B1A7B3",
  "#7364F2",
  "#895140",
  "#C5A267",
]);

/** Sign colors of equal perceived lightness: blue, gray, orange. */
export const SIGN_COLORS = Object.freeze({
  negative: "#2D83D3",
  neutral: "#808080",
  positive: "#C55E28",
});

/** The light theme's tokens, as rtemis.draw's `theme_light()` sets them. */
export function lightThemeTokens(overrides: Partial<ThemeTokens> = {}): ThemeTokens {
  return Object.freeze({
    scheme: "light",
    base_font_size: 12,
    palette: RTEMIS_PALETTE,
    bg_color: "#ffffff",
    fg_color: "rgba(0, 0, 0, 0.7)",
    negative_color: SIGN_COLORS.negative,
    neutral_color: SIGN_COLORS.neutral,
    positive_color: SIGN_COLORS.positive,
    ...overrides,
  });
}

/** The dark theme's tokens, as rtemis.draw's `theme_dark()` sets them. */
export function darkThemeTokens(overrides: Partial<ThemeTokens> = {}): ThemeTokens {
  return Object.freeze({
    scheme: "dark",
    base_font_size: 12,
    palette: RTEMIS_PALETTE,
    bg_color: "#181818",
    fg_color: "rgba(255, 255, 255, 0.7)",
    title_color: "rgba(255, 255, 255, 0.9)",
    subtitle_color: "rgba(255, 255, 255, 0.5)",
    axis_color: "rgba(255, 255, 255, 0.3)",
    grid_color: "rgba(255, 255, 255, 0.1)",
    tooltip_bg: "rgba(20, 20, 20, 0.9)",
    tooltip_border_color: "rgba(255, 255, 255, 0.1)",
    negative_color: SIGN_COLORS.negative,
    neutral_color: SIGN_COLORS.neutral,
    positive_color: SIGN_COLORS.positive,
    ...overrides,
  });
}

/** Font stack that follows a font family, and the whole stack when none is set. */
export const DEFAULT_FONT_FAMILY =
  "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif";

// CSS generic family names and the vendor keywords for the system UI font,
// which are keywords: quoting one turns it into a family name no font has.
const GENERIC_FONT_FAMILIES = new Set([
  "serif",
  "sans-serif",
  "monospace",
  "cursive",
  "fantasy",
  "math",
  "emoji",
  "fangsong",
  "system-ui",
  "ui-serif",
  "ui-sans-serif",
  "ui-monospace",
  "ui-rounded",
  "-apple-system",
  "blinkmacsystemfont",
]);

/**
 * A complete CSS font stack: every family name that is not a sequence of CSS
 * identifiers is quoted, and {@link DEFAULT_FONT_FAMILY} follows unless the
 * stack ends in a generic family. Matches rtemis.draw's `font_stack()`.
 */
export function fontStack(fontFamily?: string | null): string {
  if (fontFamily === undefined || fontFamily === null) return DEFAULT_FONT_FAMILY;
  const entries = fontFamily
    .split(",")
    .map((e) => e.trim())
    .filter((e) => e.length > 0);
  if (entries.length === 0) throw new Error("`font_family` must be a nonempty font family.");
  const ident = /^-?[A-Za-z_][A-Za-z0-9_-]*( +-?[A-Za-z_][A-Za-z0-9_-]*)*$/;
  const quoted = /^(['"]).*\1$/;
  const out = entries.map((e) => {
    const generic = GENERIC_FONT_FAMILIES.has(e.toLowerCase());
    return quoted.test(e) || generic || ident.test(e) ? e : `'${e.replaceAll("'", "\\'")}'`;
  });
  const last = entries[entries.length - 1] ?? "";
  if (!GENERIC_FONT_FAMILIES.has(last.toLowerCase())) out.push(DEFAULT_FONT_FAMILY);
  return out.join(", ");
}

/** Round half to even, as R's `round()` does. */
function roundHalfEven(x: number): number {
  const floor = Math.floor(x);
  const diff = x - floor;
  if (Math.abs(diff - 0.5) > 1e-9) return Math.round(x);
  return floor % 2 === 0 ? floor : floor + 1;
}

/** The text sizes of a theme, each resolved from `base_font_size` when unset. */
export function themeFontSizes(tokens: ThemeTokens) {
  const base = tokens.base_font_size;
  return {
    base,
    title: tokens.title_font_size ?? roundHalfEven(base * 1.2),
    subtitle: tokens.subtitle_font_size ?? base,
    axisLabel: tokens.axis_label_font_size ?? base,
    legend: tokens.legend_font_size ?? base,
    tooltip: tokens.tooltip_font_size ?? base,
  };
}

type Json = Record<string, unknown>;

function present(entries: Record<string, unknown>): Json {
  return Object.fromEntries(
    Object.entries(entries).filter(([, v]) => v !== undefined && v !== null),
  );
}

/**
 * The ECharts theme object of a theme's tokens, for `echarts.registerTheme()`
 * or `chart.setTheme()`. Every component with its own default text size takes
 * the global text style, since a global `textStyle` does not reach them; log
 * and time axes take the value axis's style.
 */
export function themeOption(tokens: ThemeTokens): Json {
  const sizes = themeFontSizes(tokens);
  const dark = tokens.scheme === "dark";
  const fg = tokens.fg_color;
  const textStyle = { color: fg, fontFamily: fontStack(tokens.font_family), fontSize: sizes.base };
  const withText = (style: Json): Json => ({ ...textStyle, ...style });
  const axisLabel = { fontSize: sizes.axisLabel, color: fg };
  const axisLine = tokens.axis_color ? { lineStyle: { color: tokens.axis_color } } : undefined;
  const valueAxis = present({
    axisLabel,
    axisLine,
    splitLine: tokens.grid_color ? { lineStyle: { color: tokens.grid_color } } : undefined,
  });
  return {
    color: [...tokens.palette],
    backgroundColor: tokens.bg_color,
    textStyle,
    title: {
      textStyle: withText(present({ color: tokens.title_color, fontSize: sizes.title })),
      subtextStyle: withText(present({ color: tokens.subtitle_color, fontSize: sizes.subtitle })),
    },
    legend: {
      textStyle: withText({ color: tokens.legend_color ?? fg, fontSize: sizes.legend }),
    },
    tooltip: present({
      backgroundColor: tokens.tooltip_bg,
      borderColor: tokens.tooltip_border_color,
      textStyle: withText({ color: tokens.tooltip_color ?? fg, fontSize: sizes.tooltip }),
    }),
    visualMap: { textStyle },
    // Neutral controls leave the palette to encode data; every accent-bearing
    // slider state is set so ECharts' blue defaults cannot show.
    // spec: draw/visual-qa-docs#neutral-data-zoom-sliders
    dataZoom: {
      backgroundColor: "rgba(128,128,128,0.06)",
      borderColor: dark ? "#606060" : "#b0b0b0",
      fillerColor: "rgba(128,128,128,0.18)",
      dataBackground: {
        lineStyle: { color: "#808080" },
        areaStyle: { color: "#808080", opacity: 0.12 },
      },
      selectedDataBackground: {
        lineStyle: { color: dark ? "#b0b0b0" : "#707070" },
        areaStyle: { color: "#808080", opacity: 0.25 },
      },
      handleStyle: {
        color: dark ? "#303030" : "#ffffff",
        borderColor: dark ? "#909090" : "#808080",
      },
      moveHandleStyle: { color: "#909090", opacity: 0.6 },
      brushStyle: { color: "#808080", opacity: 0.3 },
      textStyle: axisLabel,
      emphasis: {
        handleStyle: { borderColor: dark ? "#cccccc" : "#505050" },
        moveHandleStyle: { color: dark ? "#b0b0b0" : "#707070" },
      },
    },
    line: { symbol: "circle" },
    categoryAxis: present({ axisLabel, axisLine, splitLine: { show: false } }),
    valueAxis,
    logAxis: valueAxis,
    timeAxis: valueAxis,
  };
}
