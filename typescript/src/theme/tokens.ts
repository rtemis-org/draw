/**
 * Theme tokens: the constants chosen once for a set of charts, independent of
 * the data and the surface, and their projection to the ECharts theme object.
 * The token document is generated from rtemis.draw's `Theme` class.
 * spec: draw/chart-appearance#the-theme
 */

/** Light or dark: which set of neutral control colors the projection uses. */
export type ColorScheme = "light" | "dark";

/** A font weight from 1 to 1000: 400 is normal, 700 bold. */
export type FontWeight = number;

/** Where an axis name sits along its axis. */
export type AxisNameLocation = "start" | "middle" | "end";

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
  /** Text size of color scale labels; unset is `base_font_size`. */
  readonly color_scale_font_size?: number | null;
  /** Weight of every text; unset is ECharts' default, normal (400). */
  readonly font_weight?: FontWeight | null;
  /** Weight of titles; unset is ECharts' default, bold (700). */
  readonly title_font_weight?: FontWeight | null;
  /** Categorical palette. */
  readonly palette: readonly string[];
  readonly bg_color: string;
  readonly fg_color: string;
  readonly title_color?: string | null;
  readonly subtitle_color?: string | null;
  readonly legend_color?: string | null;
  readonly axis_color?: string | null;
  readonly grid_color?: string | null;
  /** Opacity of gridlines, from 0 to 1; unset is 1. */
  readonly grid_opacity?: number | null;
  /** Dash style of gridlines; unset is `solid`. */
  readonly grid_style?: "solid" | "dashed" | "dotted" | null;
  readonly tooltip_bg?: string | null;
  readonly tooltip_border_color?: string | null;
  readonly tooltip_color?: string | null;
  /** Colors of negative values, values of neither sign, and positive values. */
  readonly negative_color: string;
  readonly neutral_color: string;
  readonly positive_color: string;
  /**
   * Distance from the plot to the tick labels, in em of the axis label font
   * size; unset is ECharts' default, 8 pixels.
   */
  readonly axis_label_gap?: number | null;
  /**
   * Margin around an axis name, in em of the base font size: a centered name
   * keeps it from the tick labels, which ECharts moves the name clear of.
   * Unset is ECharts' default, 5 pixels on a horizontal axis and 8 on a
   * vertical one.
   */
  readonly axis_name_gap?: number | null;
  /** Where every axis name sits; unset leaves it to the chart and ECharts. */
  readonly axis_name_location?: AxisNameLocation | null;
  /** Whether axis lines show; unset is ECharts' default for each axis type. */
  readonly axis_line_show?: boolean | null;
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

/**
 * The axis spacing of rtemis.draw's light and dark themes: tick labels 2/3 em
 * from the plot (8 pixels at the 12-pixel base), axis names 0.625 em clear of
 * the tick labels, and names centered on their axis.
 */
const THEME_SPACING = Object.freeze({
  axis_label_gap: 2 / 3,
  axis_name_gap: 0.625,
  axis_name_location: "middle",
} as const);

/** The light theme's tokens, as rtemis.draw's `theme_light()` sets them. */
export function lightThemeTokens(overrides: Partial<ThemeTokens> = {}): ThemeTokens {
  return Object.freeze({
    scheme: "light",
    base_font_size: 12,
    palette: RTEMIS_PALETTE,
    ...THEME_SPACING,
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
    ...THEME_SPACING,
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
 * identifiers or a CSS variable reference is quoted, and
 * {@link DEFAULT_FONT_FAMILY} follows unless the stack ends in a generic
 * family. Matches rtemis.draw's `font_stack()`.
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
  // A CSS variable reference (`var(--font-sans)`) is kept as written.
  const variable = /^var\(.*\)$/i;
  const out = entries.map((e) => {
    const generic = GENERIC_FONT_FAMILIES.has(e.toLowerCase());
    return quoted.test(e) || generic || ident.test(e) || variable.test(e)
      ? e
      : `'${e.replaceAll("'", "\\'")}'`;
  });
  const last = entries[entries.length - 1] ?? "";
  if (!GENERIC_FONT_FAMILIES.has(last.toLowerCase())) out.push(DEFAULT_FONT_FAMILY);
  return out.join(", ");
}

/** A length in em of `size` as pixels, or undefined when the length is unset. */
function em(length: number | null | undefined, size: number): number | undefined {
  return length === undefined || length === null ? undefined : length * size;
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
 * Neutral gray styling of a zoom slider in a light or dark theme, every
 * accent-bearing state included (preview, brush, and hover), so the chart
 * palette alone encodes data. Part of {@link themeOption}; a host that draws
 * without the theme merges it into each slider. Component options still win.
 */
export function zoomSliderStyle(dark: boolean): Record<string, unknown> {
  return {
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
    emphasis: {
      handleStyle: { borderColor: dark ? "#cccccc" : "#505050" },
      moveHandleStyle: { color: dark ? "#b0b0b0" : "#707070" },
    },
  };
}

/**
 * Series defaults of every rtemis chart: filled circle symbols on line and
 * radar series, where ECharts' default `emptyCircle` fills with the canvas
 * background. Part of {@link themeOption}; a host that draws without the
 * theme passes it to `echarts.init()` as the theme.
 */
export const SERIES_THEME = Object.freeze({
  line: Object.freeze({ symbol: "circle" }),
  radar: Object.freeze({ symbol: "circle" }),
});

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
  const textStyle = present({
    color: fg,
    fontFamily: fontStack(tokens.font_family),
    fontSize: sizes.base,
    fontWeight: tokens.font_weight,
  });
  const withText = (style: Json): Json => ({ ...textStyle, ...style });
  const axisLabelText = present({
    fontSize: sizes.axisLabel,
    color: fg,
    fontWeight: tokens.font_weight,
  });
  const axisLabel = present({
    ...axisLabelText,
    margin: em(tokens.axis_label_gap, sizes.axisLabel),
  });
  const nameMargin = em(tokens.axis_name_gap, sizes.base);
  const nameTextStyle =
    nameMargin === undefined ? undefined : { textMargin: [nameMargin, 3, nameMargin, 3] };
  const axisLineStyle = tokens.axis_color ? { lineStyle: { color: tokens.axis_color } } : {};
  const axisLine =
    Object.keys(axisLineStyle).length > 0 ||
    (tokens.axis_line_show !== undefined && tokens.axis_line_show !== null)
      ? present({ show: tokens.axis_line_show, ...axisLineStyle })
      : undefined;
  const axisTick = { show: false };
  const gridLine = present({
    color: tokens.grid_color,
    opacity: tokens.grid_opacity,
    type: tokens.grid_style,
  });
  const splitLine = Object.keys(gridLine).length > 0 ? { lineStyle: gridLine } : undefined;
  const axisName = present({ nameLocation: tokens.axis_name_location, nameTextStyle });
  const valueAxis = present({ ...axisName, axisLabel, axisLine, axisTick, splitLine });
  // The radar coordinate system: indicator names in the axis label style,
  // spokes in the axis color, and rings and their bands in the grid color.
  // ECharts merges a theme's `radar` entry into the radar component and into
  // radar series, so the series' symbol sits in the same entry.
  const radar = present({
    ...SERIES_THEME.radar,
    axisName: axisLabelText,
    axisLine,
    splitLine,
    splitArea: tokens.grid_color
      ? { areaStyle: { color: ["rgba(0, 0, 0, 0)", tokens.grid_color] } }
      : undefined,
  });
  return {
    color: [...tokens.palette],
    backgroundColor: tokens.bg_color,
    textStyle,
    title: {
      textStyle: withText(
        present({
          color: tokens.title_color,
          fontSize: sizes.title,
          fontWeight: tokens.title_font_weight,
        }),
      ),
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
    visualMap: {
      textStyle: present({ ...textStyle, fontSize: tokens.color_scale_font_size ?? sizes.base }),
    },
    // Neutral controls leave the palette to encode data; every accent-bearing
    // slider state is set so ECharts' blue defaults cannot show.
    // spec: draw/visual-qa-docs#neutral-data-zoom-sliders
    dataZoom: { ...zoomSliderStyle(dark), textStyle: axisLabelText },
    ...SERIES_THEME,
    radar,
    categoryAxis: present({
      ...axisName,
      axisLabel,
      axisLine,
      axisTick,
      splitLine: { show: false },
    }),
    valueAxis,
    logAxis: valueAxis,
    timeAxis: valueAxis,
  };
}

/**
 * A theme as rtemis.draw's `Theme` writes it: the tokens, and `echarts`, an
 * ECharts theme-object patch merged over their projection.
 */
export type ThemeDocument = ThemeTokens & {
  readonly echarts?: Record<string, unknown> | null;
  /** Where each token came from; recorded by rtemis.draw's theme constructors. */
  readonly origin?: Readonly<Record<string, ThemeOrigin>> | null;
  /** Which interface wrote a resolved theme, as `name` and `version`. */
  readonly writer?: Readonly<{ name: string; version: string }> | null;
};

/** Where a token's value came from. */
export type ThemeOrigin = "user" | "default" | "derived";

/** The members of a theme document, as rtemis.draw's `Theme` declares them. */
export const THEME_DOCUMENT_MEMBERS = Object.freeze([
  "scheme",
  "font_family",
  "base_font_size",
  "title_font_size",
  "subtitle_font_size",
  "axis_label_font_size",
  "legend_font_size",
  "tooltip_font_size",
  "color_scale_font_size",
  "font_weight",
  "title_font_weight",
  "palette",
  "bg_color",
  "fg_color",
  "title_color",
  "subtitle_color",
  "legend_color",
  "axis_color",
  "grid_color",
  "grid_opacity",
  "grid_style",
  "tooltip_bg",
  "tooltip_border_color",
  "tooltip_color",
  "negative_color",
  "neutral_color",
  "positive_color",
  "axis_label_gap",
  "axis_name_gap",
  "axis_name_location",
  "axis_line_show",
  "echarts",
  "origin",
  "writer",
] as const);

/**
 * A theme with the tokens that follow rules of the theme itself filled in and
 * marked `"derived"`: the title size, 1.2 times `base_font_size` rounded half
 * to even; the subtitle, axis label, legend, tooltip, and color scale sizes,
 * `base_font_size`; the title, subtitle, legend, and tooltip colors,
 * `fg_color`; and the font family, {@link DEFAULT_FONT_FAMILY}. Tokens left to
 * a backend's own default stay unset. rtemis.draw's `resolve_theme()` ports
 * these rules. spec: draw/chart-appearance#the-theme
 */
export function resolveTheme(doc: ThemeDocument): ThemeDocument {
  const origin: Record<string, ThemeOrigin> = { ...(doc.origin ?? {}) };
  const out: Record<string, unknown> = { ...doc };
  const derive = (name: string, value: unknown) => {
    if (out[name] === undefined || out[name] === null) {
      out[name] = value;
      origin[name] = "derived";
    }
  };
  const base = doc.base_font_size;
  derive("title_font_size", roundHalfEven(base * 1.2));
  for (const name of [
    "subtitle_font_size",
    "axis_label_font_size",
    "legend_font_size",
    "tooltip_font_size",
    "color_scale_font_size",
  ]) {
    derive(name, base);
  }
  for (const name of ["title_color", "subtitle_color", "legend_color", "tooltip_color"]) {
    derive(name, doc.fg_color);
  }
  derive("font_family", DEFAULT_FONT_FAMILY);
  return { ...(out as unknown as ThemeDocument), origin };
}

/**
 * Merges a patch into a JSON object: objects merge recursively, any other
 * value, an array included, replaces the target's, and `null` removes the key.
 * Returns a new object.
 */
export function mergePatch(target: Json, patch: Json): Json {
  const out: Json = { ...target };
  for (const [key, value] of Object.entries(patch)) {
    if (value === null) {
      delete out[key];
    } else if (
      typeof value === "object" &&
      !Array.isArray(value) &&
      typeof out[key] === "object" &&
      out[key] !== null &&
      !Array.isArray(out[key])
    ) {
      out[key] = mergePatch(out[key] as Json, value as Json);
    } else {
      out[key] = value;
    }
  }
  return out;
}

/** The ECharts theme object of a theme document: the projection, then the patch. */
export function themeObject(doc: ThemeDocument): Json {
  const { echarts, ...tokens } = doc;
  const option = themeOption(tokens);
  return echarts ? mergePatch(option, echarts) : option;
}
