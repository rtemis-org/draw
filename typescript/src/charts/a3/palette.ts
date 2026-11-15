import type { A3VisualizationTheme } from "./types.ts";

const DEFAULT_REGION_PALETTE = Object.freeze([
  "#0F766E",
  "#2563EB",
  "#D97706",
  "#7C3AED",
  "#DC2626",
  "#0891B2",
]);

const DEFAULT_SITE_PALETTE = Object.freeze([
  "#0EA5E9",
  "#14B8A6",
  "#E11D48",
  "#A855F7",
  "#F97316",
  "#16A34A",
]);

const DEFAULT_PTM_PALETTE = Object.freeze([
  "#0284C7",
  "#F59E0B",
  "#10B981",
  "#8B5CF6",
  "#EF4444",
  "#14B8A6",
]);

const DEFAULT_PROCESSING_PALETTE = Object.freeze([
  "#D97706",
  "#DC2626",
  "#0EA5E9",
  "#9333EA",
  "#16A34A",
  "#DB2777",
]);

const DEFAULT_GEIST_FONT_STACK =
  "Geist, 'Geist Sans', 'Inter', 'Avenir Next', 'Segoe UI', sans-serif";

/**
 * Default (light) theme shared by visualization renderers.
 */
export const DEFAULT_A3_VISUALIZATION_THEME: A3VisualizationTheme = Object.freeze({
  fontFamily: DEFAULT_GEIST_FONT_STACK,
  annotationFontFamily: DEFAULT_GEIST_FONT_STACK,
  legendFontFamily: DEFAULT_GEIST_FONT_STACK,
  backgroundColor: "transparent",
  plotBackgroundColor: "transparent",
  residueFillColor: "#E7E5E4",
  residueStrokeColor: "#44403C",
  labelColor: "#1C1917",
  positionLabelColor: "#78716C",
  legendColor: "#1C1917",
  gridColor: "#E7E5E4",
  variantColor: "#FA6E1E",
  diseaseVariantColor: "#E266AE",
  regionPalette: DEFAULT_REGION_PALETTE,
  sitePalette: DEFAULT_SITE_PALETTE,
  ptmPalette: DEFAULT_PTM_PALETTE,
  processingPalette: DEFAULT_PROCESSING_PALETTE,
});

/**
 * Dark theme for A3 visualizations. Uses transparent backgrounds so the
 * app's own dark background shows through, with light text and adjusted
 * marker colors for dark-mode contrast.
 */
export const DARK_A3_VISUALIZATION_THEME: A3VisualizationTheme = Object.freeze({
  fontFamily: DEFAULT_GEIST_FONT_STACK,
  annotationFontFamily: DEFAULT_GEIST_FONT_STACK,
  legendFontFamily: DEFAULT_GEIST_FONT_STACK,
  backgroundColor: "transparent",
  plotBackgroundColor: "transparent",
  residueFillColor: "#3f3f46", // zinc-700
  residueStrokeColor: "#a1a1aa", // zinc-400
  labelColor: "#fafafa", // zinc-50
  positionLabelColor: "#a1a1aa", // zinc-400
  legendColor: "#e4e4e7", // zinc-200
  gridColor: "#3f3f46", // zinc-700
  variantColor: "#fb923c", // orange-400 (brighter for dark bg)
  diseaseVariantColor: "#f472b6", // pink-400 (brighter for dark bg)
  regionPalette: DEFAULT_REGION_PALETTE,
  sitePalette: DEFAULT_SITE_PALETTE,
  ptmPalette: DEFAULT_PTM_PALETTE,
  processingPalette: DEFAULT_PROCESSING_PALETTE,
});

/**
 * Resolves a full theme by layering user overrides onto the package defaults.
 */
export function resolveA3VisualizationTheme(
  overrides: Partial<A3VisualizationTheme> = {},
): A3VisualizationTheme {
  const fallbackFontFamily =
    overrides.fontFamily ?? DEFAULT_A3_VISUALIZATION_THEME.annotationFontFamily;

  return Object.freeze({
    ...DEFAULT_A3_VISUALIZATION_THEME,
    ...overrides,
    fontFamily: fallbackFontFamily,
    annotationFontFamily: overrides.annotationFontFamily ?? fallbackFontFamily,
    legendFontFamily: overrides.legendFontFamily ?? fallbackFontFamily,
    regionPalette: overrides.regionPalette ?? DEFAULT_REGION_PALETTE,
    sitePalette: overrides.sitePalette ?? DEFAULT_SITE_PALETTE,
    ptmPalette: overrides.ptmPalette ?? DEFAULT_PTM_PALETTE,
    processingPalette: overrides.processingPalette ?? DEFAULT_PROCESSING_PALETTE,
  });
}

/**
 * Returns a recycled palette color for a zero-based index.
 */
export function paletteColorAt(palette: readonly string[], index: number): string {
  if (palette.length === 0) {
    throw new RangeError("A3 visualization palettes cannot be empty.");
  }

  return palette[index % palette.length] ?? palette[0] ?? "#000000";
}

/**
 * Converts a hex color into an rgba string with the requested alpha value.
 */
export function withAlpha(color: string, alpha: number): string {
  const normalized = color.trim();
  if (!normalized.startsWith("#")) {
    return normalized;
  }

  const hex = normalized.slice(1);
  const expanded =
    hex.length === 3
      ? hex
          .split("")
          .map((character) => `${character}${character}`)
          .join("")
      : hex;

  if (expanded.length !== 6) {
    return normalized;
  }

  const red = Number.parseInt(expanded.slice(0, 2), 16);
  const green = Number.parseInt(expanded.slice(2, 4), 16);
  const blue = Number.parseInt(expanded.slice(4, 6), 16);

  return `rgba(${red}, ${green}, ${blue}, ${alpha})`;
}
