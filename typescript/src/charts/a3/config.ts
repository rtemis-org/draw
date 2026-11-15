import type { A3Data } from "@rtemis/a3";
import { createA3EChartsOption } from "./echarts.ts";
import { createA3LayoutModel, resolveA3VisualizationHeight } from "./layout.ts";
import { DARK_A3_VISUALIZATION_THEME } from "./palette.ts";
import type {
  A3EChartsOption,
  A3PtmPlacement,
  A3VisualizationOptions,
  A3VisualizationTheme,
} from "./types.ts";

/**
 * An instance of the `chart/a3/v1` schema, written by rtemis.draw's `A3Config`
 * class. Every member is optional; an absent member takes the default below.
 * `annotation_gap` and `position_label_gap` are members the R class gains
 * with this package (spec: draw/shared-builders#config-contract).
 */
export interface A3ChartConfig {
  readonly type?: "a3";
  /** Residues per row. Default 21. */
  readonly n_per_row?: number;
  /** Label every nth position; `null` draws no position labels. Default 10. */
  readonly position_every?: number | null;
  /** How modifications are placed relative to the backbone. Default `"radial"`. */
  readonly ptm_placement?: A3PtmPlacement;
  /** Shift+wheel zoom and drag panning along the sequence. Default `true`. */
  readonly zoom?: boolean;
  /** Gap between residues. Default 0.3. */
  readonly residue_spacing?: number;
  /** Residue marker size in pixels. Default 28. */
  readonly marker_size?: number;
  /** Residue label font size in pixels. Default 18. */
  readonly font_size?: number;
  /** Backbone line width in pixels. Default 2. */
  readonly line_width?: number;
  /** Draw residue markers. Default `true`. */
  readonly show_markers?: boolean;
  /** Draw residue labels. Default `true`. */
  readonly show_labels?: boolean;
  /** Gap in pixels between residue anchors and processing overlays. Default 16. */
  readonly annotation_gap?: number;
  /** Gap in pixels between residue anchors and position labels. Default 16. */
  readonly position_label_gap?: number;
  /** Region band opacity in [0, 1]. Default 0.3. */
  readonly region_opacity?: number;
  /** Residue marker fill; `null` selects the theme color. */
  readonly residue_fill?: string | null;
  /** Residue marker outline; `null` selects the theme color. */
  readonly residue_stroke?: string | null;
  /** Residue label color; `null` selects the theme color. */
  readonly label_color?: string | null;
  /** Position label color; `null` selects the theme color. */
  readonly pos_label_color?: string | null;
  /** Variant label color; `null` selects the theme color. */
  readonly variant_color?: string | null;
  /** Disease-associated variant label color; `null` selects the theme color. */
  readonly disease_variant_color?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** Every member of {@link A3ChartConfig}, checked against `chart/a3/v1`. */
export const A3_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "n_per_row",
  "position_every",
  "ptm_placement",
  "zoom",
  "residue_spacing",
  "marker_size",
  "font_size",
  "line_width",
  "show_markers",
  "show_labels",
  "annotation_gap",
  "position_label_gap",
  "region_opacity",
  "residue_fill",
  "residue_stroke",
  "label_color",
  "pos_label_color",
  "variant_color",
  "disease_variant_color",
  "title",
] as const satisfies readonly (keyof A3ChartConfig)[]);

// Fails to compile when A3ChartConfig gains a member the list omits.
type MissingMember = Exclude<keyof A3ChartConfig, (typeof A3_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/**
 * Appearance shared by every chart type, and the light or dark theme. The
 * defaults equal rtemislive's A3 defaults until the appearance contract is
 * reconciled across chart types (spec: draw/shared-builders/notes#inputs-that-are-not-the-chart-config).
 */
export interface A3Appearance {
  /** Select the dark theme. Default `false`. */
  readonly dark?: boolean;
  /** Font family for residue labels, titles, and the legend. */
  readonly fontFamily?: string;
  /** Vertical inset of the title in pixels. */
  readonly titleTop?: number;
  /** Gap in pixels between the plot and the legend rail. Default 90. */
  readonly legendGap?: number;
  /** Animate the first render and updates. Default `true`. */
  readonly animation?: boolean;
}

/**
 * Build the ECharts option of an A3 diagram from an A3 document and its
 * `chart/a3/v1` config.
 */
export function buildA3Option(
  document: A3Data,
  config: A3ChartConfig = {},
  appearance: A3Appearance = {},
): A3EChartsOption {
  const option = createA3EChartsOption(document, visualizationOptions(config, appearance));
  return config.zoom === false ? option : Object.freeze({ ...option, dataZoom: A3_ZOOM });
}

/** The sizes a renderer needs to fit an A3 option to its surface. */
export interface A3SurfaceHints {
  /** Chart height in pixels that keeps the row spacing of the layout. */
  readonly height: number;
  /** Height in pixels of the plot body, from the first row to the last. */
  readonly bodyHeight: number;
  /** Residue marker size in pixels. */
  readonly markerSize: number;
  /** Residue label font size in pixels. */
  readonly fontSize: number;
  /** Gap between residues. */
  readonly residueSpacing: number;
  /** Gap in pixels between the plot and the legend rail. */
  readonly legendGap: number;
}

/**
 * The height and glyph sizes of an A3 diagram, for a renderer that sizes its
 * surface before drawing (rtemislive's chart container, rtemis.draw's widget
 * and SVG export).
 */
export function a3SurfaceHints(
  document: A3Data,
  config: A3ChartConfig = {},
  appearance: A3Appearance = {},
): A3SurfaceHints {
  const options = visualizationOptions(config, appearance);
  const model = createA3LayoutModel(document, options);
  const markerSize = options.markerSize ?? 28;
  return Object.freeze({
    height: resolveA3VisualizationHeight(document, options),
    bodyHeight: markerSize * (2 * (model.bounds.maxY - model.bounds.minY) + 1),
    markerSize,
    fontSize: options.fontSize ?? 18,
    residueSpacing: options.residueSpacing ?? 0.3,
    legendGap: options.legendGap ?? 90,
  });
}

function visualizationOptions(
  config: A3ChartConfig,
  appearance: A3Appearance,
): A3VisualizationOptions {
  const dark = appearance.dark ?? false;
  const base: Partial<A3VisualizationTheme> = dark ? DARK_A3_VISUALIZATION_THEME : {};
  const theme: Partial<A3VisualizationTheme> = {
    ...base,
    ...(appearance.fontFamily
      ? {
          annotationFontFamily: appearance.fontFamily,
          legendFontFamily: appearance.fontFamily,
        }
      : {}),
    ...colorOverride("residueStrokeColor", config.residue_stroke),
    ...colorOverride("labelColor", config.label_color),
    ...colorOverride("positionLabelColor", config.pos_label_color),
    ...colorOverride("variantColor", config.variant_color),
    ...colorOverride("diseaseVariantColor", config.disease_variant_color),
  };
  return {
    nPerRow: config.n_per_row ?? 21,
    residueSpacing: config.residue_spacing ?? 0.3,
    showMarkers: config.show_markers ?? true,
    showLabels: config.show_labels ?? true,
    positionEvery: config.position_every === undefined ? 10 : config.position_every,
    markerSize: config.marker_size ?? 28,
    fontSize: config.font_size ?? 18,
    lineWidth: config.line_width ?? 2,
    annotationGap: config.annotation_gap ?? 16,
    positionLabelGap: config.position_label_gap ?? 16,
    markerColor: config.residue_fill ?? (dark ? "#181818" : "#efefef"),
    regionOpacity: config.region_opacity ?? 0.3,
    ptmPlacement: config.ptm_placement ?? "radial",
    legendGap: appearance.legendGap ?? 90,
    enableAnimation: appearance.animation ?? true,
    theme,
    ...(config.title ? { title: config.title } : {}),
    ...(appearance.titleTop === undefined ? {} : { titleTop: appearance.titleTop }),
  };
}

// Shift+wheel zooms the sequence axis and drag pans it, so the plain wheel
// scrolls the host page (spec: draw/zoom-controls#design).
const A3_ZOOM: readonly Record<string, unknown>[] = Object.freeze([
  {
    type: "inside",
    xAxisIndex: 0,
    zoomOnMouseWheel: "shift",
    moveOnMouseMove: true,
    moveOnMouseWheel: true,
    preventDefaultMouseMove: false,
  },
  {
    type: "inside",
    yAxisIndex: 0,
    zoomOnMouseWheel: false,
    moveOnMouseWheel: false,
    moveOnMouseMove: false,
  },
]);

function colorOverride(
  key: keyof A3VisualizationTheme,
  value: string | null | undefined,
): Partial<A3VisualizationTheme> {
  return value ? { [key]: value } : {};
}
