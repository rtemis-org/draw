import type { A3Data } from "@rtemis/a3";

/**
 * Supported PTM placement strategies for renderer overlays.
 */
export type A3PtmPlacement = "outerRadial" | "innerRadial" | "radial";

/**
 * Shared visual theme values used by all renderers.
 */
export interface A3VisualizationTheme {
  /** @deprecated Use `annotationFontFamily` and `legendFontFamily` instead. */
  readonly fontFamily?: string;
  /** Font family used for residue labels and numeric position annotations. */
  readonly annotationFontFamily: string;
  /** Font family used for titles and legends. */
  readonly legendFontFamily: string;
  /** Background color outside the plot area. */
  readonly backgroundColor: string;
  /** Background color inside the plot area. */
  readonly plotBackgroundColor: string;
  /** Primary residue marker fill. */
  readonly residueFillColor: string;
  /** Primary residue marker stroke and backbone line color. */
  readonly residueStrokeColor: string;
  /** Default residue text color. */
  readonly labelColor: string;
  /** Color used for numeric position labels. */
  readonly positionLabelColor: string;
  /** Legend text color. */
  readonly legendColor: string;
  /** Grid line color for value axes. */
  readonly gridColor: string;
  /** Variant label highlight color. */
  readonly variantColor: string;
  /** Disease-associated variant highlight color. */
  readonly diseaseVariantColor: string;
  /** Colors recycled for region overlays. */
  readonly regionPalette: readonly string[];
  /** Colors recycled for site overlays. */
  readonly sitePalette: readonly string[];
  /** Colors recycled for PTM overlays. */
  readonly ptmPalette: readonly string[];
  /** Colors recycled for processing annotation overlays. */
  readonly processingPalette: readonly string[];
}

/**
 * High-level options shared across all renderers.
 */
export interface A3VisualizationOptions {
  /** Number of residues before the layout wraps. Defaults to `21`. */
  readonly nPerRow?: number;
  /** Horizontal spacing multiplier between consecutive residues. Defaults to `0.3`. */
  readonly residueSpacing?: number;
  /** Whether to render backbone markers and lines. Defaults to `true`. */
  readonly showMarkers?: boolean;
  /** Whether to render single-letter residue labels. Defaults to `true`. */
  readonly showLabels?: boolean;
  /** Frequency for numeric position labels. Set `null` to disable them. */
  readonly positionEvery?: number | null;
  /** Optional chart title. */
  readonly title?: string;
  /** Vertical inset in pixels for the title. */
  readonly titleTop?: number;
  /** Suggested output width in pixels. */
  readonly width?: number;
  /** Suggested output height in pixels. Also controls the effective vertical zoom. */
  readonly height?: number;
  /** Base font size in pixels. */
  readonly fontSize?: number;
  /** Base residue marker size in pixels. */
  readonly markerSize?: number;
  /** Backbone line width in pixels. */
  readonly lineWidth?: number;
  /** Gap in pixels between residue anchors and processing overlays. */
  readonly annotationGap?: number;
  /** PTM placement strategy for renderers that support multiple overlay modes. Defaults to `"radial"`. */
  readonly ptmPlacement?: A3PtmPlacement;
  /** Residue marker fill color. Defaults to `"#181818"`. */
  readonly markerColor?: string;
  /** Opacity used for region annotation bands. Must be between `0` and `1`. */
  readonly regionOpacity?: number;
  /** Gap in pixels between the chart area and the legend rail. Defaults to `90`. */
  readonly legendGap?: number;
  /** Gap in pixels between residue anchors and numeric position labels. */
  readonly positionLabelGap?: number;
  /** Animate the first render and updates. */
  readonly enableAnimation?: boolean;
  /** Initial animation duration in milliseconds for the ECharts renderer. */
  readonly animationDuration?: number;
  /** Update animation duration in milliseconds for the ECharts renderer. */
  readonly animationDurationUpdate?: number;
  /** Optional extra positions to treat as disease-associated variants. */
  readonly diseaseVariants?: readonly number[];
  /** Partial theme overrides. */
  readonly theme?: Partial<A3VisualizationTheme>;
}

/**
 * Single amino acid placement in normalized chart coordinates.
 */
export interface A3ResiduePoint {
  /** One-based residue position. */
  readonly position: number;
  /** One-letter amino acid code. */
  readonly residue: string;
  /** Expanded residue name for hover text. */
  readonly residueName: string;
  /** Layout x coordinate. */
  readonly x: number;
  /** Layout y coordinate. */
  readonly y: number;
}

/**
 * Bounding box of a layout in normalized coordinate space.
 */
export interface A3LayoutBounds {
  readonly minX: number;
  readonly maxX: number;
  readonly minY: number;
  readonly maxY: number;
}

/**
 * Shared geometry model consumed by multiple renderers.
 */
export interface A3LayoutModel {
  /** Validated A3 data object. */
  readonly data: A3Data;
  /** Number of residues per wrapped row. */
  readonly nPerRow: number;
  /** Residue placements in order. */
  readonly residues: readonly A3ResiduePoint[];
  /** Layout bounds for axis configuration. */
  readonly bounds: A3LayoutBounds;
}

/**
 * Minimal ECharts series shape emitted by this package.
 */
export interface A3EChartsSeries {
  readonly type: string;
  readonly name?: string;
  readonly [key: string]: unknown;
}

/**
 * Minimal option object consumed by Apache ECharts.
 */
export interface A3EChartsOption {
  readonly backgroundColor: string;
  readonly animation?: boolean;
  readonly animationDuration?: number;
  readonly animationDurationUpdate?: number;
  readonly title?: Record<string, unknown>;
  readonly grid: Record<string, unknown>;
  readonly xAxis: Record<string, unknown>;
  readonly yAxis: Record<string, unknown>;
  readonly legend: Record<string, unknown>;
  readonly tooltip: Record<string, unknown>;
  readonly dataZoom?: readonly Record<string, unknown>[];
  readonly series: readonly A3EChartsSeries[];
}

/**
 * Inputs accepted by visualization helpers.
 * Accepts validated A3Data (plain object from a3.toData()) or raw input
 * that will be validated via the A3 constructor.
 */
export type A3VisualizationInput = A3Data;
