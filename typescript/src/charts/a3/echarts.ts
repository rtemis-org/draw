import type { A3FlexData } from "@rtemis/a3";
import { A3_THEME_TOKENS, type A3ThemeTokens } from "../../theme/tokens.ts";
import { createA3LayoutModel, resolveA3TitleMarginTop, resolveA3TitleTop } from "./layout.ts";
import { paletteColorAt, withAlpha } from "./palette.ts";
import type {
  A3EChartsOption,
  A3EChartsSeries,
  A3PtmPlacement,
  A3VisualizationInput,
  A3VisualizationOptions,
} from "./types.ts";

// Axis padding in layout coordinate units. Kept large enough to avoid
// clipping the boundary residue circles and the numeric position labels (which
// are drawn above the first row), but far smaller than a full coordinate unit —
// a full unit (the previous value) is ~3 residue spacings wide in x and ~2
// marker heights tall in y, which produced a large empty band above the first
// row and a lopsided left/right gap. Pixel-precise breathing room is instead
// controlled by the grid margins, which the caller can set directly.
const X_AXIS_PADDING = 0.2;
const Y_AXIS_PADDING_TOP = 0.7;
const Y_AXIS_PADDING_BOTTOM = 0.4;

const REGION_LEGEND_ICON = "path://M2 5 H22 V11 H2 Z";
const LEGEND_HEADING_REGIONS = "__legend_heading_regions__";
const LEGEND_HEADING_PTMS = "__legend_heading_ptms__";
const LEGEND_HEADING_PROCESSING = "__legend_heading_processing__";

interface LegendVisualEntry {
  readonly [key: string]: unknown;
  readonly name: string;
  readonly icon?: string;
  readonly itemStyle?: Record<string, unknown>;
}

type AnnotationFamily = "Sites" | "Regions" | "PTMs" | "Processing" | "Variants";

interface AnnotationHit {
  readonly family: AnnotationFamily;
  readonly name: string;
  readonly color: string;
  /** Muted text after the name: a variant's sources. */
  readonly detail?: string;
}

// Fixed display order so a residue's tooltip groups its annotations the same
// way the legend rail does, regardless of which family populated first.
const ANNOTATION_FAMILY_ORDER: readonly AnnotationFamily[] = [
  "Sites",
  "Regions",
  "PTMs",
  "Processing",
  "Variants",
];

/**
 * Builds an Apache ECharts option object using the shared A3 layout geometry.
 */
export function createA3EChartsOption(
  input: A3VisualizationInput,
  options: A3VisualizationOptions = {},
): A3EChartsOption {
  const model = createA3LayoutModel(input, options);
  const theme = options.theme ?? A3_THEME_TOKENS.light;
  const markerSize = options.markerSize ?? 28;
  const fontSize = options.fontSize ?? 18;
  const lineWidth = options.lineWidth ?? 2;
  const showMarkers = options.showMarkers ?? true;
  const showLabels = options.showLabels ?? true;
  const annotationGap = options.annotationGap ?? 16;
  const ptmPlacement = options.ptmPlacement ?? "radial";
  const markerColor = options.markerColor ?? "transparent";
  const regionOpacity = clampOpacity(options.regionOpacity);
  const legendRightInset = 16;
  const legendRailWidth = 196;
  const legendGap = options.legendGap ?? 90;
  const positionLabelGap = options.positionLabelGap ?? 16;
  const enableAnimation = options.enableAnimation ?? true;
  const animationDuration = options.animationDuration ?? 220;
  const animationDurationUpdate = options.animationDurationUpdate ?? 140;
  const titleTop = resolveA3TitleTop(options);
  const titleMarginTop = resolveA3TitleMarginTop(options);
  const series: A3EChartsSeries[] = [];
  // Per-residue annotation index: lets the large residue circle surface every
  // annotation at that position in one tooltip, instead of forcing the user to
  // hover the individual sub-marker symbols (which are only a few pixels wide).
  const annotationsByPosition = new Map<number, AnnotationHit[]>();
  const addAnnotationHit = (position: number, hit: AnnotationHit) => {
    const existing = annotationsByPosition.get(position);
    if (existing) {
      existing.push(hit);
    } else {
      annotationsByPosition.set(position, [hit]);
    }
  };
  const regionLegendEntries: LegendVisualEntry[] = [];
  const siteLegendEntries: LegendVisualEntry[] = [];
  const ptmLegendEntries: LegendVisualEntry[] = [];
  const processingLegendEntries: LegendVisualEntry[] = [];
  const labelColors = buildResidueLabelColors(model, options, theme);

  if (showMarkers) {
    series.push({
      type: "line",
      name: "Primary structure",
      z: 10,
      data: model.residues.map((residue) => ({
        value: [residue.x, residue.y],
        residue: residue.residue,
        residueName: residue.residueName,
        position: residue.position,
      })),
      tooltip: {
        formatter: (params: { data?: Record<string, unknown> }) =>
          formatResidueTooltip(
            params.data?.position as number | undefined,
            params.data?.residueName as string | undefined,
            annotationsByPosition,
          ),
      },
      showSymbol: true,
      symbol: "circle",
      symbolSize: markerSize,
      lineStyle: {
        color: theme.residue_stroke_color,
        width: lineWidth,
      },
      itemStyle: {
        color: markerColor,
        borderColor: theme.residue_stroke_color,
        borderWidth: 1,
      },
    });
  }

  // ── Regions ────────────────────────────────────────────────────────────────
  // Each region entry has { index: [start, end][], type: string }
  for (const [index, [name, entry]] of Object.entries(model.data.annotations.region).entries()) {
    const color = paletteColorAt(theme.region_palette, index);
    const data = [];

    for (const [start, end] of entry.index) {
      for (let position = start; position <= end; position += 1) {
        const residue = model.residues[position - 1];
        if (residue !== undefined) {
          data.push([residue.x, residue.y]);
          addAnnotationHit(position, { family: "Regions", name, color });
        }
      }
      data.push([null, null]);
    }

    series.push({
      type: "line",
      symbol: "circle",
      name,
      z: 15,
      // Purely visual: ignore pointer events so hovering the band falls through
      // to the residue circle beneath, which renders the unified tooltip.
      silent: true,
      data,
      connectNulls: false,
      showSymbol: false,
      itemStyle: {
        color: withAlpha(color, regionOpacity),
      },
      lineStyle: {
        color: withAlpha(color, regionOpacity),
        width: markerSize,
        cap: "round",
        join: "round",
      },
    });
    regionLegendEntries.push({
      name,
      icon: REGION_LEGEND_ICON,
    });
  }

  // ── Sites ──────────────────────────────────────────────────────────────────
  // Each site entry has { index: number[], type: string }
  for (const [index, [name, entry]] of Object.entries(model.data.annotations.site).entries()) {
    const color = paletteColorAt(theme.site_palette, index);
    series.push({
      type: "scatter",
      name,
      z: 20,
      // See region note: silent so the residue circle owns the tooltip.
      silent: true,
      data: entry.index.flatMap((position) => {
        const residue = model.residues[position - 1];
        if (residue === undefined) return [];
        addAnnotationHit(position, { family: "Sites", name, color });
        return [[residue.x, residue.y]];
      }),
      symbol: "circle",
      symbolSize: markerSize,
      itemStyle: {
        color: "rgba(0, 0, 0, 0)",
        borderColor: color,
        borderWidth: 1.5,
      },
    });
    // Built-in "circle" icon + per-item itemStyle gives a hollow ring that is
    // CSP-safe (no data: URI) and respects the site color.
    siteLegendEntries.push({
      name,
      icon: "circle",
      itemStyle: {
        color: "rgba(0,0,0,0)",
        borderColor: color,
        borderWidth: 2,
      },
    });
  }

  // ── PTMs ───────────────────────────────────────────────────────────────────
  // Each PTM entry has { index: number[] | [number, number][], type: string }
  // Extract positions from flex entries (handles both positions and ranges).
  for (const [index, [name, entry]] of Object.entries(model.data.annotations.ptm).entries()) {
    const color = paletteColorAt(theme.ptm_palette, index);
    const positions = extractPositionsFromFlexEntry(entry);
    const ptmSymbolSize = markerSize / 4.5;
    const offset = resolvePtmPixelOffset(
      index,
      Math.max(Object.keys(model.data.annotations.ptm).length, 1),
      ptmPlacement,
      markerSize,
      ptmSymbolSize,
    );

    series.push({
      type: "scatter",
      name,
      z: 30,
      silent: true,
      data: positions.flatMap((position) => {
        const residue = model.residues[position - 1];
        if (residue === undefined) return [];
        addAnnotationHit(position, { family: "PTMs", name, color });
        return [[residue.x, residue.y]];
      }),
      symbol: "circle",
      symbolSize: ptmSymbolSize,
      symbolOffset: [offset.x, offset.y],
      itemStyle: {
        color,
        opacity: 1,
      },
    });
    // No custom icon: ECharts uses the series symbol (filled circle) coloured
    // by the series-level itemStyle.color.
    ptmLegendEntries.push({ name });
  }

  // ── Processing ─────────────────────────────────────────────────────────
  // Each processing entry has { index: number[] | [number, number][], type: string }
  for (const [index, [name, entry]] of Object.entries(
    model.data.annotations.processing,
  ).entries()) {
    const color = paletteColorAt(theme.processing_palette, index);
    const positions = extractPositionsFromFlexEntry(entry);
    const offset = circularPixelOffset(
      index + 1,
      Math.max(Object.keys(model.data.annotations.processing).length, 1),
      annotationGap,
    );

    series.push({
      type: "scatter",
      name,
      z: 31,
      silent: true,
      data: positions.flatMap((position) => {
        const residue = model.residues[position - 1];
        if (residue === undefined) return [];
        addAnnotationHit(position, { family: "Processing", name, color });
        return [[residue.x, residue.y]];
      }),
      symbol: "triangle",
      symbolRotate: 180,
      symbolSize: markerSize / 4,
      symbolOffset: [offset.x, offset.y],
      itemStyle: {
        color,
      },
    });
    // No custom icon: ECharts uses the series symbol (triangle) for the legend icon.
    processingLegendEntries.push({ name });
  }

  // One tooltip row per distinct label at a position, listing the sources of
  // every record with that label, so per-source records of one change read once.
  const variantSourcesByPosition = new Map<number, Map<string, Set<string>>>();
  for (const variant of model.data.annotations.variant) {
    const labels = variantSourcesByPosition.get(variant.position) ?? new Map<string, Set<string>>();
    variantSourcesByPosition.set(variant.position, labels);
    const label = describeVariant(variant);
    const sources = labels.get(label) ?? new Set<string>();
    labels.set(label, sources);
    for (const source of variantSources(variant)) sources.add(source);
  }
  for (const [position, labels] of variantSourcesByPosition) {
    for (const [label, sources] of labels) {
      addAnnotationHit(position, {
        family: "Variants",
        name: label,
        color: theme.variant_color,
        ...(sources.size > 0 ? { detail: [...sources].join(", ") } : {}),
      });
    }
  }

  if (regionLegendEntries.length > 0) {
    series.push(buildLegendHeadingSeries(LEGEND_HEADING_REGIONS));
  }

  if (ptmLegendEntries.length > 0) {
    series.push(buildLegendHeadingSeries(LEGEND_HEADING_PTMS));
  }

  if (processingLegendEntries.length > 0) {
    series.push(buildLegendHeadingSeries(LEGEND_HEADING_PROCESSING));
  }

  if (showLabels) {
    series.push({
      type: "scatter",
      name: "Residue labels",
      z: 40,
      data: model.residues.map((residue, index) => ({
        value: [residue.x, residue.y],
        label: {
          show: true,
          formatter: residue.residue,
          color: labelColors[index],
          fontSize,
        },
      })),
      silent: true,
      symbolSize: 0,
      tooltip: {
        show: false,
      },
    });
  }

  const positionEvery = options.positionEvery === undefined ? 10 : options.positionEvery;
  if (positionEvery !== null && model.data.sequence.length > positionEvery) {
    series.push({
      type: "scatter",
      name: "Position labels",
      z: 41,
      data: model.residues
        .filter((residue) => residue.position % positionEvery === 0)
        .map((residue) => ({
          value: [residue.x, residue.y],
          label: {
            show: true,
            position: "top",
            distance: positionLabelGap,
            formatter: String(residue.position),
            color: theme.position_label_color,
            fontSize: Math.max(10, fontSize - 6),
          },
        })),
      symbolSize: 0,
      tooltip: {
        show: false,
      },
    });
  }

  return Object.freeze({
    animation: enableAnimation,
    animationDuration,
    animationDurationUpdate,
    ...(options.title
      ? {
          title: {
            text: options.title,
            left: "5.5%",
            top: titleTop,
          },
        }
      : {}),
    grid: {
      left: 24,
      right: legendRightInset + legendRailWidth + legendGap,
      top: titleMarginTop,
      bottom: 24,
      containLabel: false,
    },
    xAxis: {
      type: "value",
      min: model.bounds.minX - X_AXIS_PADDING,
      max: model.bounds.maxX + X_AXIS_PADDING,
      show: false,
    },
    yAxis: {
      type: "value",
      min: model.bounds.minY - Y_AXIS_PADDING_TOP,
      max: model.bounds.maxY + Y_AXIS_PADDING_BOTTOM,
      inverse: true,
      show: false,
      scale: true,
    },
    legend: {
      type: "plain",
      orient: "vertical",
      // Align the first legend item with the first sequence row. The y-axis
      // spans [minY - paddingTop, maxY + paddingBottom] over a grid of height
      // markerSize*(2*verticalSpan+1) px, so row 1's center sits
      // paddingTop / (verticalSpan + paddingTop + paddingBottom) down.
      top: (() => {
        const vSpan = model.bounds.maxY - model.bounds.minY;
        const gridH = markerSize * (2 * vSpan + 1);
        const yRange = vSpan + Y_AXIS_PADDING_TOP + Y_AXIS_PADDING_BOTTOM;
        return titleMarginTop + Math.round((gridH * Y_AXIS_PADDING_TOP) / yRange);
      })(),
      right: legendRightInset,
      bottom: 24,
      width: legendRailWidth,
      data: buildLegendData(
        showMarkers,
        siteLegendEntries,
        regionLegendEntries,
        ptmLegendEntries,
        processingLegendEntries,
      ),
      formatter: (name: string) => {
        if (name === LEGEND_HEADING_REGIONS) {
          return "{heading|Regions}";
        }

        if (name === LEGEND_HEADING_PTMS) {
          return "{heading|PTMs}";
        }

        if (name === LEGEND_HEADING_PROCESSING) {
          return "{heading|Processing}";
        }

        return name;
      },
      // The family headings: the legend's text, semibold, set off above.
      textStyle: {
        rich: {
          heading: {
            fontWeight: 600,
            padding: [12, 0, 2, 0],
          },
        },
      },
    },
    tooltip: {
      trigger: "item",
      confine: true,
    },
    series: Object.freeze(series),
  });
}

/**
 * Extracts flat position array from a A3FlexData.
 * A3FlexData.index can be number[] (positions) or [number, number][] (ranges).
 * When ranges, expands each [start, end] into individual positions.
 */
function extractPositionsFromFlexEntry(entry: A3FlexData): number[] {
  const index = entry.index;
  if (index.length === 0) return [];

  // Check if first element is an array → ranges
  if (Array.isArray(index[0])) {
    const ranges = index as [number, number][];
    const positions: number[] = [];
    for (const [start, end] of ranges) {
      for (let pos = start; pos <= end; pos++) {
        positions.push(pos);
      }
    }
    return positions;
  }

  // Plain positions
  return index as number[];
}

function buildResidueLabelColors(
  model: ReturnType<typeof createA3LayoutModel>,
  options: A3VisualizationOptions,
  theme: A3ThemeTokens,
): string[] {
  const diseaseVariants = new Set(options.diseaseVariants ?? []);

  // Look for a site annotation matching "disease_associated_variant" (case-insensitive)
  for (const [key, entry] of Object.entries(model.data.annotations.site)) {
    if (key.toLowerCase().replace(/[-\s]/g, "_") === "disease_associated_variant") {
      for (const position of entry.index) {
        diseaseVariants.add(position);
      }
    }
  }

  const variantPositions = new Set(
    model.data.annotations.variant.map((variant) => variant.position),
  );

  return model.residues.map((residue) => {
    if (diseaseVariants.has(residue.position)) {
      return theme.disease_variant_color;
    }

    if (variantPositions.has(residue.position)) {
      return theme.variant_color;
    }

    return theme.label_color;
  });
}

function circularPixelOffset(
  index: number,
  count: number,
  gap: number,
): { readonly x: number; readonly y: number } {
  const angle = (2 * Math.PI * index) / count;

  return Object.freeze({
    x: Math.sin(angle) * gap,
    y: Math.cos(angle) * gap,
  });
}

function resolvePtmPixelOffset(
  index: number,
  count: number,
  placement: A3PtmPlacement,
  markerSize: number,
  ptmSymbolSize: number,
): { readonly x: number; readonly y: number } {
  const residueRadius = markerSize / 2;
  const ptmRadius = ptmSymbolSize / 2;
  const radialDistance =
    placement === "innerRadial"
      ? Math.max(0, residueRadius - ptmRadius)
      : placement === "radial"
        ? residueRadius
        : residueRadius + ptmRadius;

  return circularPixelOffset(index + 1, count, radialDistance);
}

function clampOpacity(value: number | undefined, fallback = 0.35): number {
  if (value === undefined || Number.isNaN(value)) {
    return fallback;
  }

  return Math.min(1, Math.max(0, value));
}

function buildLegendData(
  includePrimaryStructure: boolean,
  siteLegendEntries: readonly LegendVisualEntry[],
  regionLegendEntries: readonly LegendVisualEntry[],
  ptmLegendEntries: readonly LegendVisualEntry[],
  processingLegendEntries: readonly LegendVisualEntry[],
): readonly (string | Record<string, unknown>)[] {
  const entries: Array<string | Record<string, unknown>> = [];

  if (includePrimaryStructure) {
    entries.push("Primary structure");
  }

  entries.push(...siteLegendEntries);

  if (regionLegendEntries.length > 0) {
    entries.push({ name: LEGEND_HEADING_REGIONS, icon: "none" });
    entries.push(...regionLegendEntries);
  }

  if (ptmLegendEntries.length > 0) {
    entries.push({ name: LEGEND_HEADING_PTMS, icon: "none" });
    entries.push(...ptmLegendEntries);
  }

  if (processingLegendEntries.length > 0) {
    entries.push({ name: LEGEND_HEADING_PROCESSING, icon: "none" });
    entries.push(...processingLegendEntries);
  }

  return entries;
}

function escapeHtml(value: string): string {
  return value.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function describeVariant(variant: { position: number; [key: string]: unknown }): string {
  const wild = variant.from ?? variant.wild_type ?? variant.wildType ?? variant.ref;
  const mutant = variant.to ?? variant.mutant ?? variant.alt ?? variant.variant;
  if (wild !== undefined && variant.type === "deletion") {
    return `${String(wild)}${variant.position}del`;
  }
  if (wild !== undefined && mutant !== undefined) {
    return `${String(wild)}${variant.position}${String(mutant)}`;
  }
  return `Variant @ ${variant.position}`;
}

/** A variant's `source`, a string or a list of strings, as a list. */
function variantSources(variant: { [key: string]: unknown }): string[] {
  const { source } = variant;
  if (typeof source === "string") return [source];
  if (Array.isArray(source)) {
    return source.filter((s): s is string => typeof s === "string");
  }
  return [];
}

function formatResidueTooltip(
  position: number | undefined,
  residueName: string | undefined,
  annotationsByPosition: ReadonlyMap<number, readonly AnnotationHit[]>,
): string {
  const name = escapeHtml(residueName ?? "");
  const header =
    position === undefined
      ? name
      : `<span style="opacity:0.6;font-weight:400">${position}</span> ${name}`;
  const headerHtml = `<div style="font-weight:600">${header}</div>`;
  const wrap = (inner: string) => `<div>${inner}</div>`;

  const hits = position === undefined ? undefined : annotationsByPosition.get(position);
  if (!hits || hits.length === 0) {
    return wrap(headerHtml);
  }

  const rows: string[] = [];
  for (const family of ANNOTATION_FAMILY_ORDER) {
    const familyHits = hits.filter((hit) => hit.family === family);
    if (familyHits.length === 0) {
      continue;
    }
    rows.push(
      `<div style="margin-top:4px;font-size:11px;opacity:0.6;text-transform:uppercase;letter-spacing:0.04em">${family}</div>`,
    );
    for (const hit of familyHits) {
      rows.push(
        `<div style="display:flex;align-items:center;gap:6px">` +
          `<span style="display:inline-block;width:8px;height:8px;border-radius:50%;background:${hit.color}"></span>` +
          `<span>${escapeHtml(hit.name)}</span>` +
          (hit.detail ? `<span style="opacity:0.6">${escapeHtml(hit.detail)}</span>` : "") +
          `</div>`,
      );
    }
  }

  return wrap(headerHtml + rows.join(""));
}

function buildLegendHeadingSeries(name: string): A3EChartsSeries {
  return {
    type: "scatter",
    name,
    data: [],
    symbolSize: 0,
    silent: true,
    tooltip: {
      show: false,
    },
  };
}
