"use strict";
var RtemisDraw = (() => {
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, { get: all[name], enumerable: true });
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

  // src/index.ts
  var index_exports = {};
  __export(index_exports, {
    A3_CHART_CONFIG_MEMBERS: () => A3_CHART_CONFIG_MEMBERS,
    BAR_CHART_CONFIG_MEMBERS: () => BAR_CHART_CONFIG_MEMBERS,
    BOXPLOT_CHART_CONFIG_MEMBERS: () => BOXPLOT_CHART_CONFIG_MEMBERS,
    CONFUSION_CHART_CONFIG_MEMBERS: () => CONFUSION_CHART_CONFIG_MEMBERS,
    DEFAULT_FONT_FAMILY: () => DEFAULT_FONT_FAMILY,
    DEFAULT_MARGIN: () => DEFAULT_MARGIN,
    DEFAULT_PAD: () => DEFAULT_PAD,
    DEFAULT_POINT_SIZE: () => DEFAULT_POINT_SIZE,
    DENSITY_CHART_CONFIG_MEMBERS: () => DENSITY_CHART_CONFIG_MEMBERS,
    DENSITY_VALUE_LABEL: () => DENSITY_VALUE_LABEL,
    FIT_MEMBERS: () => FIT_MEMBERS,
    GANTT_CHART_CONFIG_MEMBERS: () => GANTT_CHART_CONFIG_MEMBERS,
    GANTT_ROW_PX: () => GANTT_ROW_PX,
    HISTOGRAM_CHART_CONFIG_MEMBERS: () => HISTOGRAM_CHART_CONFIG_MEMBERS,
    LINE_CHART_CONFIG_MEMBERS: () => LINE_CHART_CONFIG_MEMBERS,
    MAX_SYMBOL_SIZE: () => MAX_SYMBOL_SIZE,
    PR_CHART_CONFIG_MEMBERS: () => PR_CHART_CONFIG_MEMBERS,
    RENDERER_NAMES: () => RENDERER_NAMES,
    ROC_CHART_CONFIG_MEMBERS: () => ROC_CHART_CONFIG_MEMBERS,
    RTEMIS_PALETTE: () => RTEMIS_PALETTE,
    SANKEY_CHART_CONFIG_MEMBERS: () => SANKEY_CHART_CONFIG_MEMBERS,
    SCATTER_CHART_CONFIG_MEMBERS: () => SCATTER_CHART_CONFIG_MEMBERS,
    SIGNIFICANCE_CHART_CONFIG_MEMBERS: () => SIGNIFICANCE_CHART_CONFIG_MEMBERS,
    SIGNIFICANT_DIGITS: () => SIGNIFICANT_DIGITS,
    SIGN_COLORS: () => SIGN_COLORS,
    SIZE_KEY_ID: () => SIZE_KEY_ID,
    SURVIVAL_CHART_CONFIG_MEMBERS: () => SURVIVAL_CHART_CONFIG_MEMBERS,
    TIMESERIES_CHART_CONFIG_MEMBERS: () => TIMESERIES_CHART_CONFIG_MEMBERS,
    ZOOM_SLIDER_ID: () => ZOOM_SLIDER_ID,
    a3SurfaceHints: () => a3SurfaceHints,
    appendLayers: () => appendLayers,
    applyRepelLabels: () => applyRepelLabels,
    aspectHints: () => aspectHints,
    autoAlpha: () => autoAlpha,
    boxplotStats: () => boxplotStats,
    buildA3Option: () => buildA3Option,
    buildBarOption: () => buildBarOption,
    buildBoxplotOption: () => buildBoxplotOption,
    buildConfusionOption: () => buildConfusionOption,
    buildDensityOption: () => buildDensityOption,
    buildGanttOption: () => buildGanttOption,
    buildHistogramOption: () => buildHistogramOption,
    buildLineOption: () => buildLineOption,
    buildPrOption: () => buildPrOption,
    buildRocOption: () => buildRocOption,
    buildSankeyOption: () => buildSankeyOption,
    buildScatterOption: () => buildScatterOption,
    buildSignificanceOption: () => buildSignificanceOption,
    buildSurvivalOption: () => buildSurvivalOption,
    buildTimeSeriesOption: () => buildTimeSeriesOption,
    builders: () => builders,
    calcLimits: () => calcLimits,
    calcLimitsFromZero: () => calcLimitsFromZero,
    classCount: () => classCount,
    confusionHeight: () => confusionHeight,
    confusionLayout: () => confusionLayout,
    confusionRates: () => confusionRates,
    curveAspect: () => curveAspect,
    curveLabel: () => curveLabel,
    curveLegendTitle: () => curveLegendTitle,
    darkThemeTokens: () => darkThemeTokens,
    ddSci: () => ddSci,
    fitConfusion: () => fitConfusion,
    fitKeys: () => fitKeys,
    fitLabel: () => fitLabel,
    fitLegend: () => fitLegend,
    fontStack: () => fontStack,
    formatNumber: () => formatNumber,
    ganttHeight: () => ganttHeight,
    histogramEdges: () => histogramEdges,
    histogramValueLabel: () => histogramValueLabel,
    kernelDensity: () => kernelDensity,
    keyPlacement: () => keyPlacement,
    keyState: () => keyState,
    layoutSurvivalRisk: () => layoutSurvivalRisk,
    lightThemeTokens: () => lightThemeTokens,
    lineAspect: () => lineAspect,
    pretty: () => pretty,
    radicalInverse: () => radicalInverse,
    registerRenderers: () => registerRenderers,
    repelLabels: () => repelLabels,
    rollingValues: () => rollingValues,
    ruleBandwidth: () => ruleBandwidth,
    sankeyLinks: () => sankeyLinks,
    scatterAspect: () => lineAspect,
    significanceAxisNames: () => significanceAxisNames,
    significanceGroupNames: () => significanceGroupNames,
    sizeKeyValues: () => sizeKeyValues,
    survivalMedian: () => survivalMedian,
    themeFontSizes: () => themeFontSizes,
    themeOption: () => themeOption,
    timeLabel: () => timeLabel,
    timeMs: () => timeMs,
    valueLabel: () => valueLabel
  });

  // src/charts/a3/layout.ts
  var AMINO_ACID_NAMES = Object.freeze({
    A: "Alanine",
    R: "Arginine",
    N: "Asparagine",
    D: "Aspartate",
    C: "Cysteine",
    Q: "Glutamine",
    E: "Glutamate",
    G: "Glycine",
    H: "Histidine",
    I: "Isoleucine",
    L: "Leucine",
    K: "Lysine",
    M: "Methionine",
    F: "Phenylalanine",
    P: "Proline",
    S: "Serine",
    T: "Threonine",
    W: "Tryptophan",
    Y: "Tyrosine",
    V: "Valine",
    B: "Aspartic acid or Asparagine",
    Z: "Glutamine or Glutamic acid",
    X: "(Any)",
    "*": "Termination codon"
  });
  var DEFAULT_MARKER_SIZE = 28;
  var DEFAULT_FONT_SIZE = 18;
  var DEFAULT_MARGIN_TOP = 24;
  var DEFAULT_MARGIN_TOP_WITH_TITLE = 64;
  var DEFAULT_MARGIN_BOTTOM = 24;
  var DEFAULT_TITLE_TOP = 32;
  var DEFAULT_TITLE_BOTTOM_GAP = 14;
  var DEFAULT_N_PER_ROW = 21;
  var DEFAULT_RESIDUE_SPACING = 0.3;
  function createA3LayoutModel(input, options = {}) {
    const data = input;
    const sequenceLength = data.sequence.length;
    const nPerRow = normalizeNPerRow(options.nPerRow, sequenceLength);
    const residueSpacing = normalizeResidueSpacing(options.residueSpacing);
    const coordinates = createSequenceCoordinates(sequenceLength, nPerRow);
    const residues = Array.from(data.sequence).map(
      (residue, index) => Object.freeze({
        position: index + 1,
        residue,
        residueName: AMINO_ACID_NAMES[residue] ?? residue,
        x: scaleResidueXCoordinate(coordinates.xs[index] ?? 0, residueSpacing),
        y: coordinates.ys[index] ?? 0
      })
    );
    return Object.freeze({
      data,
      nPerRow,
      residues: Object.freeze(residues),
      bounds: computeBounds(residues)
    });
  }
  function resolveA3VisualizationHeight(input, options = {}) {
    const model = createA3LayoutModel(input, options);
    return resolveA3VisualizationHeightFromModel(model, options);
  }
  function resolveA3TitleTop(options = {}) {
    return options.titleTop ?? DEFAULT_TITLE_TOP;
  }
  function resolveA3TitleMarginTop(options = {}) {
    if (options.title === void 0) {
      return DEFAULT_MARGIN_TOP;
    }
    const fontSize = options.fontSize ?? DEFAULT_FONT_SIZE;
    return Math.max(
      DEFAULT_MARGIN_TOP_WITH_TITLE,
      resolveA3TitleTop(options) + fontSize + DEFAULT_TITLE_BOTTOM_GAP
    );
  }
  function createSequenceCoordinates(length, nPerRow) {
    if (!Number.isInteger(length) || length <= 0) {
      throw new RangeError("A3 layout length must be a positive integer.");
    }
    const xs = [];
    const ys = [];
    const xPattern = [...range(1, nPerRow), ...range(nPerRow - 1, 2)];
    const yPattern = [1];
    const nRows = Math.ceil((length - 1) / (nPerRow - 1));
    const repeated = repeatEach(stepSequence(1, nRows, 1), nPerRow - 1, Math.max(0, length - 1));
    yPattern.push(...repeated);
    for (let index = 0; index < length; index += 1) {
      xs.push(xPattern[index % xPattern.length] ?? 1);
      ys.push(yPattern[index] ?? 1);
    }
    const period = 2 * nPerRow - 2;
    const borderOffset = 1 - Math.sqrt(3) / 2;
    for (let index = nPerRow - 1; index < length; index += period) {
      ys[index] = (ys[index] ?? 0) + 0.5;
      xs[index] = (xs[index] ?? 0) - borderOffset;
    }
    for (let index = 2 * nPerRow - 2; index < length; index += period) {
      ys[index] = (ys[index] ?? 0) + 0.5;
      xs[index] = (xs[index] ?? 0) + borderOffset;
    }
    return Object.freeze({ xs: Object.freeze(xs), ys: Object.freeze(ys) });
  }
  function normalizeNPerRow(value, _length) {
    const resolved = value ?? DEFAULT_N_PER_ROW;
    if (!Number.isInteger(resolved) || resolved <= 1) {
      throw new RangeError("A3 visualization nPerRow must be an integer > 1.");
    }
    return resolved;
  }
  function normalizeResidueSpacing(value) {
    const resolved = value ?? DEFAULT_RESIDUE_SPACING;
    if (!Number.isFinite(resolved) || resolved <= 0) {
      throw new RangeError("A3 visualization residueSpacing must be > 0.");
    }
    return resolved;
  }
  function scaleResidueXCoordinate(x, residueSpacing) {
    if (residueSpacing === 1) {
      return x;
    }
    return 1 + (x - 1) * residueSpacing;
  }
  function resolveA3VisualizationHeightFromModel(model, options = {}) {
    if (options.height !== void 0) {
      return options.height;
    }
    const markerSize = options.markerSize ?? DEFAULT_MARKER_SIZE;
    const topMargin = resolveA3TitleMarginTop(options);
    const bottomMargin = DEFAULT_MARGIN_BOTTOM;
    const verticalSpan = model.bounds.maxY - model.bounds.minY;
    const bodyHeight = markerSize * (2 * verticalSpan + 1);
    return Math.ceil(topMargin + bottomMargin + bodyHeight);
  }
  function computeBounds(residues) {
    const xs = residues.map((residue) => residue.x);
    const ys = residues.map((residue) => residue.y);
    return Object.freeze({
      minX: Math.min(...xs),
      maxX: Math.max(...xs),
      minY: Math.min(...ys),
      maxY: Math.max(...ys)
    });
  }
  function range(start, end) {
    if (start <= end) {
      return Array.from({ length: end - start + 1 }, (_, index) => start + index);
    }
    return Array.from({ length: start - end + 1 }, (_, index) => start - index);
  }
  function stepSequence(start, end, step) {
    const values = [];
    for (let value = start; value <= end; value += step) {
      values.push(value);
    }
    return values;
  }
  function repeatEach(values, repeatCount, outputLength) {
    const output = [];
    for (const value of values) {
      for (let index = 0; index < repeatCount && output.length < outputLength; index += 1) {
        output.push(value);
      }
    }
    return output;
  }

  // src/charts/a3/palette.ts
  var DEFAULT_REGION_PALETTE = Object.freeze([
    "#0F766E",
    "#2563EB",
    "#D97706",
    "#7C3AED",
    "#DC2626",
    "#0891B2"
  ]);
  var DEFAULT_SITE_PALETTE = Object.freeze([
    "#0EA5E9",
    "#14B8A6",
    "#E11D48",
    "#A855F7",
    "#F97316",
    "#16A34A"
  ]);
  var DEFAULT_PTM_PALETTE = Object.freeze([
    "#0284C7",
    "#F59E0B",
    "#10B981",
    "#8B5CF6",
    "#EF4444",
    "#14B8A6"
  ]);
  var DEFAULT_PROCESSING_PALETTE = Object.freeze([
    "#D97706",
    "#DC2626",
    "#0EA5E9",
    "#9333EA",
    "#16A34A",
    "#DB2777"
  ]);
  var DEFAULT_GEIST_FONT_STACK = "Geist, 'Geist Sans', 'Inter', 'Avenir Next', 'Segoe UI', sans-serif";
  var DEFAULT_A3_VISUALIZATION_THEME = Object.freeze({
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
    processingPalette: DEFAULT_PROCESSING_PALETTE
  });
  var DARK_A3_VISUALIZATION_THEME = Object.freeze({
    fontFamily: DEFAULT_GEIST_FONT_STACK,
    annotationFontFamily: DEFAULT_GEIST_FONT_STACK,
    legendFontFamily: DEFAULT_GEIST_FONT_STACK,
    backgroundColor: "transparent",
    plotBackgroundColor: "transparent",
    residueFillColor: "#3f3f46",
    // zinc-700
    residueStrokeColor: "#a1a1aa",
    // zinc-400
    labelColor: "#fafafa",
    // zinc-50
    positionLabelColor: "#a1a1aa",
    // zinc-400
    legendColor: "#e4e4e7",
    // zinc-200
    gridColor: "#3f3f46",
    // zinc-700
    variantColor: "#fb923c",
    // orange-400 (brighter for dark bg)
    diseaseVariantColor: "#f472b6",
    // pink-400 (brighter for dark bg)
    regionPalette: DEFAULT_REGION_PALETTE,
    sitePalette: DEFAULT_SITE_PALETTE,
    ptmPalette: DEFAULT_PTM_PALETTE,
    processingPalette: DEFAULT_PROCESSING_PALETTE
  });
  function resolveA3VisualizationTheme(overrides = {}) {
    const fallbackFontFamily = overrides.fontFamily ?? DEFAULT_A3_VISUALIZATION_THEME.annotationFontFamily;
    return Object.freeze({
      ...DEFAULT_A3_VISUALIZATION_THEME,
      ...overrides,
      fontFamily: fallbackFontFamily,
      annotationFontFamily: overrides.annotationFontFamily ?? fallbackFontFamily,
      legendFontFamily: overrides.legendFontFamily ?? fallbackFontFamily,
      regionPalette: overrides.regionPalette ?? DEFAULT_REGION_PALETTE,
      sitePalette: overrides.sitePalette ?? DEFAULT_SITE_PALETTE,
      ptmPalette: overrides.ptmPalette ?? DEFAULT_PTM_PALETTE,
      processingPalette: overrides.processingPalette ?? DEFAULT_PROCESSING_PALETTE
    });
  }
  function paletteColorAt(palette, index) {
    if (palette.length === 0) {
      throw new RangeError("A3 visualization palettes cannot be empty.");
    }
    return palette[index % palette.length] ?? palette[0] ?? "#000000";
  }
  function withAlpha(color, alpha) {
    const normalized = color.trim();
    if (!normalized.startsWith("#")) {
      return normalized;
    }
    const hex = normalized.slice(1);
    const expanded = hex.length === 3 ? hex.split("").map((character) => `${character}${character}`).join("") : hex;
    if (expanded.length !== 6) {
      return normalized;
    }
    const red = Number.parseInt(expanded.slice(0, 2), 16);
    const green = Number.parseInt(expanded.slice(2, 4), 16);
    const blue = Number.parseInt(expanded.slice(4, 6), 16);
    return `rgba(${red}, ${green}, ${blue}, ${alpha})`;
  }

  // src/charts/a3/echarts.ts
  var X_AXIS_PADDING = 0.2;
  var Y_AXIS_PADDING_TOP = 0.7;
  var Y_AXIS_PADDING_BOTTOM = 0.4;
  var REGION_LEGEND_ICON = "path://M2 5 H22 V11 H2 Z";
  var LEGEND_HEADING_REGIONS = "__legend_heading_regions__";
  var LEGEND_HEADING_PTMS = "__legend_heading_ptms__";
  var LEGEND_HEADING_PROCESSING = "__legend_heading_processing__";
  var ANNOTATION_FAMILY_ORDER = [
    "Sites",
    "Regions",
    "PTMs",
    "Processing",
    "Variants"
  ];
  function createA3EChartsOption(input, options = {}) {
    const model = createA3LayoutModel(input, options);
    const theme = resolveA3VisualizationTheme(options.theme);
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
    const series = [];
    const annotationsByPosition = /* @__PURE__ */ new Map();
    const addAnnotationHit = (position, hit) => {
      const existing = annotationsByPosition.get(position);
      if (existing) {
        existing.push(hit);
      } else {
        annotationsByPosition.set(position, [hit]);
      }
    };
    const regionLegendEntries = [];
    const siteLegendEntries = [];
    const ptmLegendEntries = [];
    const processingLegendEntries = [];
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
          position: residue.position
        })),
        tooltip: {
          formatter: (params) => formatResidueTooltip(
            params.data?.position,
            params.data?.residueName,
            annotationsByPosition,
            theme
          )
        },
        showSymbol: true,
        symbol: "circle",
        symbolSize: markerSize,
        lineStyle: {
          color: theme.residueStrokeColor,
          width: lineWidth
        },
        itemStyle: {
          color: markerColor,
          borderColor: theme.residueStrokeColor,
          borderWidth: 1
        }
      });
    }
    for (const [index, [name, entry]] of Object.entries(model.data.annotations.region).entries()) {
      const color = paletteColorAt(theme.regionPalette, index);
      const data = [];
      for (const [start, end] of entry.index) {
        for (let position = start; position <= end; position += 1) {
          const residue = model.residues[position - 1];
          if (residue !== void 0) {
            data.push([residue.x, residue.y]);
            addAnnotationHit(position, { family: "Regions", name, color });
          }
        }
        data.push([null, null]);
      }
      series.push({
        type: "line",
        name,
        z: 15,
        // Purely visual: ignore pointer events so hovering the band falls through
        // to the residue circle beneath, which renders the unified tooltip.
        silent: true,
        data,
        connectNulls: false,
        showSymbol: false,
        itemStyle: {
          color: withAlpha(color, regionOpacity)
        },
        lineStyle: {
          color: withAlpha(color, regionOpacity),
          width: markerSize,
          cap: "round",
          join: "round"
        }
      });
      regionLegendEntries.push({
        name,
        icon: REGION_LEGEND_ICON
      });
    }
    for (const [index, [name, entry]] of Object.entries(model.data.annotations.site).entries()) {
      const color = paletteColorAt(theme.sitePalette, index);
      series.push({
        type: "scatter",
        name,
        z: 20,
        // See region note: silent so the residue circle owns the tooltip.
        silent: true,
        data: entry.index.flatMap((position) => {
          const residue = model.residues[position - 1];
          if (residue === void 0) return [];
          addAnnotationHit(position, { family: "Sites", name, color });
          return [[residue.x, residue.y]];
        }),
        symbol: "circle",
        symbolSize: markerSize,
        itemStyle: {
          color: "rgba(0, 0, 0, 0)",
          borderColor: color,
          borderWidth: 1.5
        }
      });
      siteLegendEntries.push({
        name,
        icon: "circle",
        itemStyle: {
          color: "rgba(0,0,0,0)",
          borderColor: color,
          borderWidth: 2
        }
      });
    }
    for (const [index, [name, entry]] of Object.entries(model.data.annotations.ptm).entries()) {
      const color = paletteColorAt(theme.ptmPalette, index);
      const positions = extractPositionsFromFlexEntry(entry);
      const ptmSymbolSize = markerSize / 4.5;
      const offset = resolvePtmPixelOffset(
        index,
        Math.max(Object.keys(model.data.annotations.ptm).length, 1),
        ptmPlacement,
        markerSize,
        ptmSymbolSize
      );
      series.push({
        type: "scatter",
        name,
        z: 30,
        silent: true,
        data: positions.flatMap((position) => {
          const residue = model.residues[position - 1];
          if (residue === void 0) return [];
          addAnnotationHit(position, { family: "PTMs", name, color });
          return [[residue.x, residue.y]];
        }),
        symbol: "circle",
        symbolSize: ptmSymbolSize,
        symbolOffset: [offset.x, offset.y],
        itemStyle: {
          color,
          opacity: 1
        }
      });
      ptmLegendEntries.push({ name });
    }
    for (const [index, [name, entry]] of Object.entries(
      model.data.annotations.processing
    ).entries()) {
      const color = paletteColorAt(theme.processingPalette, index);
      const positions = extractPositionsFromFlexEntry(entry);
      const offset = circularPixelOffset(
        index + 1,
        Math.max(Object.keys(model.data.annotations.processing).length, 1),
        annotationGap
      );
      series.push({
        type: "scatter",
        name,
        z: 31,
        silent: true,
        data: positions.flatMap((position) => {
          const residue = model.residues[position - 1];
          if (residue === void 0) return [];
          addAnnotationHit(position, { family: "Processing", name, color });
          return [[residue.x, residue.y]];
        }),
        symbol: "triangle",
        symbolRotate: 180,
        symbolSize: markerSize / 4,
        symbolOffset: [offset.x, offset.y],
        itemStyle: {
          color
        }
      });
      processingLegendEntries.push({ name });
    }
    const variantSourcesByPosition = /* @__PURE__ */ new Map();
    for (const variant of model.data.annotations.variant) {
      const labels = variantSourcesByPosition.get(variant.position) ?? /* @__PURE__ */ new Map();
      variantSourcesByPosition.set(variant.position, labels);
      const label = describeVariant(variant);
      const sources = labels.get(label) ?? /* @__PURE__ */ new Set();
      labels.set(label, sources);
      for (const source of variantSources(variant)) sources.add(source);
    }
    for (const [position, labels] of variantSourcesByPosition) {
      for (const [label, sources] of labels) {
        addAnnotationHit(position, {
          family: "Variants",
          name: label,
          color: theme.variantColor,
          ...sources.size > 0 ? { detail: [...sources].join(", ") } : {}
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
            fontFamily: theme.annotationFontFamily,
            fontSize
          }
        })),
        silent: true,
        symbolSize: 0,
        tooltip: {
          show: false
        }
      });
    }
    const positionEvery = options.positionEvery === void 0 ? 10 : options.positionEvery;
    if (positionEvery !== null && model.data.sequence.length > positionEvery) {
      series.push({
        type: "scatter",
        name: "Position labels",
        z: 41,
        data: model.residues.filter((residue) => residue.position % positionEvery === 0).map((residue) => ({
          value: [residue.x, residue.y],
          label: {
            show: true,
            position: "top",
            distance: positionLabelGap,
            formatter: String(residue.position),
            color: theme.positionLabelColor,
            fontFamily: theme.annotationFontFamily,
            fontSize: Math.max(10, fontSize - 6)
          }
        })),
        symbolSize: 0,
        tooltip: {
          show: false
        }
      });
    }
    return Object.freeze({
      backgroundColor: theme.backgroundColor,
      animation: enableAnimation,
      animationDuration,
      animationDurationUpdate,
      ...options.title ? {
        title: {
          text: options.title,
          left: "5.5%",
          top: titleTop,
          textStyle: {
            fontFamily: theme.legendFontFamily,
            fontSize,
            color: theme.legendColor
          }
        }
      } : {},
      grid: {
        left: 24,
        right: legendRightInset + legendRailWidth + legendGap,
        top: titleMarginTop,
        bottom: 24,
        containLabel: false
      },
      xAxis: {
        type: "value",
        min: model.bounds.minX - X_AXIS_PADDING,
        max: model.bounds.maxX + X_AXIS_PADDING,
        show: false
      },
      yAxis: {
        type: "value",
        min: model.bounds.minY - Y_AXIS_PADDING_TOP,
        max: model.bounds.maxY + Y_AXIS_PADDING_BOTTOM,
        inverse: true,
        show: false,
        scale: true
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
          return titleMarginTop + Math.round(gridH * Y_AXIS_PADDING_TOP / yRange);
        })(),
        right: legendRightInset,
        bottom: 24,
        width: legendRailWidth,
        data: buildLegendData(
          showMarkers,
          siteLegendEntries,
          regionLegendEntries,
          ptmLegendEntries,
          processingLegendEntries
        ),
        formatter: (name) => {
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
        textStyle: {
          color: theme.legendColor,
          fontFamily: theme.legendFontFamily,
          fontSize,
          rich: {
            heading: {
              color: theme.legendColor,
              fontFamily: theme.legendFontFamily,
              fontSize: Math.max(12, fontSize - 1),
              fontWeight: 600,
              padding: [12, 0, 2, 0]
            }
          }
        }
      },
      tooltip: {
        trigger: "item",
        confine: true
      },
      series: Object.freeze(series)
    });
  }
  function extractPositionsFromFlexEntry(entry) {
    const index = entry.index;
    if (index.length === 0) return [];
    if (Array.isArray(index[0])) {
      const ranges = index;
      const positions = [];
      for (const [start, end] of ranges) {
        for (let pos = start; pos <= end; pos++) {
          positions.push(pos);
        }
      }
      return positions;
    }
    return index;
  }
  function buildResidueLabelColors(model, options, theme) {
    const diseaseVariants = new Set(options.diseaseVariants ?? []);
    for (const [key, entry] of Object.entries(model.data.annotations.site)) {
      if (key.toLowerCase().replace(/[-\s]/g, "_") === "disease_associated_variant") {
        for (const position of entry.index) {
          diseaseVariants.add(position);
        }
      }
    }
    const variantPositions = new Set(
      model.data.annotations.variant.map((variant) => variant.position)
    );
    return model.residues.map((residue) => {
      if (diseaseVariants.has(residue.position)) {
        return theme.diseaseVariantColor;
      }
      if (variantPositions.has(residue.position)) {
        return theme.variantColor;
      }
      return theme.labelColor;
    });
  }
  function circularPixelOffset(index, count, gap) {
    const angle = 2 * Math.PI * index / count;
    return Object.freeze({
      x: Math.sin(angle) * gap,
      y: Math.cos(angle) * gap
    });
  }
  function resolvePtmPixelOffset(index, count, placement, markerSize, ptmSymbolSize) {
    const residueRadius = markerSize / 2;
    const ptmRadius = ptmSymbolSize / 2;
    const radialDistance = placement === "innerRadial" ? Math.max(0, residueRadius - ptmRadius) : placement === "radial" ? residueRadius : residueRadius + ptmRadius;
    return circularPixelOffset(index + 1, count, radialDistance);
  }
  function clampOpacity(value, fallback = 0.35) {
    if (value === void 0 || Number.isNaN(value)) {
      return fallback;
    }
    return Math.min(1, Math.max(0, value));
  }
  function buildLegendData(includePrimaryStructure, siteLegendEntries, regionLegendEntries, ptmLegendEntries, processingLegendEntries) {
    const entries = [];
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
  function escapeHtml(value) {
    return value.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }
  function describeVariant(variant) {
    const wild = variant.from ?? variant.wild_type ?? variant.wildType ?? variant.ref;
    const mutant = variant.to ?? variant.mutant ?? variant.alt ?? variant.variant;
    if (wild !== void 0 && variant.type === "deletion") {
      return `${String(wild)}${variant.position}del`;
    }
    if (wild !== void 0 && mutant !== void 0) {
      return `${String(wild)}${variant.position}${String(mutant)}`;
    }
    return `Variant @ ${variant.position}`;
  }
  function variantSources(variant) {
    const { source } = variant;
    if (typeof source === "string") return [source];
    if (Array.isArray(source)) {
      return source.filter((s) => typeof s === "string");
    }
    return [];
  }
  function formatResidueTooltip(position, residueName, annotationsByPosition, theme) {
    const name = escapeHtml(residueName ?? "");
    const header = position === void 0 ? name : `<span style="opacity:0.6;font-weight:400">${position}</span> ${name}`;
    const headerHtml = `<div style="font-weight:600">${header}</div>`;
    const wrap = (inner) => `<div style="color:${theme.legendColor};font-family:${theme.legendFontFamily}">${inner}</div>`;
    const hits = position === void 0 ? void 0 : annotationsByPosition.get(position);
    if (!hits || hits.length === 0) {
      return wrap(headerHtml);
    }
    const rows = [];
    for (const family of ANNOTATION_FAMILY_ORDER) {
      const familyHits = hits.filter((hit) => hit.family === family);
      if (familyHits.length === 0) {
        continue;
      }
      rows.push(
        `<div style="margin-top:4px;font-size:11px;opacity:0.6;text-transform:uppercase;letter-spacing:0.04em">${family}</div>`
      );
      for (const hit of familyHits) {
        rows.push(
          `<div style="display:flex;align-items:center;gap:6px"><span style="display:inline-block;width:8px;height:8px;border-radius:50%;background:${hit.color}"></span><span>${escapeHtml(hit.name)}</span>` + (hit.detail ? `<span style="opacity:0.6">${escapeHtml(hit.detail)}</span>` : "") + `</div>`
        );
      }
    }
    return wrap(headerHtml + rows.join(""));
  }
  function buildLegendHeadingSeries(name) {
    return {
      type: "scatter",
      name,
      data: [],
      symbolSize: 0,
      silent: true,
      tooltip: {
        show: false
      }
    };
  }

  // src/charts/a3/config.ts
  var A3_CHART_CONFIG_MEMBERS = Object.freeze([
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
    "title"
  ]);
  function buildA3Option(document, config = {}, appearance = {}) {
    const option = createA3EChartsOption(document, visualizationOptions(config, appearance));
    return config.zoom === false ? option : Object.freeze({ ...option, dataZoom: A3_ZOOM });
  }
  function a3SurfaceHints(document, config = {}, appearance = {}) {
    const options = visualizationOptions(config, appearance);
    const model = createA3LayoutModel(document, options);
    const markerSize = options.markerSize ?? 28;
    return Object.freeze({
      height: resolveA3VisualizationHeight(document, options),
      bodyHeight: markerSize * (2 * (model.bounds.maxY - model.bounds.minY) + 1),
      markerSize,
      fontSize: options.fontSize ?? 18,
      residueSpacing: options.residueSpacing ?? 0.3,
      legendGap: options.legendGap ?? 90
    });
  }
  function visualizationOptions(config, appearance) {
    const dark = appearance.dark ?? false;
    const base = dark ? DARK_A3_VISUALIZATION_THEME : {};
    const theme = {
      ...base,
      ...appearance.fontFamily ? {
        annotationFontFamily: appearance.fontFamily,
        legendFontFamily: appearance.fontFamily
      } : {},
      ...colorOverride("residueStrokeColor", config.residue_stroke),
      ...colorOverride("labelColor", config.label_color),
      ...colorOverride("positionLabelColor", config.pos_label_color),
      ...colorOverride("variantColor", config.variant_color),
      ...colorOverride("diseaseVariantColor", config.disease_variant_color)
    };
    return {
      nPerRow: config.n_per_row ?? 21,
      residueSpacing: config.residue_spacing ?? 0.3,
      showMarkers: config.show_markers ?? true,
      showLabels: config.show_labels ?? true,
      positionEvery: config.position_every === void 0 ? 10 : config.position_every,
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
      ...config.title ? { title: config.title } : {},
      ...appearance.titleTop === void 0 ? {} : { titleTop: appearance.titleTop }
    };
  }
  var A3_ZOOM = Object.freeze([
    {
      type: "inside",
      xAxisIndex: 0,
      zoomOnMouseWheel: "shift",
      moveOnMouseMove: true,
      moveOnMouseWheel: true,
      preventDefaultMouseMove: false
    },
    {
      type: "inside",
      yAxisIndex: 0,
      zoomOnMouseWheel: false,
      moveOnMouseWheel: false,
      moveOnMouseMove: false
    }
  ]);
  function colorOverride(key, value) {
    return value ? { [key]: value } : {};
  }

  // src/shared/format.ts
  var SIGNIFICANT_DIGITS = 3;
  function formatNumber(value, digits = SIGNIFICANT_DIGITS) {
    if (typeof value !== "number" || !Number.isFinite(value)) return value;
    if (Number.isInteger(value)) return String(value);
    const magnitude = Math.abs(value);
    if (magnitude < 1e-3 || magnitude >= 1e15) return value.toExponential(digits - 1);
    if (Math.floor(Math.log10(magnitude)) + 1 >= digits) return value.toFixed(0);
    const rounded = Number(value.toPrecision(digits));
    const decimals = Math.max(0, digits - 1 - Math.floor(Math.log10(Math.abs(rounded))));
    return rounded.toFixed(decimals);
  }
  function ddSci(value, decimals = 2, hi = 1e6) {
    if (value === null || value === void 0 || !Number.isFinite(value)) return "NA";
    if (value === 0) return 0 .toFixed(decimals);
    if (Math.abs(value) >= hi) return scientific(Number(value.toFixed(decimals)), decimals);
    if (Number(value.toFixed(decimals)) === 0) return scientific(value, 2);
    return value.toFixed(decimals);
  }
  function scientific(value, digits) {
    const [mantissa = "", exponent = "0"] = value.toExponential(Math.max(0, digits - 1)).split("e");
    const trimmed = mantissa.includes(".") ? mantissa.replace(/\.?0+$/, "") : mantissa;
    const e = Number(exponent);
    return `${trimmed}e${e < 0 ? "-" : "+"}${String(Math.abs(e)).padStart(2, "0")}`;
  }

  // src/shared/layout.ts
  var DEFAULT_MARGIN = 36;
  var SIDES = ["top", "right", "bottom", "left"];
  function cartesianGrid(config, axisNames) {
    const set = SIDES.filter((side) => typeof config[`margin_${side}`] === "number");
    const grid = {};
    for (const side of SIDES) {
      const value = config[`margin_${side}`];
      if (set.length === 0) grid[side] = DEFAULT_MARGIN;
      else if (typeof value === "number") grid[side] = value;
    }
    grid.outerBoundsMode = "same";
    grid.outerBoundsContain = axisNames ? "all" : "axisLabel";
    return grid;
  }
  var ZOOM_SLIDER_ID = "rtemis-zoom-slider";
  var SLIDER_SIZE = 30;
  var SLIDER_HANDLE = 7;
  var SLIDER_GAP = 8;
  function axisZoom(axis, grid, options = {}) {
    const side = axis === "x" ? "bottom" : "right";
    const current = grid[side];
    const margin = typeof current === "number" ? current : axis === "x" ? 80 : 15;
    const index = axis === "x" ? { xAxisIndex: 0 } : { yAxisIndex: 0 };
    grid[side] = margin + SLIDER_HANDLE + SLIDER_SIZE + SLIDER_GAP;
    return [
      {
        type: "slider",
        id: ZOOM_SLIDER_ID,
        start: 0,
        end: 100,
        ...index,
        ...options,
        [side]: margin + SLIDER_HANDLE,
        ...axis === "x" ? { height: SLIDER_SIZE } : { width: SLIDER_SIZE }
      },
      { type: "inside", ...index, ...options, zoomOnMouseWheel: "shift", moveOnMouseMove: true }
    ];
  }
  function xyZoom(boxZoom) {
    const inside = (index) => ({
      type: "inside",
      ...index,
      filterMode: "none",
      zoomOnMouseWheel: "shift",
      moveOnMouseMove: true
    });
    return {
      dataZoom: [inside({ xAxisIndex: 0 }), inside({ yAxisIndex: 0 })],
      toolbox: {
        show: true,
        right: 12,
        top: 8,
        itemSize: 14,
        showTitle: false,
        iconStyle: { borderColor: "#808080" },
        emphasis: { iconStyle: { borderColor: "#3F6FB5" } },
        feature: {
          ...boxZoom ? {
            dataZoom: {
              filterMode: "none",
              brushStyle: {
                color: "rgba(128,128,128,0.2)",
                borderColor: "rgba(128,128,128,0.6)",
                borderWidth: 1
              },
              title: { zoom: "Box zoom", back: "Undo zoom" }
            }
          } : {},
          restore: { title: "Reset" }
        }
      }
    };
  }

  // src/shared/source.ts
  function isTime(data, name) {
    return data.types?.[name] === "time";
  }
  var ISO_TIME = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2})(?:\.(\d+))?)?)?/;
  function timeMs(value) {
    if (typeof value === "number") return Number.isFinite(value) ? value : null;
    if (typeof value !== "string") return null;
    const m = ISO_TIME.exec(value);
    if (!m) return null;
    const [, y, mo, d, h, mi, s, frac] = m;
    const ms = frac ? Math.round(Number(`0.${frac}`) * 1e3) : 0;
    return Date.UTC(
      Number(y),
      Number(mo) - 1,
      Number(d),
      Number(h ?? 0),
      Number(mi ?? 0),
      Number(s ?? 0),
      ms
    );
  }
  function timeLabel(ms) {
    const iso = new Date(ms).toISOString();
    if (ms % 864e5 === 0) return iso.slice(0, 10);
    return `${iso.slice(0, 10)} ${iso.slice(11, ms % 1e3 === 0 ? 19 : 23)}`;
  }
  function textColumn(data, name, member) {
    const values = column(data.columns, name, member);
    if (!isTime(data, name)) return values.map((v) => String(v));
    return values.map((v) => {
      const ms = timeMs(v);
      return ms === null ? String(v) : timeLabel(ms);
    });
  }
  function distinct(values, levels) {
    const seen = new Set(values);
    const ordered = (levels ?? []).filter((level) => seen.has(level));
    const known = new Set(ordered);
    return [...ordered, ...[...seen].filter((v) => !known.has(v))];
  }
  function column(source, name, member) {
    const values = source[name];
    if (values === void 0) {
      throw new Error(`Column '${name}' (config member '${member}') is not in the data.`);
    }
    return Array.isArray(values) ? values : Array.from(values);
  }
  function asArray(value) {
    if (value === null || value === void 0) return [];
    return Array.isArray(value) ? value : [value];
  }
  function numberOrNull(value) {
    return typeof value === "number" && Number.isFinite(value) ? value : null;
  }

  // src/charts/bar/config.ts
  var BAR_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "y",
    "group",
    "aggregate",
    "order",
    "stack",
    "horizontal",
    "zoom",
    "bar_width",
    "palette",
    "ylim",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  function buildBarOption(data, config, appearance = {}) {
    const source = data.columns;
    const yColumns = asArray(config.y);
    if (!config.x) {
      throw new Error("A bar chart needs `x` set to draw.");
    }
    const xs = textColumn(data, config.x, "x");
    const aggregate = config.aggregate ?? "none";
    const counting = yColumns.length === 0;
    if (counting && aggregate !== "none") {
      throw new Error(
        "`aggregate` combines the values of `y`, and `y` is unset: each bar counts the rows of its category."
      );
    }
    const frequency = data.frequency ? column(source, data.frequency, "frequency") : null;
    const valuesOf = (y) => y === null ? xs.map((_, row) => frequency ? numberOrNull(frequency[row]) : 1) : column(source, y, "y").map(numberOrNull);
    const measures = counting ? [null] : [...yColumns];
    const groupValues = config.group ? textColumn(data, config.group, "group") : null;
    if (!counting) requireOneRowPer(xs, groupValues ?? void 0);
    let categories;
    let series;
    if (groupValues || counting) {
      categories = distinct(xs, data.levels?.[config.x]);
      const groups = groupValues ? distinct(groupValues, data.levels?.[config.group]) : [null];
      const at = new Map(categories.map((c, i) => [c, i]));
      series = groups.flatMap(
        (g) => measures.map((y) => {
          const values = valuesOf(y);
          const cells = categories.map(() => null);
          xs.forEach((x, row) => {
            const i = at.get(x);
            if (i === void 0 || groupValues && groupValues[row] !== g) return;
            const value2 = values[row] ?? null;
            cells[i] = counting ? (cells[i] ?? 0) + (value2 ?? 0) : value2;
          });
          const name = g !== null && groups.length > 1 ? measures.length > 1 ? `${y} / ${g}` : g : y ?? "Count";
          return { name, data: cells };
        })
      );
    } else {
      categories = xs;
      series = measures.map((y) => ({ name: y, data: valuesOf(y) }));
    }
    const permutation = order(categories, series, config.order ?? "input");
    categories = permutation.map((i) => categories[i]);
    series = series.map((s) => ({ ...s, data: permutation.map((i) => s.data[i] ?? null) }));
    const palette = asArray(config.palette);
    const stack = config.stack ? "total" : void 0;
    const barWidth = typeof config.bar_width === "number" ? config.bar_width : void 0;
    const builtSeries = series.map((s, i) => {
      const single = series.length === 1;
      const perBar = single && palette.length > 1;
      const color = perBar ? void 0 : palette.length > 0 ? palette[i % palette.length] : void 0;
      return {
        type: "bar",
        ...single ? {} : { name: s.name },
        data: perBar ? s.data.map((value2, j) => ({
          value: value2,
          itemStyle: { color: palette[(permutation[j] ?? j) % palette.length] }
        })) : s.data,
        ...stack ? { stack } : {},
        ...barWidth === void 0 ? {} : { barWidth },
        ...color === void 0 ? {} : { itemStyle: { color } }
      };
    });
    const horizontal = config.horizontal ?? false;
    const category = {
      type: "category",
      data: categories,
      axisPointer: { type: "none" },
      ...horizontal ? { inverse: true } : {}
    };
    const value = {
      type: "value",
      ...config.ylim ? { min: config.ylim[0], max: config.ylim[1] } : {}
    };
    const xAxis = horizontal ? value : category;
    const yAxis = horizontal ? category : value;
    if (config.xlab) Object.assign(xAxis, { name: config.xlab, nameLocation: "middle" });
    if (config.ylab) Object.assign(yAxis, { name: config.ylab, nameLocation: "middle" });
    const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
    const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : void 0;
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "axis", valueFormatter: formatNumber },
      ...series.length > 1 ? { legend: { data: series.map((s) => s.name) } } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      series: builtSeries,
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  function order(categories, series, how) {
    const index = categories.map((_, i) => i);
    if (how === "input") return index;
    const total = (i) => series.reduce((sum, s) => sum + (s.data[i] ?? 0), 0);
    const sign = how === "increasing" ? 1 : -1;
    return index.sort((a, b) => sign * (total(a) - total(b)) || a - b);
  }
  function requireOneRowPer(xs, groups) {
    const seen = /* @__PURE__ */ new Set();
    xs.forEach((x, row) => {
      const key = groups ? `${x}\0${groups[row]}` : x;
      if (seen.has(key)) {
        const where = groups ? `category '${x}' in group '${groups[row]}'` : `category '${x}'`;
        throw new Error(
          `Several rows hold ${where}. Combine them before building, as the \`aggregate\` member says (sum, mean, or median).`
        );
      }
      seen.add(key);
    });
  }

  // src/shared/limits.ts
  var DEFAULT_PAD = 0.04;
  function calcLimits(values, pad = DEFAULT_PAD) {
    let min = Number.POSITIVE_INFINITY;
    let max = Number.NEGATIVE_INFINITY;
    for (const v of values) {
      if (typeof v !== "number" || !Number.isFinite(v)) continue;
      if (v < min) min = v;
      if (v > max) max = v;
    }
    if (min > max) return null;
    const span = max - min === 0 ? Math.abs(min) * 0.1 : max - min;
    return [min - pad * span, max + pad * span];
  }
  function calcLimitsFromZero(values, pad = DEFAULT_PAD) {
    const range2 = calcLimits(values, 0);
    if (!range2) return null;
    const min = Math.min(range2[0], 0);
    const max = Math.max(range2[1], 0);
    const span = max - min;
    return [min === 0 ? 0 : min - pad * span, max === 0 ? 0 : max + pad * span];
  }
  function equalAxisLimits(x, y, xlim, ylim, pad, square, equalAxes) {
    if (!square || !equalAxes) return { xlim, ylim };
    if (xlim && ylim) {
      const same = xlim.every(
        (v, i) => Math.abs(v - ylim[i]) <= 15e-9 * Math.max(1, Math.abs(v))
      );
      if (!same) {
        throw new Error(
          `\`square\` and \`equal_axes\` together require both axes to span the same interval, but \`xlim\` is [${xlim.join(", ")}] and \`ylim\` is [${ylim.join(", ")}]. Give them the same limits, or unset one of the two.`
        );
      }
      return { xlim, ylim };
    }
    const common = xlim ?? ylim ?? calcLimits([...x ?? [], ...y], pad);
    return common ? { xlim: common, ylim: common } : { xlim, ylim };
  }

  // src/theme/tokens.ts
  var RTEMIS_PALETTE = Object.freeze([
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
    "#C5A267"
  ]);
  var SIGN_COLORS = Object.freeze({
    negative: "#2D83D3",
    neutral: "#808080",
    positive: "#C55E28"
  });
  function lightThemeTokens(overrides = {}) {
    return Object.freeze({
      scheme: "light",
      base_font_size: 12,
      palette: RTEMIS_PALETTE,
      bg_color: "#ffffff",
      fg_color: "rgba(0, 0, 0, 0.7)",
      negative_color: SIGN_COLORS.negative,
      neutral_color: SIGN_COLORS.neutral,
      positive_color: SIGN_COLORS.positive,
      ...overrides
    });
  }
  function darkThemeTokens(overrides = {}) {
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
      ...overrides
    });
  }
  var DEFAULT_FONT_FAMILY = "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif";
  var GENERIC_FONT_FAMILIES = /* @__PURE__ */ new Set([
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
    "blinkmacsystemfont"
  ]);
  function fontStack(fontFamily) {
    if (fontFamily === void 0 || fontFamily === null) return DEFAULT_FONT_FAMILY;
    const entries = fontFamily.split(",").map((e) => e.trim()).filter((e) => e.length > 0);
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
  function roundHalfEven(x) {
    const floor = Math.floor(x);
    const diff = x - floor;
    if (Math.abs(diff - 0.5) > 1e-9) return Math.round(x);
    return floor % 2 === 0 ? floor : floor + 1;
  }
  function themeFontSizes(tokens) {
    const base = tokens.base_font_size;
    return {
      base,
      title: tokens.title_font_size ?? roundHalfEven(base * 1.2),
      subtitle: tokens.subtitle_font_size ?? base,
      axisLabel: tokens.axis_label_font_size ?? base,
      legend: tokens.legend_font_size ?? base,
      tooltip: tokens.tooltip_font_size ?? base
    };
  }
  function present(entries) {
    return Object.fromEntries(
      Object.entries(entries).filter(([, v]) => v !== void 0 && v !== null)
    );
  }
  function themeOption(tokens) {
    const sizes = themeFontSizes(tokens);
    const dark = tokens.scheme === "dark";
    const fg = tokens.fg_color;
    const textStyle = { color: fg, fontFamily: fontStack(tokens.font_family), fontSize: sizes.base };
    const withText = (style) => ({ ...textStyle, ...style });
    const axisLabel = { fontSize: sizes.axisLabel, color: fg };
    const axisLine = tokens.axis_color ? { lineStyle: { color: tokens.axis_color } } : void 0;
    const axisTick = { show: false };
    const valueAxis = present({
      axisLabel,
      axisLine,
      axisTick,
      splitLine: tokens.grid_color ? { lineStyle: { color: tokens.grid_color } } : void 0
    });
    return {
      color: [...tokens.palette],
      backgroundColor: tokens.bg_color,
      textStyle,
      title: {
        textStyle: withText(present({ color: tokens.title_color, fontSize: sizes.title })),
        subtextStyle: withText(present({ color: tokens.subtitle_color, fontSize: sizes.subtitle }))
      },
      legend: {
        textStyle: withText({ color: tokens.legend_color ?? fg, fontSize: sizes.legend })
      },
      tooltip: present({
        backgroundColor: tokens.tooltip_bg,
        borderColor: tokens.tooltip_border_color,
        textStyle: withText({ color: tokens.tooltip_color ?? fg, fontSize: sizes.tooltip })
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
          areaStyle: { color: "#808080", opacity: 0.12 }
        },
        selectedDataBackground: {
          lineStyle: { color: dark ? "#b0b0b0" : "#707070" },
          areaStyle: { color: "#808080", opacity: 0.25 }
        },
        handleStyle: {
          color: dark ? "#303030" : "#ffffff",
          borderColor: dark ? "#909090" : "#808080"
        },
        moveHandleStyle: { color: "#909090", opacity: 0.6 },
        brushStyle: { color: "#808080", opacity: 0.3 },
        textStyle: axisLabel,
        emphasis: {
          handleStyle: { borderColor: dark ? "#cccccc" : "#505050" },
          moveHandleStyle: { color: dark ? "#b0b0b0" : "#707070" }
        }
      },
      line: { symbol: "circle" },
      categoryAxis: present({ axisLabel, axisLine, axisTick, splitLine: { show: false } }),
      valueAxis,
      logAxis: valueAxis,
      timeAxis: valueAxis
    };
  }

  // src/charts/boxplot/renderers.ts
  var BOXPLOT_POINTS = "rtemis.boxplot_points.v1";
  var BOXPLOT_VIOLIN = "rtemis.violin.v1";
  var BOXPLOT_PAIRS = "rtemis.boxplot_pairs.v1";
  var BOXPLOT_BRACKETS = "rtemis.boxplot_brackets.v1";
  var BOXPLOT_MEAN = "rtemis.boxplot_mean.v1";
  function boxPoint(settings, api, boxIndex, category, value, jitter) {
    const active = api.currentSeriesIndices();
    const boxes = settings.boxSeries.filter((i) => active.includes(i));
    const index = boxes.indexOf(boxIndex);
    if (index < 0) return null;
    const horizontal = settings.horizontal;
    const dim = horizontal ? 1 : 0;
    const point = api.coord(horizontal ? [value, category] : [category, value]);
    const band = Math.abs(api.size(horizontal ? [0, 1] : [1, 0])[dim]);
    const available = band * 0.8 - 2;
    const gap = available / boxes.length * 0.3;
    const width = (available - gap * (boxes.length - 1)) / boxes.length;
    const offset = width / 2 - available / 2 + index * (gap + width);
    const visibleWidth = Math.min(Math.max(width, 7), 50);
    point[dim] = point[dim] + offset + jitter * visibleWidth;
    return { point, width: visibleWidth };
  }
  var boxplotRenderers = {
    // Data: [category, value, observation, offset].
    [BOXPLOT_POINTS]: (params, api) => {
      const settings = params.itemPayload;
      const layout = boxPoint(
        settings,
        api,
        settings.boxIndex,
        api.value(0),
        api.value(1),
        api.value(3)
      );
      if (!layout) return;
      return {
        type: "circle",
        shape: { cx: layout.point[0], cy: layout.point[1], r: settings.pointSize / 2 },
        style: { fill: api.visual("color"), opacity: settings.pointAlpha }
      };
    },
    // Profiles are density positions and relative widths; a one-position
    // profile is a constant sample, drawn as a crossbar.
    [BOXPLOT_VIOLIN]: (params, api) => {
      const settings = params.itemPayload;
      const profile = settings.profiles[params.dataIndex];
      if (!profile?.position || !profile.width) return;
      const dim = settings.horizontal ? 1 : 0;
      const left = [];
      const right = [];
      for (let i = 0; i < profile.position.length; i++) {
        const layout = boxPoint(
          settings,
          api,
          settings.boxIndex,
          api.value(0),
          profile.position[i],
          0
        );
        if (!layout) return;
        const half = profile.width[i] * layout.width / 2;
        const a = layout.point.slice();
        const b = layout.point.slice();
        a[dim] = a[dim] - half;
        b[dim] = b[dim] + half;
        left.push(a);
        right.push(b);
      }
      return {
        type: left.length === 1 ? "polyline" : "polygon",
        shape: { points: left.concat(right.reverse()) },
        style: {
          fill: left.length === 1 ? null : api.visual("color"),
          fillOpacity: settings.fillAlpha,
          stroke: api.visual("color"),
          lineWidth: 1
        }
      };
    },
    // Data: [from category, value, offset, to category, value, offset, observation].
    [BOXPLOT_PAIRS]: (params, api) => {
      const settings = params.itemPayload;
      const from = boxPoint(
        settings,
        api,
        settings.boxIndex,
        api.value(0),
        api.value(1),
        api.value(2)
      );
      const to = boxPoint(settings, api, settings.boxIndex, api.value(3), api.value(4), api.value(5));
      if (!from || !to) return;
      return {
        type: "polyline",
        shape: { points: [from.point, to.point] },
        style: {
          fill: null,
          stroke: api.visual("color"),
          opacity: settings.alpha,
          lineWidth: settings.width
        }
      };
    },
    // Data: [category, mean]. A diamond at the box's center line.
    [BOXPLOT_MEAN]: (params, api) => {
      const settings = params.itemPayload;
      const layout = boxPoint(settings, api, settings.boxIndex, api.value(0), api.value(1), 0);
      if (!layout) return;
      const [x, y] = layout.point;
      const r = settings.size / 2;
      return {
        type: "polygon",
        shape: {
          points: [
            [x, y - r],
            [x + r, y],
            [x, y + r],
            [x - r, y]
          ]
        },
        style: { fill: api.visual("color") }
      };
    },
    // Data: [from category, to category, value position, from series, to series, label].
    [BOXPLOT_BRACKETS]: (params, api) => {
      const settings = params.itemPayload;
      const from = boxPoint(settings, api, api.value(3), api.value(0), api.value(2), 0);
      const to = boxPoint(settings, api, api.value(4), api.value(1), api.value(2), 0);
      if (!from || !to) return;
      const a = from.point;
      const b = to.point;
      const dim = settings.horizontal ? 0 : 1;
      const tick = settings.horizontal ? -6 : 6;
      const endA = a.slice();
      const endB = b.slice();
      endA[dim] = endA[dim] + tick;
      endB[dim] = endB[dim] + tick;
      const center = [
        (a[0] + b[0]) / 2,
        (a[1] + b[1]) / 2
      ];
      center[dim] = center[dim] + (settings.horizontal ? 5 : -5);
      return {
        type: "group",
        children: [
          {
            type: "polyline",
            shape: { points: [endA, a, b, endB] },
            style: { fill: null, stroke: "#888888", lineWidth: 1 }
          },
          {
            type: "text",
            rotation: settings.horizontal ? -Math.PI / 2 : 0,
            originX: center[0],
            originY: center[1],
            style: {
              x: center[0],
              y: center[1],
              text: String(api.value(5)),
              fill: "#888888",
              font: api.font({ fontSize: 12 }),
              align: "center",
              verticalAlign: "bottom"
            }
          }
        ]
      };
    }
  };

  // src/charts/boxplot/config.ts
  var BOXPLOT_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "group",
    "observation",
    "order",
    "transform",
    "geometry",
    "bandwidth",
    "adjust",
    "density_points",
    "paired",
    "pair_alpha",
    "pair_width",
    "comparisons",
    "quartiles",
    "whisker",
    "boxpoints",
    "point_size",
    "point_alpha",
    "point_spread",
    "mean_marker",
    "horizontal",
    "zoom",
    "na_rm",
    "labels",
    "palette",
    "fill_alpha",
    "group_layout",
    "color_by_group",
    "ylim",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  function buildBoxplotOption(data, config, appearance = {}) {
    const variables = data.variables;
    if (variables.length === 0) {
      throw new Error("A boxplot needs `x` set to one or more columns to draw.");
    }
    const labels = asArray(config.labels);
    if (labels.length > 0 && labels.length !== variables.length) {
      throw new Error(
        `\`labels\` must have one label per \`x\` column (${variables.length}); it has ${labels.length}.`
      );
    }
    const name = (variable) => labels[variables.indexOf(variable)] ?? variable;
    const levels = data.levels && data.levels.length > 0 ? data.levels : null;
    const layout = !levels ? "none" : (config.group_layout ?? "auto") === "auto" ? variables.length === 1 ? "x-axis" : "legend" : config.group_layout;
    const cell = (variable, level) => data.cells.find((c) => c.variable === variable && (c.level ?? null) === level) ?? null;
    let categories;
    let series;
    if (!levels) {
      categories = variables.map(name);
      series = [{ name: null, cells: variables.map((v) => cell(v, null)) }];
    } else if (layout === "x-axis") {
      categories = [...levels];
      series = variables.map((v) => ({
        name: variables.length > 1 ? name(v) : null,
        cells: levels.map((l) => cell(v, l))
      }));
    } else {
      categories = variables.map(name);
      series = levels.map((l) => ({ name: l, cells: variables.map((v) => cell(v, l)) }));
    }
    if (new Set(categories).size !== categories.length) {
      throw new Error("Use unique category labels for a boxplot.");
    }
    if (!data.cells.some((c) => c.n > 0)) {
      throw new Error("Supply at least one available boxplot value; missing groups are not zeros.");
    }
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette ?? RTEMIS_PALETTE;
    const single = series.length === 1;
    const perBox = single && (config.color_by_group === true || memberPalette.length > 1);
    const colorOf = (s, j) => perBox ? palette[j % palette.length] : palette[s % palette.length];
    const fillAlpha = config.fill_alpha ?? 0.25;
    const geometry = config.geometry ?? "box";
    const boxpoints = config.boxpoints ?? "none";
    const horizontal = config.horizontal ?? false;
    const spread = config.point_spread ?? 0.5;
    const boxSeries = series.map((_, s) => s);
    const visible = [];
    const boxes = series.map((s, si) => {
      const values = s.cells.map((c, j) => {
        const stats = c?.stats ?? [null, null, null, null, null];
        for (const v of c?.stats ?? []) visible.push(v);
        if (!perBox) return stats;
        const color2 = colorOf(si, j);
        return {
          name: categories[j],
          value: stats,
          itemStyle: {
            color: withAlpha2(color2, fillAlpha),
            borderColor: color2,
            ...geometry === "violin" ? { opacity: 0 } : {}
          }
        };
      });
      const color = colorOf(si, 0);
      return {
        type: "boxplot",
        ...s.name === null ? {} : { name: s.name },
        data: values,
        ...geometry === "both" ? { boxWidth: [7, 14] } : {},
        ...geometry === "violin" ? { silent: true } : {},
        itemStyle: {
          color: withAlpha2(color, fillAlpha),
          borderColor: color,
          ...geometry === "violin" ? { opacity: 0 } : {}
        }
      };
    });
    const encodeOne = horizontal ? { x: 1, y: 0 } : { x: 0, y: 1 };
    const overlays = [];
    const offsetOf = (index) => radicalInverse(index) * spread;
    series.forEach((s, si) => {
      if (boxpoints === "none") return;
      const points = s.cells.flatMap(
        (c, j) => (c?.points ?? []).map((p) => {
          visible.push(p.value);
          return {
            value: [j, p.value, p.id, offsetOf(p.index)],
            itemStyle: { color: colorOf(si, j), opacity: config.point_alpha ?? 0.6 }
          };
        })
      );
      if (points.length === 0) return;
      overlays.push({
        type: "custom",
        ...s.name === null ? {} : { name: s.name },
        renderItem: BOXPLOT_POINTS,
        clip: true,
        z: 3,
        dimensions: ["Category", "Value", "Observation", "Offset"],
        encode: { ...encodeOne, tooltip: [1, 2] },
        itemPayload: {
          horizontal,
          boxSeries,
          boxIndex: si,
          pointSize: config.point_size ?? 5,
          pointAlpha: config.point_alpha ?? 0.6
        },
        data: points
      });
    });
    if (config.mean_marker) {
      series.forEach((s, si) => {
        const means = s.cells.flatMap(
          (c, j) => typeof c?.mean === "number" ? [{ value: [j, c.mean], itemStyle: { color: colorOf(si, j) } }] : []
        );
        if (means.length === 0) return;
        overlays.push({
          type: "custom",
          ...s.name === null ? {} : { name: s.name },
          renderItem: BOXPLOT_MEAN,
          clip: true,
          z: 4,
          dimensions: ["Category", "Mean"],
          encode: { ...encodeOne, tooltip: [1] },
          itemPayload: { horizontal, boxSeries, boxIndex: si, size: 9 },
          data: means
        });
      });
    }
    const layers = [];
    const rangeOf = (c) => c?.range ?? null;
    series.forEach((s, si) => {
      const payload = { horizontal, boxSeries, boxIndex: si };
      if (geometry !== "box") {
        layers.push({
          type: "custom",
          ...s.name === null ? {} : { name: s.name },
          renderItem: BOXPLOT_VIOLIN,
          clip: true,
          z: 1,
          dimensions: ["Category", "Minimum", "Maximum"],
          encode: horizontal ? { x: [1, 2], y: 0 } : { x: 0, y: [1, 2] },
          itemPayload: {
            ...payload,
            profiles: s.cells.map((c) => c?.profile ?? {}),
            fillAlpha
          },
          data: s.cells.map((c, j) => {
            const range2 = rangeOf(c);
            return {
              value: [j, range2?.[0] ?? null, range2?.[1] ?? null],
              itemStyle: { color: colorOf(si, j) }
            };
          })
        });
      }
      if (config.paired) {
        const pairs = [];
        s.cells.forEach((from, j) => {
          const to = s.cells[j + 1];
          if (!from || !to) return;
          const shown = (c) => new Set((c.points ?? []).map((p) => p.id));
          const fromShown = shown(from);
          const toShown = shown(to);
          const target = new Map((to.observations ?? []).map((o) => [o.id, o]));
          for (const a of from.observations ?? []) {
            const b = target.get(a.id);
            if (!b) continue;
            pairs.push([
              j,
              a.value,
              fromShown.has(a.id) ? offsetOf(a.index) : 0,
              j + 1,
              b.value,
              toShown.has(b.id) ? offsetOf(b.index) : 0,
              a.id
            ]);
          }
        });
        layers.push({
          type: "custom",
          ...s.name === null ? {} : { name: s.name },
          renderItem: BOXPLOT_PAIRS,
          silent: true,
          clip: true,
          z: 2,
          dimensions: [
            "From",
            "From value",
            "From offset",
            "To",
            "To value",
            "To offset",
            "Observation"
          ],
          encode: horizontal ? { x: [1, 4], y: [0, 3] } : { x: [0, 3], y: [1, 4] },
          itemPayload: {
            ...payload,
            alpha: config.pair_alpha ?? 0.35,
            width: config.pair_width ?? 1
          },
          itemStyle: { color: s.name === null ? NEUTRAL : colorOf(si, 0) },
          data: pairs
        });
      }
    });
    if (geometry !== "box" || config.paired) {
      for (const s of series)
        for (const c of s.cells) for (const v of rangeOf(c) ?? []) visible.push(v);
    }
    let limits = calcLimits(visible, 0) ?? [0, 1];
    const comparisons = data.comparisons ?? [];
    if (comparisons.length > 0) {
      const resolved = bracketRecords(comparisons, categories, series, limits);
      limits = resolved.limits;
      layers.push({
        type: "custom",
        renderItem: BOXPLOT_BRACKETS,
        silent: true,
        clip: true,
        z: 4,
        dimensions: ["From", "To", "Position", "From series", "To series", "Label"],
        encode: horizontal ? { x: 2, y: [0, 1] } : { x: [0, 1], y: 2 },
        itemPayload: { horizontal, boxSeries },
        data: resolved.records
      });
    }
    const padded = boxpoints !== "none" || geometry !== "box" || config.paired || comparisons.length > 0;
    const valueLimits = config.ylim ?? (padded ? calcLimits(limits, 0.05) : null);
    const axisLimits = valueLimits ?? limits;
    const categoryAxis = {
      type: "category",
      data: categories,
      // Every box keeps its label; ECharts would otherwise thin them.
      axisLabel: { interval: 0 },
      // A horizontal chart lists its categories top to bottom, in reading order.
      ...horizontal ? { inverse: true } : {},
      axisLine: { show: axisLimits[0] <= 0 && axisLimits[1] >= 0, onZero: true }
    };
    const valueAxis = {
      type: "value",
      scale: true,
      ...valueLimits ? { min: valueLimits[0], max: valueLimits[1] } : {}
    };
    const xAxis = horizontal ? valueAxis : categoryAxis;
    const yAxis = horizontal ? categoryAxis : valueAxis;
    if (config.xlab) Object.assign(xAxis, { name: config.xlab, nameLocation: "middle" });
    if (config.ylab) Object.assign(yAxis, { name: config.ylab, nameLocation: "middle" });
    const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
    const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : void 0;
    const named = series.filter((s) => s.name !== null);
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "item", valueFormatter: formatNumber },
      ...named.length > 0 ? {
        legend: {
          data: named.map((s) => s.name),
          ...geometry === "violin" ? { itemStyle: { opacity: 1 } } : {}
        }
      } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      series: [...boxes, ...overlays, ...layers],
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  var NEUTRAL = "#888888";
  function radicalInverse(index) {
    let x = index;
    let value = 0;
    let weight = 0.5;
    while (x > 0) {
      value += x % 2 * weight;
      x = Math.floor(x / 2);
      weight /= 2;
    }
    return value - 0.5;
  }
  function withAlpha2(color, alpha) {
    const m = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(color);
    if (!m) return color;
    const [r, g, b] = [m[1], m[2], m[3]].map((h) => Number.parseInt(h, 16));
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
  }
  function bracketRecords(comparisons, categories, series, limits) {
    const grouped = series.length > 1;
    const span = limits[1] - limits[0] === 0 ? Math.max(Math.abs(limits[1]), 1) : limits[1] - limits[0];
    const positions = [];
    const records = comparisons.map((c, i) => {
      if (!grouped && (c.from_group || c.to_group)) {
        throw new Error("Omit comparison group columns for a single series; use category names.");
      }
      const from = categories.indexOf(c.from);
      const to = categories.indexOf(c.to);
      const seriesIndex = (g) => grouped ? series.findIndex((s) => s.name === g) : 0;
      const fromSeries = seriesIndex(c.from_group);
      const toSeries = seriesIndex(c.to_group);
      if (from < 0 || to < 0 || fromSeries < 0 || toSeries < 0) {
        throw new Error("Use existing category and group names in comparison endpoints.");
      }
      if (from === to && fromSeries === toSeries) {
        throw new Error("Choose two distinct comparison endpoints.");
      }
      for (const [cat, s] of [
        [from, fromSeries],
        [to, toSeries]
      ]) {
        if (!((series[s]?.cells[cat]?.n ?? 0) > 0)) {
          throw new Error("Choose comparison endpoints with available observations.");
        }
      }
      const position = c.position ?? limits[1] + (i + 1) * span * 0.12;
      positions.push(position);
      return [from, to, position, fromSeries, toSeries, c.label];
    });
    const extended = calcLimits(
      [...limits, ...positions.map((p) => p - span * 0.04), ...positions.map((p) => p + span * 0.12)],
      0
    );
    return { records, limits: extended ?? limits };
  }

  // src/charts/boxplot/stats.ts
  function boxplotStats(values, options = {}) {
    const finite2 = values.filter((v) => Number.isFinite(v));
    if (finite2.length === 0) return null;
    const sorted = [...finite2].sort((a, b) => a - b);
    const [q1, q2, q3] = (options.quartiles ?? "linear") === "hinges" ? hinges(sorted) : linear(sorted);
    const whisker = options.whisker ?? 1.5;
    const iqr = q3 - q1;
    const inside = (v) => whisker === 0 || v >= q1 - whisker * iqr && v <= q3 + whisker * iqr;
    let low = Math.min(q1, q3);
    let high = Math.max(q1, q3);
    for (const v of finite2) {
      if (!inside(v)) continue;
      if (v < low) low = v;
      if (v > high) high = v;
    }
    return {
      stats: [low, q1, q2, q3, high],
      outlier: values.map((v) => Number.isFinite(v) && !inside(v))
    };
  }
  function linear(sorted) {
    const at = (p) => {
      const h = (sorted.length - 1) * p;
      const lo = Math.floor(h);
      const a = sorted[lo];
      const b = sorted[Math.min(lo + 1, sorted.length - 1)];
      return a + (h - lo) * (b - a);
    };
    return [at(0.25), at(0.5), at(0.75)];
  }
  function hinges(sorted) {
    const n = sorted.length;
    const n4 = Math.floor((n + 3) / 2) / 2;
    const at = (d) => 0.5 * (sorted[Math.floor(d) - 1] + sorted[Math.ceil(d) - 1]);
    return [at(n4), at((n + 1) / 2), at(n + 1 - n4)];
  }

  // src/charts/confusion/config.ts
  var CONFUSION_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "reference",
    "predicted",
    "count",
    "panel",
    "classes",
    "show_metrics",
    "digits",
    "ncol",
    "correct_color",
    "incorrect_color",
    "low_color",
    "summary_color",
    "font_size",
    "xlab",
    "ylab",
    "title"
  ]);
  var ratio = (a, b) => b > 0 ? a / b : null;
  function confusionRates(counts) {
    const k = counts.length;
    const at = (r, c) => counts[r]?.[c] ?? 0;
    const actual = counts.map((row) => row.reduce((a, b) => a + b, 0));
    const predicted = counts[0]?.map((_, c) => counts.reduce((a, row) => a + (row[c] ?? 0), 0)) ?? [];
    const total = actual.reduce((a, b) => a + b, 0);
    const hits = Array.from({ length: k }, (_, i) => at(i, i));
    const tn = hits.map((h, i) => total - (actual[i] ?? 0) - (predicted[i] ?? 0) + h);
    const sensitivity = hits.map((h, i) => ratio(h, actual[i] ?? 0));
    return {
      total,
      fraction: counts.map((row, r) => row.map((v) => ratio(v, actual[r] ?? 0))),
      sensitivity,
      specificity: tn.map((t, i) => ratio(t, total - (actual[i] ?? 0))),
      ppv: hits.map((h, i) => ratio(h, predicted[i] ?? 0)),
      npv: tn.map((t, i) => ratio(t, total - (predicted[i] ?? 0))),
      accuracy: ratio(
        hits.reduce((a, b) => a + b, 0),
        total
      ),
      balancedAccuracy: sensitivity.some((s) => s === null) ? null : sensitivity.reduce((a, b) => a + b, 0) / k
    };
  }
  function confusionLayout(config) {
    return {
      metrics: config.show_metrics ?? true,
      ncol: config.ncol ?? 2,
      fontSize: config.font_size ?? 12,
      ...config.low_color ? { lowColor: config.low_color } : {},
      ...config.summary_color ? { summaryColor: config.summary_color } : {}
    };
  }
  function percent(value) {
    return `${+value.toPrecision(15)}%`;
  }
  function channels(hex) {
    const m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex);
    if (!m) return [255, 255, 255];
    return [m[1], m[2], m[3]].map((h) => Number.parseInt(h, 16));
  }
  function ramp(low, high, fraction) {
    const a = channels(low);
    const b = channels(high);
    return `#${a.map(
      (v, i) => Math.trunc(v + (b[i] - v) * fraction).toString(16).padStart(2, "0")
    ).join("")}`;
  }
  function contrast(color) {
    const luminance = channels(color).map((v) => {
      const c = v / 255;
      return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    }).reduce((sum, v, i) => sum + v * [0.2126, 0.7152, 0.0722][i], 0);
    return luminance > 0.179 ? "#000000" : "#FFFFFF";
  }
  function buildConfusionOption(data, config, appearance = {}) {
    const { classes, panels } = data;
    const k = classes.length;
    if (k === 0) throw new Error("A confusion chart needs at least one class.");
    if (panels.length === 0) throw new Error("A confusion chart needs at least one panel.");
    for (const p of panels) {
      if (p.counts.length !== k || p.counts.some((row) => row.length !== k)) {
        throw new Error(`Panel '${p.name}' needs a ${k} by ${k} count matrix.`);
      }
    }
    const metrics = config.show_metrics ?? true;
    const digits = config.digits ?? 2;
    const fontSize = config.font_size ?? 12;
    const sign = appearance.signColors ?? SIGN_COLORS;
    const correctColor = config.correct_color ?? sign.negative;
    const incorrectColor = config.incorrect_color ?? sign.positive;
    const low = config.low_color ?? "#FFFFFF";
    const summary = config.summary_color ?? "#EFEFEF";
    const xlab = config.xlab === void 0 ? "Predicted" : config.xlab;
    const ylab = config.ylab === void 0 ? "Reference" : config.ylab;
    const rate = (v) => ddSci(v, digits);
    const label = (text, color) => ({
      show: true,
      position: "inside",
      formatter: text,
      fontSize,
      color: contrast(color)
    });
    const cols = Math.min(config.ncol ?? 2, panels.length);
    const rows = Math.ceil(panels.length / cols);
    const topOffset = config.title ? 7 : 0;
    const titles = config.title ? [{ text: config.title, left: "center", top: 0 }] : [];
    const grids = [];
    const xAxes = [];
    const yAxes = [];
    const maps = [];
    const series = [];
    const named = panels.length > 1 || Boolean(config.panel);
    panels.forEach((p, i) => {
      const r = confusionRates(p.counts);
      const left = 100 * (i % cols) / cols;
      const top = topOffset + (100 - topOffset) * Math.floor(i / cols) / rows;
      const pw = 100 / cols;
      const ph = (100 - topOffset) / rows;
      titles.push({
        ...named ? { text: p.name } : {},
        textStyle: { fontSize: fontSize + 2 },
        left: percent(left + pw / 2),
        top: percent(top),
        textAlign: "center"
      });
      const base = grids.length;
      const layouts = [
        [0.18, 0.2, metrics ? 0.56 : 0.76, metrics ? 0.6 : 0.74]
      ];
      const xLabels = [[...classes]];
      const yLabels = [[...classes]];
      if (metrics) {
        layouts.push([0.75, 0.2, 0.22, 0.6], [0.18, 0.82, 0.56, 0.15], [0.75, 0.82, 0.22, 0.15]);
        xLabels.push(["Sens.", "Spec."], [...classes], ["Summary"]);
        yLabels.push([...classes], ["PPV", "NPV"], ["Accuracy", "BA"]);
      }
      layouts.forEach((box, j) => {
        const index = base + j;
        grids.push({
          left: percent(left + pw * box[0]),
          top: percent(top + ph * box[1]),
          width: percent(pw * box[2]),
          height: percent(ph * box[3]),
          containLabel: false,
          outerBoundsMode: "none"
        });
        xAxes.push({
          type: "category",
          gridIndex: index,
          data: xLabels[j],
          position: "top",
          boundaryGap: true,
          ...j === 0 && xlab ? { name: xlab } : {},
          nameLocation: "middle",
          nameGap: 30,
          axisLine: { show: false },
          splitLine: { show: false },
          axisLabel: { show: j === 0 || j === 1, interval: 0, fontSize }
        });
        yAxes.push({
          type: "category",
          gridIndex: index,
          data: yLabels[j],
          inverse: true,
          boundaryGap: true,
          ...j === 0 && ylab ? { name: ylab } : {},
          nameLocation: "middle",
          nameGap: 50,
          axisLine: { show: false },
          splitLine: { show: false },
          axisLabel: { show: j === 0 || j === 2, interval: 0, fontSize }
        });
      });
      for (const correct of [true, false]) {
        const hue = correct ? correctColor : incorrectColor;
        const marks = [];
        for (let col = 0; col < k; col++) {
          for (let row = 0; row < k; row++) {
            if (row === col !== correct) continue;
            const fraction = r.fraction[row]?.[col] ?? null;
            const count = p.counts[row]?.[col] ?? 0;
            marks.push({
              value: [
                col,
                row,
                fraction ?? 0,
                count,
                classes[row],
                classes[col],
                fraction,
                rate(fraction)
              ],
              label: label(
                Number.isInteger(count) ? count.toFixed(0) : String(count),
                ramp(low, hue, fraction ?? 0)
              )
            });
          }
        }
        maps.push({
          type: "continuous",
          show: false,
          min: 0,
          max: 1,
          inRange: { color: [low, hue] },
          dimension: 2,
          seriesIndex: series.length
        });
        series.push({
          type: "heatmap",
          name: p.name,
          data: marks,
          xAxisIndex: base,
          yAxisIndex: base,
          itemStyle: { borderWidth: 1, borderColor: low },
          dimensions: [
            "Column",
            "Row",
            "Color fraction",
            "Count",
            "Reference",
            "Predicted",
            "Row fraction",
            // A display name and an ordinal type, so the tooltip reads the
            // fixed-decimal text without a formatter (DimensionDefinition).
            { name: "Row fraction label", displayName: "Row fraction", type: "ordinal" }
          ],
          encode: { x: 0, y: 1, tooltip: [4, 5, 3, 7] }
        });
      }
      if (metrics) {
        const right = [];
        const bottom = [];
        for (let j = 0; j < k; j++) {
          [r.sensitivity, r.specificity].forEach((values, m) => {
            right.push({ value: [m, j, 0], label: label(rate(values[j] ?? null), summary) });
          });
          [r.ppv, r.npv].forEach((values, m) => {
            bottom.push({ value: [j, m, 0], label: label(rate(values[j] ?? null), summary) });
          });
        }
        const overall = [
          { value: [0, 0, 0], label: label(`Accuracy
${rate(r.accuracy)}`, summary) },
          { value: [0, 1, 0], label: label(`BA
${rate(r.balancedAccuracy)}`, summary) }
        ];
        [right, bottom, overall].forEach((strip, j) => {
          maps.push({
            type: "continuous",
            show: false,
            min: 0,
            max: 1,
            inRange: { color: [summary, summary] },
            dimension: 2,
            seriesIndex: series.length
          });
          series.push({
            type: "heatmap",
            data: strip,
            xAxisIndex: base + j + 1,
            yAxisIndex: base + j + 1,
            silent: true,
            itemStyle: { borderWidth: 1, borderColor: low }
          });
        });
      }
    });
    return {
      title: titles,
      grid: grids,
      xAxis: xAxes,
      yAxis: yAxes,
      visualMap: maps,
      series,
      legend: { show: false },
      tooltip: { trigger: "item", confine: true }
    };
  }

  // src/charts/confusion/fit.ts
  function rgb(echarts, color, fallback) {
    return color && echarts.color.parse(color) || echarts.color.parse(fallback) || [0, 0, 0];
  }
  function blend(echarts, low, high, fraction) {
    const a = rgb(echarts, low, "#ffffff");
    const b = rgb(echarts, high, "#000000");
    return `#${a.slice(0, 3).map(
      (v, i) => Math.round(v + (b[i] - v) * fraction).toString(16).padStart(2, "0")
    ).join("")}`;
  }
  function contrast2(echarts, color) {
    const luminance = rgb(echarts, color, "#ffffff").slice(0, 3).map((v) => {
      const c = v / 255;
      return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    }).reduce((sum, v, i) => sum + v * [0.2126, 0.7152, 0.0722][i], 0);
    return luminance > 0.179 ? "#000000" : "#ffffff";
  }
  function dimensions(echarts, payload, theme, width, height = Number.POSITIVE_INFINITY) {
    const meta = payload.confusion;
    const option = payload.option;
    const family = option.textStyle?.fontFamily || theme?.textStyle?.fontFamily || "sans-serif";
    const step = meta.metrics ? 4 : 1;
    const panels = option.grid.length / step;
    const classes = option.yAxis[0].data;
    const cols = Math.max(1, Math.min(meta.ncol, panels, Math.floor(width / 360)));
    const rows = Math.ceil(panels / cols);
    const panelWidth = width / cols;
    const titleOffset = option.title.length - panels;
    const metricLabels = meta.metrics ? option.series.filter((_, i) => i % 5 >= 2).flatMap(
      (s) => s.data.map((d) => d.label.formatter)
    ) : [];
    for (let fontSize = meta.fontSize; ; fontSize = Math.max(8, fontSize - 1)) {
      const measure = (text) => echarts.format.getTextRect(String(text), `${fontSize}px ${family}`).width;
      const wrappedHeight = (text, w) => new echarts.graphic.Text({
        style: {
          text: String(text),
          fontSize,
          fontFamily: family,
          width: Math.max(1, w),
          overflow: "break",
          lineHeight: fontSize + 2
        }
      }).getBoundingRect().height;
      const labelWidth = Math.min(
        Math.max(...classes.map(measure), meta.metrics ? measure("NPV") : 0),
        panelWidth * 0.26
      );
      const left = 12 + labelWidth + 10 + (option.yAxis[0].name ? fontSize + 16 : 0);
      const rateWidth = Math.max(
        0,
        ...metricLabels.map((text) => measure(String(text).split("\n").pop() ?? ""))
      );
      const strip = meta.metrics ? 2 * (Math.max(measure("Sens."), measure("Spec."), rateWidth) + 16) : 0;
      const header = option.title.slice(titleOffset).some((t) => t.text) ? meta.fontSize + 14 : 0;
      const bottom = meta.metrics ? 8 + 2 * (2 * fontSize + 12) : 0;
      const footer = 12;
      const globalTop = titleOffset ? meta.fontSize + 28 : 0;
      const availableWidth = panelWidth - left - strip - (strip ? 8 : 0) - 12;
      const panelHeight = (height - globalTop) / rows;
      let side = availableWidth;
      let top = 0;
      let columnLabelHeight = 0;
      let columnLabelWidth = 0;
      let columnRotation = 0;
      for (let pass = 0; pass < 20; pass++) {
        columnRotation = Math.max(...classes.map(measure)) > side / classes.length - 4 ? 90 : 0;
        columnLabelWidth = columnRotation ? Math.max(labelWidth, Math.min(180, panelHeight * 0.28)) : Math.max(1, side / classes.length - 4);
        columnLabelHeight = columnRotation ? Math.min(columnLabelWidth, Math.max(...classes.map(measure))) : fontSize + 2;
        top = 12 + header + (option.xAxis[0].name ? fontSize + 18 : 0) + columnLabelHeight + 12;
        const next = Math.max(1, Math.min(availableWidth, panelHeight - top - bottom - footer));
        if (Math.abs(next - side) < 0.01) break;
        side = next;
      }
      const rowHeight = Math.max(
        ...classes.map((t) => wrappedHeight(t, labelWidth)),
        ...columnRotation ? classes.map((t) => wrappedHeight(t, columnLabelWidth)) : []
      );
      if (Math.max(rowHeight + 2, rateWidth + 4) <= side / classes.length && side > 0) {
        return {
          fontSize,
          family,
          step,
          panels,
          cols,
          rows,
          titleOffset,
          header,
          left,
          strip,
          top,
          bottom,
          footer,
          labelWidth,
          columnLabelHeight,
          columnLabelWidth,
          columnRotation,
          globalTop
        };
      }
      if (fontSize <= Math.min(8, meta.fontSize)) {
        throw new Error(
          "Increase confusion figure dimensions or reduce panels per figure to keep class labels and metrics readable."
        );
      }
    }
  }
  function confusionHeight(echarts, payload, theme, width) {
    const d = dimensions(echarts, payload, theme, width);
    const side = Math.max(
      1,
      Math.min(400, width / d.cols - d.left - d.strip - (d.strip ? 8 : 0) - 12)
    );
    return Math.ceil(d.globalTop + d.rows * (d.top + side + d.bottom + d.footer));
  }
  function fitConfusion(echarts, payload, theme, width, height) {
    const option = payload.option;
    const meta = payload.confusion;
    const d = dimensions(echarts, payload, theme, width, height);
    const bg = option.backgroundColor || theme?.backgroundColor || "#ffffff";
    const fg = option.textStyle?.color || theme?.textStyle?.color || contrast2(echarts, bg);
    const low = meta.lowColor || bg;
    const neutral = contrast2(echarts, bg);
    const summary = meta.summaryColor || blend(echarts, bg, neutral, 0.05);
    const overall = meta.summaryColor || blend(echarts, bg, neutral, 0.025);
    const muted = blend(echarts, bg, fg, 0.72);
    const panelWidth = width / d.cols;
    const panelHeight = (height - d.globalTop) / d.rows;
    const side = Math.max(
      1,
      Math.min(
        panelWidth - d.left - d.strip - (d.strip ? 8 : 0) - 12,
        panelHeight - d.top - d.bottom - d.footer
      )
    );
    const wholeWidth = d.left + side + (d.strip ? 8 : 0) + d.strip + 12;
    const wholeHeight = d.top + side + d.bottom + d.footer;
    const seriesStep = meta.metrics ? 5 : 2;
    for (let i = 0; i < d.panels; i++) {
      const x = i % d.cols * panelWidth + (panelWidth - wholeWidth) / 2;
      const y = d.globalTop + Math.floor(i / d.cols) * panelHeight + (panelHeight - wholeHeight) / 2;
      const left = x + d.left;
      const top = y + d.top;
      const boxes = [[left, top, side, side]];
      if (meta.metrics) {
        boxes.push(
          [left + side + 8, top, d.strip, side],
          [left, top + side + 8, side, d.bottom - 8],
          [left + side + 8, top + side + 8, d.strip, d.bottom - 8]
        );
      }
      boxes.forEach((box, j) => {
        const index = i * d.step + j;
        Object.assign(option.grid[index], {
          left: box[0],
          top: box[1],
          width: box[2],
          height: box[3],
          containLabel: false,
          outerBoundsMode: "none"
        });
        for (const axis of [option.xAxis[index], option.yAxis[index]]) {
          Object.assign(axis.axisLabel, {
            color: muted,
            fontSize: d.fontSize,
            lineHeight: d.fontSize + 2
          });
          axis.axisTick = { show: false };
          axis.nameMoveOverlap = false;
          axis.nameTextStyle = { fontSize: d.fontSize, color: muted };
        }
        option.xAxis[index].nameGap = d.columnLabelHeight + 18;
        if (j === 0) {
          Object.assign(option.xAxis[index].axisLabel, {
            width: d.columnLabelWidth,
            rotate: d.columnRotation,
            overflow: "break"
          });
          Object.assign(option.yAxis[index].axisLabel, {
            width: d.labelWidth + 2,
            overflow: "break"
          });
        }
        option.yAxis[index].nameGap = d.labelWidth + 20;
      });
      const title = option.title[i + d.titleOffset];
      Object.assign(title, { left: left + side / 2, top: y + 4, textAlign: "center" });
      title.textStyle = { ...title.textStyle, color: fg };
      for (let j = 0; j < seriesStep; j++) {
        const index = i * seriesStep + j;
        const series = option.series[index];
        const map = option.visualMap[index];
        const high = j < 2 ? map.inRange.color[1] : j === 4 ? overall : summary;
        map.inRange.color = j < 2 ? [low, high] : [high, high];
        Object.assign(series.itemStyle, { borderColor: bg, borderWidth: 1 });
        for (const point of series.data) {
          point.label.color = j < 2 ? contrast2(echarts, blend(echarts, low, high, point.value[2])) : meta.summaryColor ? contrast2(echarts, high) : fg;
          point.label.fontWeight = j < 2 ? "bold" : "normal";
          point.label.fontSize = d.fontSize;
        }
      }
    }
  }

  // src/charts/curves/config.ts
  var SHARED_MEMBERS = [
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "class_label",
    "split",
    "fold",
    "omitted",
    "variant",
    "digits",
    "palette",
    "legend",
    "square",
    "line_width",
    "fold_opacity",
    "xlab",
    "ylab"
  ];
  var ROC_CHART_CONFIG_MEMBERS = Object.freeze([
    ...SHARED_MEMBERS,
    "fpr",
    "tpr",
    "auc",
    "diagonal",
    "diagonal_color"
  ]);
  var PR_CHART_CONFIG_MEMBERS = Object.freeze([
    ...SHARED_MEMBERS,
    "recall",
    "precision",
    "ap",
    "prevalence",
    "baseline"
  ]);
  var ROC = {
    name: "ROC",
    summary: "AUC",
    x: "FPR",
    y: "TPR",
    xlab: "False positive rate",
    ylab: "True positive rate",
    step: false
  };
  var PR = {
    name: "PR",
    summary: "AP",
    x: "Recall",
    y: "Precision",
    xlab: "Recall",
    ylab: "Precision",
    // Precision at a vertex holds back to the previous recall, so the drawn area
    // is the average precision; precision is not linear in recall in between.
    step: "start"
  };
  function makeUnique(names) {
    const seen = /* @__PURE__ */ new Map();
    const used = new Set(names);
    return names.map((name) => {
      if (!seen.has(name)) {
        seen.set(name, 0);
        return name;
      }
      let k = seen.get(name);
      let candidate;
      do {
        k += 1;
        candidate = `${name}.${k}`;
      } while (used.has(candidate));
      seen.set(name, k);
      used.add(candidate);
      return candidate;
    });
  }
  var capitalize = (s) => s ? s.charAt(0).toUpperCase() + s.slice(1) : s;
  function curveLabel(group, context) {
    const fmt = (v) => ddSci(v, context.digits);
    const multiclass = context.classes.length > 1;
    const parts = [];
    if (multiclass) parts.push(group.class);
    if ((context.splits.length > 1 || !multiclass) && group.split !== "Sample") {
      parts.push(capitalize(group.split));
    }
    if (parts.length === 0) parts.push(group.class);
    const summary = context.perResample ? `${fmt(group.mean)} (${fmt(group.sd)})` : fmt(group.mean);
    return `${parts.join(" \xB7 ")} ${summary}`;
  }
  function curveLegendTitle(type, config) {
    if (config.legend_title) return config.legend_title;
    const summary = type === "roc" ? ROC.summary : PR.summary;
    return (config.variant ?? "aggregate") === "per_resample" ? `${summary}: mean (SD)` : summary;
  }
  function buildCurveOption(kind, data, config, appearance, before, baselines) {
    const { groups, curves } = data;
    if (curves.length === 0) throw new Error(`A ${kind.name} chart needs at least one curve.`);
    for (const c of curves) {
      if (!groups[c.group]) throw new Error(`Curve group ${c.group} is not among the groups.`);
      if (c.x.length !== c.y.length) {
        throw new Error(`A ${kind.name} curve needs one ${kind.y} per ${kind.x}.`);
      }
    }
    const digits = config.digits ?? 2;
    const perResample = (config.variant ?? "aggregate") === "per_resample";
    const fmt = (v) => ddSci(v, digits);
    const classes = [...new Set(groups.map((g) => g.class))];
    const splits = [...new Set(groups.map((g) => g.split))];
    const labels = makeUnique(
      groups.map((g) => curveLabel(g, { classes, splits, perResample, digits }))
    );
    const palette = asArray(config.palette);
    const colors = palette.length > 0 ? palette : appearance.palette ?? [];
    const colorOf = (g) => {
      const id = classes.length > 1 ? classes.indexOf(g.class) : splits.indexOf(g.split);
      return colors.length > 0 ? colors[id % colors.length] : void 0;
    };
    const lineTypes = ["solid", "dashed", "dotted"];
    const display = [kind.x, kind.y, kind.summary];
    const series = [...before];
    for (const c of curves) {
      const g = groups[c.group];
      const color = colorOf(g);
      series.push({
        type: "line",
        name: labels[c.group],
        data: c.x.map((x, j) => [
          x,
          c.y[j],
          c.summary,
          c.fold,
          c.omitted ?? 0,
          fmt(x),
          fmt(c.y[j]),
          fmt(c.summary)
        ]),
        smooth: false,
        step: kind.step,
        // Transparent points are the hover targets of the point tooltip; only
        // the hovered point shows, so the drawn chart and SVG keep the line.
        showSymbol: true,
        symbol: "circle",
        symbolSize: 8,
        connectNulls: false,
        clip: true,
        lineStyle: {
          ...color ? { color } : {},
          width: config.line_width ?? 2,
          opacity: perResample ? config.fold_opacity ?? 0.45 : 1,
          type: lineTypes[splits.indexOf(g.split) % 3]
        },
        itemStyle: { ...color ? { color } : {}, opacity: 0 },
        z: 3,
        emphasis: { itemStyle: { opacity: 1 } },
        // A display name and an ordinal type, so the tooltip reads each value's
        // `ddSci()` text without a formatter (DimensionDefinition).
        dimensions: [
          ...display,
          "Resample",
          "Missing pairs",
          ...display.map((label) => ({
            name: `${label} label`,
            displayName: label,
            type: "ordinal"
          }))
        ],
        encode: { x: 0, y: 1, tooltip: [5, 6, 7, 3] }
      });
    }
    const active = [...new Set(curves.map((c) => c.group))];
    if (baselines) {
      for (const id of active) {
        const g = groups[id];
        const level = g.reference;
        if (level === null || level === void 0 || !Number.isFinite(level)) continue;
        const color = colorOf(g);
        series.push({
          type: "line",
          name: labels[id],
          data: [
            [0, level],
            [1, level]
          ],
          showSymbol: false,
          silent: true,
          legendHoverLink: false,
          lineStyle: { ...color ? { color } : {}, type: "dotted", width: 1 },
          z: 1
        });
      }
    }
    const xlab = config.xlab === void 0 ? kind.xlab : config.xlab;
    const ylab = config.ylab === void 0 ? kind.ylab : config.ylab;
    return {
      ...config.title ? { title: { text: config.title, left: "center", textAlign: "center" } } : {},
      grid: {
        left: 70,
        right: 24,
        top: 25 + (config.title ? 25 : 0),
        bottom: 65,
        containLabel: false,
        outerBoundsMode: "none"
      },
      xAxis: {
        type: "value",
        min: 0,
        max: 1,
        ...xlab ? { name: xlab } : {},
        nameLocation: "middle",
        nameGap: 30
      },
      yAxis: {
        type: "value",
        min: 0,
        max: 1,
        ...ylab ? { name: ylab } : {},
        nameLocation: "middle",
        nameGap: 45
      },
      legend: {
        show: config.legend ?? true,
        orient: "horizontal",
        padding: 0,
        itemGap: 6,
        icon: "roundRect",
        itemWidth: 20,
        itemHeight: 3,
        // Legend keys stay visible whatever the transparent hover targets.
        itemStyle: { opacity: 1 },
        data: active.map((id) => labels[id]),
        textStyle: { fontSize: 12, lineHeight: 14 }
      },
      tooltip: { trigger: "item", confine: true },
      series,
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  function buildRocOption(data, config, appearance = {}) {
    const color = config.diagonal_color ?? "#888888";
    const diagonal = config.diagonal ?? true ? [
      {
        type: "line",
        data: [
          [0, 0],
          [1, 1]
        ],
        showSymbol: false,
        silent: true,
        legendHoverLink: false,
        // A reference line takes no palette color from the groups.
        itemStyle: { color },
        lineStyle: { color, type: "dashed", width: 1 },
        z: 1
      }
    ] : [];
    return buildCurveOption(ROC, data, config, appearance, diagonal, false);
  }
  function buildPrOption(data, config, appearance = {}) {
    return buildCurveOption(PR, data, config, appearance, [], config.baseline ?? true);
  }
  function curveAspect(option, config) {
    if (!(config.square ?? true)) return null;
    const grid = option.grid;
    return {
      ratio: 1,
      leftPx: grid.left,
      rightPx: grid.right,
      topPx: grid.top,
      botPx: grid.bottom
    };
  }

  // src/shared/ridge.ts
  function ridgeOption(input) {
    const { names, xlab } = input;
    const rows = names.length;
    const [low, high] = input.y;
    return {
      ...input.common,
      legend: { show: false },
      grid: names.map((_, i) => ({
        left: 100,
        right: 30,
        top: `${8 + i * 80 / rows}%`,
        height: `${64 / rows}%`,
        outerBoundsMode: "none"
      })),
      xAxis: names.map((_, i) => ({
        type: "value",
        min: input.x[0],
        max: input.x[1],
        gridIndex: i,
        show: i === rows - 1,
        ...i === rows - 1 && xlab ? { name: xlab, nameLocation: "middle" } : {}
      })),
      yAxis: names.map((name, i) => ({
        type: "value",
        min: low < 0 ? low * 1.05 : 0,
        max: high > 0 ? high * 1.05 : 1,
        gridIndex: i,
        name,
        nameLocation: "middle",
        nameGap: 55,
        splitNumber: 2,
        axisLabel: { showMinLabel: false, showMaxLabel: false }
      })),
      ...input.zoom ? {
        dataZoom: [
          {
            type: "inside",
            xAxisIndex: names.map((_, i) => i),
            filterMode: "none",
            zoomOnMouseWheel: "shift",
            moveOnMouseMove: true
          }
        ]
      } : {},
      series: input.series.map((s) => {
        const row = names.indexOf(s.name);
        return { ...s, xAxisIndex: row, yAxisIndex: row };
      })
    };
  }

  // src/charts/density/config.ts
  var DENSITY_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "group",
    "mode",
    "order",
    "zoom",
    "n",
    "bw",
    "bandwidth",
    "kernel",
    "adjust",
    "fill_alpha",
    "na_rm",
    "palette",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  var DENSITY_VALUE_LABEL = "Density";
  function buildDensityOption(data, config, appearance = {}) {
    const samples2 = data.samples;
    if (samples2.length === 0) {
      throw new Error("A density chart needs `x` set to one or more columns to draw.");
    }
    for (const s of samples2) {
      if (s.x.length !== s.y.length) {
        throw new Error(`Sample '${s.name ?? "x"}' needs one density per evaluation point.`);
      }
    }
    const names = samples2.map((s, i) => s.name ?? `Series ${i + 1}`);
    const series = samples2.map((s, i) => ({
      type: "line",
      name: names[i],
      showSymbol: false,
      areaStyle: { opacity: config.fill_alpha ?? 0.25 },
      data: s.x.map((x, j) => [x, s.y[j]])
    }));
    const xs = samples2.flatMap((s) => s.x);
    const ys = samples2.flatMap((s) => s.y);
    const palette = asArray(config.palette);
    const colors = palette.length > 0 ? palette : appearance.palette;
    const xlab = config.xlab || null;
    const ylab = config.ylab || null;
    const common = {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "axis", valueFormatter: formatNumber },
      ...colors && colors.length > 0 ? { color: [...colors] } : {},
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
    if ((config.mode ?? "overlap") === "ridge" && samples2.length > 1) {
      return ridgeOption({
        common,
        series,
        names,
        x: [Math.min(...xs), Math.max(...xs)],
        y: [Math.min(0, ...ys), Math.max(0, ...ys)],
        xlab,
        zoom: config.zoom ?? false
      });
    }
    const xAxis = {
      type: "value",
      scale: true,
      ...xlab ? { name: xlab, nameLocation: "middle" } : {}
    };
    const yAxis = {
      type: "value",
      min: 0,
      ...ylab ? { name: ylab, nameLocation: "middle" } : {}
    };
    const grid = cartesianGrid(config, Boolean(xlab || ylab));
    const dataZoom = config.zoom ? axisZoom("x", grid, { filterMode: "none" }) : void 0;
    return {
      ...common,
      ...samples2.length > 1 ? { legend: { data: names } } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      series
    };
  }

  // src/charts/density/kde.ts
  function ruleBandwidth(summary, rule = "nrd0") {
    const sd = summary.sd ?? 0;
    const iqr = ((summary.q3 ?? 0) - (summary.q1 ?? 0)) / 1.34;
    const scale = summary.n ** -0.2;
    switch (rule) {
      case "nrd0": {
        const lo = Math.min(sd, iqr) || sd || Math.abs(summary.min) || 1;
        return 0.9 * lo * scale;
      }
      case "nrd":
        return 1.06 * Math.min(sd, iqr) * scale;
      default:
        throw new Error(
          `The bandwidth rule '${rule}' is computed in R; set \`bw\` to "nrd0" or "nrd", or set \`bandwidth\`.`
        );
    }
  }
  function kernelAt(kernel, u, bw) {
    const au = Math.abs(u);
    switch (kernel) {
      case "gaussian":
        return Math.exp(-0.5 * (u / bw) ** 2) / (bw * Math.sqrt(2 * Math.PI));
      case "rectangular": {
        const a = bw * Math.sqrt(3);
        return au < a ? 0.5 / a : 0;
      }
      case "triangular": {
        const a = bw * Math.sqrt(6);
        return au < a ? (1 - au / a) / a : 0;
      }
      case "epanechnikov": {
        const a = bw * Math.sqrt(5);
        return au < a ? 0.75 * (1 - (au / a) ** 2) / a : 0;
      }
      case "biweight": {
        const a = bw * Math.sqrt(7);
        return au < a ? 15 / 16 * (1 - (au / a) ** 2) ** 2 / a : 0;
      }
      case "cosine": {
        const a = bw / Math.sqrt(1 / 3 - 2 / Math.PI ** 2);
        return au < a ? (1 + Math.cos(Math.PI * u / a)) / (2 * a) : 0;
      }
      case "optcosine": {
        const a = bw / Math.sqrt(1 - 8 / Math.PI ** 2);
        return au < a ? Math.PI / 4 * Math.cos(Math.PI * u / (2 * a)) / a : 0;
      }
    }
  }
  function kernelDensity(summary, counts, options = {}) {
    const { n: size, min, max } = summary;
    const explicit = options.bandwidth ?? null;
    if (!(size > 0) || explicit === null && (size < 2 || max === min)) return null;
    const bw = (explicit ?? ruleBandwidth(summary, options.bw)) * (options.adjust ?? 1);
    if (!(bw > 0) || !Number.isFinite(bw)) return null;
    const kernel = options.kernel ?? "gaussian";
    const points = options.n ?? 512;
    const cut = options.cut ?? 3;
    const lo = min - cut * bw;
    const hi = max + cut * bw;
    const bins = counts.length;
    const step = (max - min) / bins;
    const centers = Array.from(
      { length: bins },
      (_, b) => max === min ? min : min + (b + 0.5) * step
    );
    const x = Array.from({ length: points }, (_, i) => lo + (hi - lo) * i / (points - 1));
    const y = x.map((at) => {
      let sum = 0;
      for (let b = 0; b < bins; b++) {
        const count = counts[b];
        if (count !== 0) sum += count * kernelAt(kernel, at - centers[b], bw);
      }
      return sum / size;
    });
    return { x, y, bw };
  }

  // src/charts/gantt/renderers.ts
  var GANTT_BAR = "rtemis.gantt.v1";
  var ganttRenderers = {
    // Data: [row, start, end, flag]. A bar spans start to end on its row, at
    // least one pixel wide, filled in its series color; a flag of 1 outlines
    // it without changing the fill.
    [GANTT_BAR]: (params, api) => {
      const p = params.itemPayload;
      const row = api.value(0);
      const [x0, y] = api.coord([api.value(1), row]);
      const [x1] = api.coord([api.value(2), row]);
      const size = api.size([0, 1]);
      const band = Array.isArray(size) ? size[1] : size;
      const height = band * p.barHeight;
      const style = { fill: api.visual("color") };
      if (api.value(3)) {
        style.stroke = p.borderColor;
        style.lineWidth = p.borderWidth;
      }
      return {
        type: "rect",
        transition: ["shape"],
        shape: { x: x0, y: y - height / 2, width: Math.max(1, x1 - x0), height, r: p.barRadius },
        style
      };
    }
  };

  // src/charts/gantt/config.ts
  var GANTT_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "label",
    "start",
    "end",
    "group",
    "tooltip",
    "border",
    "axis_type",
    "zoom",
    "guides",
    "bar_height",
    "bar_radius",
    "border_color",
    "border_width",
    "palette",
    "xlab"
  ]);
  var GANTT_ROW_PX = 20;
  function ganttHeight(rows, rowPx = GANTT_ROW_PX) {
    return Math.max(rows * rowPx + 96, 220);
  }
  function buildGanttOption(data, config, appearance = {}) {
    const labelColumn = config.label ?? "label";
    const labels = textColumn(data, labelColumn, "label");
    if (labels.length === 0) throw new Error("A Gantt chart needs at least one task.");
    const bound = (member) => {
      const name = config[member] ?? member;
      const time2 = isTime(data, name);
      return column(data.columns, name, member).map((v, i) => {
        const value = time2 ? timeMs(v) : typeof v === "number" && Number.isFinite(v) ? v : null;
        if (value === null) {
          throw new Error(
            `Column '${name}' (config member '${member}') must hold a number or time in every row; row ${i + 1} holds '${String(v)}'.`
          );
        }
        return value;
      });
    };
    const starts = bound("start");
    const ends = bound("end");
    const tips = config.tooltip ? textColumn(data, config.tooltip, "tooltip") : labels;
    const flags = config.border ? column(data.columns, config.border, "border").map((v) => v === true || v === 1 ? 1 : 0) : null;
    const rows = distinct(labels);
    const rowOf = new Map(rows.map((r, i) => [r, i]));
    const groups = config.group ? textColumn(data, config.group, "group") : null;
    const levels = groups ? distinct(groups, data.levels?.[config.group]) : [""];
    const palette = asArray(config.palette);
    const colors = palette.length > 0 ? palette : appearance.palette ?? [];
    const payload = {
      barHeight: config.bar_height ?? 0.6,
      barRadius: config.bar_radius ?? 0,
      borderColor: config.border_color ?? "#E53935",
      borderWidth: config.border_width ?? 1.5
    };
    const series = levels.map((level, k) => {
      const color = colors.length > 0 ? colors[k % colors.length] : void 0;
      const items = [];
      labels.forEach((label, i) => {
        if (groups && groups[i] !== level) return;
        const value = [rowOf.get(label), starts[i], ends[i]];
        items.push({ value: flags ? [...value, flags[i]] : value, name: tips[i] });
      });
      return {
        type: "custom",
        ...groups ? { name: level } : {},
        renderItem: GANTT_BAR,
        itemPayload: payload,
        encode: { x: [1, 2], y: 0 },
        clip: true,
        ...color ? { itemStyle: { color } } : {},
        data: items
      };
    });
    const zoom = config.zoom ?? true;
    const time = config.axis_type === "time";
    const controls = zoom ? xyZoom(true) : null;
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      ...time ? { useUTC: true } : {},
      tooltip: { trigger: "item", confine: true, formatter: "{b}" },
      legend: { show: groups !== null, data: groups ? levels : [] },
      ...controls ? { toolbox: controls.toolbox } : {},
      grid: {
        left: 8,
        right: 16,
        // The toolbox icons take a row above the plot, ending about 40 pixels
        // from the top.
        top: Math.max(config.title ? 40 : 12, zoom ? 48 : 0),
        bottom: 8,
        outerBoundsMode: "same",
        outerBoundsContain: config.xlab ? "all" : "axisLabel"
      },
      xAxis: {
        type: time ? "time" : "value",
        ...config.xlab ? { name: config.xlab, nameLocation: "middle", nameGap: 28 } : {},
        ...config.guides ?? true ? {
          axisPointer: {
            show: true,
            type: "line",
            snap: false,
            label: { show: true, backgroundColor: "#666666" },
            lineStyle: { color: "#808080", width: 1, type: "dashed" }
          }
        } : {}
      },
      yAxis: {
        type: "category",
        // Leading spaces indent a label; the SVG renderer collapses them unless
        // they are no-break spaces.
        data: rows.map((r) => r.replace(/^ +/, (m) => "\xA0".repeat(m.length))),
        inverse: true,
        boundaryGap: true,
        // Every row is labeled; `ganttHeight()` gives each row its line.
        axisLabel: { interval: 0 },
        axisTick: { show: false }
      },
      ...controls ? { dataZoom: controls.dataZoom } : {},
      series
    };
  }

  // src/charts/histogram/renderers.ts
  var HISTOGRAM = "rtemis.histogram.v1";
  var histogramRenderers = {
    // Data: [lower edge, upper edge, height, count]. A bar spans its edges on
    // a numeric x axis, so unequal widths draw true and a density curve
    // aligns. Dodging and stacking follow the series the legend shows, so a
    // hidden group's bars give their room and their stack height back.
    [HISTOGRAM]: (params, api) => {
      const settings = params.itemPayload;
      const height = api.value(2);
      if (height === 0) return void 0;
      let lo = api.value(0);
      let hi = api.value(1);
      let base = 0;
      if (settings.mode !== "overlay") {
        const active = api.currentSeriesIndices();
        const shown = settings.seriesIndices.filter((i) => active.includes(i));
        const position = shown.indexOf(params.seriesIndex);
        if (position < 0) return void 0;
        if (settings.mode === "group") {
          const span = (hi - lo) / shown.length;
          lo += position * span;
          hi = lo + span;
        } else {
          for (const index of shown.slice(0, position)) {
            const row = settings.heights[settings.seriesIndices.indexOf(index)];
            const value = row?.[params.dataIndex] ?? 0;
            if (height >= 0 && value >= 0 || height < 0 && value < 0) base += value;
          }
        }
      }
      const left = api.coord([lo, base + height]);
      const right = api.coord([hi, base]);
      const [x1, y1] = left;
      const [x2, y2] = right;
      const color = api.visual("color");
      return {
        type: "rect",
        shape: {
          x: Math.min(x1, x2),
          y: Math.min(y1, y2),
          width: Math.abs(x2 - x1),
          height: Math.abs(y2 - y1)
        },
        style: {
          fill: color,
          fillOpacity: settings.fillAlpha,
          stroke: color,
          strokeOpacity: settings.borderAlpha,
          lineWidth: settings.borderAlpha > 0 ? 1 : 0
        }
      };
    }
  };

  // src/charts/histogram/config.ts
  var HISTOGRAM_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "group",
    "breaks",
    "bins",
    "bin_edges",
    "normalization",
    "bin_stat",
    "bar_mode",
    "mode",
    "order",
    "density",
    "n",
    "bw",
    "bandwidth",
    "kernel",
    "adjust",
    "na_rm",
    "fill_alpha",
    "border_alpha",
    "zoom",
    "palette",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  function heightName(config) {
    const stat = config.bin_stat ?? "count";
    if (stat !== "count") return stat;
    switch (config.normalization ?? "count") {
      case "count":
        return "Height";
      case "probability":
        return "Probability";
      case "percent":
        return "Percent";
      case "density":
        return "Density";
      case "count_density":
        return "Count density";
    }
  }
  function histogramValueLabel(config, name = (column2) => column2) {
    const xs = asArray(config.x);
    const only = xs.length === 1 ? name(xs[0]) : null;
    switch (config.bin_stat ?? "count") {
      case "sum":
        return only === null ? "Sum" : `Sum of ${only}`;
      case "mean":
        return only === null ? "Mean" : `Mean ${only}`;
      case "min":
        return only === null ? "Minimum" : `Minimum ${only}`;
      case "max":
        return only === null ? "Maximum" : `Maximum ${only}`;
      default: {
        const height = heightName(config);
        return height === "Height" ? "Count" : height;
      }
    }
  }
  function normalize(counts, widths, n, normalization) {
    if (n === 0) return counts.map(() => 0);
    const scale = {
      count: (c) => c,
      probability: (c) => c / n,
      percent: (c) => 100 * c / n,
      density: (c, w) => c / (n * w),
      count_density: (c, w) => c / w
    };
    return counts.map((c, i) => scale[normalization](c, widths[i]));
  }
  function buildHistogramOption(data, config, appearance = {}) {
    const edges = data.edges;
    if (edges.length < 2 || edges.some((e, i) => i > 0 && !(e > edges[i - 1]))) {
      throw new Error("Histogram `edges` must be at least two strictly increasing values.");
    }
    const samples2 = data.samples;
    if (samples2.length === 0) {
      throw new Error("A histogram needs `x` set to one or more columns to draw.");
    }
    const nBins = edges.length - 1;
    for (const s of samples2) {
      if (s.counts.length !== nBins || s.values && s.values.length !== nBins) {
        throw new Error(
          `Each sample needs one count per bin (${nBins}); '${s.name ?? "x"}' has ${s.counts.length}.`
        );
      }
    }
    const normalization = config.normalization ?? "count";
    const binStat = config.bin_stat ?? "count";
    const barMode = config.bar_mode ?? "overlay";
    const ridge = (config.mode ?? "overlap") === "ridge";
    const density = config.density ?? false;
    if (binStat !== "count" && (normalization !== "count" || density)) {
      throw new Error(
        'A `bin_stat` other than count takes `normalization = "count"` and no `density`.'
      );
    }
    if (barMode !== "overlay" && (ridge || density)) {
      throw new Error('A `bar_mode` other than overlay takes `mode = "overlap"` and no `density`.');
    }
    const widths = edges.slice(1).map((e, i) => e - edges[i]);
    const first = widths[0];
    const equalWidth = Math.max(...widths.map((w) => Math.abs(w / first - 1))) < 1e-7;
    if (binStat === "count" && !equalWidth && normalization !== "density" && normalization !== "count_density") {
      throw new Error('Use `normalization = "density"` or `"count_density"` for unequal-width bins.');
    }
    const names = samples2.map((s, i) => s.name ?? `Series ${i + 1}`);
    const nonempty = samples2.filter((s) => s.n > 0).length;
    const fillAlpha = config.fill_alpha ?? (barMode === "overlay" && !ridge && nonempty > 1 ? 0.5 : 0.75);
    const borderAlpha = config.border_alpha ?? 1;
    const height = heightName(config);
    const heights = samples2.map(
      (s) => binStat === "count" ? normalize(s.counts, widths, s.n, normalization) : [...s.values ?? []]
    );
    const series = [];
    const barIndices = [];
    samples2.forEach((s, i) => {
      const name = names[i];
      barIndices.push(series.length);
      series.push({
        type: "custom",
        name,
        renderItem: HISTOGRAM,
        clip: true,
        z: 2,
        dimensions: ["Lower", "Upper", height, "Count"],
        encode: {
          x: [0, 1],
          y: 2,
          tooltip: normalization === "count" && binStat === "count" ? [0, 1, 3] : [0, 1, 2, 3]
        },
        data: s.counts.map((c, b) => [edges[b], edges[b + 1], heights[i]?.[b] ?? 0, c])
      });
      if (density) {
        const multiplier = normalization === "count" ? s.n * first : normalization === "probability" ? first : normalization === "percent" ? 100 * first : normalization === "density" ? 1 : s.n;
        const curve = s.density ?? { x: [], y: [] };
        series.push({
          type: "line",
          name,
          // Transparent point targets give the curve an item tooltip, which an
          // unmarked line does not; the SVG export draws them invisibly.
          showSymbol: true,
          symbol: "circle",
          symbolSize: 8,
          itemStyle: { opacity: 0 },
          lineStyle: { opacity: 1 },
          emphasis: { itemStyle: { opacity: 1 } },
          z: 3,
          data: curve.x.map((x, j) => [x, curve.y[j] * multiplier])
        });
      }
    });
    const payload = {
      fillAlpha,
      borderAlpha,
      mode: barMode,
      seriesIndices: barIndices,
      heights
    };
    for (const i of barIndices) series[i].itemPayload = payload;
    const curveHeights = series.filter((s) => s.type === "line").flatMap((s) => s.data.map((d) => d[1]));
    const totals = barMode === "stack" ? edges.slice(1).flatMap((_, b) => [
      heights.reduce((sum, h) => sum + Math.max(h[b] ?? 0, 0), 0),
      heights.reduce((sum, h) => sum + Math.min(h[b] ?? 0, 0), 0)
    ]) : heights.flat();
    const low = Math.min(0, ...totals, ...curveHeights);
    const high = Math.max(0, ...totals, ...curveHeights);
    const palette = asArray(config.palette);
    const colors = palette.length > 0 ? palette : appearance.palette;
    const xlab = config.xlab || null;
    const ylab = config.ylab || null;
    const common = {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "item", valueFormatter: formatNumber },
      // A custom bar hovered during an update transition leaves the y axis at
      // an intermediate scale (ECharts 6.1.0), so updates apply at once.
      animationDurationUpdate: 0,
      ...colors && colors.length > 0 ? { color: [...colors] } : {},
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
    if (ridge && samples2.length > 1) {
      const curveX = series.filter((s) => s.type === "line").flatMap((s) => s.data.map((d) => d[0]));
      return ridgeOption({
        common,
        series,
        names,
        x: [
          Math.min(edges[0], ...curveX),
          Math.max(edges[edges.length - 1], ...curveX)
        ],
        y: [low, high],
        xlab,
        zoom: config.zoom ?? false
      });
    }
    const xAxis = {
      type: "value",
      scale: true,
      ...xlab ? { name: xlab, nameLocation: "middle" } : {}
    };
    const yAxis = {
      type: "value",
      min: low < 0 ? low * 1.05 : 0,
      ...barMode === "stack" && high > 0 ? { max: high * 1.05 } : {},
      ...ylab ? { name: ylab, nameLocation: "middle" } : {}
    };
    const grid = cartesianGrid(config, Boolean(xlab || ylab));
    const dataZoom = config.zoom ? axisZoom("x", grid, { filterMode: "none" }) : void 0;
    return {
      ...common,
      ...samples2.length > 1 ? { legend: { data: names, itemStyle: { opacity: 1 } } } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      series
    };
  }

  // src/charts/histogram/edges.ts
  var MAX_CLASSES = 1e6;
  function classCount(summary, breaks) {
    const { n } = summary;
    const range2 = summary.max - summary.min;
    switch (breaks) {
      case "Sturges":
        return Math.ceil(Math.log2(n) + 1);
      case "Scott": {
        const h = 3.5 * (summary.sd ?? 0) * n ** (-1 / 3);
        return h > 0 ? Math.max(1, Math.ceil(range2 / h)) : 1;
      }
      case "FD":
      case "Freedman-Diaconis": {
        const iqr = (summary.q3 ?? 0) - (summary.q1 ?? 0);
        const h = 2 * iqr * n ** (-1 / 3);
        return h > 0 ? Math.max(1, Math.ceil(range2 / h)) : 1;
      }
    }
  }
  function pretty(lo, hi, n = 5, minN = Math.floor(n / 3)) {
    const h = 1.5;
    const h5 = 0.5 + 1.5 * h;
    const shrink = 0.75;
    const roundingEps = 1e-10;
    const dx = hi - lo;
    let cell;
    let small;
    if (dx === 0 && hi === 0) {
      cell = 1;
      small = true;
    } else {
      cell = Math.max(Math.abs(lo), Math.abs(hi));
      const u2 = (1 + (h5 >= 1.5 * h + 0.5 ? 1 / (1 + h) : 1.5 / (1 + h5))) * Math.max(1, n);
      small = dx < cell * u2 * Number.EPSILON * 3;
    }
    if (small) {
      if (cell > 10) cell = 9 + cell / 10;
      cell *= shrink;
      if (minN > 1) cell /= minN;
    } else {
      cell = dx;
      if (n > 1) cell /= n;
    }
    const tiny = 2 ** -20 * Number.MIN_VALUE;
    if (cell < (tiny === 0 ? Number.MIN_VALUE : tiny)) cell = Number.MIN_VALUE;
    else if (cell > Number.MAX_VALUE / 1.25) cell = 0.1 * Number.MAX_VALUE;
    const base = 10 ** Math.floor(Math.log10(cell));
    let unit = base;
    if (2 * base - cell < h * (cell - unit)) {
      unit = 2 * base;
      if (5 * base - cell < h5 * (cell - unit)) {
        unit = 5 * base;
        if (10 * base - cell < h * (cell - unit)) unit = 10 * base;
      }
    }
    let ns = Math.floor(lo / unit + roundingEps);
    let nu = Math.ceil(hi / unit - roundingEps);
    while (ns * unit > lo + roundingEps * unit) ns--;
    while (nu * unit < hi - roundingEps * unit) nu++;
    let k = Math.trunc(0.5 + nu - ns);
    if (k < minN) {
      const d = minN - k;
      if (ns >= 0) {
        nu += Math.trunc(d / 2);
        ns -= Math.trunc(d / 2) + d % 2;
      } else {
        ns -= Math.trunc(d / 2);
        nu += Math.trunc(d / 2) + d % 2;
      }
      k = minN;
    }
    const l = Math.min(lo, ns * unit);
    const u = Math.max(hi, nu * unit);
    const by = (u - l) / k;
    return Array.from({ length: k + 1 }, (_, i) => {
      const v = i === k ? u : l + i * by;
      return Math.abs(v) < 1e-14 * by ? 0 : v;
    });
  }
  function histogramEdges(summary, options = {}) {
    if (!(summary.n > 0) || !Number.isFinite(summary.min) || !Number.isFinite(summary.max)) {
      throw new Error("A histogram needs at least one finite value to bin.");
    }
    const count = Math.min(
      options.bins ?? classCount(summary, options.breaks ?? "Sturges"),
      MAX_CLASSES
    );
    if (!(count >= 1)) {
      throw new Error(`\`bins\` must be at least 1; it is ${count}.`);
    }
    return pretty(summary.min, summary.max, count, 1);
  }

  // src/shared/fit.ts
  var FIT_MEMBERS = Object.freeze([
    "fit",
    "fit_params",
    "fit_per_group",
    "se",
    "se_times",
    "rsq",
    "n_fit",
    "fit_alpha",
    "fit_name"
  ]);
  var POOLED_FIT_COLOR = "#808080";
  function fitLabel(base, config, rsq) {
    let name = base;
    if (config.fit_name) name = [base, config.fit_name].filter(Boolean).join(" - ");
    if (config.rsq) {
      const value = ddSci(rsq, 3);
      name = `${name ?? "Fit"} (R\xB2 = ${value})`;
    }
    return name;
  }
  function fitRenames(config) {
    return Boolean(config.fit) && Boolean(config.fit_name || config.rsq);
  }
  function checkCurve(curve, at) {
    const n = curve.x.length;
    const where = `fits[${at}]`;
    if (curve.fitted.length !== n) {
      throw new Error(`${where}: \`x\` and \`fitted\` must have the same length.`);
    }
    const lower = curve.lower ?? null;
    const upper = curve.upper ?? null;
    if (lower === null !== (upper === null)) {
      throw new Error(`${where}: give both interval bounds, \`lower\` and \`upper\`, or neither.`);
    }
    if (lower && upper && (lower.length !== n || upper.length !== n)) {
      throw new Error(`${where}: \`lower\` and \`upper\` must have the length of \`x\`.`);
    }
  }
  function fitSeries(curves, targets, config, lineType = "solid") {
    const pooled = config.fit_per_group === false;
    const names = /* @__PURE__ */ new Map();
    const series = [];
    const multiY = new Set(targets.map((t) => t.y)).size > 1;
    curves.forEach((curve, at) => {
      checkCurve(curve, at);
      if (curve.x.length === 0) return;
      const target = targets.find(
        (t) => (!multiY || t.y === curve.y) && (pooled || (curve.group ?? null) === t.group || t.group === null)
      );
      if (!target) {
        throw new Error(
          `fits[${at}] (y = ${String(curve.y ?? null)}, group = ${String(curve.group ?? null)}) matches no series of the chart.`
        );
      }
      const owned = !pooled || targets.every((t) => t.group === null);
      const name = owned ? fitLabel(target.name, config, curve.rsq) : pooledLabel(multiY ? target.y : null, config, curve.rsq);
      if (owned && fitRenames(config)) names.set(target, name);
      const color = owned ? target.color : POOLED_FIT_COLOR;
      const named = name === null ? {} : { name };
      if (config.se !== false && curve.lower && curve.upper) {
        const upper = curve.x.map((x, i) => [x, curve.upper?.[i] ?? null]);
        const lower = curve.x.map((x, i) => [x, curve.lower?.[i] ?? null]).reverse();
        series.push({
          type: "line",
          ...named,
          data: [...upper, ...lower],
          showSymbol: false,
          lineStyle: { opacity: 0 },
          areaStyle: { color, opacity: config.fit_alpha ?? 0.25 },
          // The legend key of a series sharing the fit's name takes this color.
          itemStyle: { color },
          silent: true,
          z: 0,
          legendHoverLink: false
        });
      }
      series.push({
        type: "line",
        ...named,
        data: curve.x.map((x, i) => [x, curve.fitted[i] ?? null]),
        showSymbol: false,
        lineStyle: { color, ...lineType === "dashed" ? { type: "dashed" } : {} },
        itemStyle: { color },
        silent: true
      });
    });
    return { series, names };
  }
  function pooledLabel(y, config, rsq) {
    const label = fitLabel(y, config, rsq);
    if (label !== y && label !== null) return label;
    return y === null ? "Fit" : `Fit (${y})`;
  }

  // src/charts/line/config.ts
  var LINE_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "y",
    "group",
    "aggregate",
    "blocks",
    "smooth",
    "area",
    "points",
    "stack",
    "zoom",
    "palette",
    "line_style",
    "block_color",
    "block_opacity",
    "square",
    "equal_axes",
    "pad",
    "xlim",
    "ylim",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title",
    ...FIT_MEMBERS
  ]);
  function buildLineOption(data, config, appearance = {}) {
    const yColumns = asArray(config.y);
    if (!config.x || yColumns.length === 0) {
      throw new Error("A line chart needs both `x` and `y` set to draw.");
    }
    const xType = xAxisType(data, config.x);
    if (config.group && config.blocks) {
      throw new Error(
        "`blocks` shades runs along `x`, and with `group` each x repeats once per group: bind one of the two."
      );
    }
    if (config.equal_axes && xType !== "value") {
      throw new Error(
        `\`equal_axes\` compares the two axes' scales, which needs a numeric x. This chart's x is ${xType === "time" ? "a date or time" : "categorical"}.`
      );
    }
    if (config.xlim && xType === "category") {
      throw new Error("`xlim` only applies when `x` is numeric or a date.");
    }
    if (config.stack && config.equal_axes) {
      throw new Error(
        "A stacked y axis shows sums, so `equal_axes` cannot apply: unset one of the two."
      );
    }
    if (config.fit && (config.aggregate ?? "none") !== "none") {
      throw new Error(
        "A fit models observations, and with `aggregate` each point is a summary of several: set `aggregate` to none, or unset `fit`."
      );
    }
    if (config.fit && xType !== "value") {
      throw new Error(
        "A fit needs a numeric x; this chart's x is " + (xType === "time" ? "a date or time." : "categorical.")
      );
    }
    if (config.fit && config.area) {
      throw new Error("A fit is drawn over lines, and `area` fills them: unset one of the two.");
    }
    if (config.stack && !config.area) {
      throw new Error(
        "`stack` draws each line on the sum of those below it, which only filled bands show: set `area` too."
      );
    }
    const rawX = column(data.columns, config.x, "x");
    const xNum = xType === "category" ? null : rawX.map(xType === "time" ? timeMs : numberOrNull);
    const xText = xType === "category" ? textColumn(data, config.x, "x") : null;
    const categories = xText ? distinct(xText, data.levels?.[config.x]) : null;
    const xKey = (row) => (xNum ? xNum[row] : xText?.[row]) ?? null;
    const series = lineSeries(data, config, yColumns, xKey);
    for (const s of series) requireOneRowPerX(s, xKey, xText !== null);
    if (config.stack) stackOnKnownSums(series, xKey);
    const pad = config.pad ?? DEFAULT_PAD;
    const yValues = series.flatMap((s) => s.y);
    const limits = equalAxisLimits(
      xNum,
      yValues,
      config.xlim ?? null,
      config.ylim ?? null,
      pad,
      config.square ?? false,
      config.equal_axes ?? false
    );
    const xLim = xNum ? limits.xlim ?? calcLimits(xNum, pad) : null;
    const yDerive = config.area ? calcLimitsFromZero : calcLimits;
    const yLim = limits.ylim ?? yDerive(config.stack ? stackedValues(series, xKey) : yValues, pad);
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 || !config.fit ? memberPalette : appearance.palette ?? RTEMIS_PALETTE;
    const targets = series.map((s, i) => ({
      y: s.column,
      group: s.group,
      name: series.length === 1 ? null : s.name,
      color: palette.length > 0 ? palette[i % palette.length] : ""
    }));
    const fits = config.fit ? fitSeries(data.fits ?? [], targets, config, "dashed") : null;
    const dashes = asArray(config.line_style);
    const at = categories ? new Map(categories.map((c, i) => [c, i])) : null;
    const inOrder = xText !== null && categories !== null && !config.group && xText.length === categories.length && xText.every((x, i) => x === categories[i]);
    const builtSeries = series.map((s, i) => {
      const points = s.rows.map((row, j) => {
        const y = s.y[j] ?? null;
        if (inOrder) return y;
        if (at && xText) return [at.get(xText[row]) ?? null, y];
        return [xNum?.[row] ?? null, y];
      });
      const isolated = config.points ? [] : isolatedPoints(s.y);
      const marked = isolated.length > 0;
      const values = marked ? points.map((value, j) => isolated.includes(j) ? value : { value, symbol: "none" }) : points;
      const color = palette.length > 0 ? palette[i % palette.length] : void 0;
      const dash = dashes.length > 0 ? dashes[i % dashes.length] : void 0;
      const name = fits?.names.get(targets[i]) ?? (series.length === 1 ? null : s.name);
      return {
        type: "line",
        ...name === null ? {} : { name },
        data: values,
        ...config.smooth ? { smooth: true } : {},
        ...config.points || marked ? {} : { showSymbol: false },
        ...config.stack ? { stack: "total" } : {},
        ...config.area ? { areaStyle: {} } : {},
        ...color === void 0 ? {} : { itemStyle: { color } },
        ...dash === void 0 ? {} : { lineStyle: { type: dash } }
      };
    });
    if (config.blocks) {
      const markArea = blockMarkArea(data, config, config.blocks, xNum, xText);
      if (markArea && builtSeries[0]) builtSeries[0].markArea = markArea;
    }
    const xAxis = {
      type: xType,
      ...config.xlab ? { name: config.xlab, nameLocation: "middle" } : {},
      ...categories ? { data: categories } : {},
      ...xType === "value" ? { scale: true } : {},
      ...xLim ? { min: xLim[0], max: xLim[1] } : {},
      ...xType === "category" ? {} : { splitLine: NO_CORNER_LINES },
      ...xType === "time" ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {},
      ...yLim ? { axisLine: zeroAxisLine(yLim) } : {}
    };
    const yAxis = {
      type: "value",
      ...config.ylab ? { name: config.ylab, nameLocation: "middle" } : {},
      scale: true,
      ...yLim ? { min: yLim[0], max: yLim[1] } : {},
      splitLine: NO_CORNER_LINES,
      ...xLim ? { axisLine: zeroAxisLine(xLim) } : {}
    };
    const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
    const dataZoom = config.zoom ? axisZoom("x", grid) : void 0;
    const legendNames = [
      ...new Set(
        [...builtSeries, ...fits?.series ?? []].map((s) => s.name).filter((n) => typeof n === "string")
      )
    ];
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "axis", valueFormatter: formatNumber },
      ...legendNames.length > 0 ? { legend: { data: legendNames } } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      ...xType === "time" ? { useUTC: true } : {},
      series: [...builtSeries, ...fits?.series ?? []],
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  var NO_CORNER_LINES = Object.freeze({ showMinLine: false, showMaxLine: false });
  function zeroAxisLine(otherLimits) {
    return { show: otherLimits[0] <= 0 && otherLimits[1] >= 0, onZero: true };
  }
  function xAxisType(data, name) {
    if (isTime(data, name)) return "time";
    const values = column(data.columns, name, "x");
    if (!Array.isArray(data.columns[name])) return "value";
    const present2 = values.filter((v) => v !== null && v !== void 0);
    return present2.length > 0 && present2.every((v) => typeof v === "number") ? "value" : "category";
  }
  function lineSeries(data, config, yColumns, xKey) {
    const ys = new Map(yColumns.map((y) => [y, column(data.columns, y, "y").map(numberOrNull)]));
    const allRows = (ys.get(yColumns[0]) ?? []).map((_, i) => i).filter((row) => xKey(row) !== null);
    const select = (y, rows) => {
      const values = ys.get(y) ?? [];
      return rows.map((row) => values[row] ?? null);
    };
    if (!config.group) {
      return yColumns.map((y) => ({
        name: y,
        column: y,
        group: null,
        rows: allRows,
        y: select(y, allRows)
      }));
    }
    const raw = column(data.columns, config.group, "group");
    const labels = textColumn(data, config.group, "group");
    const missing = (v) => v === null || v === void 0 || Number.isNaN(v);
    const present2 = labels.filter((_, row) => !missing(raw[row]));
    const groups = distinct(present2, data.levels?.[config.group]);
    const multiY = yColumns.length > 1;
    return groups.flatMap((g) => {
      const rows = allRows.filter((row) => !missing(raw[row]) && labels[row] === g);
      return yColumns.map((y) => ({
        name: groups.length > 1 ? multiY ? `${y} / ${g}` : g : y,
        column: y,
        group: g,
        rows,
        y: select(y, rows)
      }));
    });
  }
  function requireOneRowPerX(line, xKey, category) {
    const seen = /* @__PURE__ */ new Set();
    for (const row of line.rows) {
      const key = xKey(row);
      if (seen.has(key)) {
        const shown = category ? `'${String(key)}'` : String(key);
        throw new Error(
          `Several rows hold x = ${shown} in line '${line.name}'. Combine them before building, as the \`aggregate\` member says (sum, mean, or median).`
        );
      }
      seen.add(key);
    }
  }
  function stackOnKnownSums(series, xKey) {
    const known = [];
    series.forEach((s, i) => {
      const below = known.slice(0, i);
      const y = s.y.map(
        (value, j) => value !== null && below.every((keys) => keys.has(xKey(s.rows[j]))) ? value : null
      );
      series[i] = { ...s, y };
      known.push(new Set(s.rows.filter((_, j) => y[j] !== null).map((row) => xKey(row))));
    });
  }
  function stackedValues(series, xKey) {
    const tops = { positive: /* @__PURE__ */ new Map(), negative: /* @__PURE__ */ new Map() };
    const out = [];
    for (const s of series) {
      s.rows.forEach((row, j) => {
        const y = s.y[j];
        if (y === null || y === void 0) return;
        const key = xKey(row);
        const side = y >= 0 ? "positive" : "negative";
        const value = (tops[side].get(key) ?? 0) + y;
        tops[side].set(key, value);
        out.push(value);
      });
    }
    return out;
  }
  function isolatedPoints(y) {
    const present2 = (j) => y[j] !== null && y[j] !== void 0;
    return y.flatMap((_, j) => present2(j) && !present2(j - 1) && !present2(j + 1) ? [j] : []);
  }
  function blockMarkArea(data, config, blocks, xNum, xText) {
    const colors = asArray(config.block_color);
    if (colors.length === 0) {
      throw new Error("`block_color` must be set when `blocks` is.");
    }
    const raw = column(data.columns, blocks, "blocks");
    const missing = (v) => v === null || v === void 0 || Number.isNaN(v);
    const keys = raw.map((v) => missing(v) ? null : String(v));
    const present2 = new Set(keys.filter((k) => k !== null));
    const declared = data.levels?.[blocks];
    const levels = declared ? declared.filter((level) => present2.has(level)) : sortedLevels(raw.filter((v) => !missing(v)));
    if (colors.length !== levels.length) {
      throw new Error(
        `\`block_color\` must have length ${levels.length} (one per level of \`blocks\`); it has ${colors.length}.`
      );
    }
    const colorOf = new Map(levels.map((level, i) => [level, colors[i] ?? null]));
    const drawn = (key) => {
      if (key === null) return false;
      const color = colorOf.get(key);
      return typeof color === "string" && color !== "transparent";
    };
    const position = (row) => (xNum ? xNum[row] : xText?.[row]) ?? null;
    const runs = [];
    keys.forEach((key, row) => {
      const last = runs[runs.length - 1];
      if (last && last.key === key) last.end = row;
      else runs.push({ key, start: row, end: row });
    });
    const pairs = runs.flatMap((run, i) => {
      if (!drawn(run.key)) return [];
      const next = runs[i + 1];
      const end = xNum && next && drawn(next.key) ? next.start : run.end;
      return [
        [
          {
            xAxis: position(run.start),
            name: run.key,
            itemStyle: {
              color: colorOf.get(run.key),
              opacity: config.block_opacity ?? 0.2
            }
          },
          { xAxis: position(end) }
        ]
      ];
    });
    return pairs.length > 0 ? { data: pairs, silent: true } : null;
  }
  function sortedLevels(values) {
    const unique = [...new Set(values)];
    if (unique.every((v) => typeof v === "number")) {
      return unique.sort((a, b) => a - b).map(String);
    }
    return unique.map(String).sort((a, b) => a.localeCompare(b));
  }
  function lineAspect(option, config) {
    const square = config.square ?? false;
    const equal = config.equal_axes ?? false;
    if (!square && !equal) return null;
    const request = square ? "square" : "equal_axes";
    const span = (axis, which) => {
      const a = axis ?? {};
      if (typeof a.min !== "number" || typeof a.max !== "number") {
        throw new Error(
          `\`${request}\` needs a numeric ${which} axis with known limits, and this chart's is ${a.type === "category" ? "categorical" : "unbounded"}. Plot numeric values, or set \`${which}lim\`.`
        );
      }
      return [a.min, a.max];
    };
    const x = span(option.xAxis, "x");
    const y = span(option.yAxis, "y");
    const grid = option.grid ?? {};
    const px = (side) => {
      const value = grid[side];
      if (typeof value !== "number" || !Number.isFinite(value)) {
        throw new Error(
          `\`square\` and \`equal_axes\` need all four margins as pixel numbers, and \`${side}\` is ${value === void 0 ? "unset" : `'${String(value)}'`}. Set the four \`margin_*\` members.`
        );
      }
      return value;
    };
    return {
      ratio: equal ? (y[1] - y[0]) / (x[1] - x[0]) : 1,
      leftPx: px("left"),
      rightPx: px("right"),
      topPx: px("top"),
      botPx: px("bottom")
    };
  }

  // src/charts/sankey/config.ts
  var SANKEY_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "source",
    "target",
    "value",
    "orient",
    "node_align",
    "node_width",
    "node_gap",
    "palette",
    "link_color",
    "link_opacity",
    "label_font_size",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left"
  ]);
  function sankeyLinks(data, config) {
    const sourceName = config.source ?? "source";
    const valueName = config.value ?? "value";
    const sources = textColumn(data, sourceName, "source");
    const targets = textColumn(data, config.target ?? "target", "target");
    const values = column(data.columns, valueName, "value");
    const sums = /* @__PURE__ */ new Map();
    sources.forEach((source, i) => {
      const target = targets[i];
      const v = values[i];
      if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
        throw new Error(
          `Column '${valueName}' (config member 'value') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`
        );
      }
      const key = JSON.stringify([source, target]);
      const link = sums.get(key);
      sums.set(key, { source, target, value: (link?.value ?? 0) + v });
    });
    const links = [...sums.values()];
    const cycle = findCycle(links);
    if (cycle) {
      throw new Error(`A Sankey diagram's links must not form a cycle; ${cycle.join(" \u2192 ")} is one.`);
    }
    return links;
  }
  function findCycle(links) {
    const next = /* @__PURE__ */ new Map();
    for (const { source, target } of links) next.set(source, [...next.get(source) ?? [], target]);
    const state = /* @__PURE__ */ new Map();
    const path = [];
    const visit = (node) => {
      state.set(node, "open");
      path.push(node);
      for (const to of next.get(node) ?? []) {
        if (state.get(to) === "open") return [...path.slice(path.indexOf(to)), to];
        if (!state.has(to)) {
          const found = visit(to);
          if (found) return found;
        }
      }
      path.pop();
      state.set(node, "done");
      return null;
    };
    for (const node of next.keys()) {
      if (!state.has(node)) {
        const found = visit(node);
        if (found) return found;
      }
    }
    return null;
  }
  function buildSankeyOption(data, config, appearance = {}) {
    const links = sankeyLinks(data, config);
    if (links.length === 0) throw new Error("A Sankey diagram needs at least one link.");
    const names = [.../* @__PURE__ */ new Set([...links.map((l) => l.source), ...links.map((l) => l.target)])];
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette ?? RTEMIS_PALETTE;
    const fontSize = config.label_font_size ?? 12;
    const vertical = config.orient === "vertical";
    const nodes = names.map((name, i) => ({
      name,
      itemStyle: { color: palette[i % palette.length] }
    }));
    const sources = new Set(links.map((l) => l.source));
    const longest = Math.max(0, ...names.filter((n) => !sources.has(n)).map((n) => n.length));
    const auto = vertical ? { left: 12, right: 12, top: 48, bottom: 24 } : {
      left: 12,
      right: Math.max(24, Math.min(Math.ceil(longest * 0.52 * fontSize) + 24, 540)),
      top: 48,
      bottom: 24
    };
    const box = Object.fromEntries(
      ["top", "right", "bottom", "left"].map((side) => {
        const set = config[`margin_${side}`];
        return [side, typeof set === "number" ? set : auto[side]];
      })
    );
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      tooltip: { trigger: "item" },
      series: [
        {
          type: "sankey",
          data: nodes,
          links,
          orient: vertical ? "vertical" : "horizontal",
          ...config.node_align ? { nodeAlign: config.node_align } : {},
          ...typeof config.node_width === "number" ? { nodeWidth: config.node_width } : {},
          ...typeof config.node_gap === "number" ? { nodeGap: config.node_gap } : {},
          emphasis: { focus: "adjacency" },
          lineStyle: {
            color: config.link_color ?? "gradient",
            opacity: config.link_opacity ?? 0.2,
            curveness: 0.5
          },
          label: { fontSize },
          ...box
        }
      ]
    };
  }

  // src/layout/keys.ts
  var SIZE_KEY_ID = "rtemis-size-key";
  var GAP = 16;
  var LABEL_GAP = 6;
  var KEY_TITLE_GAP = 8;
  var DEFAULT_KEY_TEXT = Object.freeze({
    fontFamily: "sans-serif",
    titleSize: 12,
    labelSize: 12,
    color: "#888888",
    titleGap: KEY_TITLE_GAP
  });
  var CIRCLE_COLOR = "#808080";
  var COLOR_SCALE_LENGTH = 140;
  var KEY_COLUMN_WIDTH = 100;
  function sizeKeyValues(min, max) {
    if (!(max > 0)) return [];
    const lo = Math.max(0, min);
    const span = max - lo;
    if (span <= 0) return [max];
    const raw = span / 3;
    const magnitude = 10 ** Math.floor(Math.log10(raw));
    const step = [1, 2, 5, 10].map((m) => m * magnitude).reduce((best, s) => Math.abs(Math.log(s / raw)) < Math.abs(Math.log(best / raw)) ? s : best);
    const values = [];
    for (let v = Math.ceil(lo / step) * step; v <= max + step * 1e-9; v += step) {
      if (v > 0) values.push(Number(v.toPrecision(12)));
    }
    const chosen = values.length >= 2 ? values.slice(-3) : [lo > 0 ? lo : max / 2, max];
    return [...new Set(chosen)].sort((a, b) => b - a);
  }
  function textWidth(text, size) {
    return text.length * size * 0.6;
  }
  function titleHeight(text) {
    return Math.ceil(text.titleSize * 1.2) + text.titleGap;
  }
  function sizeKeyElement(spec, orient, at, text = DEFAULT_KEY_TEXT, labelWidths, columns) {
    const largest = Math.max(...spec.items.map((i) => i.diameter), 1);
    const lead = titleHeight(text);
    const children = [
      {
        id: `${SIZE_KEY_ID}-title`,
        type: "text",
        x: 0,
        y: 0,
        style: {
          text: spec.title,
          fontSize: text.titleSize,
          fontFamily: text.fontFamily,
          fontWeight: "bold",
          fill: text.color,
          verticalAlign: "top"
        }
      }
    ];
    let x = 0;
    let y = lead;
    for (const [i, item] of spec.items.entries()) {
      const label = String(formatNumber(item.value));
      const vertical = orient === "vertical";
      const cx = vertical && columns ? columns.center : largest / 2;
      const cy = vertical ? y + largest / 2 : lead + largest / 2;
      const left = vertical ? 0 : x;
      const labelX = vertical && columns ? columns.label : left + largest + LABEL_GAP;
      children.push(
        {
          id: `${SIZE_KEY_ID}-circle-${i}`,
          type: "circle",
          shape: { cx: left + cx, cy, r: item.diameter / 2 },
          style: { fill: CIRCLE_COLOR, opacity: 0.6 }
        },
        {
          id: `${SIZE_KEY_ID}-label-${i}`,
          type: "text",
          x: labelX,
          y: cy,
          style: {
            text: label,
            fontSize: text.labelSize,
            fontFamily: text.fontFamily,
            fill: text.color,
            verticalAlign: "middle"
          }
        }
      );
      if (orient === "vertical") y += largest + LABEL_GAP;
      else x += largest + LABEL_GAP + (labelWidths?.[i] ?? textWidth(label, text.labelSize)) + GAP;
    }
    return {
      id: SIZE_KEY_ID,
      type: "group",
      silent: true,
      z: 100,
      info: { sizeKey: spec },
      children,
      ...at
    };
  }
  var COLOR_KEY_TITLE_ID = "rtemis-color-key-title";
  var reserved = /* @__PURE__ */ new WeakMap();
  function keyPlacement(legendPosition, legendPlacement, hasLegend) {
    const position = legendPosition ?? "right";
    const side = position.endsWith("right") ? "right" : position.endsWith("left") ? "left" : position === "bottom" ? "bottom" : "top";
    if (!hasLegend) return { side, inside: legendPlacement === "inside" };
    return { side: side === "right" ? "left" : "right", inside: false };
  }
  function keyText(chart, titleGap) {
    const global = chart.getModel().getModel?.("textStyle");
    const scale = chart.getModel().getComponent("visualMap")?.getModel?.("textStyle");
    const pick = (key) => scale?.get(key) ?? global?.get(key);
    const number = (v, fallback) => typeof v === "number" && v > 0 ? v : fallback;
    const titleSize = number(global?.get("fontSize"), DEFAULT_KEY_TEXT.titleSize);
    return {
      fontFamily: String(pick("fontFamily") ?? DEFAULT_KEY_TEXT.fontFamily),
      titleSize,
      labelSize: number(pick("fontSize"), titleSize),
      color: String(pick("color") ?? DEFAULT_KEY_TEXT.color),
      titleGap
    };
  }
  function transparent(fill) {
    if (fill === void 0 || fill === null || fill === "none" || fill === "transparent") return true;
    return typeof fill === "string" && /^rgba\(.*,\s*0(\.0*)?\s*\)$/.test(fill);
  }
  function measurer(echarts, text) {
    const regular = (size) => `${size}px ${text.fontFamily}`;
    return (t, size, bold = false) => {
      const width = echarts.format.getTextRect(
        t,
        bold ? `bold ${regular(size)}` : regular(size)
      ).width;
      return bold && width === echarts.format.getTextRect(t, regular(size)).width ? width * 1.1 : width;
    };
  }
  function sizeKeyBox(echarts, spec, orient, text) {
    const measure = measurer(echarts, text);
    const title = measure(spec.title, text.titleSize, true);
    const largest = Math.max(...spec.items.map((i) => i.diameter), 1);
    const labels = spec.items.map((i) => measure(String(formatNumber(i.value)), text.labelSize));
    const lead = titleHeight(text);
    if (orient === "vertical") {
      const w = Math.max(title, largest + LABEL_GAP + Math.max(0, ...labels));
      return { w, h: lead + spec.items.length * (largest + LABEL_GAP) - LABEL_GAP, labels };
    }
    const row = labels.reduce((sum, l) => sum + largest + LABEL_GAP + l + GAP, 0) - GAP;
    return { w: Math.max(title, row), h: lead + largest, labels };
  }
  function fitKeys(echarts, chart, context) {
    const option = context.option;
    const placement = context.placement ?? { side: "right", inside: false };
    const vertical = placement.side === "left" || placement.side === "right";
    const orient = vertical ? "vertical" : "horizontal";
    const graphics = [option.graphic ?? []].flat().flatMap((g) => g.elements ?? [g]);
    const sizeSpec = graphics.find((g) => g.id === SIZE_KEY_ID)?.info?.sizeKey;
    const scaleOption = [option.visualMap ?? []].flat()[0];
    const scaleModel = chart.getModel().getComponent("visualMap");
    const hasScale = Boolean(scaleOption && scaleModel && scaleModel.get("show") !== false);
    if (!sizeSpec && !hasScale) return null;
    const scaleTitle = String([scaleOption?.text ?? []].flat()[0] ?? "");
    const text = keyText(chart, context.titleGap ?? KEY_TITLE_GAP);
    const width = chart.getWidth();
    const height = chart.getHeight();
    let scaleBox = null;
    let bar = null;
    let labelsLeft = null;
    let symbolTop = null;
    if (hasScale) {
      chart.setOption({
        visualMap: {
          orient,
          // ECharts' `align` names the bar's side: left of the labels when
          // stacked, above them in a row.
          align: vertical ? "left" : "top",
          itemHeight: COLOR_SCALE_LENGTH,
          text: ["", ""],
          left: 0,
          top: 0,
          right: null,
          bottom: null
        }
      });
      const model = chart.getModel().getComponent("visualMap");
      const view = model && chart.getViewOfComponentModel(model);
      if (view) {
        scaleBox = view.group.getBoundingRect().clone();
        scaleBox.applyTransform(view.group.getComputedTransform());
        const absolute = (el) => {
          const r = el.getBoundingRect().clone();
          r.applyTransform(el.getComputedTransform());
          return r;
        };
        view.group.traverse((el) => {
          if (el.isGroup) return;
          const fill = el.style?.fill;
          if (fill && typeof fill === "object" && fill.colorStops) {
            const r = absolute(el);
            if (!bar || r.width * r.height > bar.width * bar.height) bar = r;
            symbolTop = symbolTop === null ? r.y : Math.min(symbolTop, r.y);
          } else if (el.type === "text" && el.style?.text) {
            const r = absolute(el);
            labelsLeft = labelsLeft === null ? r.x : Math.min(labelsLeft, r.x);
          } else if (!el.invisible && !transparent(el.style?.fill)) {
            const r = absolute(el);
            if (r.width > 0 && r.height > 0) {
              symbolTop = symbolTop === null ? r.y : Math.min(symbolTop, r.y);
            }
          }
        });
      }
    }
    const symbolOffset = scaleBox && symbolTop !== null ? symbolTop - scaleBox.y : 0;
    const scaleLead = scaleTitle ? titleHeight(text) - symbolOffset : 0;
    const scaleTitleWidth = scaleTitle ? measurer(echarts, text)(scaleTitle, text.titleSize, true) : 0;
    const parts = [];
    if (scaleBox) {
      parts.push({
        kind: "scale",
        w: Math.max(scaleBox.width, scaleTitleWidth),
        h: scaleBox.height + scaleLead,
        dx: scaleBox.x,
        dy: scaleBox.y
      });
    }
    const sized = sizeSpec ? sizeKeyBox(echarts, sizeSpec, orient, text) : null;
    if (sized) parts.push({ kind: "size", w: sized.w, h: sized.h, dx: 0, dy: 0 });
    let columns = null;
    if (vertical && scaleBox) {
      const box2 = scaleBox;
      const b = bar ?? box2;
      const largest = sizeSpec ? Math.max(...sizeSpec.items.map((i) => i.diameter), 1) : 0;
      const symbol = Math.max(b.width, largest);
      const barOffset = b.x - box2.x;
      const scaleLeft = Math.max(0, symbol / 2 - b.width / 2 - barOffset);
      const center = scaleLeft + barOffset + b.width / 2;
      const scaleLabel = labelsLeft === null ? center + b.width / 2 + LABEL_GAP : scaleLeft + (labelsLeft - box2.x);
      const label = Math.max(scaleLabel, center + largest / 2 + LABEL_GAP);
      columns = { center, label, scaleLeft };
      for (const part of parts) {
        if (part.kind === "scale") part.w = Math.max(scaleLeft + box2.width, scaleTitleWidth);
        else if (sized && sizeSpec) {
          const titleWidth = measurer(echarts, text)(sizeSpec.title, text.titleSize, true);
          part.w = Math.max(titleWidth, center + largest / 2, label + Math.max(0, ...sized.labels));
        }
      }
    }
    if (parts.length === 0) return null;
    const blockW = vertical ? Math.max(...parts.map((p) => p.w)) : parts.reduce((sum, p) => sum + p.w, 0) + GAP * (parts.length - 1);
    const blockH = vertical ? parts.reduce((sum, p) => sum + p.h, 0) + GAP * (parts.length - 1) : Math.max(...parts.map((p) => p.h));
    const authored = [option.grid ?? {}].flat()[0] ?? {};
    const px = (v, extent, fallback) => typeof v === "number" ? v : typeof v === "string" && v.endsWith("%") ? Number.parseFloat(v) * extent / 100 : fallback;
    const side = placement.side;
    const extra = placement.inside ? 0 : (vertical ? blockW : blockH) + GAP;
    const margin = (s) => px(authored[s], s === "left" || s === "right" ? width : height, 36);
    const before = reserved.get(chart);
    const grid = {};
    if (before && before !== side) grid[before] = margin(before);
    if (!placement.inside) {
      Object.assign(grid, { [side]: margin(side) + extra, width: null, height: null });
    }
    if (Object.keys(grid).length > 0) chart.setOption({ grid });
    if (placement.inside) reserved.delete(chart);
    else reserved.set(chart, side);
    context.afterReserve?.();
    const box = (type) => {
      const model = chart.getModel().getComponent(type);
      const view = model && chart.getViewOfComponentModel(model);
      if (!view) return null;
      const r = view.group.getBoundingRect().clone();
      r.applyTransform(view.group.getComputedTransform());
      return r.width > 0 && r.height > 0 ? r : null;
    };
    let reservedExtra = extra;
    if (!placement.inside && (side === "left" || side === "bottom")) {
      const axis = box(side === "left" ? "yAxis" : "xAxis");
      const deficit = !axis ? 0 : side === "left" ? GAP + blockW + GAP / 2 - axis.x : axis.y + axis.height + GAP / 2 + blockH - (height - GAP);
      if (deficit > 0) {
        reservedExtra += deficit;
        chart.setOption({ grid: { [side]: margin(side) + reservedExtra } });
        context.afterReserve?.();
      }
    }
    const gridModel = chart.getModel().getComponent("grid");
    const area = gridModel?.coordinateSystem?.getRect() ?? {
      x: GAP,
      y: GAP,
      width: width - 2 * GAP,
      height: height - 2 * GAP
    };
    let x;
    let y;
    if (vertical) {
      x = placement.inside ? side === "right" ? area.x + area.width - blockW - GAP : area.x + GAP : side === "right" ? width - GAP - blockW : GAP;
      y = placement.inside ? area.y + GAP : area.y + Math.max(0, (area.height - blockH) / 2);
    } else {
      x = placement.inside ? area.x + area.width - blockW - GAP : area.x + (area.width - blockW) / 2;
      y = placement.inside ? side === "bottom" ? area.y + area.height - blockH - GAP : area.y + GAP : side === "bottom" ? height - GAP - blockH : Math.max(GAP, area.y - GAP - blockH);
    }
    if (!placement.inside) {
      if (side === "bottom") {
        const axis = box("xAxis");
        if (axis) y = Math.max(y, axis.y + axis.height + GAP / 2);
      } else if (side === "left") {
        const axis = box("yAxis");
        if (axis) x = Math.min(x, axis.x - GAP / 2 - blockW);
      } else if (side === "top") {
        const title = box("title");
        if (title) y = Math.max(y, title.y + title.height + GAP / 2);
      }
    }
    x = Math.max(0, x);
    y = Math.max(0, y);
    let offset = 0;
    const elements = [];
    for (const part of parts) {
      const left = vertical ? x : x + offset;
      const top = vertical ? y + offset : y;
      if (part.kind === "scale") {
        chart.setOption({
          visualMap: {
            left: left + (columns?.scaleLeft ?? 0) - part.dx,
            top: top + scaleLead - part.dy
          }
        });
        elements.push({
          id: COLOR_KEY_TITLE_ID,
          type: "text",
          left,
          top,
          silent: true,
          z: 100,
          invisible: !scaleTitle,
          style: {
            text: scaleTitle,
            fontSize: text.titleSize,
            fontFamily: text.fontFamily,
            fontWeight: "bold",
            fill: text.color
          }
        });
      } else if (sizeSpec && sized) {
        elements.push(
          sizeKeyElement(sizeSpec, orient, { left, top }, text, sized.labels, columns ?? void 0)
        );
      }
      offset += (vertical ? part.h : part.w) + GAP;
    }
    if (elements.length > 0) chart.setOption({ graphic: { elements } });
    return { side, extra: reservedExtra };
  }
  function keyState(chart) {
    const option = chart.getOption();
    const first = (v) => [v ?? []].flat()[0] ?? {};
    const pick = (o, keys2) => keys2.map((k) => o[k] ?? null);
    const scale = first(option.visualMap);
    const grid = first(option.grid);
    const graphics = [option.graphic ?? []].flat().flatMap((g) => g.elements ?? []);
    const keys = graphics.filter((g) => g?.id === SIZE_KEY_ID || g?.id === COLOR_KEY_TITLE_ID).map((g) => [g.id, ...pick(g, ["left", "top", "right", "invisible"])]);
    return JSON.stringify([
      chart.getWidth(),
      chart.getHeight(),
      pick(scale, ["left", "top", "orient", "align", "text"]),
      pick(grid, ["left", "right", "top", "bottom"]),
      pick(first(option.legend), ["left", "right", "top", "bottom"]),
      keys
    ]);
  }
  function fitLegend(chart, context) {
    const authored = [context.option.legend ?? []].flat()[0] ?? null;
    const model = chart.getModel().getComponent("legend");
    if (!authored || !model || model.get("show") === false) return;
    if (authored.orient === "vertical") return;
    const gridModel = chart.getModel().getComponent("grid");
    const area = gridModel?.coordinateSystem?.getRect();
    const view = chart.getViewOfComponentModel(model);
    if (!area || !view) return;
    const width = chart.getWidth();
    const legendWidth = view.group.getBoundingRect().width;
    const anchor = authored.right !== void 0 && authored.right !== null && authored.left === void 0 ? "right" : authored.left === "center" || authored.left === void 0 ? "center" : "left";
    const position = anchor === "right" ? { right: Math.max(0, width - (area.x + area.width)), left: null } : anchor === "left" ? { left: area.x, right: null } : { left: Math.max(0, area.x + (area.width - legendWidth) / 2), right: null };
    chart.setOption({ legend: position });
  }

  // src/charts/scatter/renderers.ts
  var RUG = "rtemis.rug.v1";
  var RUG_LENGTH = 6;
  var scatterRenderers = {
    // A mark inside the plotting box at the bottom edge for x and at the left
    // edge for y, so the marks stay on the axes through zoom and resize.
    [RUG]: (params, api) => {
      const point = api.coord([api.value(0), api.value(1)]);
      const box = params.coordSys;
      const style = { stroke: api.visual("color"), opacity: 0.5, lineWidth: 1 };
      const px = point[0];
      const py = point[1];
      const bottom = box.y + box.height;
      return {
        type: "group",
        children: [
          { type: "line", shape: { x1: px, x2: px, y1: bottom, y2: bottom - RUG_LENGTH }, style },
          { type: "line", shape: { x1: box.x, x2: box.x + RUG_LENGTH, y1: py, y2: py }, style }
        ]
      };
    }
  };

  // src/charts/scatter/config.ts
  var SCATTER_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "x",
    "y",
    "group",
    "size",
    "color",
    "color_scale",
    "colormap",
    "color_title",
    "size_title",
    "hover",
    "rug",
    "diagonal",
    "diagonal_color",
    "zoom",
    "point_size",
    "point_alpha",
    "palette",
    "square",
    "equal_axes",
    "pad",
    "xlim",
    "ylim",
    "xlab",
    "ylab",
    "title",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title",
    ...FIT_MEMBERS
  ]);
  var MAX_SYMBOL_SIZE = 30;
  var DEFAULT_POINT_SIZE = 6;
  function autoAlpha(n) {
    return Math.min(0.9, Math.max(0.15, 0.9 - 0.15 * Math.log10(Math.max(n, 1))));
  }
  function buildScatterOption(data, config, appearance = {}) {
    const yColumns = asArray(config.y);
    if (!config.x || yColumns.length === 0) {
      throw new Error("A scatter chart needs both `x` and `y` set to draw.");
    }
    if (config.color && config.group) {
      throw new Error("`color` and `group` both set the color of a point: bind one of the two.");
    }
    if (config.color && yColumns.length > 1) {
      throw new Error(
        "With `color`, a point's color shows its `color` value, so points of several `y` columns cannot be told apart: bind one `y` column."
      );
    }
    const time = isTime(data, config.x);
    const xValues = time ? column(data.columns, config.x, "x").map(timeMs) : numberColumn(data, config.x, "x");
    if (config.equal_axes && time) {
      throw new Error("`equal_axes` compares the two axes' scales, which needs a numeric x.");
    }
    const ys = new Map(yColumns.map((y) => [y, numberColumn(data, y, "y")]));
    const size = config.size ? numberColumn(data, config.size, "size") : null;
    if (size?.some((v) => v !== null && v < 0)) {
      throw new Error(
        `Column '${config.size}' (config member 'size') has negative values; a symbol's area shows a nonnegative quantity.`
      );
    }
    const colorValues = config.color ? numberColumn(data, config.color, "color") : null;
    const hover = config.hover ? column(data.columns, config.hover, "hover") : null;
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette ?? RTEMIS_PALETTE;
    const series = pointSeries(data, config, yColumns, xValues, ys, [size, colorValues], palette).map(
      (s) => colorValues ? { ...s, color: SIGN_COLORS.neutral } : s
    );
    if (series.every((s) => s.rows.length === 0)) {
      throw new Error("A scatter chart needs at least one row with every bound column set.");
    }
    const pad = config.pad ?? DEFAULT_PAD;
    const drawnX = series.flatMap((s) => s.rows.map((row) => xValues[row]));
    const drawnY = series.flatMap((s) => s.rows.map((row) => ys.get(s.y)?.[row] ?? null));
    const limits = equalAxisLimits(
      drawnX,
      drawnY,
      config.xlim ?? null,
      config.ylim ?? null,
      pad,
      config.square ?? false,
      config.equal_axes ?? false
    );
    const xLim = limits.xlim ?? calcLimits(drawnX, pad);
    const yLim = limits.ylim ?? calcLimits(drawnY, pad);
    const fits = config.fit ? fitSeries(data.fits ?? [], series, config) : null;
    const nameOf = (s) => fits?.names.get(s) ?? s.name;
    const alpha = config.point_alpha ?? autoAlpha(drawnX.length);
    const maxSize = size ? Math.max(0, ...series.flatMap((s) => s.rows.map((r) => size[r] ?? 0))) : 0;
    const largestSymbol = MAX_SYMBOL_SIZE * (config.point_size ?? DEFAULT_POINT_SIZE) / DEFAULT_POINT_SIZE;
    const symbolSize = (row) => {
      const v = size?.[row] ?? 0;
      return maxSize > 0 ? largestSymbol * Math.sqrt(v / maxSize) : 0;
    };
    const points = series.map((s) => {
      const y = ys.get(s.y) ?? [];
      const values = s.rows.map((row) => {
        const value = [
          xValues[row],
          y[row],
          ...colorValues ? [colorValues[row]] : [],
          ...size ? [size[row]] : []
        ];
        if (!size && !hover) return value;
        return {
          value,
          ...size ? { symbolSize: symbolSize(row) } : {},
          ...hover ? { name: String(hover[row]) } : {}
        };
      });
      const name = nameOf(s);
      return {
        type: "scatter",
        ...name === null ? {} : { name },
        data: values,
        ...size ? {} : { symbolSize: config.point_size ?? DEFAULT_POINT_SIZE },
        itemStyle: { ...colorValues ? {} : { color: s.color }, opacity: alpha }
      };
    });
    const rugs = config.rug ? series.map((s) => {
      const y = ys.get(s.y) ?? [];
      const name = nameOf(s);
      return {
        type: "custom",
        ...name === null ? {} : { name },
        renderItem: RUG,
        silent: true,
        clip: true,
        itemStyle: { color: s.color },
        data: s.rows.map((row) => [xValues[row], y[row]]),
        encode: { x: 0, y: 1 }
      };
    }) : [];
    const diagonal = config.diagonal && xLim && yLim ? diagonalSeries(xLim, yLim, config) : [];
    const legendNames = [
      ...new Set(
        [...points, ...fits?.series ?? []].map((s) => s.name).filter((n) => typeof n === "string")
      )
    ];
    const xName = config.xlab ?? null;
    const yName = config.ylab ?? null;
    const xAxis = {
      type: time ? "time" : "value",
      ...xName ? { name: xName, nameLocation: "middle" } : {},
      ...time ? {} : { scale: true },
      ...xLim ? { min: xLim[0], max: xLim[1] } : {},
      splitLine: NO_CORNER_LINES2,
      ...time ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {},
      ...yLim ? { axisLine: zeroAxisLine2(yLim) } : {}
    };
    const yAxis = {
      type: "value",
      ...yName ? { name: yName, nameLocation: "middle" } : {},
      scale: true,
      ...yLim ? { min: yLim[0], max: yLim[1] } : {},
      splitLine: NO_CORNER_LINES2,
      ...xLim && !time ? { axisLine: zeroAxisLine2(xLim) } : {}
    };
    const grid = cartesianGrid(config, Boolean(xName || yName));
    const sizeKey = size && maxSize > 0 ? {
      title: config.size_title ?? config.size,
      items: sizeKeyValues(
        Math.min(...series.flatMap((s) => s.rows.map((r) => size[r] ?? 0))),
        maxSize
      ).map((value) => ({ value, diameter: largestSymbol * Math.sqrt(value / maxSize) }))
    } : null;
    if ((colorValues || sizeKey) && typeof config.margin_right !== "number") {
      grid.right = Math.max(Number(grid.right ?? 0), KEY_COLUMN_WIDTH);
    }
    const keysTop = typeof grid.top === "number" ? grid.top : 36;
    const zoom = config.zoom ? xyZoom(!(config.square || config.equal_axes)) : null;
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: {
        trigger: "item",
        formatter: pointTooltip(config, series.length > 1, xName ?? config.x, yColumns, time)
      },
      ...legendNames.length > 0 ? { legend: { data: legendNames, itemStyle: { opacity: 1 } } } : {},
      xAxis,
      yAxis,
      grid,
      ...zoom ? zoom : {},
      ...colorValues ? { visualMap: colorScale(config, colorValues, points.length, keysTop) } : {},
      ...sizeKey ? {
        graphic: {
          elements: [
            sizeKeyElement(sizeKey, "vertical", {
              right: 10,
              top: keysTop + (colorValues ? COLOR_SCALE_LENGTH + 60 : 0)
            })
          ]
        }
      } : {},
      ...time ? { useUTC: true } : {},
      series: [...points, ...fits?.series ?? [], ...rugs, ...diagonal],
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  var NO_CORNER_LINES2 = Object.freeze({ showMinLine: false, showMaxLine: false });
  function zeroAxisLine2(otherLimits) {
    return { show: otherLimits[0] <= 0 && otherLimits[1] >= 0, onZero: true };
  }
  function numberColumn(data, name, member) {
    const values = column(data.columns, name, member);
    const out = values.map(numberOrNull);
    const text = values.findIndex((v) => v !== null && v !== void 0 && typeof v !== "number");
    if (text >= 0) {
      throw new Error(
        `Column '${name}' (config member '${member}') must be numeric; row ${text + 1} holds '${String(values[text])}'.`
      );
    }
    return out;
  }
  function pointSeries(data, config, yColumns, x, ys, extra, palette) {
    const complete = (row) => x[row] !== null && extra.every((values) => values === null || values[row] !== null);
    const rowsOf = (y, candidates) => candidates.filter((row) => complete(row) && (ys.get(y)?.[row] ?? null) !== null);
    const all = x.map((_, row) => row);
    const color = (i) => palette[i % palette.length];
    if (!config.group) {
      return yColumns.map((y, i) => ({
        y,
        group: null,
        name: yColumns.length > 1 ? y : null,
        color: color(i),
        rows: rowsOf(y, all)
      }));
    }
    const raw = column(data.columns, config.group, "group");
    const labels = textColumn(data, config.group, "group");
    const missing = (v) => v === null || v === void 0 || Number.isNaN(v);
    const present2 = labels.filter((_, row) => !missing(raw[row]));
    const groups = distinct(present2, data.levels?.[config.group]);
    const multiY = yColumns.length > 1;
    return groups.flatMap((g, gi) => {
      const rows = all.filter((row) => !missing(raw[row]) && labels[row] === g);
      return yColumns.map((y, yi) => ({
        y,
        group: g,
        name: multiY ? `${y} / ${g}` : g,
        color: color(gi * yColumns.length + yi),
        rows: rowsOf(y, rows)
      }));
    });
  }
  function diagonalSeries(xLim, yLim, config) {
    const lo = Math.max(xLim[0], yLim[0]);
    const hi = Math.min(xLim[1], yLim[1]);
    if (!(lo < hi)) return [];
    return [
      {
        type: "line",
        data: [
          [lo, lo],
          [hi, hi]
        ],
        showSymbol: false,
        silent: true,
        legendHoverLink: false,
        lineStyle: { color: config.diagonal_color ?? SIGN_COLORS.neutral, type: "dashed" },
        z: 1
      }
    ];
  }
  function colorScale(config, values, pointSeriesCount, top) {
    const finite2 = values.filter((v) => v !== null);
    const diverging = config.color_scale === "diverging";
    let min = Math.min(...finite2);
    let max = Math.max(...finite2);
    if (diverging) {
      const m = Math.max(Math.abs(min), Math.abs(max)) || 1;
      min = -m;
      max = m;
    } else if (min === max) {
      max = min + 1;
    }
    const given = asArray(config.colormap);
    if (given.length === 1) {
      throw new Error("`colormap` needs at least two colors, low to high.");
    }
    const colors = given.length > 0 ? given : diverging ? [SIGN_COLORS.negative, SIGN_COLORS.neutral, SIGN_COLORS.positive] : [SIGN_COLORS.neutral, SIGN_COLORS.positive];
    return {
      type: "continuous",
      dimension: 2,
      seriesIndex: Array.from({ length: pointSeriesCount }, (_, i) => i),
      min,
      max,
      // ECharts labels the ends with `precision` decimals (default 0, which
      // reads 0.4 to 0.65 as 0 to 1): two significant digits of the span.
      precision: Math.max(0, 1 - Math.floor(Math.log10(max - min))),
      calculable: true,
      orient: "vertical",
      itemHeight: COLOR_SCALE_LENGTH,
      right: 10,
      top,
      inRange: { color: colors },
      text: [config.color_title ?? config.color, ""]
    };
  }
  function pointTooltip(config, named, xName, yColumns, time) {
    const html = (s) => String(s).replace(
      /[&<>"]/g,
      (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]
    );
    return (params) => {
      const p = params;
      const value = Array.isArray(p.value) ? p.value : [];
      const x = time && typeof value[0] === "number" ? new Date(value[0]).toISOString().slice(0, 19).replace("T", " ") : formatNumber(value[0]);
      const yName = yColumns.length === 1 ? config.ylab ?? yColumns[0] : config.ylab ?? "y";
      const lines = [
        ...config.hover && p.name ? [`<b>${html(p.name)}</b>`] : [],
        ...named && p.seriesName ? [`${p.marker ?? ""}${html(p.seriesName)}`] : [],
        `${html(xName)}: ${html(x)}`,
        `${html(yName)}: ${html(formatNumber(value[1]))}`,
        ...config.color ? [`${html(config.color)}: ${html(formatNumber(value[2]))}`] : [],
        ...config.size ? [`${html(config.size)}: ${html(formatNumber(value[config.color ? 3 : 2]))}`] : []
      ];
      return lines.join("<br/>");
    };
  }

  // src/charts/significance/config.ts
  var SIGNIFICANCE_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "view",
    "estimate",
    "p_value",
    "label",
    "group",
    "palette",
    "p_adjust_method",
    "n_tests",
    "p_transform",
    "p_thresh",
    "x_thresh",
    "zero_cap",
    "annotate_n",
    "reference",
    "legend",
    "zoom",
    "negative_color",
    "neutral_color",
    "positive_color",
    "alpha",
    "point_size",
    "xlab",
    "ylab",
    "xlim",
    "ylim",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left"
  ]);
  var METHOD_LABELS = {
    holm: "Holm",
    hochberg: "Hochberg",
    hommel: "Hommel",
    bonferroni: "Bonferroni",
    BH: "BH",
    BY: "BY",
    fdr: "FDR",
    none: ""
  };
  function significanceAxisNames(config) {
    const method = METHOD_LABELS[config.p_adjust_method ?? "holm"];
    const p = method ? `${method}-adjusted p` : "p";
    const transform = config.p_transform ?? "neg_log10";
    const y = transform === "neg_log10" ? `-log10(${p})` : transform === "one_minus" ? `1 - ${p}` : p;
    return { x: (config.view ?? "volcano") === "volcano" ? "Coefficient" : "Outcome", y };
  }
  function significanceGroupNames(x_thresh = 0) {
    return x_thresh === 0 ? ["Significant negative", "Not significant", "Significant positive"] : ["Significant low", "Not significant", "Significant high"];
  }
  function referenceLabelMargin(label) {
    return Math.max(36, Math.ceil(label.length * 12 * 0.6) + 5 + 8);
  }
  var finite = (v) => typeof v === "number" && Number.isFinite(v);
  function buildSignificanceOption(data, config, appearance = {}) {
    const n = data.p_value.length;
    for (const [name, values2] of [
      ["label", data.label],
      ["estimate", data.estimate],
      ["p_adjusted", data.p_adjusted]
    ]) {
      if (values2.length !== n) {
        throw new Error(`Significance data needs one ${name} per p-value.`);
      }
    }
    const grouped = Boolean(config.group) && Array.isArray(data.group);
    if (grouped && data.group.length !== n) {
      throw new Error("Significance data needs one group per p-value.");
    }
    const volcano = (config.view ?? "volcano") === "volcano";
    const transform = config.p_transform ?? "neg_log10";
    const pThresh = config.p_thresh ?? 0.05;
    const xThresh = config.x_thresh ?? 0;
    const annotateN = config.annotate_n ?? 7;
    const reference = config.reference ?? true;
    const legend = config.legend ?? true;
    const alpha = config.alpha ?? 0.8;
    const pointSize = config.point_size ?? 8;
    const apply = (p) => transform === "neg_log10" ? -Math.log10(p) : transform === "one_minus" ? 1 - p : p;
    const threshold = apply(pThresh);
    const keep = data.p_value.map(
      (p, i) => finite(p) && finite(data.estimate[i]) && (!grouped || data.group?.[i] != null)
    );
    if (!keep.some(Boolean)) {
      throw new Error(
        "A significance chart needs at least one complete estimate and p-value, with a display group when grouping."
      );
    }
    const adjusted = data.p_adjusted;
    const values = adjusted.map((p) => finite(p) ? apply(p) : Number.NaN);
    const capped = adjusted.map((p) => p === 0 && transform === "neg_log10");
    let cap = null;
    if (capped.some((c, i) => c && keep[i])) {
      let highest = Math.max(0, threshold);
      values.forEach((v, i) => {
        if (keep[i] && Number.isFinite(v) && v > highest) highest = v;
      });
      cap = config.zero_cap ?? Math.ceil(highest) + 1;
      if (cap <= highest) {
        throw new Error(
          "Set zero_cap above all finite displayed heights and the significance threshold."
        );
      }
      capped.forEach((c, i) => {
        if (c) values[i] = cap;
      });
    }
    const sig = data.p_value.map((_, i) => {
      const p = adjusted[i];
      if (!keep[i] || !finite(p) || p >= pThresh) return 1;
      return data.estimate[i] < xThresh ? 0 : 2;
    });
    const annotate = new Array(n).fill(false);
    if (volcano) {
      for (const side of [0, 2]) {
        const at = sig.flatMap((g, i) => keep[i] && g === side ? [i] : []);
        at.sort((a, b) => data.p_value[a] - data.p_value[b] || a - b);
        for (const i of at.slice(0, annotateN)) annotate[i] = true;
      }
    }
    const sign = appearance.signColors ?? SIGN_COLORS;
    let names = significanceGroupNames(xThresh);
    let colors = [
      config.negative_color ?? sign.negative,
      config.neutral_color ?? sign.neutral,
      config.positive_color ?? sign.positive
    ];
    let groupOf = sig;
    if (grouped) {
      const labels = data.group;
      const order2 = [...data.groups ?? []];
      labels.forEach((g, i) => {
        if (keep[i] && g != null && !order2.includes(g)) order2.push(g);
      });
      const present2 = order2.filter((g) => labels.some((l, i) => keep[i] && l === g));
      names = present2;
      groupOf = labels.map((g) => g == null ? -1 : present2.indexOf(g));
      const palette2 = asArray(config.palette);
      colors = present2.map((_, k) => palette2.length > 0 ? palette2[k % palette2.length] : void 0);
    }
    const shown = [...new Set(groupOf.filter((g, i) => keep[i] && g >= 0))].sort((a, b) => a - b);
    const dimensions2 = [
      volcano ? "Estimate" : "Position",
      "Display value",
      { name: "Estimate label", displayName: "Estimate", type: "ordinal" },
      { name: "Raw p label", displayName: "Raw p-value", type: "ordinal" },
      { name: "Adjusted p label", displayName: "Adjusted p-value", type: "ordinal" }
    ];
    const text = (v) => finite(v) ? String(formatNumber(v)) : "NA";
    const series = shown.map((g) => {
      const color = colors[g];
      const marks = groupOf.flatMap((gi, i) => {
        if (!keep[i] || gi !== g) return [];
        const item = {
          name: data.label[i],
          value: [
            volcano ? data.estimate[i] : i,
            values[i],
            text(data.estimate[i]),
            text(data.p_value[i]),
            text(adjusted[i])
          ]
        };
        if (volcano) {
          item.symbol = capped[i] ? "triangle" : "circle";
          if (annotate[i]) {
            item.label = { show: true, position: "top", formatter: "{b}", fontSize: 11 };
          }
        }
        return [item];
      });
      const itemStyle = { ...color ? { color } : {}, opacity: alpha };
      const common = {
        name: names[g],
        data: marks,
        itemStyle,
        dimensions: dimensions2,
        encode: { x: 0, y: 1, tooltip: [2, 3, 4] }
      };
      return volcano ? {
        type: "scatter",
        ...common,
        symbolSize: pointSize,
        rtemisRepelLabels: true,
        labelLine: { show: true, ...color ? { lineStyle: { color } } : {} }
      } : (
        // Disjoint groups share one category position: each outcome has one bar.
        { type: "bar", ...common, barGap: "-100%" }
      );
    });
    const thresholdLabel = `p < ${pThresh}`;
    if (reference) {
      const refs = [
        // Past the grid's right edge, where no mark sits under it.
        { yAxis: threshold, label: { formatter: thresholdLabel, position: "end" } }
      ];
      if (volcano) refs.push({ xAxis: xThresh, label: { show: false } });
      series.push({
        type: "scatter",
        data: [],
        silent: true,
        markLine: {
          silent: true,
          animation: false,
          symbol: "none",
          lineStyle: { color: "#888888", type: "dashed", width: 1 },
          data: refs
        }
      });
    }
    const cappedShown = capped.flatMap((c, i) => c && keep[i] ? [i] : []);
    if (!volcano && cappedShown.length > 0) {
      for (const g of [...new Set(cappedShown.map((i) => groupOf[i]))].sort(
        (a, b) => a - b
      )) {
        const color = colors[g];
        series.push({
          type: "scatter",
          name: names[g],
          data: cappedShown.filter((i) => groupOf[i] === g).map((i) => ({ value: [i, values[i]] })),
          itemStyle: { ...color ? { color } : {}, opacity: alpha },
          symbol: "triangle",
          symbolSize: pointSize,
          silent: true,
          z: 5
        });
      }
    }
    const note = cap !== null && cappedShown.length > 0 ? `p = 0 shown at ${cap} (triangles)` : null;
    const kept = (v) => v.filter((_, i) => keep[i]);
    const xRange = volcano ? config.xlim ?? calcLimits([...kept(data.estimate), ...reference ? [xThresh] : []], 0.12) : null;
    const yRange = config.ylim ?? calcLimits([0, ...kept(values), ...reference ? [threshold] : []], 0.12);
    if (cap !== null && cappedShown.length > 0 && yRange && (cap < yRange[0] || cap > yRange[1])) {
      throw new Error("Expand ylim to include zero_cap, or change the display transform.");
    }
    const grid = {
      left: config.margin_left ?? 60,
      right: config.margin_right ?? (reference ? referenceLabelMargin(thresholdLabel) : 36),
      top: config.margin_top ?? (note ? 70 : 42),
      bottom: config.margin_bottom ?? (legend ? 68 : 40)
    };
    const zoom = config.zoom ?? false;
    const xyZoomed = zoom && volcano ? xyZoom(true) : null;
    const dataZoom = zoom ? volcano ? xyZoomed?.dataZoom : axisZoom("x", grid) : void 0;
    const axisNames = significanceAxisNames(config);
    const xlab = config.xlab === void 0 ? axisNames.x : config.xlab;
    const ylab = config.ylab === void 0 ? axisNames.y : config.ylab;
    const splitLine = { showMinLine: false, showMaxLine: false };
    const palette = asArray(config.palette);
    const dense = !volcano && n > 40;
    return {
      ...palette.length > 0 ? { color: [...palette] } : {},
      ...config.title || note ? {
        title: {
          ...config.title ? { text: config.title } : {},
          ...note ? { subtext: note } : {},
          left: "center"
        }
      } : {},
      ...legend ? { legend: { data: shown.map((g) => names[g]), bottom: 0, itemGap: 20 } } : {},
      tooltip: { trigger: "item" },
      xAxis: volcano ? {
        type: "value",
        ...xlab ? { name: xlab } : {},
        nameLocation: "middle",
        scale: true,
        splitLine,
        ...xRange ? { min: xRange[0], max: xRange[1] } : {}
      } : {
        type: "category",
        ...xlab ? { name: xlab } : {},
        nameLocation: "middle",
        data: [...data.label],
        ...dense ? { axisLabel: { show: false }, axisTick: { show: false } } : {}
      },
      yAxis: {
        type: "value",
        ...ylab ? { name: ylab } : {},
        nameLocation: "middle",
        splitLine,
        ...yRange ? { min: yRange[0], max: yRange[1] } : {}
      },
      grid,
      ...dataZoom ? { dataZoom } : {},
      ...xyZoomed ? { toolbox: xyZoomed.toolbox } : {},
      series
    };
  }

  // src/charts/survival/renderers.ts
  var SURVIVAL_RISK = "rtemis.survival_risk.v1";
  var survivalRenderers = {
    // Data: [time, row]; row -1 is the caption. A row's series is its curve's,
    // and its text takes the curve's color. The counts sit under the time
    // axis's own coordinate of each time, so they stay aligned with its ticks
    // through resizing; the rows start `offset` pixels below the plot's bottom
    // edge, the coordinate of survival 0.
    [SURVIVAL_RISK]: (params, api) => {
      const p = params.itemPayload;
      const [x, edge] = api.coord([api.value(0), 0]);
      const top = edge + p.offset;
      const row = api.value(1);
      if (row < 0) {
        return {
          type: "text",
          x: p.captionX,
          y: top + p.rows * p.lineHeight / 2,
          rotation: Math.PI / 2,
          style: {
            text: p.caption,
            align: "center",
            verticalAlign: "middle",
            fontSize: p.fontSize,
            fill: p.textColor
          }
        };
      }
      return {
        type: "text",
        style: {
          x,
          y: top + (row + 0.5) * p.lineHeight,
          text: p.labels[params.dataIndex] ?? "",
          align: "center",
          verticalAlign: "middle",
          fontSize: p.fontSize,
          fill: api.visual("color")
        }
      };
    }
  };

  // src/charts/survival/config.ts
  var SURVIVAL_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "time",
    "survival",
    "group",
    "lower",
    "upper",
    "n_censor",
    "n_risk",
    "show_ci",
    "show_censors",
    "show_median",
    "landmarks",
    "risk_table",
    "ci_opacity",
    "line_width",
    "censor_size",
    "digits",
    "palette",
    "legend",
    "xlab",
    "ylab"
  ]);
  function survivalMedian(curve) {
    const tol = Math.sqrt(Number.EPSILON);
    const s = curve.survival;
    const t = curve.time;
    const i = s.findIndex((v) => v <= 0.5 + tol);
    if (i < 0) return null;
    if (Math.abs(s[i] - 0.5) > tol) return t[i];
    let end = t[t.length - 1];
    for (let k = i + 1; k < s.length; k++) {
      if (s[k] < 0.5 - tol) {
        end = t[k];
        break;
      }
    }
    return (t[i] + end) / 2;
  }
  var RISK_GAP = 12;
  var RISK_PAD = 10;
  function buildSurvivalOption(data, config, appearance = {}) {
    const curves = data.curves.filter((c) => c.time.length > 0);
    if (curves.length === 0) throw new Error("A survival chart needs at least one recorded curve.");
    for (const c of curves) {
      if (c.survival.length !== c.time.length) {
        throw new Error("A survival curve needs one survival value per time.");
      }
    }
    const digits = config.digits ?? 3;
    const fmt = (v) => ddSci(v, digits);
    const count = (v) => typeof v === "number" && Number.isFinite(v) ? String(v) : "NA";
    const palette = asArray(config.palette);
    const colors = palette.length > 0 ? palette : appearance.palette ?? [];
    const colorOf = (i) => colors.length > 0 ? colors[i % colors.length] : void 0;
    const styled = (color) => color ? { color } : {};
    const names = curves.map((c, i) => c.name ?? (curves.length > 1 ? `Curve ${i + 1}` : "Survival"));
    let lo = Number.POSITIVE_INFINITY;
    let hi = Number.NEGATIVE_INFINITY;
    for (const c of curves) {
      lo = Math.min(lo, c.time[0]);
      hi = Math.max(hi, c.time[c.time.length - 1]);
    }
    const domain = hi > lo ? [lo, hi] : [lo - 0.5, hi + 0.5];
    const dimensions2 = [
      "time",
      "survival",
      ...["Survival", "Lower", "Upper", "At risk", "Censored"].map((name) => ({
        name,
        type: "ordinal"
      }))
    ];
    const encode = { x: 0, y: 1, tooltip: [0, 2, 3, 4, 5, 6] };
    const series = [];
    const showMedian = config.show_median ?? true;
    const landmarks = [...new Set(asArray(config.landmarks))].sort((a, b) => a - b);
    curves.forEach((c, i) => {
      const name = names[i];
      const color = colorOf(i);
      const values = c.time.map((t, j) => [
        t,
        c.survival[j],
        fmt(c.survival[j]),
        fmt(c.lower?.[j]),
        fmt(c.upper?.[j]),
        count(c.n_risk?.[j]),
        count(c.n_censor?.[j])
      ]);
      series.push({
        type: "line",
        name,
        data: values,
        step: "end",
        smooth: false,
        showSymbol: true,
        symbol: "circle",
        symbolSize: 6,
        clip: false,
        lineStyle: { ...styled(color), width: config.line_width ?? 2 },
        // Transparent symbols are the hover targets of each step.
        itemStyle: { ...styled(color), opacity: c.time.length === 1 ? 1 : 0 },
        emphasis: { itemStyle: { opacity: 1 } },
        dimensions: dimensions2,
        encode,
        z: 3
      });
      if ((config.show_ci ?? true) && c.lower && c.upper) {
        const lower = c.lower;
        const upper = c.upper;
        const known = c.time.map((_, j) => lower[j] != null && upper[j] != null);
        for (const band of ["lower", "width"]) {
          const at = (j) => band === "lower" ? lower[j] : upper[j] - lower[j];
          const points = [];
          c.time.forEach((t, j) => {
            if (j > 0 && known[j - 1] && !known[j]) points.push([t, at(j - 1)]);
            points.push([t, known[j] ? at(j) : null]);
          });
          series.push({
            type: "line",
            name,
            step: "end",
            stack: `ci-${i}`,
            stackStrategy: "all",
            data: points,
            showSymbol: false,
            connectNulls: false,
            silent: true,
            legendHoverLink: false,
            itemStyle: styled(color),
            lineStyle: { opacity: 0 },
            areaStyle: {
              ...styled(color),
              opacity: band === "lower" ? 0 : config.ci_opacity ?? 0.15
            },
            z: 1
          });
        }
      }
      if ((config.show_censors ?? true) && c.n_censor) {
        const censored = values.filter((_, j) => (c.n_censor?.[j] ?? 0) > 0);
        if (censored.length > 0) {
          series.push({
            type: "scatter",
            name,
            data: censored,
            symbol: "rect",
            symbolSize: [1.5, config.censor_size ?? 8],
            clip: false,
            itemStyle: styled(color),
            dimensions: dimensions2,
            encode,
            z: 4
          });
        }
      }
      const median = showMedian ? survivalMedian(c) : null;
      if (median !== null) {
        series.push({
          type: "line",
          name,
          data: [
            [domain[0], 0.5],
            [median, 0.5],
            [median, 0]
          ],
          showSymbol: false,
          silent: true,
          legendHoverLink: false,
          itemStyle: styled(color),
          lineStyle: { ...styled(color), type: "dashed", opacity: 0.7 },
          z: 2
        });
        series.push({
          type: "scatter",
          name,
          data: [{ value: [median, 0], name: `Median ${formatNumber(median)}` }],
          symbolSize: 0,
          silent: true,
          legendHoverLink: false,
          clip: false,
          itemStyle: styled(color),
          // Upright beside the vertical dropline, rising from the time axis, so
          // the labels of different medians stay apart.
          label: {
            show: true,
            position: "top",
            distance: 4,
            rotate: 90,
            align: "left",
            verticalAlign: "bottom",
            formatter: "{b}",
            color: "inherit",
            fontSize: 11
          },
          z: 5
        });
      }
      const shown = landmarks.filter(
        (t) => t >= c.time[0] && t <= c.time[c.time.length - 1]
      );
      if (shown.length > 0) {
        series.push({
          type: "scatter",
          name,
          data: shown.map((t) => {
            let s = c.survival[0];
            c.time.forEach((u, j) => {
              if (u <= t) s = c.survival[j];
            });
            return { value: [t, s], name: `${Math.round(s * 100)}%` };
          }),
          symbolSize: 6,
          clip: false,
          itemStyle: styled(color),
          label: { show: true, position: "top", formatter: "{b}", color: "inherit" },
          z: 5
        });
      }
    });
    const references = landmarks.filter((t) => t >= domain[0] && t <= domain[1]);
    if (references.length > 0) {
      series.push({
        type: "line",
        data: [],
        silent: true,
        markLine: {
          silent: true,
          animation: false,
          symbol: "none",
          lineStyle: { color: "#888888", type: "dashed", width: 1, opacity: 0.7 },
          label: { show: false },
          data: references.map((t) => ({ xAxis: t }))
        }
      });
    }
    const risk = config.risk_table ? data.risk : null;
    if (config.risk_table && !risk) {
      throw new Error("Supply risk records to draw the number-at-risk table.");
    }
    const grid = {
      left: 70,
      right: 30,
      top: config.title ? 50 : 25,
      bottom: 60,
      containLabel: false
    };
    if (risk) {
      if (risk.counts.length !== curves.length || risk.counts.some((r) => r.length !== risk.time.length)) {
        throw new Error("Supply one risk count per curve and risk time.");
      }
      const base = {
        offset: 0,
        lineHeight: 0,
        rows: curves.length,
        fontSize: 0,
        caption: "At risk",
        // Where the y axis name sits: its gap left of the grid.
        captionX: grid.left - 45 + 4,
        textColor: appearance.textColor ?? (appearance.dark ? darkThemeTokens() : lightThemeTokens()).fg_color
      };
      curves.forEach((_, i) => {
        const payload = {
          ...base,
          labels: risk.time.map((_2, k) => count(risk.counts[i]?.[k]))
        };
        series.push({
          type: "custom",
          name: names[i],
          renderItem: SURVIVAL_RISK,
          clip: false,
          silent: true,
          legendHoverLink: false,
          tooltip: { show: false },
          itemStyle: styled(colorOf(i)),
          itemPayload: payload,
          data: risk.time.map((t) => [t, i]),
          z: 5
        });
      });
      series.push({
        type: "custom",
        renderItem: SURVIVAL_RISK,
        clip: false,
        silent: true,
        tooltip: { show: false },
        itemPayload: { ...base, labels: [] },
        data: [[domain[0], -1]],
        z: 5
      });
    }
    const legendShown = (config.legend ?? true) && !(curves.length === 1 && curves[0]?.name == null);
    const riskTimes = risk ? [...risk.time] : null;
    return layoutSurvivalRisk({
      ...config.title ? { title: { text: config.title, left: "center", textAlign: "center" } } : {},
      grid,
      xAxis: {
        type: "value",
        min: domain[0],
        max: domain[1],
        name: config.xlab ?? "Time",
        nameLocation: "middle",
        nameGap: 30,
        // With the strip, the axis labels exactly the risk times.
        ...riskTimes ? { axisLabel: { customValues: riskTimes }, axisTick: { customValues: riskTimes } } : {}
      },
      yAxis: {
        type: "value",
        min: 0,
        max: 1,
        name: config.ylab ?? "Survival probability",
        nameLocation: "middle",
        nameGap: 45
      },
      legend: {
        show: legendShown,
        data: names,
        padding: 0,
        itemStyle: { opacity: 1 },
        textStyle: { fontSize: 12, lineHeight: 14 }
      },
      tooltip: { trigger: "item", confine: true },
      series
    });
  }
  function layoutSurvivalRisk(option) {
    const series = Array.isArray(option.series) ? option.series : [];
    const strips = series.filter((s) => s?.renderItem === SURVIVAL_RISK);
    if (strips.length === 0) return option;
    const x = Array.isArray(option.xAxis) ? option.xAxis[0] : option.xAxis;
    const size = (style) => {
      const v = style?.fontSize;
      return typeof v === "number" ? v : 12;
    };
    const nameGap = typeof x?.nameGap === "number" ? x.nameGap : 30;
    const fontSize = size(x?.axisLabel);
    const margin = x?.nameTextStyle?.textMargin;
    const [above, below] = Array.isArray(margin) ? [Number(margin[0]) || 0, Number(margin[2] ?? margin[0]) || 0] : typeof margin === "number" ? [margin, margin] : [0, 0];
    const offset = nameGap + above + Math.ceil(size(x?.nameTextStyle) * 1.3) + below + RISK_GAP;
    const lineHeight = Math.round(fontSize * 1.6);
    let rows = 0;
    for (const s of strips) {
      const payload = s.itemPayload;
      rows = payload.rows;
      s.itemPayload = { ...payload, offset, lineHeight, fontSize };
    }
    if (x) x.axisLabel = { ...x.axisLabel, showMinLabel: true, showMaxLabel: true };
    const grid = Array.isArray(option.grid) ? option.grid[0] : option.grid;
    if (grid) grid.bottom = offset + rows * lineHeight + RISK_PAD;
    return option;
  }

  // src/charts/timeseries/renderers.ts
  var AXIS_LABELS = "rtemis.axis_labels.v1";
  var timeSeriesRenderers = {
    // Data: [time, text]. Each label sits 10 pixels below the plot under its
    // time's coordinate, so irregular spacing stays true through zoom and
    // resizing; a time outside the plot draws nothing.
    [AXIS_LABELS]: (params, api) => {
      const [x] = api.coord([api.value(0), 0]);
      const box = params.coordSys;
      if (x < box.x - 1 || x > box.x + box.width + 1) return void 0;
      return {
        type: "text",
        style: {
          x,
          y: box.y + box.height + 10,
          text: String(api.value(1)),
          align: "center",
          verticalAlign: "top",
          fill: "#888888",
          font: api.font({ fontSize: 12 })
        }
      };
    }
  };

  // src/charts/timeseries/config.ts
  var TIMESERIES_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "x",
    "y",
    "group",
    "xlab",
    "ylab",
    "palette",
    "zoom",
    "x2",
    "zt",
    "shade_bin",
    "show_zt_every",
    "y2",
    "y2lab",
    "window",
    "roll_fn",
    "align",
    "min_obs",
    "roll_width",
    "roll_color",
    "raw_opacity",
    "points"
  ]);
  var SUMMARIES = {
    mean: (v) => v.reduce((a, b) => a + b, 0) / v.length,
    median: (v) => {
      const s = [...v].sort((a, b) => a - b);
      const m = s.length >> 1;
      return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
    },
    min: (v) => Math.min(...v),
    max: (v) => Math.max(...v),
    sum: (v) => v.reduce((a, b) => a + b, 0)
  };
  function rollingValues(values, config) {
    const fn = config.roll_fn ?? "none";
    if (fn === "none") return values.map(() => null);
    const width = config.window ?? 7;
    const required = config.min_obs ?? (fn === "sum" ? width : Math.floor(width / 2) + 1);
    const align = config.align ?? "center";
    const offset = align === "right" ? width - 1 : align === "left" ? 0 : Math.floor((width - 1) / 2);
    return values.map((_, i) => {
      const start = Math.max(i - offset, 0);
      const end = Math.min(i - offset + width - 1, values.length - 1);
      const present2 = [];
      for (let k = start; k <= end; k++) {
        const v = values[k];
        if (typeof v === "number") present2.push(v);
      }
      return present2.length < required ? null : SUMMARIES[fn](present2);
    });
  }
  function samples(data, config) {
    const x = config.x;
    const yColumns = asArray(config.y);
    if (!x || yColumns.length === 0) {
      throw new Error("A time series chart needs both `x` and `y` set to draw.");
    }
    if (config.group && yColumns.length > 1) {
      throw new Error("`group` splits one `y` column: bind one `y` column, or unset `group`.");
    }
    const times = (name, member) => {
      const time = isTime(data, name);
      const values = column(data.columns, name, member).map((v, i) => {
        const t = time ? timeMs(v) : typeof v === "number" && !Number.isNaN(v) ? v : null;
        if (t !== null && !Number.isFinite(t)) {
          throw new Error(
            `Column '${name}' (config member '${member}') must hold finite times; row ${i + 1} holds '${String(v)}'.`
          );
        }
        return t;
      });
      return { time, values };
    };
    const numbers = (name, member) => column(data.columns, name, member).map((v, i) => {
      if (v === null || v === void 0 || typeof v === "number" && Number.isNaN(v)) return null;
      if (typeof v !== "number" || !Number.isFinite(v)) {
        throw new Error(
          `Column '${name}' (config member '${member}') must hold finite numbers or missing values; row ${i + 1} holds '${String(v)}'.`
        );
      }
      return v;
    });
    const sample = (name, axis, xs, ys, keep) => {
      const rows = xs.flatMap((t, i) => t !== null && keep(i) ? [i] : []);
      rows.sort((a, b) => xs[a] - xs[b]);
      return { name, axis, x: rows.map((i) => xs[i]), y: rows.map((i) => ys[i] ?? null) };
    };
    const primaryX = times(x, "x");
    let primary;
    if (config.group) {
      const groups = textColumn(data, config.group, "group");
      const raw = column(data.columns, config.group, "group");
      const present2 = (i) => raw[i] !== null && raw[i] !== void 0;
      const levels = distinct(
        groups.filter((_, i) => present2(i)),
        data.levels?.[config.group]
      );
      const ys = numbers(yColumns[0], "y");
      primary = levels.map(
        (level) => sample(level, 0, primaryX.values, ys, (i) => present2(i) && groups[i] === level)
      );
    } else {
      primary = yColumns.map(
        (name) => sample(name, 0, primaryX.values, numbers(name, "y"), () => true)
      );
    }
    const y2Columns = asArray(config.y2);
    if (config.x2 && y2Columns.length === 0) {
      throw new Error("Supply `y2` with `x2`, or unset both.");
    }
    const secondaryX = config.x2 ? times(config.x2, "x2") : primaryX;
    if (secondaryX.time !== primaryX.time) {
      throw new Error("Use numeric time throughout, or dates and date-times throughout.");
    }
    const taken = new Set(primary.map((s) => s.name));
    const secondary = y2Columns.map(
      (name) => sample(
        taken.has(name) ? `${name} (right)` : name,
        1,
        secondaryX.values,
        numbers(name, "y2"),
        () => true
      )
    );
    if (![...primary, ...secondary].some((s) => s.y.some((v) => v !== null))) {
      throw new Error("A time series chart needs at least one time with a value.");
    }
    return { primary, secondary, time: primaryX.time };
  }
  function sortedRows(xs) {
    const rows = xs.flatMap((t, i) => t === null ? [] : [i]);
    return rows.sort((a, b) => xs[a] - xs[b]);
  }
  function buildTimeSeriesOption(data, config) {
    const { primary, secondary, time } = samples(data, config);
    const points = config.points ?? true;
    const rawOpacity = config.raw_opacity ?? 0.5;
    const pair = (xs2, ys) => xs2.map((t, i) => [t, ys[i] ?? null]);
    const series = [];
    for (const s of [...primary, ...secondary]) {
      const raw = {
        type: "line",
        name: s.name,
        data: pair(s.x, s.y),
        showSymbol: points,
        symbolSize: 5,
        lineStyle: { width: points ? 0 : 1, opacity: rawOpacity },
        itemStyle: { opacity: rawOpacity },
        yAxisIndex: s.axis
      };
      series.push(raw);
      if ((config.roll_fn ?? "none") !== "none") {
        const rolled = rollingValues(s.y, config);
        if (rolled.some((v) => v !== null && !Number.isFinite(v))) {
          throw new Error("Rescale values to keep rolling summaries finite.");
        }
        series.push({
          ...raw,
          data: pair(s.x, rolled),
          showSymbol: false,
          lineStyle: {
            width: config.roll_width ?? 2,
            ...config.roll_color ? { color: config.roll_color } : {}
          },
          itemStyle: { opacity: 1 }
        });
      }
    }
    const xAxis = {
      type: time ? "time" : "value",
      scale: true,
      ...config.xlab ? { name: config.xlab, nameLocation: "middle" } : {}
    };
    if (config.zt || config.shade_bin) {
      if (config.x2) {
        throw new Error("Zeitgeber labels and shading follow `x`: unset `x2`.");
      }
    }
    const xs = time ? column(data.columns, config.x, "x").map(timeMs) : column(data.columns, config.x, "x").map(
      (v) => typeof v === "number" && Number.isFinite(v) ? v : null
    );
    const order2 = sortedRows(xs);
    if (config.zt) {
      const every = config.show_zt_every ?? 1;
      const labels = column(data.columns, config.zt, "zt");
      labels.forEach((v, i) => {
        if (typeof v !== "number" || !Number.isFinite(v)) {
          throw new Error(
            `Column '${config.zt}' (config member 'zt') must hold a finite number in every row; row ${i + 1} holds '${String(v)}'.`
          );
        }
      });
      const keep = order2.filter((row, k) => k % every === 0 || labels[row] === 0);
      xAxis.axisLabel = { show: false };
      series.push({
        type: "custom",
        name: "",
        renderItem: AXIS_LABELS,
        silent: true,
        clip: false,
        dimensions: [
          { name: "time", type: "float" },
          { name: "text", type: "ordinal" }
        ],
        encode: { x: 0, y: -1, tooltip: -1 },
        data: keep.map((row) => [xs[row], String(labels[row])])
      });
    }
    if (config.shade_bin) {
      const flags = column(data.columns, config.shade_bin, "shade_bin");
      flags.forEach((v, i) => {
        if (v !== 0 && v !== 1) {
          throw new Error(
            `Column '${config.shade_bin}' (config member 'shade_bin') must hold 0 or 1 in every row; row ${i + 1} holds '${String(v)}'.`
          );
        }
      });
      const runs = [];
      order2.forEach((row, k) => {
        const on = flags[row] === 1;
        if (runs[runs.length - 1]?.on !== on) runs.push({ on, start: k });
      });
      const area = runs.flatMap((run, r) => {
        if (!run.on) return [];
        const next = runs[r + 1];
        const end = next ? next.start - 1 : order2.length - 1;
        return [
          [
            { xAxis: xs[order2[run.start]], itemStyle: { color: "#888888", opacity: 0.15 } },
            { xAxis: xs[order2[end]] }
          ]
        ];
      });
      if (area.length > 0 && series[0]) {
        series[0].markArea = { silent: true, label: { show: false }, data: area };
      }
    }
    const yAxis = [
      {
        type: "value",
        scale: true,
        ...config.ylab ? { name: config.ylab, nameLocation: "middle" } : {}
      }
    ];
    if (secondary.length > 0) {
      yAxis.push({
        type: "value",
        scale: true,
        position: "right",
        ...config.y2lab ? { name: config.y2lab, nameLocation: "middle" } : {},
        splitLine: { show: false }
      });
    }
    const grid = cartesianGrid({}, Boolean(config.xlab || config.ylab || config.y2lab));
    const dataZoom = config.zoom ? axisZoom("x", grid) : void 0;
    const palette = asArray(config.palette);
    return {
      ...config.title ? { title: { text: config.title } } : {},
      ...time ? { useUTC: true } : {},
      ...palette.length > 0 ? { color: [...palette] } : {},
      tooltip: { trigger: "axis", valueFormatter: formatNumber },
      legend: {
        data: [...primary, ...secondary].map((s) => s.name),
        itemStyle: { opacity: 1 }
      },
      grid,
      ...dataZoom ? { dataZoom } : {},
      xAxis,
      // One left axis is an object, as in the other builders; with `y2`, the
      // two axes are an array.
      yAxis: yAxis.length === 1 ? yAxis[0] : yAxis,
      series
    };
  }

  // src/shared/labels.ts
  function valueLabel(config, name = (column2) => column2) {
    const ys = asArray(config.y);
    if (ys.length === 0) return "Count";
    const only = ys.length === 1 ? name(ys[0]) : null;
    switch (config.aggregate ?? "none") {
      case "mean":
        return only === null ? "Mean" : `Mean ${only}`;
      case "median":
        return only === null ? "Median" : `Median ${only}`;
      case "sum":
        return only === null ? "Sum" : `Sum of ${only}`;
      default:
        return only;
    }
  }

  // src/shared/layers.ts
  function appendLayers(option, layers) {
    if (!layers || layers.length === 0) return option;
    const series = option.series === void 0 ? [] : [option.series].flat();
    const out = {
      ...option,
      series: [...series, ...layers.flatMap((layer) => layer.series)]
    };
    const names = layers.flatMap((layer) => layer.legend ? [layer.legend] : []);
    if (names.length > 0) {
      const legend = option.legend ?? {};
      out.legend = Array.isArray(legend.data) ? { ...legend, data: [.../* @__PURE__ */ new Set([...legend.data, ...names])] } : { ...legend, show: true };
    }
    return out;
  }

  // src/shared/renderers.ts
  var renderers = Object.freeze({
    ...boxplotRenderers,
    ...ganttRenderers,
    ...histogramRenderers,
    ...scatterRenderers,
    ...survivalRenderers,
    ...timeSeriesRenderers
  });
  var RENDERER_NAMES = Object.freeze(Object.keys(renderers));
  function registerRenderers(echarts) {
    for (const [name, render] of Object.entries(renderers)) {
      echarts.registerCustomSeries(name, render);
    }
  }

  // src/shared/repel.ts
  var overlaps = (a, b) => a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height;
  function repelLabels() {
    let placed = [];
    let seen = /* @__PURE__ */ new Set();
    let clearQueued = false;
    const clear = () => {
      placed = [];
      seen = /* @__PURE__ */ new Set();
    };
    return (params) => {
      const key = `${params.seriesIndex}:${params.dataIndex}`;
      if (seen.has(key)) clear();
      seen.add(key);
      if (!clearQueued) {
        clearQueued = true;
        void Promise.resolve().then(() => {
          clear();
          clearQueued = false;
        });
      }
      const host = params.rect;
      const label = params.labelRect;
      const w = label.width;
      const h = label.height;
      const gap = 2;
      const step = h + gap;
      const cx = host.x + host.width / 2;
      const cy = host.y + host.height / 2;
      const above = host.y - gap - h;
      const below = host.y + host.height + gap;
      const right = host.x + host.width + gap;
      const left = host.x - gap - w;
      const candidates = [
        { x: label.x, y: label.y },
        { x: cx - w / 2, y: below },
        { x: right, y: cy - h / 2 },
        { x: left, y: cy - h / 2 }
      ];
      for (let k = 1; k <= 6; k++) {
        candidates.push(
          { x: cx - w / 2, y: above - k * step },
          { x: cx - w / 2, y: below + k * step },
          { x: right, y: cy - h / 2 - k * step },
          { x: left, y: cy - h / 2 - k * step },
          { x: right, y: cy - h / 2 + k * step },
          { x: left, y: cy - h / 2 + k * step }
        );
      }
      placed.push({ x: host.x, y: host.y, width: host.width, height: host.height });
      const fits = (c) => c.x >= 0 && c.y >= 0 && placed.every((p) => !overlaps({ x: c.x, y: c.y, width: w, height: h }, p));
      const spot = candidates.find(fits) ?? candidates[0];
      placed.push({ x: spot.x, y: spot.y, width: w, height: h });
      return { x: spot.x, y: spot.y, align: "left", verticalAlign: "top" };
    };
  }
  function applyRepelLabels(option) {
    const series = option.series;
    if (!Array.isArray(series)) return option;
    let repel = null;
    for (const s of series) {
      if (!s?.rtemisRepelLabels) continue;
      repel = repel ?? repelLabels();
      s.labelLayout = repel;
    }
    return option;
  }

  // src/index.ts
  var builders = Object.freeze({
    a3: buildA3Option,
    bar: buildBarOption,
    boxplot: buildBoxplotOption,
    confusion: buildConfusionOption,
    density: buildDensityOption,
    gantt: buildGanttOption,
    histogram: buildHistogramOption,
    line: buildLineOption,
    pr: buildPrOption,
    roc: buildRocOption,
    sankey: buildSankeyOption,
    scatter: buildScatterOption,
    significance: buildSignificanceOption,
    survival: buildSurvivalOption,
    timeseries: buildTimeSeriesOption
  });
  var aspectHints = Object.freeze({
    line: lineAspect,
    pr: curveAspect,
    roc: curveAspect,
    scatter: lineAspect
  });
  return __toCommonJS(index_exports);
})();
if (typeof module === 'object' && module.exports) module.exports = RtemisDraw;
