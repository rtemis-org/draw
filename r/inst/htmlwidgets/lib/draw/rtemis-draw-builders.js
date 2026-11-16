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
    DEFAULT_FONT_FAMILY: () => DEFAULT_FONT_FAMILY,
    DEFAULT_MARGIN: () => DEFAULT_MARGIN,
    DEFAULT_PAD: () => DEFAULT_PAD,
    LINE_CHART_CONFIG_MEMBERS: () => LINE_CHART_CONFIG_MEMBERS,
    RENDERER_NAMES: () => RENDERER_NAMES,
    RTEMIS_PALETTE: () => RTEMIS_PALETTE,
    SIGNIFICANT_DIGITS: () => SIGNIFICANT_DIGITS,
    SIGN_COLORS: () => SIGN_COLORS,
    ZOOM_SLIDER_ID: () => ZOOM_SLIDER_ID,
    a3SurfaceHints: () => a3SurfaceHints,
    aspectHints: () => aspectHints,
    buildA3Option: () => buildA3Option,
    buildBarOption: () => buildBarOption,
    buildBoxplotOption: () => buildBoxplotOption,
    buildLineOption: () => buildLineOption,
    builders: () => builders,
    calcLimits: () => calcLimits,
    calcLimitsFromZero: () => calcLimitsFromZero,
    darkThemeTokens: () => darkThemeTokens,
    fontStack: () => fontStack,
    formatNumber: () => formatNumber,
    lightThemeTokens: () => lightThemeTokens,
    lineAspect: () => lineAspect,
    radicalInverse: () => radicalInverse,
    registerRenderers: () => registerRenderers,
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
  function axisZoom(axis, grid) {
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
        [side]: margin + SLIDER_HANDLE,
        ...axis === "x" ? { height: SLIDER_SIZE } : { width: SLIDER_SIZE }
      },
      { type: "inside", ...index, zoomOnMouseWheel: "shift", moveOnMouseMove: true }
    ];
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
  var renderers = {
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
  var RENDERER_NAMES = Object.freeze(Object.keys(renderers));
  function registerRenderers(echarts) {
    for (const [name, render] of Object.entries(renderers)) {
      echarts.registerCustomSeries(name, render);
    }
  }

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
    "legend_title"
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
    const palette = asArray(config.palette);
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
      return {
        type: "line",
        ...series.length === 1 ? {} : { name: s.name },
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
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "axis", valueFormatter: formatNumber },
      ...series.length > 1 ? { legend: { data: series.map((s) => s.name) } } : {},
      xAxis,
      yAxis,
      grid,
      ...dataZoom ? { dataZoom } : {},
      ...xType === "time" ? { useUTC: true } : {},
      series: builtSeries,
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
      return yColumns.map((y) => ({ name: y, rows: allRows, y: select(y, allRows) }));
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

  // src/index.ts
  var builders = Object.freeze({
    a3: buildA3Option,
    bar: buildBarOption,
    boxplot: buildBoxplotOption,
    line: buildLineOption
  });
  var aspectHints = Object.freeze({
    line: lineAspect
  });
  return __toCommonJS(index_exports);
})();
if (typeof module === 'object' && module.exports) module.exports = RtemisDraw;
