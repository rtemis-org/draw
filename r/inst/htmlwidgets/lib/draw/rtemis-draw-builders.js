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
    a3SurfaceHints: () => a3SurfaceHints,
    buildA3Option: () => buildA3Option,
    builders: () => builders
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

  // src/index.ts
  var builders = Object.freeze({
    a3: buildA3Option
  });
  return __toCommonJS(index_exports);
})();
if (typeof module === 'object' && module.exports) module.exports = RtemisDraw;
