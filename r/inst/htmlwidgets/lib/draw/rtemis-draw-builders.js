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
    AXIS_LABEL_INSET_EM: () => AXIS_LABEL_INSET_EM,
    BAR_CHART_CONFIG_MEMBERS: () => BAR_CHART_CONFIG_MEMBERS,
    BOXPLOT_CHART_CONFIG_MEMBERS: () => BOXPLOT_CHART_CONFIG_MEMBERS,
    CALIBRATION_CHART_CONFIG_MEMBERS: () => CALIBRATION_CHART_CONFIG_MEMBERS,
    CHOROPLETH_CHART_CONFIG_MEMBERS: () => CHOROPLETH_CHART_CONFIG_MEMBERS,
    CHOROPLETH_COLORMAPS: () => CHOROPLETH_COLORMAPS,
    CHOROPLETH_COLORS: () => CHOROPLETH_COLORS,
    CHOROPLETH_GEOMETRY: () => CHOROPLETH_GEOMETRY,
    COLORMAPS: () => COLORMAPS,
    CONFUSION_CHART_CONFIG_MEMBERS: () => CONFUSION_CHART_CONFIG_MEMBERS,
    DEFAULT_FONT_FAMILY: () => DEFAULT_FONT_FAMILY,
    DEFAULT_MARGIN: () => DEFAULT_MARGIN,
    DEFAULT_PAD: () => DEFAULT_PAD,
    DEFAULT_POINT_SIZE: () => DEFAULT_POINT_SIZE,
    DENSITY_CHART_CONFIG_MEMBERS: () => DENSITY_CHART_CONFIG_MEMBERS,
    DENSITY_VALUE_LABEL: () => DENSITY_VALUE_LABEL,
    DIAGRAM_GRAYS: () => DIAGRAM_GRAYS,
    DIAGRAM_SERIES: () => DIAGRAM_SERIES,
    FIT_EVENTS: () => FIT_EVENTS,
    FIT_MEMBERS: () => FIT_MEMBERS,
    FUNNEL_CHART_CONFIG_MEMBERS: () => FUNNEL_CHART_CONFIG_MEMBERS,
    GANTT_CHART_CONFIG_MEMBERS: () => GANTT_CHART_CONFIG_MEMBERS,
    GANTT_ROW_PX: () => GANTT_ROW_PX,
    HEATMAP_CHART_CONFIG_MEMBERS: () => HEATMAP_CHART_CONFIG_MEMBERS,
    HISTOGRAM_CHART_CONFIG_MEMBERS: () => HISTOGRAM_CHART_CONFIG_MEMBERS,
    KEY_GAP: () => KEY_GAP,
    LEGEND_TITLE_ID: () => LEGEND_TITLE_ID,
    LINE_CHART_CONFIG_MEMBERS: () => LINE_CHART_CONFIG_MEMBERS,
    MAX_SYMBOL_SIZE: () => MAX_SYMBOL_SIZE,
    NETWORK_CHART_CONFIG_MEMBERS: () => NETWORK_CHART_CONFIG_MEMBERS,
    NEUTRAL_EDGE_COLOR: () => NEUTRAL_EDGE_COLOR,
    OBSERVATIONS: () => OBSERVATIONS,
    PIE_CHART_CONFIG_MEMBERS: () => PIE_CHART_CONFIG_MEMBERS,
    PR_CHART_CONFIG_MEMBERS: () => PR_CHART_CONFIG_MEMBERS,
    RADAR_ALL_ROWS: () => RADAR_ALL_ROWS,
    RADAR_CHART_CONFIG_MEMBERS: () => RADAR_CHART_CONFIG_MEMBERS,
    RENDERER_NAMES: () => RENDERER_NAMES,
    ROC_CHART_CONFIG_MEMBERS: () => ROC_CHART_CONFIG_MEMBERS,
    RTEMIS_PALETTE: () => RTEMIS_PALETTE,
    SANKEY_CHART_CONFIG_MEMBERS: () => SANKEY_CHART_CONFIG_MEMBERS,
    SCATTER3D_CHART_CONFIG_MEMBERS: () => SCATTER3D_CHART_CONFIG_MEMBERS,
    SCATTER_CHART_CONFIG_MEMBERS: () => SCATTER_CHART_CONFIG_MEMBERS,
    SERIES_THEME: () => SERIES_THEME,
    SIGNIFICANCE_CHART_CONFIG_MEMBERS: () => SIGNIFICANCE_CHART_CONFIG_MEMBERS,
    SIGNIFICANT_DIGITS: () => SIGNIFICANT_DIGITS,
    SIGN_COLORS: () => SIGN_COLORS,
    SIZE_KEY_ID: () => SIZE_KEY_ID,
    SPECTROGRAM_CHART_CONFIG_MEMBERS: () => SPECTROGRAM_CHART_CONFIG_MEMBERS,
    SUNBURST_CHART_CONFIG_MEMBERS: () => SUNBURST_CHART_CONFIG_MEMBERS,
    SURVIVAL_CHART_CONFIG_MEMBERS: () => SURVIVAL_CHART_CONFIG_MEMBERS,
    THEME_DOCUMENT_MEMBERS: () => THEME_DOCUMENT_MEMBERS,
    TIMESERIES_CHART_CONFIG_MEMBERS: () => TIMESERIES_CHART_CONFIG_MEMBERS,
    TREEMAP_CHART_CONFIG_MEMBERS: () => TREEMAP_CHART_CONFIG_MEMBERS,
    ZOOM_SLIDER_ID: () => ZOOM_SLIDER_ID,
    a3SurfaceHints: () => a3SurfaceHints,
    appendLayers: () => appendLayers,
    applyRepelLabels: () => applyRepelLabels,
    aspectHints: () => aspectHints,
    attachLabelLayouts: () => attachLabelLayouts,
    autoAlpha: () => autoAlpha,
    axisLabelInset: () => axisLabelInset,
    axisTooltip: () => axisTooltip,
    boxplotStats: () => boxplotStats,
    branchColors: () => branchColors,
    buildA3Option: () => buildA3Option,
    buildBarOption: () => buildBarOption,
    buildBoxplotOption: () => buildBoxplotOption,
    buildCalibrationOption: () => buildCalibrationOption,
    buildChoroplethScale: () => buildChoroplethScale,
    buildConfusionOption: () => buildConfusionOption,
    buildDensityOption: () => buildDensityOption,
    buildFunnelOption: () => buildFunnelOption,
    buildGanttOption: () => buildGanttOption,
    buildHeatmapOption: () => buildHeatmapOption,
    buildHistogramOption: () => buildHistogramOption,
    buildLineOption: () => buildLineOption,
    buildNetworkGraph: () => buildNetworkGraph,
    buildPieOption: () => buildPieOption,
    buildPrOption: () => buildPrOption,
    buildRadarOption: () => buildRadarOption,
    buildRocOption: () => buildRocOption,
    buildSankeyOption: () => buildSankeyOption,
    buildScatter3DOption: () => buildScatter3DOption,
    buildScatterOption: () => buildScatterOption,
    buildSignificanceOption: () => buildSignificanceOption,
    buildSpectrogramOption: () => buildSpectrogramOption,
    buildSunburstOption: () => buildSunburstOption,
    buildSurvivalOption: () => buildSurvivalOption,
    buildTimeSeriesOption: () => buildTimeSeriesOption,
    buildTreemapOption: () => buildTreemapOption,
    builders: () => builders,
    calcLimits: () => calcLimits,
    calcLimitsFromZero: () => calcLimitsFromZero,
    centerColorScales: () => centerColorScales,
    choroplethColor: () => choroplethColor,
    choroplethFillExpression: () => choroplethFillExpression,
    choroplethTheme: () => choroplethTheme,
    classCount: () => classCount,
    colorRamp: () => colorRamp,
    confusionHeight: () => confusionHeight,
    confusionLayout: () => confusionLayout,
    confusionRates: () => confusionRates,
    curveAspect: () => curveAspect,
    curveLabel: () => curveLabel,
    curveLegendTitle: () => curveLegendTitle,
    dark3: () => dark3,
    darkThemeTokens: () => darkThemeTokens,
    ddSci: () => ddSci,
    diagramTheme: () => diagramTheme,
    divergingRamp: () => divergingRamp,
    drawLegendTitle: () => drawLegendTitle,
    fit: () => fit,
    fitA3: () => fitA3,
    fitAspectAxes: () => fitAspectAxes,
    fitAspectBox: () => fitAspectBox,
    fitConfusion: () => fitConfusion,
    fitEndpointLabels: () => fitEndpointLabels,
    fitGantt: () => fitGantt,
    fitHeatmap: () => fitHeatmap,
    fitKeys: () => fitKeys,
    fitLabel: () => fitLabel,
    fitLegend: () => fitLegend,
    fitPieLabels: () => fitPieLabels,
    fitSankey: () => fitSankey,
    fitSquareCells: () => fitSquareCells,
    fitState: () => fitState,
    fitSurfaceAxes: () => fitSurfaceAxes,
    flattenColor: () => flattenColor,
    fontStack: () => fontStack,
    formatNumber: () => formatNumber,
    funnelStages: () => funnelStages,
    ganttHeight: () => ganttHeight,
    hcl: () => hcl,
    heatmapHints: () => heatmapHints,
    heatmapMargins: () => heatmapMargins,
    hierarchyBorder: () => hierarchyBorder,
    hierarchyDepth: () => hierarchyDepth,
    hierarchyFromTable: () => hierarchyFromTable,
    hierarchyOf: () => hierarchyOf,
    hierarchyPalette: () => hierarchyPalette,
    histogramEdges: () => histogramEdges,
    histogramValueLabel: () => histogramValueLabel,
    joinChoropleth: () => joinChoropleth,
    kernelDensity: () => kernelDensity,
    keyPlacement: () => keyPlacement,
    keyState: () => keyState,
    labeledSums: () => labeledSums,
    layoutColorScale: () => layoutColorScale,
    layoutLegend: () => layoutLegend,
    layoutSurvivalRisk: () => layoutSurvivalRisk,
    lightThemeTokens: () => lightThemeTokens,
    lineAspect: () => lineAspect,
    mapFeatures: () => mapFeatures,
    measureLayout: () => measureLayout,
    measureLegendTitle: () => measureLegendTitle,
    mergePatch: () => mergePatch,
    networkReducers: () => networkReducers,
    networkTheme: () => networkTheme,
    normalizeLocation: () => normalizeLocation,
    pieSlices: () => pieSlices,
    placeColorScale: () => placeColorScale,
    placeZoomSlider: () => placeZoomSlider,
    pretty: () => pretty,
    radarSeries: () => radarSeries,
    radicalInverse: () => radicalInverse,
    registerRenderers: () => registerRenderers,
    repelLabels: () => repelLabels,
    resolveTheme: () => resolveTheme,
    rollingValues: () => rollingValues,
    ruleBandwidth: () => ruleBandwidth,
    sankeyLinks: () => sankeyLinks,
    scatterAspect: () => lineAspect,
    significanceAxisNames: () => significanceAxisNames,
    significanceGroupNames: () => significanceGroupNames,
    sizeKeyValues: () => sizeKeyValues,
    spectrogramColors: () => spectrogramColors,
    surfaceSeries: () => surfaceSeries,
    survivalMedian: () => survivalMedian,
    themeFontSizes: () => themeFontSizes,
    themeObject: () => themeObject,
    themeOption: () => themeOption,
    timeLabel: () => timeLabel,
    timeMs: () => timeMs,
    valueLabel: () => valueLabel,
    zoomSliderStyle: () => zoomSliderStyle
  });

  // src/theme/tokens.ts
  var A3_THEME_MEMBERS = Object.freeze([
    "region_palette",
    "site_palette",
    "ptm_palette",
    "processing_palette",
    "residue_fill_color",
    "residue_stroke_color",
    "label_color",
    "position_label_color",
    "variant_color",
    "disease_variant_color"
  ]);
  var A3_PALETTES = Object.freeze({
    region_palette: Object.freeze(["#0F766E", "#2563EB", "#D97706", "#7C3AED", "#DC2626", "#0891B2"]),
    site_palette: Object.freeze(["#0EA5E9", "#14B8A6", "#E11D48", "#A855F7", "#F97316", "#16A34A"]),
    ptm_palette: Object.freeze(["#0284C7", "#F59E0B", "#10B981", "#8B5CF6", "#EF4444", "#14B8A6"]),
    processing_palette: Object.freeze([
      "#D97706",
      "#DC2626",
      "#0EA5E9",
      "#9333EA",
      "#16A34A",
      "#DB2777"
    ])
  });
  var A3_THEME_TOKENS = Object.freeze({
    light: Object.freeze({
      ...A3_PALETTES,
      residue_fill_color: "#efefef",
      residue_stroke_color: "#44403C",
      label_color: "#1C1917",
      position_label_color: "#78716C",
      variant_color: "#FA6E1E",
      disease_variant_color: "#E266AE"
    }),
    dark: Object.freeze({
      ...A3_PALETTES,
      residue_fill_color: "#181818",
      residue_stroke_color: "#a1a1aa",
      label_color: "#fafafa",
      position_label_color: "#a1a1aa",
      variant_color: "#fb923c",
      disease_variant_color: "#f472b6"
    })
  });
  function a3ThemeTokens(scheme, a3) {
    const defaults = A3_THEME_TOKENS[scheme === "dark" ? "dark" : "light"];
    return Object.freeze({ ...defaults, ...present(a3 ?? {}) });
  }
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
  var THEME_SPACING = Object.freeze({
    axis_label_gap: 2 / 3,
    axis_name_gap: 0.625,
    axis_name_location: "middle"
  });
  function lightThemeTokens(overrides = {}) {
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
      a3: A3_THEME_TOKENS.light,
      ...overrides
    });
  }
  function darkThemeTokens(overrides = {}) {
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
      a3: A3_THEME_TOKENS.dark,
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
    const variable = /^var\(.*\)$/i;
    const out = entries.map((e) => {
      const generic = GENERIC_FONT_FAMILIES.has(e.toLowerCase());
      return quoted.test(e) || generic || ident.test(e) || variable.test(e) ? e : `'${e.replaceAll("'", "\\'")}'`;
    });
    const last = entries[entries.length - 1] ?? "";
    if (!GENERIC_FONT_FAMILIES.has(last.toLowerCase())) out.push(DEFAULT_FONT_FAMILY);
    return out.join(", ");
  }
  function em(length, size) {
    return length === void 0 || length === null ? void 0 : length * size;
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
  function zoomSliderStyle(dark) {
    return {
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
      emphasis: {
        handleStyle: { borderColor: dark ? "#cccccc" : "#505050" },
        moveHandleStyle: { color: dark ? "#b0b0b0" : "#707070" }
      }
    };
  }
  var SERIES_THEME = Object.freeze({
    line: Object.freeze({ symbol: "circle" }),
    radar: Object.freeze({ symbol: "circle" })
  });
  function themeOption(tokens) {
    const sizes = themeFontSizes(tokens);
    const dark = tokens.scheme === "dark";
    const fg = tokens.fg_color;
    const textStyle = present({
      color: fg,
      fontFamily: fontStack(tokens.font_family),
      fontSize: sizes.base,
      fontWeight: tokens.font_weight
    });
    const withText = (style) => ({ ...textStyle, ...style });
    const axisLabelText = present({
      fontSize: sizes.axisLabel,
      color: fg,
      fontWeight: tokens.font_weight
    });
    const axisLabel = present({
      ...axisLabelText,
      margin: em(tokens.axis_label_gap, sizes.axisLabel)
    });
    const nameMargin = em(tokens.axis_name_gap, sizes.base);
    const nameTextStyle = nameMargin === void 0 ? void 0 : { textMargin: [nameMargin, 3, nameMargin, 3] };
    const axisLineStyle = tokens.axis_color ? { lineStyle: { color: tokens.axis_color } } : {};
    const axisLine = Object.keys(axisLineStyle).length > 0 || tokens.axis_line_show !== void 0 && tokens.axis_line_show !== null ? present({ show: tokens.axis_line_show, ...axisLineStyle }) : void 0;
    const axisTick = { show: false };
    const gridLine = present({
      color: tokens.grid_color,
      opacity: tokens.grid_opacity,
      type: tokens.grid_style
    });
    const splitLine = Object.keys(gridLine).length > 0 ? { lineStyle: gridLine } : void 0;
    const axisName = present({ nameLocation: tokens.axis_name_location, nameTextStyle });
    const valueAxis2 = present({ ...axisName, axisLabel, axisLine, axisTick, splitLine });
    const gridLine3D = present({ color: tokens.grid_color, opacity: tokens.grid_opacity });
    const axis3D = present({
      axisLabel: present({ ...axisLabelText, fontFamily: textStyle.fontFamily }),
      nameTextStyle: textStyle,
      axisLine: { lineStyle: { color: tokens.axis_color ?? tokens.neutral_color } },
      splitLine: Object.keys(gridLine3D).length > 0 ? { lineStyle: gridLine3D } : void 0
    });
    const radar = present({
      ...SERIES_THEME.radar,
      axisName: axisLabelText,
      axisLine,
      splitLine,
      splitArea: tokens.grid_color ? { areaStyle: { color: ["rgba(0, 0, 0, 0)", tokens.grid_color] } } : void 0
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
            fontWeight: tokens.title_font_weight
          })
        ),
        subtextStyle: withText(present({ color: tokens.subtitle_color, fontSize: sizes.subtitle }))
      },
      legend: present({
        itemGap: em(tokens.legend_item_gap, sizes.legend),
        textStyle: withText({ color: tokens.legend_color ?? fg, fontSize: sizes.legend })
      }),
      // The pointer line and its value label on an axis, in the neutral color.
      axisPointer: {
        lineStyle: { color: tokens.neutral_color },
        label: { backgroundColor: tokens.neutral_color }
      },
      tooltip: present({
        backgroundColor: tokens.tooltip_bg,
        borderColor: tokens.tooltip_border_color,
        textStyle: withText({ color: tokens.tooltip_color ?? fg, fontSize: sizes.tooltip })
      }),
      visualMap: {
        textStyle: present({ ...textStyle, fontSize: tokens.color_scale_font_size ?? sizes.base })
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
        splitLine: { show: false }
      }),
      valueAxis: valueAxis2,
      logAxis: valueAxis2,
      timeAxis: valueAxis2,
      valueAxis3D: axis3D,
      categoryAxis3D: axis3D,
      logAxis3D: axis3D,
      timeAxis3D: axis3D
    };
  }
  var THEME_DOCUMENT_MEMBERS = Object.freeze([
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
    "legend_item_gap",
    "a3",
    "echarts",
    "origin",
    "writer"
  ]);
  function resolveTheme(doc) {
    const origin = { ...doc.origin ?? {} };
    const out = { ...doc };
    const derive = (name, value) => {
      if (out[name] === void 0 || out[name] === null) {
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
      "color_scale_font_size"
    ]) {
      derive(name, base);
    }
    for (const name of ["title_color", "subtitle_color", "legend_color", "tooltip_color"]) {
      derive(name, doc.fg_color);
    }
    derive("font_family", DEFAULT_FONT_FAMILY);
    if (out.a3 === void 0 || out.a3 === null) {
      derive("a3", a3ThemeTokens(doc.scheme));
    } else {
      out.a3 = a3ThemeTokens(doc.scheme, doc.a3);
    }
    return { ...out, origin };
  }
  function mergePatch(target, patch) {
    const out = { ...target };
    for (const [key, value] of Object.entries(patch)) {
      if (value === null) {
        delete out[key];
      } else if (typeof value === "object" && !Array.isArray(value) && typeof out[key] === "object" && out[key] !== null && !Array.isArray(out[key])) {
        out[key] = mergePatch(out[key], value);
      } else {
        out[key] = value;
      }
    }
    return out;
  }
  function themeObject(doc) {
    const { echarts, ...tokens } = doc;
    const option = themeOption(tokens);
    return echarts ? mergePatch(option, echarts) : option;
  }

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
    const coordinates2 = createSequenceCoordinates(sequenceLength, nPerRow);
    const residues = Array.from(data.sequence).map(
      (residue, index) => Object.freeze({
        position: index + 1,
        residue,
        residueName: AMINO_ACID_NAMES[residue] ?? residue,
        x: scaleResidueXCoordinate(coordinates2.xs[index] ?? 0, residueSpacing),
        y: coordinates2.ys[index] ?? 0
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
            annotationsByPosition
          )
        },
        showSymbol: true,
        symbol: "circle",
        symbolSize: markerSize,
        lineStyle: {
          color: theme.residue_stroke_color,
          width: lineWidth
        },
        itemStyle: {
          color: markerColor,
          borderColor: theme.residue_stroke_color,
          borderWidth: 1
        }
      });
    }
    for (const [index, [name, entry]] of Object.entries(model.data.annotations.region).entries()) {
      const color = paletteColorAt(theme.region_palette, index);
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
      const color = paletteColorAt(theme.site_palette, index);
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
      const color = paletteColorAt(theme.ptm_palette, index);
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
      const color = paletteColorAt(theme.processing_palette, index);
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
          color: theme.variant_color,
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
            color: theme.position_label_color,
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
      animation: enableAnimation,
      animationDuration,
      animationDurationUpdate,
      ...options.title ? {
        title: {
          text: options.title,
          left: "5.5%",
          top: titleTop
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
        // The family headings: the legend's text, semibold, set off above.
        textStyle: {
          rich: {
            heading: {
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
        return theme.disease_variant_color;
      }
      if (variantPositions.has(residue.position)) {
        return theme.variant_color;
      }
      return theme.label_color;
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
  function formatResidueTooltip(position, residueName, annotationsByPosition) {
    const name = escapeHtml(residueName ?? "");
    const header = position === void 0 ? name : `<span style="opacity:0.6;font-weight:400">${position}</span> ${name}`;
    const headerHtml = `<div style="font-weight:600">${header}</div>`;
    const wrap = (inner) => `<div>${inner}</div>`;
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
    const a3 = a3ThemeTokens(appearance.dark ? "dark" : "light", appearance.a3);
    const theme = Object.freeze({
      ...a3,
      ...present2({
        residue_fill_color: config.residue_fill,
        residue_stroke_color: config.residue_stroke,
        label_color: config.label_color,
        position_label_color: config.pos_label_color,
        variant_color: config.variant_color,
        disease_variant_color: config.disease_variant_color
      })
    });
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
      markerColor: theme.residue_fill_color,
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
  function present2(entries) {
    return Object.fromEntries(
      Object.entries(entries).filter((e) => Boolean(e[1]))
    );
  }

  // src/charts/a3/fit.ts
  function fitA3(chart, context) {
    const hint = context.hints;
    const bounded = context.bounded ?? true;
    const source = context.option;
    const width = chart.getWidth();
    const xRange = source.xAxis.max - source.xAxis.min;
    const yRange = source.yAxis.max - source.yAxis.min;
    const marker = hint.markerSize;
    const step = hint.residueSpacing;
    const glyph = Math.max(1, marker, hint.fontSize);
    if (![marker, hint.fontSize].every((v) => Number.isFinite(v) && v >= 0) || ![step, xRange, yRange].every((v) => Number.isFinite(v) && v > 0)) {
      throw new Error(
        "Supply nonnegative A3 glyph sizes, positive residue spacing, and increasing axis ranges."
      );
    }
    const grid = { ...source.grid };
    const selected = {};
    chart.getModel().eachComponent(
      "legend",
      (model2) => Object.assign(selected, model2.get("selected"))
    );
    chart.setOption({
      legend: {
        ...source.legend,
        selected,
        orient: "vertical",
        align: "left",
        left: null,
        bottom: null,
        width: null,
        height: 1e6
      }
    });
    const model = chart.getModel().getComponent("legend");
    const box = chart.getViewOfComponentModel(model).group.getBoundingRect();
    if (hint.autoGrid) grid.right = source.legend.right + box.width + (hint.legendGap ?? 40);
    if (hint.autoGrid && hint.autoHeight && !bounded) {
      grid.height = hint.bodyHeight ?? glyph * 2 * yRange;
      const height = Math.max(
        grid.top + grid.height + grid.bottom,
        source.legend.top + box.height + grid.bottom
      );
      chart.resize({ width, height: Math.ceil(height) });
    }
    chart.setOption({ grid });
    const area = chart.getModel().getComponent("grid").coordinateSystem.getRect();
    if (area.width <= 0 || area.height <= 0) {
      throw new Error("Increase A3 figure dimensions to leave room for residues and the legend.");
    }
    const dx = area.width / xRange * step;
    const dy = area.height / yRange;
    const turn = Math.hypot(Math.sqrt(3) / 2 * dx, dy / 2);
    const scale = Math.min(1, Math.min(dx, dy, turn) / (glyph + 4));
    const scaled = (object, keys) => {
      if (!object) return void 0;
      const update = {};
      for (const key of keys) {
        if (typeof object[key] === "number") update[key] = object[key] * scale;
        else if (Array.isArray(object[key])) {
          update[key] = object[key].map((v) => v * scale);
        }
      }
      return update;
    };
    chart.setOption({
      series: source.series.map((series) => ({
        ...scaled(series, ["symbolSize", "symbolOffset"]),
        lineStyle: { ...scaled(series.lineStyle, ["width"]) },
        itemStyle: { ...scaled(series.itemStyle, ["borderWidth"]) },
        data: series.data.map(
          (datum) => datum?.label ? {
            ...datum,
            label: { ...datum.label, ...scaled(datum.label, ["fontSize", "distance"]) }
          } : datum
        )
      }))
    });
  }

  // src/shared/format.ts
  var SIGNIFICANT_DIGITS = 3;
  function formatNumber(value, digits = SIGNIFICANT_DIGITS) {
    if (typeof value !== "number" || !Number.isFinite(value)) return value;
    if (Number.isInteger(value)) return String(value);
    const magnitude = Math.abs(value);
    if (magnitude < 1e-3 || magnitude >= 1e15) return value.toExponential(digits - 1);
    if (Math.floor(Math.log10(magnitude)) + 1 >= digits) return value.toFixed(0);
    const rounded2 = Number(value.toPrecision(digits));
    const decimals = Math.max(0, digits - 1 - Math.floor(Math.log10(Math.abs(rounded2))));
    return rounded2.toFixed(decimals);
  }
  function tooltipValue(value) {
    return formatNumber(value);
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
  function tickLabel(decimals) {
    return (value) => ddSci(typeof value === "number" ? value : Number(value), decimals);
  }

  // src/shared/layout.ts
  var DEFAULT_MARGIN = 36;
  var AXIS_LABEL_INSET_EM = 0.09;
  function axisLabelInset(fontSize) {
    return fontSize * AXIS_LABEL_INSET_EM;
  }
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
  function placeZoomSlider(option) {
    const zooms = [option.dataZoom ?? []].flat();
    const slider = zooms.find((z) => z?.id === ZOOM_SLIDER_ID);
    const grid = [option.grid ?? []].flat()[0];
    if (!slider || !grid) return;
    const side = slider.yAxisIndex !== void 0 ? "right" : "bottom";
    const current = grid[side];
    const margin = typeof current === "number" ? current : side === "bottom" ? 80 : 15;
    slider[side] = margin + SLIDER_HANDLE;
    grid[side] = margin + SLIDER_HANDLE + SLIDER_SIZE + SLIDER_GAP;
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
    if (config.xlab) Object.assign(xAxis, { name: config.xlab });
    if (config.ylab) Object.assign(yAxis, { name: config.ylab });
    const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
    const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : void 0;
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "axis", valueFormatter: tooltipValue },
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
    const range3 = calcLimits(values, 0);
    if (!range3) return null;
    const min = Math.min(range3[0], 0);
    const max = Math.max(range3[1], 0);
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
  function niceAxis(min, max, fewest = 3, most = 6) {
    let lo = min;
    let hi = max;
    if (lo === hi) {
      const r = Math.max(1, Math.abs(lo) * 0.05);
      lo -= r;
      hi += r;
    }
    const decimalsOf = (interval2) => {
      const text = Number(interval2.toPrecision(12)).toString();
      const [, frac = ""] = text.split(".");
      return text.includes("e-") ? Number(text.split("e-")[1]) + frac.length : frac.length;
    };
    let best = null;
    let bestSpan = Number.POSITIVE_INFINITY;
    const top = Math.floor(Math.log10((hi - lo) / fewest)) + 1;
    for (let e = top; e >= top - 2; e--) {
      for (const m of [1, 2, 2.5, 5]) {
        const interval2 = m * 10 ** e;
        const decimals = Math.max(0, decimalsOf(interval2));
        const clean = (v) => Number(v.toFixed(Math.min(20, decimals + 2)));
        const a = clean(Math.floor(lo / interval2 + 1e-9) * interval2);
        const b = clean(Math.ceil(hi / interval2 - 1e-9) * interval2);
        const n = Math.round((b - a) / interval2);
        if (n < fewest || n > most) continue;
        const span = b - a;
        const better = best === null || span < bestSpan - 1e-9 * span || Math.abs(span - bestSpan) <= 1e-9 * span && (decimals < best.decimals || decimals === best.decimals && Math.abs(n - 4) < Math.abs(Math.round((best.max - best.min) / best.interval) - 4));
        if (better) {
          best = { min: a, max: b, interval: clean(interval2), decimals };
          bestSpan = span;
        }
      }
    }
    if (best) return best;
    const interval = (hi - lo) / 4;
    return { min: lo, max: hi, interval, decimals: Math.max(0, decimalsOf(interval)) };
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
      const layout2 = boxPoint(
        settings,
        api,
        settings.boxIndex,
        api.value(0),
        api.value(1),
        api.value(3)
      );
      if (!layout2) return;
      return {
        type: "circle",
        shape: { cx: layout2.point[0], cy: layout2.point[1], r: settings.pointSize / 2 },
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
        const layout2 = boxPoint(
          settings,
          api,
          settings.boxIndex,
          api.value(0),
          profile.position[i],
          0
        );
        if (!layout2) return;
        const half = profile.width[i] * layout2.width / 2;
        const a = layout2.point.slice();
        const b = layout2.point.slice();
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
      const layout2 = boxPoint(settings, api, settings.boxIndex, api.value(0), api.value(1), 0);
      if (!layout2) return;
      const [x, y] = layout2.point;
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
            style: { fill: null, stroke: api.visual("color"), lineWidth: 1 }
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
              fill: api.visual("color"),
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
    const layout2 = !levels ? "none" : (config.group_layout ?? "auto") === "auto" ? variables.length === 1 ? "x-axis" : "legend" : config.group_layout;
    const cell = (variable, level) => data.cells.find((c) => c.variable === variable && (c.level ?? null) === level) ?? null;
    let categories;
    let series;
    if (!levels) {
      categories = variables.map(name);
      series = [{ name: null, cells: variables.map((v) => cell(v, null)) }];
    } else if (layout2 === "x-axis") {
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
    const neutral = appearance.neutral ?? SIGN_COLORS.neutral;
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
            const range3 = rangeOf(c);
            return {
              value: [j, range3?.[0] ?? null, range3?.[1] ?? null],
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
          itemStyle: { color: s.name === null ? neutral : colorOf(si, 0) },
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
        // The renderer draws the brackets and their labels in the series color.
        itemStyle: { color: neutral },
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
    const valueAxis2 = {
      type: "value",
      scale: true,
      ...valueLimits ? { min: valueLimits[0], max: valueLimits[1] } : {}
    };
    const xAxis = horizontal ? valueAxis2 : categoryAxis;
    const yAxis = horizontal ? categoryAxis : valueAxis2;
    if (config.xlab) Object.assign(xAxis, { name: config.xlab });
    if (config.ylab) Object.assign(yAxis, { name: config.ylab });
    const grid = cartesianGrid(config, Boolean(config.xlab || config.ylab));
    const dataZoom = config.zoom ? axisZoom(horizontal ? "y" : "x", grid) : void 0;
    const named = series.filter((s) => s.name !== null);
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
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

  // src/charts/calibration/config.ts
  var CALIBRATION_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "observed",
    "probability",
    "group",
    "n_bins",
    "bin_method",
    "na_rm",
    "mode",
    "show_brier",
    "rug",
    "rug_size",
    "rug_opacity",
    "point_size",
    "line_width",
    "digits",
    "diagonal",
    "diagonal_color",
    "palette",
    "legend",
    "square",
    "xlab",
    "ylab",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  var TOOLTIP_DIMENSIONS = [
    "Mean probability",
    "Observed proportion",
    "Bin lower",
    "Bin upper",
    "Brier"
  ];
  function buildCalibrationOption(data, config, appearance = {}) {
    const digits = config.digits ?? 3;
    const fmt = (x) => ddSci(x, digits);
    const mode = config.mode ?? "lines+markers";
    const palette = asArray(config.palette);
    const color = (i) => palette.length > 0 ? palette[i % palette.length] : void 0;
    const labels = data.groups.map(
      (g) => `${g.name}${config.show_brier ?? true ? ` (Brier ${fmt(g.brier)})` : ""}`
    );
    const series = [];
    if (config.diagonal ?? true) {
      const diagonal = config.diagonal_color ?? appearance.neutral ?? SIGN_COLORS.neutral;
      series.push({
        data: [
          [0, 0],
          [1, 1]
        ],
        silent: true,
        legendHoverLink: false,
        z: 1,
        showSymbol: false,
        // A set color keeps the diagonal from taking a group's palette color.
        lineStyle: { color: diagonal, width: 1, type: "dashed" },
        itemStyle: { color: diagonal },
        type: "line"
      });
    }
    data.groups.forEach((g, i) => {
      const c = color(i);
      const b = g.bins;
      const nBins = b.probability.length;
      series.push({
        name: labels[i],
        data: b.probability.map((p, j) => [
          p,
          b.observed[j],
          b.n[j],
          fmt(p),
          fmt(b.observed[j]),
          fmt(b.lower[j]),
          fmt(b.upper[j]),
          fmt(g.brier),
          g.omitted
        ]),
        z: 3,
        smooth: false,
        connectNulls: false,
        clip: false,
        showSymbol: true,
        symbol: "circle",
        symbolSize: config.point_size ?? 7,
        lineStyle: {
          ...c ? { color: c } : {},
          width: config.line_width ?? 2,
          opacity: mode === "markers" ? 0 : 1
        },
        itemStyle: {
          ...c ? { color: c } : {},
          opacity: mode === "lines" && nBins > 1 ? 0 : 1
        },
        type: "line",
        emphasis: { itemStyle: { opacity: 1 } },
        dimensions: [
          "probability",
          "observed",
          "Count",
          ...TOOLTIP_DIMENSIONS.map((name) => ({ name, displayName: name, type: "ordinal" })),
          "Missing pairs"
        ],
        encode: { x: 0, y: 1, tooltip: [3, 4, 2, 5, 6, 7, 8] }
      });
      if (config.rug ?? true) {
        const size = config.rug_size ?? 8;
        series.push({
          name: labels[i],
          data: g.probability.map((p) => [p, 0, fmt(p)]),
          z: 2,
          clip: false,
          symbol: "rect",
          symbolSize: [1, size],
          symbolOffset: [0, -size / 2],
          itemStyle: { ...c ? { color: c } : {}, opacity: config.rug_opacity ?? 0.3 },
          type: "scatter",
          dimensions: ["probability", "baseline", { name: "Probability", type: "ordinal" }],
          encode: { x: 0, y: 1, tooltip: [2] }
        });
      }
    });
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      legend: {
        show: config.legend ?? true,
        orient: "horizontal",
        padding: 0,
        itemStyle: { opacity: 1 },
        data: labels
      },
      grid: {
        left: 70,
        right: 24,
        top: config.title ? 50 : 25,
        bottom: 65,
        containLabel: false,
        outerBoundsMode: "none"
      },
      xAxis: {
        type: "value",
        name: config.xlab ?? "Mean predicted probability",
        nameGap: 30,
        min: 0,
        max: 1
      },
      yAxis: {
        type: "value",
        name: config.ylab ?? "Observed proportion",
        nameGap: 45,
        min: 0,
        max: 1
      },
      tooltip: { trigger: "item", confine: true },
      series
    };
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
    const summary2 = config.summary_color ?? "#EFEFEF";
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
          nameGap: 30,
          axisLine: { show: false },
          axisLabel: { show: j === 0 || j === 1, interval: 0, fontSize }
        });
        yAxes.push({
          type: "category",
          gridIndex: index,
          data: yLabels[j],
          inverse: true,
          boundaryGap: true,
          ...j === 0 && ylab ? { name: ylab } : {},
          nameGap: 50,
          axisLine: { show: false },
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
            right.push({ value: [m, j, 0], label: label(rate(values[j] ?? null), summary2) });
          });
          [r.ppv, r.npv].forEach((values, m) => {
            bottom.push({ value: [j, m, 0], label: label(rate(values[j] ?? null), summary2) });
          });
        }
        const overall = [
          { value: [0, 0, 0], label: label(`Accuracy
${rate(r.accuracy)}`, summary2) },
          { value: [0, 1, 0], label: label(`BA
${rate(r.balancedAccuracy)}`, summary2) }
        ];
        [right, bottom, overall].forEach((strip, j) => {
          maps.push({
            type: "continuous",
            show: false,
            min: 0,
            max: 1,
            inRange: { color: [summary2, summary2] },
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
    const summary2 = meta.summaryColor || blend(echarts, bg, neutral, 0.05);
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
        const high = j < 2 ? map.inRange.color[1] : j === 4 ? overall : summary2;
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
    const summary2 = context.perResample ? `${fmt(group.mean)} (${fmt(group.sd)})` : fmt(group.mean);
    return `${parts.join(" \xB7 ")} ${summary2}`;
  }
  function curveLegendTitle(type, config) {
    if (config.legend_title) return config.legend_title;
    const summary2 = type === "roc" ? ROC.summary : PR.summary;
    return (config.variant ?? "aggregate") === "per_resample" ? `${summary2}: mean (SD)` : summary2;
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
          symbol: "circle",
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
        nameGap: 30
      },
      yAxis: {
        type: "value",
        min: 0,
        max: 1,
        ...ylab ? { name: ylab } : {},
        nameGap: 45
      },
      legend: {
        show: config.legend ?? true,
        orient: "horizontal",
        padding: 0,
        icon: "roundRect",
        itemWidth: 20,
        itemHeight: 3,
        // Legend keys stay visible whatever the transparent hover targets.
        itemStyle: { opacity: 1 },
        data: active.map((id) => labels[id])
      },
      tooltip: { trigger: "item", confine: true },
      series,
      ...appearance.animation === void 0 ? {} : { animation: appearance.animation }
    };
  }
  function buildRocOption(data, config, appearance = {}) {
    const color = config.diagonal_color ?? appearance.neutral ?? SIGN_COLORS.neutral;
    const diagonal = config.diagonal ?? true ? [
      {
        type: "line",
        symbol: "circle",
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
        ...i === rows - 1 && xlab ? { name: xlab } : {}
      })),
      yAxis: names.map((name, i) => ({
        type: "value",
        min: low < 0 ? low * 1.05 : 0,
        max: high > 0 ? high * 1.05 : 1,
        gridIndex: i,
        name,
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
      symbol: "circle",
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
      tooltip: { trigger: "axis", valueFormatter: tooltipValue },
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
      ...xlab ? { name: xlab } : {}
    };
    const yAxis = {
      type: "value",
      min: 0,
      ...ylab ? { name: ylab } : {}
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
  function ruleBandwidth(summary2, rule = "nrd0") {
    const sd = summary2.sd ?? 0;
    const iqr = ((summary2.q3 ?? 0) - (summary2.q1 ?? 0)) / 1.34;
    const scale = summary2.n ** -0.2;
    switch (rule) {
      case "nrd0": {
        const lo = Math.min(sd, iqr) || sd || Math.abs(summary2.min) || 1;
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
  function kernelDensity(summary2, counts, options = {}) {
    const { n: size, min, max } = summary2;
    const explicit = options.bandwidth ?? null;
    if (!(size > 0) || explicit === null && (size < 2 || max === min)) return null;
    const bw = (explicit ?? ruleBandwidth(summary2, options.bw)) * (options.adjust ?? 1);
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

  // src/charts/pie/config.ts
  var PIE_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "values",
    "labels",
    "label_format",
    "percent_digits",
    "show_labels",
    "rose_type",
    "radius",
    "inner_radius",
    "palette",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  var LABEL_TEMPLATES = {
    name: "{b}",
    value: (p) => String(formatNumber(p.value)),
    percent: "{d}%",
    name_value: (p) => `${p.name ?? ""}: ${formatNumber(p.value)}`,
    name_percent: "{b}: {d}%"
  };
  var isMissing = (v) => v === null || v === void 0 || typeof v === "number" && Number.isNaN(v);
  function pieSlices(data, config) {
    return labeledSums(data, config, "A pie chart");
  }
  function labeledSums(data, config, chart) {
    const valueName = config.values;
    const labelName = config.labels;
    if (!valueName || !labelName) {
      throw new Error(`${chart} needs the config members 'values' and 'labels' set.`);
    }
    const raw = column(data.columns, labelName, "labels");
    const labels = textColumn(data, labelName, "labels");
    const values = column(data.columns, valueName, "values");
    const sums = /* @__PURE__ */ new Map();
    labels.forEach((label, i) => {
      if (isMissing(raw[i])) {
        throw new Error(
          `Column '${labelName}' (config member 'labels') must hold a label in every row; row ${i + 1} is missing.`
        );
      }
      const v = values[i];
      if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
        throw new Error(
          `Column '${valueName}' (config member 'values') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`
        );
      }
      sums.set(label, (sums.get(label) ?? 0) + v);
    });
    const names = distinct([...sums.keys()], data.levels?.[labelName]);
    const slices = names.map((name) => ({ name, value: sums.get(name) }));
    if (!slices.some((s) => s.value > 0)) {
      throw new Error(`${chart} needs a positive total; column '${valueName}' sums to 0.`);
    }
    return slices;
  }
  function buildPieOption(data, config, appearance = {}) {
    const slices = pieSlices(data, config);
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette ?? [];
    const items = slices.map(
      (slice, i) => palette.length > 0 ? { ...slice, itemStyle: { color: palette[i % palette.length] } } : { ...slice }
    );
    const outer = config.radius ?? "75%";
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
      ...slices.length > 1 ? { legend: { data: slices.map((s) => s.name) } } : {},
      series: [
        {
          type: "pie",
          data: items,
          radius: config.inner_radius ? [config.inner_radius, outer] : outer,
          ...config.rose_type ? { roseType: config.rose_type } : {},
          percentPrecision: config.percent_digits ?? 1,
          avoidLabelOverlap: true,
          // Edge alignment keeps outside labels within the chart at narrow widths.
          label: {
            show: config.show_labels !== false,
            alignTo: "edge",
            edgeDistance: 8,
            formatter: LABEL_TEMPLATES[config.label_format ?? "name"]
          }
        }
      ]
    };
  }

  // src/charts/funnel/config.ts
  var FUNNEL_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "values",
    "labels",
    "sort",
    "show_labels",
    "label_format",
    "palette",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  var LABEL_TEMPLATES2 = {
    name: "{b}",
    value: (p) => String(formatNumber(p.value)),
    name_value: (p) => `${p.name ?? ""}: ${formatNumber(p.value)}`
  };
  function funnelStages(data, config) {
    return labeledSums(data, config, "A funnel");
  }
  function buildFunnelOption(data, config, appearance = {}) {
    const stages = funnelStages(data, config);
    const own = asArray(config.palette);
    const palette = own.length > 0 ? own : appearance.palette ?? [];
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
      ...stages.length > 1 ? { legend: { data: stages.map((s) => s.name) } } : {},
      series: [
        {
          type: "funnel",
          sort: config.sort ?? "descending",
          left: "10%",
          width: "80%",
          top: config.title ? 70 : 50,
          bottom: 20,
          minSize: "0%",
          maxSize: "100%",
          label: {
            show: config.show_labels !== false,
            position: "inside",
            formatter: LABEL_TEMPLATES2[config.label_format ?? "name"]
          },
          data: stages.map(
            (s, i) => palette.length > 0 ? { ...s, itemStyle: { color: palette[i % palette.length] } } : { ...s }
          )
        }
      ]
    };
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
        ...config.xlab ? { name: config.xlab, nameGap: 28 } : {},
        ...config.guides ?? true ? {
          axisPointer: {
            show: true,
            type: "line",
            snap: false,
            label: { show: true }
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
        axisLabel: { interval: 0 }
      },
      ...controls ? { dataZoom: controls.dataZoom } : {},
      series
    };
  }

  // src/charts/gantt/fit.ts
  function fitGantt(echarts, chart, context) {
    const source = context.option;
    const series = Array.isArray(source?.series) ? source.series : [source?.series];
    if (!series.some((s) => s?.renderItem === GANTT_BAR) || !source.grid || Array.isArray(source.grid) || !source.xAxis || !source.yAxis || Array.isArray(source.xAxis) || Array.isArray(source.yAxis)) {
      return;
    }
    const width = chart.getWidth();
    const model = chart.getModel();
    const y = model.getComponent("yAxis");
    const font = y.getModel("axisLabel").getFont();
    const labelWidth = Math.min(
      width * 0.35,
      y.get("data").reduce(
        (max, label) => Math.max(max, echarts.format.getTextRect(String(label), font).width + 2),
        0
      )
    );
    const update = {
      grid: { ...source.grid },
      yAxis: { axisLabel: { width: labelWidth, overflow: "truncate", ...source.yAxis.axisLabel } }
    };
    const automatic = !context.legendPosition && source.legend?.orient === "vertical" && source.legend.right === 8 && source.legend.top === "middle" && source.legend.show !== false;
    let below = false;
    if (automatic) {
      const legend = model.getComponent("legend");
      const legendFont = legend.getModel("textStyle").getFont();
      const natural = Math.max(
        0,
        ...legend.getData().map(
          (item) => echarts.format.getTextRect(item.get("name"), legendFont).width
        )
      );
      const right = Math.max(source.grid.right, natural + legend.get("itemWidth") + 29);
      below = width - labelWidth - right - 32 < 240;
      const available = Math.max(24, width - legend.get("itemWidth") - 31);
      update.legend = {
        ...source.legend,
        selected: legend.get("selected"),
        left: below ? 8 : null,
        right: 8,
        top: below ? null : "middle",
        bottom: below ? 4 : null,
        orient: below ? "horizontal" : "vertical",
        textStyle: {
          ...source.legend.textStyle,
          width: below && natural > available ? available : null,
          overflow: "break"
        }
      };
      update.grid.right = below ? 16 : right;
    }
    if (source.toolbox?.show !== false && Number.isFinite(source.toolbox?.top)) {
      const title = model.getComponent("title");
      const toolbox = model.getComponent("toolbox");
      const bounds = (component) => {
        if (!component) return null;
        const group = chart.getViewOfComponentModel(component).group;
        const rect = group.getBoundingRect().clone();
        rect.applyTransform(group.getComputedTransform());
        return rect;
      };
      const titleBox = bounds(title);
      const toolsBox = bounds(toolbox);
      const overlap = titleBox && toolsBox && titleBox.x < toolsBox.x + toolsBox.width + 8 && toolsBox.x < titleBox.x + titleBox.width + 8;
      const top = overlap && titleBox ? Math.max(source.toolbox.top, titleBox.y + titleBox.height + 8) : source.toolbox.top;
      update.toolbox = { top };
      update.grid.top = Math.max(source.grid.top, top + (toolsBox?.height || 14) + 12);
    }
    chart.setOption(update);
    if (below) {
      const legend = chart.getModel().getComponent("legend");
      const height = chart.getViewOfComponentModel(legend).group.getBoundingRect().height;
      chart.setOption({ grid: { bottom: source.grid.bottom + height + 12 } });
    }
    const grid = chart.getModel().getComponent("grid").coordinateSystem.getRect();
    chart.setOption({
      xAxis: {
        splitNumber: source.xAxis.splitNumber ?? Math.max(2, Math.min(5, Math.floor(grid.width / 80))),
        axisLabel: { hideOverlap: true, ...source.xAxis.axisLabel }
      }
    });
  }

  // src/shared/color.ts
  function rgbOf(hex) {
    const m = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})(?:[0-9a-f]{2})?$/i.exec(hex);
    if (!m) throw new Error(`'${hex}' is not a #rrggbb color.`);
    return [
      Number.parseInt(m[1], 16),
      Number.parseInt(m[2], 16),
      Number.parseInt(m[3], 16)
    ];
  }
  var hex2 = (v, round2 = Math.round) => round2(Math.min(255, Math.max(0, v))).toString(16).padStart(2, "0").toUpperCase();
  function colorRamp(colors, n) {
    const stops = colors.map(rgbOf);
    if (stops.length === 0) throw new Error("A color ramp needs at least one color.");
    if (stops.length === 1 || n === 1) {
      const [r, g, b] = stops[0];
      return Array.from({ length: n }, () => `#${hex2(r)}${hex2(g)}${hex2(b)}`);
    }
    const k = stops.length;
    const at = (i, m) => i * (1 / (m - 1));
    return Array.from({ length: n }, (_, i) => {
      const x = at(i, n);
      let j = 0;
      while (j < k - 2 && x > at(j + 1, k)) j++;
      const x0 = at(j, k);
      const x1 = at(j + 1, k);
      const a = stops[j];
      const b = stops[j + 1];
      const channel = (c) => {
        const y0 = a[c] / 255;
        const y1 = b[c] / 255;
        const y = y0 + (y1 - y0) * ((x - x0) / (x1 - x0));
        return Math.min(1, Math.max(0, y)) * 255;
      };
      return `#${[0, 1, 2].map((c) => hex2(channel(c), Math.trunc)).join("")}`;
    });
  }
  function divergingRamp(negative, mid, positive, zlim, n = 101) {
    const span = zlim[1] - zlim[0];
    const nNeg = Math.max(2, Math.min(n - 1, Math.round(Math.abs(zlim[0]) / span * (n - 1)) + 1));
    const nPos = Math.max(2, n - nNeg + 1);
    return [...colorRamp([negative, mid], nNeg), ...colorRamp([mid, positive], nPos).slice(1)];
  }
  var WHITE = [95.047, 100, 108.883];
  function hcl(h, c, l) {
    const rad = h * Math.PI / 180;
    const u = c * Math.cos(rad);
    const v = c * Math.sin(rad);
    const [xn, yn, zn] = WHITE;
    const y = l > 7.999592 ? yn * ((l + 16) / 116) ** 3 : yn * l / 903.3;
    const t = xn + 15 * yn + 3 * zn;
    const un = 4 * xn / t;
    const vn = 9 * yn / t;
    const uu = u / (13 * l) + un;
    const vv = v / (13 * l) + vn;
    const x = 9 * y * uu / (4 * vv);
    const z = -x / 3 - 5 * y + 3 * y / vv;
    const lin = [
      (3.240479 * x - 1.53715 * y - 0.498535 * z) / yn,
      (-0.969256 * x + 1.875992 * y + 0.041556 * z) / yn,
      (0.055648 * x - 0.204043 * y + 1.057311 * z) / yn
    ];
    const gamma = (u2) => u2 > 304e-5 ? 1.055 * u2 ** (1 / 2.4) - 0.055 : 12.92 * u2;
    return `#${lin.map((ch) => hex2(255 * Math.min(1, Math.max(0, gamma(ch))))).join("")}`;
  }
  function dark3(n) {
    return Array.from({ length: n }, (_, i) => hcl(360 * i / n, 80, 60));
  }

  // src/charts/heatmap/renderers.ts
  var DENDROGRAM = "rtemis.dendrogram.v1";
  var HEATMAP_TRACKS = "rtemis.heatmap_tracks.v1";
  var TRACK_SIZE = 10;
  var TRACK_STEP = 12;
  var heatmapRenderers = {
    // Data: [left position, right position, left height, right height, merge
    // height]. Each merge draws one three-segment U shape, in its branch color
    // when the payload has one per merge.
    [DENDROGRAM]: (params, api) => {
      const p = params.itemPayload;
      const [lp, rp, lh, rh, mh] = [0, 1, 2, 3, 4].map((d) => api.value(d));
      const points = p.orientation === "row" ? [
        [lh, lp],
        [mh, lp],
        [mh, rp],
        [rh, rp]
      ] : [
        [lp, lh],
        [lp, mh],
        [rp, mh],
        [rp, rh]
      ];
      return {
        type: "polyline",
        shape: { points: points.map((point) => api.coord(point)) },
        style: { stroke: p.colors ? p.colors[params.dataIndex] : p.color, lineWidth: 1, fill: null }
      };
    },
    // Data: [index]. Each row (or column) draws one rectangle per track, left
    // of the cells (below them, or above when the column labels are on top).
    [HEATMAP_TRACKS]: (params, api) => {
      const p = params.itemPayload;
      const index = api.value(0);
      const row = p.orientation === "row";
      const rect = params.coordSys;
      const center = api.coord(row ? [0, index] : [index, 0]);
      const raw = api.size([1, 1]);
      const size = Array.isArray(raw) ? raw : [raw, raw];
      const w = Math.abs(size[0]);
      const h = Math.abs(size[1]);
      return {
        type: "group",
        children: (p.colors[index] ?? []).map((color, track) => ({
          type: "rect",
          shape: row ? {
            x: rect.x - 4 - (track + 1) * TRACK_STEP,
            y: center[1] - h / 2,
            width: TRACK_SIZE,
            height: h
          } : {
            x: center[0] - w / 2,
            y: p.top ? rect.y - 4 - (track + 1) * TRACK_STEP : rect.y + rect.height + 4 + track * TRACK_STEP,
            width: w,
            height: TRACK_SIZE
          },
          style: { fill: color }
        }))
      };
    }
  };

  // src/charts/heatmap/config.ts
  var HEATMAP_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "legend_position",
    "legend_placement",
    "legend_title",
    "row_names",
    "col_names",
    "triangle",
    "cluster_rows",
    "cluster_cols",
    "dist_method",
    "hclust_method",
    "square_cells",
    "show_row_dendro",
    "show_col_dendro",
    "dendro_row_width",
    "dendro_col_height",
    "dendro_color",
    "dendro_uniform",
    "dendro_row_side",
    "dendro_col_side",
    "colormap",
    "zlim",
    "show_notes",
    "row_cut",
    "col_cut",
    "show_values",
    "value_digits",
    "show_colorbar",
    "colorbar_orient",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left"
  ]);
  function axisOrder(n, tree, axis) {
    const order2 = Array.from({ length: n }, (_, i) => i);
    if (!tree) return { order: order2, tree: null, positions: [] };
    const leaves = tree.leaves ?? Array.from({ length: n }, (_, i) => i);
    const m = tree.order.length;
    if (tree.merge.length !== m - 1 || tree.height.length !== m - 1 || leaves.length !== m) {
      throw new Error(
        `The ${axis} tree needs one merge and height per joined pair, and one leaf per ${axis}.`
      );
    }
    const positions = [...leaves].sort((a, b) => a - b);
    tree.order.forEach((leaf, k) => {
      order2[positions[k]] = leaves[leaf - 1];
    });
    return { order: order2, tree, positions };
  }
  function dendrogramData(a, uniform) {
    const tree = a.tree;
    const n = tree.order.length;
    const leafPos = new Array(n).fill(0);
    tree.order.forEach((leaf, p) => {
      leafPos[leaf - 1] = a.positions[p];
    });
    const heights = uniform ? tree.merge.map((_, k) => k + 1) : [...tree.height];
    const nodePos = [];
    const pos = (j) => j < 0 ? leafPos[-j - 1] : nodePos[j - 1];
    const hgt = (j) => j < 0 ? 0 : heights[j - 1];
    const data = tree.merge.map(([l, r], k) => {
      nodePos[k] = (pos(l) + pos(r)) / 2;
      return [pos(l), pos(r), hgt(l), hgt(r), heights[k]];
    });
    return {
      data,
      min: Math.min(...heights),
      max: Math.max(...heights, Number.EPSILON)
    };
  }
  function dendroAxisMin(min, max, stub = 0.1) {
    return min >= max ? min * (1 - stub) : (min - stub * max) / (1 - stub);
  }
  function branchColors(tree, k, color) {
    if (k === null || k === void 0) return null;
    const n = tree.order.length;
    if (!Number.isInteger(k) || k < 1 || k > n) {
      throw new Error(`A tree cut needs a whole number of clusters between 1 and ${n}; it is ${k}.`);
    }
    const cluster = Array.from({ length: n }, (_, i) => i);
    const find = (i) => {
      if (cluster[i] === i) return i;
      const root = find(cluster[i]);
      cluster[i] = root;
      return root;
    };
    const rep = [];
    tree.merge.forEach(([l, r], step) => {
      const a = l < 0 ? -l - 1 : rep[l - 1];
      const b = r < 0 ? -r - 1 : rep[r - 1];
      rep[step] = a;
      if (step < n - k) cluster[find(b)] = find(a);
    });
    const label = /* @__PURE__ */ new Map();
    for (const leaf of tree.order) {
      const root = find(leaf - 1);
      if (!label.has(root)) label.set(root, label.size + 1);
    }
    const palette = dark3(k);
    const group = [];
    return tree.merge.map(([l, r], step) => {
      const g = (j) => j < 0 ? label.get(find(-j - 1)) : group[j - 1];
      const [gl, gr] = [g(l), g(r)];
      group[step] = gl !== 0 && gl === gr ? gl : 0;
      return group[step] ? palette[group[step] - 1] : color;
    });
  }
  var BRANCH_GRAY = "#99999988";
  function layout(data, config) {
    const values = data.values;
    const nRows = values.length;
    const nCols = values[0]?.length ?? 0;
    if (nRows === 0 || nCols === 0)
      throw new Error("A heatmap needs at least one row and one column.");
    for (const row of values) {
      if (row.length !== nCols) throw new Error("Every heatmap row needs one value per column.");
    }
    const labels = (given, member, n, axis) => {
      const names = given ?? (asArray(member).length > 0 ? asArray(member) : Array.from({ length: n }, (_, i) => String(i + 1)));
      if (names.length !== n) {
        throw new Error(
          `Supply one ${axis} label per matrix ${axis}; there are ${n}, and ${names.length} labels.`
        );
      }
      return [...names];
    };
    const rowNames = labels(data.row_names, config.row_names, nRows, "row");
    const colNames = labels(data.col_names, config.col_names, nCols, "column");
    const longest = (names) => Math.max(0, ...names.map((s) => s.length));
    const rotate = longest(colNames) > 4 ? 45 : 0;
    const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
    const showBar = config.show_colorbar ?? true;
    const set = (side, auto) => {
      const v = config[`margin_${side}`];
      return typeof v === "number" ? v : auto;
    };
    const rowTracks = data.row_colors?.[0]?.length ?? 0;
    const colTracks = data.col_colors?.[0]?.length ?? 0;
    const rowTrackPx = rowTracks > 0 ? TRACK_STEP * rowTracks + 4 : 0;
    const colTrackPx = colTracks > 0 ? TRACK_STEP * colTracks + 4 : 0;
    const left = set("left", Math.min(20 + longest(rowNames) * 8, 200)) + rowTrackPx;
    const right = set("right", showBar && vertical ? 90 : 20);
    const top = set("top", config.title ? 40 : 10);
    const bottom = set("bottom", rotate > 0 ? Math.min(20 + longest(colNames) * 5, 140) : 36) + colTrackPx;
    const rows = axisOrder(nRows, data.row_tree, "row");
    const cols = axisOrder(nCols, data.col_tree, "column");
    const rowDendro = rows.tree !== null && (config.show_row_dendro ?? true);
    const colDendro = cols.tree !== null && (config.show_col_dendro ?? true);
    const rowSide = config.dendro_row_side ?? "right";
    const colSide = config.dendro_col_side ?? "top";
    const rowWidth = config.dendro_row_width ?? 60;
    const colHeight = config.dendro_col_height ?? 60;
    const labelsTop = colDendro && colSide === "bottom";
    const hm = {
      left: left + (rowDendro && rowSide === "left" ? rowWidth : 0),
      right: right + (rowDendro && rowSide === "right" ? rowWidth : 0),
      top: top + (colDendro && colSide === "top" ? colHeight : 0) + (labelsTop ? bottom : 0),
      bottom: labelsTop ? colHeight : bottom + (colDendro && colSide === "bottom" ? colHeight : 0)
    };
    return {
      nRows,
      nCols,
      rowNames,
      colNames,
      rotate,
      margins: { left, right, top, bottom },
      rowTrackPx,
      colTrackPx,
      rows,
      cols,
      rowDendro,
      colDendro,
      rowSide,
      colSide,
      rowWidth,
      colHeight,
      labelsTop,
      hm,
      square: config.square_cells ?? nRows === nCols
    };
  }
  function heatmapHints(data, config) {
    const l = layout(data, config);
    const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
    return {
      legendReserveRight: (config.show_colorbar ?? true) && vertical && typeof config.margin_right !== "number" ? 70 : 0,
      squareCells: l.square,
      nRows: l.nRows,
      nCols: l.nCols,
      leftPx: l.hm.left,
      rightPx: l.hm.right,
      topPx: l.hm.top,
      botPx: l.hm.bottom
    };
  }
  function heatmapMargins(data, config) {
    const l = layout(data, config);
    return {
      left: l.margins.left - l.rowTrackPx,
      right: l.margins.right,
      top: l.margins.top,
      bottom: l.margins.bottom - l.colTrackPx
    };
  }
  function fitHeatmap(chart, option, hints) {
    if (!hints.squareCells) return;
    const { nRows, nCols, leftPx, rightPx, topPx, botPx } = hints;
    const width = chart.getWidth() - leftPx - rightPx;
    const height = chart.getHeight() - topPx - botPx;
    const cell = Math.min(width / nCols, height / nRows);
    if (!(cell > 0)) return;
    const dx = (width - nCols * cell) / 2;
    const dy = (height - nRows * cell) / 2;
    const list = (v) => [v ?? []].flat();
    const update = {
      grid: list(option.grid).map((grid) => {
        const fitted = {
          ...grid,
          containLabel: false,
          outerBoundsMode: "none"
        };
        for (const side of ["left", "right", "top", "bottom"]) {
          const v = grid[side];
          if (typeof v === "number")
            fitted[side] = v + (side === "left" || side === "right" ? dx : dy);
        }
        return fitted;
      })
    };
    const maps = list(option.visualMap);
    if (maps.length > 0) {
      update.visualMap = maps.map((map) => {
        const side = map.orient === "horizontal" ? "bottom" : "right";
        const anchor = map[side] === side ? 0 : map[side];
        return typeof anchor === "number" ? { [side]: anchor + (side === "right" ? dx : dy) } : {};
      });
    }
    const titles = list(option.title);
    if (titles.length > 0) {
      update.title = titles.map(
        (title) => typeof title.left === "number" ? { left: title.left + dx } : {}
      );
    }
    chart.setOption(update);
  }
  var escapeHtml2 = (text) => text.replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);
  function buildHeatmapOption(data, config, appearance = {}) {
    const l = layout(data, config);
    const digits = config.value_digits ?? 2;
    const showValues = config.show_values ?? false;
    const showNotes = config.show_notes ?? false;
    const notes = data.cell_notes ?? null;
    if (showNotes && (!notes || showValues)) {
      throw new Error(
        "`show_notes` writes the cell notes the data supplies, in place of the values: supply cell notes, and unset `show_values`."
      );
    }
    const rowOrder = l.rows.order;
    const colOrder = l.cols.order;
    const cell = (i, j) => data.values[rowOrder[i]]?.[colOrder[j]] ?? null;
    let lo = Number.POSITIVE_INFINITY;
    let hi = Number.NEGATIVE_INFINITY;
    for (let i = 0; i < l.nRows; i++) {
      for (let j = 0; j < l.nCols; j++) {
        const v = cell(i, j);
        if (typeof v === "number" && Number.isFinite(v)) {
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
    }
    if (lo > hi) throw new Error("A heatmap needs at least one finite value.");
    const zlim = config.zlim && config.zlim.length === 2 ? [config.zlim[0], config.zlim[1]] : [lo, hi];
    const memberColors = asArray(config.colormap);
    let colormap;
    if (memberColors.length > 0) {
      colormap = [...memberColors];
    } else {
      const sign = appearance.signColors ?? SIGN_COLORS;
      const bg = appearance.background ?? (appearance.dark ? darkThemeTokens() : lightThemeTokens()).bg_color;
      colormap = zlim[0] < 0 && zlim[1] > 0 ? divergingRamp(sign.negative, bg, sign.positive, zlim) : zlim[1] <= 0 ? colorRamp([sign.negative, bg], 101) : colorRamp([bg, sign.positive], 101);
    }
    const labelled = showValues || notes !== null;
    const cells = [];
    for (let i = 0; i < l.nRows; i++) {
      for (let j = 0; j < l.nCols; j++) {
        const v = cell(i, j);
        const value = [j, i, v];
        if (!labelled) {
          cells.push(value);
          continue;
        }
        const note = notes ? notes[rowOrder[i]]?.[colOrder[j]] ?? "" : "";
        cells.push({
          value,
          name: note,
          note,
          label: { formatter: showNotes ? "{b}" : v === null ? "" : ddSci(v, digits) }
        });
      }
    }
    const branch = config.dendro_color ?? BRANCH_GRAY;
    const colAxis = {
      type: "category",
      data: colOrder.map((j) => l.colNames[j]),
      splitArea: { show: false },
      axisLine: { show: false },
      axisLabel: { rotate: l.rotate, margin: 8 + l.colTrackPx },
      boundaryGap: true,
      ...l.labelsTop ? { position: "top" } : {}
    };
    const rowAxis = {
      type: "category",
      data: rowOrder.map((i) => l.rowNames[i]),
      inverse: true,
      splitArea: { show: false },
      axisLine: { show: false },
      axisLabel: { margin: 8 + l.rowTrackPx },
      boundaryGap: true
    };
    const grids = [];
    const xAxes = [];
    const yAxes = [];
    const series = [];
    const m = l.margins;
    if (l.rowDendro) {
      const d = dendrogramData(l.rows, config.dendro_uniform ?? false);
      const index = grids.length;
      grids.push(
        l.rowSide === "right" ? { right: m.right, width: l.rowWidth, top: l.hm.top, bottom: l.hm.bottom } : { left: 0, width: l.rowWidth, top: l.hm.top, bottom: l.hm.bottom }
      );
      xAxes.push({
        type: "value",
        inverse: l.rowSide === "left",
        show: false,
        min: dendroAxisMin(d.min, d.max),
        max: d.max,
        gridIndex: index
      });
      yAxes.push({
        type: "value",
        min: -0.5,
        max: l.nRows - 0.5,
        inverse: true,
        show: false,
        gridIndex: index
      });
      const colors = branchColors(l.rows.tree, config.row_cut, branch);
      series.push(dendrogramSeries(index, d.data, "row", branch, colors));
    } else if (config.row_cut !== null && config.row_cut !== void 0) {
      throw new Error(
        "`row_cut` colors the branches of the row tree: cluster the rows, or supply a row tree."
      );
    }
    if (l.colDendro) {
      const d = dendrogramData(l.cols, config.dendro_uniform ?? false);
      const index = grids.length;
      grids.push(
        l.colSide === "top" ? { left: l.hm.left, right: l.hm.right, top: m.top, height: l.colHeight } : { left: l.hm.left, right: l.hm.right, bottom: 0, height: l.colHeight }
      );
      xAxes.push({ type: "value", min: -0.5, max: l.nCols - 0.5, show: false, gridIndex: index });
      yAxes.push({
        type: "value",
        show: false,
        inverse: l.colSide === "bottom",
        min: dendroAxisMin(d.min, d.max),
        max: d.max,
        gridIndex: index
      });
      const colors = branchColors(l.cols.tree, config.col_cut, branch);
      series.push(dendrogramSeries(index, d.data, "column", branch, colors));
    } else if (config.col_cut !== null && config.col_cut !== void 0) {
      throw new Error(
        "`col_cut` colors the branches of the column tree: cluster the columns, or supply a column tree."
      );
    }
    const hmIndex = grids.length;
    const dendro = l.rowDendro || l.colDendro;
    grids.push(
      dendro ? {
        left: l.hm.left,
        right: l.hm.right,
        top: l.hm.top,
        bottom: l.hm.bottom,
        containLabel: false
      } : { left: m.left, right: m.right, top: m.top, bottom: m.bottom, containLabel: false }
    );
    xAxes.push(dendro ? { ...colAxis, gridIndex: hmIndex } : colAxis);
    yAxes.push(dendro ? { ...rowAxis, gridIndex: hmIndex } : rowAxis);
    const heatmapIndex = series.length;
    series.push({
      type: "heatmap",
      // Without animation and hover emphasis, a large matrix neither tweens
      // every cell on redraw nor recomputes a highlight on each pointer move.
      animation: false,
      emphasis: { disabled: true },
      data: cells,
      ...showValues || showNotes ? { label: { show: true } } : {},
      ...dendro ? { xAxisIndex: hmIndex, yAxisIndex: hmIndex } : {},
      ...showNotes ? { rtemisCellNotes: true } : {}
    });
    for (const [orientation, tracks, order2] of [
      ["row", data.row_colors, rowOrder],
      ["column", data.col_colors, colOrder]
    ]) {
      if (!tracks) continue;
      series.push({
        type: "custom",
        renderItem: HEATMAP_TRACKS,
        ...dendro ? { xAxisIndex: hmIndex, yAxisIndex: hmIndex } : {},
        data: order2.map((_, k) => [k]),
        silent: true,
        clip: false,
        animation: false,
        itemPayload: {
          orientation,
          top: l.labelsTop,
          colors: order2.map((index) => [...tracks[index] ?? []])
        }
      });
    }
    const rowLabels = rowAxis.data;
    const colLabels = colAxis.data;
    const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
    const single = grids.length === 1;
    return {
      ...config.title ? { title: { text: config.title, left: dendro ? l.hm.left : m.left } } : {},
      tooltip: {
        trigger: "item",
        formatter: (p) => {
          if (!Array.isArray(p.value)) return "";
          const v = p.value[2];
          let text = typeof v === "number" ? ddSci(v, digits) : "NA";
          if (showValues && p.data?.label?.formatter) text = p.data.label.formatter;
          const note = p.data?.note ? `<br>${escapeHtml2(p.data.note)}` : "";
          return `${escapeHtml2(String(rowLabels[p.value[1]]))} \xD7 ${escapeHtml2(String(colLabels[p.value[0]]))}: ${escapeHtml2(text)}${note}`;
        }
      },
      grid: single ? grids[0] : grids,
      xAxis: single ? xAxes[0] : xAxes,
      yAxis: single ? yAxes[0] : yAxes,
      visualMap: {
        type: "continuous",
        min: zlim[0],
        max: zlim[1],
        precision: digits,
        calculable: true,
        show: config.show_colorbar ?? true,
        orient: vertical ? "vertical" : "horizontal",
        inRange: { color: colormap },
        // A vertical scale's labels sit right of its bar (`align` names the bar's
        // side), away from the cells.
        ...vertical ? { right: "right", top: "middle", align: "left" } : { left: "center", bottom: "bottom" },
        seriesIndex: heatmapIndex
      },
      series
    };
  }
  function dendrogramSeries(index, data, orientation, color, colors) {
    return {
      type: "custom",
      xAxisIndex: index,
      yAxisIndex: index,
      data,
      silent: true,
      clip: true,
      animation: false,
      renderItem: DENDROGRAM,
      itemPayload: { orientation, color, ...colors ? { colors } : {} }
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
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
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
      ...xlab ? { name: xlab } : {}
    };
    const yAxis = {
      type: "value",
      min: low < 0 ? low * 1.05 : 0,
      ...barMode === "stack" && high > 0 ? { max: high * 1.05 } : {},
      ...ylab ? { name: ylab } : {}
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
  function classCount(summary2, breaks) {
    const { n } = summary2;
    const range3 = summary2.max - summary2.min;
    switch (breaks) {
      case "Sturges":
        return Math.ceil(Math.log2(n) + 1);
      case "Scott": {
        const h = 3.5 * (summary2.sd ?? 0) * n ** (-1 / 3);
        return h > 0 ? Math.max(1, Math.ceil(range3 / h)) : 1;
      }
      case "FD":
      case "Freedman-Diaconis": {
        const iqr = (summary2.q3 ?? 0) - (summary2.q1 ?? 0);
        const h = 2 * iqr * n ** (-1 / 3);
        return h > 0 ? Math.max(1, Math.ceil(range3 / h)) : 1;
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
  function histogramEdges(summary2, options = {}) {
    if (!(summary2.n > 0) || !Number.isFinite(summary2.min) || !Number.isFinite(summary2.max)) {
      throw new Error("A histogram needs at least one finite value to bin.");
    }
    const count = Math.min(
      options.bins ?? classCount(summary2, options.breaks ?? "Sturges"),
      MAX_CLASSES
    );
    if (!(count >= 1)) {
      throw new Error(`\`bins\` must be at least 1; it is ${count}.`);
    }
    return pretty(summary2.min, summary2.max, count, 1);
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
          symbol: "circle",
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
        symbol: "circle",
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
        symbol: "circle",
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
      ...config.xlab ? { name: config.xlab } : {},
      ...categories ? { data: categories } : {},
      ...xType === "value" ? { scale: true } : {},
      ...xLim ? { min: xLim[0], max: xLim[1] } : {},
      ...xType === "category" ? {} : { splitLine: NO_CORNER_LINES },
      ...xType === "time" ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {},
      ...yLim ? { axisLine: zeroAxisLine(yLim) } : {}
    };
    const yAxis = {
      type: "value",
      ...config.ylab ? { name: config.ylab } : {},
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
      tooltip: { trigger: "axis", valueFormatter: tooltipValue },
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
    const present3 = values.filter((v) => v !== null && v !== void 0);
    return present3.length > 0 && present3.every((v) => typeof v === "number") ? "value" : "category";
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
    const present3 = labels.filter((_, row) => !missing(raw[row]));
    const groups = distinct(present3, data.levels?.[config.group]);
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
    const present3 = (j) => y[j] !== null && y[j] !== void 0;
    return y.flatMap((_, j) => present3(j) && !present3(j - 1) && !present3(j + 1) ? [j] : []);
  }
  function blockMarkArea(data, config, blocks, xNum, xText) {
    const colors = asArray(config.block_color);
    if (colors.length === 0) {
      throw new Error("`block_color` must be set when `blocks` is.");
    }
    const raw = column(data.columns, blocks, "blocks");
    const missing = (v) => v === null || v === void 0 || Number.isNaN(v);
    const keys = raw.map((v) => missing(v) ? null : String(v));
    const present3 = new Set(keys.filter((k) => k !== null));
    const declared = data.levels?.[blocks];
    const levels = declared ? declared.filter((level) => present3.has(level)) : sortedLevels(raw.filter((v) => !missing(v)));
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

  // src/charts/pie/fit.ts
  function fitPieLabels(echarts, chart, context) {
    const definitions = context.option.series;
    const source = Array.isArray(definitions) ? definitions : [definitions];
    const updates = source.map((definition, index) => {
      if (definition?.type !== "pie" || definition.label?.alignTo !== "edge") return {};
      const model = chart.getModel().getSeriesByIndex(index);
      const data = model.getData();
      const rect = data.getLayout("viewRect");
      const px = (value, extent2) => typeof value === "string" && value.endsWith("%") ? Number.parseFloat(value) * extent2 / 100 : +value || 0;
      const base = Math.min(rect.width, rect.height) / 2;
      const radii = Array.isArray(definition.radius) ? definition.radius : [0, definition.radius ?? "75%"];
      const outer = px(radii[1], base);
      const inner = px(radii[0], base);
      let band = 0;
      for (let i = 0; i < data.count(); i++) {
        const label = data.getItemModel(i).getModel("label");
        if (label.get("show") === false || !["outer", "outside"].includes(label.get("position"))) {
          continue;
        }
        const line = data.getItemModel(i).getModel("labelLine");
        const width = echarts.format.getTextRect(data.getName(i), label.getFont()).width;
        band = Math.max(
          band,
          width + px(label.get("edgeDistance"), rect.width) + px(line.get("length"), rect.width) + px(line.get("length2"), rect.width) + (label.get("distanceToLabelLine") || 0) + 4
        );
      }
      const radius = Math.min(outer, Math.max(10, rect.width / 2 - band));
      return { radius: [outer ? inner * radius / outer : 0, radius] };
    });
    if (updates.some((update) => "radius" in update)) chart.setOption({ series: updates });
  }

  // src/charts/radar/config.ts
  var RADAR_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "indicators",
    "group",
    "aggregate",
    "area_opacity",
    "line_width",
    "palette",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  var RADAR_ALL_ROWS = "All rows";
  var summary = (values, how) => {
    if (how === "sum") return values.reduce((a, b) => a + b, 0);
    if (how === "mean") return values.reduce((a, b) => a + b, 0) / values.length;
    const sorted = [...values].sort((a, b) => a - b);
    const mid = Math.floor(sorted.length / 2);
    return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
  };
  function radarSeries(data, config) {
    const indicators = asArray(config.indicators);
    if (indicators.length < 2) {
      throw new Error("A radar chart needs at least two columns in the config member 'indicators'.");
    }
    const columns = indicators.map((name) => column(data.columns, name, "indicators"));
    const n = columns[0]?.length ?? 0;
    const groupName = config.group;
    const groups = groupName ? textColumn(data, groupName, "group") : null;
    const names = groups ? distinct(groups, data.levels?.[groupName]) : n > 0 ? [RADAR_ALL_ROWS] : [];
    const rowsOf = new Map(names.map((name) => [name, []]));
    for (let i = 0; i < n; i++) rowsOf.get(groups ? groups[i] : RADAR_ALL_ROWS)?.push(i);
    const aggregate = config.aggregate ?? "none";
    return names.map((name) => {
      const rows = rowsOf.get(name);
      if (aggregate === "none" && rows.length > 1) {
        throw new Error(
          `Rows ${rows[0] + 1} and ${rows[1] + 1} both belong to series '${name}', which is one polygon. Set the config member 'aggregate' to sum, mean, or median to combine them${groupName ? "" : ", or set 'group'"}.`
        );
      }
      const values = indicators.map((indicator, j) => {
        const finite2 = [];
        for (const i of rows) {
          const v = columns[j][i];
          if (typeof v === "number" && Number.isFinite(v)) finite2.push(v);
          else if (aggregate === "none") {
            throw new Error(
              `Column '${indicator}' (config member 'indicators') must hold a finite number; row ${i + 1} holds '${String(v)}'.`
            );
          }
        }
        if (finite2.length === 0) {
          throw new Error(
            `Series '${name}' has no finite value in column '${indicator}' (config member 'indicators').`
          );
        }
        return aggregate === "none" ? finite2[0] : summary(finite2, aggregate);
      });
      return { name, values };
    });
  }
  function buildRadarOption(data, config, appearance = {}) {
    const series = radarSeries(data, config);
    const indicators = asArray(config.indicators);
    const own = asArray(config.palette);
    const palette = own.length > 0 ? own : appearance.palette ?? [];
    const color = (i) => palette.length > 0 ? palette[i % palette.length] : void 0;
    const indicator = indicators.map((name, j) => {
      const values = series.map((s) => s.values[j]);
      const max = Math.max(...values);
      const min = Math.min(...values);
      return { name, max: max > 0 ? max : 1, ...min < 0 ? { min } : { min: 0 } };
    });
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
      ...series.length > 1 ? { legend: { data: series.map((s) => s.name) } } : {},
      // A legend or title above the chart lowers and narrows the radar so the
      // top indicator's name stays clear of them.
      radar: series.length > 1 || config.title ? {
        center: ["50%", "56%"],
        radius: "62%",
        indicator,
        splitArea: { areaStyle: { opacity: 0.25 } }
      } : { radius: "68%", indicator, splitArea: { areaStyle: { opacity: 0.25 } } },
      series: [
        {
          type: "radar",
          data: series.map((s, i) => {
            const c = color(i);
            return {
              name: s.name,
              value: [...s.values],
              areaStyle: { opacity: config.area_opacity ?? 0.12 },
              lineStyle: { ...c ? { color: c } : {}, width: config.line_width ?? 2 },
              ...c ? { itemStyle: { color: c } } : {}
            };
          })
        }
      ]
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
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
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

  // src/charts/sankey/fit.ts
  function fitSankey(echarts, chart, context) {
    const option = context.option;
    const sources = Array.isArray(option.series) ? option.series : [option.series];
    const updates = sources.map((source, index) => {
      if (source?.type !== "sankey" || source.orient !== "vertical" || source.label?.position != null || source.label?.width != null) {
        return {};
      }
      const series = chart.getModel().getSeriesByIndex(index);
      const data = series.getData();
      const fontSize = series.getModel("label").get("fontSize") || 12;
      const fontFamily = series.getModel("label").get("fontFamily") || "sans-serif";
      const targets = new Set(source.links.map((edge) => edge.target));
      const origins = new Set(source.links.map((edge) => edge.source));
      let top = 0;
      let bottom = 0;
      const nodes = source.data.map((node, i) => {
        if (node.label?.position != null || node.label?.width != null) return node;
        const width = Math.max(1, data.getItemLayout(i).dx - 4);
        const position = origins.has(node.name) ? "top" : "bottom";
        const label = {
          ...node.label,
          position,
          width,
          overflow: "break",
          lineHeight: fontSize + 2
        };
        const height = new echarts.graphic.Text({
          style: {
            text: node.name,
            fontSize,
            fontFamily,
            width,
            overflow: "break",
            lineHeight: fontSize + 2
          }
        }).getBoundingRect().height + 12;
        if (!targets.has(node.name)) top = Math.max(top, height);
        if (!origins.has(node.name)) bottom = Math.max(bottom, height);
        return { ...node, label };
      });
      const px = (value) => typeof value === "string" && value.endsWith("%") ? Number.parseFloat(value) * chart.getHeight() / 100 : Number(value) || 0;
      return {
        data: nodes,
        top: Math.max(px(source.top), top),
        bottom: Math.max(px(source.bottom), bottom)
      };
    });
    if (updates.some((update) => "data" in update)) chart.setOption({ series: updates });
  }

  // src/layout/keys.ts
  var SIZE_KEY_ID = "rtemis-size-key";
  var KEY_GAP = 16;
  var GAP = KEY_GAP;
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
    let barLead = 0;
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
      barLead = center - b.width / 2;
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
    const px = (v, extent2, fallback) => typeof v === "number" ? v : typeof v === "string" && v.endsWith("%") ? Number.parseFloat(v) * extent2 / 100 : fallback;
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
      x = placement.inside ? side === "right" ? area.x + area.width - blockW - GAP : area.x + GAP : side === "right" ? area.x + area.width + GAP - barLead : GAP;
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
  function placeColorScale(chart, context = {}) {
    const model = chart.getModel();
    const scale = model.getComponent("visualMap");
    if (!scale || scale.get("show") === false || scale.get("orient") !== "vertical") return false;
    let right = Number.NEGATIVE_INFINITY;
    let top = Number.POSITIVE_INFINITY;
    let bottom = Number.NEGATIVE_INFINITY;
    for (let i = 0; ; i++) {
      const grid = model.getComponent("grid", i);
      if (!grid) break;
      const rect = grid.coordinateSystem?.getRect();
      if (!rect || !(rect.width > 0)) continue;
      right = Math.max(right, rect.x + rect.width);
      top = Math.min(top, rect.y);
      bottom = Math.max(bottom, rect.y + rect.height);
    }
    if (!Number.isFinite(right)) return false;
    const authored = [context.option?.visualMap ?? []].flat()[0];
    const title = String([authored?.text ?? []].flat()[0] ?? "");
    const text = keyText(chart, context.titleGap ?? KEY_TITLE_GAP);
    const lead = title ? titleHeight(text) : 0;
    chart.setOption({
      visualMap: {
        left: 0,
        top: 0,
        right: null,
        bottom: null,
        ...title ? { text: ["", ""] } : {}
      }
    });
    const placed = model.getComponent("visualMap") ?? scale;
    const view = chart.getViewOfComponentModel(placed);
    if (!view) return false;
    const box = view.group.getBoundingRect().clone();
    box.applyTransform(view.group.getComputedTransform());
    let barX = null;
    view.group.traverse((el) => {
      if (el.isGroup) return;
      const fill = el.style?.fill;
      if (fill && typeof fill === "object" && fill.colorStops) {
        const r = el.getBoundingRect().clone();
        r.applyTransform(el.getComputedTransform());
        barX = barX === null ? r.x : Math.min(barX, r.x);
      }
    });
    const offset = barX ?? box.x;
    const left = right + KEY_GAP - offset;
    const blockTop = (top + bottom) / 2 - (box.height + lead) / 2;
    chart.setOption({
      visualMap: { left, top: blockTop + lead - box.y },
      ...title ? {
        graphic: {
          elements: [
            {
              id: COLOR_KEY_TITLE_ID,
              type: "text",
              left: right + KEY_GAP,
              top: blockTop,
              silent: true,
              z: 100,
              style: {
                text: title,
                fontSize: text.titleSize,
                fontFamily: text.fontFamily,
                fontWeight: "bold",
                fill: text.color
              }
            }
          ]
        }
      } : {}
    });
    return true;
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
      ...xName ? { name: xName } : {},
      ...time ? {} : { scale: true },
      ...xLim ? { min: xLim[0], max: xLim[1] } : {},
      splitLine: NO_CORNER_LINES2,
      ...time ? { axisLabel: { showMinLabel: false, showMaxLabel: false } } : {},
      ...yLim ? { axisLine: zeroAxisLine2(yLim) } : {}
    };
    const yAxis = {
      type: "value",
      ...yName ? { name: yName } : {},
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
    const present3 = labels.filter((_, row) => !missing(raw[row]));
    const groups = distinct(present3, data.levels?.[config.group]);
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
        symbol: "circle",
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

  // src/charts/scatter3d/config.ts
  var SCATTER3D_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "x",
    "y",
    "z",
    "group",
    "alpha",
    "beta",
    "projection",
    "distance",
    "view_size",
    "point_size",
    "opacity",
    "mode",
    "order",
    "line_width",
    "xlab",
    "ylab",
    "zlab",
    "palette",
    "fit",
    "fit_params",
    "n_fit",
    "fit_alpha"
  ]);
  var OBSERVATIONS = "Observations";
  var AXIS_KEYS = ["xAxis3D", "yAxis3D", "zAxis3D"];
  var isMissing2 = (v) => v === null || v === void 0 || typeof v === "number" && Number.isNaN(v);
  function coordinates(data, name, member) {
    return column(data.columns, name, member).map((v, i) => {
      if (isMissing2(v)) return null;
      if (typeof v !== "number" || !Number.isFinite(v)) {
        throw new Error(
          `Column '${name}' (config member '${member}') must hold finite numbers or missing values; row ${i + 1} holds '${String(v)}'.`
        );
      }
      return v;
    });
  }
  function surfaceSeries(surface, opacity = 0.35) {
    const ix = surface.x.map((_, i) => i).sort((a, b) => surface.x[a] - surface.x[b]);
    const iy = surface.y.map((_, j) => j).sort((a, b) => surface.y[a] - surface.y[b]);
    const data = iy.flatMap(
      (j) => ix.map((i) => {
        const z = surface.z[i]?.[j];
        return [surface.x[i], surface.y[j], typeof z === "number" && Number.isFinite(z) ? z : null];
      })
    );
    return {
      type: "surface",
      name: surface.name,
      shading: "color",
      wireframe: { show: false },
      dataShape: [iy.length, ix.length],
      itemStyle: {
        ...surface.color ? { color: surface.color } : {},
        opacity: surface.opacity ?? opacity
      },
      data
    };
  }
  function axisRange(values) {
    let lo = Number.POSITIVE_INFINITY;
    let hi = Number.NEGATIVE_INFINITY;
    for (const v of values) {
      if (v < lo) lo = v;
      if (v > hi) hi = v;
    }
    if (lo === hi) {
      const r = Math.max(1, Math.abs(lo) * 0.05);
      lo -= r;
      hi += r;
    }
    if (!Number.isFinite(lo) || !Number.isFinite(hi) || !Number.isFinite(hi - lo) || hi - lo <= 0) {
      throw new Error("Rescale 3D coordinates to a finite nonzero range.");
    }
    return [lo, hi];
  }
  function axisTicks(nice, skipMin) {
    const label = tickLabel(nice.decimals);
    return (value) => skipMin && Math.abs(Number(value) - nice.min) <= nice.interval * 1e-6 ? "" : label(value);
  }
  function valueAxis(min, max) {
    const nice = niceAxis(min, max);
    return { min: nice.min, max: nice.max, interval: nice.interval };
  }
  function buildScatter3DOption(data, config, appearance = {}) {
    if (!config.x || !config.y || !config.z) {
      throw new Error("A 3D scatter chart needs the config members 'x', 'y', and 'z' set.");
    }
    const xs = coordinates(data, config.x, "x");
    const ys = coordinates(data, config.y, "y");
    const zs = coordinates(data, config.z, "z");
    const rawGroup = config.group ? column(data.columns, config.group, "group") : null;
    const group = xs.map((_, i) => {
      if (!rawGroup) return OBSERVATIONS;
      const g = rawGroup[i];
      return isMissing2(g) ? null : String(g);
    });
    const keep = xs.map(
      (x, i) => x !== null && ys[i] !== null && zs[i] !== null && group[i] !== null
    );
    const complete = keep.filter(Boolean).length;
    if (complete === 0)
      throw new Error("A 3D scatter chart needs at least one complete observation.");
    const groups = distinct(
      group.filter((g, i) => keep[i] === true && g !== null),
      config.group ? data.levels?.[config.group] : void 0
    );
    const mode = config.mode ?? "points";
    const opacity = config.opacity ?? (mode === "lines" ? 1 : autoAlpha(complete));
    const series = [];
    for (const g of groups) {
      const runs = [];
      let run = [];
      group.forEach((value, i) => {
        if (value !== g) return;
        if (keep[i]) run.push(i);
        else if (run.length > 0) {
          runs.push(run);
          run = [];
        }
      });
      if (run.length > 0) runs.push(run);
      const points = runs.map((r) => {
        const rows = config.order === "x" ? [...r].sort((a, b) => xs[a] - xs[b]) : r;
        return rows.map((i) => [xs[i], ys[i], zs[i]]);
      });
      if (mode === "points" || mode === "both") {
        series.push({
          type: "scatter3D",
          name: g,
          symbolSize: config.point_size ?? 8,
          itemStyle: { opacity },
          data: points.flat()
        });
      }
      if (mode === "lines" || mode === "both") {
        for (const path of points) {
          if (path.length < 2) continue;
          series.push({
            type: "line3D",
            name: g,
            lineStyle: { width: config.line_width ?? 2, opacity },
            data: path
          });
        }
      }
    }
    if (series.length === 0) {
      throw new Error("A 3D path needs at least two consecutive complete observations in a group.");
    }
    const surfaces = data.surfaces ?? [];
    const legendNames = [.../* @__PURE__ */ new Set([...groups, ...surfaces.map((surface) => surface.name)])];
    for (const surface of surfaces) series.push(surfaceSeries(surface, config.fit_alpha ?? 0.35));
    const finite2 = (v) => typeof v === "number" && Number.isFinite(v);
    const values = [xs, ys, zs].map((col, d) => [
      ...col.filter((v, i) => keep[i] === true && v !== null),
      ...surfaces.flatMap(
        (s) => d === 0 ? s.x.filter(finite2) : d === 1 ? s.y.filter(finite2) : s.z.flat().filter(finite2)
      )
    ]);
    const labels = [config.xlab ?? config.x, config.ylab ?? config.y, config.zlab ?? config.z];
    const axes = values.map((v, d) => {
      const [min, max] = axisRange(v);
      return {
        type: "value",
        ...valueAxis(min, max),
        name: labels[d],
        nameGap: 25,
        axisLabel: { formatter: axisTicks(niceAxis(min, max), d === 2) }
      };
    });
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette;
    const viewSize = config.view_size ?? 190;
    const distance = config.distance ?? 250;
    return {
      ...config.title ? { title: { text: config.title } } : {},
      ...palette && palette.length > 0 ? { color: [...palette] } : {},
      tooltip: {
        trigger: "item",
        formatter: (p) => {
          const v = Array.isArray(p.value) ? p.value : [];
          const lines = labels.map((name, d) => `${name}: ${formatNumber(v[d])}`);
          return [`${p.marker ?? ""}${p.seriesName ?? ""}`, ...lines].join("<br/>");
        }
      },
      legend: legendNames.length > 1 ? { data: legendNames } : { show: false },
      ...Object.fromEntries(AXIS_KEYS.map((key, d) => [key, axes[d]])),
      grid3D: {
        boxWidth: 100,
        boxDepth: 100,
        boxHeight: 100,
        top: 60,
        bottom: 20,
        left: 12,
        right: 12,
        viewControl: {
          projection: config.projection ?? "perspective",
          distance,
          minDistance: 100,
          maxDistance: 1e3,
          alpha: config.alpha ?? 20,
          beta: config.beta ?? 40,
          orthographicSize: viewSize - 1,
          minOrthographicSize: 20,
          maxOrthographicSize: 1e3,
          animation: false,
          autoRotate: false,
          panMouseButton: "right"
        },
        // rtemis.draw's widget scales the camera from these at each width.
        rtemisViewSize: viewSize,
        rtemisDistance: distance
      },
      series
    };
  }
  function fitSurfaceAxes(option, surfaces, expand = true) {
    if (surfaces.length === 0) return option;
    const out = { ...option };
    AXIS_KEYS.forEach((key, d) => {
      const axis = option[key];
      const min = axis?.min;
      const max = axis?.max;
      if (!axis || typeof min !== "number" || typeof max !== "number") {
        throw new Error("A surface layer needs a 3D chart with numeric axis limits.");
      }
      const coords = surfaces.flatMap(
        (s) => (s.data ?? []).map((p) => p[d]).filter((v) => typeof v === "number" && Number.isFinite(v))
      );
      const [lo, hi] = axisRange([min, max, ...coords]);
      if (!expand && (lo < min || hi > max)) {
        throw new Error(
          `A surface layer drawn without expanding the axes must lie within them; its ${"xyz"[d]} coordinates span beyond [${min}, ${max}].`
        );
      }
      if (lo < min || hi > max) {
        const nice = niceAxis(lo, hi);
        out[key] = {
          ...axis,
          ...valueAxis(lo, hi),
          axisLabel: { ...axis.axisLabel, formatter: axisTicks(nice, d === 2) }
        };
      }
    });
    return out;
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
      const present3 = order2.filter((g) => labels.some((l, i) => keep[i] && l === g));
      names = present3;
      groupOf = labels.map((g) => g == null ? -1 : present3.indexOf(g));
      const palette2 = asArray(config.palette);
      colors = present3.map((_, k) => palette2.length > 0 ? palette2[k % palette2.length] : void 0);
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
          lineStyle: { color: sign.neutral, type: "dashed", width: 1 },
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
      ...legend ? { legend: { data: shown.map((g) => names[g]), bottom: 0 } } : {},
      tooltip: { trigger: "item" },
      xAxis: volcano ? {
        type: "value",
        ...xlab ? { name: xlab } : {},
        scale: true,
        splitLine,
        ...xRange ? { min: xRange[0], max: xRange[1] } : {}
      } : {
        type: "category",
        ...xlab ? { name: xlab } : {},
        data: [...data.label],
        ...dense ? { axisLabel: { show: false }, axisTick: { show: false } } : {}
      },
      yAxis: {
        type: "value",
        ...ylab ? { name: ylab } : {},
        splitLine,
        ...yRange ? { min: yRange[0], max: yRange[1] } : {}
      },
      grid,
      ...dataZoom ? { dataZoom } : {},
      ...xyZoomed ? { toolbox: xyZoomed.toolbox } : {},
      series
    };
  }

  // src/shared/colormaps.ts
  var COLORMAPS = {
    magma: [
      "#000004",
      "#010005",
      "#010106",
      "#010108",
      "#020109",
      "#02020B",
      "#02020D",
      "#03030F",
      "#030312",
      "#040414",
      "#050416",
      "#060518",
      "#06051A",
      "#07061C",
      "#08071E",
      "#090720",
      "#0A0822",
      "#0B0924",
      "#0C0926",
      "#0D0A29",
      "#0E0B2B",
      "#100B2D",
      "#110C2F",
      "#120D31",
      "#130D34",
      "#140E36",
      "#150E38",
      "#160F3B",
      "#180F3D",
      "#19103F",
      "#1A1042",
      "#1C1044",
      "#1D1147",
      "#1E1149",
      "#20114B",
      "#21114E",
      "#221150",
      "#241253",
      "#251255",
      "#271258",
      "#29115A",
      "#2A115C",
      "#2C115F",
      "#2D1161",
      "#2F1163",
      "#311165",
      "#331067",
      "#341069",
      "#36106B",
      "#38106C",
      "#390F6E",
      "#3B0F70",
      "#3D0F71",
      "#3F0F72",
      "#400F74",
      "#420F75",
      "#440F76",
      "#451077",
      "#471078",
      "#491078",
      "#4A1079",
      "#4C117A",
      "#4E117B",
      "#4F127B",
      "#51127C",
      "#52137C",
      "#54137D",
      "#56147D",
      "#57157E",
      "#59157E",
      "#5A167E",
      "#5C167F",
      "#5D177F",
      "#5F187F",
      "#601880",
      "#621980",
      "#641A80",
      "#651A80",
      "#671B80",
      "#681C81",
      "#6A1C81",
      "#6B1D81",
      "#6D1D81",
      "#6E1E81",
      "#701F81",
      "#721F81",
      "#732081",
      "#752181",
      "#762181",
      "#782281",
      "#792282",
      "#7B2382",
      "#7C2382",
      "#7E2482",
      "#802582",
      "#812581",
      "#832681",
      "#842681",
      "#862781",
      "#882781",
      "#892881",
      "#8B2981",
      "#8C2981",
      "#8E2A81",
      "#902A81",
      "#912B81",
      "#932B80",
      "#942C80",
      "#962C80",
      "#982D80",
      "#992D80",
      "#9B2E7F",
      "#9C2E7F",
      "#9E2F7F",
      "#A02F7F",
      "#A1307E",
      "#A3307E",
      "#A5317E",
      "#A6317D",
      "#A8327D",
      "#AA337D",
      "#AB337C",
      "#AD347C",
      "#AE347B",
      "#B0357B",
      "#B2357B",
      "#B3367A",
      "#B5367A",
      "#B73779",
      "#B83779",
      "#BA3878",
      "#BC3978",
      "#BD3977",
      "#BF3A77",
      "#C03A76",
      "#C23B75",
      "#C43C75",
      "#C53C74",
      "#C73D73",
      "#C83E73",
      "#CA3E72",
      "#CC3F71",
      "#CD4071",
      "#CF4070",
      "#D0416F",
      "#D2426F",
      "#D3436E",
      "#D5446D",
      "#D6456C",
      "#D8456C",
      "#D9466B",
      "#DB476A",
      "#DC4869",
      "#DE4968",
      "#DF4A68",
      "#E04C67",
      "#E24D66",
      "#E34E65",
      "#E44F64",
      "#E55064",
      "#E75263",
      "#E85362",
      "#E95462",
      "#EA5661",
      "#EB5760",
      "#EC5860",
      "#ED5A5F",
      "#EE5B5E",
      "#EF5D5E",
      "#F05F5E",
      "#F1605D",
      "#F2625D",
      "#F2645C",
      "#F3655C",
      "#F4675C",
      "#F4695C",
      "#F56B5C",
      "#F66C5C",
      "#F66E5C",
      "#F7705C",
      "#F7725C",
      "#F8745C",
      "#F8765C",
      "#F9785D",
      "#F9795D",
      "#F97B5D",
      "#FA7D5E",
      "#FA7F5E",
      "#FA815F",
      "#FB835F",
      "#FB8560",
      "#FB8761",
      "#FC8961",
      "#FC8A62",
      "#FC8C63",
      "#FC8E64",
      "#FC9065",
      "#FD9266",
      "#FD9467",
      "#FD9668",
      "#FD9869",
      "#FD9A6A",
      "#FD9B6B",
      "#FE9D6C",
      "#FE9F6D",
      "#FEA16E",
      "#FEA36F",
      "#FEA571",
      "#FEA772",
      "#FEA973",
      "#FEAA74",
      "#FEAC76",
      "#FEAE77",
      "#FEB078",
      "#FEB27A",
      "#FEB47B",
      "#FEB67C",
      "#FEB77E",
      "#FEB97F",
      "#FEBB81",
      "#FEBD82",
      "#FEBF84",
      "#FEC185",
      "#FEC287",
      "#FEC488",
      "#FEC68A",
      "#FEC88C",
      "#FECA8D",
      "#FECC8F",
      "#FECD90",
      "#FECF92",
      "#FED194",
      "#FED395",
      "#FED597",
      "#FED799",
      "#FED89A",
      "#FDDA9C",
      "#FDDC9E",
      "#FDDEA0",
      "#FDE0A1",
      "#FDE2A3",
      "#FDE3A5",
      "#FDE5A7",
      "#FDE7A9",
      "#FDE9AA",
      "#FDEBAC",
      "#FCECAE",
      "#FCEEB0",
      "#FCF0B2",
      "#FCF2B4",
      "#FCF4B6",
      "#FCF6B8",
      "#FCF7B9",
      "#FCF9BB",
      "#FCFBBD",
      "#FCFDBF"
    ],
    inferno: [
      "#000004",
      "#010005",
      "#010106",
      "#010108",
      "#02010A",
      "#02020C",
      "#02020E",
      "#030210",
      "#040312",
      "#040314",
      "#050417",
      "#060419",
      "#07051B",
      "#08051D",
      "#09061F",
      "#0A0722",
      "#0B0724",
      "#0C0826",
      "#0D0829",
      "#0E092B",
      "#10092D",
      "#110A30",
      "#120A32",
      "#140B34",
      "#150B37",
      "#160B39",
      "#180C3C",
      "#190C3E",
      "#1B0C41",
      "#1C0C43",
      "#1E0C45",
      "#1F0C48",
      "#210C4A",
      "#230C4C",
      "#240C4F",
      "#260C51",
      "#280B53",
      "#290B55",
      "#2B0B57",
      "#2D0B59",
      "#2F0A5B",
      "#310A5C",
      "#320A5E",
      "#340A5F",
      "#360961",
      "#380962",
      "#390963",
      "#3B0964",
      "#3D0965",
      "#3E0966",
      "#400A67",
      "#420A68",
      "#440A68",
      "#450A69",
      "#470B6A",
      "#490B6A",
      "#4A0C6B",
      "#4C0C6B",
      "#4D0D6C",
      "#4F0D6C",
      "#510E6C",
      "#520E6D",
      "#540F6D",
      "#550F6D",
      "#57106E",
      "#59106E",
      "#5A116E",
      "#5C126E",
      "#5D126E",
      "#5F136E",
      "#61136E",
      "#62146E",
      "#64156E",
      "#65156E",
      "#67166E",
      "#69166E",
      "#6A176E",
      "#6C186E",
      "#6D186E",
      "#6F196E",
      "#71196E",
      "#721A6E",
      "#741A6E",
      "#751B6E",
      "#771C6D",
      "#781C6D",
      "#7A1D6D",
      "#7C1D6D",
      "#7D1E6D",
      "#7F1E6C",
      "#801F6C",
      "#82206C",
      "#84206B",
      "#85216B",
      "#87216B",
      "#88226A",
      "#8A226A",
      "#8C2369",
      "#8D2369",
      "#8F2469",
      "#902568",
      "#922568",
      "#932667",
      "#952667",
      "#972766",
      "#982766",
      "#9A2865",
      "#9B2964",
      "#9D2964",
      "#9F2A63",
      "#A02A63",
      "#A22B62",
      "#A32C61",
      "#A52C60",
      "#A62D60",
      "#A82E5F",
      "#A92E5E",
      "#AB2F5E",
      "#AD305D",
      "#AE305C",
      "#B0315B",
      "#B1325A",
      "#B3325A",
      "#B43359",
      "#B63458",
      "#B73557",
      "#B93556",
      "#BA3655",
      "#BC3754",
      "#BD3853",
      "#BF3952",
      "#C03A51",
      "#C13A50",
      "#C33B4F",
      "#C43C4E",
      "#C63D4D",
      "#C73E4C",
      "#C83F4B",
      "#CA404A",
      "#CB4149",
      "#CC4248",
      "#CE4347",
      "#CF4446",
      "#D04545",
      "#D24644",
      "#D34743",
      "#D44842",
      "#D54A41",
      "#D74B3F",
      "#D84C3E",
      "#D94D3D",
      "#DA4E3C",
      "#DB503B",
      "#DD513A",
      "#DE5238",
      "#DF5337",
      "#E05536",
      "#E15635",
      "#E25734",
      "#E35933",
      "#E45A31",
      "#E55C30",
      "#E65D2F",
      "#E75E2E",
      "#E8602D",
      "#E9612B",
      "#EA632A",
      "#EB6429",
      "#EB6628",
      "#EC6726",
      "#ED6925",
      "#EE6A24",
      "#EF6C23",
      "#EF6E21",
      "#F06F20",
      "#F1711F",
      "#F1731D",
      "#F2741C",
      "#F3761B",
      "#F37819",
      "#F47918",
      "#F57B17",
      "#F57D15",
      "#F67E14",
      "#F68013",
      "#F78212",
      "#F78410",
      "#F8850F",
      "#F8870E",
      "#F8890C",
      "#F98B0B",
      "#F98C0A",
      "#F98E09",
      "#FA9008",
      "#FA9207",
      "#FA9407",
      "#FB9606",
      "#FB9706",
      "#FB9906",
      "#FB9B06",
      "#FB9D07",
      "#FC9F07",
      "#FCA108",
      "#FCA309",
      "#FCA50A",
      "#FCA60C",
      "#FCA80D",
      "#FCAA0F",
      "#FCAC11",
      "#FCAE12",
      "#FCB014",
      "#FCB216",
      "#FCB418",
      "#FBB61A",
      "#FBB81D",
      "#FBBA1F",
      "#FBBC21",
      "#FBBE23",
      "#FAC026",
      "#FAC228",
      "#FAC42A",
      "#FAC62D",
      "#F9C72F",
      "#F9C932",
      "#F9CB35",
      "#F8CD37",
      "#F8CF3A",
      "#F7D13D",
      "#F7D340",
      "#F6D543",
      "#F6D746",
      "#F5D949",
      "#F5DB4C",
      "#F4DD4F",
      "#F4DF53",
      "#F4E156",
      "#F3E35A",
      "#F3E55D",
      "#F2E661",
      "#F2E865",
      "#F2EA69",
      "#F1EC6D",
      "#F1ED71",
      "#F1EF75",
      "#F1F179",
      "#F2F27D",
      "#F2F482",
      "#F3F586",
      "#F3F68A",
      "#F4F88E",
      "#F5F992",
      "#F6FA96",
      "#F8FB9A",
      "#F9FC9D",
      "#FAFDA1",
      "#FCFFA4"
    ],
    plasma: [
      "#0D0887",
      "#100788",
      "#130789",
      "#16078A",
      "#19068C",
      "#1B068D",
      "#1D068E",
      "#20068F",
      "#220690",
      "#240691",
      "#260591",
      "#280592",
      "#2A0593",
      "#2C0594",
      "#2E0595",
      "#2F0596",
      "#310597",
      "#330597",
      "#350498",
      "#370499",
      "#38049A",
      "#3A049A",
      "#3C049B",
      "#3E049C",
      "#3F049C",
      "#41049D",
      "#43039E",
      "#44039E",
      "#46039F",
      "#48039F",
      "#4903A0",
      "#4B03A1",
      "#4C02A1",
      "#4E02A2",
      "#5002A2",
      "#5102A3",
      "#5302A3",
      "#5502A4",
      "#5601A4",
      "#5801A4",
      "#5901A5",
      "#5B01A5",
      "#5C01A6",
      "#5E01A6",
      "#6001A6",
      "#6100A7",
      "#6300A7",
      "#6400A7",
      "#6600A7",
      "#6700A8",
      "#6900A8",
      "#6A00A8",
      "#6C00A8",
      "#6E00A8",
      "#6F00A8",
      "#7100A8",
      "#7201A8",
      "#7401A8",
      "#7501A8",
      "#7701A8",
      "#7801A8",
      "#7A02A8",
      "#7B02A8",
      "#7D03A8",
      "#7E03A8",
      "#8004A8",
      "#8104A7",
      "#8305A7",
      "#8405A7",
      "#8606A6",
      "#8707A6",
      "#8808A6",
      "#8A09A5",
      "#8B0AA5",
      "#8D0BA5",
      "#8E0CA4",
      "#8F0DA4",
      "#910EA3",
      "#920FA3",
      "#9410A2",
      "#9511A1",
      "#9613A1",
      "#9814A0",
      "#99159F",
      "#9A169F",
      "#9C179E",
      "#9D189D",
      "#9E199D",
      "#A01A9C",
      "#A11B9B",
      "#A21D9A",
      "#A31E9A",
      "#A51F99",
      "#A62098",
      "#A72197",
      "#A82296",
      "#AA2395",
      "#AB2494",
      "#AC2694",
      "#AD2793",
      "#AE2892",
      "#B02991",
      "#B12A90",
      "#B22B8F",
      "#B32C8E",
      "#B42E8D",
      "#B52F8C",
      "#B6308B",
      "#B7318A",
      "#B83289",
      "#BA3388",
      "#BB3488",
      "#BC3587",
      "#BD3786",
      "#BE3885",
      "#BF3984",
      "#C03A83",
      "#C13B82",
      "#C23C81",
      "#C33D80",
      "#C43E7F",
      "#C5407E",
      "#C6417D",
      "#C7427C",
      "#C8437B",
      "#C9447A",
      "#CA457A",
      "#CB4679",
      "#CC4778",
      "#CC4977",
      "#CD4A76",
      "#CE4B75",
      "#CF4C74",
      "#D04D73",
      "#D14E72",
      "#D24F71",
      "#D35171",
      "#D45270",
      "#D5536F",
      "#D5546E",
      "#D6556D",
      "#D7566C",
      "#D8576B",
      "#D9586A",
      "#DA5A6A",
      "#DA5B69",
      "#DB5C68",
      "#DC5D67",
      "#DD5E66",
      "#DE5F65",
      "#DE6164",
      "#DF6263",
      "#E06363",
      "#E16462",
      "#E26561",
      "#E26660",
      "#E3685F",
      "#E4695E",
      "#E56A5D",
      "#E56B5D",
      "#E66C5C",
      "#E76E5B",
      "#E76F5A",
      "#E87059",
      "#E97158",
      "#E97257",
      "#EA7457",
      "#EB7556",
      "#EB7655",
      "#EC7754",
      "#ED7953",
      "#ED7A52",
      "#EE7B51",
      "#EF7C51",
      "#EF7E50",
      "#F07F4F",
      "#F0804E",
      "#F1814D",
      "#F1834C",
      "#F2844B",
      "#F3854B",
      "#F3874A",
      "#F48849",
      "#F48948",
      "#F58B47",
      "#F58C46",
      "#F68D45",
      "#F68F44",
      "#F79044",
      "#F79143",
      "#F79342",
      "#F89441",
      "#F89540",
      "#F9973F",
      "#F9983E",
      "#F99A3E",
      "#FA9B3D",
      "#FA9C3C",
      "#FA9E3B",
      "#FB9F3A",
      "#FBA139",
      "#FBA238",
      "#FCA338",
      "#FCA537",
      "#FCA636",
      "#FCA835",
      "#FCA934",
      "#FDAB33",
      "#FDAC33",
      "#FDAE32",
      "#FDAF31",
      "#FDB130",
      "#FDB22F",
      "#FDB42F",
      "#FDB52E",
      "#FEB72D",
      "#FEB82C",
      "#FEBA2C",
      "#FEBB2B",
      "#FEBD2A",
      "#FEBE2A",
      "#FEC029",
      "#FDC229",
      "#FDC328",
      "#FDC527",
      "#FDC627",
      "#FDC827",
      "#FDCA26",
      "#FDCB26",
      "#FCCD25",
      "#FCCE25",
      "#FCD025",
      "#FCD225",
      "#FBD324",
      "#FBD524",
      "#FBD724",
      "#FAD824",
      "#FADA24",
      "#F9DC24",
      "#F9DD25",
      "#F8DF25",
      "#F8E125",
      "#F7E225",
      "#F7E425",
      "#F6E626",
      "#F6E826",
      "#F5E926",
      "#F5EB27",
      "#F4ED27",
      "#F3EE27",
      "#F3F027",
      "#F2F227",
      "#F1F426",
      "#F1F525",
      "#F0F724",
      "#F0F921"
    ],
    viridis: [
      "#440154",
      "#440256",
      "#450457",
      "#450559",
      "#46075A",
      "#46085C",
      "#460A5D",
      "#460B5E",
      "#470D60",
      "#470E61",
      "#471063",
      "#471164",
      "#471365",
      "#481467",
      "#481668",
      "#481769",
      "#48186A",
      "#481A6C",
      "#481B6D",
      "#481C6E",
      "#481D6F",
      "#481F70",
      "#482071",
      "#482173",
      "#482374",
      "#482475",
      "#482576",
      "#482677",
      "#482878",
      "#482979",
      "#472A7A",
      "#472C7A",
      "#472D7B",
      "#472E7C",
      "#472F7D",
      "#46307E",
      "#46327E",
      "#46337F",
      "#463480",
      "#453581",
      "#453781",
      "#453882",
      "#443983",
      "#443A83",
      "#443B84",
      "#433D84",
      "#433E85",
      "#423F85",
      "#424086",
      "#424186",
      "#414287",
      "#414487",
      "#404588",
      "#404688",
      "#3F4788",
      "#3F4889",
      "#3E4989",
      "#3E4A89",
      "#3E4C8A",
      "#3D4D8A",
      "#3D4E8A",
      "#3C4F8A",
      "#3C508B",
      "#3B518B",
      "#3B528B",
      "#3A538B",
      "#3A548C",
      "#39558C",
      "#39568C",
      "#38588C",
      "#38598C",
      "#375A8C",
      "#375B8D",
      "#365C8D",
      "#365D8D",
      "#355E8D",
      "#355F8D",
      "#34608D",
      "#34618D",
      "#33628D",
      "#33638D",
      "#32648E",
      "#32658E",
      "#31668E",
      "#31678E",
      "#31688E",
      "#30698E",
      "#306A8E",
      "#2F6B8E",
      "#2F6C8E",
      "#2E6D8E",
      "#2E6E8E",
      "#2E6F8E",
      "#2D708E",
      "#2D718E",
      "#2C718E",
      "#2C728E",
      "#2C738E",
      "#2B748E",
      "#2B758E",
      "#2A768E",
      "#2A778E",
      "#2A788E",
      "#29798E",
      "#297A8E",
      "#297B8E",
      "#287C8E",
      "#287D8E",
      "#277E8E",
      "#277F8E",
      "#27808E",
      "#26818E",
      "#26828E",
      "#26828E",
      "#25838E",
      "#25848E",
      "#25858E",
      "#24868E",
      "#24878E",
      "#23888E",
      "#23898E",
      "#238A8D",
      "#228B8D",
      "#228C8D",
      "#228D8D",
      "#218E8D",
      "#218F8D",
      "#21908D",
      "#21918C",
      "#20928C",
      "#20928C",
      "#20938C",
      "#1F948C",
      "#1F958B",
      "#1F968B",
      "#1F978B",
      "#1F988B",
      "#1F998A",
      "#1F9A8A",
      "#1E9B8A",
      "#1E9C89",
      "#1E9D89",
      "#1F9E89",
      "#1F9F88",
      "#1FA088",
      "#1FA188",
      "#1FA187",
      "#1FA287",
      "#20A386",
      "#20A486",
      "#21A585",
      "#21A685",
      "#22A785",
      "#22A884",
      "#23A983",
      "#24AA83",
      "#25AB82",
      "#25AC82",
      "#26AD81",
      "#27AD81",
      "#28AE80",
      "#29AF7F",
      "#2AB07F",
      "#2CB17E",
      "#2DB27D",
      "#2EB37C",
      "#2FB47C",
      "#31B57B",
      "#32B67A",
      "#34B679",
      "#35B779",
      "#37B878",
      "#38B977",
      "#3ABA76",
      "#3BBB75",
      "#3DBC74",
      "#3FBC73",
      "#40BD72",
      "#42BE71",
      "#44BF70",
      "#46C06F",
      "#48C16E",
      "#4AC16D",
      "#4CC26C",
      "#4EC36B",
      "#50C46A",
      "#52C569",
      "#54C568",
      "#56C667",
      "#58C765",
      "#5AC864",
      "#5CC863",
      "#5EC962",
      "#60CA60",
      "#63CB5F",
      "#65CB5E",
      "#67CC5C",
      "#69CD5B",
      "#6CCD5A",
      "#6ECE58",
      "#70CF57",
      "#73D056",
      "#75D054",
      "#77D153",
      "#7AD151",
      "#7CD250",
      "#7FD34E",
      "#81D34D",
      "#84D44B",
      "#86D549",
      "#89D548",
      "#8BD646",
      "#8ED645",
      "#90D743",
      "#93D741",
      "#95D840",
      "#98D83E",
      "#9BD93C",
      "#9DD93B",
      "#A0DA39",
      "#A2DA37",
      "#A5DB36",
      "#A8DB34",
      "#AADC32",
      "#ADDC30",
      "#B0DD2F",
      "#B2DD2D",
      "#B5DE2B",
      "#B8DE29",
      "#BADE28",
      "#BDDF26",
      "#C0DF25",
      "#C2DF23",
      "#C5E021",
      "#C8E020",
      "#CAE11F",
      "#CDE11D",
      "#D0E11C",
      "#D2E21B",
      "#D5E21A",
      "#D8E219",
      "#DAE319",
      "#DDE318",
      "#DFE318",
      "#E2E418",
      "#E5E419",
      "#E7E419",
      "#EAE51A",
      "#ECE51B",
      "#EFE51C",
      "#F1E51D",
      "#F4E61E",
      "#F6E620",
      "#F8E621",
      "#FBE723",
      "#FDE725"
    ],
    cividis: [
      "#00204D",
      "#00214E",
      "#002250",
      "#002252",
      "#002353",
      "#002455",
      "#002557",
      "#002558",
      "#00265A",
      "#00275C",
      "#00275E",
      "#002860",
      "#002961",
      "#002A63",
      "#002A65",
      "#002B67",
      "#002C69",
      "#002C6A",
      "#002D6C",
      "#002E6E",
      "#002E6F",
      "#002F6F",
      "#002F6F",
      "#00306F",
      "#00306F",
      "#00316F",
      "#00326F",
      "#00336F",
      "#00336F",
      "#00346F",
      "#00356E",
      "#01366E",
      "#06366E",
      "#0B376E",
      "#0F386E",
      "#12386D",
      "#15396D",
      "#183A6D",
      "#1A3B6D",
      "#1D3B6D",
      "#1F3C6D",
      "#213D6D",
      "#233E6C",
      "#243E6C",
      "#263F6C",
      "#28406C",
      "#2A406C",
      "#2B416C",
      "#2D426C",
      "#2E436C",
      "#30436C",
      "#31446B",
      "#32456B",
      "#34456B",
      "#35466B",
      "#36476B",
      "#38486B",
      "#39486B",
      "#3A496B",
      "#3B4A6B",
      "#3D4A6B",
      "#3E4B6B",
      "#3F4C6B",
      "#404D6B",
      "#414D6B",
      "#424E6B",
      "#434F6B",
      "#444F6B",
      "#46506B",
      "#47516B",
      "#48526B",
      "#49526B",
      "#4A536B",
      "#4B546C",
      "#4C546C",
      "#4D556C",
      "#4E566C",
      "#4F576C",
      "#50576C",
      "#51586C",
      "#52596C",
      "#53596C",
      "#545A6C",
      "#555B6D",
      "#565C6D",
      "#575C6D",
      "#585D6D",
      "#595E6D",
      "#595F6D",
      "#5A5F6D",
      "#5B606E",
      "#5C616E",
      "#5D616E",
      "#5E626E",
      "#5F636E",
      "#60646F",
      "#61646F",
      "#62656F",
      "#63666F",
      "#64666F",
      "#646770",
      "#656870",
      "#666970",
      "#676970",
      "#686A71",
      "#696B71",
      "#6A6C71",
      "#6B6C71",
      "#6C6D72",
      "#6C6E72",
      "#6D6E72",
      "#6E6F73",
      "#6F7073",
      "#707173",
      "#717174",
      "#727274",
      "#727374",
      "#737475",
      "#747475",
      "#757575",
      "#767676",
      "#777776",
      "#787777",
      "#787877",
      "#797977",
      "#7A7A78",
      "#7B7A78",
      "#7C7B78",
      "#7D7C78",
      "#7E7D78",
      "#7F7D78",
      "#807E79",
      "#817F79",
      "#828079",
      "#838079",
      "#848179",
      "#848279",
      "#858379",
      "#868379",
      "#878479",
      "#888579",
      "#898679",
      "#8A8779",
      "#8B8779",
      "#8C8879",
      "#8D8979",
      "#8E8A79",
      "#8F8A79",
      "#908B79",
      "#918C78",
      "#928D78",
      "#938E78",
      "#948E78",
      "#958F78",
      "#969078",
      "#979178",
      "#989278",
      "#999278",
      "#9A9377",
      "#9B9477",
      "#9C9577",
      "#9D9677",
      "#9E9677",
      "#9F9777",
      "#A09877",
      "#A19976",
      "#A29A76",
      "#A39A76",
      "#A49B76",
      "#A59C76",
      "#A69D75",
      "#A89E75",
      "#A99F75",
      "#AA9F75",
      "#ABA074",
      "#ACA174",
      "#ADA274",
      "#AEA374",
      "#AFA473",
      "#B0A473",
      "#B1A573",
      "#B2A672",
      "#B3A772",
      "#B4A872",
      "#B5A971",
      "#B6A971",
      "#B7AA71",
      "#B8AB70",
      "#B9AC70",
      "#BAAD70",
      "#BBAE6F",
      "#BCAF6F",
      "#BEAF6F",
      "#BFB06E",
      "#C0B16E",
      "#C1B26D",
      "#C2B36D",
      "#C3B46D",
      "#C4B56C",
      "#C5B56C",
      "#C6B66B",
      "#C7B76B",
      "#C8B86A",
      "#C9B96A",
      "#CBBA69",
      "#CCBB69",
      "#CDBC68",
      "#CEBC68",
      "#CFBD67",
      "#D0BE67",
      "#D1BF66",
      "#D2C066",
      "#D3C165",
      "#D4C264",
      "#D6C364",
      "#D7C463",
      "#D8C563",
      "#D9C562",
      "#DAC661",
      "#DBC761",
      "#DCC860",
      "#DDC95F",
      "#DECA5F",
      "#E0CB5E",
      "#E1CC5D",
      "#E2CD5C",
      "#E3CE5C",
      "#E4CF5B",
      "#E5D05A",
      "#E6D159",
      "#E8D259",
      "#E9D358",
      "#EAD357",
      "#EBD456",
      "#ECD555",
      "#EDD654",
      "#EFD753",
      "#F0D852",
      "#F1D951",
      "#F2DA50",
      "#F3DB4F",
      "#F4DC4E",
      "#F6DD4D",
      "#F7DE4C",
      "#F8DF4B",
      "#F9E04A",
      "#FAE149",
      "#FBE248",
      "#FDE346",
      "#FEE445",
      "#FFE544",
      "#FFE642",
      "#FFE742",
      "#FFE843",
      "#FFE944",
      "#FFEA46"
    ],
    rocket: [
      "#03051A",
      "#04051A",
      "#05061B",
      "#06071C",
      "#07071D",
      "#08081E",
      "#0A091F",
      "#0B0920",
      "#0D0A21",
      "#0E0B22",
      "#100B23",
      "#110C24",
      "#130D25",
      "#140E26",
      "#160E27",
      "#170F28",
      "#180F29",
      "#1A102A",
      "#1B112B",
      "#1D112C",
      "#1E122D",
      "#20122E",
      "#211330",
      "#221331",
      "#241432",
      "#251433",
      "#271534",
      "#281535",
      "#2A1636",
      "#2B1637",
      "#2D1738",
      "#2E1739",
      "#30173A",
      "#31183B",
      "#33183C",
      "#34193D",
      "#35193E",
      "#37193F",
      "#381A40",
      "#3A1A41",
      "#3C1A42",
      "#3D1A42",
      "#3F1B43",
      "#401B44",
      "#421B45",
      "#431C46",
      "#451C47",
      "#461C48",
      "#481C48",
      "#491D49",
      "#4B1D4A",
      "#4C1D4B",
      "#4E1D4B",
      "#501D4C",
      "#511E4D",
      "#531E4D",
      "#541E4E",
      "#561E4F",
      "#581E4F",
      "#591E50",
      "#5B1E51",
      "#5C1E51",
      "#5E1F52",
      "#601F52",
      "#611F53",
      "#631F53",
      "#641F54",
      "#661F54",
      "#681F55",
      "#691F55",
      "#6B1F56",
      "#6D1F56",
      "#6E1F57",
      "#701F57",
      "#711F57",
      "#731F58",
      "#751F58",
      "#761F58",
      "#781F59",
      "#7A1F59",
      "#7B1F59",
      "#7D1F5A",
      "#7F1E5A",
      "#811E5A",
      "#821E5A",
      "#841E5A",
      "#861E5B",
      "#871E5B",
      "#891E5B",
      "#8B1D5B",
      "#8C1D5B",
      "#8E1D5B",
      "#901D5B",
      "#921C5B",
      "#931C5B",
      "#951C5B",
      "#971C5B",
      "#981B5B",
      "#9A1B5B",
      "#9C1B5B",
      "#9E1A5B",
      "#9F1A5B",
      "#A11A5B",
      "#A3195B",
      "#A4195B",
      "#A6195A",
      "#A8185A",
      "#AA185A",
      "#AB185A",
      "#AD1759",
      "#AF1759",
      "#B01759",
      "#B21758",
      "#B41658",
      "#B51657",
      "#B71657",
      "#B91657",
      "#BA1656",
      "#BC1656",
      "#BD1655",
      "#BF1654",
      "#C11754",
      "#C21753",
      "#C41753",
      "#C51852",
      "#C71951",
      "#C81951",
      "#CA1A50",
      "#CB1B4F",
      "#CD1C4E",
      "#CE1D4E",
      "#CF1E4D",
      "#D11F4C",
      "#D2204C",
      "#D3214B",
      "#D5224A",
      "#D62449",
      "#D72549",
      "#D82748",
      "#D92847",
      "#DB2946",
      "#DC2B46",
      "#DD2C45",
      "#DE2E44",
      "#DF2F44",
      "#E03143",
      "#E13342",
      "#E23442",
      "#E33641",
      "#E43841",
      "#E53940",
      "#E63B40",
      "#E73D3F",
      "#E83F3F",
      "#E8403E",
      "#E9423E",
      "#EA443E",
      "#EB463E",
      "#EB483E",
      "#EC4A3E",
      "#EC4C3E",
      "#ED4E3E",
      "#ED503E",
      "#EE523F",
      "#EE543F",
      "#EF5640",
      "#EF5840",
      "#EF5A41",
      "#F05C42",
      "#F05E42",
      "#F06043",
      "#F16244",
      "#F16445",
      "#F16646",
      "#F26747",
      "#F26948",
      "#F26B49",
      "#F26D4B",
      "#F26F4C",
      "#F3714D",
      "#F3734E",
      "#F37450",
      "#F37651",
      "#F37852",
      "#F47A54",
      "#F47C55",
      "#F47D57",
      "#F47F58",
      "#F4815A",
      "#F4835B",
      "#F4845D",
      "#F4865E",
      "#F58860",
      "#F58A61",
      "#F58B63",
      "#F58D64",
      "#F58F66",
      "#F59067",
      "#F59269",
      "#F5946B",
      "#F5966C",
      "#F5976E",
      "#F59970",
      "#F69B71",
      "#F69C73",
      "#F69E75",
      "#F6A077",
      "#F6A178",
      "#F6A37A",
      "#F6A47C",
      "#F6A67E",
      "#F6A880",
      "#F6A981",
      "#F6AB83",
      "#F6AD85",
      "#F6AE87",
      "#F6B089",
      "#F6B18B",
      "#F6B38D",
      "#F6B48F",
      "#F6B691",
      "#F6B893",
      "#F6B995",
      "#F6BB97",
      "#F6BC99",
      "#F6BE9B",
      "#F6BF9D",
      "#F6C19F",
      "#F7C2A2",
      "#F7C4A4",
      "#F7C6A6",
      "#F7C7A8",
      "#F7C9AA",
      "#F7CAAC",
      "#F7CCAF",
      "#F7CDB1",
      "#F7CFB3",
      "#F7D0B5",
      "#F8D1B8",
      "#F8D3BA",
      "#F8D4BC",
      "#F8D6BE",
      "#F8D7C0",
      "#F8D9C3",
      "#F8DAC5",
      "#F8DCC7",
      "#F9DDC9",
      "#F9DFCB",
      "#F9E0CD",
      "#F9E2D0",
      "#F9E3D2",
      "#F9E5D4",
      "#FAE6D6",
      "#FAE8D8",
      "#FAE9DA",
      "#FAEBDD"
    ],
    mako: [
      "#0B0405",
      "#0D0406",
      "#0E0508",
      "#0F0609",
      "#10060A",
      "#11070C",
      "#12080D",
      "#13090F",
      "#140910",
      "#150A12",
      "#160B13",
      "#170C15",
      "#180D16",
      "#190E18",
      "#1A0E19",
      "#1B0F1A",
      "#1C101C",
      "#1D111D",
      "#1E111F",
      "#1F1220",
      "#201322",
      "#211423",
      "#221425",
      "#231526",
      "#241628",
      "#251729",
      "#26172B",
      "#27182D",
      "#28192E",
      "#291930",
      "#291A31",
      "#2A1B33",
      "#2B1C35",
      "#2C1C36",
      "#2D1D38",
      "#2E1E39",
      "#2E1E3B",
      "#2F1F3D",
      "#30203E",
      "#312140",
      "#312142",
      "#322243",
      "#332345",
      "#342447",
      "#342548",
      "#35254A",
      "#35264C",
      "#36274D",
      "#37284F",
      "#372851",
      "#382953",
      "#382A54",
      "#392B56",
      "#3A2C58",
      "#3A2C59",
      "#3B2D5B",
      "#3B2E5D",
      "#3B2F5F",
      "#3C3060",
      "#3C3162",
      "#3D3164",
      "#3D3266",
      "#3E3367",
      "#3E3469",
      "#3E356B",
      "#3F366D",
      "#3F366F",
      "#3F3770",
      "#403872",
      "#403974",
      "#403A76",
      "#403B78",
      "#403C79",
      "#413D7B",
      "#413E7D",
      "#413E7F",
      "#413F80",
      "#414082",
      "#414184",
      "#414285",
      "#414387",
      "#414488",
      "#40468A",
      "#40478B",
      "#40488D",
      "#40498E",
      "#3F4A8F",
      "#3F4B90",
      "#3F4C92",
      "#3E4D93",
      "#3E4F94",
      "#3E5095",
      "#3D5195",
      "#3D5296",
      "#3C5397",
      "#3C5598",
      "#3B5698",
      "#3B5799",
      "#3B589A",
      "#3A599A",
      "#3A5B9B",
      "#3A5C9B",
      "#395D9C",
      "#395E9C",
      "#385F9C",
      "#38619D",
      "#38629D",
      "#38639D",
      "#37649E",
      "#37659E",
      "#37669E",
      "#37689F",
      "#36699F",
      "#366A9F",
      "#366B9F",
      "#366CA0",
      "#366DA0",
      "#366FA0",
      "#3670A0",
      "#3671A0",
      "#3572A1",
      "#3573A1",
      "#3574A1",
      "#3575A1",
      "#3576A2",
      "#3578A2",
      "#3579A2",
      "#357AA2",
      "#357BA3",
      "#357CA3",
      "#357DA3",
      "#357EA4",
      "#347FA4",
      "#3480A4",
      "#3482A4",
      "#3483A5",
      "#3484A5",
      "#3485A5",
      "#3486A5",
      "#3487A6",
      "#3488A6",
      "#3489A6",
      "#348BA6",
      "#348CA7",
      "#348DA7",
      "#348EA7",
      "#348FA7",
      "#3490A8",
      "#3491A8",
      "#3492A8",
      "#3493A8",
      "#3495A9",
      "#3496A9",
      "#3497A9",
      "#3498A9",
      "#3499AA",
      "#349AAA",
      "#359BAA",
      "#359CAA",
      "#359EAA",
      "#359FAB",
      "#35A0AB",
      "#35A1AB",
      "#36A2AB",
      "#36A3AB",
      "#36A4AB",
      "#37A5AC",
      "#37A6AC",
      "#37A8AC",
      "#38A9AC",
      "#38AAAC",
      "#39ABAC",
      "#39ACAC",
      "#3AADAC",
      "#3AAEAD",
      "#3BAFAD",
      "#3CB1AD",
      "#3CB2AD",
      "#3DB3AD",
      "#3EB4AD",
      "#3FB5AD",
      "#3FB6AD",
      "#40B7AD",
      "#41B8AD",
      "#42B9AD",
      "#43BAAD",
      "#44BCAD",
      "#45BDAD",
      "#46BEAD",
      "#47BFAD",
      "#48C0AD",
      "#49C1AD",
      "#4BC2AD",
      "#4CC3AD",
      "#4DC4AD",
      "#4FC5AD",
      "#50C6AD",
      "#52C7AD",
      "#53C9AD",
      "#55CAAD",
      "#57CBAD",
      "#59CCAD",
      "#5BCDAD",
      "#5ECDAD",
      "#60CEAC",
      "#62CFAC",
      "#65D0AD",
      "#68D1AD",
      "#6AD2AD",
      "#6DD3AD",
      "#70D4AD",
      "#73D4AD",
      "#76D5AE",
      "#79D6AE",
      "#7CD6AF",
      "#7FD7AF",
      "#82D8B0",
      "#85D9B1",
      "#88D9B1",
      "#8BDAB2",
      "#8EDBB3",
      "#91DBB4",
      "#94DCB5",
      "#96DDB5",
      "#99DDB6",
      "#9CDEB7",
      "#9EDFB8",
      "#A1DFB9",
      "#A4E0BB",
      "#A6E1BC",
      "#A9E1BD",
      "#ABE2BE",
      "#AEE3C0",
      "#B0E4C1",
      "#B2E4C2",
      "#B5E5C4",
      "#B7E6C5",
      "#B9E6C7",
      "#BBE7C8",
      "#BEE8CA",
      "#C0E9CC",
      "#C2E9CD",
      "#C4EACF",
      "#C6EBD1",
      "#C8ECD2",
      "#CAEDD4",
      "#CCEDD6",
      "#CEEED7",
      "#D0EFD9",
      "#D2F0DB",
      "#D4F1DC",
      "#D6F1DE",
      "#D8F2E0",
      "#DAF3E1",
      "#DCF4E3",
      "#DEF5E5"
    ],
    turbo: [
      "#30123B",
      "#321543",
      "#33184A",
      "#341B51",
      "#351E58",
      "#36215F",
      "#372466",
      "#38276D",
      "#392A73",
      "#3A2D79",
      "#3B2F80",
      "#3C3286",
      "#3D358B",
      "#3E3891",
      "#3F3B97",
      "#3F3E9C",
      "#4040A2",
      "#4143A7",
      "#4146AC",
      "#4249B1",
      "#424BB5",
      "#434EBA",
      "#4451BF",
      "#4454C3",
      "#4456C7",
      "#4559CB",
      "#455CCF",
      "#455ED3",
      "#4661D6",
      "#4664DA",
      "#4666DD",
      "#4669E0",
      "#466BE3",
      "#476EE6",
      "#4771E9",
      "#4773EB",
      "#4776EE",
      "#4778F0",
      "#477BF2",
      "#467DF4",
      "#4680F6",
      "#4682F8",
      "#4685FA",
      "#4687FB",
      "#458AFC",
      "#458CFD",
      "#448FFE",
      "#4391FE",
      "#4294FF",
      "#4196FF",
      "#4099FF",
      "#3E9BFE",
      "#3D9EFE",
      "#3BA0FD",
      "#3AA3FC",
      "#38A5FB",
      "#37A8FA",
      "#35ABF8",
      "#33ADF7",
      "#31AFF5",
      "#2FB2F4",
      "#2EB4F2",
      "#2CB7F0",
      "#2AB9EE",
      "#28BCEB",
      "#27BEE9",
      "#25C0E7",
      "#23C3E4",
      "#22C5E2",
      "#20C7DF",
      "#1FC9DD",
      "#1ECBDA",
      "#1CCDD8",
      "#1BD0D5",
      "#1AD2D2",
      "#1AD4D0",
      "#19D5CD",
      "#18D7CA",
      "#18D9C8",
      "#18DBC5",
      "#18DDC2",
      "#18DEC0",
      "#18E0BD",
      "#19E2BB",
      "#19E3B9",
      "#1AE4B6",
      "#1CE6B4",
      "#1DE7B2",
      "#1FE9AF",
      "#20EAAC",
      "#22EBAA",
      "#25ECA7",
      "#27EEA4",
      "#2AEFA1",
      "#2CF09E",
      "#2FF19B",
      "#32F298",
      "#35F394",
      "#38F491",
      "#3CF58E",
      "#3FF68A",
      "#43F787",
      "#46F884",
      "#4AF880",
      "#4EF97D",
      "#52FA7A",
      "#55FA76",
      "#59FB73",
      "#5DFC6F",
      "#61FC6C",
      "#65FD69",
      "#69FD66",
      "#6DFE62",
      "#71FE5F",
      "#75FE5C",
      "#79FE59",
      "#7DFF56",
      "#80FF53",
      "#84FF51",
      "#88FF4E",
      "#8BFF4B",
      "#8FFF49",
      "#92FF47",
      "#96FE44",
      "#99FE42",
      "#9CFE40",
      "#9FFD3F",
      "#A1FD3D",
      "#A4FC3C",
      "#A7FC3A",
      "#A9FB39",
      "#ACFB38",
      "#AFFA37",
      "#B1F936",
      "#B4F836",
      "#B7F735",
      "#B9F635",
      "#BCF534",
      "#BEF434",
      "#C1F334",
      "#C3F134",
      "#C6F034",
      "#C8EF34",
      "#CBED34",
      "#CDEC34",
      "#D0EA34",
      "#D2E935",
      "#D4E735",
      "#D7E535",
      "#D9E436",
      "#DBE236",
      "#DDE037",
      "#DFDF37",
      "#E1DD37",
      "#E3DB38",
      "#E5D938",
      "#E7D739",
      "#E9D539",
      "#EBD339",
      "#ECD13A",
      "#EECF3A",
      "#EFCD3A",
      "#F1CB3A",
      "#F2C93A",
      "#F4C73A",
      "#F5C53A",
      "#F6C33A",
      "#F7C13A",
      "#F8BE39",
      "#F9BC39",
      "#FABA39",
      "#FBB838",
      "#FBB637",
      "#FCB336",
      "#FCB136",
      "#FDAE35",
      "#FDAC34",
      "#FEA933",
      "#FEA732",
      "#FEA431",
      "#FEA130",
      "#FE9E2F",
      "#FE9B2D",
      "#FE992C",
      "#FE962B",
      "#FE932A",
      "#FE9029",
      "#FD8D27",
      "#FD8A26",
      "#FC8725",
      "#FC8423",
      "#FB8122",
      "#FB7E21",
      "#FA7B1F",
      "#F9781E",
      "#F9751D",
      "#F8721C",
      "#F76F1A",
      "#F66C19",
      "#F56918",
      "#F46617",
      "#F36315",
      "#F26014",
      "#F15D13",
      "#F05B12",
      "#EF5811",
      "#ED5510",
      "#EC530F",
      "#EB500E",
      "#EA4E0D",
      "#E84B0C",
      "#E7490C",
      "#E5470B",
      "#E4450A",
      "#E2430A",
      "#E14109",
      "#DF3F08",
      "#DD3D08",
      "#DC3B07",
      "#DA3907",
      "#D83706",
      "#D63506",
      "#D43305",
      "#D23105",
      "#D02F05",
      "#CE2D04",
      "#CC2B04",
      "#CA2A04",
      "#C82803",
      "#C52603",
      "#C32503",
      "#C12302",
      "#BE2102",
      "#BC2002",
      "#B91E02",
      "#B71D02",
      "#B41B01",
      "#B21A01",
      "#AF1801",
      "#AC1701",
      "#A91601",
      "#A71401",
      "#A41301",
      "#A11201",
      "#9E1001",
      "#9B0F01",
      "#980E01",
      "#950D01",
      "#920B01",
      "#8E0A01",
      "#8B0902",
      "#880802",
      "#850702",
      "#810602",
      "#7E0502",
      "#7A0403"
    ]
  };

  // src/charts/spectrogram/config.ts
  var SPECTROGRAM_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "sample_rate",
    "n_fft",
    "window",
    "overlap",
    "power",
    "db",
    "db_range",
    "freq_scale",
    "freq_range",
    "time_range",
    "freq_unit",
    "time_unit",
    "colormap",
    "colormap_reverse",
    "n_colors",
    "zlim",
    "show_colorbar",
    "colorbar_title",
    "xlab",
    "ylab",
    "margin_top",
    "margin_right",
    "margin_bottom",
    "margin_left",
    "legend_position",
    "legend_placement",
    "legend_title"
  ]);
  function spectrogramColors(colormap, n, reverse, zlim, appearance = {}) {
    let colors;
    const name = typeof colormap === "string" ? colormap : colormap.length === 1 ? colormap[0] : null;
    if (name !== null && name !== void 0) {
      if (name === "diverging") {
        const sym = zlim[0] < 0 && zlim[1] > 0 ? [
          -Math.max(Math.abs(zlim[0]), Math.abs(zlim[1])),
          Math.max(Math.abs(zlim[0]), Math.abs(zlim[1]))
        ] : zlim[0] === zlim[1] ? [-1, 1] : [zlim[0], zlim[1]];
        const sign = appearance.signColors ?? SIGN_COLORS;
        const bg = appearance.background ?? (appearance.dark ? darkThemeTokens() : lightThemeTokens()).bg_color;
        colors = divergingRamp(sign.negative, bg, sign.positive, sym, n);
      } else {
        const table = COLORMAPS[name];
        if (!table) {
          throw new Error(
            `Colormap '${name}' is not recognized; use one of ${Object.keys(COLORMAPS).join(", ")}, diverging, or two or more colors.`
          );
        }
        colors = n === table.length ? [...table] : colorRamp(table, n);
      }
    } else {
      colors = colorRamp(colormap, n);
    }
    return reverse ? colors.reverse() : colors;
  }
  function approx(x, y, xout) {
    const pts = x.flatMap((xi, i) => {
      const yi = y[i];
      return typeof yi === "number" && Number.isFinite(yi) ? [[xi, yi]] : [];
    });
    if (pts.length === 0) return xout.map(() => null);
    return xout.map((v) => {
      const first = pts[0];
      const last = pts[pts.length - 1];
      if (v <= first[0]) return first[1];
      if (v >= last[0]) return last[1];
      let k = 0;
      while (k < pts.length - 2 && v > pts[k + 1][0]) k++;
      const [x0, y0] = pts[k];
      const [x1, y1] = pts[k + 1];
      return y0 + (y1 - y0) * (v - x0) / (x1 - x0);
    });
  }
  var range2 = (r) => r && r.length >= 2 ? [r[0], r[1]] : null;
  function niceBinTicks(values, log) {
    const ticks = /* @__PURE__ */ new Map();
    const n = values.length;
    if (n === 0) return ticks;
    const at = (i) => values[Math.min(Math.max(i, 0), n - 1)];
    const scale = (v) => log ? Math.log10(v) : v;
    const halfBin = (i) => {
      const lo = Math.max(i - 1, 0);
      const hi = Math.min(i + 1, n - 1);
      return hi > lo ? Math.abs(scale(at(hi)) - scale(at(lo))) / (hi - lo) / 2 : Infinity;
    };
    const first = at(0);
    const last = at(n - 1);
    const candidates = [];
    if (log) {
      if (first > 0) {
        for (let e = Math.floor(Math.log10(first)); e <= Math.ceil(Math.log10(last)); e++) {
          for (const m of [1, 2, 5]) {
            const v = Number((m * 10 ** e).toPrecision(12));
            candidates.push([v, String(v)]);
          }
        }
      }
    } else {
      const nice = niceAxis(first, last);
      const label = tickLabel(nice.decimals);
      for (let k = Math.ceil(first / nice.interval - 1e-9); k * nice.interval <= last * (1 + 1e-12) + 1e-12; k++) {
        const v = Number((k * nice.interval).toPrecision(12));
        candidates.push([v, label(v)]);
      }
    }
    for (const [v, text] of candidates) {
      let best = 0;
      for (let i = 1; i < n; i++) {
        if (Math.abs(scale(at(i)) - scale(v)) < Math.abs(scale(at(best)) - scale(v))) best = i;
      }
      if (Math.abs(scale(at(best)) - scale(v)) <= halfBin(best) && !ticks.has(best)) {
        ticks.set(best, text);
      }
    }
    return ticks;
  }
  function buildSpectrogramOption(data, config, appearance = {}) {
    let freq = [...data.frequency];
    let time = [...data.time];
    let rows = data.values.map((row) => row.map((v) => typeof v === "number" ? v : null));
    if (rows.length !== freq.length || rows.some((row) => row.length !== time.length)) {
      throw new Error(
        "A spectrogram needs one row of values per frequency and one value per time in each row."
      );
    }
    const log = config.freq_scale === "log";
    const keepRows = (keep) => {
      freq = freq.filter((_, i) => keep[i]);
      rows = rows.filter((_, i) => keep[i]);
    };
    if (log) {
      const keep = freq.map((f) => f > 0);
      if (!keep.some(Boolean)) throw new Error("A log frequency scale needs a frequency above 0 Hz.");
      keepRows(keep);
    }
    const fr = range2(config.freq_range);
    if (fr) {
      const keep = freq.map((f) => f >= fr[0] && f <= fr[1]);
      if (!keep.some(Boolean)) {
        throw new Error(`No frequency bins fall within freq_range [${fr[0]}, ${fr[1]}] Hz.`);
      }
      keepRows(keep);
    }
    const tr = range2(config.time_range);
    if (tr) {
      const keep = time.map((t) => t >= tr[0] && t <= tr[1]);
      if (!keep.some(Boolean)) {
        throw new Error(`No time frames fall within time_range [${tr[0]}, ${tr[1]}] s.`);
      }
      time = time.filter((_, j) => keep[j]);
      rows = rows.map((row) => row.filter((_, j) => keep[j]));
    }
    if (log) {
      const n = freq.length;
      const lo = Math.log(freq[0]);
      const hi = Math.log(freq[n - 1]);
      const logFreq = Array.from(
        { length: n },
        (_, i) => Math.exp(n === 1 ? lo : lo + (hi - lo) * i / (n - 1))
      );
      const cols = time.map(
        (_, j) => approx(
          freq,
          rows.map((row) => row[j] ?? null),
          logFreq
        )
      );
      rows = logFreq.map((_, i) => cols.map((col) => col[i] ?? null));
      freq = logFreq;
    }
    const timeUnit = config.time_unit ?? "s";
    const freqUnit = config.freq_unit ?? "Hz";
    const timeShown = time.map((t) => timeUnit === "ms" ? t * 1e3 : t);
    const freqShown = freq.map((f) => freqUnit === "kHz" ? f / 1e3 : f);
    let zlim = range2(config.zlim);
    if (!zlim) {
      let lo = Number.POSITIVE_INFINITY;
      let hi = Number.NEGATIVE_INFINITY;
      for (const row of rows) {
        for (const v of row) {
          if (v !== null && Number.isFinite(v)) {
            if (v < lo) lo = v;
            if (v > hi) hi = v;
          }
        }
      }
      if (!Number.isFinite(lo)) {
        throw new Error("A spectrogram needs a finite value, or zlim, for its color scale.");
      }
      zlim = [lo, hi];
    }
    const db = config.db ?? true;
    const power = config.power ?? true;
    const colors = spectrogramColors(
      config.colormap ?? "magma",
      config.n_colors ?? 256,
      config.colormap_reverse ?? false,
      zlim,
      appearance
    );
    const timeDigits = timeUnit === "ms" ? 1 : 3;
    const freqDigits = freqUnit === "kHz" ? 3 : 1;
    const valueDigits = db ? 1 : niceAxis(zlim[0], zlim[1]).decimals + 1;
    const xlab = config.xlab ?? `Time (${timeUnit})`;
    const ylab = config.ylab ?? `Frequency (${freqUnit})`;
    const valueName = config.colorbar_title ?? (db ? "dB" : power ? "Power" : "Amplitude");
    const timeLabels = timeShown.map((t) => t.toFixed(timeDigits));
    const freqLabels = freqShown.map((f) => f.toFixed(freqDigits));
    const cells = time.flatMap(
      (_, j) => freq.map((_2, i) => {
        const v = rows[i]?.[j];
        return [j, i, v !== null && v !== void 0 && Number.isFinite(v) ? v : null];
      })
    );
    const set = (side) => config[`margin_${side}`];
    const anyMargin = ["top", "right", "bottom", "left"].some(
      (side) => typeof set(side) === "number"
    );
    const grid = anyMargin ? Object.fromEntries(
      ["top", "right", "bottom", "left"].flatMap((side) => {
        const v = set(side);
        return typeof v === "number" ? [[side, v]] : [];
      })
    ) : { top: config.title ? 50 : 36, right: 90, bottom: 36, left: 36 };
    const axis = (name, labels, ticks) => ({
      type: "category",
      data: labels,
      name,
      boundaryGap: true,
      splitArea: { show: false },
      axisLine: { show: false },
      // Labels and ticks sit at the bins nearest the axis's nice values.
      axisLabel: {
        interval: (index) => ticks.has(index),
        formatter: (_value, index) => ticks.get(index) ?? ""
      },
      axisTick: { interval: (index) => ticks.has(index), alignWithLabel: true }
    });
    return {
      ...config.title ? { title: { text: config.title } } : {},
      tooltip: {
        trigger: "item",
        formatter: (p) => {
          const [t, f, v] = p.value ?? [];
          if (typeof v !== "number") return "N/A";
          return `${xlab}: ${timeLabels[t]}<br/>${ylab}: ${freqLabels[f]}<br/>${valueName}: ${ddSci(v, valueDigits)}`;
        }
      },
      grid,
      xAxis: axis(xlab, timeLabels, niceBinTicks(timeShown, false)),
      yAxis: axis(ylab, freqLabels, niceBinTicks(freqShown, log)),
      visualMap: {
        type: "continuous",
        min: zlim[0],
        max: zlim[1],
        precision: valueDigits,
        calculable: true,
        show: config.show_colorbar ?? true,
        orient: "vertical",
        // The labels sit right of the bar (`align` names the bar's side), away
        // from the cells.
        align: "left",
        right: "right",
        top: "middle",
        inRange: { color: colors }
      },
      series: [{ type: "heatmap", data: cells }]
    };
  }

  // src/shared/hierarchy.ts
  var isMissing3 = (v) => v === null || v === void 0 || v === "" || typeof v === "number" && Number.isNaN(v);
  function hierarchyFromTable(data, members, chart) {
    const levels = typeof members.levels === "string" ? [members.levels] : [...members.levels ?? []];
    if (levels.length === 0) throw new Error(`${chart} needs the config member 'levels' set.`);
    const raw = levels.map((name) => column(data.columns, name, "levels"));
    const text = levels.map((name) => textColumn(data, name, "levels"));
    const valueName = members.value;
    const values = valueName ? column(data.columns, valueName, "value") : null;
    const n = raw[0]?.length ?? 0;
    const root = { name: "", value: 0, rows: [], children: /* @__PURE__ */ new Map() };
    let total = 0;
    for (let i = 0; i < n; i++) {
      if (isMissing3(raw[0]?.[i])) {
        throw new Error(
          `Column '${levels[0]}' (config member 'levels') must name a node in every row; row ${i + 1} is missing.`
        );
      }
      const v = values ? values[i] : 1;
      if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
        throw new Error(
          `Column '${valueName}' (config member 'value') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`
        );
      }
      let node = root;
      for (let d = 0; d < levels.length && !isMissing3(raw[d]?.[i]); d++) {
        const name = text[d]?.[i];
        let child = node.children.get(name);
        if (!child) {
          child = { name, value: 0, rows: [], children: /* @__PURE__ */ new Map() };
          node.children.set(name, child);
        }
        node = child;
      }
      node.value += v;
      node.rows.push(i + 1);
      total += v;
    }
    if (!(total > 0)) {
      throw new Error(
        valueName ? `${chart} needs a positive total; column '${valueName}' sums to 0.` : `${chart} needs at least one row.`
      );
    }
    const finish = (node, depth, path) => {
      const level = levels[depth];
      const names = distinct([...node.children.keys()], data.levels?.[level]);
      return names.map((name) => {
        const child = node.children.get(name);
        const at = [...path, name];
        if (child.children.size === 0) return { name, value: child.value };
        if (child.rows.length > 0) {
          throw new Error(
            `Node '${at.join(" / ")}' has children, which give its value; row ${child.rows[0]} ends at it. Give the row its deeper levels, or leave it out.`
          );
        }
        return { name, children: finish(child, depth + 1, at) };
      });
    };
    return finish(root, 0, []);
  }
  function hierarchyOf(data, members, chart) {
    return "tree" in data ? data.tree : hierarchyFromTable(data, members, chart);
  }
  function hierarchyDepth(nodes) {
    let depth = 0;
    for (const node of nodes) {
      depth = Math.max(depth, 1 + (node.children ? hierarchyDepth(node.children) : 0));
    }
    return depth;
  }

  // src/charts/treemap/config.ts
  var TREEMAP_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "levels",
    "value",
    "show_labels",
    "border_width",
    "palette"
  ]);
  function hierarchyBorder(appearance) {
    return appearance.background ?? (appearance.dark ? darkThemeTokens().bg_color : lightThemeTokens().bg_color);
  }
  function hierarchyPalette(config, appearance) {
    const own = asArray(config.palette);
    return own.length > 0 ? own : appearance.palette ?? [];
  }
  function buildTreemapOption(data, config, appearance = {}) {
    const tree = hierarchyOf(data, config, "A treemap");
    const palette = hierarchyPalette(config, appearance);
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      ...palette.length > 0 ? { color: [...palette] } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
      series: [
        {
          type: "treemap",
          // ECharts reserves room below the tiles for a breadcrumb, which the
          // treemap does not draw.
          top: config.title ? 40 : 8,
          bottom: 8,
          left: 8,
          right: 8,
          roam: false,
          nodeClick: false,
          breadcrumb: { show: false },
          label: { show: config.show_labels !== false, formatter: "{b}" },
          upperLabel: { show: false },
          // The border alone separates siblings; a gap would add a second band
          // in the parent's color.
          itemStyle: {
            borderColor: hierarchyBorder(appearance),
            borderWidth: config.border_width ?? 1,
            gapWidth: 0
          },
          data: tree
        }
      ]
    };
  }

  // src/charts/sunburst/config.ts
  var SUNBURST_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "levels",
    "value",
    "label_depth",
    "min_angle",
    "label_rotate",
    "radius",
    "inner_radius",
    "border_width",
    "palette"
  ]);
  function buildSunburstOption(data, config, appearance = {}) {
    const tree = hierarchyOf(data, config, "A sunburst");
    const palette = hierarchyPalette(config, appearance);
    const labelDepth = config.label_depth ?? 2;
    const minAngle = config.min_angle ?? 0;
    const rotate = config.label_rotate === "horizontal" ? 0 : config.label_rotate ?? "radial";
    const levels = Array.from({ length: hierarchyDepth(tree) + 1 }, (_, i) => ({
      label: { show: i > 0 && i <= labelDepth, rotate, minAngle }
    }));
    return {
      ...config.title ? { title: { text: config.title, left: "center" } } : {},
      ...palette.length > 0 ? { color: [...palette] } : {},
      tooltip: { trigger: "item", valueFormatter: tooltipValue },
      series: [
        {
          type: "sunburst",
          radius: [config.inner_radius ?? "18%", config.radius ?? "85%"],
          ...config.title ? { center: ["50%", "54%"] } : {},
          nodeClick: false,
          data: tree,
          label: { rotate, minAngle },
          itemStyle: {
            borderColor: hierarchyBorder(appearance),
            borderWidth: config.border_width ?? 1
          },
          levels
        }
      ]
    };
  }

  // src/charts/survival/renderers.ts
  var SURVIVAL_RISK = "rtemis.survival_risk.v1";
  var survivalRenderers = {
    // Data: [time, row]; row -1 is the caption. A row's series is its curve's,
    // and its text takes the curve's color. The counts sit under the time
    // axis's own coordinate of each time, so they stay aligned with its ticks
    // through resizing; the rows start `offset` pixels below the plot's bottom
    // edge, the coordinate of survival 0. The caption's datum is at the time
    // axis's minimum, so its coordinate is the plot's left edge.
    [SURVIVAL_RISK]: (params, api) => {
      const p = params.itemPayload;
      const [x, edge] = api.coord([api.value(0), 0]);
      const top = edge + p.offset;
      const row = api.value(1);
      if (row < 0) {
        return {
          type: "text",
          x: x - p.captionOffset,
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
  var Y_NAME_GAP = 45;
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
            symbol: "circle",
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
          symbol: "circle",
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
          lineStyle: {
            color: appearance.neutral ?? SIGN_COLORS.neutral,
            type: "dashed",
            width: 1,
            opacity: 0.7
          },
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
        // `layoutSurvivalRisk()` sets it from the y axis name below.
        captionOffset: 0,
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
        nameGap: 30,
        // With the strip, the axis labels exactly the risk times.
        ...riskTimes ? { axisLabel: { customValues: riskTimes }, axisTick: { customValues: riskTimes } } : {}
      },
      yAxis: {
        type: "value",
        min: 0,
        max: 1,
        name: config.ylab ?? "Survival probability",
        nameGap: Y_NAME_GAP
      },
      legend: {
        show: legendShown,
        data: names,
        padding: 0,
        itemStyle: { opacity: 1 }
      },
      tooltip: { trigger: "item", confine: true },
      series
    });
  }
  function firstMargin(margin, fallback) {
    if (Array.isArray(margin)) return Number(margin[0]) || 0;
    return typeof margin === "number" ? margin : fallback;
  }
  function layoutSurvivalRisk(option, tokens) {
    const series = Array.isArray(option.series) ? option.series : [];
    const strips = series.filter((s) => s?.renderItem === SURVIVAL_RISK);
    if (strips.length === 0) return option;
    const x = Array.isArray(option.xAxis) ? option.xAxis[0] : option.xAxis;
    const sizes = tokens ? themeFontSizes(tokens) : null;
    const size = (style, fallback = 12) => {
      const v = style?.fontSize;
      return typeof v === "number" ? v : fallback;
    };
    const labelSize = sizes?.axisLabel ?? 12;
    const nameSize = sizes?.base ?? 12;
    const themeMargin = tokens?.axis_name_gap === void 0 || tokens.axis_name_gap === null ? 0 : tokens.axis_name_gap * nameSize;
    const nameGap = typeof x?.nameGap === "number" ? x.nameGap : 30;
    const fontSize = size(x?.axisLabel, labelSize);
    const margin = x?.nameTextStyle?.textMargin;
    const [above, below] = Array.isArray(margin) ? [Number(margin[0]) || 0, Number(margin[2] ?? margin[0]) || 0] : typeof margin === "number" ? [margin, margin] : [themeMargin, themeMargin];
    const offset = nameGap + above + Math.ceil(size(x?.nameTextStyle, nameSize) * 1.3) + below + RISK_GAP;
    const lineHeight = Math.round(fontSize * 1.6);
    const y = Array.isArray(option.yAxis) ? option.yAxis[0] : option.yAxis;
    const location = y?.nameLocation ?? tokens?.axis_name_location ?? "middle";
    const centered = location === "middle" || location === "center";
    const yGap = centered && typeof y?.nameGap === "number" ? y.nameGap : Y_NAME_GAP;
    const yMargin = firstMargin(
      y?.nameTextStyle?.textMargin,
      themeMargin
    );
    const yLabel = y?.axisLabel;
    const labelsExtent = (typeof yLabel?.margin === "number" ? yLabel.margin : 8) + 4 * 0.6 * size(y?.axisLabel, labelSize);
    const nameEdge = Math.max(yGap, Y_NAME_GAP, centered && yMargin > 0 ? labelsExtent + yMargin : 0);
    const captionOffset = nameEdge + size(y?.nameTextStyle, nameSize) / 2;
    let rows = 0;
    for (const s of strips) {
      const payload = s.itemPayload;
      rows = payload.rows;
      s.itemPayload = { ...payload, offset, lineHeight, fontSize, captionOffset };
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
          fill: api.visual("color"),
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
      const present3 = [];
      for (let k = start; k <= end; k++) {
        const v = values[k];
        if (typeof v === "number") present3.push(v);
      }
      return present3.length < required ? null : SUMMARIES[fn](present3);
    });
  }
  function samples(data, config) {
    const x = config.x;
    const yColumns = asArray(config.y);
    if (!x || yColumns.length === 0) {
      throw new Error("A time series chart needs both `x` and `y` set to draw.");
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
    const y2Columns = asArray(config.y2);
    if (config.x2 && y2Columns.length === 0) {
      throw new Error("Supply `y2` with `x2`, or unset both.");
    }
    const secondaryX = config.x2 ? times(config.x2, "x2") : primaryX;
    if (secondaryX.time !== primaryX.time) {
      throw new Error("Use numeric time throughout, or dates and date-times throughout.");
    }
    const bound = [
      ...yColumns.map((name) => ({ name, label: name, axis: 0, member: "y" })),
      ...y2Columns.map((name) => ({
        name,
        label: yColumns.includes(name) ? `${name} (right)` : name,
        axis: 1,
        member: "y2"
      }))
    ];
    const split = (axis) => {
      const xs = axis === 0 ? primaryX.values : secondaryX.values;
      const parts = bound.filter((b) => b.axis === axis);
      if (!config.group) {
        return parts.map((b) => sample(b.label, axis, xs, numbers(b.name, b.member), () => true));
      }
      const groups = textColumn(data, config.group, "group");
      const raw = column(data.columns, config.group, "group");
      const present3 = (i) => raw[i] !== null && raw[i] !== void 0;
      const levels = distinct(
        groups.filter((_, i) => present3(i)),
        data.levels?.[config.group]
      );
      return parts.flatMap((b) => {
        const ys = numbers(b.name, b.member);
        return levels.map(
          (level) => sample(
            bound.length === 1 ? level : `${b.label} / ${level}`,
            axis,
            xs,
            ys,
            (i) => present3(i) && groups[i] === level
          )
        );
      });
    };
    const primary = split(0);
    const secondary = split(1);
    if (![...primary, ...secondary].some((s) => s.y.some((v) => v !== null))) {
      throw new Error("A time series chart needs at least one time with a value.");
    }
    return { primary, secondary, time: primaryX.time };
  }
  function sortedRows(xs) {
    const rows = xs.flatMap((t, i) => t === null ? [] : [i]);
    return rows.sort((a, b) => xs[a] - xs[b]);
  }
  function buildTimeSeriesOption(data, config, appearance = {}) {
    const neutral = appearance.neutral ?? SIGN_COLORS.neutral;
    const { primary, secondary, time } = samples(data, config);
    const points = config.points ?? true;
    const rawOpacity = config.raw_opacity ?? 0.5;
    const pair = (xs2, ys) => xs2.map((t, i) => [t, ys[i] ?? null]);
    const series = [];
    const rolling = config.roll_fn ?? "none";
    const rolledIndices = /* @__PURE__ */ new Set();
    for (const s of [...primary, ...secondary]) {
      const raw = {
        type: "line",
        name: s.name,
        data: pair(s.x, s.y),
        showSymbol: points,
        // Filled, where ECharts' default `emptyCircle` fills with the canvas
        // background.
        symbol: "circle",
        symbolSize: 5,
        lineStyle: { width: points ? 0 : 1, opacity: rawOpacity },
        itemStyle: { opacity: rawOpacity },
        yAxisIndex: s.axis
      };
      series.push(raw);
      if (rolling !== "none") {
        const rolled = rollingValues(s.y, config);
        if (rolled.some((v) => v !== null && !Number.isFinite(v))) {
          throw new Error("Rescale values to keep rolling summaries finite.");
        }
        rolledIndices.add(series.length);
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
      ...config.xlab ? { name: config.xlab } : {}
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
        // The renderer draws the labels in the series color.
        itemStyle: { color: neutral },
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
            { xAxis: xs[order2[run.start]], itemStyle: { color: neutral, opacity: 0.15 } },
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
        ...config.ylab ? { name: config.ylab } : {}
      }
    ];
    if (secondary.length > 0) {
      yAxis.push({
        type: "value",
        scale: true,
        position: "right",
        ...config.y2lab ? { name: config.y2lab } : {},
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
      tooltip: {
        trigger: "axis",
        formatter: (params) => axisTooltip(params, rolledIndices, rolling)
      },
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
  var escapeHtml3 = (text) => text.replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);
  function axisTooltip(params, rolledIndices, rolling) {
    const list = [params].flat();
    if (list.length === 0) return "";
    const rows = /* @__PURE__ */ new Map();
    for (const p of list) {
      const y = Array.isArray(p.value) ? p.value[1] : p.value;
      if (typeof y !== "number" || !Number.isFinite(y)) continue;
      const name = p.seriesName ?? "";
      const row = rows.get(name) ?? { marker: p.marker ?? "" };
      if (rolledIndices.has(p.seriesIndex ?? -1)) row.roll = y;
      else row.raw = y;
      rows.set(name, row);
    }
    const value = (v) => v === void 0 ? "\u2013" : String(tooltipValue(v));
    const lines = [...rows].map(([name, row]) => {
      const summary2 = row.roll === void 0 ? "" : ` <span style="opacity:0.7">${rolling} ${value(row.roll)}</span>`;
      return `${row.marker}${escapeHtml3(name)}&nbsp;&nbsp;<b>${value(row.raw)}</b>${summary2}`;
    });
    return [escapeHtml3(list[0]?.axisValueLabel ?? ""), ...lines].join("<br/>");
  }

  // src/charts/choropleth/locations.ts
  var STATE_ABBR_TO_FIPS = {
    AL: "01",
    AK: "02",
    AZ: "04",
    AR: "05",
    CA: "06",
    CO: "08",
    CT: "09",
    DE: "10",
    DC: "11",
    FL: "12",
    GA: "13",
    HI: "15",
    ID: "16",
    IL: "17",
    IN: "18",
    IA: "19",
    KS: "20",
    KY: "21",
    LA: "22",
    ME: "23",
    MD: "24",
    MA: "25",
    MI: "26",
    MN: "27",
    MS: "28",
    MO: "29",
    MT: "30",
    NE: "31",
    NV: "32",
    NH: "33",
    NJ: "34",
    NM: "35",
    NY: "36",
    NC: "37",
    ND: "38",
    OH: "39",
    OK: "40",
    OR: "41",
    PA: "42",
    RI: "44",
    SC: "45",
    SD: "46",
    TN: "47",
    TX: "48",
    UT: "49",
    VT: "50",
    VA: "51",
    WA: "53",
    WV: "54",
    WI: "55",
    WY: "56",
    PR: "72",
    VI: "78",
    GU: "66",
    AS: "60",
    MP: "69"
  };
  var STATE_NAME_TO_FIPS = {
    alabama: "01",
    alaska: "02",
    "american samoa": "60",
    arizona: "04",
    arkansas: "05",
    california: "06",
    colorado: "08",
    "commonwealth of the northern mariana islands": "69",
    connecticut: "09",
    delaware: "10",
    "district of columbia": "11",
    florida: "12",
    georgia: "13",
    guam: "66",
    hawaii: "15",
    idaho: "16",
    illinois: "17",
    indiana: "18",
    iowa: "19",
    kansas: "20",
    kentucky: "21",
    louisiana: "22",
    maine: "23",
    maryland: "24",
    massachusetts: "25",
    michigan: "26",
    minnesota: "27",
    mississippi: "28",
    missouri: "29",
    montana: "30",
    nebraska: "31",
    nevada: "32",
    "new hampshire": "33",
    "new jersey": "34",
    "new mexico": "35",
    "new york": "36",
    "north carolina": "37",
    "north dakota": "38",
    ohio: "39",
    oklahoma: "40",
    oregon: "41",
    pennsylvania: "42",
    "puerto rico": "72",
    "rhode island": "44",
    "south carolina": "45",
    "south dakota": "46",
    tennessee: "47",
    texas: "48",
    "united states virgin islands": "78",
    utah: "49",
    vermont: "50",
    virginia: "51",
    washington: "53",
    "west virginia": "54",
    wisconsin: "55",
    wyoming: "56"
  };
  var ISO2_TO_ISO3 = {
    AE: "ARE",
    AF: "AFG",
    AL: "ALB",
    AM: "ARM",
    AO: "AGO",
    AQ: "ATA",
    AR: "ARG",
    AT: "AUT",
    AU: "AUS",
    AZ: "AZE",
    BA: "BIH",
    BD: "BGD",
    BE: "BEL",
    BF: "BFA",
    BG: "BGR",
    BI: "BDI",
    BJ: "BEN",
    BN: "BRN",
    BO: "BOL",
    BR: "BRA",
    BS: "BHS",
    BT: "BTN",
    BW: "BWA",
    BY: "BLR",
    BZ: "BLZ",
    CA: "CAN",
    CD: "COD",
    CF: "CAF",
    CG: "COG",
    CH: "CHE",
    CI: "CIV",
    CL: "CHL",
    CM: "CMR",
    CN: "CHN",
    CO: "COL",
    CR: "CRI",
    CU: "CUB",
    CY: "CYP",
    CZ: "CZE",
    DE: "DEU",
    DJ: "DJI",
    DK: "DNK",
    DO: "DOM",
    DZ: "DZA",
    EC: "ECU",
    EE: "EST",
    EG: "EGY",
    EH: "ESH",
    ER: "ERI",
    ES: "ESP",
    ET: "ETH",
    FI: "FIN",
    FJ: "FJI",
    FK: "FLK",
    FR: "FRA",
    GA: "GAB",
    GB: "GBR",
    GE: "GEO",
    GH: "GHA",
    GL: "GRL",
    GM: "GMB",
    GN: "GIN",
    GQ: "GNQ",
    GR: "GRC",
    GT: "GTM",
    GW: "GNB",
    GY: "GUY",
    HN: "HND",
    HR: "HRV",
    HT: "HTI",
    HU: "HUN",
    ID: "IDN",
    IE: "IRL",
    IL: "ISR",
    IN: "IND",
    IQ: "IRQ",
    IR: "IRN",
    IS: "ISL",
    IT: "ITA",
    JM: "JAM",
    JO: "JOR",
    JP: "JPN",
    KE: "KEN",
    KG: "KGZ",
    KH: "KHM",
    KP: "PRK",
    KR: "KOR",
    KW: "KWT",
    KZ: "KAZ",
    LA: "LAO",
    LB: "LBN",
    LK: "LKA",
    LR: "LBR",
    LS: "LSO",
    LT: "LTU",
    LU: "LUX",
    LV: "LVA",
    LY: "LBY",
    MA: "MAR",
    MD: "MDA",
    ME: "MNE",
    MG: "MDG",
    MK: "MKD",
    ML: "MLI",
    MM: "MMR",
    MN: "MNG",
    MR: "MRT",
    MW: "MWI",
    MX: "MEX",
    MY: "MYS",
    MZ: "MOZ",
    NA: "NAM",
    NC: "NCL",
    NE: "NER",
    NG: "NGA",
    NI: "NIC",
    NL: "NLD",
    NO: "NOR",
    NP: "NPL",
    NZ: "NZL",
    OM: "OMN",
    PA: "PAN",
    PE: "PER",
    PG: "PNG",
    PH: "PHL",
    PK: "PAK",
    PL: "POL",
    PR: "PRI",
    PS: "PSE",
    PT: "PRT",
    PY: "PRY",
    QA: "QAT",
    RO: "ROU",
    RS: "SRB",
    RU: "RUS",
    RW: "RWA",
    SA: "SAU",
    SB: "SLB",
    SD: "SDN",
    SE: "SWE",
    SI: "SVN",
    SK: "SVK",
    SL: "SLE",
    SN: "SEN",
    SO: "SOM",
    SR: "SUR",
    SS: "SSD",
    SV: "SLV",
    SY: "SYR",
    SZ: "SWZ",
    TD: "TCD",
    TF: "ATF",
    TG: "TGO",
    TH: "THA",
    TJ: "TJK",
    TL: "TLS",
    TM: "TKM",
    TN: "TUN",
    TR: "TUR",
    TT: "TTO",
    TW: "TWN",
    TZ: "TZA",
    UA: "UKR",
    UG: "UGA",
    US: "USA",
    UY: "URY",
    UZ: "UZB",
    VE: "VEN",
    VN: "VNM",
    VU: "VUT",
    YE: "YEM",
    ZA: "ZAF",
    ZM: "ZMB",
    ZW: "ZWE"
  };
  var digitsOf = (s) => s.replace(/[^0-9]/g, "");
  function normalizeLocation(raw, resolution) {
    if (raw === null || raw === void 0) return null;
    const s = String(raw).trim();
    if (!s) return null;
    if (resolution === "county") {
      const d = digitsOf(s);
      return d && d.length <= 5 ? d.padStart(5, "0") : null;
    }
    if (resolution === "state") {
      const d = digitsOf(s);
      if (d) return d.length <= 2 ? d.padStart(2, "0") : null;
      return STATE_ABBR_TO_FIPS[s.toUpperCase()] ?? STATE_NAME_TO_FIPS[s.toLowerCase()] ?? null;
    }
    const up = s.toUpperCase();
    if (up.length === 3) return up;
    if (up.length === 2) return ISO2_TO_ISO3[up] ?? null;
    return null;
  }

  // src/charts/choropleth/config.ts
  var CHOROPLETH_COLORMAPS = Object.freeze([
    "blues",
    "viridis",
    "ylorrd",
    "greens",
    "magma",
    "rdbu",
    "rdylgn",
    "spectral",
    "brbg"
  ]);
  var CHOROPLETH_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "location",
    "value",
    "tooltip",
    "resolution",
    "aggregate",
    "classification",
    "num_classes",
    "colormap",
    "opacity",
    "show_boundaries",
    "outline_width",
    "show_legend",
    "legend_position",
    "tooltip_position",
    "value_label"
  ]);
  var CHOROPLETH_GEOMETRY = Object.freeze({
    country: { object: "countries", center: [0, 20], zoom: 0.4 },
    state: { object: "states", center: [-96, 38], zoom: 2.6 },
    county: { object: "counties", center: [-96, 38], zoom: 2.6 }
  });
  function mapFeatures(topology, resolution, feature) {
    const name = CHOROPLETH_GEOMETRY[resolution].object;
    const object = topology.objects[name];
    if (!object) throw new Error(`The map geometry has no object '${name}'.`);
    const collection = feature(topology, object);
    const ids = /* @__PURE__ */ new Set();
    for (const f of collection.features) {
      const id = f.id === void 0 || f.id === null ? "" : String(f.id);
      f.properties = { ...f.properties ?? {}, joinId: id };
      if (id) ids.add(id);
    }
    return { features: collection, ids };
  }
  var fieldText = (v) => typeof v === "number" ? String(formatNumber(v)) : v;
  function combine(values, aggregate) {
    if (aggregate === "sum") return values.reduce((a, b) => a + b, 0);
    if (aggregate === "mean") return values.reduce((a, b) => a + b, 0) / values.length;
    const sorted = [...values].sort((a, b) => a - b);
    const mid = Math.floor(sorted.length / 2);
    return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
  }
  function joinChoropleth(model, ids, aggregate = "none") {
    const resolution = model.resolution ?? "country";
    const fields = model.tooltipFields ?? [];
    const members = /* @__PURE__ */ new Map();
    const unmatchedKeys = [];
    let unmatched = 0;
    model.rows.forEach((row, i) => {
      const id = normalizeLocation(row.location, resolution);
      if (!id || !ids.has(id)) {
        unmatched++;
        if (unmatchedKeys.length < 12) unmatchedKeys.push(row.location);
        return;
      }
      const rows = members.get(id);
      if (!rows) {
        members.set(id, [i]);
        return;
      }
      if (aggregate === "none") {
        const first = rows[0];
        throw new Error(
          `A region takes one value; rows ${first + 1} and ${i + 1} both name region '${id}' ('${model.rows[first]?.location}', '${row.location}'). Set \`aggregate\` to sum, mean, or median to combine them.`
        );
      }
      rows.push(i);
    });
    const values = /* @__PURE__ */ new Map();
    const extras = /* @__PURE__ */ new Map();
    const counts = /* @__PURE__ */ new Map();
    let matched = 0;
    for (const [id, indices] of members) {
      const rows = indices.map((i) => model.rows[i]);
      matched += rows.length;
      counts.set(id, rows.length);
      const finite2 = rows.map((r) => r.value).filter((v) => typeof v === "number" && Number.isFinite(v));
      if (finite2.length > 0) {
        values.set(id, aggregate === "none" ? finite2[0] : combine(finite2, aggregate));
      }
      const lines = [];
      for (const f of fields) {
        const shown = new Set(rows.map((r) => r.extras?.[f] ?? null));
        const [v] = shown;
        if (shown.size === 1 && v !== null && v !== void 0) {
          lines.push({ label: f, value: fieldText(v) });
        }
      }
      if (rows.length > 1) lines.push({ label: "Rows", value: String(rows.length) });
      if (lines.length > 0) extras.set(id, lines);
    }
    return { values, extras, rows: counts, matched, unmatched, unmatchedKeys };
  }
  var INTERPOLATOR = {
    blues: "interpolateBlues",
    viridis: "interpolateViridis",
    ylorrd: "interpolateYlOrRd",
    greens: "interpolateGreens",
    magma: "interpolateMagma",
    rdbu: "interpolateRdBu",
    rdylgn: "interpolateRdYlGn",
    spectral: "interpolateSpectral",
    brbg: "interpolateBrBG"
  };
  var DIVERGING = /* @__PURE__ */ new Set(["rdbu", "rdylgn", "spectral", "brbg"]);
  var CHOROPLETH_COLORS = Object.freeze({
    missing: Object.freeze({ light: "#e4e4e7", dark: "#3f3f46" }),
    outline: Object.freeze({ light: "#cbd5e1", dark: "#3f3f46" })
  });
  function rampColors(interp, n, dark, diverging) {
    const t0 = diverging ? 0 : dark ? 0.3 : 0.12;
    const t1 = diverging ? 1 : dark ? 1 : 0.95;
    return Array.from({ length: n }, (_, i) => interp(t0 + (t1 - t0) * i / (n - 1)));
  }
  function quantileBreaks(sorted, classes) {
    const breaks = [];
    for (let i = 1; i < classes; i++) {
      const q = (sorted.length - 1) * (i / classes);
      const lo = sorted[Math.floor(q)];
      const hi = sorted[Math.ceil(q)];
      breaks.push(lo + (hi - lo) * (q - Math.floor(q)));
    }
    return breaks;
  }
  function equalBreaks(min, max, classes) {
    const step = (max - min) / classes;
    return Array.from({ length: classes - 1 }, (_, i) => min + step * (i + 1));
  }
  var JENKS_CAP = 1500;
  function jenksBreaks(values, classes) {
    let data = values;
    if (data.length > JENKS_CAP) {
      const step = data.length / JENKS_CAP;
      data = Array.from({ length: JENKS_CAP }, (_, i) => values[Math.floor(i * step)]);
    }
    const n = data.length;
    if (n <= classes) return quantileBreaks(values, classes);
    const lower = Array.from({ length: n + 1 }, () => new Array(classes + 1).fill(0));
    const cost = Array.from({ length: n + 1 }, () => new Array(classes + 1).fill(0));
    for (let j = 1; j <= classes; j++) {
      lower[1][j] = 1;
      for (let i = 2; i <= n; i++) cost[i][j] = Number.POSITIVE_INFINITY;
    }
    for (let l = 2; l <= n; l++) {
      const lowerL = lower[l];
      const costL = cost[l];
      let s1 = 0;
      let s2 = 0;
      for (let m = 1; m <= l; m++) {
        const i3 = l - m + 1;
        const val = data[i3 - 1];
        s1 += val;
        s2 += val * val;
        const variance = s2 - s1 * s1 / m;
        const i4 = i3 - 1;
        if (i4 === 0) continue;
        const costI4 = cost[i4];
        for (let j = 2; j <= classes; j++) {
          const candidate = variance + costI4[j - 1];
          if (costL[j] >= candidate) {
            lowerL[j] = i3;
            costL[j] = candidate;
          }
        }
      }
      lowerL[1] = 1;
      costL[1] = s2 - s1 * s1 / l;
    }
    const breaks = [];
    let k = n;
    for (let j = classes; j >= 2 && k >= 1; j--) {
      const start = lower[k][j] - 1;
      breaks.push(data[start]);
      k = start;
    }
    return breaks.reverse();
  }
  var boundText = (v) => String(formatNumber(v));
  function buildChoroplethScale(values, config, appearance, interpolators) {
    const dark = appearance.scheme === "dark";
    const missingColor = CHOROPLETH_COLORS.missing[dark ? "dark" : "light"];
    const colormap = config.colormap ?? "blues";
    const interp = interpolators[INTERPOLATOR[colormap] ?? "interpolateBlues"];
    const classes = Math.max(2, Math.min(12, Math.round(config.num_classes ?? 5)));
    const colors = rampColors(interp, classes, dark, DIVERGING.has(colormap));
    const sorted = [...values].filter((v) => typeof v === "number" && Number.isFinite(v)).sort((a, b) => a - b);
    if (sorted.length === 0) {
      return { thresholds: [], colors: colors.slice(0, 1), missingColor, min: 0, max: 0, legend: [] };
    }
    const min = sorted[0];
    const max = sorted[sorted.length - 1];
    const method = config.classification ?? "quantile";
    const raw = min === max ? [] : method === "equal" ? equalBreaks(min, max, classes) : method === "jenks" ? jenksBreaks(sorted, classes) : quantileBreaks(sorted, classes);
    const thresholds = raw.filter((t, i, a) => i === 0 || t > a[i - 1]).filter((t) => t > min && t < max);
    const bounds = [min, ...thresholds, max];
    const used = colors.slice(0, thresholds.length + 1);
    return {
      thresholds,
      colors: used,
      missingColor,
      min,
      max,
      legend: used.map((color, i) => ({
        color,
        label: `${boundText(bounds[i])} \u2013 ${boundText(bounds[i + 1])}`
      }))
    };
  }
  function choroplethColor(scale, value) {
    if (typeof value !== "number" || !Number.isFinite(value)) return scale.missingColor;
    let i = 0;
    while (i < scale.thresholds.length && value >= scale.thresholds[i]) i++;
    return scale.colors[Math.min(i, scale.colors.length - 1)] ?? scale.missingColor;
  }
  function choroplethFillExpression(scale) {
    const matched = scale.thresholds.length === 0 ? scale.colors[0] ?? scale.missingColor : [
      "step",
      ["to-number", ["feature-state", "v"]],
      scale.colors[0],
      ...scale.thresholds.flatMap((t, i) => [t, scale.colors[i + 1]])
    ];
    return ["case", ["!=", ["feature-state", "v"], null], matched, scale.missingColor];
  }

  // src/charts/network/config.ts
  var NETWORK_CHART_CONFIG_MEMBERS = Object.freeze([
    "type",
    "title",
    "directed",
    "threshold",
    "self_loops",
    "layout",
    "seed",
    "cluster_resolution",
    "scale_by_degree",
    "color_by_group",
    "blend_edges",
    "show_labels",
    "node_size",
    "edge_scale",
    "node_opacity",
    "edge_opacity",
    "palette",
    "node_color",
    "positive_color",
    "negative_color"
  ]);
  function seededRandom(start) {
    let seed = Math.trunc(start) >>> 0;
    return () => {
      seed = Math.imul(1664525, seed) + 1013904223 >>> 0;
      return seed / 4294967296;
    };
  }
  var absWeight = (e) => Math.abs(e.weight ?? 1);
  function buildNetworkGraph(model, config, libraries) {
    const { Graph, louvain, forceAtlas2, circlepack, random } = libraries;
    const rng = seededRandom(config.seed ?? 1);
    const graph = new Graph({ type: model.directed ? "directed" : "undirected" });
    const nodes = model.nodes;
    const maxValue = nodes.reduce((m, n) => Math.max(m, n.value ?? 0), 1);
    nodes.forEach((node, i) => {
      const angle = 2 * Math.PI * i / nodes.length;
      graph.addNode(node.id, {
        label: node.label ?? node.id,
        x: Math.cos(angle),
        y: Math.sin(angle),
        value: node.value ?? 0,
        valueNorm: (node.value ?? 0) / maxValue,
        group: node.group ?? null,
        community: 0
      });
    });
    const kept = [];
    const pairs = /* @__PURE__ */ new Set();
    for (const e of model.edges) {
      if (!graph.hasNode(e.source) || !graph.hasNode(e.target)) continue;
      const pair = JSON.stringify(
        model.directed || e.source <= e.target ? [e.source, e.target] : [e.target, e.source]
      );
      if (pairs.has(pair)) continue;
      pairs.add(pair);
      kept.push(e);
    }
    const maxWeight = kept.reduce((m, e) => Math.max(m, absWeight(e)), 1e-9);
    for (const e of kept) {
      const sign = e.sign ?? 0;
      const weightNorm = absWeight(e) / maxWeight;
      graph.addEdgeWithKey(String(graph.size), e.source, e.target, {
        weightNorm,
        sign,
        louvainWeight: sign < 0 ? 0 : weightNorm
      });
    }
    if (graph.size > 0) {
      louvain.assign(graph, {
        nodeCommunityAttribute: "community",
        getEdgeWeight: "louvainWeight",
        resolution: config.cluster_resolution ?? 1,
        rng
      });
    }
    const layout2 = config.layout ?? "force";
    if (layout2 === "circular") {
      const community = (id) => nodeAttributes(graph, id).community;
      const ids = graph.nodes().sort((a, b) => community(a) - community(b));
      ids.forEach((id, i) => {
        const angle = 2 * Math.PI * i / ids.length;
        graph.setNodeAttribute(id, "x", Math.cos(angle));
        graph.setNodeAttribute(id, "y", Math.sin(angle));
      });
    } else if (layout2 === "circlepack") {
      circlepack.assign(graph, { hierarchyAttributes: ["community"], rng });
    } else if (layout2 === "random") {
      random.assign(graph, { rng });
    } else if (graph.order > 0) {
      for (const id of graph.nodes()) {
        graph.setNodeAttribute(id, "x", 2 * rng() - 1);
        graph.setNodeAttribute(id, "y", 2 * rng() - 1);
      }
      forceAtlas2.assign(graph, {
        iterations: 200,
        settings: { ...forceAtlas2.inferSettings(graph), scalingRatio: 10 }
      });
    }
    return graph;
  }
  var nodeAttributes = (graph, id) => graph.getNodeAttributes(id);
  var edgeAttributes = (graph, id) => graph.getEdgeAttributes(id);
  var FADED_ALPHA = 77;
  var NEUTRAL_EDGE_COLOR = "#9aa0a6";
  var HEX6 = /^#[0-9a-f]{6}$/i;
  function withAlpha3(color, alpha) {
    if (!HEX6.test(color)) return color;
    const a = Math.max(0, Math.min(255, Math.round(alpha)));
    return `${color}${a.toString(16).padStart(2, "0")}`;
  }
  function blendHex(a, b) {
    if (!HEX6.test(a) || !HEX6.test(b)) return a;
    const pa = Number.parseInt(a.slice(1), 16);
    const pb = Number.parseInt(b.slice(1), 16);
    const r = (pa >> 16) + (pb >> 16) >> 1;
    const g = (pa >> 8 & 255) + (pb >> 8 & 255) >> 1;
    const bl = (pa & 255) + (pb & 255) >> 1;
    return `#${(1 << 24 | r << 16 | g << 8 | bl).toString(16).slice(1)}`;
  }
  function networkReducers(graph, config, appearance = {}, hovered = () => null) {
    const memberPalette = asArray(config.palette);
    const palette = memberPalette.length > 0 ? memberPalette : appearance.palette && appearance.palette.length > 0 ? appearance.palette : RTEMIS_PALETTE;
    const nodeColor = config.node_color ?? palette[0];
    const byGroup = config.color_by_group ?? false;
    const colorOf = (community) => byGroup ? palette[(community % palette.length + palette.length) % palette.length] : nodeColor;
    const positive = config.positive_color ?? appearance.positive_color ?? SIGN_COLORS.positive;
    const negative = config.negative_color ?? appearance.negative_color ?? SIGN_COLORS.negative;
    const dark = appearance.scheme === "dark";
    const labelColor = appearance.label_color ?? (dark ? "#e6e6e6" : "#1a1a1a");
    const nodeSize = config.node_size ?? 10;
    const byDegree = config.scale_by_degree ?? true;
    const nodeOpacity = config.node_opacity ?? 0.95;
    const edgeScale = config.edge_scale ?? 3;
    const edgeOpacity = config.edge_opacity ?? 0.4;
    let minX = Number.POSITIVE_INFINITY;
    let maxX = Number.NEGATIVE_INFINITY;
    for (const id of graph.nodes()) {
      const x = nodeAttributes(graph, id).x;
      minX = Math.min(minX, x);
      maxX = Math.max(maxX, x);
    }
    const centerX = graph.order > 0 ? (minX + maxX) / 2 : 0;
    return {
      nodeReducer(node, data) {
        const attrs = nodeAttributes(graph, node);
        const out = {
          ...data,
          size: byDegree ? Math.max(1, nodeSize * (0.5 + (attrs.valueNorm || 0))) : nodeSize,
          color: colorOf(attrs.community || 0),
          opacity: nodeOpacity,
          labelPosition: attrs.x > centerX ? "left" : "right",
          labelColor,
          ...appearance.font_family ? { labelFont: appearance.font_family } : {},
          backdropVisibility: "hidden"
        };
        const over = hovered();
        if (over === node) {
          out.backdropVisibility = "visible";
          out.backdropColor = dark ? "#505050" : "#cbcbcb";
          out.backdropShadowColor = "transparent";
          out.backdropShadowBlur = 0;
        } else if (over && !graph.areNeighbors(over, node)) {
          out.color = withAlpha3(out.color, FADED_ALPHA);
          out.opacity = FADED_ALPHA / 255;
          out.label = "";
        }
        return out;
      },
      edgeReducer(edge, data) {
        const attrs = edgeAttributes(graph, edge);
        const [source, target] = graph.extremities(edge);
        const color = config.blend_edges ? blendHex(
          colorOf(nodeAttributes(graph, source).community),
          colorOf(nodeAttributes(graph, target).community)
        ) : attrs.sign > 0 ? positive : attrs.sign < 0 ? negative : NEUTRAL_EDGE_COLOR;
        const over = hovered();
        return {
          ...data,
          size: Math.max(0.5, (attrs.weightNorm ?? 0.5) * edgeScale),
          color,
          opacity: over && source !== over && target !== over ? Math.min(edgeOpacity, 0.06) : edgeOpacity
        };
      }
    };
  }

  // src/layout/aspect.ts
  function fitAspectBox(option, aspect, width, height) {
    const grid = option.grid;
    if (!grid || Array.isArray(grid)) {
      throw new Error("A fixed-aspect panel requires one plotting grid.");
    }
    const a = aspect;
    if (![a.ratio, a.leftPx, a.rightPx, a.topPx, a.botPx].every(Number.isFinite) || a.ratio <= 0 || [a.leftPx, a.rightPx, a.topPx, a.botPx].some((v) => v < 0)) {
      throw new Error("Supply a positive aspect ratio and nonnegative pixel margins.");
    }
    const available = Math.min(width - a.leftPx - a.rightPx, (height - a.topPx - a.botPx) / a.ratio);
    const preferred = a.widthPx === void 0 || a.widthPx === null ? available : a.widthPx;
    if (!Number.isFinite(preferred) || preferred <= 0 || available <= 0) {
      throw new Error("Increase panel dimensions to leave room for the plotting grid and margins.");
    }
    const box = grid;
    box.width = Math.min(available, preferred);
    box.height = box.width * a.ratio;
    box.left = a.leftPx + (width - a.leftPx - a.rightPx - box.width) / 2;
    box.top = a.topPx + (height - a.topPx - a.botPx - box.height) / 2;
  }
  function fitAspectAxes(chart, aspect) {
    const rect = chart.getModel().getComponent("grid").coordinateSystem.getRect();
    const width = Math.min(rect.width, rect.height / aspect.ratio);
    const height = width * aspect.ratio;
    chart.setOption({
      grid: {
        left: rect.x + (rect.width - width) / 2,
        top: rect.y + (rect.height - height) / 2,
        width,
        height,
        containLabel: false,
        outerBoundsMode: "none"
      }
    });
  }

  // src/layout/axes.ts
  function fitEndpointLabels(chart, context) {
    const source = context?.option;
    if (typeof chart?.getModel !== "function" || !source) return;
    const update = {};
    for (const type of ["xAxis", "yAxis"]) {
      const defined = [].concat(source[type] ?? []);
      const axes = [];
      let changed = false;
      chart.getModel().eachComponent(type, (model) => {
        const label = defined[model.componentIndex]?.axisLabel || {};
        const scale = model.axis?.scale;
        const next = {};
        const interval = scale?.getConfig?.()?.interval ?? scale?.getInterval?.();
        if (model.axis?.type === "value" && Number.isFinite(interval)) {
          const extent2 = scale.getExtent();
          const onTick = (v) => interval > 0 && Math.abs(v / interval - Math.round(v / interval)) < 1e-6;
          if (label.showMinLabel === void 0) next.showMinLabel = onTick(extent2[0]);
          if (label.showMaxLabel === void 0) next.showMaxLabel = onTick(extent2[1]);
          const current = model.get("axisLabel");
          if (Object.keys(next).some((k) => current?.[k] !== next[k])) changed = true;
        }
        axes[model.componentIndex] = Object.keys(next).length ? { axisLabel: next } : {};
      });
      if (changed) update[type] = axes;
    }
    if (Object.keys(update).length) chart.setOption(update);
  }

  // src/layout/legend.ts
  var LEGEND_TITLE_ID = "rtemis-legend-title";
  function measureLegendTitle(echarts, chart, model, text, maxWidth = Number.POSITIVE_INFINITY) {
    if (!text || !echarts) return null;
    const own = model.getModel("textStyle");
    const global = chart.getModel().getModel("textStyle");
    const fontSize = own.get("fontSize") || global.getShallow("fontSize") || 12;
    const fontFamily = own.get("fontFamily") || global.getShallow("fontFamily") || "sans-serif";
    const fill = own.get("color") || global.getShallow("color") || "#333";
    const font = `bold ${fontSize}px ${fontFamily}`;
    const regularFont = `${fontSize}px ${fontFamily}`;
    const measure = (t) => {
      const width = echarts.format.getTextRect(t, font).width;
      return width === echarts.format.getTextRect(t, regularFont).width ? width * 1.1 : width;
    };
    const lines = [];
    let current = "";
    for (const word of String(text).split(/\s+/)) {
      const next = current ? `${current} ${word}` : word;
      if (current && measure(next) > maxWidth) {
        lines.push(current);
        current = word;
      } else {
        current = next;
      }
    }
    lines.push(current);
    const lineHeight = Math.ceil(echarts.format.getTextRect("Mg", font).height);
    const line = Math.max(
      model.get("itemHeight") || 14,
      echarts.format.getTextRect("Mg", regularFont).height
    );
    const padding = [].concat(model.get("padding") ?? 5);
    return {
      text: lines.join("\n"),
      fontSize,
      fontFamily,
      fill,
      line,
      lineHeight,
      width: Math.ceil(Math.max(...lines.map(measure))),
      height: lines.length * lineHeight,
      padTop: padding[0],
      padLeft: padding.length === 4 ? padding[3] : padding[padding.length > 1 ? 1 : 0]
    };
  }
  function drawLegendTitle(chart, title, left, top) {
    chart.setOption({
      graphic: {
        elements: [
          {
            id: LEGEND_TITLE_ID,
            type: "text",
            left,
            top,
            silent: true,
            z: 100,
            style: {
              text: title.text,
              fontSize: title.fontSize,
              fontFamily: title.fontFamily,
              fontWeight: "bold",
              fill: title.fill,
              lineHeight: title.lineHeight
            }
          }
        ]
      }
    });
  }
  function layoutLegend(echarts, chart, context) {
    const position = context.position;
    if (!position || context.target === "visualMap") return;
    const inside = (context.placement || "inside") === "inside";
    const vertical = position === "left" || position === "right";
    const lower = position.startsWith("bottom");
    const right = position.endsWith("right");
    const left = position.endsWith("left");
    const width = chart.getWidth();
    const height = chart.getHeight();
    const gap = 12;
    const source = context.option;
    let models = [];
    chart.getModel().eachComponent("legend", (model) => {
      if (model.get("show") && model.getData().length) models.push(model);
    });
    if (!models.length) return;
    const bounds = (model) => {
      const group = chart.getViewOfComponentModel(model).group;
      const box = group.getBoundingRect().clone();
      box.applyTransform(group.getComputedTransform());
      return box;
    };
    const px = (value, extent2, fallback) => typeof value === "number" ? value : typeof value === "string" && value.endsWith("%") ? Number.parseFloat(value) * extent2 / 100 : fallback;
    let header = gap;
    for (const type of ["title", "toolbox"]) {
      chart.getModel().eachComponent(type, (model) => {
        const box = bounds(model);
        if (box.width && box.height && box.y < height / 2) {
          header = Math.max(header, box.y + box.height + gap);
        }
      });
    }
    let grids = source.grid ? (Array.isArray(source.grid) ? source.grid : [source.grid]).map((g) => ({
      ...g
    })) : [];
    const aspect = context.aspect;
    if (aspect) {
      grids = [
        {
          ...grids[0],
          left: aspect.leftPx,
          right: aspect.rightPx,
          top: aspect.topPx,
          bottom: aspect.botPx,
          width: null,
          height: null
        }
      ];
    }
    const series = Array.isArray(source.series) ? source.series : [source.series];
    const isGantt = series.some((s) => s?.renderItem === "rtemis.gantt.v1");
    if (isGantt) {
      grids = grids.map((g) => ({ ...g, right: 16, top: Math.max(g.top, header) }));
    }
    if (grids.length) chart.setOption({ grid: grids });
    const area = () => {
      const model = chart.getModel().getComponent("grid");
      return model ? model.coordinateSystem.getRect() : { x: gap, y: header, width: width - 2 * gap, height: height - header - gap };
    };
    const initial = area();
    const availableWidth = vertical ? Math.max(60, Math.min(initial.width * 0.4, 240)) : Math.max(1, initial.width - (inside ? 2 * gap : 0));
    let title = measureLegendTitle(echarts, chart, models[0], context.title);
    const inline = !!title && !vertical && !context.titleAbove && title.width + 10 <= availableWidth / 2;
    if (title && !inline) {
      title = measureLegendTitle(
        echarts,
        chart,
        models[0],
        context.title,
        availableWidth - 2 * title.padLeft
      );
    }
    const indent = inline && title ? title.width + 10 : 0;
    const lead = title && !inline ? title.height + 2 : 0;
    const itemsWidth = availableWidth - indent;
    const availableHeight = Math.max(1, initial.height - (inside ? 2 * gap : 0) - lead);
    const legends = models.map((model) => {
      const font = model.getModel("textStyle").getFont();
      const natural = Math.max(
        0,
        ...model.getData().map((item) => echarts.format.getTextRect(item.get("name"), font).width)
      );
      const textWidth2 = Math.max(1, itemsWidth - model.get("itemWidth") - 15);
      return {
        id: model.id,
        orient: vertical ? "vertical" : "horizontal",
        left: 0,
        right: null,
        top: 0,
        bottom: null,
        width: itemsWidth,
        height: vertical ? availableHeight : 1e6,
        textStyle: { width: natural > textWidth2 ? textWidth2 : null, overflow: "break" }
      };
    });
    chart.setOption({ legend: legends });
    models = models.map((model) => chart.getModel().getComponent("legend", model.componentIndex));
    const boxes = models.map(bounds);
    const rows = [];
    boxes.forEach((box, index) => {
      let row = rows[rows.length - 1];
      if (!row || vertical || row.width + 16 + box.width > itemsWidth) {
        row = { items: [], width: 0, height: 0 };
        rows.push(row);
      }
      row.items.push({ index, offset: row.width + (row.items.length ? 16 : 0) });
      row.width += box.width + (row.items.length > 1 ? 16 : 0);
      row.height = Math.max(row.height, box.height);
    });
    const itemsHeight = rows.reduce((sum, row) => sum + row.height, 0) + (rows.length - 1) * 6;
    const legendHeight = itemsHeight + lead;
    const legendWidth = Math.max(
      indent + Math.max(...rows.map((row) => row.width)),
      title && !inline ? title.width + 2 * title.padLeft : 0
    );
    const extra = vertical ? legendWidth + gap : legendHeight + gap;
    if (!inside) {
      grids = grids.map((g) => {
        const next = { ...g };
        if (vertical) {
          const side = right ? "right" : "left";
          next[side] = px(g[side], width, 36) + extra;
        } else if (lower) {
          next.bottom = px(g.bottom, height, 36) + extra;
        } else {
          next.top = Math.max(px(g.top, height, 36), header + extra);
        }
        return next;
      });
      const update = {};
      if (grids.length) {
        update.grid = grids;
      } else {
        update.series = series.map((s) => {
          if (s?.type !== "pie") return {};
          return {
            left: gap + (vertical && left ? extra : 0),
            right: gap + (vertical && right ? extra : 0),
            top: header + (!vertical && !lower ? extra : 0),
            bottom: gap + (!vertical && lower ? extra : 0)
          };
        });
      }
      const zooms = source.dataZoom ? Array.isArray(source.dataZoom) ? source.dataZoom : [source.dataZoom] : null;
      if (!vertical && lower && zooms) {
        update.dataZoom = zooms.map(
          (z) => z.type === "slider" ? { bottom: px(z.bottom, height, 0) + extra } : {}
        );
      } else if (zooms) {
        update.dataZoom = zooms.map((z) => z.type === "slider" ? { bottom: z.bottom ?? null } : {});
      }
      chart.setOption(update);
    }
    if (aspect) fitAspectAxes(chart, aspect);
    const grid = area();
    let top = inside ? lower ? grid.y + grid.height - legendHeight - gap : vertical ? grid.y + (grid.height - legendHeight) / 2 : grid.y + gap : vertical ? grid.y + (grid.height - legendHeight) / 2 : lower ? height - gap - legendHeight : header;
    const rowX = (w) => vertical ? inside ? right ? grid.x + grid.width - w - gap : grid.x + gap : right ? width - gap - w : gap : right ? grid.x + grid.width - w - (inside ? gap : 0) : left ? grid.x + (inside ? gap : 0) : grid.x + (grid.width - w) / 2;
    const blockX = rowX(legendWidth);
    if (title) {
      drawLegendTitle(
        chart,
        title,
        blockX + title.padLeft,
        top + title.padTop + (inline ? (title.line - title.height) / 2 : 0)
      );
    }
    top += lead;
    const updates = [];
    for (const row of rows) {
      const x = title ? blockX + indent : rowX(row.width);
      for (const item of row.items) {
        updates.push({
          id: models[item.index].id,
          left: x + item.offset,
          right: null,
          top,
          bottom: null
        });
      }
      top += row.height + 6;
    }
    chart.setOption({ legend: updates });
  }

  // src/layout/scales.ts
  function layoutColorScale(echarts, chart, context) {
    const position = context.position;
    const model = chart.getModel().getComponent("visualMap");
    if (!position || !model?.get("show")) return;
    const source = context.option;
    const hints = context.heatmap ?? {};
    const gap = 12;
    const width = chart.getWidth();
    const height = chart.getHeight();
    const vertical = position === "left" || position === "right";
    const inside = context.placement === "inside";
    const right = position.endsWith("right");
    const left = position.endsWith("left");
    const lower = position.startsWith("bottom");
    const bounds = (component) => {
      const group = chart.getViewOfComponentModel(component).group;
      const box2 = group.getBoundingRect().clone();
      box2.applyTransform(group.getComputedTransform());
      return box2;
    };
    const px = (v, extent2) => typeof v === "string" && v.endsWith("%") ? Number.parseFloat(v) * extent2 / 100 : +v || 0;
    const grids = (Array.isArray(source.grid) ? source.grid : [source.grid]).map(
      (g) => ({ ...g })
    );
    const reserved2 = hints.legendReserveRight || 0;
    for (const g of grids) {
      if (g.right != null) g.right = px(g.right, width) - reserved2;
    }
    chart.setOption({
      grid: grids,
      visualMap: {
        orient: vertical ? "vertical" : "horizontal",
        left: 0,
        top: 0,
        right: null,
        bottom: null,
        itemHeight: Math.min(140, Math.max(30, (vertical ? height : width) - 160))
      }
    });
    const native = bounds(model);
    let barX = native.x;
    chart.getViewOfComponentModel(model).group.traverse((el) => {
      if (el.isGroup || !el.style?.fill?.colorStops) return;
      const r = el.getBoundingRect().clone();
      r.applyTransform(el.getComputedTransform());
      barX = Math.min(barX === native.x ? r.x : barX, r.x);
    });
    let title = measureLegendTitle(echarts, chart, model, context.title);
    const inline = !!title && !vertical && title.width + 10 + native.width <= width - 2 * gap;
    if (title && !inline) {
      title = measureLegendTitle(
        echarts,
        chart,
        model,
        context.title,
        vertical ? Math.max(native.width, Math.min(160, width / 4)) - 2 * title.padLeft : width - 2 * gap
      );
    }
    const indent = inline && title ? title.width + 10 : 0;
    const lead = title && !inline ? title.height + 2 : 0;
    const box = {
      width: Math.max(native.width + indent, lead && title ? title.width + 2 * title.padLeft : 0),
      height: Math.max(native.height + lead, title ? title.height : 0)
    };
    const side = vertical ? right ? "right" : "left" : lower ? "bottom" : "top";
    let header = gap;
    chart.getModel().eachComponent("title", (component) => {
      const t = bounds(component);
      if (t.width && t.height) header = Math.max(header, t.y + t.height + gap);
    });
    const firstTop = Math.min(...grids.map((g) => px(g.top, height)));
    const extra = inside ? 0 : side === "top" ? Math.max(0, header + box.height + gap - firstTop) : (vertical ? box.width : box.height) + gap;
    const adjusted = { ...hints, rightPx: (hints.rightPx ?? Number.NaN) - reserved2 };
    for (const g of grids) {
      if (g[side] != null) g[side] = px(g[side], vertical ? width : height) + extra;
    }
    const key = { top: "topPx", bottom: "botPx", left: "leftPx", right: "rightPx" }[side];
    if (Number.isFinite(adjusted[key])) adjusted[key] += extra;
    chart.setOption({ grid: grids });
    if (hints.squareCells) {
      const cell = Math.min(
        (width - adjusted.leftPx - adjusted.rightPx) / adjusted.nCols,
        (height - adjusted.topPx - adjusted.botPx) / adjusted.nRows
      );
      if (!(cell > 0)) {
        throw new Error("Increase figure dimensions to leave room for heatmap cells and margins.");
      }
      fitHeatmap(chart, { ...source, grid: grids }, adjusted);
    }
    const areas = [];
    model.eachTargetSeries((series) => {
      if (series.coordinateSystem?.type === "cartesian2d") {
        areas.push(series.coordinateSystem.getArea());
      }
    });
    const grid = areas[0];
    if (!grid) return;
    const dx = hints.squareCells ? Math.max(0, (width - adjusted.leftPx - adjusted.rightPx - grid.width) / 2) : 0;
    let plotRight = grid.x + grid.width;
    chart.getModel().eachComponent("grid", (g) => {
      const r = g.coordinateSystem?.getRect?.();
      if (r && r.width > 0) plotRight = Math.max(plotRight, r.x + r.width);
    });
    const x = vertical ? inside ? right ? grid.x + grid.width - box.width - gap : grid.x + gap : right ? plotRight + KEY_GAP - (barX - native.x) - (title ? 0 : native.x) : gap + dx : left ? grid.x + (inside ? gap : 0) : right ? grid.x + grid.width - box.width - (inside ? gap : 0) : grid.x + (grid.width - box.width) / 2;
    const y = vertical ? grid.y + (grid.height - box.height) / 2 : inside ? lower ? grid.y + grid.height - box.height - gap : grid.y + gap : lower ? height - gap - box.height : header;
    if (title) {
      drawLegendTitle(
        chart,
        title,
        x + title.padLeft,
        y + (inline ? (native.height - title.height) / 2 : title.padTop)
      );
    }
    const shiftX = title ? indent - native.x : 0;
    const shiftY = title ? lead - native.y : 0;
    chart.setOption({
      visualMap: { left: x + shiftX, right: null, top: y + shiftY, bottom: null }
    });
  }
  function centerColorScales(chart, context) {
    const definitions = context.option.visualMap;
    if (!definitions) return;
    const maps = Array.isArray(definitions) ? definitions : [definitions];
    let changed = false;
    const updates = maps.map((definition, index) => {
      if (!["middle", "center"].includes(definition.top)) return {};
      const model = chart.getModel().getComponent("visualMap", index);
      if (!model?.get("show") || model.get("orient") !== "vertical") return {};
      const areas = [];
      model.eachTargetSeries((series) => {
        if (series.subType === "heatmap" && series.coordinateSystem?.type === "cartesian2d") {
          areas.push(series.coordinateSystem.getArea());
        }
      });
      if (!areas.length) return {};
      const top = Math.min(...areas.map((area) => area.y));
      const bottom = Math.max(...areas.map((area) => area.y + area.height));
      const view = chart.getViewOfComponentModel(model);
      const height = view.group.getBoundingRect().height;
      changed = true;
      return { top: (top + bottom - height) / 2 };
    });
    if (changed) chart.setOption({ visualMap: updates });
  }

  // src/layout/fit.ts
  var FIT_EVENTS = Object.freeze([
    "legendselectchanged",
    "legendscroll",
    "datazoom",
    "restore"
  ]);
  var OWN_LAYOUT = /* @__PURE__ */ new Set(["a3"]);
  function fit(echarts, chart, context) {
    if (context.members) return fitWithMembers(echarts, chart, context, context.members);
    const { option, type } = context;
    const a3 = context.hints?.a3;
    if (a3) {
      fitA3(chart, { option, hints: a3, bounded: context.bounded ?? true });
    } else if (!(type && OWN_LAYOUT.has(type))) {
      const heatmap = context.hints?.heatmap;
      if (heatmap) fitHeatmap(chart, option, heatmap);
      if (type === "scatter") {
        fitKeys(echarts, chart, {
          option,
          ...context.placement ? { placement: context.placement } : {}
        });
      } else {
        placeColorScale(chart, { option });
      }
      fitLegend(chart, { option });
    }
    return { state: fitState(chart) };
  }
  function fitWithMembers(echarts, chart, context, members) {
    const instance = chart;
    const { option, type } = context;
    const hints = context.hints ?? {};
    const bounded = context.bounded ?? true;
    fitGantt(echarts, instance, { option, legendPosition: members.position ?? null });
    let height;
    if (hints.a3) {
      fitA3(instance, { option, hints: hints.a3, bounded });
      if (!bounded && hints.a3.autoHeight) height = instance.getHeight();
    }
    if (hints.aspect) fitAspectAxes(instance, hints.aspect);
    if (hints.heatmap?.squareCells) fitSquareCells(instance, option, hints.heatmap);
    fitSankey(echarts, instance, { option });
    fitEndpointLabels(instance, { option });
    layoutLegend(echarts, instance, {
      option,
      position: members.position ?? null,
      placement: members.placement ?? null,
      title: members.title ?? null,
      titleAbove: members.titleAbove ?? false,
      target: members.target ?? null,
      aspect: hints.aspect ?? null
    });
    fitPieLabels(echarts, instance, { option });
    if (type === "scatter") {
      let hasLegend = false;
      instance.getModel().eachComponent("legend", (model) => {
        if (model.get("show") && model.getData().length) hasLegend = true;
      });
      const aspect = hints.aspect;
      fitKeys(echarts, chart, {
        option,
        placement: keyPlacement(members.position, members.placement, hasLegend),
        ...aspect ? { afterReserve: () => fitAspectAxes(instance, aspect) } : {}
      });
    } else if (members.target === "visualMap") {
      layoutColorScale(echarts, instance, {
        option,
        position: members.position ?? null,
        placement: members.placement ?? null,
        title: members.title ?? null,
        heatmap: hints.heatmap ?? null
      });
    } else {
      centerColorScales(instance, { option });
    }
    fitEndpointLabels(instance, { option });
    return { state: fitState(chart), ...height === void 0 ? {} : { height } };
  }
  function fitSquareCells(chart, option, hints) {
    if (!hints.squareCells) return;
    const { nRows, nCols, leftPx, rightPx, topPx, botPx } = hints;
    if (![nRows, nCols].every((v) => Number.isInteger(v) && v > 0) || ![leftPx, rightPx, topPx, botPx].every((v) => Number.isFinite(v) && v >= 0)) {
      throw new Error("Supply positive heatmap dimensions and nonnegative pixel margins.");
    }
    const cell = Math.min(
      (chart.getWidth() - leftPx - rightPx) / nCols,
      (chart.getHeight() - topPx - botPx) / nRows
    );
    if (cell <= 0) {
      throw new Error("Increase figure dimensions to leave room for heatmap cells and margins.");
    }
    fitHeatmap(chart, option, hints);
  }
  function fitState(chart) {
    return keyState(chart);
  }

  // src/layout/measure.ts
  var round = (v) => Math.round(v * 10) / 10;
  var rounded = (r) => ({
    x: round(r.x),
    y: round(r.y),
    width: round(r.width),
    height: round(r.height)
  });
  function blank(color) {
    if (color === null || color === void 0 || color === "none" || color === "transparent") {
      return true;
    }
    return typeof color === "string" && /^rgba\(.*,\s*0(\.0*)?\s*\)$/.test(color);
  }
  function paints(el) {
    const style = el.style ?? {};
    if (typeof style.text === "string") return style.text.length > 0;
    if (!blank(style.fill)) return true;
    const width = typeof style.lineWidth === "number" ? style.lineWidth : 1;
    return !blank(style.stroke) && width > 0;
  }
  function extent(group, keep = () => true) {
    let x0 = Number.POSITIVE_INFINITY;
    let y0 = Number.POSITIVE_INFINITY;
    let x1 = Number.NEGATIVE_INFINITY;
    let y1 = Number.NEGATIVE_INFINITY;
    group.traverse((el) => {
      if (el.isGroup || el.invisible || el.ignore || !paints(el) || !keep(el)) return;
      const r = el.getBoundingRect().clone();
      r.applyTransform(el.getComputedTransform());
      if (!(r.width > 0 || r.height > 0)) return;
      x0 = Math.min(x0, r.x);
      y0 = Math.min(y0, r.y);
      x1 = Math.max(x1, r.x + r.width);
      y1 = Math.max(y1, r.y + r.height);
    });
    return Number.isFinite(x0) ? rounded({ x: x0, y: y0, width: x1 - x0, height: y1 - y0 }) : null;
  }
  function componentExtent(chart, type, keep) {
    const model = chart.getModel().getComponent(type);
    if (!model || model.get("show") === false) return null;
    const view = chart.getViewOfComponentModel(model);
    return view ? extent(view.group, keep) : null;
  }
  var isGradient = (el) => {
    const fill = el.style?.fill;
    return Boolean(fill && typeof fill === "object" && fill.colorStops);
  };
  function measureLayout(chart) {
    const model = chart.getModel();
    const plots = [];
    for (let i = 0; ; i++) {
      const grid = model.getComponent("grid", i);
      if (!grid) break;
      const rect = grid.coordinateSystem?.getRect?.();
      if (rect && rect.width > 0) plots.push(rounded(rect));
    }
    const fromGrid = plots.length > 0;
    if (!fromGrid && chart.getViewOfSeriesModel) {
      for (const series of model.getSeries?.() ?? []) {
        const view = chart.getViewOfSeriesModel(series);
        const rect = view ? extent(view.group) : null;
        if (rect) plots.push(rect);
      }
    }
    const scale = model.getComponent("visualMap");
    const scaleView = scale && scale.get("show") !== false ? chart.getViewOfComponentModel(scale) : void 0;
    return {
      pane: { width: chart.getWidth(), height: chart.getHeight() },
      plots,
      plotsFrom: fromGrid ? "grid" : plots.length > 0 ? "series" : "none",
      legend: componentExtent(chart, "legend"),
      colorScale: scaleView ? extent(scaleView.group) : null,
      colorScaleBar: scaleView ? extent(scaleView.group, isGradient) : null,
      title: componentExtent(chart, "title")
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
    let out = {
      ...option,
      series: [...series, ...layers.flatMap((layer) => layer.series)]
    };
    if (option.grid3D) {
      for (const layer of layers) {
        const surfaces = layer.series.filter((s) => s.type === "surface");
        out = fitSurfaceAxes(out, surfaces, layer.expand !== false);
      }
    }
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
    ...heatmapRenderers,
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
  function attachLabelLayouts(option) {
    applyRepelLabels(option);
    const series = option.series;
    if (!Array.isArray(series)) return option;
    for (const s of series) {
      if (!s?.rtemisCellNotes) continue;
      s.label = { ...s.label, overflow: "truncate", ellipsis: "..." };
      s.labelLayout = (params) => ({
        width: Math.max(0, params.rect.width - 5),
        height: Math.max(0, params.rect.height - 3),
        hideOverlap: true
      });
    }
    return option;
  }

  // src/theme/backends.ts
  function overlayBackground(dark) {
    return dark ? "rgba(40,40,40,0.85)" : "rgba(255,255,255,0.85)";
  }
  function networkTheme(tokens) {
    const dark = tokens.scheme === "dark";
    return {
      palette: [...tokens.palette],
      positive_color: tokens.positive_color,
      negative_color: tokens.negative_color,
      label_color: tokens.fg_color,
      font_family: fontStack(tokens.font_family),
      scheme: tokens.scheme,
      background: tokens.bg_color,
      tooltip_background: overlayBackground(dark)
    };
  }
  function choroplethTheme(tokens) {
    const dark = tokens.scheme === "dark";
    return {
      scheme: tokens.scheme,
      background: tokens.bg_color,
      foreground: tokens.fg_color,
      font_family: fontStack(tokens.font_family),
      overlay: {
        background: overlayBackground(dark),
        foreground: tokens.fg_color,
        muted: dark ? "rgba(230,230,230,0.7)" : "rgba(26,26,26,0.65)"
      }
    };
  }
  var DIAGRAM_GRAYS = Object.freeze({
    light: Object.freeze({
      muted: "#6e6e6e",
      line: "#5c5c5c",
      surface: "#f3f3f3",
      "surface-alt": "#e6e6e6",
      border: "#8a8a8a"
    }),
    dark: Object.freeze({
      muted: "#999999",
      line: "#a3a3a3",
      surface: "#232323",
      "surface-alt": "#2e2e2e",
      border: "#6a6a6a"
    })
  });
  var DIAGRAM_SERIES = 8;
  function hex6(color) {
    let h = color.trim().replace("#", "");
    if (h.length === 3) h = [...h].map((c) => c + c).join("");
    return /^[0-9a-f]{6}$/i.test(h) ? h.toLowerCase() : null;
  }
  function flattenColor(color, background) {
    const m = /^rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*(?:,\s*([\d.]+)\s*)?\)$/i.exec(
      color.trim()
    );
    if (!m) return color;
    const a = m[4] === void 0 ? 1 : Number(m[4]);
    const base = hex6(background) ?? "ffffff";
    const channels2 = [m[1], m[2], m[3]].map((c, i) => {
      const under = Number.parseInt(base.slice(2 * i, 2 * i + 2), 16);
      const v = Math.round(a * Number(c) + (1 - a) * under);
      return Math.max(0, Math.min(255, v)).toString(16).padStart(2, "0");
    });
    return `#${channels2.join("")}`;
  }
  function diagramTheme(tokens) {
    const out = {
      bg: tokens.bg_color,
      fg: flattenColor(tokens.fg_color, tokens.bg_color),
      ...DIAGRAM_GRAYS[tokens.scheme],
      font: fontStack(tokens.font_family)
    };
    const palette = tokens.palette;
    if (palette.length > 0) {
      for (let i = 0; i < DIAGRAM_SERIES; i++) {
        out[`series-${i + 1}`] = palette[i % palette.length];
      }
      out.accent = palette[0];
    }
    return out;
  }

  // src/index.ts
  var builders = Object.freeze({
    a3: buildA3Option,
    bar: buildBarOption,
    boxplot: buildBoxplotOption,
    calibration: buildCalibrationOption,
    confusion: buildConfusionOption,
    density: buildDensityOption,
    funnel: buildFunnelOption,
    gantt: buildGanttOption,
    heatmap: buildHeatmapOption,
    histogram: buildHistogramOption,
    line: buildLineOption,
    pie: buildPieOption,
    pr: buildPrOption,
    radar: buildRadarOption,
    roc: buildRocOption,
    sankey: buildSankeyOption,
    scatter: buildScatterOption,
    scatter3d: buildScatter3DOption,
    significance: buildSignificanceOption,
    spectrogram: buildSpectrogramOption,
    sunburst: buildSunburstOption,
    survival: buildSurvivalOption,
    timeseries: buildTimeSeriesOption,
    treemap: buildTreemapOption
  });
  var aspectHints = Object.freeze({
    calibration: curveAspect,
    line: lineAspect,
    pr: curveAspect,
    roc: curveAspect,
    scatter: lineAspect
  });
  return __toCommonJS(index_exports);
})();
if (typeof module === 'object' && module.exports) module.exports = RtemisDraw;
