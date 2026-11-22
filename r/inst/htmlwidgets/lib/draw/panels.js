// Shared geometry for independent chart panels in browser and SVG export.
// spec: draw/first-cran-release#current-static-export-boundary
// Child EChartsOption references stay local to each ECharts instance.
// Its fit steps delegate to the bundled @rtemis/draw: in Node the module
// beside this file, in the browser the RtemisDraw global the widget loads.
(function(root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory(() => require('./rtemis-draw-builders.js'));
  } else {
    root.rtemisPanels = factory(() => root.RtemisDraw);
  }
})(typeof globalThis !== 'undefined' ? globalThis : this, function(rtemisDraw) {
  'use strict';
  // A shared surface has a meaningful fill only when all child backgrounds
  // agree. Mixed figures remain transparent instead of privileging one panel.
  function background(payload, dark = false) {
    const colors = payload.panels.map(panel => {
      const theme = panel.autoTheme && dark ? panel.themeDark : panel.theme;
      const color = panel.option?.backgroundColor || theme?.bg_color;
      // ECharts also accepts gradients; those remain local to their child.
      return typeof color === 'string' ? color : 'transparent';
    });
    return colors.every(color => color === colors[0]) ? colors[0] : 'transparent';
  }
  function cells(count, layout, width, height) {
    const cols = layout.ncol, gap = layout.gap, pad = layout.padding;
    const rows = Math.ceil(count / cols);
    if (!Number.isInteger(count) || count < 1 || !Number.isInteger(cols) || cols < 1 ||
        ![width, height, gap, pad].every(Number.isFinite) || gap < 0 || pad < 0) {
      throw new Error('Supply a valid panel count, layout, and finite figure dimensions.');
    }
    const w = (width - 2 * pad - (cols - 1) * gap) / cols;
    const h = (height - 2 * pad - (rows - 1) * gap) / rows;
    if (w <= 0 || h <= 0) throw new Error('Increase figure dimensions to leave room for panels.');
    return Array.from({length: count}, (_, i) => ({
      x: pad + (i % cols) * (w + gap), y: pad + Math.floor(i / cols) * (h + gap),
      width: w, height: h
    }));
  }
  // The fixed-aspect plotting box set on the option before drawing
  // (`fitAspectBox()` in @rtemis/draw); `fitChart()` constrains it again
  // inside the area ECharts' outer bounds leave for the plot.
  function fit(payload, width, height) {
    if (!payload.aspect) return;
    rtemisDraw().fitAspectBox(payload.option, payload.aspect, width, height);
  }

  // Square heatmap cells (`fitSquareCells()` in @rtemis/draw), from the
  // payload's heatmap hints; it fails on hints or a surface that leave no
  // room for the cells.
  function fitHeatmap(chart, payload) {
    if (!payload.squareCells) return;
    rtemisDraw().fitSquareCells(chart, payload.option, payload);
  }
  // The label layouts the built option asks for by flag, as functions its
  // JSON cannot carry (`attachLabelLayouts()` in @rtemis/draw): repelled
  // labels and a heatmap's truncated cell notes. Before setOption.
  function prepareLabels(payload) {
    rtemisDraw().attachLabelLayouts(payload.option);
  }
  // Value axis end labels only on ticks (`fitEndpointLabels()` in
  // @rtemis/draw). spec: draw/zoom-controls#endpoint-labels
  function fitEndpointLabels(chart, payload) {
    rtemisDraw().fitEndpointLabels(chart, {option: payload?.option});
  }
  // The whole fit of a payload (`fit()` in @rtemis/draw): its chart type, its
  // option, the build-time hints the payload carries (heatmap cells, the
  // aspect box, A3 sizes), and its legend members. Returns the fit's result,
  // with the height an unbounded A3 surface takes.
  // spec: draw/chart-appearance#build-and-fit
  function fitChart(echarts, chart, payload, bounded = true) {
    return rtemisDraw().fit(echarts, chart, {
      type: payload.fitKeys ? 'scatter' : (payload.build?.type ?? null),
      option: payload.option,
      hints: {heatmap: payload, aspect: payload.aspect ?? null, a3: payload.a3 ?? null},
      members: {position: payload.legendPosition ?? null, placement: payload.legendPlacement ?? null,
        title: payload.legendTitle ?? null, titleAbove: !!payload.legendTitleAbove,
        target: payload.legendTarget ?? null},
      bounded
    });
  }
  return {fitChart, background, cells, fit, fitHeatmap, prepareLabels, fitEndpointLabels};
});
