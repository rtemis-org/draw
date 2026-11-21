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
  // The fixed-aspect plotting box (`fitAspectBox()` and `fitAspectAxes()` in
  // @rtemis/draw): set on the option before drawing, then constrained again
  // inside the area ECharts' outer bounds leave for the plot.
  function fit(payload, width, height) {
    if (!payload.aspect) return;
    rtemisDraw().fitAspectBox(payload.option, payload.aspect, width, height);
  }
  function fitAxes(chart, payload) {
    if (!payload.aspect) return;
    rtemisDraw().fitAspectAxes(chart, payload.aspect);
  }
  // Square heatmap cells (`fitHeatmap()` in @rtemis/draw), from the payload's
  // heatmap hints. An export fails on hints or a surface that leave no room
  // for the cells, where a browser host draws the chart as built.
  function fitHeatmap(chart, payload) {
    if (!payload.squareCells) return;
    const {nRows, nCols, leftPx, rightPx, topPx, botPx} = payload;
    if (![nRows, nCols].every(v => Number.isInteger(v) && v > 0) ||
        ![leftPx, rightPx, topPx, botPx].every(v => Number.isFinite(v) && v >= 0)) {
      throw new Error('Supply positive heatmap dimensions and nonnegative pixel margins.');
    }
    const cell = Math.min((chart.getWidth() - leftPx - rightPx) / nCols,
      (chart.getHeight() - topPx - botPx) / nRows);
    if (cell <= 0) throw new Error('Increase figure dimensions to leave room for heatmap cells and margins.');
    rtemisDraw().fitHeatmap(chart, payload.option, payload);
  }
  // Native label layout knows each cell's final size, after grid fitting and
  // resizing. Keep literal full notes in data, and truncate only their labels.
  // Series flagged rtemisRepelLabels share one placement state per chart
  // (`repelLabels()` in @rtemis/draw), so labels of different series avoid
  // each other too.
  function prepareLabels(payload) {
    let repel = null;
    (payload.option.series || []).forEach(series => {
      if (series.rtemisRepelLabels) {
        const draw = rtemisDraw();
        repel = repel || draw.repelLabels();
        series.labelLayout = repel;
      }
      if (!series.rtemisCellNotes) return;
      series.label = {...series.label, overflow: 'truncate', ellipsis: '...'};
      series.labelLayout = params => ({
        width: Math.max(0, params.rect.width - 5),
        height: Math.max(0, params.rect.height - 3),
        hideOverlap: true
      });
    });
  }
  // Resolve the precomputed heatmap/spectrogram palettes from the actual theme,
  // including explicit overrides and offline SVG's resolved light default.
  // The same choice in both targets avoids exporting a dark plot's white ramp.
  function prepareColors(echarts, payload, theme) {
    if (!payload.colorLight && !payload.colorDark) return;
    const rgb = echarts.color.parse(payload.option.backgroundColor || theme?.backgroundColor || '#ffffff');
    const dark = rgb && rgb.slice(0, 3).reduce((sum, value, i) => sum + value * [.2126, .7152, .0722][i], 0) < 128;
    const colors = (dark ? payload.colorDark : payload.colorLight) || payload.colorLight || payload.colorDark;
    const maps = payload.option.visualMap;
    (Array.isArray(maps) ? maps : [maps]).filter(Boolean).forEach(map => {
      map.inRange = {...map.inRange, color: colors};
    });
  }
  // The member-driven legend layout (`layoutLegend()` in @rtemis/draw).
  // spec: draw/legend-title
  function layoutLegend(echarts, chart, payload) {
    rtemisDraw().layoutLegend(echarts, chart, {option: payload.option,
      position: payload.legendPosition, placement: payload.legendPlacement,
      title: payload.legendTitle, titleAbove: payload.legendTitleAbove,
      target: payload.legendTarget, aspect: payload.aspect});
  }

  // A color scale the legend members place (`layoutColorScale()` in
  // @rtemis/draw), with the payload's heatmap hints.
  function positionVisualMaps(chart, payload, echarts) {
    rtemisDraw().layoutColorScale(echarts, chart, {option: payload.option,
      position: payload.legendPosition, placement: payload.legendPlacement,
      title: payload.legendTitle, heatmap: payload});
  }
  // Sankey labels, the categorical legend, and edge-aligned pie labels
  // (`fitSankey()`, `layoutLegend()`, and `fitPieLabels()` in @rtemis/draw).
  function positionLegend(echarts, chart, payload) {
    fitSankey(echarts, chart, payload);
    layoutLegend(echarts, chart, payload);
    rtemisDraw().fitPieLabels(echarts, chart, {option: payload.option});
  }
  // The color scale and size key of a chart @rtemis/draw builds are placed by
  // the package's fitKeys(), on the side the legend members ask for and away
  // from a categorical legend. A fixed-aspect plot is fitted again inside the
  // narrowed area before the keys are placed. spec: draw/shared-builders#keys
  function fitKeys(chart, payload, echarts) {
    const draw = rtemisDraw();
    if (!draw?.fitKeys) return;
    let hasLegend = false;
    chart.getModel().eachComponent('legend', model => {
      if (model.get('show') && model.getData().length) hasLegend = true;
    });
    const placement = draw.keyPlacement(payload.legendPosition, payload.legendPlacement, hasLegend);
    draw.fitKeys(echarts, chart, {option: payload.option, placement,
      afterReserve: payload.aspect ? () => fitAxes(chart, payload) : undefined});
  }
  // A centered vertical colorbar belongs beside the data grid, which may be
  // offset by titles, rotated labels, or dendrograms. Measure the native view
  // after layout so its handles and endpoint text are included. Keep this in
  // the shared renderer so browser resize and vector export agree.
  function centerVisualMaps(chart, payload, echarts) {
    if (payload.fitKeys) return fitKeys(chart, payload, echarts);
    if (payload.legendTarget === 'visualMap') return positionVisualMaps(chart, payload, echarts);
    rtemisDraw().centerColorScales(chart, {option: payload.option});
  }
  // The package's fit of the timeline renderer (`fitGantt()` in
  // @rtemis/draw). spec: draw/first-cran-release#responsive-timelines
  function fitGantt(echarts, chart, payload) {
    rtemisDraw().fitGantt(echarts, chart, {option: payload.option,
      legendPosition: payload.legendPosition});
  }
  // Vertical Sankey labels broken to their node widths (`fitSankey()` in
  // @rtemis/draw).
  function fitSankey(echarts, chart, payload) {
    rtemisDraw().fitSankey(echarts, chart, {option: payload.option});
  }
  // Value axis end labels only on ticks (`fitEndpointLabels()` in
  // @rtemis/draw). spec: draw/zoom-controls#endpoint-labels
  function fitEndpointLabels(chart, payload) {
    rtemisDraw().fitEndpointLabels(chart, {option: payload?.option});
  }
  return {background, cells, fit, fitAxes, fitHeatmap, prepareLabels, prepareColors, positionLegend, centerVisualMaps, fitGantt, fitSankey, fitEndpointLabels};
});
