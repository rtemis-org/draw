// Shared geometry for independent chart panels in browser and SVG export.
// spec: draw/first-cran-release#current-static-export-boundary
// Child EChartsOption references stay local to each ECharts instance.
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.rtemisPanels = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  'use strict';
  // A shared surface has a meaningful fill only when all child backgrounds
  // agree. Mixed figures remain transparent instead of privileging one panel.
  function background(payload, dark = false) {
    const colors = payload.panels.map(panel => {
      const theme = panel.autoTheme && dark ? panel.themeDark : panel.theme;
      const color = panel.option?.backgroundColor || theme?.backgroundColor;
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
  function fit(payload, width, height) {
    const a = payload.aspect;
    if (!a) return;
    const grid = payload.option.grid;
    if (!grid || Array.isArray(grid)) throw new Error('A fixed-aspect panel requires one plotting grid.');
    if (![a.ratio, a.leftPx, a.rightPx, a.topPx, a.botPx].every(Number.isFinite) ||
        a.ratio <= 0 || [a.leftPx, a.rightPx, a.topPx, a.botPx].some(v => v < 0)) {
      throw new Error('Supply a positive aspect ratio and nonnegative pixel margins.');
    }
    const available = Math.min(width - a.leftPx - a.rightPx,
      (height - a.topPx - a.botPx) / a.ratio);
    const preferred = a.widthPx == null ? available : a.widthPx;
    if (!Number.isFinite(preferred) || preferred <= 0 || available <= 0) {
      throw new Error('Increase panel dimensions to leave room for the plotting grid and margins.');
    }
    grid.width = Math.min(available, preferred);
    grid.height = grid.width * a.ratio;
    grid.left = a.leftPx + (width - a.leftPx - a.rightPx - grid.width) / 2;
    grid.top = a.topPx + (height - a.topPx - a.botPx - grid.height) / 2;
  }
  // Native outer bounds reserve axis-label/name space before we enforce the
  // data-area ratio. Pinning the unmeasured outer box instead clips labels;
  // leaving native shrinkage unconstrained makes an identity line non-square.
  // Shared by standalone widgets, bounded panels, and Node SVG rendering.
  function fitAxes(chart, payload) {
    const a = payload.aspect;
    if (!a) return;
    const rect = chart.getModel().getComponent('grid').coordinateSystem.getRect();
    const width = Math.min(rect.width, rect.height / a.ratio);
    const height = width * a.ratio;
    chart.setOption({grid: {
      left: rect.x + (rect.width - width) / 2,
      top: rect.y + (rect.height - height) / 2,
      width, height, containLabel: false, outerBoundsMode: 'none'
    }});
  }
  // Fit all heatmap grids as one scene. The row/column dendrogram strips keep
  // their thickness and remain aligned with the matrix when space is added
  // around a square-cell plot. Always derive from the original option so a
  // resize cannot accumulate offsets. GridOption: coord/cartesian/GridModel.ts.
  function fitHeatmap(chart, payload) {
    if (!payload.squareCells) return;
    const {nRows, nCols, leftPx, rightPx, topPx, botPx} = payload;
    if (![nRows, nCols].every(v => Number.isInteger(v) && v > 0) ||
        ![leftPx, rightPx, topPx, botPx].every(v => Number.isFinite(v) && v >= 0)) {
      throw new Error('Supply positive heatmap dimensions and nonnegative pixel margins.');
    }
    const availableWidth = chart.getWidth() - leftPx - rightPx;
    const availableHeight = chart.getHeight() - topPx - botPx;
    const cell = Math.min(availableWidth / nCols, availableHeight / nRows);
    if (cell <= 0) throw new Error('Increase figure dimensions to leave room for heatmap cells and margins.');
    const dx = (availableWidth - nCols * cell) / 2;
    const dy = (availableHeight - nRows * cell) / 2;
    const grids = Array.isArray(payload.option.grid) ? payload.option.grid : [payload.option.grid];
    const update = {grid: grids.map(grid => {
      const update = {...grid, containLabel: false, outerBoundsMode: 'none'};
      for (const side of ['left', 'right', 'top', 'bottom']) {
        if (grid[side] != null) update[side] = grid[side] + (['left', 'right'].includes(side) ? dx : dy);
      }
      return update;
    })};
    // Keep marginal components with the fitted scene instead of leaving the
    // colorbar at the far canvas edge when height constrains the matrix.
    const maps = payload.option.visualMap;
    if (maps) update.visualMap = (Array.isArray(maps) ? maps : [maps]).map(map => {
      const side = map.orient === 'horizontal' ? 'bottom' : 'right';
      const offset = side === 'right' ? dx : dy;
      const anchor = map[side] === side ? 0 : map[side];
      return Number.isFinite(anchor) ? {[side]: anchor + offset} : {};
    });
    const titles = payload.option.title;
    if (titles) update.title = (Array.isArray(titles) ? titles : [titles]).map(title =>
      Number.isFinite(title.left) ? {left: title.left + dx} : {});
    chart.setOption(update);
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
  // Position a ROC legend relative to the measured data area, not the canvas.
  // ECharts LegendModel uses content-sized boxes (ignoreSize: true), so use
  // one anchor per axis. Measure the active font before constraining long
  // labels; native wrapping preserves every character in browser and SVG.
  // spec: draw/first-cran-release#roc-views
  function positionLegend(echarts, chart, payload) {
    const position = payload.legendPosition;
    if (!position) return;
    const model = chart.getModel().getComponent('legend');
    if (!model?.get('show')) return;
    const grid = chart.getModel().getComponent('grid').coordinateSystem.getRect();
    const inset = 12;
    const font = model.getModel('textStyle').getFont();
    const names = model.getData().map(item => item.get('name'));
    if (!names.length) return;
    const naturalWidth = Math.max(...names.map(name =>
      echarts.format.getTextRect(name, font).width));
    // The native marker-to-label gap is 5px (LegendView._createItem).
    const available = grid.width - 2 * inset - model.get('itemWidth') - 5;
    const right = position.endsWith('right');
    const bottom = position.startsWith('bottom');
    chart.setOption({legend: {
      left: right ? null : grid.x + inset,
      right: right ? chart.getWidth() - grid.x - grid.width + inset : null,
      top: bottom ? null : grid.y + inset,
      bottom: bottom ? chart.getHeight() - grid.y - grid.height + inset : null,
      // Leave short labels unconstrained: measuring then imposing that exact
      // width can wrap a word because native text layout rounds differently.
      textStyle: {width: naturalWidth > available ? Math.max(1, available) : null, overflow: 'break'}
    }});
  }
  // A centered vertical colorbar belongs beside the data grid, which may be
  // offset by titles, rotated labels, or dendrograms. Measure the native view
  // after layout so its handles and endpoint text are included. Keep this in
  // the shared renderer so browser resize and vector export agree.
  function centerVisualMaps(chart, payload) {
    const definitions = payload.option?.visualMap;
    if (!definitions) return;
    const maps = Array.isArray(definitions) ? definitions : [definitions];
    let changed = false;
    const updates = maps.map((definition, index) => {
      if (!['middle', 'center'].includes(definition.top)) return {};
      const model = chart.getModel().getComponent('visualMap', index);
      if (!model?.get('show') || model.get('orient') !== 'vertical') return {};
      const areas = [];
      model.eachTargetSeries(series => {
        if (series.subType === 'heatmap' && series.coordinateSystem?.type === 'cartesian2d') {
          areas.push(series.coordinateSystem.getArea());
        }
      });
      if (!areas.length) return {};
      const top = Math.min(...areas.map(area => area.y));
      const bottom = Math.max(...areas.map(area => area.y + area.height));
      const view = chart.getViewOfComponentModel(model);
      const height = view.group.getBoundingRect().height;
      changed = true;
      return {top: (top + bottom - height) / 2};
    });
    if (changed) chart.setOption({visualMap: updates});
  }
  return {background, cells, fit, fitAxes, fitHeatmap, prepareColors, positionLegend, centerVisualMaps};
});
