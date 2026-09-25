// Shared geometry for independent chart panels in browser and SVG export.
// Child EChartsOption references stay local to each ECharts instance.
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.rtemisPanels = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  'use strict';
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
    grid.containLabel = false;
    grid.outerBoundsMode = 'none';
  }
  return {cells, fit};
});
