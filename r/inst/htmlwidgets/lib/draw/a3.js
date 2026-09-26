// Shared A3 surface fitting: native ECharts marks remain JSON and vector-safe.
// GridOption (coord/cartesian/GridModel.ts), LegendOption (component/legend/
// LegendModel.ts), and series symbol/label styles (util/types.ts).
// spec: draw/first-cran-release#a3-responsive-surface-layout
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.rtemisA3 = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  'use strict';
  function fit(chart, payload, bounded = true) {
    const hint = payload.a3;
    if (!hint) return;
    const source = payload.option, width = chart.getWidth();
    const xRange = source.xAxis.max - source.xAxis.min;
    const yRange = source.yAxis.max - source.yAxis.min;
    const marker = hint.markerSize, step = hint.residueSpacing;
    const glyph = Math.max(1, marker, hint.fontSize);
    if (![marker, hint.fontSize].every(v => Number.isFinite(v) && v >= 0) ||
        ![step, xRange, yRange].every(v => Number.isFinite(v) && v > 0)) {
      throw new Error('Supply nonnegative A3 glyph sizes, positive residue spacing, and increasing axis ranges.');
    }
    let legendHeight = 0, below = false;
    const grid = {...source.grid};
    if (hint.autoGrid) {
      const required = xRange / step * (glyph + 4);
      below = width - grid.left - grid.right < required;
      // Use one native legend component per annotation family. Plain legend
      // data deduplicates repeated newline entries; independent groups preserve
      // headings with their annotations at every width without callbacks.
      const groups = [];
      source.legend.data.forEach(entry => {
        const name = typeof entry === 'string' ? entry : entry.name;
        if (!groups.length || name.startsWith('{heading|')) groups.push([]);
        groups[groups.length - 1].push(entry);
      });
      const selected = {};
      chart.getModel().eachComponent('legend', model => Object.assign(selected, model.get('selected')));
      const legends = groups.map((data, index) => ({
        ...source.legend, id: `rtemis-a3-legend-${index}`, data, selected,
        orient: below ? 'horizontal' : 'vertical',
        left: below ? 24 : null, right: below ? 24 : source.legend.right,
        top: 0, bottom: null,
        width: below ? Math.max(1, width - 48) : source.legend.width,
        height: below ? null : 1000000,
        textStyle: {...source.legend.textStyle, rich: {heading: {
          ...source.legend.textStyle.rich.heading, padding: [2, 0, 2, 0]
        }}}
      }));
      chart.setOption({legend: legends}, {replaceMerge: ['legend']});
      const heights = legends.map((_, index) => {
        const model = chart.getModel().getComponent('legend', index);
        return chart.getViewOfComponentModel(model).group.getBoundingRect().height;
      });
      legendHeight = heights.reduce((sum, value) => sum + value, 0) + Math.max(0, heights.length - 1) * 6;
      if (below) {
        grid.right = 24;
        grid.bottom = 24 + legendHeight + 24;
      }
      if (hint.autoHeight && !bounded) {
        const body = glyph * 2 * yRange;
        const height = grid.top + (below ? body : Math.max(body, legendHeight)) + grid.bottom;
        chart.resize({width, height: Math.ceil(height)});
      }
      let top = below ? chart.getHeight() - 24 - legendHeight : grid.top;
      chart.setOption({grid, legend: legends.map((legend, index) => {
        const position = {id: legend.id, top};
        top += heights[index] + 6;
        return position;
      })});
    }
    // Actual native grid dimensions also respect caller-supplied margins.
    const area = chart.getModel().getComponent('grid').coordinateSystem.getRect();
    if (area.width <= 0 || area.height <= 0) {
      throw new Error('Increase A3 figure dimensions to leave room for residues and the legend.');
    }
    const dx = area.width / xRange * step, dy = area.height / yRange;
    const turn = Math.hypot(Math.sqrt(3) / 2 * dx, dy / 2);
    const scale = Math.min(1, Math.min(dx, dy, turn) / (glyph + 4));
    // Scale every glyph dimension from its original value, never from the
    // previous frame. Geometry/data identities and all annotation colors stay.
    const scaled = (object, keys) => {
      if (!object) return undefined;
      const update = {};
      keys.forEach(key => {
        if (typeof object[key] === 'number') update[key] = object[key] * scale;
        else if (Array.isArray(object[key])) update[key] = object[key].map(v => v * scale);
      });
      return update;
    };
    chart.setOption({series: source.series.map(series => ({
      ...scaled(series, ['symbolSize', 'symbolOffset']),
      lineStyle: scaled(series.lineStyle, ['width']),
      itemStyle: scaled(series.itemStyle, ['borderWidth']),
      data: series.data.map(datum => datum?.label ? {
        ...datum, label: {...datum.label, ...scaled(datum.label, ['fontSize', 'distance'])}
      } : datum)
    }))});
  }
  return {fit};
});
