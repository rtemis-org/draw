// Exercise the actual widget binding with the native SVG engine, including
// bounded panel dimensions, repeat resizes, dendrogram alignment, and palettes.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
const layout = require(input.layout);
const directory = path.dirname(input.layout);
require(path.join(directory, 'renderers.js'))(echarts);
const body = {style: {}, classList: {contains: () => false}};
const context = {
  document: {body}, window: {}, HTMLWidgets: {widget() {}},
  RtemisDraw: require(path.join(directory, 'rtemis-draw-builders.js')),
  RtemisVectorTheme: require(path.join(directory, 'vector_theme.js')),
  rtemisA3: require(path.join(directory, 'a3.js')),
  rtemisPanels: layout,
  echarts: {...echarts, init: (_, theme, options) =>
    echarts.init(null, theme, {...options, renderer: 'svg', ssr: true})}
};
vm.createContext(context);
// The binding's shared theme detection must see this context's stub DOM.
vm.runInContext(fs.readFileSync(path.join(directory, 'theme_watch.js'), 'utf8'), context);
vm.runInContext(fs.readFileSync(input.binding, 'utf8'), context);
const near = (actual, expected) => assert.ok(Math.abs(actual - expected) < 1e-6, `${actual} != ${expected}`);
for (const payload of input.charts) {
  const element = {style: {}, parentElement: body};
  const widget = context.rtemisDrawFactory(element, 800, 450, true);
  try {
    // The binding builds the option from the request and lays out the cells.
    widget.renderValue(payload);
    payload.option.animation = false;
    const original = JSON.stringify(payload.option.grid);
    for (const [width, height] of [[800,450], [480,620], [900,400], [800,450]]) {
      widget.resize(width, height);
      const chart = widget.getChart();
      near(chart.getWidth(), width);
      near(chart.getHeight(), height);
      const heat = chart.getModel().getSeries().find(s => s.subType === 'heatmap');
      const area = heat.coordinateSystem.getArea();
      near(area.width / payload.nCols, area.height / payload.nRows);
      const shape = heat.getData().getItemGraphicEl(0).shape;
      near(shape.width, shape.height);
      assert.ok(area.x >= 0 && area.y >= 0 && area.x + area.width <= width && area.y + area.height <= height);
      chart.getModel().getSeries().filter(s => s.subType === 'custom').forEach(s => {
        const dendro = s.coordinateSystem.getArea();
        if (s.get('itemPayload').orientation === 'row') {
          near(dendro.y, area.y); near(dendro.height, area.height);
        } else {
          near(dendro.x, area.x); near(dendro.width, area.width);
        }
      });
      const visualMap = chart.getModel().getComponent('visualMap');
      const group = chart.getViewOfComponentModel(visualMap).group;
      const box = group.getBoundingRect().clone(); box.applyTransform(group.getComputedTransform());
      near(box.y + box.height / 2, area.y + area.height / 2);
      assert.ok(box.x + box.width <= width);
      // The bar starts 16 px (KEY_GAP) right of the rightmost plotting area.
      let plotRight = area.x + area.width, barX = Infinity;
      chart.getModel().eachComponent('grid', g => {
        const r = g.coordinateSystem.getRect(); plotRight = Math.max(plotRight, r.x + r.width);
      });
      group.traverse(el => {
        if (el.isGroup || !el.style?.fill?.colorStops) return;
        const r = el.getBoundingRect().clone(); r.applyTransform(el.getComputedTransform());
        barX = Math.min(barX, r.x);
      });
      near(barX, plotRight + 16);
      const [min, max] = visualMap.getExtent();
      const selected = [min + (max-min)/4, max - (max-min)/4];
      chart.dispatchAction({type:'selectDataRange', selected});
      layout.fitHeatmap(chart, payload);
      assert.deepEqual(visualMap.getSelected(), selected);
      assert.equal(JSON.stringify(payload.option.grid), original, 'Layout mutated its source margins');
      // The derived scale runs through the theme's background at zero, the
      // midpoint of a range symmetric about it.
      const colors = visualMap.get('inRange').color;
      assert.equal(colors[50].toLowerCase(), payload.theme.backgroundColor.toLowerCase());
    }
  } finally {widget.dispose();}
}
// No square-cell hint means no constraint; malformed or impossible canvases
// must fail explicitly instead of generating non-finite or negative geometry.
layout.fitHeatmap(null, {});
for (const changes of [{nRows:0}, {leftPx:-1}, {leftPx:1000}]) {
  const payload = {...input.charts[0], ...changes};
  assert.throws(() => layout.fitHeatmap({getWidth:()=>800,getHeight:()=>450}, payload), /Supply|Increase/);
}
console.log('Square cells, bounded canvases, dendrograms, palettes, and repeat resize passed');
