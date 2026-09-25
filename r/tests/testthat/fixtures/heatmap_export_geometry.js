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
  rtemisPanels: layout, rtemisConfusion: require(path.join(directory, 'confusion.js')),
  echarts: {...echarts, init: (_, theme, options) =>
    echarts.init(null, theme, {...options, renderer: 'svg', ssr: true})}
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(input.binding, 'utf8'), context);
const near = (actual, expected) => assert.ok(Math.abs(actual - expected) < 1e-6, `${actual} != ${expected}`);
for (const payload of input.charts) {
  payload.option.animation = false;
  const original = JSON.stringify(payload.option.grid);
  const element = {style: {}, parentElement: body};
  const widget = context.rtemisDrawFactory(element, 800, 450, true);
  try {
    widget.renderValue(payload);
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
      near(visualMap.get('right'), (width - payload.leftPx - payload.rightPx - area.width) / 2);
      const [min, max] = visualMap.getExtent();
      const selected = [min + (max-min)/4, max - (max-min)/4];
      chart.dispatchAction({type:'selectDataRange', selected});
      layout.fitHeatmap(chart, payload);
      assert.deepEqual(visualMap.getSelected(), selected);
      assert.equal(JSON.stringify(payload.option.grid), original, 'Layout mutated its source margins');
      const colors = visualMap.get('inRange').color;
      assert.deepEqual(Array.from(colors), payload.theme.backgroundColor === '#181818' ? payload.colorDark : payload.colorLight);
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
