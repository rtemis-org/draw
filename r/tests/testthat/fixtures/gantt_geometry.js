// Exercise the real binding with native ECharts SVG geometry at each size.
const assert = require('node:assert/strict'), fs = require('node:fs');
const vm = require('node:vm'), path = require('node:path');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), directory = path.dirname(input.layout);
require(path.join(directory, 'renderers.js'))(echarts);
const body = {style: {}, classList: {contains: () => false}};
const context = {document: {body}, window: {}, HTMLWidgets: {widget() {}},
  rtemisPanels: require(input.layout), rtemisA3: require(path.join(directory, 'a3.js')),
  rtemisConfusion: require(path.join(directory, 'confusion.js')),
  echarts: {...echarts, init: (_, theme, options) => echarts.init(null, theme,
    {...options, renderer: 'svg', ssr: true})}};
vm.createContext(context);
vm.runInContext(fs.readFileSync(input.binding, 'utf8'), context);
function bounds(chart, type) {
  const model = chart.getModel().getComponent(type);
  const group = chart.getViewOfComponentModel(model).group;
  const rect = group.getBoundingRect().clone();
  rect.applyTransform(group.getComputedTransform());
  return rect;
}
for (const payload of input.payloads) {
  payload.option.animation = false;
  const original = JSON.stringify(payload);
  const widget = context.rtemisDrawFactory({style: {}, parentElement: body}, 900, 700, true);
  try {
    widget.renderValue(payload);
    for (const width of [900, 390, 344, 560, 900]) {
      widget.resize(width, 700);
      const chart = widget.getChart(), model = chart.getModel();
      const rect = model.getComponent('grid').coordinateSystem.getRect();
      assert.ok(rect.width > width * 0.45, `Too little time-axis space: ${rect.width}/${width}`);
      assert.ok(rect.height > 250);
      assert.equal(chart.getHeight(), 700);
      const legend = model.getComponent('legend');
      if (legend) {
        if (width <= 390) assert.equal(legend.get('orient'), 'horizontal');
        if (width === 900) assert.equal(legend.get('orient'), 'horizontal');
        const b = bounds(chart, 'legend');
        assert.ok(b.x >= -1 && b.y >= -1 && b.x + b.width <= width + 1 && b.y + b.height <= 701);
        if (legend.get('orient') === 'horizontal') {
          assert.ok(b.y + b.height <= rect.y, 'Legend overlaps timeline');
        }
        chart.dispatchAction({type: 'legendUnSelect', name: 'train'});
        chart.dispatchAction({type: 'dataZoom', dataZoomIndex: 0, start: 10, end: 80});
        widget.resize(width, 700);
        assert.equal(chart.getModel().getComponent('legend').isSelected('train'), false);
        assert.equal(chart.getModel().getComponent('dataZoom', 0).get('start'), 10);
        chart.dispatchAction({type: 'legendSelect', name: 'train'});
      }
      const title = bounds(chart, 'title'), toolbox = bounds(chart, 'toolbox');
      assert.ok(title.x + title.width <= toolbox.x || title.y + title.height <= toolbox.y,
        `Title overlaps toolbar: ${width} ${JSON.stringify({title,toolbox})}`);
      assert.ok(model.getComponent('xAxis').get('splitNumber') <= 5);
      assert.equal(JSON.stringify(payload), original, 'Resize mutated source options');
      const svg = chart.renderToSVGString();
      assert.ok(svg.includes('<path') && !svg.includes('<image'));
    }
  } finally { widget.dispose(); }
}
// Callers using low-level options retain their chosen legend placement.
const manual = JSON.parse(JSON.stringify(input.payloads[0]));
delete manual.legendPosition; delete manual.legendPlacement;
manual.option.legend = {orient: 'horizontal', left: 'center', top: 50};
const widget = context.rtemisDrawFactory({style: {}, parentElement: body}, 390, 700, true);
try {
  widget.renderValue(manual);
  widget.resize(900, 700);
  const legend = widget.getChart().getModel().getComponent('legend');
  assert.equal(legend.get('orient'), 'horizontal');
  assert.equal(legend.get('left'), 'center');
  assert.equal(legend.get('top'), 50);
} finally { widget.dispose(); }
console.log('Gantt bounds, legends, toolbar, zoom, selection, and vector marks passed');
