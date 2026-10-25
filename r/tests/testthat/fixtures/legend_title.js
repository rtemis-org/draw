// Legend titles: drawn once, inside the canvas, clear of the legend items and
// of the plotting area for outside placement, and stable across repeated fits.
// spec: draw/legend-title
const assert = require('node:assert/strict'), fs = require('node:fs'), path = require('node:path');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), layout = require(input.layout);
require(path.join(path.dirname(input.layout), 'renderers.js'))(echarts);
const near = (a, b, message) => assert.ok(Math.abs(a - b) < .05, `${message}: ${a} / ${b}`);
const overlap = (a, b) => a.x < b.x + b.width - .05 && b.x < a.x + a.width - .05 &&
  a.y < b.y + b.height - .05 && b.y < a.y + a.height - .05;
// Wrapped titles carry line breaks in place of spaces.
const squash = text => String(text ?? '').replace(/\s+/g, ' ');
const transformed = el => {
  const box = el.getBoundingRect().clone(); box.applyTransform(el.getComputedTransform()); return box;
};
for (const [name, original] of Object.entries(input.charts)) {
  for (const position of input.positions) for (const placement of ['outside', 'inside']) {
    const payload = structuredClone(original);
    payload.legendPosition = position; payload.legendPlacement = placement;
    payload.option.animation = false;
    const target = payload.legendTarget || 'legend';
    const chart = echarts.init(null, payload.theme, {renderer: 'svg', ssr: true, width: 900, height: 650});
    const titles = () => {
      const found = [];
      chart.getModel().eachComponent('graphic', model => {
        chart.getViewOfComponentModel(model).group.traverse(el => {
          if (el.type === 'text' && squash(el.style?.text) === squash(payload.legendTitle)) found.push(transformed(el));
        });
      });
      return found;
    };
    const legends = () => {
      const found = [];
      chart.getModel().eachComponent(target, model => {
        if (model.get('show') !== false) {
          const box = transformed(chart.getViewOfComponentModel(model).group);
          box.padding = [].concat(model.get('padding') ?? 5)[0];
          found.push(box);
        }
      });
      return found;
    };
    const fit = () => {layout.positionLegend(echarts, chart, payload); layout.centerVisualMaps(chart, payload, echarts);};
    const context = `${name}/${position}/${placement}`;
    try {
      chart.setOption(payload.option);
      for (const width of [900, 390, 900]) {
        chart.resize({width, height: 650}); fit();
        const before = titles();
        fit(); const after = titles();
        assert.equal(after.length, 1, `${context}/${width} title count`);
        const box = after[0];
        near(box.x, before[0].x, `${context}/${width} title x accumulates`);
        near(box.y, before[0].y, `${context}/${width} title y accumulates`);
        assert.ok(box.x >= -.05 && box.y >= -.05 && box.x + box.width <= width + .05 && box.y + box.height <= 650.05,
          `${context}/${width} title clipped: ${JSON.stringify(box)}`);
        // Legend and visualMap bounds include the component padding; the items lie inside it.
        for (const legend of legends()) {
          const p = legend.padding;
          const items = {x: legend.x + p, y: legend.y + p, width: legend.width - 2 * p, height: legend.height - 2 * p};
          assert.ok(!overlap(box, items),
            `${context}/${width} title overlaps legend items: ${JSON.stringify([box, items])}`);
        }
        const area = chart.getModel().getSeries().find(s => s.subType === 'heatmap')?.coordinateSystem.getArea() ||
          chart.getModel().getComponent('grid')?.coordinateSystem.getRect();
        if (area && placement === 'outside') {
          assert.ok(!overlap(box, area), `${context}/${width} title overlaps plot: ${JSON.stringify([box, area])}`);
        }
      }
      const svg = chart.renderToSVGString();
      assert.ok(payload.legendTitle.split(' ').every(word => svg.includes(word)), `${context} SVG title`);
    } catch (e) {throw new Error(`${context}: ${e.message}`, {cause: e});}
    finally {chart.dispose();}
  }
}
for (const [name, original] of Object.entries(input.untitled)) {
  const payload = structuredClone(original);
  const chart = echarts.init(null, payload.theme, {renderer: 'svg', ssr: true, width: 900, height: 650});
  try {
    chart.setOption(payload.option);
    layout.positionLegend(echarts, chart, payload); layout.centerVisualMaps(chart, payload, echarts);
    let graphics = 0;
    chart.getModel().eachComponent('graphic', () => {graphics++;});
    assert.equal(graphics, 0, `${name} untitled chart draws a graphic`);
  } finally {chart.dispose();}
}
console.log('Legend titles passed: placement, clearance, resize, SVG, untitled');
