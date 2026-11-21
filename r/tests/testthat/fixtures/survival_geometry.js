// Assert the rendered ECharts vector geometry as well as the serialized options.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
// The widget projects a theme document with the builders' `themeObject()`.
const themeOf = (doc) => doc && typeof doc === 'object' ? require(require('node:path').join(require('node:path').dirname(input.echarts), '..', 'draw', 'rtemis-draw-builders.js')).themeObject(doc) : doc || null;
const layout = require(input.layout);
// The number-at-risk strip is a custom series drawn by the bundle's renderer.
require(input.builders).registerRenderers(echarts);
const close = (actual, expected, label) => {
  assert.equal(actual.length, expected.length, label);
  actual.forEach((v, i) => assert.ok(Math.abs(v - expected[i]) < .02, `${label}: ${v} != ${expected[i]}`));
};
for (const payload of input.charts) {
  for (const [width, height] of [[800, 600], [390, 600]]) {
    const chart = echarts.init(null, themeOf(payload.theme), {renderer: 'svg', ssr: true, width, height});
    try {
      payload.option.animation = false;
      chart.setOption(payload.option);
      layout.positionLegend(echarts, chart, payload);
      const svg = chart.renderToSVGString(); // Resolve text attachment transforms.
      const models = chart.getModel().getSeries();
      const curve = models[0];
      const pixel = (points) => points.flatMap(p => curve.coordinateSystem.dataToPoint(p));
      const view = s => chart.getViewOfSeriesModel(s);
      close(Array.from(view(curve)._polyline.shape.points), pixel([[0,1],[2,1],[2,.6],[5,.6],[5,.2]]), 'right-continuous steps');
      close(Array.from(view(models[2])._polygon.shape.points), pixel([[0,1],[2,1],[2,.8],[5,.8],[5,.5]]), 'upper confidence edge');
      close(Array.from(view(models[2])._polygon.shape.stackedOnPoints), pixel([[0,1],[2,1],[2,.4],[5,.4],[5,.1]]), 'lower confidence edge');
      close(Array.from(view(models[4])._polyline.shape.points), pixel([[0,.5],[5,.5],[5,0]]), 'median dropline');
      const color = curve.getData().getVisual('style').fill;
      for (const model of models.filter(s => s.name === 'A')) assert.equal(model.getData().getVisual('style').fill, color, 'group colors');
      const censors = models[3].getData();
      for (let i = 0; i < censors.count(); i++) {
        const el = censors.getItemGraphicEl(i);
        const rect = el.getBoundingRect().clone(); rect.applyTransform(el.getComputedTransform());
        const p = curve.coordinateSystem.dataToPoint(i === 0 ? [2,.6] : [5,.2]);
        close([rect.x + rect.width/2, rect.y + rect.height/2], p, 'censor position');
        assert.ok(rect.height >= 7.9, 'censor tick height');
      }
      // Risk counts are text marks aligned to their time coordinates below the
      // time axis, including a literal NA rather than a blank or invented zero.
      const data = models[8].getData();
      for (let i = 0; i < data.count(); i++) {
        const el = data.getItemGraphicEl(i);
        const rect = el.getBoundingRect().clone(); rect.applyTransform(el.getComputedTransform());
        const x = curve.coordinateSystem.dataToPoint([[0,0],[1,0],[5,0]][i])[0];
        assert.ok(Math.abs(rect.x + rect.width/2 - x) < .1, 'risk column alignment');
        assert.ok(rect.y > chart.getModel().getComponent('grid').coordinateSystem.getRect().y + chart.getModel().getComponent('grid').coordinateSystem.getRect().height + 48, 'risk below the axis name');
        assert.ok(rect.y + rect.height < height, 'risk text inside export');
      }
      assert.ok(!svg.includes('<image'), 'no raster marks');
      for (const text of ['>At risk<', '>NA<', '>100%<', '>60%<', '>Median 5<']) assert.ok(svg.includes(text), `SVG label ${text}`);
      chart.dispatchAction({type: 'legendUnSelect', name: 'A'});
      for (const s of models.filter(s => s.name === 'A')) assert.ok(chart.getModel().isSeriesFiltered(s), 'all group layers hidden');
      chart.dispatchAction({type: 'legendSelect', name: 'A'});
      for (const s of models.filter(s => s.name === 'A')) assert.ok(!chart.getModel().isSeriesFiltered(s), 'all group layers restored');
    } finally { chart.dispose(); }
  }
}
console.log('Survival vector geometry, risk labels, and group selection passed');
