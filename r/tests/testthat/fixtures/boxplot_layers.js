// Validate actual ECharts/ZRender geometry against native boxes and data values.
const fs = require('node:fs');
const assert = require('node:assert/strict');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
require(input.renderers)(echarts);
const near = (a, b) => assert.ok(Math.abs(a - b) < 1e-7, `${a} != ${b}`);
let cases = 0;
for (const item of input.charts) {
  const chart = echarts.init(null, item.theme || null, { renderer: 'svg', ssr: true, width: 800, height: 600 });
  item.option.animation = false;
  chart.setOption(item.option);
  const cdim = item.horizontal ? 1 : 0;
  const vdim = 1 - cdim;
  const pixel = (category, value) => chart.convertToPixel({ xAxisIndex: 0, yAxisIndex: 0 },
    item.horizontal ? [value, category] : [category, value]);
  function center(seriesIndex, category) {
    const ends = chart.getModel().getSeriesByIndex(seriesIndex).getData().getItemLayout(category).ends;
    return (ends[0][cdim] + ends[1][cdim]) / 2;
  }
  function check(hidden) {
    const model = chart.getModel();
    const grid = model.getComponent('grid').coordinateSystem.getRect();
    let polygons = 0, pairs = 0, brackets = 0;
    model.eachSeries(series => {
      const option = series.option;
      const data = series.getData();
      const payload = option.itemPayload;
      const renderer = option.renderItem;
      if (option.type === 'boxplot' && item.geometry === 'violin') {
        for (let i = 0; i < data.count(); i++) {
          const mark = data.getItemGraphicEl(i);
          if (mark) assert.equal(mark.style.opacity, 0, 'Violin-only anchor is visible');
        }
      }
      for (let i = 0; i < data.count(); i++) {
        const el = data.getItemGraphicEl(i);
        const raw = data.getRawDataItem(i);
        const values = raw.value || raw;
        if (renderer === 'rtemis.violin.v1') {
          const profile = payload.profiles[i];
          if (!profile.position) { assert.ok(!el); continue; }
          assert.ok(el);
          assert.equal(el.type, profile.position.length === 1 ? 'polyline' : 'polygon');
          const n = profile.position.length;
          assert.equal(el.shape.points.length, 2 * n);
          for (let j = 0; j < n; j++) {
            const a = el.shape.points[j], b = el.shape.points[2 * n - 1 - j];
            near((a[cdim] + b[cdim]) / 2, center(payload.boxIndex, values[0]));
            near(a[vdim], pixel(values[0], profile.position[j])[vdim]);
            near(a[vdim], b[vdim]);
            assert.ok(grid.contain(...a) && grid.contain(...b), 'Violin clipped');
          }
          assert.equal(el.style.fillOpacity, .25);
          polygons++;
        } else if (renderer === 'rtemis.boxplot_pairs.v1') {
          assert.ok(el);
          const points = model.getSeries().find(s => s.option.name === option.name && s.option.renderItem === 'rtemis.boxplot_points.v1');
          for (const [end, category, value] of [[0, values[0], values[1]], [1, values[3], values[4]]]) {
            near(el.shape.points[end][vdim], pixel(category, value)[vdim]);
            const pointData = points.getData();
            let found = false;
            for (let k = 0; k < pointData.count(); k++) {
              const row = pointData.getRawDataItem(k).value;
              if (row[0] === category && row[2] === values[6]) {
                const circle = pointData.getItemGraphicEl(k).shape;
                near(el.shape.points[end][0], circle.cx);
                near(el.shape.points[end][1], circle.cy);
                found = true;
              }
            }
            assert.ok(found, 'Paired endpoint has no matching observation mark');
          }
          pairs++;
        } else if (renderer === 'rtemis.boxplot_brackets.v1') {
          if (hidden) { assert.ok(!el, 'Orphan bracket after legend filtering'); continue; }
          const children = el.children();
          const shape = children[0].shape.points;
          near(shape[1][cdim], center(values[3], values[0]));
          near(shape[2][cdim], center(values[4], values[1]));
          near(shape[1][vdim], pixel(values[0], values[2])[vdim]);
          assert.equal(children[1].style.text, 'Comparison');
          const bounds = children[1].getBoundingRect().clone();
          bounds.applyTransform(children[1].getComputedTransform());
          assert.ok(grid.contain(bounds.x, bounds.y) && grid.contain(bounds.x + bounds.width, bounds.y + bounds.height), 'Bracket label clipped');
          brackets++;
        }
      }
    });
    const svg = chart.renderToSVGString();
    assert.ok(!svg.includes('<image'), 'Rasterized chart marks');
    assert.ok(!svg.includes('NaN'), 'Invalid geometry');
    if (item.option.legend) {
      assert.equal(pairs, hidden ? 2 : 4);
      assert.equal(brackets, hidden ? 0 : 1);
      if (!hidden) assert.ok(svg.includes('Comparison'));
      if (item.geometry !== 'box') assert.equal(polygons, hidden ? 2 : 4);
    }
    cases++;
  }
  try {
    for (const width of [800, 344]) {
      chart.resize({ width, height: 600 });
      check(false);
      if (item.option.legend) {
        chart.dispatchAction({ type: 'legendUnSelect', name: 'G1' });
        check(true);
        chart.dispatchAction({ type: 'legendSelect', name: 'G1' });
        check(false);
      }
    }
  } finally { chart.dispose(); }
}
process.stdout.write(`${cases} native distribution geometry cases passed.\n`);
