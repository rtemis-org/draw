// Check actual vector rectangles against numeric endpoints, not category labels.
const fs = require('node:fs');
const assert = require('node:assert/strict');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
require(input.renderers)(echarts);
const near = (a, b, tolerance = 1e-7) => assert.ok(Math.abs(a - b) < tolerance, `${a} != ${b}`);
let cases = 0;
for (const item of input.charts) {
  const chart = echarts.init(null, item.theme, { renderer: 'svg', ssr: true, width: 800, height: 600 });
  item.option.animation = false;
  chart.setOption(item.option);
  function check(hidden) {
    const model = chart.getModel();

    assert.equal(model.getComponent('xAxis').get('type'), 'value');
    for (const series of model.getSeries()) {
      if (series.name === 'A' && item.option.legend?.show!==false) assert.equal(model.isSeriesFiltered(series), hidden);
      if (model.isSeriesFiltered(series)) continue;
      const data = series.getData();
      const xi=series.get('xAxisIndex')||0, yi=series.get('yAxisIndex')||0;
      const grid=model.getComponent('grid',model.getComponent('xAxis',xi).get('gridIndex')||0).coordinateSystem.getRect();
      const pixel=value=>chart.convertToPixel({xAxisIndex:xi,yAxisIndex:yi},value);
      if (series.subType === 'custom') {
        for (let i = 0; i < data.count(); i++) {
          const raw = data.getRawDataItem(i);
          const mark = data.getItemGraphicEl(i);
          assert.equal(mark.type, 'rect');
          const settings=series.get('itemPayload');
          const active=model.getSeries().filter(s=>s.subType==='custom'&&!model.isSeriesFiltered(s));
          const position=active.indexOf(series);
          let lo=raw[0],hi=raw[1],base=0;
          if(settings.mode==='group') {
            const span=(hi-lo)/active.length;lo+=position*span;hi=lo+span;
          } else if(settings.mode==='stack') {
            base=active.slice(0,position).reduce((sum,s)=>{
              const value=s.getData().getRawDataItem(i)[2];
              return sum+(Math.sign(value)===Math.sign(raw[2])?value:0);
            },0);
          }
          const left = pixel([lo, base+raw[2]]), right = pixel([hi, base]);
          near(mark.shape.x, left[0]); near(mark.shape.y, Math.min(left[1],right[1]));
          near(mark.shape.width, right[0] - left[0]);
          near(mark.shape.height, Math.abs(right[1] - left[1]));
          assert.equal(mark.style.opacity, .25);
          assert.ok(left[0] >= grid.x - 1e-7 && right[0] <= grid.x + grid.width + 1e-7, 'Bin clipped horizontally');
          assert.ok(left[1] >= grid.y - 1e-7 && right[1] <= grid.y + grid.height + 1e-7, 'Bin clipped vertically');
        }
      } else if (series.subType === 'line' && data.count()) {
        const line = chart.getViewOfSeriesModel(series)._polyline;
        const hist = model.getSeriesByName(series.name).find(s => s.subType === 'custom');
        assert.equal(line.style.stroke, hist.getData().getItemGraphicEl(0).style.fill, 'Layer colors disagree');
        const points = line.shape.points;
        for (let i = 0; i < data.count(); i++) {
          const raw = data.getRawDataItem(i);
          const expected = pixel(raw);
          near(points[i * 2], expected[0], 1e-4);
          near(points[i * 2 + 1], expected[1], 1e-4);
        }
      }
    }
    const svg = chart.renderToSVGString();
    assert.ok(svg.includes('<path'));
    assert.ok(!svg.includes('<image'));
    assert.ok(!svg.includes('NaN'));
    cases++;
  }
  try {
    for (const width of [800, 344]) {
      chart.resize({ width, height: 600 });
      check(false);
      if (item.option.legend && item.option.legend.show!==false) {
        chart.dispatchAction({ type: 'legendUnSelect', name: 'A' }); check(true);
        chart.dispatchAction({ type: 'legendSelect', name: 'A' }); check(false);
      }
    }
  } finally { chart.dispose(); }
}
process.stdout.write(`${cases} histogram/density native geometry cases passed.\n`);
