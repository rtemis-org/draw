// Compare the shared point renderer with ECharts' native box layout. This
// catches changes to the upstream dodge/width algorithm, not just JSON shape.
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
require(input.renderers)(echarts);
const chart = echarts.init(null, null, { renderer: 'svg', ssr: true, width: 800, height: 600 });
const option = input.option;
option.animation = false;
chart.setOption(option);
function check(expected) {
  const model = chart.getModel();
  let count = 0;
  for (let s = 2; s < 4; s++) {
    const series = model.getSeriesByIndex(s);
    if (model.isSeriesFiltered(series)) continue;
    const data = series.getData();
    const boxes = model.getSeriesByIndex(s - 2).getData();
    for (let i = 0; i < data.count(); i++) {
      const raw = data.getRawDataItem(i).value;
      const ends = boxes.getItemLayout(raw[0]).ends;
      const c = input.horizontal ? 1 : 0;
      const center = (ends[0][c] + ends[1][c]) / 2;
      const width = Math.abs(ends[0][c] - ends[1][c]);
      const mark = data.getItemGraphicEl(i);
      const xy = [mark.shape.cx, mark.shape.cy];
      const pixel = chart.convertToPixel({ xAxisIndex: 0, yAxisIndex: 0 },
        input.horizontal ? [raw[1], raw[0]] : [raw[0], raw[1]]);
      if (Math.abs(xy[c] - center - raw[3] * width) > 1e-8 ||
          Math.abs(xy[1-c] - pixel[1-c]) > 1e-8 || mark.style.opacity !== 0.6) {
        throw new Error('Box/point geometry or opacity differs');
      }
      const grid = model.getComponent('grid').coordinateSystem.getRect();
      const r = mark.shape.r;
      if (!grid.contain(xy[0] - r, xy[1] - r) || !grid.contain(xy[0] + r, xy[1] + r)) {
        throw new Error('A point extends beyond the grid');
      }
      count++;
    }
  }
  if (count !== expected) throw new Error('Wrong visible point count: ' + count);
}
try {
  check(8);
  chart.dispatchAction({ type: 'legendUnSelect', name: 'Train' });
  check(4);
  chart.dispatchAction({ type: 'legendSelect', name: 'Train' });
  chart.resize({ width: 620, height: 400 });
  check(8);
  process.stdout.write('Box/point geometry, clipping, opacity, legend, and resize passed.');
} finally {
  chart.dispose();
}
