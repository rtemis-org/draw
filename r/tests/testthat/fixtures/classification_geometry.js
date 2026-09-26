// Inspect native text geometry after actual layout, including resize. This
// catches collapsed ROC axes and colliding confusion-summary labels which
// remain syntactically valid ECharts options and nonempty SVG exports.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
const layout = require(input.layout);
const confusion = require(input.confusion);
for (const original of input.charts) {
  const chart = echarts.init(null, original.theme, {
    renderer: 'svg', ssr: true, width: 800, height: 550
  });
  try {
    for (const [width, height] of [[344, 600], [800, 550], [344, 600]]) {
      const current = JSON.parse(JSON.stringify(original));
      current.option.animation = false;
      chart.resize({width, height});
      layout.fit(current, width, height);
      confusion.prepare(echarts, current, current.theme, width, height);
      chart.setOption(current.option, true);
      layout.fitAxes(chart, current);
      layout.positionLegend(echarts, chart, current);
      if (current.aspect) {
        const grid = chart.getModel().getComponent('grid').coordinateSystem.getRect();
        assert.ok(grid.width >= 200, 'ROC plotting area collapsed');
        assert.ok(Math.abs(grid.width - grid.height) < 1e-8, 'ROC axes are not square');
      }
      const boxes = chart.getZr().storage.getDisplayList(true)
        .filter(el => el.type === 'tspan' && String(el.style.text).trim())
        .map(el => {
          const rect = el.getBoundingRect().clone();
          rect.applyTransform(el.getComputedTransform());
          assert.ok(rect.x >= -0.5 && rect.y >= -0.5 &&
            rect.x + rect.width <= width + 0.5 && rect.y + rect.height <= height + 0.5,
            `Clipped text ${el.style.text}: ${JSON.stringify(rect)}`);
          return {text: el.style.text, rect};
        });
      for (let i = 0; i < boxes.length; i++) {
        for (let j = i + 1; j < boxes.length; j++) {
          const a = boxes[i].rect, b = boxes[j].rect;
          const overlapX = Math.min(a.x + a.width, b.x + b.width) - Math.max(a.x, b.x);
          const overlapY = Math.min(a.y + a.height, b.y + b.height) - Math.max(a.y, b.y);
          assert.ok(overlapX < 0.5 || overlapY < 0.5,
            `Overlapping labels: ${boxes[i].text} / ${boxes[j].text}`);
        }
      }
    }
  } finally {
    chart.dispose();
  }
}
console.log('Classification phone labels, full legends, square axes, and resize passed');
