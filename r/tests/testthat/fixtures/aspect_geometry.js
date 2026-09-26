// Check rendered geometry, not only the requested option dimensions.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const payload = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(payload.echarts);
const layout = require(payload.layout);
for (const original of payload.charts) {
  const chart = echarts.init(null, original.theme, {
    renderer: 'svg', ssr: true, width: 800, height: 500
  });
  try {
    for (const [width, height] of [[800, 500], [500, 600], [800, 500]]) {
      const current = JSON.parse(JSON.stringify(original));
      current.option.animation = false;
      current.option.series.forEach(series => { series.animation = false; });
      layout.fit(current, width, height);
      chart.resize({width, height});
      chart.setOption(current.option, true);
      layout.fitAxes(chart, current);
      const grid = chart.getModel().getComponent('grid').coordinateSystem.getRect();
      assert.ok(Math.abs(grid.height / grid.width - current.aspect.ratio) < 1e-10);
      const texts = chart.getZr().storage.getDisplayList(true).filter(el => el.type === 'tspan');
      const boxes = texts.map(el => {
        const rect = el.getBoundingRect().clone();
        rect.applyTransform(el.getComputedTransform());
        assert.ok(rect.x >= -0.5 && rect.y >= -0.5 &&
          rect.x + rect.width <= width + 0.5 && rect.y + rect.height <= height + 0.5,
          `Clipped text: ${el.style.text} at ${JSON.stringify(rect)}`);
        return {text: el.style.text, rect};
      });
      const axisName = boxes.find(b => b.text === 'True');
      const legends = boxes.filter(b => b.text.startsWith('Training') || b.text.startsWith('Test'));
      if (axisName && legends.length) {
        for (const legend of legends) {
          assert.ok(axisName.rect.y + axisName.rect.height < legend.rect.y,
            'Legend overlaps the x-axis name');
        }
      }
    }
  } finally {
    chart.dispose();
  }
}
console.log('Fixed-aspect labels, legends, ratios, and resize passed');
