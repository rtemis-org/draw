// Verify actual native layout: all corner anchors, responsive wrapping, and
// full vector labels. Exercise selection without replacing the chart model.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
const layout = require(input.layout);
for (const original of input.charts) {
  const chart = echarts.init(null, original.theme, {
    renderer: 'svg', ssr: true, width: 800, height: 550
  });
  try {
    for (const [width, height] of [[800, 550], [344, 600], [800, 550]]) {
      const payload = JSON.parse(JSON.stringify(original));
      payload.option.animation = false;
      chart.resize({width, height});
      layout.fit(payload, width, height);
      chart.setOption(payload.option, true);
      layout.fitAxes(chart, payload);
      layout.positionLegend(echarts, chart, payload);
      const grid = chart.getModel().getComponent('grid').coordinateSystem.getRect();
      const legend = chart.getModel().getComponent('legend');
      const group = chart.getViewOfComponentModel(legend).group;
      const box = group.getBoundingRect().clone();
      box.applyTransform(group.getComputedTransform());
      const position = payload.legendPosition;
      const xGap = position.endsWith('right') ? grid.x + grid.width - box.x - box.width : box.x - grid.x;
      const yGap = position.startsWith('bottom') ? grid.y + grid.height - box.y - box.height : box.y - grid.y;
      assert.ok(Math.abs(xGap - 12) < 0.01, `Wrong horizontal anchor: ${xGap}`);
      assert.ok(Math.abs(yGap - 12) < 0.01, `Wrong vertical anchor: ${yGap}`);
      assert.ok(box.x >= grid.x && box.y >= grid.y &&
        box.x + box.width <= grid.x + grid.width &&
        box.y + box.height <= grid.y + grid.height, 'Legend extends outside the grid');
      const text = [];
      group.traverse(el => {
        if (el.type === 'text') {
          el.update();
          // Inspect rendered spans, not the original unwrapped style string.
          const spans = el.childrenRef().filter(child => child.type === 'tspan');
          if (!String(el.style.text).includes('AUC mean')) {
            assert.equal(spans.length, 1, 'Ordinary class/AUC label wrapped unnecessarily');
          }
          text.push(spans.map(child => child.style.text).join(''));
        }
      });
      const compact = value => value.replace(/\s/g, '');
      const names = legend.getData().map(item => item.get('name'));
      for (const name of names) {
        assert.ok(text.some(value => compact(value) === compact(name)), `Missing label: ${name}`);
      }
      assert.ok(!chart.renderToSVGString().includes('<image'), 'Unexpected bitmap');
      chart.dispatchAction({type: 'legendUnSelect', name: names[0]});
      const hidden = [];
      chart.getModel().eachSeries(series => hidden.push(series.name));
      assert.ok(!hidden.includes(names[0]), 'Legend did not hide its curves');
      assert.equal(hidden.length, payload.option.series.length -
        payload.option.series.filter(series => series.name === names[0]).length);
      // Layout changes preserve the user's selection.
      layout.positionLegend(echarts, chart, payload);
      assert.equal(chart.getModel().getComponent('legend').isSelected(names[0]), false);
      chart.dispatchAction({type: 'legendSelect', name: names[0]});
    }
  } finally {
    chart.dispose();
  }
}
console.log('ROC corner anchors, wrapping, full SVG text, and selection passed');
