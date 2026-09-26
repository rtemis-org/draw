// Inspect native axis marks in the SVG renderer, not just serialized flags.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
for (const item of input.charts) {
  const chart = echarts.init(null, item.theme, {renderer: 'svg', ssr: true, width: 700, height: 500});
  try {
    item.option.animation = false;
    chart.setOption(item.option);
    const axisName = item.horizontal ? 'yAxis' : 'xAxis';
    const model = chart.getModel().getComponent(axisName);
    const lines = [];
    chart.getViewOfComponentModel(model).group.traverse(el => {
      if (el.anid === 'line') lines.push(el);
    });
    assert.equal(lines.length, item.zero ? 1 : 0, 'Nonzero boundary received axis emphasis');
    if (item.zero) {
      const rect = lines[0].getBoundingRect().clone();
      rect.applyTransform(lines[0].getComputedTransform());
      const coordinate = item.horizontal ? rect.x + rect.width / 2 : rect.y + rect.height / 2;
      const zero = chart.convertToPixel(item.horizontal ? {xAxisIndex: 0} : {yAxisIndex: 0}, 0);
      // Native subpixel stroke alignment can offset a one-pixel line by .5px.
      assert.ok(Math.abs(coordinate - zero) <= .51, 'Emphasis line is not at zero');
    }
    const svg = chart.renderToSVGString();
    assert.ok(!/missing|omitted/.test(svg), 'Console diagnostic leaked into SVG');
    assert.ok(!svg.includes('<image'), 'Boxplot SVG contains a raster');
  } finally { chart.dispose(); }
}
console.log('Boxplot native zero baselines and clean vector output passed');
