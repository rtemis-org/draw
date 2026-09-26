// Exercise native heatmap geometry and theme resolution, including reusing a
// payload across resizes and theme changes. SVG paths use these same marks.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts);
const confusion = require(input.confusion);
const colorsEqual = (a, b) => assert.deepEqual(echarts.color.parse(a), echarts.color.parse(b));
const luminance = rgb => rgb.slice(0, 3).map(v => v / 255)
  .map(c => c <= .04045 ? c / 12.92 : ((c + .055) / 1.055) ** 2.4)
  .reduce((sum, v, i) => sum + v * [.2126, .7152, .0722][i], 0);
for (const source of input.charts) {
  const payload = JSON.parse(JSON.stringify(source));
  for (const theme of [payload.theme, payload.themeDark || payload.theme, payload.theme]) {
    for (const [width, height] of [[800, 550], [344, 900], [1100, 500]]) {
      confusion.prepare(echarts, payload, theme, width, height);
      payload.option.animation = false;
      const chart = echarts.init(null, theme, {renderer: 'svg', ssr: true, width, height});
      try {
        chart.setOption(payload.option);
        const countStep = payload.confusion.metrics ? 5 : 2;
        chart.getModel().getSeries().forEach((series, index) => {
          const isCount = index % countStep < 2;
          const data = series.getData();
          for (let i = 0; i < data.count(); i++) {
            const element = data.getItemGraphicEl(i);
            assert.ok(element, 'Missing native heatmap cell');
            const fill = echarts.color.parse(element.style.fill);
            const text = echarts.color.parse(element.getTextContent().style.fill);
            const paintedText = text.slice(0, 3).map((v, c) => v * text[3] + fill[c] * (1 - text[3]));
            const a = luminance(fill), b = luminance(paintedText);
            assert.ok((Math.max(a, b) + .05) / (Math.min(a, b) + .05) >= 4.5,
              'Cell text does not contrast with its resolved fill');
            if (isCount) {
              // Exercise ECharts' native tooltip, not just the serialized
              // string: ordinal display text must retain trailing zeros.
              const tooltip = series.formatTooltip(i, false);
              const fraction = tooltip.blocks.find(block => block.name === 'Row fraction');
              const count = tooltip.blocks.find(block => block.name === 'Count');
              assert.equal(fraction.value, series.getRawValue(i)[7]);
              assert.equal(fraction.valueType, 'ordinal');
              assert.equal(count.value, series.getRawValue(i)[3]);
              assert.equal(typeof count.value, 'number');
              assert.ok(Math.abs(element.shape.width - element.shape.height) < 1e-8,
                `Non-square count cell: ${element.shape.width} x ${element.shape.height}`);
              if (data.get('Color fraction', i) === 0) {
                colorsEqual(element.style.fill, payload.confusion.lowColor || theme.backgroundColor);
              }
            } else if (payload.confusion.summaryColor) {
              colorsEqual(element.style.fill, payload.confusion.summaryColor);
            } else {
              const bg = echarts.color.parse(theme.backgroundColor);
              const fill = echarts.color.parse(element.style.fill);
              assert.ok(fill.slice(0, 3).every((v, c) => Math.abs(v - bg[c]) <= 13),
                'Metric background is not a faint theme tint');
              if (bg[0] === bg[1] && bg[1] === bg[2]) {
                assert.equal(fill[0], fill[1]); assert.equal(fill[1], fill[2]);
              }
            }
          }
        });
        const boxes = chart.getZr().storage.getDisplayList(true)
          .filter(el => el.type === 'tspan' && String(el.style.text).trim())
          .map(el => {
            const rect = el.getBoundingRect().clone();
            rect.applyTransform(el.getComputedTransform());
            assert.ok(rect.x >= -0.5 && rect.y >= -0.5 && rect.x + rect.width <= width + .5 &&
              rect.y + rect.height <= height + .5, `Clipped ${el.style.text}: ${JSON.stringify(rect)}`);
            return {rect, text: el.style.text};
          });
        for (let i = 0; i < boxes.length; i++) for (let j = i + 1; j < boxes.length; j++) {
          const a = boxes[i].rect, b = boxes[j].rect;
          assert.ok(Math.min(a.x + a.width, b.x + b.width) - Math.max(a.x, b.x) < .5 ||
            Math.min(a.y + a.height, b.y + b.height) - Math.max(a.y, b.y) < .5,
            `Overlapping ${boxes[i].text} / ${boxes[j].text}`);
        }
        assert.ok(payload.option.title.every(title => !title.subtext), 'Sample size remains a subtitle');
        assert.ok(!boxes.some(box => box.text.startsWith('n = ')), 'Redundant sample-size footer remains');
        assert.ok(!boxes.some(box => /omitted|missing pair/.test(box.text)),
          'Console diagnostics leaked into the chart');
      } finally { chart.dispose(); }
    }
  }
  const wide = confusion.heightForWidth(echarts, payload, payload.theme, 1100);
  const narrow = confusion.heightForWidth(echarts, payload, payload.theme, 344);
  assert.ok(Number.isFinite(wide) && Number.isFinite(narrow));
}
console.log('Confusion squares, theme fades, faint margins, clean labels, bounds, and resize passed');
