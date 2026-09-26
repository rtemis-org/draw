// Inspect rendered colorbar and grid geometry, rather than just the option's
// top value. Check real labels and preserve drag selection across resizing.
const assert = require('node:assert/strict');
const input = JSON.parse(require('node:fs').readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), layout = require(input.layout);
const register = require(require('node:path').join(require('node:path').dirname(input.layout), 'renderers.js'));
register(echarts);
for (const payload of input.charts) {
  const chart = echarts.init(null, payload.theme, {renderer: 'svg', ssr: true, width: 800, height: 600});
  try {
    payload.option.animation = false;
    chart.setOption(payload.option);
    for (const [width, height] of [[800, 600], [390, 500], [950, 700]]) {
      chart.resize({width, height});
      layout.centerVisualMaps(chart, payload);
      const model = chart.getModel().getComponent('visualMap');
      const series = chart.getModel().getSeries().find(s => s.subType === 'heatmap');
      if (model.get('show') && model.get('orient') === 'vertical') {
        const area = series.coordinateSystem.getArea();
        const view = chart.getViewOfComponentModel(model);
        const box = view.group.getBoundingRect().clone();
        box.applyTransform(view.group.getComputedTransform());
        assert.ok(Math.abs(box.y + box.height / 2 - area.y - area.height / 2) < 1e-7,
          `Colorbar center ${box.y + box.height / 2} differs from grid center ${area.y + area.height / 2}`);
      } else if (model.get('show')) {
        const box = chart.getViewOfComponentModel(model).group.getBoundingRect();
        assert.ok(model.get('top') >= series.coordinateSystem.getArea().y + series.coordinateSystem.getArea().height);
        assert.ok(box.height > 0);
      }
      const data = series.getData();
      for (let i = 0; i < data.count(); i++) {
        const text = data.getItemGraphicEl(i).getTextContent().style.text;
        assert.match(text, /^-?\d+\.\d{2}$/, `Unformatted cell label: ${text}`);
      }
      const [min, max] = model.getExtent();
      const range = [min + (max - min) / 4, max - (max - min) / 4];
      chart.dispatchAction({type: 'selectDataRange', visualMapIndex: 0, selected: range});
      layout.centerVisualMaps(chart, payload);
      assert.deepEqual(model.getSelected(), range, 'Layout reset the selected color range');
    }
  } finally {chart.dispose();}
}
console.log('Heatmap colorbar centers, two-decimal labels, and resize selection passed');
