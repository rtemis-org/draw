// Native ECharts geometry and interaction state, including repeated fitting.
const assert = require('node:assert/strict'), fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), layout = require(input.layout);
// The widget projects a theme document with the builders' `themeObject()`.
const themeOf = (doc) => doc && typeof doc === 'object' ? require(require('node:path').join(require('node:path').dirname(input.echarts), '..', 'draw', 'rtemis-draw-builders.js')).themeObject(doc) : doc || null;
require(require('node:path').join(require('node:path').dirname(input.layout), 'renderers.js'))(echarts);
const near = (a, b, message) => assert.ok(Math.abs(a - b) < .05, `${message}: ${a} / ${b}`);
// The drawn marks of a color scale: its bar, handles, and labels, without its
// transparent layout background.
const drawn = group => {
  let box = null;
  group.traverse(el => {
    if (el.isGroup || el.invisible || el.style?.fill === 'rgba(0,0,0,0)') return;
    const r = el.getBoundingRect().clone(); r.applyTransform(el.getComputedTransform());
    if (!(r.width > 0 && r.height > 0)) return;
    if (!box) box = r; else box.union(r);
  });
  return box;
};
for (const [name, original] of Object.entries(input.charts)) {
  for (const position of input.positions) for (const placement of ['outside', 'inside']) {
    const payload = structuredClone(original);
    payload.legendPosition = position; payload.legendPlacement = placement;
    payload.option.animation = false;
    const source = JSON.stringify(payload);
    const chart = echarts.init(null, themeOf(payload.theme), {renderer: 'svg', ssr: true, width: 900, height: 650});
    const boxes = () => {
      const result = [];
      chart.getModel().eachComponent(payload.legendTarget || 'legend', model => {
        const group = chart.getViewOfComponentModel(model).group;
        const box = model.mainType === 'visualMap' ? drawn(group) : group.getBoundingRect().clone();
        if (model.mainType !== 'visualMap') box.applyTransform(group.getComputedTransform());
        result.push({box, model});
      }); return result;
    };
    try {
      chart.setOption(payload.option);
      const fit = () => {layout.fitChart(echarts, chart, payload);};
      for (const width of [900, 390, 900]) {
        chart.resize({width, height: 650}); fit();
        const before = boxes().map(x => x.box);
        fit(); const after = boxes();
        const area = chart.getModel().getSeries().find(s => s.subType === 'heatmap')?.coordinateSystem.getArea() ||
          chart.getModel().getComponent('grid')?.coordinateSystem.getRect();
        for (let i = 0; i < after.length; i++) {
          const {box, model} = after[i];
          const context = `${name}/${position}/${placement}/${width}`;
          near(box.x, before[i].x, context + ' x accumulates'); near(box.y, before[i].y, context + ' y accumulates');
          assert.ok(box.x >= -.05 && box.y >= -.05 && box.x + box.width <= width + .05 && box.y + box.height <= 650.05,
            context + ' clipped: ' + JSON.stringify(box));
          if (area && placement === 'outside') {
            if (position.startsWith('top')) assert.ok(box.y + box.height <= area.y + .05, context + ' overlaps top');
            if (position.startsWith('bottom')) assert.ok(box.y >= area.y + area.height - .05, context + ' overlaps bottom');
            if (position === 'left') assert.ok(box.x + box.width <= area.x + .05, context + ' overlaps left');
            if (position === 'right') assert.ok(box.x >= area.x + area.width - .05, context + ' overlaps right');
          }
          if (model.mainType === 'legend') {
            const names = model.getData().map(item => item.get('name'));
            const group = chart.getViewOfComponentModel(model).group;
            const rendered = [];
            group.traverse(el => {if (el.type === 'text') {el.update(); rendered.push(el.childrenRef().map(x => x.style?.text || '').join(''));}});
            for (const label of names) assert.ok(rendered.some(x => x.replace(/\s/g, '') === label.replace(/\s/g, '')), context + ' missing ' + label);
            chart.dispatchAction({type: 'legendUnSelect', name: names[0]}); fit();
            assert.equal(chart.getModel().getComponent('legend').isSelected(names[0]), false, context + ' selection reset');
            chart.dispatchAction({type: 'legendSelect', name: names[0]});
          }
        }
        if (payload.aspect) near(area.width, area.height, `${name} square`);
        if (payload.squareCells) near(area.width / payload.nCols, area.height / payload.nRows, 'square cells');
        const svg = chart.renderToSVGString(); assert.ok(svg.includes('<path') && !svg.includes('<image'));
        assert.equal(JSON.stringify(payload), source, 'Source payload mutated');
      }
    } catch (e) {throw new Error(`${name}/${position}/${placement}: ${e.message}`, {cause: e});}
    finally {chart.dispose();}
  }
}
console.log('Shared legend layout passed: anchors, spaces, complete labels, state, SVG, resize');
