// Native vertical Sankey labels must remain complete, distinct and in bounds.
const assert = require('node:assert/strict');
const input = JSON.parse(require('node:fs').readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), layout = require(input.layout);
for (const theme of [input.light, input.dark]) {
  const payload = structuredClone(input.payload);
  const chart = echarts.init(null, theme, {renderer:'svg',ssr:true,width:1200,height:650});
  try {
    payload.option.animation = false;
    chart.setOption(payload.option);
    for (const width of [1200,390,760,1200]) {
      chart.resize({width,height:650});
      layout.fitSankey(echarts, chart, payload);
      const data=chart.getModel().getSeriesByIndex(0).getData();
      const boxes=[];
      for(let i=0;i<data.count();i++) {
        const text=data.getItemGraphicEl(i).getTextContent();
        assert.equal(text.style.text, data.getName(i));
      }
      for (const text of chart.getZr().storage.getDisplayList(true).filter(el => el.type === 'tspan')) {
        const rect=text.getBoundingRect().clone();rect.applyTransform(text.getComputedTransform());
        assert.ok(rect.x>=-.5 && rect.y>=-.5 && rect.x+rect.width<=width+.5 && rect.y+rect.height<=650.5,
          `Clipped ${text.style.text} at width ${width}: ${JSON.stringify(rect)}`);
        boxes.push(rect);
      }
      for(let i=0;i<boxes.length;i++)for(let j=i+1;j<boxes.length;j++) {
        const a=boxes[i],b=boxes[j];
        assert.ok(Math.min(a.x+a.width,b.x+b.width)-Math.max(a.x,b.x)<.5 ||
          Math.min(a.y+a.height,b.y+b.height)-Math.max(a.y,b.y)<.5, 'Overlapping Sankey labels');
      }
    }
    // An explicit low-level label position must not be replaced by auto-fit.
    payload.option.series[0].label.position = 'inside';
    chart.setOption(payload.option, true);
    layout.fitSankey(echarts,chart,payload);
    assert.equal(chart.getModel().getSeriesByIndex(0).get('label.position'),'inside');
  } finally {chart.dispose();}
}
console.log('Vertical Sankey labels, bounds, resize and explicit overrides passed');
