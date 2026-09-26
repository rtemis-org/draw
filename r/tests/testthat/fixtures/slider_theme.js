// Verify theme inheritance in the bundled renderer, including explicit overrides.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const payload = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(payload.echarts);
for (const theme of payload.themes) {
  const chart = echarts.init(null, theme, {renderer:'svg', ssr:true, width:700, height:450});
  try {
    chart.setOption({animation:false, xAxis:{type:'value'}, yAxis:{type:'value'},
      series:[{type:'line', data:[[0,1],[1,3],[2,2],[3,4]]}],
      dataZoom:[{type:'slider', start:20, end:80}]});
    const zoom = chart.getModel().getComponent('dataZoom').option;
    const paths = ['backgroundColor', 'borderColor', 'fillerColor',
      'dataBackground.lineStyle.color', 'dataBackground.areaStyle.color',
      'selectedDataBackground.lineStyle.color', 'selectedDataBackground.areaStyle.color',
      'handleStyle.color', 'handleStyle.borderColor', 'moveHandleStyle.color',
      'brushStyle.color', 'emphasis.handleStyle.borderColor', 'emphasis.moveHandleStyle.color'];
    for (const path of paths) {
      const get = x => path.split('.').reduce((a,k)=>a[k],x);
      assert.equal(get(zoom), get(theme.dataZoom), path);
      const rgba = echarts.color.parse(get(zoom));
      assert.equal(rgba[0],rgba[1],path);
      assert.equal(rgba[1],rgba[2],path);
    }
    const svg = chart.renderToSVGString();
    assert.ok(svg.includes('rgb(128,128,128)'));
    assert.ok(!svg.includes('<image'));
    chart.setOption({dataZoom:[{fillerColor:'#ff0000',handleStyle:{borderColor:'#00ff00'}}]});
    const changed = chart.getModel().getComponent('dataZoom');
    assert.equal(changed.get('fillerColor'),'#ff0000');
    assert.equal(changed.get(['handleStyle','borderColor']),'#00ff00');
  } finally {chart.dispose();}
}
console.log('Neutral slider themes and explicit overrides passed');
