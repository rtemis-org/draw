// Native geometry checks run through the real browser binding, including
// standalone height and fixed panel/SVG bounds, repeat resize, and toggles.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), directory = path.dirname(input.layout);
const body = {style:{}, classList:{contains:()=>false}};
const context = {document:{body}, window:{}, HTMLWidgets:{widget() {}},
  rtemisPanels: require(input.layout), rtemisA3: require(path.join(directory,'a3.js')),
  rtemisConfusion: require(path.join(directory,'confusion.js')),
  echarts:{...echarts, init:(_,theme,options)=>echarts.init(null,theme,{...options,renderer:'svg',ssr:true})}};
vm.createContext(context);
vm.runInContext(fs.readFileSync(input.binding,'utf8'),context);
for (const dark of [false,true]) for (const longNames of [false,true]) for (const bounded of [false,true]) {
  const payload = structuredClone(input.payload);
  payload.option.animation=false;
  payload.option.backgroundColor=dark?'#181818':'#ffffff';
  if (longNames) {
    const name = 'Microtubule Binding Domain (R1, R3, R4)';
    payload.option.series.find(s => s.name === 'Site').name = name;
    payload.option.legend.data.find(d => d.name === 'Site').name = name;
  }
  const original=JSON.stringify(payload);
  const element={style:{},parentElement:body};
  const widget=context.rtemisDrawFactory(element,1100,550,bounded);
  try {
    widget.renderValue(payload);
    for (const width of (longNames ? [1100,700,1100] : [1100,560,390,1100])) {
      widget.resize(width,550);
      const c=widget.getChart(), model=c.getModel();
      assert.equal(c.getWidth(),width);
      if (bounded) assert.equal(c.getHeight(),550);
      const series=model.getSeries().find(s=>s.name==='Primary structure');
      const data=series.getData(), diameter=series.get('symbolSize');
      assert.equal(series.get(['itemStyle','color']),dark?'#181818':'#efefef');
      assert.equal(series.get(['itemStyle','borderColor']),dark?'#a1a1aa':'#44403C');
      assert.equal(model.getSeries().find(s=>s.name==='Residue labels').get('data')[0].label.color,dark?'#fafafa':'#1C1917');
      for (let i=1;i<data.count();i++) {
        const point = j => series.coordinateSystem.dataToPoint([data.get('x',j),data.get('y',j)]);
        const a=point(i-1),b=point(i);
        assert.ok(Math.hypot(a[0]-b[0],a[1]-b[1]) > diameter, 'Overlapping residues');
      }
      assert.ok(diameter<=payload.a3.markerSize);
      model.eachComponent('legend', legend => {
        assert.equal(legend.get('orient'),'vertical');
        assert.equal(model.findComponents({mainType:'legend'}).length,1);
        const area=model.getComponent('grid').coordinateSystem.getRect();
        const view=c.getViewOfComponentModel(legend),box=view.group.getBoundingRect().clone();
        box.applyTransform(view.group.getComputedTransform());
        assert.ok(box.x >= area.x + area.width + payload.a3.legendGap - 1, 'Legend overlaps sequence');
        assert.ok(Math.abs(box.x + box.width - (width - payload.option.legend.right)) < 1, 'Legend lost right anchor');
        assert.ok(Math.abs(box.y - payload.option.legend.top) < 1, 'Legend lost top anchor');
        assert.ok(box.x>=-1 && box.y>=-1 && box.x+box.width<=width+1 && box.y+box.height<=c.getHeight()+1, JSON.stringify(box));
      });
      c.dispatchAction({type:'legendUnSelect', name:'Domain'});
      widget.resize(width,550);
      assert.equal(c.getModel().getComponent('legend').isSelected('Domain'),false);
      c.dispatchAction({type:'legendSelect', name:'Domain'});
      assert.equal(JSON.stringify(payload),original,'Resize changed source payload');
    }
  } finally {widget.dispose();}
}
console.log('A3 spacing, bounded dimensions, full legends, repeated resize, and selection passed');
