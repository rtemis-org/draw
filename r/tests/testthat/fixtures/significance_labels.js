const assert=require('node:assert/strict'), path=require('node:path'), fs=require('node:fs');
const root=process.argv[2];
const echarts=require(path.join(root,'htmlwidgets/lib/echarts/echarts.min.js'));
const layout=require(path.join(root,'htmlwidgets/lib/draw/panels.js'));
const options=JSON.parse(fs.readFileSync(process.argv[3],'utf8'));
const overlaps=(a,b)=>a.x<b.x+b.width&&b.x<a.x+a.width&&a.y<b.y+b.height&&b.y<a.y+a.height;
const textRect=el=>{const r=el.getBoundingRect().clone();r.applyTransform(el.getComputedTransform());return r;};
// Annotations of a dense volcano clear each other on every layout pass.
{
  const payload={option:options.volcano};
  layout.prepareLabels(payload);
  const chart=echarts.init(null,null,{renderer:'svg',ssr:true,width:640,height:480});
  chart.setOption(payload.option);
  const check=()=>{
    const rects=[];
    chart.getModel().getSeries().forEach(series=>{
      const data=series.getData();
      for(let i=0;i<data.count();i++){
        // Scatter labels hang off the symbol path inside each symbol group.
        const symbol=data.getItemGraphicEl(i);
        const text=symbol?.getSymbolPath().getTextContent();
        if(text&&!text.ignore&&text.style.text) rects.push(textRect(text));
      }
    });
    assert(rects.length>=8, 'every annotation renders');
    rects.forEach((a,i)=>rects.slice(i+1).forEach(b=>assert(!overlaps(a,b),'annotations overlap')));
    return JSON.stringify(rects.map(r=>[r.x,r.y].map(Math.round)));
  };
  const first=check();
  chart.resize({width:560,height:420});check();
  // Each pass starts clear: labels must not avoid the previous pass's own
  // positions, so the same size reproduces the same placement.
  chart.resize({width:640,height:480});
  assert.equal(check(), first, 'placement is reproducible across passes');
  chart.dispose();
}
// The threshold label sits outside the plotting grid, inside the canvas.
for(const view of ['volcano','manhattan']){
  const payload={option:options[view]};
  layout.prepareLabels(payload);
  const chart=echarts.init(null,null,{renderer:'svg',ssr:true,width:640,height:480});
  chart.setOption(payload.option);
  const grid=chart.getModel().getComponent('grid').coordinateSystem.getRect();
  let found=false;
  chart.getZr().storage.getDisplayList(true).forEach(el=>{
    // Rendered text is drawn as tspan children of the label element.
    if(!/^p < /.test(el.style?.text||'')) return;
    const r=textRect(el); found=true;
    assert(r.x>=grid.x+grid.width, view+' threshold label clears the grid');
    assert(r.x+r.width<=chart.getWidth(), view+' threshold label fits the canvas');
  });
  assert(found, view+' threshold label renders');
  chart.dispose();
}
console.log('Significance labels clear marks and each other');
process.exit(0);
