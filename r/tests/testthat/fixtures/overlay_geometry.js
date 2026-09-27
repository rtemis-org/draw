// Inspect actual native vector marks after layout and resizing.
const fs=require('node:fs'),assert=require('node:assert/strict');
const input=JSON.parse(fs.readFileSync(process.argv[2],'utf8'));
const echarts=require(input.echarts);require(input.renderers)(echarts);
const near=(a,b)=>assert.ok(Math.abs(a-b)<1e-6,`${a} != ${b}`);
let count=0;
for(const item of input.charts) {
 const chart=echarts.init(null,item.theme,{renderer:'svg',ssr:true,width:800,height:600});
 item.option.animation=false;chart.setOption(item.option);
 try {for(const width of [800,390]) {
  chart.resize({width,height:600});
  const model=chart.getModel();
  for(const series of model.getSeries()) {
   const data=series.getData(),renderer=series.get('renderItem');
   const pixel=p=>chart.convertToPixel({xAxisIndex:series.get('xAxisIndex')||0,yAxisIndex:series.get('yAxisIndex')||0},p);
   if(renderer==='rtemis.ribbon.v1') {
    const polygon=data.getItemGraphicEl(0);
    assert.equal(polygon.type,'polygon');
    series.get('itemPayload').vertices.forEach((p,i)=>{
     const xy=pixel(p);near(polygon.shape.points[i][0],xy[0]);near(polygon.shape.points[i][1],xy[1]);
    });
   } else if(renderer==='rtemis.rug.v1') {
    const rect=model.getComponent('grid').coordinateSystem.getRect();
    for(let i=0;i<data.count();i++) {
     const p=pixel(data.getRawDataItem(i)),marks=data.getItemGraphicEl(i).children();
     near(marks[0].shape.x1,p[0]);near(marks[0].shape.y1,rect.y+rect.height);
     near(marks[1].shape.x1,rect.x);near(marks[1].shape.y1,p[1]);
    }
   } else if(renderer==='rtemis.axis_labels.v1') {
    for(let i=0;i<data.count();i++) {
     const p=data.getRawDataItem(i),mark=data.getItemGraphicEl(i);
     near(mark.style.x,pixel([p[0],0])[0]);assert.equal(mark.style.text,p[1]);
    }
   }
  }
  const svg=chart.renderToSVGString();
  assert.ok(!svg.includes('<image'));assert.ok(!svg.includes('NaN'));
  for(const text of item.labels)assert.ok(svg.includes(text),`Missing label ${text}`);
  count++;
 }} finally {chart.dispose();}
}
console.log(`${count} overlay geometry cases passed`);
