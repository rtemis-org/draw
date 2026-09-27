// Inspect native custom track and tree marks against the final heatmap grid.
const fs=require('node:fs'), assert=require('node:assert/strict');
const input=JSON.parse(fs.readFileSync(process.argv[2],'utf8'));
const echarts=require(input.echarts);require(input.renderers)(echarts);
const near=(a,b)=>assert.ok(Math.abs(a-b)<1e-6,`${a} != ${b}`);
let count=0;
for(const item of input.charts) {
 const chart=echarts.init(null,item.theme,{renderer:'svg',ssr:true,width:800,height:600});
 require(input.panels).prepareLabels(item);
 item.option.animation=false;chart.setOption(item.option);
 try {for(const width of [800,390]) {
  chart.resize({width,height:600});
  const model=chart.getModel(),heatmap=model.getSeries().find(s=>s.subType==='heatmap');
  assert.deepEqual(model.getComponent('visualMap').getTargetSeriesIndices(),[heatmap.seriesIndex]);
  for(const series of model.getSeries()) {
   const data=series.getData(),payload=series.get('itemPayload'),renderer=series.get('renderItem');
   if(renderer==='rtemis.heatmap_tracks.v1') {
    const rect=series.coordinateSystem.getArea(),row=payload.orientation==='row';
    for(let i=0;i<data.count();i++) {
     const center=series.coordinateSystem.dataToPoint(row?[0,i]:[i,0]);
     const children=data.getItemGraphicEl(i).children();
     assert.equal(children.length,payload.colors[i].length);
     children.forEach((mark,j)=>{
      assert.equal(mark.type,'rect');assert.equal(mark.style.fill,payload.colors[i][j]);
      if(row) {
       near(mark.shape.y+mark.shape.height/2,center[1]);near(mark.shape.x+mark.shape.width,rect.x-6-j*12);
      } else {
       near(mark.shape.x+mark.shape.width/2,center[0]);
       near(payload.top?mark.shape.y+mark.shape.height:mark.shape.y,
            payload.top?rect.y-6-j*12:rect.y+rect.height+4+j*12);
      }
     });
    }
   } else if(renderer==='rtemis.dendrogram.v1') {
    for(let i=0;i<data.count();i++) {
     const p=data.getRawDataItem(i),mark=data.getItemGraphicEl(i);
     const points=payload.orientation==='row'
       ? [[p[2],p[0]],[p[4],p[0]],[p[4],p[1]],[p[3],p[1]]]
       : [[p[0],p[2]],[p[0],p[4]],[p[1],p[4]],[p[1],p[3]]];
     points.forEach((p,j)=>{const xy=series.coordinateSystem.dataToPoint(p);
       near(mark.shape.points[j][0],xy[0]);near(mark.shape.points[j][1],xy[1]);});
     assert.equal(mark.style.stroke,payload.colors[i]);
    }
   }
  }
  for(let i=0;i<heatmap.getData().count();i++) {
    const mark=heatmap.getData().getItemGraphicEl(i),label=mark.getTextContent();
    assert.ok(label.getBoundingRect().width <= mark.shape.width+1);
  }
  const svg=chart.renderToSVGString();
  if(width===800)assert.ok(svg.includes('note {b}'));assert.ok(!svg.includes('<image'));assert.ok(!svg.includes('NaN'));
  count++;
 }}finally{chart.dispose();}
}
console.log(`${count} annotated heatmap geometry cases passed`);
