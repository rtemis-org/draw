const assert=require('node:assert/strict'), path=require('node:path');
const root=process.argv[2];
const echarts=require(path.join(root,'htmlwidgets/lib/echarts/echarts.min.js'));
const layout=require(path.join(root,'htmlwidgets/lib/draw/panels.js'));
for(const roseType of [null,'radius']) {
  const labels=['Adelie','Chinstrap','Gentoo'];
  const payload={legendPosition:'top',legendPlacement:'outside',option:{animation:false,legend:{},series:[{type:'pie',radius:'75%',roseType,label:{alignTo:'edge',edgeDistance:8},data:labels.map((name,i)=>({name,value:[151,68,123][i]}))}]}};
  const chart=echarts.init(null,null,{renderer:'svg',ssr:true,width:390,height:600});
  chart.setOption(payload.option);
  layout.positionLegend(echarts,chart,payload);
  const check=()=>{
    const data=chart.getModel().getSeries()[0].getData();
    for(let i=0;i<data.count();i++) {
      const text=data.getItemGraphicEl(i).getTextContent();
      const spans=text.childrenRef().filter(c=>c.type==='tspan');
      assert(spans.some(c=>c.style.text===labels[i]), labels[i]+' remains complete');
      const rect=text.getBoundingRect().clone();rect.applyTransform(text.getComputedTransform());
      assert(rect.x>=0 && rect.x+rect.width<=chart.getWidth());
    }
  };
  check(); const small=chart.getModel().getSeries()[0].getData().getLayout('r');
  chart.resize({width:1100,height:600});layout.positionLegend(echarts,chart,payload);check();
  assert(chart.getModel().getSeries()[0].getData().getLayout('r')>small);
  chart.dispose();
}
console.log('Pie labels fit both widths');
