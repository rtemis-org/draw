// Measured native scene geometry for the targeted release QA harness.
window.targetedQA = {
  charts() {
    const el = document.querySelector('.rtemis-draw, .rtemis-panels');
    return el && HTMLWidgets.find('#' + el.id)?.getCharts?.() || [];
  },
  ready() {
    const cs = this.charts();
    return cs.length > 0 && cs.every(c => c.getZr().storage.getDisplayList(true).length && c.getZr().animation.isFinished());
  },
  bounds(el) {
    const r = el.getBoundingRect().clone();
    r.applyTransform(el.getComputedTransform());
    return {x:r.x,y:r.y,width:r.width,height:r.height};
  },
  scene() {
    return {errors: window.qaErrors, charts: this.charts().map(c => ({
      width:c.getWidth(),height:c.getHeight(),background:c.getModel().get('backgroundColor'),
      text:c.getZr().storage.getDisplayList(true).filter(e=>e.type==='tspan'&&!e.ignore&&e.style.text).map(e=>({text:e.style.text,...this.bounds(e)})),
      legends:c.getModel().findComponents({mainType:'legend'}).map(m=>this.bounds(c.getViewOfComponentModel(m).group)),
      series:c.getModel().getSeries().map(s=>({name:s.name,count:s.getData().count()}))
    }))};
  },
  legend() {
    const c=this.charts()[0],m=c.getModel().getComponent('legend');
    if(!m||!m.get('show'))return null;
    const data=m.getData(),i=data.findIndex(d=>!d.get('name').startsWith('{'));
    if(i<0)return null;
    const g=c.getViewOfComponentModel(m).getContentGroup().children().find(g=>g.__legendDataIndex===i);
    if(!g)return null;
    this.legendName=data[i].get('name');
    const r=this.bounds(g),dom=c.getDom().getBoundingClientRect();
    return {x:dom.x+r.x+r.width/2,y:dom.y+r.y+r.height/2};
  },
  a3Point() {
    const c=this.charts()[0],s=c.getModel().getSeries().find(s=>s.name==='Primary structure');
    if(!s)return null;
    const d=s.getData(),i=Math.floor(d.count()/2),p=s.coordinateSystem.dataToPoint([d.get('x',i),d.get('y',i)]),r=c.getDom().getBoundingClientRect();
    return {x:p[0]+r.x,y:p[1]+r.y};
  },
  zoom(){return this.charts()[0].getModel().findComponents({mainType:'dataZoom'}).map(m=>m.getPercentRange());},
  tooltip(){const c=this.charts()[0],v=c.getViewOfComponentModel(c.getModel().getComponent('tooltip'))?._tooltipContent?.el;
    return v&&getComputedStyle(v).visibility!=='hidden'&&Number(getComputedStyle(v).opacity)>0?v.innerText:'';},
  selected(){return this.charts()[0].getModel().getComponent('legend').isSelected(this.legendName);}
};
