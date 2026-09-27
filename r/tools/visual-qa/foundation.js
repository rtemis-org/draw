// Native chart observations for the installed-package foundation QA script.
// Interaction is sent through Chrome's pointer API, not dispatchAction().
window.foundationQA = {
  charts() {
    const el = document.querySelector('.rtemis-draw, .rtemis-panels');
    return el && HTMLWidgets.find('#' + el.id)?.getCharts?.() || [];
  },
  chart() { return this.charts()[0]; },
  bounds(el) {
    const rect = el.getBoundingRect().clone();
    rect.applyTransform(el.getComputedTransform());
    return {x: rect.x, y: rect.y, width: rect.width, height: rect.height};
  },
  pagePoint(x, y) {
    const rect = this.chart().getDom().getBoundingClientRect();
    return {x: rect.x + x, y: rect.y + y};
  },
  legend() {
    const c = this.chart();
    const model = c.getModel().getComponent('legend');
    if (!model || !model.get('show') || !model.getData().length) return null;
    const group = c.getViewOfComponentModel(model).getContentGroup().children()
      .find(g => g.__legendDataIndex === 0);
    const rect = this.bounds(group);
    return {name: model.getData()[0].get('name'),
      point: this.pagePoint(rect.x + rect.width / 2, rect.y + rect.height / 2)};
  },
  selection(name) {
    const c = this.chart();
    const pies = c.getModel().getSeries().filter(s => s.subType === 'pie');
    return {selected: c.getModel().getComponent('legend').isSelected(name),
      layers: pies.length ? pies.map(s => ({type: 'pie', filtered: s.getData().indexOfName(name) < 0})) :
        c.getModel().getSeriesByName(name).map(s => ({
          type: s.subType, filtered: c.getModel().isSeriesFiltered(s)
        }))};
  },
  hoverPoint() {
    const c = this.chart();
    const series = c.getModel().getSeries().find(s =>
      !s.get('silent') && ['scatter', 'bar', 'line', 'boxplot', 'pie', 'sankey', 'heatmap', 'custom'].includes(s.subType));
    const data = series.getData();
    let index = Math.floor(data.count() / 2);
    // Empty histogram bins have no hoverable area; choose an observed bin.
    if (series.option.renderItem === 'rtemis.histogram.v1') {
      index = Array.from({length: data.count()}, (_, i) => i)
        .find(i => data.getRawDataItem(i)[3] > 0);
    }
    let point;
    if (series.subType === 'pie') {
      const sector = data.getItemLayout(index);
      const angle = (sector.startAngle + sector.endAngle) / 2;
      const radius = (sector.r + sector.r0) / 2;
      point = [sector.cx + radius * Math.cos(angle), sector.cy + radius * Math.sin(angle)];
    } else if (['bar', 'boxplot', 'sankey', 'custom'].includes(series.subType)) {
      const rect = this.bounds(data.getItemGraphicEl(index));
      point = [rect.x + rect.width / 2, rect.y + rect.height / 2];
    } else {
      point = series.coordinateSystem.dataToPoint([
        data.get(data.mapDimension('x'), index),
        data.get(data.mapDimension('y'), index)
      ]);
    }
    return this.pagePoint(...point);
  },
  tooltip() {
    const view = this.chart().getViewOfComponentModel(
      this.chart().getModel().getComponent('tooltip'));
    const el = view?._tooltipContent?.el;
    return el && getComputedStyle(el).visibility !== 'hidden' &&
      Number(getComputedStyle(el).opacity) > 0 ? el.innerText : '';
  },
  // Calibration uses centered line symbols. Check actual rendered positions
  // after host resizing, which can schedule an update after animation.isFinished.
  linePointsAligned() {
    return this.chart().getModel().getSeries().filter(s =>
      s.subType === 'line' && !s.get('silent')).every(series => {
      const data = series.getData();
      for (let i = 0; i < data.count(); i++) {
        const el = data.getItemGraphicEl(i);
        if (!el) return false;
        const rect = this.bounds(el);
        const point = series.coordinateSystem.dataToPoint([
          data.get(data.mapDimension('x'), i), data.get(data.mapDimension('y'), i)
        ]);
        if (Math.abs(rect.x + rect.width / 2 - point[0]) > 0.1 ||
            Math.abs(rect.y + rect.height / 2 - point[1]) > 0.1) return false;
      }
      return true;
    });
  },
  center() {
    const rect = this.chart().getModel().getComponent('grid').coordinateSystem.getRect();
    return this.pagePoint(rect.x + rect.width / 2, rect.y + rect.height / 2);
  },
  zoom() {
    return this.chart().getModel().findComponents({mainType: 'dataZoom'})
      .map(m => m.getPercentRange());
  },
  heatmapTracksAligned() {
    return this.chart().getModel().getSeries().filter(s=>s.get('renderItem')==='rtemis.heatmap_tracks.v1').every(s=>{
      const data=s.getData(),row=s.get('itemPayload').orientation==='row';
      for(let i=0;i<data.count();i++) {
        const center=s.coordinateSystem.dataToPoint(row?[0,i]:[i,0]);
        for(const mark of data.getItemGraphicEl(i).children()) {
          const rect=this.bounds(mark),value=row?rect.y+rect.height/2:rect.x+rect.width/2;
          if(Math.abs(value-center[row?1:0])>.1)return false;
        }
      }
      return true;
    });
  },
  scene() {
    const c = this.chart();
    const grid = c.getModel().getSeries().find(s=>s.subType==='heatmap')?.coordinateSystem.getArea() || c.getModel().getComponent('grid')?.coordinateSystem.getRect() || {x:0,y:0,width:c.getWidth(),height:c.getHeight()};
    const text = c.getZr().storage.getDisplayList(true)
      .filter(el => el.type === 'tspan' && !el.ignore && el.style.text)
      .map(el => ({text: el.style.text, ...this.bounds(el)}));
    return {width: c.getWidth(), height: c.getHeight(),
      background: c.getModel().get('backgroundColor'),
      series: c.getModel().getSeries().map(s => ({name: s.name, type: s.subType,
        count: s.getData().count()})),
      grid: {x: grid.x, y: grid.y, width: grid.width, height: grid.height},
      text, errors: window.__qaErrors};
  }
};
