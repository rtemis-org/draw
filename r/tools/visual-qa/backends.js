window.__qaErrors=[];
window.addEventListener('error',e=>window.__qaErrors.push(e.message));
window.backendQA={
  el(){return document.querySelector('.rtemis-graph,.rtemis-map');},
  renderer(){const el=this.el();return el && window.HTMLWidgets?.find('#'+el.id)?.getRenderer?.();},
  isMap(){return this.el()?.classList.contains('rtemis-map');},
  ready(){const r=this.renderer();return !!r && (this.isMap()?r.loaded() && !!r.getSource('regions'):r.getGraph().order>0);},
  scene(){const r=this.renderer(),rect=this.el().getBoundingClientRect();
    if(this.isMap())return {width:rect.width,height:rect.height,count:r.queryRenderedFeatures({layers:['regions-fill']}).length,zoom:r.getZoom(),center:r.getCenter(),errors:window.__qaErrors};
    const nodes={};r.getGraph().forEachNode((id,a)=>{nodes[id]=r.graphToViewport(a);});
    return {width:rect.width,height:rect.height,count:r.getGraph().order,nodes,errors:window.__qaErrors};
  },
  hoverPoint(){const r=this.renderer(),rect=this.el().getBoundingClientRect();let p;
    if(this.isMap()) {
      // Search actual visible geometry, including world/county maps at narrow widths.
      outer:for(let y=80;y<rect.height-80;y+=15)for(let x=40;x<rect.width-180;x+=15){
        const features=r.queryRenderedFeatures([x,y],{layers:['regions-fill']});
        if(features.length){p={x,y};break outer;}
      }
      if(!p)throw new Error('No visible region hover target');
    } else { const graph=r.getGraph();p=r.graphToViewport(graph.getNodeAttributes(graph.nodes()[1])); }
    return {x:rect.x+p.x,y:rect.y+p.y};
  },
  tooltip(){const el=this.el();return [...el.children].filter(e=>e.style.pointerEvents==='none'&&getComputedStyle(e).display!=='none'&&e.innerText).map(e=>e.innerText).filter(t=>!t.includes('Trait network')).join('\n');},
  zoom(){const r=this.renderer();return this.isMap()?r.getZoom():r.getCamera().getState().ratio;}
};
