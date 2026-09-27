// Orthographic projection matching ECharts-GL 2.1 Cartesian3D/OrbitControl.
// The browser uses GL; Node writes vector marks without a browser or raster.
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.rtemisScatter3D = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  const esc = value => String(value).replace(/[&<>"']/g, c =>
    ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&apos;'}[c]));
  const fallback = ['#6ca3a0','#ef8a00','#bf1f59','#4078a6','#80558c'];
  const extent = (grid,width) => grid.rtemisViewSize * (width < 500 ? 2.2 : 1);
  function prepare(option, theme, width, height) {
    if (!option.grid3D) return;
    const grid = option.grid3D;
    // Match the same viewing volume at wide/narrow sizes. GL adds one to the
    // authored orthographicSize, per OrbitControl's baseOrthoSize default.
    const left = 12;
    const w = Math.max(1, width - left - 12), h = Math.max(1, height - 80);
    grid.left = left; grid.width = w; grid.height = h;
    grid.viewControl.orthographicSize = extent(grid,width) * Math.max(1, h / w) - 1;
    const color = theme?.textStyle?.color || '#333333';
    const captions=[];
    for (const [i,key] of ['xAxis3D','yAxis3D','zAxis3D'].entries()) {
      const axis=option[key], dimension=['x','y','z'][i];
      axis.rtemisAxisName=axis.rtemisAxisName || axis.name;
      axis.name=width<500?dimension:axis.rtemisAxisName;
      captions.push({id:'rtemis3d-'+dimension+'-name',type:'text',left:12,bottom:46-i*16,
        silent:true,invisible:width>=500,style:{text:dimension+': '+axis.rtemisAxisName,
          fill:color,font:'11px '+(theme?.textStyle?.fontFamily || 'sans-serif')}});
      option[key].axisLabel.color = color;
      option[key].axisLabel.formatter = v => Number(Number(v).toPrecision(4));
      option[key].axisLabel.fontSize = 11;
      option[key].axisLabel.fontFamily = theme?.textStyle?.fontFamily || "sans-serif";
      option[key].nameTextStyle.fontFamily = theme?.textStyle?.fontFamily || "sans-serif";
      option[key].nameTextStyle.fontSize = width < 500 ? 11 : 12;
      option[key].nameGap = width < 500 ? (key==='zAxis3D'?50:35) : 25;
      option[key].interval=(option[key].max-option[key].min)/(width<500?2:4);
      option[key].nameTextStyle.color = color;
    }
    const graphics=option.graphic ? (Array.isArray(option.graphic)?option.graphic:[option.graphic]) : [];
    option.graphic=graphics.filter(g=>!String(g.id||'').startsWith('rtemis3d-')).concat(captions);
    const colors = option.color || theme?.color || fallback;
    option.series.forEach((s,i) => {s.itemStyle.color = s.itemStyle.color || colors[i % colors.length];});
  }
  function geometry(option, width, height) {
    const grid = option.grid3D, vc = grid.viewControl;
    if(option.series.some(s=>s.type!=='scatter3D'))
      throw new Error('3D SVG currently supports scatter3D point series only.');
    if (vc.projection !== 'orthographic') throw new Error('3D SVG requires the supported orthographic camera.');
    const left = 12;
    const w = Math.max(1, width - left - 12), h = Math.max(1, height - 80);
    const scale = Math.min(w,h) / extent(grid,width);
    const a = vc.alpha * Math.PI / 180, b = vc.beta * Math.PI / 180;
    const sa = Math.sin(a), ca = Math.cos(a), sb = Math.sin(b), cb = Math.cos(b);
    const axes = [option.xAxis3D, option.yAxis3D, option.zAxis3D];
    // Cartesian3D maps data y to reversed world z, and data z to world y.
    const projectWorld = p => {
      const X = p[0], Y = p[1], Z = p[2];
      return [left+w/2 + scale*(cb*X-sb*Z),
        60+h/2 - scale*(-sa*sb*X+ca*Y-sa*cb*Z),
        ca*sb*X+sa*Y+ca*cb*Z];
    };
    const project = values => {
      const scaled = values.map((v,i) => 100*((v-axes[i].min)/(axes[i].max-axes[i].min)-.5));
      return projectWorld([scaled[0],scaled[2],-scaled[1]]);
    };
    const points = [];
    option.series.forEach((s,si) => {
      if (option.legend?.selected?.[s.name] === false) return;
      s.data.forEach((p,i) => points.push({point:project(p), value:p, series:si, index:i,
        color:s.itemStyle.color, opacity:s.itemStyle.opacity, size:s.symbolSize}));
    });
    points.sort((a,b)=>a.point[2]-b.point[2]);
    return {project, projectWorld, points, axes};
  }
  function svg(payload, width, height) {
    const option = payload.option, theme = payload.theme || {};
    prepare(option, theme, width, height);
    const g = geometry(option,width,height);
    const bg = option.backgroundColor || theme.backgroundColor || '#ffffff';
    const fg = theme.textStyle?.color || '#333333';
    const family = theme.textStyle?.fontFamily || 'sans-serif';
    const f = n => {if (!Number.isFinite(n)) throw new Error('Non-finite 3D SVG coordinate'); return Number(n.toFixed(5));};
    let out = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="100%" height="100%" fill="${esc(bg)}"/><g font-family="${esc(family)}" font-size="12" fill="${esc(fg)}">`;
    const path = (a,b,color='#888888') => `<path d="M${f(a[0])},${f(a[1])}L${f(b[0])},${f(b[1])}" fill="none" stroke="${esc(color)}" stroke-opacity=".55"/>`;
    // Full wireframe makes the three data dimensions readable from every view.
    for (let dim=0;dim<3;dim++) for (const u of [-50,50]) for (const v of [-50,50]) {
      const a=[u,v,0], b=[u,v,0]; a.splice(dim,0,-50); b.splice(dim,0,50); a.length=3;b.length=3;
      out += path(g.projectWorld(a),g.projectWorld(b));
    }
    // Label outer projected edges, rather than drawing all three scales from
    // the same corner inside the point cloud. Edge choice follows the camera.
    const center=g.projectWorld([0,0,0]);
    g.axes.forEach((axis,dim) => {
      const other=[0,1,2].filter(d=>d!==dim), candidates=[];
      for (const u of [0,1]) for (const v of [0,1]) {
        const start=g.axes.map(a=>a.min);
        start[other[0]]=g.axes[other[0]][u?'max':'min'];
        start[other[1]]=g.axes[other[1]][v?'max':'min'];
        const end=start.slice();end[dim]=axis.max;
        const a=g.project(start), b=g.project(end);
        const mid=[(a[0]+b[0])/2,(a[1]+b[1])/2];
        const score=dim===2 ? -mid[0] : mid[1];
        candidates.push({start,end,a,b,mid,score});
      }
      candidates.sort((a,b)=>b.score-a.score);
      const edge=candidates[0], dx=edge.b[0]-edge.a[0],dy=edge.b[1]-edge.a[1];
      const length=Math.hypot(dx,dy);
      // A dimension viewed end-on has no readable projected scale.
      if(length<12) return;
      let normal=[-dy/length,dx/length];
      if(normal[0]*(edge.mid[0]-center[0])+normal[1]*(edge.mid[1]-center[1])<0)
        normal=normal.map(v=>-v);
      const anchor=normal[0]<-.5?'end':normal[0]>.5?'start':'middle';
      // Interior ticks avoid duplicate endpoint labels at shared corners.
      for(let k=1;k<4;k++) {
        const value=axis.min+(axis.max-axis.min)*k/4,p=edge.start.slice();p[dim]=value;
        const xy=g.project(p);
        out+=`<text x="${f(xy[0]+normal[0]*9)}" y="${f(xy[1]+normal[1]*9+4)}" text-anchor="${anchor}">${esc(Number(value.toPrecision(4)))}</text>`;
      }
      const name=[edge.mid[0]+normal[0]*48,edge.mid[1]+normal[1]*48];
      // Keep long axis names inside the canvas without placing them over data.
      const nameWidth=dim===2?12:String(axis.rtemisAxisName||axis.name).length*6.5;
      name[0]=Math.max(nameWidth/2+6,Math.min(width-nameWidth/2-6,name[0]));
      out+=`<text x="${f(name[0])}" y="${f(name[1]+4)}" text-anchor="middle" font-weight="600"${dim===2?` transform="rotate(-90 ${f(name[0])} ${f(name[1]+4)})"`:""}>${esc(axis.rtemisAxisName||axis.name)}</text>`;
    });
    for (const p of g.points) {
      out += `<circle data-series="${p.series}" data-index="${p.index}" cx="${f(p.point[0])}" cy="${f(p.point[1])}" r="${f(p.size/2)}" fill="${esc(p.color)}" fill-opacity="${p.opacity}"/>`;
    }
    if (option.title?.text) out += `<text x="12" y="20" font-size="16">${esc(option.title.text)}</text>`;
    if (option.legend && option.legend.show !== false) {
      let x=12,y=option.title?.text?42:20;
      option.series.forEach(s=>{
        const len=28+7*String(s.name).length;
        if (x+len>width-12) {x=12;y+=18;}
        out+=`<rect x="${x}" y="${y-9}" width="15" height="10" rx="2" fill="${esc(s.itemStyle.color)}"/><text x="${x+20}" y="${y}">${esc(s.name)}</text>`;
        x+=len;
      });
    }
    return out+'</g></svg>';
  }
  return {prepare,geometry,svg};
});
