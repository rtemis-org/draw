// SVG geometry for the initial declarative network/map view. This module never
// starts a browser and never embeds a raster snapshot. Shared scene modules
// own the data joins, graph layout, and styling used by the interactive widget.
const themeUtils = require('../htmlwidgets/lib/draw/vector_theme.js');
const esc = themeUtils.escape;
const graphDeps = require('./graph-deps.js');
const graphScene = require('../htmlwidgets/lib/draw/graph_scene.js')(graphDeps);
const mapDeps = require('./map-deps.js');
const mapScene = require('../htmlwidgets/lib/draw/map_scene.js')(mapDeps);
const echarts = require('../htmlwidgets/lib/echarts/echarts.min.js');
const number = n => {
  if (!Number.isFinite(n)) throw new Error('Non-finite vector geometry.');
  return Number(n.toFixed(4));
};
function textWidth(text, font) {
  return echarts.format.getTextRect(String(text), font).width;
}
function frame(x, content) {
  const t = themeUtils.resolve(x.theme);
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${x.width}" height="${x.height}" viewBox="0 0 ${x.width} ${x.height}"><rect width="100%" height="100%" fill="${esc(t.bg)}"/><g font-family="${esc(t.fontFamily)}" font-size="12" fill="${esc(t.fg)}">${content}${x.title ? `<text x="12" y="24" font-size="14" font-weight="600">${esc(x.title)}</text>` : ''}</g></svg>`;
}
// Reuse Sigma v4's normalization and camera matrix, including its graph aspect
// correction and 30px stage padding. Screen-referenced sizes remain pixels.
function graphGeometry(x) {
  const t = themeUtils.resolve(x.theme), s = x.style || {};
  const scene = graphScene.create(x.model || {}, s, t), graph = scene.graph;
  const attrs = graph.mapNodes((id, a) => a);
  const extent = {x:[0,1], y:[0,1]};
  if (attrs.length) for (const axis of ['x','y']) {
    extent[axis] = attrs.reduce(([lo,hi],a)=>[Math.min(lo,a[axis]),Math.max(hi,a[axis])],[Infinity,-Infinity]);
  }
  const normalize = graphDeps.createNormalizationFunction(extent);
  const matrix = graphDeps.matrixFromCamera({x:0.5,y:0.5,angle:0,ratio:1},
    {width:x.width,height:x.height}, {width:extent.x[1]-extent.x[0] || 1,height:extent.y[1]-extent.y[0] || 1}, 30);
  const nodes = graph.mapNodes((id,a) => {
    const p = graphDeps.multiplyVec2(matrix, normalize(a));
    return {id, ...scene.nodeReducer(id,a), x:(1+p.x)*x.width/2, y:(1-p.y)*x.height/2};
  });
  const byId = new Map(nodes.map(n => [n.id,n]));
  const edges = graph.mapEdges((id,a,source,target) => ({id, ...scene.edgeReducer(id,a), source:byId.get(source),target:byId.get(target)}));
  return {nodes, edges};
}
function graphSVG(x) {
  const t = themeUtils.resolve(x.theme), s = x.style || {};
  const scene = graphGeometry(x);
  let content = scene.edges.map(e => {
    const a=e.source,b=e.target;
    // Sigma v4's default loop path is a cubic with a 4-radius bulge,
    // pi/4 direction and 80-degree spread (rendering/edges/paths/loop.ts).
    let d = `M${number(a.x)},${number(a.y)}L${number(b.x)},${number(b.y)}`;
    if (a.id === b.id) {
      const half = 40*Math.PI/180, angle = Math.PI/4;
      const distance = 4*a.size/(0.75*Math.cos(half));
      const control = theta => `${number(a.x+distance*Math.cos(theta))},${number(a.y-distance*Math.sin(theta))}`;
      d = `M${number(a.x)},${number(a.y)}C${control(angle-half)} ${control(angle+half)} ${number(a.x)},${number(a.y)}`;
    }
    return `<path data-source="${esc(a.id)}" data-target="${esc(b.id)}" d="${d}" fill="none" stroke="${esc(e.color)}" stroke-width="${number(e.size)}" opacity="${e.opacity}"/>`;
  }).join('');
  content += scene.nodes.map(n => `<circle data-node-id="${esc(n.id)}" cx="${number(n.x)}" cy="${number(n.y)}" r="${number(n.size)}" fill="${esc(n.color)}" opacity="${n.opacity}"/>`).join('');
  if (s.showLabels !== false) {
    // Prefer labels for larger nodes; prevent collisions and keep text inside
    // the exported page. Browser label culling is camera dependent.
    const occupied=[];
    for (const n of [...scene.nodes].sort((a,b)=>b.size-a.size)) {
      const label=String(n.label), w=textWidth(label,`12px ${t.fontFamily}`);
      const preferred = n.labelPosition === "left" ? n.x-n.size-3-w : n.x+n.size+3;
      const left=Math.max(4, Math.min(x.width-w-4,preferred));
      const top=Math.max(x.title ? 30 : 0, Math.min(x.height-14,n.y-6));
      const box={left,top,right:left+w,bottom:top+14};
      if (occupied.some(b=>box.left<b.right&&box.right>b.left&&box.top<b.bottom&&box.bottom>b.top)) continue;
      occupied.push(box);
      content += `<text x="${number(left)}" y="${number(top+11)}">${esc(label)}</text>`;
    }
  }
  if (!scene.nodes.length) content += `<text x="${x.width/2}" y="${x.height/2}" text-anchor="middle" opacity="0.6">No nodes to display.</text>`;
  return frame(x,content);
}
// MapLibre MercatorTransform.defaultConstrain: the world fills the viewport
// vertically; the camera is clamped at the poles. Longitude world copies repeat.
function mapCamera(x) {
  const geo=x.geo || {}, center=geo.center || [0,20];
  const world=Math.max(x.height,512*2**(geo.zoom ?? 0.4));
  const lat=Math.max(-85.051129,Math.min(85.051129,center[1]));
  const cy=(180-180/Math.PI*Math.log(Math.tan(Math.PI/4+lat*Math.PI/360)))/360;
  return {world, cx:(center[0]+180)/360, cy:Math.max(x.height/2/world,Math.min(1-x.height/2/world,cy))};
}
function mapSVG(x) {
  const t=themeUtils.resolve(x.theme), s=x.style || {};
  const scene=mapScene.create(x,t.dark), camera=mapCamera(x);
  const {world,cx,cy}=camera;
  // d3-geo's spherical winding convention is the reverse of GeoJSON. Reverse
  // entire polygons (including holes) only when the exterior covers > half
  // the sphere; MultiPolygons are normalized polygon by polygon.
  const orient = coordinates => {
    const polygon={type:'Polygon',coordinates};
    return mapDeps.geoArea(polygon)>2*Math.PI ? coordinates.map(r=>[...r].reverse()) : coordinates;
  };
  const features=scene.fc.features.map(f=>({...f,geometry:{...f.geometry,coordinates:
    f.geometry.type==='MultiPolygon' ? f.geometry.coordinates.map(orient) : orient(f.geometry.coordinates)}}));
  let content='<defs><clipPath id="map-viewport"><rect width="'+x.width+'" height="'+x.height+'"/></clipPath></defs><g clip-path="url(#map-viewport)">';
  // Geographic preclipping handles dateline crossings and holes before the
  // SVG viewport clip. Include every world copy intersecting the viewport.
  const start=Math.floor(cx-x.width/2/world), end=Math.floor(cx+x.width/2/world);
  for (let copy=start;copy<=end;copy++) {
    const projection=mapDeps.geoMercator().scale(world/(2*Math.PI))
      .translate([x.width/2+(0.5-cx+copy)*world,x.height/2+(0.5-cy)*world]);
    const path=mapDeps.geoPath(projection).digits(4);
    for (const f of features) {
      const d=path(f); if (!d) continue;
      const id=f.properties.joinId, color=mapScene.color(scene.scale,scene.values.get(id));
      content+=`<path data-region-id="${esc(id)}" d="${d}" fill="${esc(color)}" fill-opacity="${s.opacity ?? 1}" fill-rule="evenodd" stroke="${s.showBoundaries===false?'none':(t.dark?'#3f3f46':'#cbd5e1')}" stroke-width="${s.outlineWidth ?? 0.2}"/>`;
    }
  }
  content+='</g>';
  if (s.showLegend!==false && scene.scale.legend.length) {
    const label=x.model?.valueLabel;
    const entries=[...scene.scale.legend,{label:'No data',color:scene.scale.missingColor}];
    const width=Math.min(x.width-24,Math.max(label?textWidth(label,`500 12px ${t.fontFamily}`):0,...entries.map(e=>textWidth(e.label,`12px ${t.fontFamily}`)+20))+20);
    const height=12+entries.length*16.8+(label?20.8:0)+4;
    const corner=s.legendPosition || 'bottom-right';
    const left=corner.includes('left')?12:x.width-width-12;
    const top=corner.startsWith('top')?12:x.height-height-12;
    content+=`<g transform="translate(${number(left)},${number(top)})"><rect width="${number(width)}" height="${number(height)}" rx="6" fill="${t.dark?'#282828':'#ffffff'}" opacity="0.85"/>`;
    let y=6;
    if(label){content+=`<text x="10" y="${y+12}" font-weight="500">${esc(label)}</text>`;y+=20.8;}
    entries.forEach((e,i)=>{if(i===entries.length-1)y+=4;content+=`<rect x="10" y="${number(y+2)}" width="12" height="12" rx="2" fill="${esc(e.color)}"/><text x="30" y="${number(y+12)}" opacity="0.7">${esc(e.label)}</text>`;y+=16.8;});
    content+='</g>';
  }
  return frame(x,content);
}
module.exports={graphSVG,mapSVG,graphGeometry,mapCamera};
