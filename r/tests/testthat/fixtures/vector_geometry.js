const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const root = process.argv[2];
const v = require(path.join(root, 'node/vector.js'));
const builders = require(path.join(root, 'htmlwidgets/lib/draw/rtemis-draw-builders.js'));
const graphDeps = require(path.join(root, 'node/graph-deps.js'));
const mapDeps = require(path.join(root, 'node/map-deps.js'));
const theme = require(path.join(root, 'htmlwidgets/lib/draw/vector_theme.js'));
assert.equal(theme.isDark('#181818'), true);
assert.equal(theme.isDark('#fff'), false);
assert.equal(theme.escape('<A & "B">'), '&lt;A &amp; &quot;B&quot;&gt;');
const model = {nodes:[{id:'A',value:2},{id:'B',value:1},{id:'C',value:3}],edges:[{source:'A',target:'B',weight:1,sign:1},{source:'B',target:'C',weight:0.5,sign:-1}]};
for (const layout of ['force','circular','circlepack','random']) {
  const x={model,config:{layout,color_by_group:true,palette:['#123456','#abcdef']},width:800,height:600};
  const a=v.graphGeometry(x), b=v.graphGeometry(x);
  assert.deepEqual(a,b,layout+' is deterministic');
  assert.equal(a.nodes.length,3); assert.equal(a.edges.length,2);
  assert(a.nodes.every(n=>Number.isFinite(n.x)&&Number.isFinite(n.y)&&n.size>0));
  const g=builders.buildNetworkGraph(model,x.config,graphDeps);
  const n=builders.networkReducers(g,x.config).nodeReducer('A',g.getNodeAttributes('A'));
  assert.equal(n.labelPosition,a.nodes[0].labelPosition); assert.equal(n.size,a.nodes[0].size); assert.equal(n.color,a.nodes[0].color);
  assert(!/<image|NaN|Infinity/.test(v.graphSVG(x)));
}
const loop = v.graphSVG({model:{nodes:[{id:'A'}],edges:[{source:'A',target:'A'}]},config:{},width:800,height:600});
assert.match(loop, /d="M[^"]+C/);
for (const [raw,res,id] of [['CA','state','06'],['California','state','06'],[1001,'county','01001'],['US','country','USA']]) assert.equal(builders.normalizeLocation(raw,res),id);
for (const classification of ['quantile','equal','jenks']) {
  const scale=builders.buildChoroplethScale([1,2,3,4,5,6,7,8,9,10,null],{classification,colormap:'viridis',num_classes:3},{},mapDeps.chromatic);
  assert.equal(builders.choroplethColor(scale,null),scale.missingColor);
  scale.thresholds.forEach((t,i)=>assert.equal(builders.choroplethColor(scale,t),scale.colors[i+1]));
}
for (const [resolution,file,object] of [['country','countries.topo.json','countries'],['state','us-10m.topo.json','states'],['county','us-10m.topo.json','counties']]) {
  const geo={topojson:fs.readFileSync(path.join(root,'htmlwidgets/lib/geo',file),'utf8'),object,center:[0,20],zoom:0.4};
  const x={geo,model:{resolution,rows:[{location:resolution==='country'?'US':resolution==='state'?'CA':'01001',value:2}],valueLabel:'Rate'},config:{},width:800,height:600};
  const regions=builders.mapFeatures(JSON.parse(geo.topojson),resolution,mapDeps.topojsonFeature);
  assert.equal(builders.joinChoropleth(x.model,regions.ids).matched,1);
  assert(regions.features.features.length>40);
  const svg=v.mapSVG(x);
  assert(svg.includes('data-region-id=')); assert(!/<image|NaN|Infinity/.test(svg));
}
const camera=v.mapCamera({height:800,geo:{center:[0,20],zoom:0.4}});
assert.equal(camera.world,800); assert.equal(camera.cy,0.5);
console.log('Vector contracts passed');
