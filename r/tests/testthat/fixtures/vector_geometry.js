const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const root = process.argv[2];
const v = require(path.join(root, 'node/vector.js'));
const graph = require(path.join(root, 'htmlwidgets/lib/draw/graph_scene.js'))(require(path.join(root, 'node/graph-deps.js')));
const map = require(path.join(root, 'htmlwidgets/lib/draw/map_scene.js'))(require(path.join(root, 'node/map-deps.js')));
const theme = require(path.join(root, 'htmlwidgets/lib/draw/vector_theme.js'));
assert.equal(theme.isDark('#181818'), true);
assert.equal(theme.isDark('#fff'), false);
assert.equal(theme.escape('<A & "B">'), '&lt;A &amp; &quot;B&quot;&gt;');
const model = {nodes:[{id:'A',value:2},{id:'B',value:1},{id:'C',value:3}],edges:[{source:'A',target:'B',weight:1,sign:1},{source:'B',target:'C',weight:0.5,sign:-1}]};
for (const layout of ['force','circular','circlepack','random']) {
  const x={model,style:{layout,colorByGroup:true,palette:['#123456','#abcdef']},width:800,height:600};
  const a=v.graphGeometry(x), b=v.graphGeometry(x);
  assert.deepEqual(a,b,layout+' is deterministic');
  assert.equal(a.nodes.length,3); assert.equal(a.edges.length,2);
  assert(a.nodes.every(n=>Number.isFinite(n.x)&&Number.isFinite(n.y)&&n.size>0));
  const scene=graph.create(model,x.style,theme.resolve(null));
  const n=scene.nodeReducer('A',scene.graph.getNodeAttributes('A'));
  assert.equal(n.labelPosition,a.nodes[0].labelPosition); assert.equal(n.size,a.nodes[0].size); assert.equal(n.color,a.nodes[0].color);
  assert(!/<image|NaN|Infinity/.test(v.graphSVG(x)));
}
const loop = v.graphSVG({model:{nodes:[{id:'A'}],edges:[{source:'A',target:'A'}]},style:{},width:800,height:600});
assert.match(loop, /d="M[^"]+C/);
for (const [raw,res,id] of [['CA','state','06'],['California','state','06'],[1001,'county','01001'],['US','country','USA']]) assert.equal(map.normalizeKey(raw,res),id);
for (const classification of ['quantile','equal','jenks']) {
  const scale=map.buildScale([1,2,3,4,5,6,7,8,9,10,null],{classification,scheme:'viridis',classes:3,dark:false});
  assert.equal(map.color(scale,null),scale.missingColor);
  scale.thresholds.forEach((t,i)=>assert.equal(map.color(scale,t),scale.colors[i+1]));
}
for (const [resolution,file,object] of [['country','countries.topo.json','countries'],['state','us-10m.topo.json','states'],['county','us-10m.topo.json','counties']]) {
  const geo={topojson:fs.readFileSync(path.join(root,'htmlwidgets/lib/geo',file),'utf8'),object,center:[0,20],zoom:0.4};
  const x={geo,model:{resolution,rows:[{location:resolution==='country'?'US':resolution==='state'?'CA':'01001',value:2}],valueLabel:'Rate'},style:{},width:800,height:600};
  const scene=map.create(x,false);
  assert.equal(scene.report.matched,1);
  assert(scene.fc.features.length>40);
  const svg=v.mapSVG(x);
  assert(svg.includes('data-region-id=')); assert(!/<image|NaN|Infinity/.test(svg));
}
const camera=v.mapCamera({height:800,geo:{center:[0,20],zoom:0.4}});
assert.equal(camera.world,800); assert.equal(camera.cy,0.5);
console.log('Vector contracts passed');
