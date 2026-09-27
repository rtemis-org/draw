// Independent ray/plane probes validate painter order, including intersections
// where centroid sorting puts the wrong surface on top on half the canvas.
const assert=require('node:assert/strict'),fs=require('node:fs');
const input=JSON.parse(fs.readFileSync(process.argv[2],'utf8'));
const renderer=require(input.module);
const contains=(vertices,x,y)=>{
 let inside=false;
 for(let i=0,j=vertices.length-1;i<vertices.length;j=i++) {
  const a=vertices[i],b=vertices[j];
  if((a[1]>y)!==(b[1]>y) && x<(b[0]-a[0])*(y-a[1])/(b[1]-a[1])+a[0])inside=!inside;
 }
 return inside;
};
const square=(kind,index,z)=>({kind,index,series:index,color:['red','blue'][index],opacity:.5,
 vertices:[[-10,-10,z(-10,-10)],[10,-10,z(10,-10)],[10,10,z(10,10)],[-10,10,z(-10,10)]]});
const planes=[square('surface',0,x=>x),square('surface',1,x=>-x)];
const ordered=renderer.orderPrimitives(planes);
assert.ok(ordered.length>2,'Crossing surfaces must split');
for(const x of [-8,-4,4,8])for(const y of [-7,3]) {
 const layers=ordered.filter(p=>contains(p.vertices,x,y));
 assert.equal(layers.length,2);
 assert.equal(layers.at(-1).series,x>0?0:1);
}
// A disk/line at z=0 must pass from the back to the front of the tilted plane.
for(const kind of ['point','line']) {
 const marks=renderer.orderPrimitives([planes[0],square(kind,1,()=>0)]);
 for(const x of [-8,8]) {
  const layers=marks.filter(p=>contains(p.vertices,x,3));
  assert.equal(layers.at(-1).kind,x<0?kind:'surface');
 }
}
const turn=renderer.pathGeometry([[0,0,0],[10,0,1],[10,10,2]],2);
assert.deepEqual(turn[0].vertices,[[0,-1,0],[0,1,0],[11,-1,1],[9,1,1]]);
assert.deepEqual(turn[1].vertices[0],turn[0].vertices[2]);
for(const palette of ['#4078A6',['#4078A6']]) {
 const option=structuredClone(input.option);option.color=palette;
 renderer.prepare(option,{},800,600);
 assert.equal(option.series[0].itemStyle.color,'#4078A6');
 assert.equal(option.series[1].lineStyle.color,'#4078A6');
 assert.ok(renderer.svg({option},800,600).includes('fill="#4078A6"'));
}
for(const width of [390,800]) {
 const option=structuredClone(input.option);
 renderer.prepare(option,{color:['#123456','#abcdef']},width,600);
 assert.equal(option.series[0].itemStyle.color,option.series[1].lineStyle.color);
 const g=renderer.geometry(option,width,600);
 assert.equal(g.points.length,4);assert.equal(g.lines.length,3);assert.equal(g.triangles.length,2);
 assert.deepEqual(g.triangles[0].values,[[1,0,1],[1,1,3],[0,0,0]]);
 assert.deepEqual(g.triangles[1].values,[[0,0,0],[1,1,3],[0,1,2]]);
 const svg=renderer.svg({option},width,600);
 assert.ok(svg.includes('data-kind="surface"'));assert.ok(svg.includes('data-kind="line"'));
 assert.ok(svg.includes('clipPath'));assert.ok(!svg.includes('<image'));assert.ok(!svg.includes('NaN'));
 // The second panel must use distinct clip identifiers in a combined SVG.
 const second=renderer.svg({option},width,600);
 const ids=s=>[...s.matchAll(/id="([^"]+)"/g)].map(m=>m[1]);
 assert.equal(ids(svg).filter(id=>ids(second).includes(id)).length,0);
 option.legend={selected:{Surface:false}};
 assert.equal(renderer.geometry(option,width,600).triangles.length,0);
 const invalid=JSON.parse(JSON.stringify(option));invalid.series.at(-1).shading='lambert';
 delete invalid.legend;assert.throws(()=>renderer.geometry(invalid,width,600),/flat color/);
 // Missing-corner cells are entirely absent, as in SurfaceView.
 option.legend={};option.series.at(-1).data[0][2]=null;
 assert.equal(renderer.geometry(option,width,600).triangles.length,0);
}
console.log('3D surface visibility passed');
