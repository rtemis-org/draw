// Independent homogeneous camera matrices pin the GL-to-SVG coordinates.
const assert = require('node:assert/strict'), fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const renderer = require(input.module);
const close = (a,b) => assert.ok(Math.abs(a-b)<1e-8, `${a} != ${b}`);
const multiply = (m,p) => m.map(row=>row.reduce((sum,v,i)=>sum+v*p[i],0));
for (const projection of ['perspective','orthographic'])
for (const [width,height] of [[800,650],[390,650],[900,400]])
for (const [alpha,beta] of [[0,0],[20,40],[-45,-130],[80,180]])
for (const distance of [100,200,400]) {
  const o = structuredClone(input.option), vc = o.grid3D.viewControl;
  Object.assign(vc,{projection,alpha,beta}); o.grid3D.rtemisDistance=distance;
  renderer.prepare(o,{},width,height);
  const g=renderer.geometry(o,width,height), w=width-24, h=height-80;
  const a=alpha*Math.PI/180,b=beta*Math.PI/180;
  const c=Math.cos,s=Math.sin,d=vc.distance;
  const view=[[c(b),0,-s(b),0],[-s(a)*s(b),c(a),-s(a)*c(b),0],
              [c(a)*s(b),s(a),c(a)*c(b),-d],[0,0,0,1]];
  const f=1/Math.tan(50*Math.PI/360), near=.1, far=2000;
  const matrix=projection==='perspective'
    ? [[f*h/w,0,0,0],[0,f,0,0],[0,0,-(far+near)/(far-near),-2*far*near/(far-near)],[0,0,-1,0]]
    : [[2*h/(w*(vc.orthographicSize+1)),0,0,0],[0,2/(vc.orthographicSize+1),0,0],
       [0,0,-2/(far-near),-(far+near)/(far-near)],[0,0,0,1]];
  for(const X of [-50,0,50]) for(const Y of [-50,0,50]) for(const Z of [-50,0,50]) {
    const clip=multiply(matrix,multiply(view,[X,Y,Z,1]));
    const got=g.projectWorld([X,Y,Z]);
    close(got[0],12+w/2*(1+clip[0]/clip[3]));
    close(got[1],60+h/2*(1-clip[1]/clip[3]));
  }
  const svg=renderer.svg({option:o},width,height);
  assert.equal((svg.match(/<circle /g)||[]).length,150);
  assert.ok(!/NaN|Infinity|<image/.test(svg));
}
// Parallel cube edges converge only under perspective projection.
const o=structuredClone(input.option);
Object.assign(o.grid3D.viewControl,{alpha:0,beta:0});
for(const projection of ['perspective','orthographic']) {
  o.grid3D.viewControl.projection=projection;
  const g=renderer.geometry(o,800,650);
  const span=z=>g.projectWorld([50,0,z])[0]-g.projectWorld([-50,0,z])[0];
  if(projection==='perspective') assert.ok(span(50)>span(-50));
  else close(span(50),span(-50));
}
// Reciprocal projected depth preserves a tilted surface's plane equation.
o.grid3D.viewControl.projection='perspective';
const g=renderer.geometry(o,800,650);
const p=[[-40,-30,-20],[40,-30,20],[40,30,20],[-40,30,-20]].map(g.projectWorld);
const u=p[1].map((x,i)=>x-p[0][i]),v=p[2].map((x,i)=>x-p[0][i]);
const n=[u[1]*v[2]-u[2]*v[1],u[2]*v[0]-u[0]*v[2],u[0]*v[1]-u[1]*v[0]];
close(n.reduce((sum,x,i)=>sum+x*(p[3][i]-p[0][i]),0)/Math.hypot(...n),0);
console.log('Perspective and orthographic projection matrices passed');
