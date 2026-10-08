const assert = require('node:assert/strict');
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const renderer = require(input.module);
const option = structuredClone(input.option);
const pointer = () => option.grid3D.axisPointer;

// Reuse the payload as the widget does during theme changes and resizing.
for (const color of ['#333333', '#e4e4e7', '#aabbcc', '#333333']) {
  for (const width of [800, 390]) {
    renderer.prepare(option, {textStyle: {color}}, width, 650);
    assert.equal(pointer().lineStyle.color, color);
    assert.equal(pointer().label.color, color);
    assert.equal(option.xAxis3D.axisLabel.color, color);
  }
}

const custom = structuredClone(input.option);
custom.grid3D.axisPointer = {
  show: true,
  lineStyle: {color: '#ff8800', width: 3, opacity: 0.6},
  label: {color: '#0066aa', show: false}
};
custom.xAxis3D.axisPointer = {lineStyle: {color: '#cc00aa'}};
const authored = structuredClone(custom.grid3D.axisPointer);
for (const color of ['#333333', '#e4e4e7']) {
  renderer.prepare(custom, {textStyle: {color}}, 800, 650);
  assert.deepEqual(custom.grid3D.axisPointer, authored);
  assert.equal(custom.xAxis3D.axisPointer.lineStyle.color, '#cc00aa');
}

// An authored label still permits the guide line to follow the theme.
const partial = structuredClone(input.option);
partial.grid3D.axisPointer = {label: {color: '#ff8800'}};
renderer.prepare(partial, {textStyle: {color: '#e4e4e7'}}, 800, 650);
assert.equal(partial.grid3D.axisPointer.lineStyle.color, '#e4e4e7');
assert.equal(partial.grid3D.axisPointer.label.color, '#ff8800');
// A subsequent explicit override takes precedence over the injected value.
partial.grid3D.axisPointer.lineStyle.color = '#00aacc';
renderer.prepare(partial, {textStyle: {color: '#333333'}}, 800, 650);
assert.equal(partial.grid3D.axisPointer.lineStyle.color, '#00aacc');
console.log('3D hover guides follow the active theme and preserve authored colors');

// Exercise the guide geometry independently of the grid and axis geometry.
const identity = {array: [1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1]};
const segments = [];
const lines = {addLine(a, b, color, width) {segments.push({a, b, color, width});}};
const gl = {camera: {viewMatrix: identity, projectionMatrix: identity},
            viewport: {width: 200, height: 100, devicePixelRatio: 2}};
const model = {coordinateSystem: {viewGL: gl}};
const view = {_axisPointerLineMesh: {geometry: lines, worldTransform: identity}};
const chart = {getModel: () => ({eachComponent(type, fn) {
  assert.equal(type, 'grid3D'); fn(model);
}}), getViewOfComponentModel: () => view};
renderer.dottedGuides(chart);
const installed = lines.addLine;
renderer.dottedGuides(chart);
assert.equal(lines.addLine, installed);
const color = [0.8, 0.8, 0.8, 0.7];
lines.addLine([0,0,0], [1,0,0], color, 2);
assert.equal(segments.length, 23);
assert.deepEqual(segments[0], {a:[0,0,0], b:[0.015,0,0], color, width:2});
assert.deepEqual(segments[1].a, [0.045,0,0]);
assert.deepEqual(segments.at(-1).b, [1,0,0]);
segments.length = 0;
// Resizing changes the subdivision, while CSS dot size and colors persist.
gl.viewport.width = 400;
lines.addLine([0,0,0], [1,0,0], color, 2);
assert.equal(segments.length, 45);
assert.deepEqual(segments[0].b, [0.0075,0,0]);
segments.length = 0;
lines.addLine([1,1,1], [1,1,1], color, 2);
assert.equal(segments.length, 0);

// Perspective division changes world spacing while preserving screen spacing.
segments.length = 0;
gl.camera.projectionMatrix = {array: [1,0,0,0, 0,1,0,0, 0,0,1,-1, 0,0,0,2]};
lines.addLine([0,0,0], [1,0,1], color, 2);
const screen = p => p[0]/(2-p[2])*gl.viewport.width/2;
for(let i=0;i<segments.length;i++) {
  const segment=segments[i];
  assert.ok(Math.abs(screen(segment.a)-i*4.5)<1e-10);
  assert.ok(Math.abs(screen(segment.b)-Math.min(i*4.5+1.5,200))<1e-10);
}
