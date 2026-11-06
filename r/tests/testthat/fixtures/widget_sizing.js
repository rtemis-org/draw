// Execute the real browser binding with a minimal DOM to verify that chart
// geometry follows the element's content box and the host's height.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const directory = path.join(path.dirname(process.argv[2]), 'lib/draw');
const panels = require(path.join(directory, 'panels.js'));

function mount(element) {
  const inits = [], resizes = [], options = [];
  const chart = {
    setOption(option) {options.push(option);}, dispose() {}, on() {},
    resize(size) {resizes.push(size);}, getWidth: () => element.clientWidth,
    getHeight: () => resizes.at(-1)?.height ?? inits[0].height,
    getZr: () => ({on() {}, off() {}, painter: {getRenderedCanvas() {}}}),
    getDom: () => element, convertToPixel: () => [0, 0],
    getModel: () => ({getComponent: () => null}),
  };
  class Observer {observe() {} disconnect() {}}
  const body = {style: {}, classList: {contains: () => false}, children: []};
  const context = {
    document: {body, documentElement: {style: {}}, createElement: () => ({style: {}})},
    window: {MutationObserver: Observer}, MutationObserver: Observer,
    HTMLWidgets: {widget(definition) {context.definition = definition;}},
    rtemisA3: {fit() {}}, rtemisConfusion: {prepare() {}},
    rtemisPanels: {...panels, fitAxes() {}, fitHeatmap() {}, positionLegend() {},
      centerVisualMaps() {}, fitGantt() {}, fitEndpointLabels() {}},
    echarts: {registerTheme() {}, init(_, __, size) {inits.push(size); return chart;}},
  };
  vm.createContext(context);
  vm.runInContext(fs.readFileSync(path.join(directory, 'theme_watch.js'), 'utf8'), context);
  vm.runInContext(source, context);
  return {context, inits, resizes};
}

const element = (style, clientWidth) => ({style, clientWidth, parentElement: {style: {}},
  isConnected: true, children: [], replaceChildren() {}, appendChild() {}, addEventListener() {}});

// A fixed pixel width inside a full-width htmlwidgets container.
{
  const el = element({width: '900px', height: '500px'}, 900);
  const {context, inits, resizes} = mount(el);
  const widget = context.definition.factory(el, 1100, 500);
  widget.renderValue({option: {}});
  assert.equal(inits[0].width, 900);
  widget.resize(1300, 500);
  assert.equal(resizes.at(-1).width, 900);
}

// An element without a measured box falls back to the reported dimensions.
{
  const el = element({}, 0);
  const {context, inits, resizes} = mount(el);
  const widget = context.definition.factory(el, 1100, 500);
  widget.renderValue({option: {}});
  assert.equal(inits[0].width, 1100);
  widget.resize(700, 500);
  assert.equal(resizes.at(-1).width, 700);
}

const aspect = () => ({option: {grid: {left: 60, right: 40, top: 20, bottom: 60}},
  aspect: {ratio: 1, leftPx: 60, rightPx: 40, topPx: 20, botPx: 60}});

// An aspect chart in a filled host keeps the host height and fits the grid.
{
  const el = element({width: '100%', height: '100%'}, 1100);
  const {context, inits, resizes} = mount(el);
  const payload = aspect();
  const widget = context.definition.factory(el, 1100, 700);
  widget.renderValue(payload);
  assert.equal(el.style.height, '100%');
  assert.equal(inits[0].height, 700);
  assert.ok(payload.option.grid.height <= 700 - 20 - 60);
  assert.equal(payload.option.grid.width, payload.option.grid.height);
  widget.resize(1100, 400);
  assert.equal(el.style.height, '100%');
  assert.equal(resizes.at(-1).height, 400);
  assert.ok(payload.option.grid.height <= 400 - 20 - 60);
}

// An aspect chart in a document grows with the width instead.
{
  const el = element({width: '100%', height: '500px'}, 1100);
  const {context, inits} = mount(el);
  const payload = aspect();
  context.definition.factory(el, 1100, 500).renderValue(payload);
  assert.equal(el.style.height, '1080px');
  assert.equal(inits[0].height, 1080);
}

// A square-cell heatmap in a filled host keeps the host height.
{
  const el = element({width: '100%', height: '100%'}, 1100);
  const {context, inits} = mount(el);
  const payload = {option: {}, squareCells: {nRows: 150, nCols: 4, leftPx: 60, rightPx: 40,
    topPx: 20, botPx: 20}};
  Object.assign(payload, payload.squareCells);
  context.definition.factory(el, 1100, 700).renderValue(payload);
  assert.equal(el.style.height, '100%');
  assert.equal(inits[0].height, 700);
}

console.log('Widget content box and host height sizing passed');
