// Load the package-owned scripts in their declared HTML dependency order.
// The chart engine is stubbed so this checks packaging independently of layout.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const scripts = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));

function renderWithScripts(paths) {
  let binding;
  let rendered = 0;
  const chart = {setOption() {rendered++;}, resize() {}, dispose() {}};
  const body = {style: {}, classList: {contains() {return false;}}};
  const context = {
    document: {body}, window: {},
    HTMLWidgets: {widget(value) {binding = value;}},
    echarts: {registerTheme() {}, registerCustomSeries() {}, init() {return chart;}}
  };
  vm.createContext(context);
  for (const path of paths) {
    vm.runInContext(fs.readFileSync(path, 'utf8'), context, {filename: path});
  }
  const element = {style: {}, parentElement: body, replaceChildren() {}};
  const instance = binding.factory(element, 560, 450);
  instance.renderValue({option: {series: [{type: 'line', data: [1, 3, 2]}]}});
  assert.equal(instance.getChart(), chart);
  assert.equal(rendered, 1);
  instance.resize(400, 350);
  instance.dispose();
}

renderWithScripts(scripts);
// Reproduce a frozen header from before confusion.js became a dependency.
assert.throws(() => renderWithScripts(scripts.filter(path => !path.endsWith('/confusion.js'))),
  /rtemisConfusion is not defined/);
console.log('Declared dependencies render; the incomplete frozen header is detected');
