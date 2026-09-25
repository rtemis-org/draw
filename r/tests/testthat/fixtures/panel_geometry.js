// Reproduce htmlwidgets reporting the viewport size for a narrower host.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const directory = process.argv[2];
const chartSizes = [];
const host = {clientWidth: 900, clientHeight: 420, style: {},
  children: [], replaceChildren() {this.children = [];},
  appendChild(child) {this.children.push(child);}};
const context = {
  document: {createElement() {return {style: {}};}},
  rtemisPanels: require(path.join(directory, 'panels.js')),
  rtemisDrawFactory(element, width, height, bounded) {
    assert.equal(bounded, true);
    const state = {width, height};
    chartSizes.push(state);
    return {renderValue() {}, dispose() {}, resize(w,h) {state.width=w;state.height=h;}};
  }
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(path.join(directory, 'panel_widget.js'), 'utf8'), context);
const figure = context.rtemisPanelFactory(host, 1450, 1000);
figure.renderValue({panels: [{},{}], layout: {ncol: 2, gap: 20, padding: 10}});
assert.deepEqual(chartSizes, [{width:430,height:400},{width:430,height:400}]);
assert.equal(host.children[1].style.left, '460px');
host.clientWidth = 700; host.clientHeight = 360;
figure.resize(1450,1000);
assert.deepEqual(chartSizes, [{width:330,height:340},{width:330,height:340}]);
assert.equal(host.children[1].style.left, '360px');
console.log('Host geometry passed');
