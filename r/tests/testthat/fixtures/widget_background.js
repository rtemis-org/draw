// Execute the real browser binding with a minimal DOM to verify ownership of
// page backgrounds. Native browser screenshots separately check the viewport.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const path = require('node:path');
const directory = path.join(path.dirname(process.argv[2]), 'lib/draw');
for (const kind of ['single', 'panels']) {
for (const location of ['standalone', 'embedded', 'embedded-direct', 'embedded-header', 'multiple', 'bounded', 'vscode', 'vscode-header', 'vscode-outer-header', 'vscode-multiple', 'vscode-nested']) {
  const isVSCode = location.startsWith('vscode');
  const ownsPage = location === 'standalone' || location === 'vscode';
  const classes = new Set([isVSCode ? 'vscode-dark' : 'quarto-dark']);
  const body = {style: {backgroundColor: 'white'}, classList: {contains: c => classes.has(c)}};
  const html = {style: {backgroundColor: ''}};
  const host = location === 'embedded-direct' ? body :
    {style: {}, id: location === 'embedded' ? 'article' : 'htmlwidget_container', parentElement: body};
  const el = {style: {}, parentElement: host, isConnected: true, children: [],
    replaceChildren() {this.children = [];}, appendChild(child) {child.parentElement=this;this.children.push(child);}};
  host.children = ['multiple', 'vscode-multiple'].includes(location) ? [el, {}] : [el];
  if (host !== body) body.children = [host, {tagName: 'SCRIPT'}];
  if (location === 'embedded-header') body.children.push({tagName: 'H1'});
  if (isVSCode) {
    // vscode-R wraps the complete saved document in this span. HTML parsing
    // places the saved head's metadata and dependencies alongside the widget.
    const wrapper = {id: 'webview-content', parentElement: body,
      children: [{tagName: 'META'}, {tagName: 'TITLE'}, {tagName: 'LINK'},
        {tagName: 'STYLE'}, host, {tagName: 'SCRIPT'}]};
    host.parentElement = wrapper;
    body.children = [wrapper, {tagName: 'SCRIPT'}];
    if (location === 'vscode-header') wrapper.children.push({tagName: 'H1'});
    if (location === 'vscode-outer-header') body.children.push({tagName: 'H1'});
    if (location === 'vscode-nested') {
      wrapper.children = [{tagName: 'ARTICLE', children: [host]}];
      host.parentElement = wrapper.children[0];
    }
  }
  let notify;
  class Observer {
    constructor(fn) {notify = fn;}
    observe() {}
    disconnect() {}
  }
  const context = {
    document: {body, documentElement: html, createElement() {return {style: {}, isConnected: true};}},
    window: {MutationObserver: Observer}, MutationObserver: Observer,
    HTMLWidgets: {widget() {}},
    rtemisA3: {fit() {}},
    rtemisConfusion: {prepare() {}},
    rtemisPanels: {background: require(path.join(directory, 'panels.js')).background, cells: require(path.join(directory, 'panels.js')).cells, fitHeatmap() {}, fitGantt() {}, prepareColors() {}, fitAxes() {}, positionLegend() {}, centerVisualMaps() {}},
    echarts: {registerTheme() {}, init() {return {setOption() {}, dispose() {}};}}
  };
  vm.createContext(context);
  vm.runInContext(source, context);
  vm.runInContext(fs.readFileSync(path.join(directory, 'panel_widget.js'), 'utf8'), context);
  if (kind === 'panels' && location === 'bounded') continue;
  const renderer = kind === 'panels' ? context.rtemisPanelFactory(el, 560, 500) : context.rtemisDrawFactory(el, 560, 500, location === 'bounded');
  const render = payload => renderer.renderValue(kind === 'panels' ? {panels:[payload, payload], layout:{ncol:2,gap:20,padding:10}} : payload);
  render({option: {}, autoTheme: true,
    theme: {backgroundColor: '#ffffff'}, themeDark: {backgroundColor: '#181818'}});
  assert.equal(el.style.backgroundColor, '#181818');
  assert.equal(body.style.backgroundColor, ownsPage ? '#181818' : 'white');
  assert.equal(html.style.backgroundColor, ownsPage ? '#181818' : '');
  classes.delete(isVSCode ? 'vscode-dark' : 'quarto-dark');
  classes.add(isVSCode ? 'vscode-light' : 'quarto-light'); notify();
  assert.equal(el.style.backgroundColor, '#ffffff');
  assert.equal(body.style.backgroundColor, ownsPage ? '#ffffff' : 'white');
  render({option: {backgroundColor: '#253344'}, theme: {backgroundColor: '#ffffff'}});
  assert.equal(el.style.backgroundColor, '#253344', 'Explicit option background must win');
  assert.equal(body.style.backgroundColor, ownsPage ? '#253344' : 'white');
  renderer.dispose();
  assert.equal(body.style.backgroundColor, 'white', 'Disposal must restore the host page');
  assert.equal(html.style.backgroundColor, '');
  if (host !== body) assert.equal(host.style.backgroundColor, undefined);
  assert.equal(el.style.backgroundColor, undefined);
}
console.log('Standalone, VS Code, embedded, bounded, multiple-widget, theme, override, and disposal backgrounds passed');

}
