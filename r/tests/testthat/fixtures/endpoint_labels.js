// Endpoint labels of value axes appear only at tick multiples.
// spec: draw/zoom-controls#endpoint-labels
const assert = require('node:assert/strict'), fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const echarts = require(input.echarts), layout = require(input.layout);
const labels = (chart, type = 'xAxis') => {
  const label = chart.getModel().getComponent(type).get('axisLabel');
  return [label.showMinLabel, label.showMaxLabel];
};
const render = option => {
  const chart = echarts.init(null, null, {renderer: 'svg', ssr: true, width: 800, height: 500});
  chart.setOption(option);
  return chart;
};
const series = [{type: 'line', data: [[0.3, 1], [11.47, 2]]}];
let payload = {option: {xAxis: {type: 'value', min: 0.3, max: 11.47}, yAxis: {type: 'value'}, series}};
let chart = render(payload.option);
layout.fitEndpointLabels(chart, payload);
assert.deepEqual(labels(chart), [false, false], 'unrounded ends hidden');
assert.deepEqual(labels(chart, 'yAxis'), [true, true], 'nice ends shown');
chart.dispose();
payload = {option: {xAxis: {type: 'value', min: 0, max: 15}, yAxis: {type: 'value'}, series}};
chart = render(payload.option);
layout.fitEndpointLabels(chart, payload);
assert.deepEqual(labels(chart), [true, true], 'tick ends shown');
chart.dispose();
payload = {option: {xAxis: {type: 'value', min: 0, max: 15, axisLabel: {showMinLabel: false}}, yAxis: {type: 'value'}, series,
  dataZoom: [{type: 'inside', xAxisIndex: 0, filterMode: 'none'}]}};
chart = render(payload.option);
layout.fitEndpointLabels(chart, payload);
assert.deepEqual(labels(chart), [false, true], 'explicit setting kept');
chart.dispatchAction({type: 'dataZoom', start: 13, end: 77});
layout.fitEndpointLabels(chart, payload);
const extent = chart.getModel().getComponent('xAxis').axis.scale.getExtent();
assert.ok(extent[1] < 15, 'zoomed');
assert.deepEqual(labels(chart), [false, false], 'zoomed unrounded end hidden');
chart.dispose();
console.log('Endpoint labels passed');
