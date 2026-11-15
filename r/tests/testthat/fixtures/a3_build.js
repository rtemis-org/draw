// Builds the ECharts option of a widget payload's build request with the
// bundled @rtemis/draw, as the widget does, for the R tests. Prints the option
// as JSON, which drops formatter functions, the surface hints, and the residue
// tooltip text at each requested position.
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const lib = require(input.builders);
const request = input.payload.build;
const data = JSON.parse(request.data);
const appearance = {...request.appearance, dark: input.dark === true};
const option = lib.builders[request.type](data, request.config, appearance);
const backbone = option.series.find(s => s.name === 'Primary structure');
const tooltips = {};
for (const position of input.positions || []) {
  const html = backbone.tooltip.formatter({data: {position, residueName: ''}});
  tooltips[position] = html.replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ').trim();
}
process.stdout.write(JSON.stringify({
  option,
  hints: lib.a3SurfaceHints(data, request.config, appearance),
  tooltips,
}));
