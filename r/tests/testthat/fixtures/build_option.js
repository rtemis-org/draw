// Builds the ECharts option of a widget payload's build request with the
// bundled @rtemis/draw, as the widget does, for the R tests. Prints the option
// as JSON, which drops formatter functions, or with `what: "aspect"` the
// fixed-aspect box hint the widget derives from it.
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const lib = require(input.builders);
const request = input.payload.build;
const data = JSON.parse(request.data);
const appearance = {...request.appearance, dark: input.dark === true};
const option = lib.builders[request.type](data, request.config, appearance);
const aspect = lib.aspectHints[request.type]?.(option, request.config) ?? null;
process.stdout.write(JSON.stringify(input.what === 'aspect' ? aspect : option));
