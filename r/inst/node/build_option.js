// build_option.js
// Builds the ECharts option of a payload's build request with the bundled
// @rtemis/draw, as the widget does, and writes it to stdout as JSON, which
// drops formatter functions. With `what: "aspect"`, writes the fixed-aspect
// box hint the widget derives from the option instead.
//
// Invoked by R's echarts_option() via system2() with the path of a JSON file:
//   { "build": <build request>, "dark": <logical>, "what": "option" | "aspect" }

const fs = require("node:fs");
const path = require("node:path");

const builders = require(path.resolve(
	__dirname,
	"..",
	"htmlwidgets",
	"lib",
	"draw",
	"rtemis-draw-builders.js",
));
const input = JSON.parse(fs.readFileSync(process.argv[2], "utf8"));
const request = input.build;
const builder = builders.builders[request.type];
if (!builder) {
	process.stderr.write(`No @rtemis/draw builder for chart type '${request.type}'.\n`);
	process.exit(1);
}
const data = typeof request.data === "string" ? JSON.parse(request.data) : request.data;
const appearance = { ...request.appearance, dark: input.dark === true };
try {
	const option = builder(data, request.config, appearance);
	const aspect = builders.aspectHints[request.type]?.(option, request.config) ?? null;
	process.stdout.write(JSON.stringify(input.what === "aspect" ? aspect : option));
} catch (e) {
	process.stderr.write(`${e.message}\n`);
	process.exitCode = 1;
}
