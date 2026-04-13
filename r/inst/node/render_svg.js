// render_svg.js
// Node.js SSR renderer for ECharts options.
//
// Reads a JSON payload from stdin of the form:
//   { "option": <ECharts option>, "theme": <theme or null>,
//     "width": <number>, "height": <number> }
// Writes the resulting SVG string to stdout.
//
// Invoked by R's save_drawing() via system2(). Reuses the same
// echarts.min.js bundled at inst/htmlwidgets/lib/echarts/.

const path = require("node:path");

// The bundled echarts.min.js is a UMD build that detects CommonJS and
// exports its module. Require it via an absolute path resolved from
// this script's location.
const echartsPath = path.resolve(
	__dirname,
	"..",
	"htmlwidgets",
	"lib",
	"echarts",
	"echarts.min.js",
);
const echarts = require(echartsPath);

// Read the entire stdin as UTF-8.
let raw = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => {
	raw += chunk;
});
process.stdin.on("end", () => {
	let payload;
	try {
		payload = JSON.parse(raw);
	} catch (e) {
		process.stderr.write(
			`render_svg: failed to parse stdin JSON: ${e.message}\n`,
		);
		process.exit(2);
	}

	const width = payload.width || 800;
	const height = payload.height || 600;
	const option = payload.option;
	const theme = payload.theme || null;

	if (!option) {
		process.stderr.write("render_svg: payload.option is missing\n");
		process.exit(2);
	}

	if (theme) {
		echarts.registerTheme("draw_theme", theme);
	}

	const chart = echarts.init(null, theme ? "draw_theme" : null, {
		renderer: "svg",
		ssr: true,
		width: width,
		height: height,
	});

	chart.setOption(option);

	const svg = chart.renderToSVGString();
	chart.dispose();

	process.stdout.write(svg);
});
