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
const registerRenderers = require("../htmlwidgets/lib/draw/renderers.js");
const rendererNames = registerRenderers(echarts);

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
	const creator = payload.creator || "rtemis.draw";

	if (!option) {
		process.stderr.write("render_svg: payload.option is missing\n");
		process.exit(2);
	}

	let chart;
	try {
		// An unknown renderer can silently produce an empty series, especially
		// for empty data. Reject it before rendering, independently of data size.
		const series = Array.isArray(option.series) ? option.series : [option.series];
		series.filter(Boolean).forEach((item) => {
			if (item.type === "custom" && !rendererNames.includes(item.renderItem)) {
				throw new Error("Unsupported custom-series renderer. Use a built-in named renderer.");
			}
		});
		if (theme) {
			echarts.registerTheme("draw_theme", theme);
		}
		chart = echarts.init(null, theme ? "draw_theme" : null, {
			renderer: "svg",
			ssr: true,
			width: width,
			height: height,
		});

		// Static output has no animation timeline. Its paths represent the final
		// scene, including series that normally animate from collapsed geometry.
		option.animation = false;
		series.filter(Boolean).forEach((item) => { item.animation = false; });
		chart.setOption(option);

		const svg = chart.renderToSVGString();

		// Inject creator comment immediately after the opening <svg ...> tag.
		const svgOut = svg.replace(/(<svg[^>]*>)/, `$1<!-- Created by ${creator} -->`);
		process.stdout.write(svgOut);
	} catch (e) {
		process.stderr.write(`render_svg: ${e.message}\n`);
		process.exitCode = 1;
	} finally {
		if (chart) chart.dispose();
	}
});
