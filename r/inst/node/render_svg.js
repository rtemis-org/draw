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
const layout = require("../htmlwidgets/lib/draw/panels.js");
const confusion = require("../htmlwidgets/lib/draw/confusion.js");
const a3 = require("../htmlwidgets/lib/draw/a3.js");

// Escape the CSS color as XML attribute text, including caller-supplied values.
function backgroundRect(payload, width, height) {
  const color = layout.background(payload).replace(/[&<>"']/g, c =>
    ({'&':'&amp;', '<':'&lt;', '>':'&gt;', '"':'&quot;', "'":'&apos;'}[c]));
  return `<rect width="${width}" height="${height}" fill="${color}"/>`;
}

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
	const creator = payload.creator || "rtemis.draw";
	function renderPanel(panel, w, h) {
		confusion.prepare(echarts, panel, panel.theme, w, h);
		layout.prepareColors(echarts, panel, panel.theme);
		const option = panel.option;
		if (!option) throw new Error("An ECharts option is required for each panel.");
		const series = Array.isArray(option.series) ? option.series : [option.series];
		series.filter(Boolean).forEach(item => {
			if (item.type === "custom" && !rendererNames.includes(item.renderItem)) {
				throw new Error("Unsupported custom-series renderer. Use a built-in named renderer.");
			}
			item.animation = false;
		});
		option.animation = false;
		let chart;
		try {
			chart = echarts.init(null, panel.theme || null,
				{renderer: "svg", ssr: true, width: w, height: h});
			chart.setOption(option);
			a3.fit(chart, panel, true);
			layout.fitAxes(chart, panel);
			layout.fitHeatmap(chart, panel);
			layout.positionLegend(echarts, chart, panel);
			layout.centerVisualMaps(chart, panel);
			return chart.renderToSVGString();
		} finally {
			if (chart) chart.dispose();
		}
	}
	try {
		let svg;
		if (payload.panels) {
			const cells = layout.cells(payload.panels.length, payload.layout, width, height);
			const parts = payload.panels.map((panel, i) => {
				const cell = cells[i];
				layout.fit(panel, cell.width, cell.height);
				// Nested SVG viewports clip each independent scene to its cell.
				// One ECharts module supplies unique clip/style IDs across panels.
				return renderPanel(panel, cell.width, cell.height).replace("<svg ",
					`<svg x="${cell.x}" y="${cell.y}" overflow="hidden" `);
			});
			svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">${backgroundRect(payload, width, height)}${parts.join("")}</svg>`;
		} else {
			svg = renderPanel(payload, width, height);
		}
		process.stdout.write(svg.replace(/(<svg[^>]*>)/, `$1<!-- Created by ${creator} -->`));
	} catch (e) {
		process.stderr.write(`render_svg: ${e.message}\n`);
		process.exitCode = 1;
	}
});
