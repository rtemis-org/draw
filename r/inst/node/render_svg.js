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
const builders = require("../htmlwidgets/lib/draw/rtemis-draw-builders.js");
// The id and handle size of a zoom preset's slider (zoom.R, @rtemis/draw).
const ZOOM_SLIDER_ID = builders.ZOOM_SLIDER_ID;
const ZOOM_SLIDER_HANDLE = 7;

// Build the option of a payload that carries a build request with the bundled
// @rtemis/draw, in the light scheme offline export uses, without interactive
// controls. spec: draw/shared-builders#r-widget
function build(panel) {
  const request = panel.build;
  const builder = builders.builders[request.type];
  if (!builder) throw new Error(`No @rtemis/draw builder for chart type '${request.type}'.`);
  const data = typeof request.data === "string" ? JSON.parse(request.data) : request.data;
  const appearance = {...request.appearance, dark: false};
  const option = {...builder(data, request.config, appearance)};
  // The builders leave the background to the host; export paints the theme's.
  delete option.backgroundColor;
  if (request.appearance?.grid) option.grid = {...option.grid, ...request.appearance.grid};
  // A zoom preset's slider band returns to the plot, as static_controls()
  // does in R for options built there.
  for (const zoom of [].concat(option.dataZoom || [])) {
    if (zoom.id !== ZOOM_SLIDER_ID) continue;
    for (const side of ["bottom", "right"]) {
      if (typeof zoom[side] === "number" && typeof option.grid?.[side] === "number") {
        option.grid = {...option.grid, [side]: zoom[side] - ZOOM_SLIDER_HANDLE};
      }
    }
  }
  delete option.dataZoom;
  delete option.toolbox;
  panel.option = option;
  if (request.type === "a3") {
    panel.a3 = {...builders.a3SurfaceHints(data, request.config, appearance), ...panel.a3};
  }
  delete panel.build;
}

// The height an unset A3 export takes: the widget's unbounded fit, which holds
// the plot body and the legend rail, measured at the export width. A body of
// at least one pixel keeps the fit defined when glyph sizes are zero.
function a3ExportHeight(panel, width) {
  const hint = {...panel.a3, bodyHeight: Math.max(panel.a3.bodyHeight, 1)};
  const chart = echarts.init(null, panel.theme || null,
    {renderer: "svg", ssr: true, width, height: Math.max(Math.ceil(panel.a3.height), 1)});
  try {
    chart.setOption(panel.option);
    a3.fit(chart, {...panel, a3: hint}, false);
    return Math.ceil(chart.getHeight());
  } finally {
    chart.dispose();
  }
}

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
	let height = payload.height || 600;
	const creator = payload.creator || "rtemis.draw";
	function renderPanel(panel, w, h) {
    if (panel.option?.grid3D) return require("../htmlwidgets/lib/draw/scatter3d.js").svg(panel,w,h);
		confusion.prepare(echarts, panel, panel.theme, w, h);
		layout.prepareLabels(panel);
		layout.prepareColors(echarts, panel, panel.theme);
		const option = panel.option;
		if (!option) throw new Error("An ECharts option is required for each panel.");
		const series = Array.isArray(option.series) ? option.series : [option.series];
		series.filter(Boolean).forEach(item => {
			if (item.type === "custom" && !rendererNames.includes(item.renderItem)) {
				throw new Error("Unsupported custom-series renderer. Use a built-in named renderer.");
			}
			// Transparent line symbols are native hover targets (e.g. ROC points).
			// Static SVG needs only the curve. All-array data have no per-point
			// style overrides that could make a symbol visible.
			if (item.type === "line" && item.itemStyle?.opacity === 0 &&
				Array.isArray(item.data) && item.data.every(Array.isArray)) {
				item.showSymbol = false;
			}
			item.animation = false;
		});
		option.animation = false;
		let chart;
		try {
			chart = echarts.init(null, panel.theme || null,
				{renderer: "svg", ssr: true, width: w, height: h});
			chart.setOption(option);
			layout.fitGantt(echarts, chart, panel);
			a3.fit(chart, panel, true);
			layout.fitAxes(chart, panel);
			layout.fitHeatmap(chart, panel);
			layout.positionLegend(echarts, chart, panel);
			layout.centerVisualMaps(chart, panel, echarts);
			layout.fitEndpointLabels(chart, panel);
			return chart.renderToSVGString();
		} finally {
			if (chart) chart.dispose();
		}
	}
	try {
		let svg;
		if (payload.backend === "graph") {
      svg = require("./vector.js").graphSVG({...payload.scene, theme:payload.theme, width, height});
    } else if (payload.backend === "map") {
      svg = require("./vector.js").mapSVG({...payload.scene, theme:payload.theme, width, height});
    } else if (payload.backend === "diagram") {
      svg = require("./diagram.js").diagramSVG({...payload.scene, theme:payload.theme});
    } else if (payload.panels) {
			const cells = layout.cells(payload.panels.length, payload.layout, width, height);
			const parts = payload.panels.map((panel, i) => {
				const cell = cells[i];
				if (panel.build) build(panel);
				layout.fit(panel, cell.width, cell.height);
				// Nested SVG viewports clip each independent scene to its cell.
				// One ECharts module supplies unique clip/style IDs across panels.
				return renderPanel(panel, cell.width, cell.height).replace("<svg ",
					`<svg x="${cell.x}" y="${cell.y}" overflow="hidden" `);
			});
			svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">${backgroundRect(payload, width, height)}${parts.join("")}</svg>`;
		} else {
			if (payload.build) {
				build(payload);
				if (payload.a3?.exportAutoHeight) height = a3ExportHeight(payload, width);
			}
			svg = renderPanel(payload, width, height);
		}
		process.stdout.write(svg.replace(/(<svg[^>]*>)/, `$1<!-- Created by ${creator} -->`));
	} catch (e) {
		process.stderr.write(`render_svg: ${e.message}\n`);
		process.exitCode = 1;
	}
});
