// Shared custom-series renderers for browser widgets and Node SVG export.
// renderItem names and itemPayload remain plain JSON in the compiled option.
// The boxplot's, the histogram's, and the scatter rug's renderers are
// @rtemis/draw's: in Node this module registers them too, and in a browser
// the binding does.
// See CustomSeriesOption / CustomSeriesRenderItemParams in ECharts
// src/chart/custom/CustomSeries.ts and name lookup in CustomView.ts.
(function (root, register) {
  if (typeof module === "object" && module.exports) {
    // In Node the package's renderers register with the widget's own; in a
    // browser the binding registers them once RtemisDraw has loaded.
    module.exports = function (echarts) {
      const builders = require("./rtemis-draw-builders.js");
      builders.registerRenderers(echarts);
      return register(echarts).concat(builders.RENDERER_NAMES);
    };
  } else {
    register(root.echarts);
  }
})(typeof globalThis !== "undefined" ? globalThis : this, function (echarts) {
  "use strict";

  const renderers = {
    "rtemis.ribbon.v1": function(params, api) {
      return { type: "polygon", shape: {points:params.itemPayload.vertices.map(p=>api.coord(p))},
        style: {fill:api.visual("color"), opacity:params.itemPayload.opacity} };
    },
    // Materialized labels preserve real time spacing and remain vector text.
    "rtemis.axis_labels.v1": function (params, api) {
      const point = api.coord([api.value(0), 0]);
      const box = params.coordSys;
      if (point[0] < box.x - 1 || point[0] > box.x + box.width + 1) return;
      return { type: "text", style: { x: point[0], y: box.y + box.height + 10,
        text: String(api.value(1)), align: "center", verticalAlign: "top",
        fill: "#888888", font: api.font({ fontSize: 12 }) } };
    },
    // Data: [row, start, end, optional border flag]. Colors are resolved by
    // ECharts so series legends and per-datum styling use the same visual.
    "rtemis.gantt.v1": function (params, api) {
      const settings = params.itemPayload;
      const row = api.value(0);
      const start = api.coord([api.value(1), row]);
      const end = api.coord([api.value(2), row]);
      const height = api.size([0, 1])[1] * settings.barHeight;
      const style = { fill: api.visual("color") };
      if (api.value(3)) {
        style.stroke = settings.borderColor;
        style.lineWidth = settings.borderWidth;
      }
      return {
        type: "rect",
        transition: ["shape"],
        shape: {
          x: start[0], y: start[1] - height / 2,
          width: Math.max(1, end[0] - start[0]), height: height,
          r: settings.barRadius,
        },
        style: style,
      };
    },

    // Data: [left position, right position, left height, right height,
    // merge height]. Each merge contributes one three-segment U shape.
    "rtemis.heatmap_tracks.v1": function(params, api) {
      const settings = params.itemPayload, index = api.value(0);
      const row = settings.orientation === "row", rect = params.coordSys;
      const center = api.coord(row ? [0, index] : [index, 0]);
      const size = api.size([1, 1]);
      return { type: "group", children: settings.colors[index].map(function(color, track) {
        return { type: "rect", shape: row
          ? { x: rect.x - 4 - (track + 1) * 12, y: center[1] - Math.abs(size[1])/2,
              width: 10, height: Math.abs(size[1]) }
          : { x: center[0] - Math.abs(size[0])/2,
              y: settings.top ? rect.y - 4 - (track + 1) * 12 : rect.y + rect.height + 4 + track * 12,
              width: Math.abs(size[0]), height: 10 }, style: { fill: color } };
      }) };
    },
    "rtemis.dendrogram.v1": function (params, api) {
      const settings = params.itemPayload;
      const lp = api.value(0), rp = api.value(1);
      const lh = api.value(2), rh = api.value(3), mh = api.value(4);
      const points = settings.orientation === "row"
        ? [[lh, lp], [mh, lp], [mh, rp], [rh, rp]]
        : [[lp, lh], [lp, mh], [rp, mh], [rp, rh]];
      return {
        type: "polyline",
        shape: { points: points.map(function (point) { return api.coord(point); }) },
        style: { stroke: settings.colors ? settings.colors[params.dataIndex] : settings.color, lineWidth: 1, fill: null },
      };
    },
  };

  Object.keys(renderers).forEach(function (name) {
    echarts.registerCustomSeries(name, renderers[name]);
  });
  return Object.keys(renderers);
});
