// Shared custom-series renderers for browser widgets and Node SVG export.
// renderItem names and itemPayload remain plain JSON in the compiled option.
// See CustomSeriesOption / CustomSeriesRenderItemParams in ECharts
// src/chart/custom/CustomSeries.ts and name lookup in CustomView.ts.
(function (root, register) {
  if (typeof module === "object" && module.exports) {
    module.exports = register;
  } else {
    register(root.echarts);
  }
})(typeof globalThis !== "undefined" ? globalThis : this, function (echarts) {
  "use strict";

  const renderers = {
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
        style: { stroke: settings.color, lineWidth: 1, fill: null },
      };
    },
  };

  Object.keys(renderers).forEach(function (name) {
    echarts.registerCustomSeries(name, renderers[name]);
  });
  return Object.keys(renderers);
});
