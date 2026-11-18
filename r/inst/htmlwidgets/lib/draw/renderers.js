// Shared custom-series renderers for browser widgets and Node SVG export.
// renderItem names and itemPayload remain plain JSON in the compiled option.
// The boxplot's, the Gantt chart's, the heatmap's, the histogram's, the
// scatter rug's, the survival risk strip's, and the time series zeitgeber
// labels' renderers are @rtemis/draw's: in Node this module registers them
// too, and in a browser the binding does.
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
  };

  Object.keys(renderers).forEach(function (name) {
    echarts.registerCustomSeries(name, renderers[name]);
  });
  return Object.keys(renderers);
});
