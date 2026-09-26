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
    // Matches BoxplotSeries layout in src/chart/boxplot/boxplotLayout.ts.
    // Data: [category index, exact observed value, observation ID, offset].
    // Use active box series so legend filtering and resizing keep points
    // centered on their boxes. Shared by browser and static SVG rendering.
    "rtemis.boxplot_points.v1": function (params, api) {
      const settings = params.itemPayload;
      const active = api.currentSeriesIndices();
      const boxes = settings.boxSeries.filter(function (i) { return active.includes(i); });
      const index = boxes.indexOf(settings.boxIndex);
      if (index < 0) return;
      const horizontal = settings.horizontal;
      const categoryDim = horizontal ? 1 : 0;
      const coord = horizontal ? [api.value(1), api.value(0)] : [api.value(0), api.value(1)];
      const point = api.coord(coord);
      const band = Math.abs(api.size(horizontal ? [0, 1] : [1, 0])[categoryDim]);
      const available = band * 0.8 - 2;
      const gap = available / boxes.length * 0.3;
      const width = (available - gap * (boxes.length - 1)) / boxes.length;
      const offset = width / 2 - available / 2 + index * (gap + width);
      point[categoryDim] += offset + api.value(3) * Math.min(Math.max(width, 7), 50);
      return {
        type: "circle",
        shape: { cx: point[0], cy: point[1], r: settings.pointSize / 2 },
        style: { fill: api.visual("color"), opacity: settings.pointAlpha },
      };
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
