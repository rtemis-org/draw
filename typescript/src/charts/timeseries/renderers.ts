/**
 * Custom-series renderer of the time series chart's zeitgeber labels,
 * registered by name so the built option stays JSON. See
 * CustomSeriesRenderItem in ECharts src/chart/custom/CustomSeries.ts.
 * spec: draw/shared-builders#time-series
 */

interface Api {
  coord(value: number[]): number[];
  value(dimension: number): number | string;
  font(style: { fontSize: number }): string;
}
interface Params {
  readonly coordSys: { x: number; y: number; width: number; height: number };
}

/** Renderer name of the zeitgeber labels. */
export const AXIS_LABELS = "rtemis.axis_labels.v1";

/** The time series chart's renderers, by name. */
export const timeSeriesRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> =
  {
    // Data: [time, text]. Each label sits 10 pixels below the plot under its
    // time's coordinate, so irregular spacing stays true through zoom and
    // resizing; a time outside the plot draws nothing.
    [AXIS_LABELS]: (params, api) => {
      const [x] = api.coord([api.value(0) as number, 0]) as [number, number];
      const box = params.coordSys;
      if (x < box.x - 1 || x > box.x + box.width + 1) return undefined;
      return {
        type: "text",
        style: {
          x,
          y: box.y + box.height + 10,
          text: String(api.value(1)),
          align: "center",
          verticalAlign: "top",
          fill: "#888888",
          font: api.font({ fontSize: 12 }),
        },
      };
    },
  };
