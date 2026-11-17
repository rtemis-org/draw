/**
 * Custom-series renderers of the scatter chart, registered by name so the
 * built option stays JSON. See CustomSeriesRenderItem in ECharts
 * src/chart/custom/CustomSeries.ts. spec: draw/shared-builders#scatter
 */

interface Api {
  coord(value: number[]): number[];
  value(dimension: number): number;
  visual(name: string): string;
}
interface Params {
  readonly coordSys: { x: number; y: number; width: number; height: number };
}

/** Renderer name of the rug: a mark on each axis at each point. */
export const RUG = "rtemis.rug.v1";

/** Length in pixels of a rug mark. */
const RUG_LENGTH = 6;

/** The scatter chart's renderers, by name. */
export const scatterRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> = {
  // A mark inside the plotting box at the bottom edge for x and at the left
  // edge for y, so the marks stay on the axes through zoom and resize.
  [RUG]: (params, api) => {
    const point = api.coord([api.value(0), api.value(1)]);
    const box = params.coordSys;
    const style = { stroke: api.visual("color"), opacity: 0.5, lineWidth: 1 };
    const px = point[0] as number;
    const py = point[1] as number;
    const bottom = box.y + box.height;
    return {
      type: "group",
      children: [
        { type: "line", shape: { x1: px, x2: px, y1: bottom, y2: bottom - RUG_LENGTH }, style },
        { type: "line", shape: { x1: box.x, x2: box.x + RUG_LENGTH, y1: py, y2: py }, style },
      ],
    };
  },
};
