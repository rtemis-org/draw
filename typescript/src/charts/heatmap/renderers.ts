/**
 * Custom-series renderers of the heatmap's dendrograms and color tracks,
 * registered by name so the built option stays JSON. See
 * CustomSeriesRenderItem in ECharts src/chart/custom/CustomSeries.ts.
 * spec: draw/shared-builders#heatmap
 */

interface Api {
  coord(value: number[]): number[];
  size(value: number[]): number[] | number;
  value(dimension: number): number;
}
interface Params {
  readonly itemPayload: Record<string, unknown>;
  readonly dataIndex: number;
  readonly coordSys: { x: number; y: number; width: number; height: number };
}

/** Renderer name of a dendrogram panel. */
export const DENDROGRAM = "rtemis.dendrogram.v1";
/** Renderer name of the color tracks beside the cells. */
export const HEATMAP_TRACKS = "rtemis.heatmap_tracks.v1";

/** Pixels of one track, and the gap before the next. */
export const TRACK_SIZE = 10;
export const TRACK_STEP = 12;

/** The heatmap's renderers, by name. */
export const heatmapRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> = {
  // Data: [left position, right position, left height, right height, merge
  // height]. Each merge draws one three-segment U shape, in its branch color
  // when the payload has one per merge.
  [DENDROGRAM]: (params, api) => {
    const p = params.itemPayload as {
      orientation: string;
      color: string;
      colors?: readonly string[];
    };
    const [lp, rp, lh, rh, mh] = [0, 1, 2, 3, 4].map((d) => api.value(d)) as number[];
    const points =
      p.orientation === "row"
        ? [
            [lh, lp],
            [mh, lp],
            [mh, rp],
            [rh, rp],
          ]
        : [
            [lp, lh],
            [lp, mh],
            [rp, mh],
            [rp, rh],
          ];
    return {
      type: "polyline",
      shape: { points: points.map((point) => api.coord(point as number[])) },
      style: { stroke: p.colors ? p.colors[params.dataIndex] : p.color, lineWidth: 1, fill: null },
    };
  },
  // Data: [index]. Each row (or column) draws one rectangle per track, left
  // of the cells (below them, or above when the column labels are on top).
  [HEATMAP_TRACKS]: (params, api) => {
    const p = params.itemPayload as {
      orientation: string;
      top: boolean;
      colors: readonly (readonly string[])[];
    };
    const index = api.value(0);
    const row = p.orientation === "row";
    const rect = params.coordSys;
    const center = api.coord(row ? [0, index] : [index, 0]);
    const raw = api.size([1, 1]);
    const size = Array.isArray(raw) ? raw : [raw, raw];
    const w = Math.abs(size[0] as number);
    const h = Math.abs(size[1] as number);
    return {
      type: "group",
      children: (p.colors[index] ?? []).map((color, track) => ({
        type: "rect",
        shape: row
          ? {
              x: rect.x - 4 - (track + 1) * TRACK_STEP,
              y: (center[1] as number) - h / 2,
              width: TRACK_SIZE,
              height: h,
            }
          : {
              x: (center[0] as number) - w / 2,
              y: p.top
                ? rect.y - 4 - (track + 1) * TRACK_STEP
                : rect.y + rect.height + 4 + track * TRACK_STEP,
              width: w,
              height: TRACK_SIZE,
            },
        style: { fill: color },
      })),
    };
  },
};
