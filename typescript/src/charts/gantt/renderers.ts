/**
 * Custom-series renderer of the Gantt chart's bars, registered by name so the
 * built option stays JSON. See CustomSeriesRenderItem in ECharts
 * src/chart/custom/CustomSeries.ts.
 * spec: draw/shared-builders#gantt
 */

interface Api {
  coord(value: number[]): number[];
  size(value: number[]): number[] | number;
  value(dimension: number): number;
  visual(name: string): string;
}
interface Params {
  readonly itemPayload: GanttPayload;
}

/** What each Gantt series carries for its renderer. */
export interface GanttPayload {
  /** Bar thickness as a fraction of one row. */
  readonly barHeight: number;
  /** Bar corner radius in pixels. */
  readonly barRadius: number;
  /** Outline color of a flagged bar. */
  readonly borderColor: string;
  /** Outline width of a flagged bar in pixels. */
  readonly borderWidth: number;
}

/** Renderer name of the Gantt bars. */
export const GANTT_BAR = "rtemis.gantt.v1";

/** The Gantt chart's renderers, by name. */
export const ganttRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> = {
  // Data: [row, start, end, flag]. A bar spans start to end on its row, at
  // least one pixel wide, filled in its series color; a flag of 1 outlines
  // it without changing the fill.
  [GANTT_BAR]: (params, api) => {
    const p = params.itemPayload;
    const row = api.value(0);
    const [x0, y] = api.coord([api.value(1), row]) as [number, number];
    const [x1] = api.coord([api.value(2), row]) as [number, number];
    const size = api.size([0, 1]);
    const band = Array.isArray(size) ? (size[1] as number) : size;
    const height = band * p.barHeight;
    const style: Record<string, unknown> = { fill: api.visual("color") };
    if (api.value(3)) {
      style.stroke = p.borderColor;
      style.lineWidth = p.borderWidth;
    }
    return {
      type: "rect",
      transition: ["shape"],
      shape: { x: x0, y: y - height / 2, width: Math.max(1, x1 - x0), height, r: p.barRadius },
      style,
    };
  },
};
