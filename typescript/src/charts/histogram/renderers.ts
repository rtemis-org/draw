/**
 * Custom-series renderer of the histogram, registered by name so the built
 * option stays JSON. See CustomSeriesRenderItem in ECharts
 * src/chart/custom/CustomSeries.ts. spec: draw/shared-builders#histogram
 */

interface Api {
  currentSeriesIndices(): number[];
  coord(value: number[]): number[];
  value(dimension: number): number;
  visual(name: string): string;
}
interface Params {
  readonly itemPayload: HistogramPayload;
  readonly seriesIndex: number;
  readonly dataIndex: number;
}

/** What each bar series carries for its renderer. */
export interface HistogramPayload {
  readonly fillAlpha: number;
  readonly borderAlpha: number;
  /** Group layout: overlaid, dodged within each bin, or stacked. */
  readonly mode: "overlay" | "group" | "stack";
  /** The series indices of every bar series, in drawing order. */
  readonly seriesIndices: readonly number[];
  /** Every bar series' heights, in the order of `seriesIndices`. */
  readonly heights: readonly (readonly number[])[];
}

/** Renderer name of a histogram bar series. */
export const HISTOGRAM = "rtemis.histogram.v1";

/** The histogram's renderers, by name. */
export const histogramRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> = {
  // Data: [lower edge, upper edge, height, count]. A bar spans its edges on
  // a numeric x axis, so unequal widths draw true and a density curve
  // aligns. Dodging and stacking follow the series the legend shows, so a
  // hidden group's bars give their room and their stack height back.
  [HISTOGRAM]: (params, api) => {
    const settings = params.itemPayload;
    const height = api.value(2);
    // An empty bin draws nothing, rather than its border along the axis.
    if (height === 0) return undefined;
    let lo = api.value(0);
    let hi = api.value(1);
    let base = 0;
    if (settings.mode !== "overlay") {
      const active = api.currentSeriesIndices();
      const shown = settings.seriesIndices.filter((i) => active.includes(i));
      const position = shown.indexOf(params.seriesIndex);
      if (position < 0) return undefined;
      if (settings.mode === "group") {
        const span = (hi - lo) / shown.length;
        lo += position * span;
        hi = lo + span;
      } else {
        for (const index of shown.slice(0, position)) {
          const row = settings.heights[settings.seriesIndices.indexOf(index)];
          const value = row?.[params.dataIndex] ?? 0;
          if ((height >= 0 && value >= 0) || (height < 0 && value < 0)) base += value;
        }
      }
    }
    const left = api.coord([lo, base + height]);
    const right = api.coord([hi, base]);
    const [x1, y1] = left as [number, number];
    const [x2, y2] = right as [number, number];
    const color = api.visual("color");
    return {
      type: "rect",
      shape: {
        x: Math.min(x1, x2),
        y: Math.min(y1, y2),
        width: Math.abs(x2 - x1),
        height: Math.abs(y2 - y1),
      },
      style: {
        fill: color,
        fillOpacity: settings.fillAlpha,
        stroke: color,
        strokeOpacity: settings.borderAlpha,
        lineWidth: settings.borderAlpha > 0 ? 1 : 0,
      },
    };
  },
};
