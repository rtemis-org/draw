/**
 * The ridge layout of the distribution builders: one row per sample on a
 * shared x range. spec: draw/shared-builders#histogram
 */

/** What a ridge layout needs from a built distribution chart. */
export interface RidgeInput {
  /** Option members every layout keeps: title, tooltip, colors, animation. */
  readonly common: Record<string, unknown>;
  /** Series, each named after its sample. */
  readonly series: readonly Record<string, unknown>[];
  /** Sample names, top to bottom. */
  readonly names: readonly string[];
  /** The x range every row shares. */
  readonly x: readonly [number, number];
  /** The lowest and highest height of every row. */
  readonly y: readonly [number, number];
  /** Bottom axis name; `null` draws none. */
  readonly xlab: string | null;
  /** Zoom every row's x axis together. */
  readonly zoom: boolean;
}

/**
 * One row per sample: each row's y axis is named after its sample and spans
 * the heights of every row, only the bottom row shows its x axis, and rows
 * zoom together without a slider. Each row's plotting box is its grid's
 * rectangle (`outerBoundsMode: "none"`), so the rows share one left edge and
 * one x scale whatever their labels' widths.
 */
export function ridgeOption(input: RidgeInput): Record<string, unknown> {
  const { names, xlab } = input;
  const rows = names.length;
  const [low, high] = input.y;
  return {
    ...input.common,
    legend: { show: false },
    grid: names.map((_, i) => ({
      left: 100,
      right: 30,
      top: `${8 + (i * 80) / rows}%`,
      height: `${64 / rows}%`,
      outerBoundsMode: "none",
    })),
    xAxis: names.map((_, i) => ({
      type: "value",
      min: input.x[0],
      max: input.x[1],
      gridIndex: i,
      show: i === rows - 1,
      ...(i === rows - 1 && xlab ? { name: xlab, nameLocation: "middle" } : {}),
    })),
    yAxis: names.map((name, i) => ({
      type: "value",
      min: low < 0 ? low * 1.05 : 0,
      max: high > 0 ? high * 1.05 : 1,
      gridIndex: i,
      name,
      nameLocation: "middle",
      nameGap: 55,
      splitNumber: 2,
      axisLabel: { showMinLabel: false, showMaxLabel: false },
    })),
    ...(input.zoom
      ? {
          dataZoom: [
            {
              type: "inside",
              xAxisIndex: names.map((_, i) => i),
              filterMode: "none",
              zoomOnMouseWheel: "shift",
              moveOnMouseMove: true,
            },
          ],
        }
      : {}),
    series: input.series.map((s) => {
      const row = names.indexOf(s.name as string);
      return { ...s, xAxisIndex: row, yAxisIndex: row };
    }),
  };
}
