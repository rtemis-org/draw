/**
 * The fixed-aspect plotting box of a chart with an aspect hint (`square`,
 * `equal_axes`, the ROC and calibration curves): the largest box of the
 * hint's ratio within its margins, centered, at most the hint's preferred
 * width. `fitAspectBox()` sets it on the option before the chart draws;
 * `fitAspectAxes()` constrains the drawn plotting area again after ECharts'
 * outer bounds reserved the axis labels and names, so labels are not clipped
 * and an identity line stays at 45 degrees.
 * spec: draw/chart-appearance#build-and-fit
 */
import type { AspectHint } from "../charts/line/config.ts";
import type { EChartsInstance, LooseOption, OptionRecord } from "./instance.ts";

/** An aspect hint, with the box's preferred width in pixels, if any. */
export interface AspectFitHint extends AspectHint {
  readonly widthPx?: number | null;
}

/**
 * Sets the option's one grid to the aspect box for a `width` × `height`
 * surface. Throws when the surface leaves no room for the box and margins.
 */
export function fitAspectBox(
  option: OptionRecord,
  aspect: AspectFitHint,
  width: number,
  height: number,
): void {
  const grid = option.grid as LooseOption | readonly unknown[] | undefined;
  if (!grid || Array.isArray(grid)) {
    throw new Error("A fixed-aspect panel requires one plotting grid.");
  }
  const a = aspect;
  if (
    ![a.ratio, a.leftPx, a.rightPx, a.topPx, a.botPx].every(Number.isFinite) ||
    a.ratio <= 0 ||
    [a.leftPx, a.rightPx, a.topPx, a.botPx].some((v) => v < 0)
  ) {
    throw new Error("Supply a positive aspect ratio and nonnegative pixel margins.");
  }
  const available = Math.min(width - a.leftPx - a.rightPx, (height - a.topPx - a.botPx) / a.ratio);
  const preferred = a.widthPx === undefined || a.widthPx === null ? available : a.widthPx;
  if (!Number.isFinite(preferred) || preferred <= 0 || available <= 0) {
    throw new Error("Increase panel dimensions to leave room for the plotting grid and margins.");
  }
  const box = grid as LooseOption;
  box.width = Math.min(available, preferred);
  box.height = box.width * a.ratio;
  box.left = a.leftPx + (width - a.leftPx - a.rightPx - box.width) / 2;
  box.top = a.topPx + (height - a.topPx - a.botPx - box.height) / 2;
}

/**
 * Constrains a drawn chart's plotting area to the aspect ratio within the
 * rectangle ECharts laid out, centered, and fixes it there.
 */
export function fitAspectAxes(chart: EChartsInstance, aspect: AspectHint): void {
  const rect = chart.getModel().getComponent("grid").coordinateSystem.getRect();
  const width = Math.min(rect.width, rect.height / aspect.ratio);
  const height = width * aspect.ratio;
  chart.setOption({
    grid: {
      left: rect.x + (rect.width - width) / 2,
      top: rect.y + (rect.height - height) / 2,
      width,
      height,
      containLabel: false,
      outerBoundsMode: "none",
    },
  });
}
