/**
 * Grid margins and zoom presets shared by the cartesian builders.
 * spec: draw/shared-builders#appearance
 */

/** The four plot-margin members of a config. */
export interface MarginMembers {
  readonly margin_top?: number | null;
  readonly margin_right?: number | null;
  readonly margin_bottom?: number | null;
  readonly margin_left?: number | null;
}

/** Margin in pixels on every side when a config sets none. */
export const DEFAULT_MARGIN = 36;

const SIDES = ["top", "right", "bottom", "left"] as const;
type Side = (typeof SIDES)[number];

/**
 * The grid of a cartesian chart. With no margin member set, every side takes
 * {@link DEFAULT_MARGIN}; otherwise only the sides set are fixed. The grid's
 * outer bounds hold the axis labels, and the axis names when an axis has one.
 */
export function cartesianGrid(config: MarginMembers, axisNames: boolean): Record<string, unknown> {
  const set = SIDES.filter((side) => typeof config[`margin_${side}`] === "number");
  const grid: Record<string, unknown> = {};
  for (const side of SIDES) {
    const value = config[`margin_${side}`];
    if (set.length === 0) grid[side] = DEFAULT_MARGIN;
    else if (typeof value === "number") grid[side] = value;
  }
  grid.outerBoundsMode = "same";
  grid.outerBoundsContain = axisNames ? "all" : "axisLabel";
  return grid;
}

/** Identifies the slider of a zoom preset, so static export can return its band. */
export const ZOOM_SLIDER_ID = "rtemis-zoom-slider";

// ECharts' slider filler size, the brush handle drawn outside it, the gap to
// the plot, and its default grid margin on each side (SliderZoomView.ts).
const SLIDER_SIZE = 30;
const SLIDER_HANDLE = 7;
const SLIDER_GAP = 8;

/**
 * The zoom preset on one axis: a slider placed in a band the grid reserves on
 * the axis side, and an inside zoom on Shift+wheel with drag panning, so the
 * plain wheel scrolls the host page. Mutates `grid` to reserve the band.
 * spec: draw/zoom-controls#design
 */
export function axisZoom(
  axis: "x" | "y",
  grid: Record<string, unknown>,
): Record<string, unknown>[] {
  const side: Side = axis === "x" ? "bottom" : "right";
  const current = grid[side];
  const margin = typeof current === "number" ? current : axis === "x" ? 80 : 15;
  const index = axis === "x" ? { xAxisIndex: 0 } : { yAxisIndex: 0 };
  grid[side] = margin + SLIDER_HANDLE + SLIDER_SIZE + SLIDER_GAP;
  return [
    {
      type: "slider",
      id: ZOOM_SLIDER_ID,
      start: 0,
      end: 100,
      ...index,
      [side]: margin + SLIDER_HANDLE,
      ...(axis === "x" ? { height: SLIDER_SIZE } : { width: SLIDER_SIZE }),
    },
    { type: "inside", ...index, zoomOnMouseWheel: "shift", moveOnMouseMove: true },
  ];
}
