/**
 * Measurement of a rendered chart's layout: the rectangles its plotting areas,
 * legend, color scale, and title occupy, in pixels of the chart's surface.
 * It reads the rendered state of an ECharts instance only, and none of the
 * values the layout passes (`fitLegend()`, `fitKeys()`, `placeColorScale()`)
 * compute, so tests of the hosts measure the result of the layout independently
 * of the code that produces it. spec: draw/shared-builders#layout-measurement
 */

/** A rectangle in pixels of the chart's surface. */
export interface LayoutRect {
  readonly x: number;
  readonly y: number;
  readonly width: number;
  readonly height: number;
}

/** The measured layout of a rendered chart. */
export interface MeasuredLayout {
  /** The chart's surface. */
  readonly pane: { readonly width: number; readonly height: number };
  /**
   * The plotting areas: each cartesian grid's rectangle; for a chart without
   * a grid, the drawn extent of each series (pie, Sankey, funnel, treemap,
   * sunburst, radar).
   */
  readonly plots: readonly LayoutRect[];
  /** Where `plots` come from: cartesian grids, series extents, or neither. */
  readonly plotsFrom: "grid" | "series" | "none";
  /** The drawn extent of the first legend, or null when none is shown. */
  readonly legend: LayoutRect | null;
  /** The drawn extent of the first color scale (`visualMap`), or null. */
  readonly colorScale: LayoutRect | null;
  /** The color scale's gradient bar, or null. */
  readonly colorScaleBar: LayoutRect | null;
  /** The drawn extent of the first title, or null when it has no text. */
  readonly title: LayoutRect | null;
}

interface Rect extends LayoutRect {
  clone(): Rect;
  applyTransform(m: unknown): void;
}
interface Element {
  isGroup?: boolean;
  invisible?: boolean;
  ignore?: boolean;
  style?: { fill?: unknown; stroke?: unknown; lineWidth?: unknown; text?: unknown };
  getBoundingRect(): Rect;
  getComputedTransform(): unknown;
}
interface Group extends Element {
  traverse(cb: (el: Element) => void): void;
}
interface Model {
  get(key: string): unknown;
  coordinateSystem?: { getRect?(): LayoutRect };
}
interface Chart {
  getWidth(): number;
  getHeight(): number;
  getModel(): {
    getComponent(type: string, index?: number): Model | undefined;
    getSeries?(): Model[];
  };
  getViewOfComponentModel(model: Model): { group: Group } | undefined;
  getViewOfSeriesModel?(model: Model): { group: Group } | undefined;
}

const round = (v: number) => Math.round(v * 10) / 10;
const rounded = (r: LayoutRect): LayoutRect => ({
  x: round(r.x),
  y: round(r.y),
  width: round(r.width),
  height: round(r.height),
});

/** A color that paints nothing: unset, `none`, `transparent`, or zero alpha. */
function blank(color: unknown): boolean {
  if (color === null || color === undefined || color === "none" || color === "transparent") {
    return true;
  }
  return typeof color === "string" && /^rgba\(.*,\s*0(\.0*)?\s*\)$/.test(color);
}

/** Whether an element paints anything: text, a fill, or a stroke of some width. */
function paints(el: Element): boolean {
  const style = el.style ?? {};
  if (typeof style.text === "string") return style.text.length > 0;
  if (!blank(style.fill)) return true;
  const width = typeof style.lineWidth === "number" ? style.lineWidth : 1;
  return !blank(style.stroke) && width > 0;
}

/** The union of the visible drawn elements of a group, in surface pixels. */
function extent(group: Group, keep: (el: Element) => boolean = () => true): LayoutRect | null {
  let x0 = Number.POSITIVE_INFINITY;
  let y0 = Number.POSITIVE_INFINITY;
  let x1 = Number.NEGATIVE_INFINITY;
  let y1 = Number.NEGATIVE_INFINITY;
  group.traverse((el) => {
    if (el.isGroup || el.invisible || el.ignore || !paints(el) || !keep(el)) return;
    const r = el.getBoundingRect().clone();
    r.applyTransform(el.getComputedTransform());
    if (!(r.width > 0 || r.height > 0)) return;
    x0 = Math.min(x0, r.x);
    y0 = Math.min(y0, r.y);
    x1 = Math.max(x1, r.x + r.width);
    y1 = Math.max(y1, r.y + r.height);
  });
  return Number.isFinite(x0) ? rounded({ x: x0, y: y0, width: x1 - x0, height: y1 - y0 }) : null;
}

/** The extent of a component's view, or null when it is hidden or absent. */
function componentExtent(
  chart: Chart,
  type: string,
  keep?: (el: Element) => boolean,
): LayoutRect | null {
  const model = chart.getModel().getComponent(type);
  if (!model || model.get("show") === false) return null;
  const view = chart.getViewOfComponentModel(model);
  return view ? extent(view.group, keep) : null;
}

/** A fill that is a gradient: the color scale's bar. */
const isGradient = (el: Element) => {
  const fill = el.style?.fill as { colorStops?: unknown } | undefined;
  return Boolean(fill && typeof fill === "object" && fill.colorStops);
};

/**
 * Measures the layout of a rendered chart: its surface, its plotting areas,
 * and the extents of its first legend, color scale (with its gradient bar),
 * and title. Rectangles are rounded to 0.1 pixel.
 */
export function measureLayout(chart: Chart): MeasuredLayout {
  const model = chart.getModel();
  const plots: LayoutRect[] = [];
  for (let i = 0; ; i++) {
    const grid = model.getComponent("grid", i);
    if (!grid) break;
    const rect = grid.coordinateSystem?.getRect?.();
    if (rect && rect.width > 0) plots.push(rounded(rect));
  }
  const fromGrid = plots.length > 0;
  if (!fromGrid && chart.getViewOfSeriesModel) {
    for (const series of model.getSeries?.() ?? []) {
      const view = chart.getViewOfSeriesModel(series);
      const rect = view ? extent(view.group) : null;
      if (rect) plots.push(rect);
    }
  }
  const scale = model.getComponent("visualMap");
  const scaleView =
    scale && scale.get("show") !== false ? chart.getViewOfComponentModel(scale) : undefined;
  return {
    pane: { width: chart.getWidth(), height: chart.getHeight() },
    plots,
    plotsFrom: fromGrid ? "grid" : plots.length > 0 ? "series" : "none",
    legend: componentExtent(chart, "legend"),
    colorScale: scaleView ? extent(scaleView.group) : null,
    colorScaleBar: scaleView ? extent(scaleView.group, isGradient) : null,
    title: componentExtent(chart, "title"),
  };
}
