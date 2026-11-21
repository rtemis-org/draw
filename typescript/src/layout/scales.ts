/**
 * The color scale (`visualMap`) of a chart whose legend members place it,
 * and the vertical centering of a heatmap's scale on its cells. A color scale
 * placed by the members takes the same anchors and placement as a categorical
 * legend, vertical on the left or right and horizontal on the top or bottom;
 * its title is drawn above a vertical bar, and before a horizontal one when
 * both fit on one line. Reads ECharts' VisualMapOption (component/visualMap/
 * VisualMapModel.ts). spec: draw/shared-builders#keys
 */
import { fitHeatmap, type HeatmapHints } from "../charts/heatmap/config.ts";
import type { EChartsInstance, EChartsModule, LooseOption, OptionRecord } from "./instance.ts";
import { KEY_GAP } from "./keys.ts";
import { drawLegendTitle, measureLegendTitle } from "./legend.ts";

/** What the color scale layout reads from its host. */
export interface ColorScaleFitContext {
  /** The option the host set. */
  readonly option: OptionRecord;
  /** An edge or a corner of the plotting area; unset leaves the scale as built. */
  readonly position?: string | null;
  /** `outside` or `inside` the plotting area. */
  readonly placement?: string | null;
  readonly title?: string | null;
  /** A heatmap's hints: square cells and the right margin the builder reserved for the scale. */
  readonly heatmap?: Partial<HeatmapHints> | null;
}

interface Box {
  x: number;
  y: number;
  width: number;
  height: number;
}

/**
 * Places the color scale at the legend members' position: outside, the grid
 * gives up the scale's band; a heatmap's square cells are fitted again in the
 * narrowed area. Throws when the narrowed area leaves no room for the cells.
 */
export function layoutColorScale(
  echarts: EChartsModule,
  chart: EChartsInstance,
  context: ColorScaleFitContext,
): void {
  const position = context.position;
  const model = chart.getModel().getComponent("visualMap");
  if (!position || !model?.get("show")) return;
  const source = context.option as LooseOption;
  const hints = context.heatmap ?? {};
  const gap = 12;
  const width = chart.getWidth();
  const height = chart.getHeight();
  const vertical = position === "left" || position === "right";
  const inside = context.placement === "inside";
  const right = position.endsWith("right");
  const left = position.endsWith("left");
  const lower = position.startsWith("bottom");
  const bounds = (component: EChartsInstance): Box => {
    const group = chart.getViewOfComponentModel(component).group;
    const box = group.getBoundingRect().clone();
    box.applyTransform(group.getComputedTransform());
    return box;
  };
  const px = (v: unknown, extent: number) =>
    typeof v === "string" && v.endsWith("%")
      ? (Number.parseFloat(v) * extent) / 100
      : +(v as number) || 0;
  const grids: LooseOption[] = (Array.isArray(source.grid) ? source.grid : [source.grid]).map(
    (g: LooseOption) => ({ ...g }),
  );
  // Only the builder's automatic right-side allowance goes; margins the
  // caller set and dendrogram bands stay part of the plotting geometry.
  const reserved = hints.legendReserveRight || 0;
  for (const g of grids) {
    if (g.right != null) g.right = px(g.right, width) - reserved;
  }
  chart.setOption({
    grid: grids,
    visualMap: {
      orient: vertical ? "vertical" : "horizontal",
      left: 0,
      top: 0,
      right: null,
      bottom: null,
      itemHeight: Math.min(140, Math.max(30, (vertical ? height : width) - 160)),
    },
  });
  // A title above wraps at the wider of the bar and a quarter of the chart
  // (at most 160 pixels) when vertical, and at the chart width when horizontal.
  const native = bounds(model);
  // The bar's left edge in the measured scale, whose bounds start at its
  // padding and handles.
  let barX = native.x;
  chart.getViewOfComponentModel(model).group.traverse((el: EChartsInstance) => {
    if (el.isGroup || !el.style?.fill?.colorStops) return;
    const r = el.getBoundingRect().clone();
    r.applyTransform(el.getComputedTransform());
    barX = Math.min(barX === native.x ? r.x : barX, r.x);
  });
  let title = measureLegendTitle(echarts, chart, model, context.title);
  const inline = !!title && !vertical && title.width + 10 + native.width <= width - 2 * gap;
  if (title && !inline) {
    title = measureLegendTitle(
      echarts,
      chart,
      model,
      context.title,
      vertical
        ? Math.max(native.width, Math.min(160, width / 4)) - 2 * title.padLeft
        : width - 2 * gap,
    );
  }
  const indent = inline && title ? title.width + 10 : 0;
  const lead = title && !inline ? title.height + 2 : 0;
  const box = {
    width: Math.max(native.width + indent, lead && title ? title.width + 2 * title.padLeft : 0),
    height: Math.max(native.height + lead, title ? title.height : 0),
  };
  const side = vertical ? (right ? "right" : "left") : lower ? "bottom" : "top";
  let header = gap;
  chart.getModel().eachComponent("title", (component: EChartsInstance) => {
    const t = bounds(component);
    if (t.width && t.height) header = Math.max(header, t.y + t.height + gap);
  });
  const firstTop = Math.min(...grids.map((g) => px(g.top, height)));
  const extra = inside
    ? 0
    : side === "top"
      ? Math.max(0, header + box.height + gap - firstTop)
      : (vertical ? box.width : box.height) + gap;
  const adjusted: LooseOption = { ...hints, rightPx: (hints.rightPx ?? Number.NaN) - reserved };
  for (const g of grids) {
    if (g[side] != null) g[side] = px(g[side], vertical ? width : height) + extra;
  }
  const key = ({ top: "topPx", bottom: "botPx", left: "leftPx", right: "rightPx" } as const)[side];
  if (Number.isFinite(adjusted[key])) adjusted[key] += extra;
  chart.setOption({ grid: grids });
  if (hints.squareCells) {
    const cell = Math.min(
      (width - adjusted.leftPx - adjusted.rightPx) / adjusted.nCols,
      (height - adjusted.topPx - adjusted.botPx) / adjusted.nRows,
    );
    if (!(cell > 0)) {
      throw new Error("Increase figure dimensions to leave room for heatmap cells and margins.");
    }
    fitHeatmap(chart, { ...source, grid: grids }, adjusted as HeatmapHints);
  }
  const areas: Box[] = [];
  // The plotting area of the scale's series: a heatmap's cells, or a scatter
  // chart's points.
  model.eachTargetSeries((series: EChartsInstance) => {
    if (series.coordinateSystem?.type === "cartesian2d") {
      areas.push(series.coordinateSystem.getArea());
    }
  });
  const grid = areas[0];
  if (!grid) return;
  // An outside anchor sits beyond the axis and dendrogram bands.
  const dx = hints.squareCells
    ? Math.max(0, (width - adjusted.leftPx - adjusted.rightPx - grid.width) / 2)
    : 0;
  // An outside scale at the right sits KEY_GAP pixels right of the rightmost
  // plotting area, such as a row dendrogram, as the scatter chart's keys do.
  let plotRight = grid.x + grid.width;
  chart.getModel().eachComponent("grid", (g: EChartsInstance) => {
    const r = g.coordinateSystem?.getRect?.();
    if (r && r.width > 0) plotRight = Math.max(plotRight, r.x + r.width);
  });
  const x = vertical
    ? inside
      ? right
        ? grid.x + grid.width - box.width - gap
        : grid.x + gap
      : right
        ? plotRight + KEY_GAP - (barX - native.x) - (title ? 0 : native.x)
        : gap + dx
    : left
      ? grid.x + (inside ? gap : 0)
      : right
        ? grid.x + grid.width - box.width - (inside ? gap : 0)
        : grid.x + (grid.width - box.width) / 2;
  const y = vertical
    ? grid.y + (grid.height - box.height) / 2
    : inside
      ? lower
        ? grid.y + grid.height - box.height - gap
        : grid.y + gap
      : lower
        ? height - gap - box.height
        : header;
  if (title) {
    drawLegendTitle(
      chart,
      title,
      x + title.padLeft,
      y + (inline ? (native.height - title.height) / 2 : title.padTop),
    );
  }
  // Handles and end text extend beyond the layout box; with a title, the
  // measured box starts exactly at the place reserved below or after it.
  const shiftX = title ? indent - native.x : 0;
  const shiftY = title ? lead - native.y : 0;
  chart.setOption({
    visualMap: { left: x + shiftX, right: null, top: y + shiftY, bottom: null },
  });
}

/**
 * Centers each vertical color scale anchored at `middle` on the vertical
 * extent of its heatmap's cells, which titles, rotated labels, or dendrograms
 * offset from the chart's middle. Measures the drawn scale, its handles and
 * end text included.
 */
export function centerColorScales(
  chart: EChartsInstance,
  context: { readonly option: OptionRecord },
): void {
  const definitions = (context.option as LooseOption).visualMap;
  if (!definitions) return;
  const maps: LooseOption[] = Array.isArray(definitions) ? definitions : [definitions];
  let changed = false;
  const updates = maps.map((definition, index) => {
    if (!["middle", "center"].includes(definition.top)) return {};
    const model = chart.getModel().getComponent("visualMap", index);
    if (!model?.get("show") || model.get("orient") !== "vertical") return {};
    const areas: Box[] = [];
    model.eachTargetSeries((series: EChartsInstance) => {
      if (series.subType === "heatmap" && series.coordinateSystem?.type === "cartesian2d") {
        areas.push(series.coordinateSystem.getArea());
      }
    });
    if (!areas.length) return {};
    const top = Math.min(...areas.map((area) => area.y));
    const bottom = Math.max(...areas.map((area) => area.y + area.height));
    const view = chart.getViewOfComponentModel(model);
    const height = view.group.getBoundingRect().height;
    changed = true;
    return { top: (top + bottom - height) / 2 };
  });
  if (changed) chart.setOption({ visualMap: updates });
}
