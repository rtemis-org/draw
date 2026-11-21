/**
 * The categorical legend laid out from its members against the drawn
 * geometry: `position` (an edge or a corner of the plotting area),
 * `placement` (outside the plotting area, which gives up the legend's band,
 * or inside it), and `title`. ECharts' LegendView wraps the items into rows;
 * whole legend components pack into rows of their own, and a title sits
 * before a horizontal legend's first row, or above the items. ECharts
 * legends and color scales have no title field, so the title is one silent
 * graphic text element in the legend's font, bold. Reads ECharts'
 * LegendOption (component/legend/LegendModel.ts) and GridOption
 * (coord/cartesian/GridModel.ts). spec: draw/legend-title
 */
import type { AspectHint } from "../charts/line/config.ts";
import { fitAspectAxes } from "./aspect.ts";
import type { EChartsInstance, EChartsModule, LooseOption, OptionRecord } from "./instance.ts";

/** The id of the legend title's graphic element. */
export const LEGEND_TITLE_ID = "rtemis-legend-title";

/** A legend title measured in the legend's font, bold, and wrapped to a width. */
export interface LegendTitle {
  readonly text: string;
  readonly fontSize: number;
  readonly fontFamily: string;
  readonly fill: string;
  /** Height of one legend item line. */
  readonly line: number;
  readonly lineHeight: number;
  readonly width: number;
  readonly height: number;
  readonly padTop: number;
  readonly padLeft: number;
}

/**
 * The title `text` measured in the font of the component `model` (a legend
 * or a color scale), bold, with words wrapped greedily at `maxWidth`; a word
 * wider than it keeps its own line. `null` without a title.
 */
export function measureLegendTitle(
  echarts: EChartsModule,
  chart: EChartsInstance,
  model: EChartsInstance,
  text: string | null | undefined,
  maxWidth = Number.POSITIVE_INFINITY,
): LegendTitle | null {
  if (!text || !echarts) return null;
  const own = model.getModel("textStyle");
  const global = chart.getModel().getModel("textStyle");
  const fontSize = own.get("fontSize") || global.getShallow("fontSize") || 12;
  const fontFamily = own.get("fontFamily") || global.getShallow("fontFamily") || "sans-serif";
  const fill = own.get("color") || global.getShallow("color") || "#333";
  const font = `bold ${fontSize}px ${fontFamily}`;
  const regularFont = `${fontSize}px ${fontFamily}`;
  // Without a canvas (Node SVG export), zrender estimates widths from a
  // regular-weight Helvetica table and ignores the weight; bold Helvetica is
  // about 10% wider.
  const measure = (t: string) => {
    const width = echarts.format.getTextRect(t, font).width;
    return width === echarts.format.getTextRect(t, regularFont).width ? width * 1.1 : width;
  };
  const lines: string[] = [];
  let current = "";
  for (const word of String(text).split(/\s+/)) {
    const next = current ? `${current} ${word}` : word;
    if (current && measure(next) > maxWidth) {
      lines.push(current);
      current = word;
    } else {
      current = next;
    }
  }
  lines.push(current);
  const lineHeight = Math.ceil(echarts.format.getTextRect("Mg", font).height);
  const line = Math.max(
    model.get("itemHeight") || 14,
    echarts.format.getTextRect("Mg", regularFont).height,
  );
  const padding = ([] as number[]).concat(model.get("padding") ?? 5);
  return {
    text: lines.join("\n"),
    fontSize,
    fontFamily,
    fill,
    line,
    lineHeight,
    width: Math.ceil(Math.max(...lines.map(measure))),
    height: lines.length * lineHeight,
    padTop: padding[0] as number,
    padLeft: (padding.length === 4 ? padding[3] : padding[padding.length > 1 ? 1 : 0]) as number,
  };
}

/** Draws a measured legend title at `left`, `top`. */
export function drawLegendTitle(
  chart: EChartsInstance,
  title: LegendTitle,
  left: number,
  top: number,
): void {
  chart.setOption({
    graphic: {
      elements: [
        {
          id: LEGEND_TITLE_ID,
          type: "text",
          left,
          top,
          silent: true,
          z: 100,
          style: {
            text: title.text,
            fontSize: title.fontSize,
            fontFamily: title.fontFamily,
            fontWeight: "bold",
            fill: title.fill,
            lineHeight: title.lineHeight,
          },
        },
      ],
    },
  });
}

/** What the legend layout reads from its host: the legend members, and the aspect box. */
export interface LegendFitContext {
  /** The option the host set. */
  readonly option: OptionRecord;
  /** An edge or a corner of the plotting area; unset leaves the legend as built. */
  readonly position?: string | null;
  /** `outside` or `inside` the plotting area; unset is `inside`. */
  readonly placement?: string | null;
  readonly title?: string | null;
  /** The title sits above a horizontal legend's items rather than before its first row. */
  readonly titleAbove?: boolean;
  /** `visualMap` when the legend members place the color scale instead. */
  readonly target?: string | null;
  /** The fixed-aspect plotting box, which is fitted again in the narrowed area. */
  readonly aspect?: AspectHint | null;
}

interface Box {
  x: number;
  y: number;
  width: number;
  height: number;
}

export function layoutLegend(
  echarts: EChartsModule,
  chart: EChartsInstance,
  context: LegendFitContext,
): void {
  const position = context.position;
  if (!position || context.target === "visualMap") return;
  // A position without a placement keeps the inside placement of a corner.
  const inside = (context.placement || "inside") === "inside";
  const vertical = position === "left" || position === "right";
  const lower = position.startsWith("bottom");
  const right = position.endsWith("right");
  const left = position.endsWith("left");
  const width = chart.getWidth();
  const height = chart.getHeight();
  const gap = 12;
  const source = context.option as LooseOption;
  let models: EChartsInstance[] = [];
  chart.getModel().eachComponent("legend", (model: EChartsInstance) => {
    if (model.get("show") && model.getData().length) models.push(model);
  });
  if (!models.length) return;
  const bounds = (model: EChartsInstance): Box => {
    const group = chart.getViewOfComponentModel(model).group;
    const box = group.getBoundingRect().clone();
    box.applyTransform(group.getComputedTransform());
    return box;
  };
  const px = (value: unknown, extent: number, fallback: number) =>
    typeof value === "number"
      ? value
      : typeof value === "string" && value.endsWith("%")
        ? (Number.parseFloat(value) * extent) / 100
        : fallback;
  let header = gap;
  for (const type of ["title", "toolbox"]) {
    chart.getModel().eachComponent(type, (model: EChartsInstance) => {
      const box = bounds(model);
      if (box.width && box.height && box.y < height / 2) {
        header = Math.max(header, box.y + box.height + gap);
      }
    });
  }
  // Each pass starts from the authored margins, so repeated resizes do not
  // accumulate legend space; the aspect fit then constrains the plot.
  let grids: LooseOption[] = source.grid
    ? (Array.isArray(source.grid) ? source.grid : [source.grid]).map((g: LooseOption) => ({
        ...g,
      }))
    : [];
  const aspect = context.aspect;
  if (aspect) {
    grids = [
      {
        ...grids[0],
        left: aspect.leftPx,
        right: aspect.rightPx,
        top: aspect.topPx,
        bottom: aspect.botPx,
        width: null,
        height: null,
      },
    ];
  }
  const series: LooseOption[] = Array.isArray(source.series) ? source.series : [source.series];
  const isGantt = series.some((s) => s?.renderItem === "rtemis.gantt.v1");
  if (isGantt) {
    grids = grids.map((g) => ({ ...g, right: 16, top: Math.max(g.top, header) }));
  }
  if (grids.length) chart.setOption({ grid: grids });
  const area = (): Box => {
    const model = chart.getModel().getComponent("grid");
    return model
      ? model.coordinateSystem.getRect()
      : { x: gap, y: header, width: width - 2 * gap, height: height - header - gap };
  };
  const initial = area();
  const availableWidth = vertical
    ? Math.max(60, Math.min(initial.width * 0.4, 240))
    : Math.max(1, initial.width - (inside ? 2 * gap : 0));
  // A title sits before the first row of a horizontal legend, with the rows
  // indented after it, unless it takes more than half the width or the
  // context sets `titleAbove`; otherwise it sits above the items.
  let title = measureLegendTitle(echarts, chart, models[0], context.title);
  const inline =
    !!title && !vertical && !context.titleAbove && title.width + 10 <= availableWidth / 2;
  if (title && !inline) {
    title = measureLegendTitle(
      echarts,
      chart,
      models[0],
      context.title,
      availableWidth - 2 * title.padLeft,
    );
  }
  const indent = inline && title ? title.width + 10 : 0;
  const lead = title && !inline ? title.height + 2 : 0;
  const itemsWidth = availableWidth - indent;
  const availableHeight = Math.max(1, initial.height - (inside ? 2 * gap : 0) - lead);
  const legends = models.map((model) => {
    const font = model.getModel("textStyle").getFont();
    const natural = Math.max(
      0,
      ...model
        .getData()
        .map((item: EChartsInstance) => echarts.format.getTextRect(item.get("name"), font).width),
    );
    const textWidth = Math.max(1, itemsWidth - model.get("itemWidth") - 15);
    return {
      id: model.id,
      orient: vertical ? "vertical" : "horizontal",
      left: 0,
      right: null,
      top: 0,
      bottom: null,
      width: itemsWidth,
      height: vertical ? availableHeight : 1000000,
      textStyle: { width: natural > textWidth ? textWidth : null, overflow: "break" },
    };
  });
  chart.setOption({ legend: legends });
  models = models.map((model) => chart.getModel().getComponent("legend", model.componentIndex));
  const boxes = models.map(bounds);
  // Annotation families stay together, and whole components pack into
  // horizontal rows rather than each taking a full-width band.
  const rows: { items: { index: number; offset: number }[]; width: number; height: number }[] = [];
  boxes.forEach((box, index) => {
    let row = rows[rows.length - 1];
    if (!row || vertical || row.width + 16 + box.width > itemsWidth) {
      row = { items: [], width: 0, height: 0 };
      rows.push(row);
    }
    row.items.push({ index, offset: row.width + (row.items.length ? 16 : 0) });
    row.width += box.width + (row.items.length > 1 ? 16 : 0);
    row.height = Math.max(row.height, box.height);
  });
  const itemsHeight = rows.reduce((sum, row) => sum + row.height, 0) + (rows.length - 1) * 6;
  const legendHeight = itemsHeight + lead;
  const legendWidth = Math.max(
    indent + Math.max(...rows.map((row) => row.width)),
    title && !inline ? title.width + 2 * title.padLeft : 0,
  );
  const extra = vertical ? legendWidth + gap : legendHeight + gap;
  if (!inside) {
    grids = grids.map((g) => {
      const next = { ...g };
      if (vertical) {
        const side = right ? "right" : "left";
        next[side] = px(g[side], width, 36) + extra;
      } else if (lower) {
        next.bottom = px(g.bottom, height, 36) + extra;
      } else {
        next.top = Math.max(px(g.top, height, 36), header + extra);
      }
      return next;
    });
    const update: LooseOption = {};
    if (grids.length) {
      update.grid = grids;
    } else {
      // Box-layout series such as a pie reserve the same outside bands.
      update.series = series.map((s) => {
        if (s?.type !== "pie") return {};
        return {
          left: gap + (vertical && left ? extra : 0),
          right: gap + (vertical && right ? extra : 0),
          top: header + (!vertical && !lower ? extra : 0),
          bottom: gap + (!vertical && lower ? extra : 0),
        };
      });
    }
    // A bottom legend sits below the slider controls, which move up as a band.
    const zooms: LooseOption[] | null = source.dataZoom
      ? Array.isArray(source.dataZoom)
        ? source.dataZoom
        : [source.dataZoom]
      : null;
    if (!vertical && lower && zooms) {
      update.dataZoom = zooms.map((z) =>
        z.type === "slider" ? { bottom: px(z.bottom, height, 0) + extra } : {},
      );
    } else if (zooms) {
      update.dataZoom = zooms.map((z) => (z.type === "slider" ? { bottom: z.bottom ?? null } : {}));
    }
    chart.setOption(update);
  }
  if (aspect) fitAspectAxes(chart, aspect);
  const grid = area();
  let top = inside
    ? lower
      ? grid.y + grid.height - legendHeight - gap
      : vertical
        ? grid.y + (grid.height - legendHeight) / 2
        : grid.y + gap
    : vertical
      ? grid.y + (grid.height - legendHeight) / 2
      : lower
        ? height - gap - legendHeight
        : header;
  const rowX = (w: number) =>
    vertical
      ? inside
        ? right
          ? grid.x + grid.width - w - gap
          : grid.x + gap
        : right
          ? width - gap - w
          : gap
      : right
        ? grid.x + grid.width - w - (inside ? gap : 0)
        : left
          ? grid.x + (inside ? gap : 0)
          : grid.x + (grid.width - w) / 2;
  // With a title, rows align to the left edge of the whole legend block.
  const blockX = rowX(legendWidth);
  if (title) {
    drawLegendTitle(
      chart,
      title,
      blockX + title.padLeft,
      top + title.padTop + (inline ? (title.line - title.height) / 2 : 0),
    );
  }
  top += lead;
  const updates: LooseOption[] = [];
  for (const row of rows) {
    const x = title ? blockX + indent : rowX(row.width);
    for (const item of row.items) {
      updates.push({
        id: models[item.index].id,
        left: x + item.offset,
        right: null,
        top,
        bottom: null,
      });
    }
    top += row.height + 6;
  }
  chart.setOption({ legend: updates });
}
