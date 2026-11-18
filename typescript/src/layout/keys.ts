/**
 * The keys of a chart's continuous encodings: the color scale of a `color`
 * column (an ECharts `visualMap`) and the size key of a `size` column (a
 * `graphic` group). A builder places them statically in a column right of
 * the plot; `fitKeys()` places them from measured geometry after the chart
 * renders and after every resize, beside the plot or inside it, on the side a
 * host asks for. spec: draw/shared-builders#keys
 */
import { formatNumber } from "../shared/format.ts";

/** The id of the size key's `graphic` group. */
export const SIZE_KEY_ID = "rtemis-size-key";

/** Gap in pixels between the keys, and between the keys and the plot. */
export const KEY_GAP = 16;
const GAP = KEY_GAP;
/** Gap in pixels between a key's symbol and its label. */
const LABEL_GAP = 6;
/**
 * The text of both keys: titles in bold at the chart's font size, labels at
 * the color scale's, and the gap between a title and the top of its key's
 * first symbol (the bar's cap, the largest circle).
 */
export interface KeyText {
  readonly fontFamily: string;
  readonly titleSize: number;
  readonly labelSize: number;
  readonly color: string;
  readonly titleGap: number;
}

/** Gap in pixels between a key's title and its first symbol, unless a host sets one. */
export const KEY_TITLE_GAP = 8;

/** Key text before a host's theme is known: legible on light and dark backgrounds. */
export const DEFAULT_KEY_TEXT: KeyText = Object.freeze({
  fontFamily: "sans-serif",
  titleSize: 12,
  labelSize: 12,
  color: "#888888",
  titleGap: KEY_TITLE_GAP,
});
/** Fill of the size key's circles. */
const CIRCLE_COLOR = "#808080";
/** Height in pixels of the color scale's bar. */
export const COLOR_SCALE_LENGTH = 140;
/** Width in pixels the builder reserves right of the plot for the keys. */
export const KEY_COLUMN_WIDTH = 100;

/** What the size key shows: its title, and each reference value with its symbol diameter. */
export interface SizeKeySpec {
  readonly title: string;
  readonly items: readonly { readonly value: number; readonly diameter: number }[];
}

/** The orientation of a key: stacked down a side, or in a row along the top or bottom. */
export type KeyOrient = "vertical" | "horizontal";

/** A side of the plot the keys sit on, and whether outside or inside the plot. */
export interface KeyPlacement {
  readonly side: "left" | "right" | "top" | "bottom";
  readonly inside: boolean;
}

/**
 * Round reference values for a size key over `[min, max]`: multiples of the
 * 1, 2, or 5 step nearest a third of the range, the largest three, largest
 * first; the range's ends when fewer than two multiples fall in it.
 */
export function sizeKeyValues(min: number, max: number): number[] {
  if (!(max > 0)) return [];
  const lo = Math.max(0, min);
  const span = max - lo;
  if (span <= 0) return [max];
  const raw = span / 3;
  const magnitude = 10 ** Math.floor(Math.log10(raw));
  const step = [1, 2, 5, 10]
    .map((m) => m * magnitude)
    .reduce((best, s) => (Math.abs(Math.log(s / raw)) < Math.abs(Math.log(best / raw)) ? s : best));
  const values: number[] = [];
  for (let v = Math.ceil(lo / step) * step; v <= max + step * 1e-9; v += step) {
    if (v > 0) values.push(Number(v.toPrecision(12)));
  }
  const chosen = values.length >= 2 ? values.slice(-3) : [lo > 0 ? lo : max / 2, max];
  return [...new Set(chosen)].sort((a, b) => b - a);
}

/** Text width estimate in pixels, for the builder's layout before a renderer measures. */
function textWidth(text: string, size: number): number {
  return text.length * size * 0.6;
}

/** Height in pixels of a key's title line, with the gap below it to the first symbol. */
function titleHeight(text: KeyText): number {
  return Math.ceil(text.titleSize * 1.2) + text.titleGap;
}

/**
 * The size key as a `graphic` group: its title, then one circle and label per
 * reference value, stacked (`vertical`) or in a row (`horizontal`). `at` sets
 * the group's position. Stacked, `columns` sets the x of the circles' common
 * center and of the labels' left edge, relative to the group, so the key
 * lines up with a color scale above it; unset, the circles share the largest
 * one's center and the labels follow it.
 */
export function sizeKeyElement(
  spec: SizeKeySpec,
  orient: KeyOrient,
  at: Record<string, unknown>,
  text: KeyText = DEFAULT_KEY_TEXT,
  labelWidths?: readonly number[],
  columns?: { readonly center: number; readonly label: number },
): Record<string, unknown> {
  const largest = Math.max(...spec.items.map((i) => i.diameter), 1);
  const lead = titleHeight(text);
  // Stable ids let a later layout pass merge into the drawn key.
  const children: Record<string, unknown>[] = [
    {
      id: `${SIZE_KEY_ID}-title`,
      type: "text",
      x: 0,
      y: 0,
      style: {
        text: spec.title,
        fontSize: text.titleSize,
        fontFamily: text.fontFamily,
        fontWeight: "bold",
        fill: text.color,
        verticalAlign: "top",
      },
    },
  ];
  let x = 0;
  let y = lead;
  for (const [i, item] of spec.items.entries()) {
    const label = String(formatNumber(item.value));
    const vertical = orient === "vertical";
    const cx = vertical && columns ? columns.center : largest / 2;
    const cy = vertical ? y + largest / 2 : lead + largest / 2;
    const left = vertical ? 0 : x;
    const labelX = vertical && columns ? columns.label : left + largest + LABEL_GAP;
    children.push(
      {
        id: `${SIZE_KEY_ID}-circle-${i}`,
        type: "circle",
        shape: { cx: left + cx, cy, r: item.diameter / 2 },
        style: { fill: CIRCLE_COLOR, opacity: 0.6 },
      },
      {
        id: `${SIZE_KEY_ID}-label-${i}`,
        type: "text",
        x: labelX,
        y: cy,
        style: {
          text: label,
          fontSize: text.labelSize,
          fontFamily: text.fontFamily,
          fill: text.color,
          verticalAlign: "middle",
        },
      },
    );
    if (orient === "vertical") y += largest + LABEL_GAP;
    else x += largest + LABEL_GAP + (labelWidths?.[i] ?? textWidth(label, text.labelSize)) + GAP;
  }
  return {
    id: SIZE_KEY_ID,
    type: "group",
    silent: true,
    z: 100,
    info: { sizeKey: spec },
    children,
    ...at,
  };
}

/** Height in pixels of a vertical size key, for the builder's static layout. */
export function sizeKeyHeight(spec: SizeKeySpec, text: KeyText = DEFAULT_KEY_TEXT): number {
  const largest = Math.max(...spec.items.map((i) => i.diameter), 1);
  return titleHeight(text) + spec.items.length * (largest + LABEL_GAP);
}

// The parts of ECharts the layout reads; the package does not depend on its types.
interface Rect {
  x: number;
  y: number;
  width: number;
  height: number;
  clone(): Rect;
  applyTransform(m: unknown): void;
}
interface Element {
  isGroup?: boolean;
  invisible?: boolean;
  type?: string;
  style?: { fill?: unknown; text?: unknown };
  getBoundingRect(): Rect;
  getComputedTransform(): unknown;
}
interface View {
  group: Element & { traverse(cb: (el: Element) => void): void };
}
interface ComponentModel {
  get(key: string): unknown;
  getModel?(path: string): ComponentModel;
}
interface Chart {
  getOption(): Record<string, unknown>;
  getWidth(): number;
  getHeight(): number;
  setOption(option: Record<string, unknown>): void;
  getModel(): {
    getComponent(type: string, index?: number): ComponentModel | undefined;
    getModel?(path: string): ComponentModel;
  };
  getViewOfComponentModel(model: ComponentModel): View | undefined;
}
/** The ECharts module: text measurement for the size key. */
interface ECharts {
  format: { getTextRect(text: string, font: string): { width: number; height: number } };
}

/** The id of the color scale's title. */
export const COLOR_KEY_TITLE_ID = "rtemis-color-key-title";

/** The side each chart's keys took space on, so a later pass can return it. */
const reserved = new WeakMap<object, KeyPlacement["side"]>();

/** What `fitKeys()` needs from its host. */
export interface KeyContext {
  /** The option the host set: its grid margins are the authored ones each pass starts from. */
  readonly option: Record<string, unknown>;
  /** Where the keys go; unset, right of the plot. */
  readonly placement?: KeyPlacement;
  /** Gap in pixels between each key's title and its first symbol; unset, {@link KEY_TITLE_GAP}. */
  readonly titleGap?: number;
  /**
   * Called after the grid gives up the keys' space and before the keys are
   * placed, for a host that refits the plot in the narrowed area (a fixed
   * aspect box).
   */
  readonly afterReserve?: () => void;
}

/** The keys' side, and the space added to the grid on it (0 inside the plot). */
export interface KeyReserve {
  readonly side: KeyPlacement["side"];
  readonly extra: number;
}

/**
 * The keys' placement for a legend position: a side for `left` and `right`
 * and the corners on them, a row for `top` and `bottom`. With a categorical
 * legend, the keys take the right side, or the left when the legend is on the
 * right, outside the plot.
 */
export function keyPlacement(
  legendPosition: string | null | undefined,
  legendPlacement: string | null | undefined,
  hasLegend: boolean,
): KeyPlacement {
  const position = legendPosition ?? "right";
  const side: KeyPlacement["side"] = position.endsWith("right")
    ? "right"
    : position.endsWith("left")
      ? "left"
      : position === "bottom"
        ? "bottom"
        : "top";
  if (!hasLegend) return { side, inside: legendPlacement === "inside" };
  return { side: side === "right" ? "left" : "right", inside: false };
}

/**
 * The keys' text from the chart: the color scale's label style when there is
 * a scale, otherwise the chart's text style, so both keys read alike and
 * follow the host's theme and font.
 */
function keyText(chart: Chart, titleGap: number): KeyText {
  const global = chart.getModel().getModel?.("textStyle");
  const scale = chart.getModel().getComponent("visualMap")?.getModel?.("textStyle");
  const pick = (key: string): unknown => scale?.get(key) ?? global?.get(key);
  const number = (v: unknown, fallback: number) => (typeof v === "number" && v > 0 ? v : fallback);
  const titleSize = number(global?.get("fontSize"), DEFAULT_KEY_TEXT.titleSize);
  return {
    fontFamily: String(pick("fontFamily") ?? DEFAULT_KEY_TEXT.fontFamily),
    titleSize,
    labelSize: number(pick("fontSize"), titleSize),
    color: String(pick("color") ?? DEFAULT_KEY_TEXT.color),
    titleGap,
  };
}

/** Whether a fill draws nothing: unset, `none`, `transparent`, or an `rgba()` of alpha 0. */
function transparent(fill: unknown): boolean {
  if (fill === undefined || fill === null || fill === "none" || fill === "transparent") return true;
  return typeof fill === "string" && /^rgba\(.*,\s*0(\.0*)?\s*\)$/.test(fill);
}

/** Text width in pixels as the renderer measures it; bold widened where it measures regular weight. */
function measurer(echarts: ECharts, text: KeyText) {
  const regular = (size: number) => `${size}px ${text.fontFamily}`;
  return (t: string, size: number, bold = false): number => {
    const width = echarts.format.getTextRect(
      t,
      bold ? `bold ${regular(size)}` : regular(size),
    ).width;
    return bold && width === echarts.format.getTextRect(t, regular(size)).width
      ? width * 1.1
      : width;
  };
}

/** The size key's width and height in pixels, with measured text, and its label widths. */
function sizeKeyBox(
  echarts: ECharts,
  spec: SizeKeySpec,
  orient: KeyOrient,
  text: KeyText,
): { w: number; h: number; labels: number[] } {
  const measure = measurer(echarts, text);
  const title = measure(spec.title, text.titleSize, true);
  const largest = Math.max(...spec.items.map((i) => i.diameter), 1);
  const labels = spec.items.map((i) => measure(String(formatNumber(i.value)), text.labelSize));
  const lead = titleHeight(text);
  if (orient === "vertical") {
    const w = Math.max(title, largest + LABEL_GAP + Math.max(0, ...labels));
    return { w, h: lead + spec.items.length * (largest + LABEL_GAP) - LABEL_GAP, labels };
  }
  const row = labels.reduce((sum, l) => sum + largest + LABEL_GAP + l + GAP, 0) - GAP;
  return { w: Math.max(title, row), h: lead + largest, labels };
}

/**
 * Place the color scale and the size key from measured geometry: stacked on
 * a side, or in a row on the top or bottom, outside the plot (the grid gives
 * up the space) or inside it, aligned to the plot area. Both keys read alike:
 * a bold title above, the symbols on the left or top, the labels beside or
 * below them, in the chart's font and the scale's label color. Each pass
 * starts from the authored grid margin on the keys' side, so repeated resizes
 * do not accumulate. Returns the space taken, `null` without keys.
 */
export function fitKeys(echarts: ECharts, chart: Chart, context: KeyContext): KeyReserve | null {
  const option = context.option;
  const placement = context.placement ?? { side: "right", inside: false };
  const vertical = placement.side === "left" || placement.side === "right";
  const orient: KeyOrient = vertical ? "vertical" : "horizontal";
  const graphics = [option.graphic ?? []]
    .flat()
    .flatMap((g) => (g as { elements?: unknown[] }).elements ?? [g]) as Record<string, unknown>[];
  const sizeSpec = (graphics.find((g) => g.id === SIZE_KEY_ID)?.info as { sizeKey?: SizeKeySpec })
    ?.sizeKey;
  const scaleOption = [option.visualMap ?? []].flat()[0] as Record<string, unknown> | undefined;
  const scaleModel = chart.getModel().getComponent("visualMap");
  const hasScale = Boolean(scaleOption && scaleModel && scaleModel.get("show") !== false);
  if (!sizeSpec && !hasScale) return null;
  const scaleTitle = String(([scaleOption?.text ?? []].flat()[0] as string | undefined) ?? "");
  const text = keyText(chart, context.titleGap ?? KEY_TITLE_GAP);

  const width = chart.getWidth();
  const height = chart.getHeight();

  // Orient the scale for the side, its labels beside or below the bar as the
  // size key's are, and measure it; its title is drawn above it.
  let scaleBox: Rect | null = null;
  // Where the bar and the end labels sit in the measured scale.
  let bar: Rect | null = null;
  let labelsLeft: number | null = null;
  // The top of the scale's drawn symbol (bar and handle caps), below its bounds' top.
  let symbolTop: number | null = null;
  if (hasScale) {
    chart.setOption({
      visualMap: {
        orient,
        // ECharts' `align` names the bar's side: left of the labels when
        // stacked, above them in a row.
        align: vertical ? "left" : "top",
        itemHeight: COLOR_SCALE_LENGTH,
        text: ["", ""],
        left: 0,
        top: 0,
        right: null,
        bottom: null,
      },
    });
    const model = chart.getModel().getComponent("visualMap");
    const view = model && chart.getViewOfComponentModel(model);
    if (view) {
      scaleBox = view.group.getBoundingRect().clone();
      scaleBox.applyTransform(view.group.getComputedTransform());
      const absolute = (el: Element) => {
        const r = el.getBoundingRect().clone();
        r.applyTransform(el.getComputedTransform());
        return r;
      };
      view.group.traverse((el) => {
        if (el.isGroup) return;
        const fill = el.style?.fill as { colorStops?: unknown } | undefined;
        if (fill && typeof fill === "object" && fill.colorStops) {
          const r = absolute(el);
          if (!bar || r.width * r.height > bar.width * bar.height) bar = r;
          symbolTop = symbolTop === null ? r.y : Math.min(symbolTop, r.y);
        } else if (el.type === "text" && el.style?.text) {
          const r = absolute(el);
          labelsLeft = labelsLeft === null ? r.x : Math.min(labelsLeft, r.x);
        } else if (!el.invisible && !transparent(el.style?.fill)) {
          // The bar's caps; the scale's transparent background spans its bounds.
          const r = absolute(el);
          if (r.width > 0 && r.height > 0) {
            symbolTop = symbolTop === null ? r.y : Math.min(symbolTop, r.y);
          }
        }
      });
    }
  }
  // The scale's symbol starts one title line and the title gap below the
  // title, as the size key's first circle does.
  const symbolOffset = scaleBox && symbolTop !== null ? symbolTop - (scaleBox as Rect).y : 0;
  const scaleLead = scaleTitle ? titleHeight(text) - symbolOffset : 0;
  const scaleTitleWidth = scaleTitle
    ? measurer(echarts, text)(scaleTitle, text.titleSize, true)
    : 0;
  const parts: { kind: "scale" | "size"; w: number; h: number; dx: number; dy: number }[] = [];
  if (scaleBox) {
    parts.push({
      kind: "scale",
      w: Math.max(scaleBox.width, scaleTitleWidth),
      h: scaleBox.height + scaleLead,
      dx: scaleBox.x,
      dy: scaleBox.y,
    });
  }
  const sized = sizeSpec ? sizeKeyBox(echarts, sizeSpec, orient, text) : null;
  if (sized) parts.push({ kind: "size", w: sized.w, h: sized.h, dx: 0, dy: 0 });
  // Stacked, the keys share two columns, measured from the block's left edge:
  // the bar and the circles centered on one axis, and the scale's labels and
  // the size key's labels starting at one x.
  let columns: { center: number; label: number; scaleLeft: number } | null = null;
  // The bar's left edge in the block, which an outside block at the right
  // places KEY_GAP right of the plot.
  let barLead = 0;
  if (vertical && scaleBox) {
    const box: Rect = scaleBox;
    const b: Rect = bar ?? box;
    const largest = sizeSpec ? Math.max(...sizeSpec.items.map((i) => i.diameter), 1) : 0;
    const symbol = Math.max(b.width, largest);
    const barOffset = b.x - box.x;
    // The scale's left edge with the bar on the axis, never left of the block.
    const scaleLeft = Math.max(0, symbol / 2 - b.width / 2 - barOffset);
    const center = scaleLeft + barOffset + b.width / 2;
    // The size key's labels start where the scale's do, or clear of a circle
    // wider than the bar: ECharts sets the scale's label offset itself.
    const scaleLabel =
      labelsLeft === null ? center + b.width / 2 + LABEL_GAP : scaleLeft + (labelsLeft - box.x);
    const label = Math.max(scaleLabel, center + largest / 2 + LABEL_GAP);
    columns = { center, label, scaleLeft };
    barLead = center - b.width / 2;
    for (const part of parts) {
      if (part.kind === "scale") part.w = Math.max(scaleLeft + box.width, scaleTitleWidth);
      else if (sized && sizeSpec) {
        const titleWidth = measurer(echarts, text)(sizeSpec.title, text.titleSize, true);
        part.w = Math.max(titleWidth, center + largest / 2, label + Math.max(0, ...sized.labels));
      }
    }
  }
  if (parts.length === 0) return null;
  const blockW = vertical
    ? Math.max(...parts.map((p) => p.w))
    : parts.reduce((sum, p) => sum + p.w, 0) + GAP * (parts.length - 1);
  const blockH = vertical
    ? parts.reduce((sum, p) => sum + p.h, 0) + GAP * (parts.length - 1)
    : Math.max(...parts.map((p) => p.h));

  // Reserve the space on the keys' side from the authored margin.
  const authored = ([option.grid ?? {}].flat()[0] ?? {}) as Record<string, unknown>;
  const px = (v: unknown, extent: number, fallback: number) =>
    typeof v === "number"
      ? v
      : typeof v === "string" && v.endsWith("%")
        ? (Number.parseFloat(v) * extent) / 100
        : fallback;
  const side = placement.side;
  const extra = placement.inside ? 0 : (vertical ? blockW : blockH) + GAP;
  const margin = (s: KeyPlacement["side"]) =>
    px(authored[s], s === "left" || s === "right" ? width : height, 36);
  // A side an earlier pass reserved returns to its authored margin.
  const before = reserved.get(chart);
  const grid: Record<string, unknown> = {};
  if (before && before !== side) grid[before] = margin(before);
  if (!placement.inside) {
    // An explicit size from an earlier fit would override the margin.
    Object.assign(grid, { [side]: margin(side) + extra, width: null, height: null });
  }
  if (Object.keys(grid).length > 0) chart.setOption({ grid });
  if (placement.inside) reserved.delete(chart);
  else reserved.set(chart, side);
  context.afterReserve?.();
  const box = (type: string): Rect | null => {
    const model = chart.getModel().getComponent(type);
    const view = model && chart.getViewOfComponentModel(model);
    if (!view) return null;
    const r = view.group.getBoundingRect().clone();
    r.applyTransform(view.group.getComputedTransform());
    return r.width > 0 && r.height > 0 ? r : null;
  };
  // Axis labels and names can sit outside the grid's margin (an axis name
  // beyond contained labels); the keys then take that much more space.
  let reservedExtra = extra;
  if (!placement.inside && (side === "left" || side === "bottom")) {
    const axis = box(side === "left" ? "yAxis" : "xAxis");
    const deficit = !axis
      ? 0
      : side === "left"
        ? GAP + blockW + GAP / 2 - axis.x
        : axis.y + axis.height + GAP / 2 + blockH - (height - GAP);
    if (deficit > 0) {
      reservedExtra += deficit;
      chart.setOption({ grid: { [side]: margin(side) + reservedExtra } });
      context.afterReserve?.();
    }
  }
  const gridModel = chart.getModel().getComponent("grid") as unknown as
    | { coordinateSystem?: { getRect(): { x: number; y: number; width: number; height: number } } }
    | undefined;
  const area = gridModel?.coordinateSystem?.getRect() ?? {
    x: GAP,
    y: GAP,
    width: width - 2 * GAP,
    height: height - 2 * GAP,
  };

  // The block's corner: beside the plot outside, against its edge inside.
  let x: number;
  let y: number;
  if (vertical) {
    x = placement.inside
      ? side === "right"
        ? area.x + area.width - blockW - GAP
        : area.x + GAP
      : side === "right"
        ? area.x + area.width + GAP - barLead
        : GAP;
    y = placement.inside ? area.y + GAP : area.y + Math.max(0, (area.height - blockH) / 2);
  } else {
    x = placement.inside ? area.x + area.width - blockW - GAP : area.x + (area.width - blockW) / 2;
    y = placement.inside
      ? side === "bottom"
        ? area.y + area.height - blockH - GAP
        : area.y + GAP
      : side === "bottom"
        ? height - GAP - blockH
        : Math.max(GAP, area.y - GAP - blockH);
  }
  // Outside the plot, the keys clear the axis labels and names on their side
  // and the title above.
  if (!placement.inside) {
    if (side === "bottom") {
      const axis = box("xAxis");
      if (axis) y = Math.max(y, axis.y + axis.height + GAP / 2);
    } else if (side === "left") {
      const axis = box("yAxis");
      if (axis) x = Math.min(x, axis.x - GAP / 2 - blockW);
    } else if (side === "top") {
      const title = box("title");
      if (title) y = Math.max(y, title.y + title.height + GAP / 2);
    }
  }
  x = Math.max(0, x);
  y = Math.max(0, y);

  let offset = 0;
  const elements: Record<string, unknown>[] = [];
  for (const part of parts) {
    const left = vertical ? x : x + offset;
    const top = vertical ? y + offset : y;
    if (part.kind === "scale") {
      // The scale's drawing starts off its layout corner (handles, end text).
      chart.setOption({
        visualMap: {
          left: left + (columns?.scaleLeft ?? 0) - part.dx,
          top: top + scaleLead - part.dy,
        },
      });
      elements.push({
        id: COLOR_KEY_TITLE_ID,
        type: "text",
        left,
        top,
        silent: true,
        z: 100,
        invisible: !scaleTitle,
        style: {
          text: scaleTitle,
          fontSize: text.titleSize,
          fontFamily: text.fontFamily,
          fontWeight: "bold",
          fill: text.color,
        },
      });
    } else if (sizeSpec && sized) {
      elements.push(
        sizeKeyElement(sizeSpec, orient, { left, top }, text, sized.labels, columns ?? undefined),
      );
    }
    offset += (vertical ? part.h : part.w) + GAP;
  }
  if (elements.length > 0) chart.setOption({ graphic: { elements } });
  return { side, extra: reservedExtra };
}

/**
 * The keys' drawn state: the color scale's, the size key's, and the
 * legend's placement and the grid margins, with the chart's size. A host compares it after a render
 * with the state `fitKeys()` left, and fits again only when another
 * `setOption()` or a resize changed it.
 */
export function keyState(chart: Chart): string {
  const option = chart.getOption() as Record<string, unknown>;
  const first = (v: unknown) => ([v ?? []].flat()[0] ?? {}) as Record<string, unknown>;
  const pick = (o: Record<string, unknown>, keys: readonly string[]) =>
    keys.map((k) => o[k] ?? null);
  const scale = first(option.visualMap);
  const grid = first(option.grid);
  const graphics = [option.graphic ?? []]
    .flat()
    .flatMap((g) => (g as { elements?: Record<string, unknown>[] }).elements ?? []);
  const keys = graphics
    .filter((g) => g?.id === SIZE_KEY_ID || g?.id === COLOR_KEY_TITLE_ID)
    .map((g) => [g.id, ...pick(g, ["left", "top", "right", "invisible"])]);
  return JSON.stringify([
    chart.getWidth(),
    chart.getHeight(),
    pick(scale, ["left", "top", "orient", "align", "text"]),
    pick(grid, ["left", "right", "top", "bottom"]),
    pick(first(option.legend), ["left", "right", "top", "bottom"]),
    keys,
  ]);
}

/**
 * Align a categorical legend in a row above or below the plot with the plot
 * area rather than the chart's edges: a legend the host anchored right ends
 * at the plot's right edge, one anchored left starts at its left edge, and a
 * centered one centers on it. Each pass reads the anchor from the option the
 * host set. A vertical legend, or a chart without a plotting grid, is left as
 * it is. spec: draw/shared-builders#keys
 */
export function fitLegend(
  chart: Chart,
  context: { readonly option: Record<string, unknown> },
): void {
  const authored = ([context.option.legend ?? []].flat()[0] ?? null) as Record<
    string,
    unknown
  > | null;
  const model = chart.getModel().getComponent("legend");
  if (!authored || !model || model.get("show") === false) return;
  if (authored.orient === "vertical") return;
  const gridModel = chart.getModel().getComponent("grid") as unknown as
    | { coordinateSystem?: { getRect(): { x: number; width: number } } }
    | undefined;
  const area = gridModel?.coordinateSystem?.getRect();
  const view = chart.getViewOfComponentModel(model);
  if (!area || !view) return;
  const width = chart.getWidth();
  const legendWidth = view.group.getBoundingRect().width;
  const anchor =
    authored.right !== undefined && authored.right !== null && authored.left === undefined
      ? "right"
      : authored.left === "center" || authored.left === undefined
        ? "center"
        : "left";
  const position =
    anchor === "right"
      ? { right: Math.max(0, width - (area.x + area.width)), left: null }
      : anchor === "left"
        ? { left: area.x, right: null }
        : { left: Math.max(0, area.x + (area.width - legendWidth) / 2), right: null };
  chart.setOption({ legend: position });
}

/** The plotting rectangle of a grid's coordinate system. */
interface GridModel extends ComponentModel {
  coordinateSystem?: { getRect(): { x: number; y: number; width: number; height: number } };
}

/**
 * Places a chart's vertical color scale (its first `visualMap`) {@link KEY_GAP}
 * pixels right of the rightmost plotting area, such as a heatmap's row
 * dendrogram, so every chart leaves the same space between its plot and its
 * scale. The scale's title, the first entry of its `text` in the option the
 * host set, is drawn above the bar and aligned with it, in bold, as the
 * scatter chart's is; the scale and its title are centered on the plotting
 * areas' vertical extent. The grid's right margin holds the scale; a scale
 * that is hidden or horizontal stays where it is. Returns whether it moved
 * the scale. spec: draw/shared-builders#keys
 */
export function placeColorScale(
  chart: Chart,
  context: { readonly option?: Record<string, unknown>; readonly titleGap?: number } = {},
): boolean {
  const model = chart.getModel();
  const scale = model.getComponent("visualMap");
  if (!scale || scale.get("show") === false || scale.get("orient") !== "vertical") return false;
  let right = Number.NEGATIVE_INFINITY;
  let top = Number.POSITIVE_INFINITY;
  let bottom = Number.NEGATIVE_INFINITY;
  for (let i = 0; ; i++) {
    const grid = model.getComponent("grid", i) as GridModel | undefined;
    if (!grid) break;
    const rect = grid.coordinateSystem?.getRect();
    if (!rect || !(rect.width > 0)) continue;
    right = Math.max(right, rect.x + rect.width);
    top = Math.min(top, rect.y);
    bottom = Math.max(bottom, rect.y + rect.height);
  }
  if (!Number.isFinite(right)) return false;
  const authored = [context.option?.visualMap ?? []].flat()[0] as { text?: unknown } | undefined;
  const title = String(([authored?.text ?? []].flat()[0] as string | undefined) ?? "");
  const text = keyText(chart, context.titleGap ?? KEY_TITLE_GAP);
  const lead = title ? titleHeight(text) : 0;
  // Measure the scale from the origin: the bar's offset and the scale's
  // height, then place the bar KEY_GAP right of the plot.
  chart.setOption({
    visualMap: {
      left: 0,
      top: 0,
      right: null,
      bottom: null,
      ...(title ? { text: ["", ""] } : {}),
    },
  });
  const placed = model.getComponent("visualMap") ?? scale;
  const view = chart.getViewOfComponentModel(placed);
  if (!view) return false;
  const box = view.group.getBoundingRect().clone();
  box.applyTransform(view.group.getComputedTransform());
  let barX: number | null = null;
  view.group.traverse((el) => {
    if (el.isGroup) return;
    const fill = el.style?.fill as { colorStops?: unknown } | undefined;
    if (fill && typeof fill === "object" && fill.colorStops) {
      const r = el.getBoundingRect().clone();
      r.applyTransform(el.getComputedTransform());
      barX = barX === null ? r.x : Math.min(barX, r.x);
    }
  });
  const offset = barX ?? box.x;
  const left = right + KEY_GAP - offset;
  const blockTop = (top + bottom) / 2 - (box.height + lead) / 2;
  chart.setOption({
    visualMap: { left, top: blockTop + lead - box.y },
    ...(title
      ? {
          graphic: {
            elements: [
              {
                id: COLOR_KEY_TITLE_ID,
                type: "text",
                left: right + KEY_GAP,
                top: blockTop,
                silent: true,
                z: 100,
                style: {
                  text: title,
                  fontSize: text.titleSize,
                  fontFamily: text.fontFamily,
                  fontWeight: "bold",
                  fill: text.color,
                },
              },
            ],
          },
        }
      : {}),
  });
  return true;
}
