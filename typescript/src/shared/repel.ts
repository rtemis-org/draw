/**
 * Label placement that keeps the labels of a scatter chart clear of each
 * other and of the labeled points, for series a builder flags with
 * `rtemisRepelLabels`. A label layout is a function, which JSON cannot carry,
 * so the host installs it on the built option before `setOption()`
 * ({@link applyRepelLabels}).
 * spec: draw/shared-builders#significance
 */

interface Rect {
  readonly x: number;
  readonly y: number;
  readonly width: number;
  readonly height: number;
}

/** The parts of ECharts' `LabelLayoutOptionCallbackParams` the layout reads. */
export interface RepelLabelParams {
  readonly seriesIndex: number;
  readonly dataIndex: number;
  /** The labeled element's box. */
  readonly rect: Rect;
  /** The label's default box. */
  readonly labelRect: Rect;
}

/** A label position, detached from its element. */
export interface RepelLabelPosition {
  readonly x: number;
  readonly y: number;
  readonly align: "left";
  readonly verticalAlign: "top";
}

const overlaps = (a: Rect, b: Rect) =>
  a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height;

/**
 * A `labelLayout` callback shared by every flagged series of one chart, so
 * labels of different series avoid each other too.
 *
 * ECharts moves only detached labels (`label/LabelManager.ts`): under a
 * static `labelLayout` a scatter label stays attached to its symbol, and
 * `moveOverlap` has no effect. Returning a position detaches the label, so
 * placement happens here. Each label takes the first candidate around its
 * point that clears every label and labeled point placed before it in the
 * same layout pass, and keeps its default spot when none does. ECharts calls
 * the callback once per label, in order, on every layout pass; a repeated
 * label marks the start of a new synchronous pass (export fitting, resize),
 * and a microtask clears the state between asynchronous passes such as a
 * legend toggle.
 */
export function repelLabels(): (params: RepelLabelParams) => RepelLabelPosition {
  let placed: Rect[] = [];
  let seen = new Set<string>();
  let clearQueued = false;
  const clear = () => {
    placed = [];
    seen = new Set();
  };
  return (params) => {
    const key = `${params.seriesIndex}:${params.dataIndex}`;
    if (seen.has(key)) clear();
    seen.add(key);
    if (!clearQueued) {
      clearQueued = true;
      void Promise.resolve().then(() => {
        clear();
        clearQueued = false;
      });
    }
    const host = params.rect;
    const label = params.labelRect;
    const w = label.width;
    const h = label.height;
    const gap = 2;
    const step = h + gap;
    const cx = host.x + host.width / 2;
    const cy = host.y + host.height / 2;
    const above = host.y - gap - h;
    const below = host.y + host.height + gap;
    const right = host.x + host.width + gap;
    const left = host.x - gap - w;
    const candidates = [
      { x: label.x, y: label.y },
      { x: cx - w / 2, y: below },
      { x: right, y: cy - h / 2 },
      { x: left, y: cy - h / 2 },
    ];
    for (let k = 1; k <= 6; k++) {
      candidates.push(
        { x: cx - w / 2, y: above - k * step },
        { x: cx - w / 2, y: below + k * step },
        { x: right, y: cy - h / 2 - k * step },
        { x: left, y: cy - h / 2 - k * step },
        { x: right, y: cy - h / 2 + k * step },
        { x: left, y: cy - h / 2 + k * step },
      );
    }
    // Later labels must not cover this label's point either.
    placed.push({ x: host.x, y: host.y, width: host.width, height: host.height });
    const fits = (c: { x: number; y: number }) =>
      c.x >= 0 &&
      c.y >= 0 &&
      placed.every((p) => !overlaps({ x: c.x, y: c.y, width: w, height: h }, p));
    const spot = candidates.find(fits) ?? (candidates[0] as { x: number; y: number });
    placed.push({ x: spot.x, y: spot.y, width: w, height: h });
    return { x: spot.x, y: spot.y, align: "left", verticalAlign: "top" };
  };
}

/**
 * Installs one {@link repelLabels} layout on every series of `option` flagged
 * `rtemisRepelLabels`. Mutates and returns `option`.
 */
export function applyRepelLabels<T extends Record<string, unknown>>(option: T): T {
  const series = option.series;
  if (!Array.isArray(series)) return option;
  let repel: ReturnType<typeof repelLabels> | null = null;
  for (const s of series as Record<string, unknown>[]) {
    if (!s?.rtemisRepelLabels) continue;
    repel = repel ?? repelLabels();
    s.labelLayout = repel;
  }
  return option;
}

/**
 * Installs the label layouts a built option asks for by flag, as functions
 * its JSON cannot carry: one {@link repelLabels} layout shared by every
 * series flagged `rtemisRepelLabels`, and on a series flagged
 * `rtemisCellNotes` (a heatmap's notes) labels truncated with an ellipsis to
 * each cell's drawn size, overlapping ones hidden; the notes stay whole in
 * the data. Mutates and returns `option`. Call before `setOption`.
 */
export function attachLabelLayouts<T extends Record<string, unknown>>(option: T): T {
  applyRepelLabels(option);
  const series = option.series;
  if (!Array.isArray(series)) return option;
  for (const s of series as Record<string, unknown>[]) {
    if (!s?.rtemisCellNotes) continue;
    s.label = { ...(s.label as Record<string, unknown>), overflow: "truncate", ellipsis: "..." };
    s.labelLayout = (params: { rect: { width: number; height: number } }) => ({
      width: Math.max(0, params.rect.width - 5),
      height: Math.max(0, params.rect.height - 3),
      hideOverlap: true,
    });
  }
  return option;
}
