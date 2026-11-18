import { colorRamp, dark3, divergingRamp } from "../../shared/color.ts";
import { ddSci } from "../../shared/format.ts";
import type { MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import { darkThemeTokens, lightThemeTokens, SIGN_COLORS } from "../../theme/tokens.ts";
import { DENDROGRAM, HEATMAP_TRACKS, TRACK_STEP } from "./renderers.ts";

/**
 * An instance of the `chart/heatmap/v1` schema, written by rtemis.draw's
 * `HeatmapConfig` class. Every member is optional. The host applies
 * `triangle`, `cluster_rows`, `cluster_cols`, `dist_method`, and
 * `hclust_method` to the data it passes ({@link HeatmapData}); the builder
 * draws the rest. spec: draw/shared-builders#heatmap
 */
export interface HeatmapChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "heatmap";
  /** Row labels, for data without them. */
  readonly row_names?: ColumnNames | null;
  /** Column labels, for data without them. */
  readonly col_names?: ColumnNames | null;
  /** Triangle the host keeps: `"upper"` or `"lower"`, the diagonal masked. */
  readonly triangle?: "upper" | "lower" | null;
  /** Whether the host clusters the rows. Default `false`. */
  readonly cluster_rows?: boolean;
  /** Whether the host clusters the columns. Default `false`. */
  readonly cluster_cols?: boolean;
  /** The host's distance method. Default `"euclidean"`. */
  readonly dist_method?: string;
  /** The host's linkage method. Default `"complete"`. */
  readonly hclust_method?: string;
  /** Square cells; unset is square when rows and columns number the same. */
  readonly square_cells?: boolean | null;
  /** Draw the row tree's dendrogram. Default `true`. */
  readonly show_row_dendro?: boolean;
  /** Draw the column tree's dendrogram. Default `true`. */
  readonly show_col_dendro?: boolean;
  /** Row dendrogram width in pixels. Default 60. */
  readonly dendro_row_width?: number;
  /** Column dendrogram height in pixels. Default 60. */
  readonly dendro_col_height?: number;
  /** Branch color; unset is a translucent gray. */
  readonly dendro_color?: string | null;
  /** Equal height steps per merge in place of merge heights. Default `false`. */
  readonly dendro_uniform?: boolean;
  /** Side of the row dendrogram. Default `"right"`. */
  readonly dendro_row_side?: "right" | "left";
  /** Side of the column dendrogram. Default `"top"`. */
  readonly dendro_col_side?: "top" | "bottom";
  /** Color scale of at least two colors; unset derives one from the theme's sign colors. */
  readonly colormap?: ColumnNames | null;
  /** Color scale limits; unset is the range of the values. */
  readonly zlim?: readonly number[] | null;
  /** Write each cell's note in place of its value. Default `false`. */
  readonly show_notes?: boolean;
  /** Number of row clusters whose branches are colored. */
  readonly row_cut?: number | null;
  /** Number of column clusters whose branches are colored. */
  readonly col_cut?: number | null;
  /** Write each cell's value. Default `false`. */
  readonly show_values?: boolean;
  /** Decimals of the values written (`ddSci()`). Default 2. */
  readonly value_digits?: number;
  /** Draw the color scale. Default `true`. */
  readonly show_colorbar?: boolean;
  /** Orientation of the color scale. Default `"vertical"`. */
  readonly colorbar_orient?: "vertical" | "horizontal";
  readonly title?: string | null;
}

/** Every member of {@link HeatmapChartConfig}, checked against `chart/heatmap/v1`. */
export const HEATMAP_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "row_names",
  "col_names",
  "triangle",
  "cluster_rows",
  "cluster_cols",
  "dist_method",
  "hclust_method",
  "square_cells",
  "show_row_dendro",
  "show_col_dendro",
  "dendro_row_width",
  "dendro_col_height",
  "dendro_color",
  "dendro_uniform",
  "dendro_row_side",
  "dendro_col_side",
  "colormap",
  "zlim",
  "show_notes",
  "row_cut",
  "col_cut",
  "show_values",
  "value_digits",
  "show_colorbar",
  "colorbar_orient",
  "margin_top",
  "margin_right",
  "margin_bottom",
  "margin_left",
] as const satisfies readonly (keyof HeatmapChartConfig)[]);

type Missing = Exclude<keyof HeatmapChartConfig, (typeof HEATMAP_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * A hierarchical tree as R's `hclust` writes it: `merge[k]` joins two
 * children, a negative entry -i the leaf i (1-based) and a positive entry j
 * the j-th merge; `height[k]` is the merge's height; `order` lists the
 * leaves (1-based) in display order. `leaves` maps leaf i to its data row or
 * column (0-based, ascending); unset, leaf i is row i - 1.
 */
export interface HeatmapTree {
  readonly merge: readonly (readonly [number, number])[];
  readonly height: readonly number[];
  readonly order: readonly number[];
  readonly leaves?: readonly number[] | null;
}

/**
 * A heatmap's data: the matrix by row, `null` for a missing cell, with its
 * labels, the trees the host computed, cell notes, and color tracks (per row
 * or column, one color per track), all in the matrix's order.
 */
export interface HeatmapData {
  readonly values: readonly (readonly (number | null)[])[];
  readonly row_names?: readonly string[] | null;
  readonly col_names?: readonly string[] | null;
  readonly row_tree?: HeatmapTree | null;
  readonly col_tree?: HeatmapTree | null;
  readonly cell_notes?: readonly (readonly string[])[] | null;
  readonly row_colors?: readonly (readonly string[])[] | null;
  readonly col_colors?: readonly (readonly string[])[] | null;
}

/** Theme values the builder needs. */
export interface HeatmapAppearance {
  /** The chart is drawn on a dark background; the derived scale ends there. */
  readonly dark?: boolean;
  /** Background of the derived scale's zero; unset is the theme's. */
  readonly background?: string;
  /** Sign colors of the derived scale; unset is {@link SIGN_COLORS}. */
  readonly signColors?: { readonly negative: string; readonly positive: string };
}

/** The pixel layout of a heatmap, for a host that fits square cells. */
export interface HeatmapHints {
  /** Space a vertical color scale needs right of the cells, when no right margin is set. */
  readonly legendReserveRight: number;
  readonly squareCells: boolean;
  readonly nRows: number;
  readonly nCols: number;
  /** Margins of the cells' grid in pixels. */
  readonly leftPx: number;
  readonly rightPx: number;
  readonly topPx: number;
  readonly botPx: number;
}

/** One permuted axis of the matrix: display order, labels, tree, and positions. */
interface AxisOrder {
  /** Data index of each display position. */
  readonly order: number[];
  readonly tree: HeatmapTree | null;
  /** Display position of each of the tree's leaves, in `tree.order` order. */
  readonly positions: number[];
}

function axisOrder(n: number, tree: HeatmapTree | null | undefined, axis: string): AxisOrder {
  const order = Array.from({ length: n }, (_, i) => i);
  if (!tree) return { order, tree: null, positions: [] };
  const leaves = tree.leaves ?? Array.from({ length: n }, (_, i) => i);
  const m = tree.order.length;
  if (tree.merge.length !== m - 1 || tree.height.length !== m - 1 || leaves.length !== m) {
    throw new Error(
      `The ${axis} tree needs one merge and height per joined pair, and one leaf per ${axis}.`,
    );
  }
  // The leaves keep their positions in the matrix and swap among them.
  const positions = [...leaves].sort((a, b) => a - b);
  tree.order.forEach((leaf, k) => {
    order[positions[k] as number] = leaves[leaf - 1] as number;
  });
  return { order, tree, positions };
}

/** One U shape per merge: [left position, right position, left height, right height, merge height]. */
function dendrogramData(a: AxisOrder, uniform: boolean) {
  const tree = a.tree as HeatmapTree;
  const n = tree.order.length;
  const leafPos = new Array<number>(n).fill(0);
  tree.order.forEach((leaf, p) => {
    leafPos[leaf - 1] = a.positions[p] as number;
  });
  const heights = uniform ? tree.merge.map((_, k) => k + 1) : [...tree.height];
  const nodePos: number[] = [];
  const pos = (j: number) => (j < 0 ? (leafPos[-j - 1] as number) : (nodePos[j - 1] as number));
  const hgt = (j: number) => (j < 0 ? 0 : (heights[j - 1] as number));
  const data = tree.merge.map(([l, r], k) => {
    nodePos[k] = (pos(l) + pos(r)) / 2;
    return [pos(l), pos(r), hgt(l), hgt(r), heights[k] as number];
  });
  return {
    data,
    min: Math.min(...heights),
    max: Math.max(...heights, Number.EPSILON),
  };
}

/** The axis minimum that gives the leaf stubs `stub` of a dendrogram panel. */
function dendroAxisMin(min: number, max: number, stub = 0.1): number {
  return min >= max ? min * (1 - stub) : (min - stub * max) / (1 - stub);
}

/**
 * Each merge's branch color for `k` clusters, as rtemis.draw's
 * `heatmap_branch_colors()` assigns them: the clusters of R's `cutree()`,
 * numbered by first appearance in display order and colored from R's "Dark 3"
 * palette; a merge inside one cluster takes its color, any other merge
 * `color`. `null` without `k`.
 */
export function branchColors(
  tree: HeatmapTree,
  k: number | null | undefined,
  color: string,
): string[] | null {
  if (k === null || k === undefined) return null;
  const n = tree.order.length;
  if (!Number.isInteger(k) || k < 1 || k > n) {
    throw new Error(`A tree cut needs a whole number of clusters between 1 and ${n}; it is ${k}.`);
  }
  // Clusters after the first n - k merges.
  const cluster = Array.from({ length: n }, (_, i) => i);
  const find = (i: number): number => {
    if (cluster[i] === i) return i;
    const root = find(cluster[i] as number);
    cluster[i] = root;
    return root;
  };
  const rep: number[] = [];
  tree.merge.forEach(([l, r], step) => {
    const a = l < 0 ? -l - 1 : (rep[l - 1] as number);
    const b = r < 0 ? -r - 1 : (rep[r - 1] as number);
    rep[step] = a;
    if (step < n - k) cluster[find(b)] = find(a);
  });
  const label = new Map<number, number>();
  for (const leaf of tree.order) {
    const root = find(leaf - 1);
    if (!label.has(root)) label.set(root, label.size + 1);
  }
  const palette = dark3(k);
  const group: number[] = [];
  return tree.merge.map(([l, r], step) => {
    const g = (j: number) =>
      j < 0 ? (label.get(find(-j - 1)) as number) : (group[j - 1] as number);
    const [gl, gr] = [g(l), g(r)];
    group[step] = gl !== 0 && gl === gr ? gl : 0;
    return group[step] ? (palette[(group[step] as number) - 1] as string) : color;
  });
}

const BRANCH_GRAY = "#99999988";

/** The layout every heatmap derives from its labels and members. */
function layout(data: HeatmapData, config: HeatmapChartConfig) {
  const values = data.values;
  const nRows = values.length;
  const nCols = values[0]?.length ?? 0;
  if (nRows === 0 || nCols === 0)
    throw new Error("A heatmap needs at least one row and one column.");
  for (const row of values) {
    if (row.length !== nCols) throw new Error("Every heatmap row needs one value per column.");
  }
  const labels = (
    given: readonly string[] | null | undefined,
    member: ColumnNames | null | undefined,
    n: number,
    axis: string,
  ) => {
    const names =
      given ??
      (asArray(member).length > 0
        ? asArray(member)
        : Array.from({ length: n }, (_, i) => String(i + 1)));
    if (names.length !== n) {
      throw new Error(
        `Supply one ${axis} label per matrix ${axis}; there are ${n}, and ${names.length} labels.`,
      );
    }
    return [...names];
  };
  const rowNames = labels(data.row_names, config.row_names, nRows, "row");
  const colNames = labels(data.col_names, config.col_names, nCols, "column");
  const longest = (names: readonly string[]) => Math.max(0, ...names.map((s) => s.length));
  const rotate = longest(colNames) > 4 ? 45 : 0;
  const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
  const showBar = config.show_colorbar ?? true;
  const set = (side: "top" | "right" | "bottom" | "left", auto: number) => {
    const v = config[`margin_${side}`];
    return typeof v === "number" ? v : auto;
  };
  const rowTracks = data.row_colors?.[0]?.length ?? 0;
  const colTracks = data.col_colors?.[0]?.length ?? 0;
  const rowTrackPx = rowTracks > 0 ? TRACK_STEP * rowTracks + 4 : 0;
  const colTrackPx = colTracks > 0 ? TRACK_STEP * colTracks + 4 : 0;
  const left = set("left", Math.min(20 + longest(rowNames) * 8, 200)) + rowTrackPx;
  const right = set("right", showBar && vertical ? 90 : 20);
  const top = set("top", config.title ? 40 : 10);
  const bottom =
    set("bottom", rotate > 0 ? Math.min(20 + longest(colNames) * 5, 140) : 36) + colTrackPx;
  const rows = axisOrder(nRows, data.row_tree, "row");
  const cols = axisOrder(nCols, data.col_tree, "column");
  const rowDendro = rows.tree !== null && (config.show_row_dendro ?? true);
  const colDendro = cols.tree !== null && (config.show_col_dendro ?? true);
  const rowSide = config.dendro_row_side ?? "right";
  const colSide = config.dendro_col_side ?? "top";
  const rowWidth = config.dendro_row_width ?? 60;
  const colHeight = config.dendro_col_height ?? 60;
  const labelsTop = colDendro && colSide === "bottom";
  const hm = {
    left: left + (rowDendro && rowSide === "left" ? rowWidth : 0),
    right: right + (rowDendro && rowSide === "right" ? rowWidth : 0),
    top: top + (colDendro && colSide === "top" ? colHeight : 0) + (labelsTop ? bottom : 0),
    bottom: labelsTop ? colHeight : bottom + (colDendro && colSide === "bottom" ? colHeight : 0),
  };
  return {
    nRows,
    nCols,
    rowNames,
    colNames,
    rotate,
    margins: { left, right, top, bottom },
    rowTrackPx,
    colTrackPx,
    rows,
    cols,
    rowDendro,
    colDendro,
    rowSide,
    colSide,
    rowWidth,
    colHeight,
    labelsTop,
    hm,
    square: config.square_cells ?? nRows === nCols,
  };
}

/**
 * The pixel layout of a heatmap's cells, which a host fits square cells and
 * places its color scale with (rtemis.draw's widget: `fitHeatmap()`).
 */
export function heatmapHints(data: HeatmapData, config: HeatmapChartConfig): HeatmapHints {
  const l = layout(data, config);
  const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
  return {
    legendReserveRight:
      (config.show_colorbar ?? true) && vertical && typeof config.margin_right !== "number"
        ? 70
        : 0,
    squareCells: l.square,
    nRows: l.nRows,
    nCols: l.nCols,
    leftPx: l.hm.left,
    rightPx: l.hm.right,
    topPx: l.hm.top,
    botPx: l.hm.bottom,
  };
}

/**
 * The margins around a heatmap's cells and dendrograms in pixels: each
 * `margin_*` member, or the side derived from the labels, color scale, title,
 * and tracks. A host that keeps its own minimum margins passes the larger of
 * the two as the members.
 */
export function heatmapMargins(
  data: HeatmapData,
  config: HeatmapChartConfig,
): { left: number; right: number; top: number; bottom: number } {
  const l = layout(data, config);
  return {
    left: l.margins.left - l.rowTrackPx,
    right: l.margins.right,
    top: l.margins.top,
    bottom: l.margins.bottom - l.colTrackPx,
  };
}

/** The parts of an ECharts instance {@link fitHeatmap} uses. */
export interface HeatmapChart {
  getWidth(): number;
  getHeight(): number;
  setOption(option: Record<string, unknown>): void;
}

/**
 * Square cells at the chart's current size: the cells' grid takes the largest
 * square cell that fits within the margins of `hints`, centered in the
 * remaining space, and every grid, the color scale, and the title shift with
 * it, so dendrograms stay aligned with the cells. Reads the margins from the
 * built `option`, so repeated fits do not accumulate. Hints without square
 * cells leave the chart as it is. spec: draw/shared-builders#heatmap
 */
export function fitHeatmap(
  chart: HeatmapChart,
  option: Record<string, unknown>,
  hints: HeatmapHints,
): void {
  if (!hints.squareCells) return;
  const { nRows, nCols, leftPx, rightPx, topPx, botPx } = hints;
  const width = chart.getWidth() - leftPx - rightPx;
  const height = chart.getHeight() - topPx - botPx;
  const cell = Math.min(width / nCols, height / nRows);
  if (!(cell > 0)) return;
  const dx = (width - nCols * cell) / 2;
  const dy = (height - nRows * cell) / 2;
  const list = (v: unknown) => [v ?? []].flat() as Record<string, unknown>[];
  const update: Record<string, unknown> = {
    grid: list(option.grid).map((grid) => {
      const fitted: Record<string, unknown> = {
        ...grid,
        containLabel: false,
        outerBoundsMode: "none",
      };
      for (const side of ["left", "right", "top", "bottom"] as const) {
        const v = grid[side];
        if (typeof v === "number")
          fitted[side] = v + (side === "left" || side === "right" ? dx : dy);
      }
      return fitted;
    }),
  };
  const maps = list(option.visualMap);
  if (maps.length > 0) {
    update.visualMap = maps.map((map) => {
      const side = map.orient === "horizontal" ? "bottom" : "right";
      const anchor = map[side] === side ? 0 : map[side];
      return typeof anchor === "number" ? { [side]: anchor + (side === "right" ? dx : dy) } : {};
    });
  }
  const titles = list(option.title);
  if (titles.length > 0) {
    update.title = titles.map((title) =>
      typeof title.left === "number" ? { left: title.left + dx } : {},
    );
  }
  chart.setOption(update);
}

const escapeHtml = (text: string) => text.replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);

/**
 * The ECharts option of a heatmap from a host-prepared matrix: the cells in
 * the trees' display order, each colored on a continuous scale and written
 * with its value (`show_values`) or note (`show_notes`); a dendrogram panel
 * beside the cells for each tree, its branches colored by cluster with a cut;
 * a color track per annotation column beside the rows or columns; and the
 * color scale. Without `colormap`, the scale runs from the negative sign color
 * through the background at zero to the positive sign color, or from the
 * background to the one sign the values take. Hovering a cell shows its row,
 * column, value, and note.
 * spec: draw/shared-builders#heatmap
 */
export function buildHeatmapOption(
  data: HeatmapData,
  config: HeatmapChartConfig,
  appearance: HeatmapAppearance = {},
): Record<string, unknown> {
  const l = layout(data, config);
  const digits = config.value_digits ?? 2;
  const showValues = config.show_values ?? false;
  const showNotes = config.show_notes ?? false;
  const notes = data.cell_notes ?? null;
  if (showNotes && (!notes || showValues)) {
    throw new Error(
      "`show_notes` writes the cell notes the data supplies, in place of the values: supply cell notes, and unset `show_values`.",
    );
  }
  const rowOrder = l.rows.order;
  const colOrder = l.cols.order;
  const cell = (i: number, j: number) =>
    data.values[rowOrder[i] as number]?.[colOrder[j] as number] ?? null;
  // A loop, since spreading a large matrix's values into Math.min() exceeds
  // the call stack.
  let lo = Number.POSITIVE_INFINITY;
  let hi = Number.NEGATIVE_INFINITY;
  for (let i = 0; i < l.nRows; i++) {
    for (let j = 0; j < l.nCols; j++) {
      const v = cell(i, j);
      if (typeof v === "number" && Number.isFinite(v)) {
        if (v < lo) lo = v;
        if (v > hi) hi = v;
      }
    }
  }
  if (lo > hi) throw new Error("A heatmap needs at least one finite value.");
  const zlim: [number, number] =
    config.zlim && config.zlim.length === 2
      ? [config.zlim[0] as number, config.zlim[1] as number]
      : [lo, hi];
  const memberColors = asArray(config.colormap);
  let colormap: string[];
  if (memberColors.length > 0) {
    colormap = [...memberColors];
  } else {
    const sign = appearance.signColors ?? SIGN_COLORS;
    const bg =
      appearance.background ?? (appearance.dark ? darkThemeTokens() : lightThemeTokens()).bg_color;
    colormap =
      zlim[0] < 0 && zlim[1] > 0
        ? divergingRamp(sign.negative, bg, sign.positive, zlim)
        : zlim[1] <= 0
          ? colorRamp([sign.negative, bg], 101)
          : colorRamp([bg, sign.positive], 101);
  }

  const labelled = showValues || notes !== null;
  const cells: unknown[] = [];
  for (let i = 0; i < l.nRows; i++) {
    for (let j = 0; j < l.nCols; j++) {
      const v = cell(i, j);
      const value = [j, i, v];
      if (!labelled) {
        cells.push(value);
        continue;
      }
      const note = notes ? (notes[rowOrder[i] as number]?.[colOrder[j] as number] ?? "") : "";
      cells.push({
        value,
        name: note,
        note,
        label: { formatter: showNotes ? "{b}" : v === null ? "" : ddSci(v, digits) },
      });
    }
  }

  const branch = config.dendro_color ?? BRANCH_GRAY;
  const colAxis = {
    type: "category",
    data: colOrder.map((j) => l.colNames[j] as string),
    splitArea: { show: false },
    axisLine: { show: false },
    axisLabel: { rotate: l.rotate, margin: 8 + l.colTrackPx },
    boundaryGap: true,
    ...(l.labelsTop ? { position: "top" } : {}),
  };
  const rowAxis = {
    type: "category",
    data: rowOrder.map((i) => l.rowNames[i] as string),
    inverse: true,
    splitArea: { show: false },
    axisLine: { show: false },
    axisLabel: { margin: 8 + l.rowTrackPx },
    boundaryGap: true,
  };
  const grids: Record<string, unknown>[] = [];
  const xAxes: Record<string, unknown>[] = [];
  const yAxes: Record<string, unknown>[] = [];
  const series: Record<string, unknown>[] = [];
  const m = l.margins;
  if (l.rowDendro) {
    const d = dendrogramData(l.rows, config.dendro_uniform ?? false);
    const index = grids.length;
    grids.push(
      l.rowSide === "right"
        ? { right: m.right, width: l.rowWidth, top: l.hm.top, bottom: l.hm.bottom }
        : { left: 0, width: l.rowWidth, top: l.hm.top, bottom: l.hm.bottom },
    );
    xAxes.push({
      type: "value",
      inverse: l.rowSide === "left",
      show: false,
      min: dendroAxisMin(d.min, d.max),
      max: d.max,
      gridIndex: index,
    });
    yAxes.push({
      type: "value",
      min: -0.5,
      max: l.nRows - 0.5,
      inverse: true,
      show: false,
      gridIndex: index,
    });
    const colors = branchColors(l.rows.tree as HeatmapTree, config.row_cut, branch);
    series.push(dendrogramSeries(index, d.data, "row", branch, colors));
  } else if (config.row_cut !== null && config.row_cut !== undefined) {
    throw new Error(
      "`row_cut` colors the branches of the row tree: cluster the rows, or supply a row tree.",
    );
  }
  if (l.colDendro) {
    const d = dendrogramData(l.cols, config.dendro_uniform ?? false);
    const index = grids.length;
    grids.push(
      l.colSide === "top"
        ? { left: l.hm.left, right: l.hm.right, top: m.top, height: l.colHeight }
        : { left: l.hm.left, right: l.hm.right, bottom: 0, height: l.colHeight },
    );
    xAxes.push({ type: "value", min: -0.5, max: l.nCols - 0.5, show: false, gridIndex: index });
    yAxes.push({
      type: "value",
      show: false,
      inverse: l.colSide === "bottom",
      min: dendroAxisMin(d.min, d.max),
      max: d.max,
      gridIndex: index,
    });
    const colors = branchColors(l.cols.tree as HeatmapTree, config.col_cut, branch);
    series.push(dendrogramSeries(index, d.data, "column", branch, colors));
  } else if (config.col_cut !== null && config.col_cut !== undefined) {
    throw new Error(
      "`col_cut` colors the branches of the column tree: cluster the columns, or supply a column tree.",
    );
  }
  const hmIndex = grids.length;
  const dendro = l.rowDendro || l.colDendro;
  grids.push(
    dendro
      ? {
          left: l.hm.left,
          right: l.hm.right,
          top: l.hm.top,
          bottom: l.hm.bottom,
          containLabel: false,
        }
      : { left: m.left, right: m.right, top: m.top, bottom: m.bottom, containLabel: false },
  );
  xAxes.push(dendro ? { ...colAxis, gridIndex: hmIndex } : colAxis);
  yAxes.push(dendro ? { ...rowAxis, gridIndex: hmIndex } : rowAxis);
  const heatmapIndex = series.length;
  series.push({
    type: "heatmap",
    // Without animation and hover emphasis, a large matrix neither tweens
    // every cell on redraw nor recomputes a highlight on each pointer move.
    animation: false,
    emphasis: { disabled: true },
    data: cells,
    ...(showValues || showNotes ? { label: { show: true } } : {}),
    ...(dendro ? { xAxisIndex: hmIndex, yAxisIndex: hmIndex } : {}),
    ...(showNotes ? { rtemisCellNotes: true } : {}),
  });
  for (const [orientation, tracks, order] of [
    ["row", data.row_colors, rowOrder],
    ["column", data.col_colors, colOrder],
  ] as const) {
    if (!tracks) continue;
    series.push({
      type: "custom",
      renderItem: HEATMAP_TRACKS,
      ...(dendro ? { xAxisIndex: hmIndex, yAxisIndex: hmIndex } : {}),
      data: order.map((_, k) => [k]),
      silent: true,
      clip: false,
      animation: false,
      itemPayload: {
        orientation,
        top: l.labelsTop,
        colors: order.map((index) => [...(tracks[index] ?? [])]),
      },
    });
  }

  const rowLabels = rowAxis.data;
  const colLabels = colAxis.data;
  const vertical = (config.colorbar_orient ?? "vertical") === "vertical";
  const single = grids.length === 1;
  return {
    ...(config.title ? { title: { text: config.title, left: dendro ? l.hm.left : m.left } } : {}),
    tooltip: {
      trigger: "item",
      formatter: (p: {
        value?: unknown[];
        data?: { note?: string; label?: { formatter?: string } };
      }) => {
        if (!Array.isArray(p.value)) return "";
        const v = p.value[2];
        let text = typeof v === "number" ? ddSci(v, digits) : "NA";
        if (showValues && p.data?.label?.formatter) text = p.data.label.formatter;
        const note = p.data?.note ? `<br>${escapeHtml(p.data.note)}` : "";
        return `${escapeHtml(String(rowLabels[p.value[1] as number]))} × ${escapeHtml(String(colLabels[p.value[0] as number]))}: ${escapeHtml(text)}${note}`;
      },
    },
    grid: single ? grids[0] : grids,
    xAxis: single ? xAxes[0] : xAxes,
    yAxis: single ? yAxes[0] : yAxes,
    visualMap: {
      type: "continuous",
      min: zlim[0],
      max: zlim[1],
      precision: digits,
      calculable: true,
      show: config.show_colorbar ?? true,
      orient: vertical ? "vertical" : "horizontal",
      inRange: { color: colormap },
      // A vertical scale's labels sit right of its bar (`align` names the bar's
      // side), away from the cells.
      ...(vertical
        ? { right: "right", top: "middle", align: "left" }
        : { left: "center", bottom: "bottom" }),
      seriesIndex: heatmapIndex,
    },
    series,
  };
}

function dendrogramSeries(
  index: number,
  data: number[][],
  orientation: "row" | "column",
  color: string,
  colors: string[] | null,
): Record<string, unknown> {
  return {
    type: "custom",
    xAxisIndex: index,
    yAxisIndex: index,
    data,
    silent: true,
    clip: true,
    animation: false,
    renderItem: DENDROGRAM,
    itemPayload: { orientation, color, ...(colors ? { colors } : {}) },
  };
}
