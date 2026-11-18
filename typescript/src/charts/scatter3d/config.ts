import type { FitMembers } from "../../shared/fit.ts";
import { formatNumber, tickLabel } from "../../shared/format.ts";
import { type NiceAxis, niceAxis } from "../../shared/limits.ts";
import {
  asArray,
  type ColumnNames,
  column,
  distinct,
  type TabularData,
} from "../../shared/source.ts";
import { autoAlpha } from "../scatter/config.ts";

/**
 * An instance of the `chart/scatter3d/v1` schema, written by rtemis.draw's
 * `Scatter3DConfig` class. Every member is optional; `x`, `y`, and `z` name
 * numeric columns and are required to draw. `fit`, `fit_params`, and `n_fit`
 * name the surface the host fits; the builder draws the surfaces the data
 * carries.
 * spec: draw/shared-builders#scatter3d
 */
export interface Scatter3DChartConfig extends Pick<FitMembers, "fit_params"> {
  readonly type?: "scatter3d";
  readonly x?: string | null;
  readonly y?: string | null;
  readonly z?: string | null;
  /** Column whose values split the observations into one series per group. */
  readonly group?: string | null;
  /** Camera elevation in degrees. Default 20. */
  readonly alpha?: number;
  /** Camera azimuth in degrees. Default 40. */
  readonly beta?: number;
  /** Camera projection. Default `"perspective"`. */
  readonly projection?: "perspective" | "orthographic";
  /** Perspective camera distance from the center of the 100-unit cube. Default 250. */
  readonly distance?: number;
  /** Orthographic viewing extent for the 100-unit cube. Default 190. */
  readonly view_size?: number;
  /** Marker diameter in pixels. Default 8. */
  readonly point_size?: number;
  /**
   * Point and line opacity; unset is {@link autoAlpha} of the number of
   * complete observations, or 1 for paths alone.
   */
  readonly opacity?: number | null;
  /** Geometry: points, paths, or both. Default `"points"`. */
  readonly mode?: "points" | "lines" | "both";
  /** Order within complete path runs: as given, or by `x`. Default `"input"`. */
  readonly order?: "input" | "x";
  /** Path width in pixels. Default 2. */
  readonly line_width?: number;
  readonly xlab?: string | null;
  readonly ylab?: string | null;
  readonly zlab?: string | null;
  /** Group colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Surface of z on x and y the host fits per group; unset fits none. */
  readonly fit?: string | null;
  /** Grid points per axis of a fitted surface. Default 25. */
  readonly n_fit?: number;
  /** Opacity of a surface without its own. Default 0.35. */
  readonly fit_alpha?: number;
  readonly title?: string | null;
}

/** Every member of {@link Scatter3DChartConfig}, checked against `chart/scatter3d/v1`. */
export const SCATTER3D_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "x",
  "y",
  "z",
  "group",
  "alpha",
  "beta",
  "projection",
  "distance",
  "view_size",
  "point_size",
  "opacity",
  "mode",
  "order",
  "line_width",
  "xlab",
  "ylab",
  "zlab",
  "palette",
  "fit",
  "fit_params",
  "n_fit",
  "fit_alpha",
] as const satisfies readonly (keyof Scatter3DChartConfig)[]);

type Missing = Exclude<keyof Scatter3DChartConfig, (typeof SCATTER3D_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * A surface z = f(x, y) on a rectangular grid: `z[i][j]` is the value at
 * `x[i]` and `y[j]`, `null` where it is missing, which removes the grid cells
 * around it. A surface named after a group takes the group's color and legend
 * entry.
 */
export interface Surface3D {
  readonly name: string;
  readonly x: readonly number[];
  readonly y: readonly number[];
  readonly z: readonly (readonly (number | null)[])[];
  /** Fill color; unset takes the palette color of `name`. */
  readonly color?: string | null;
  /** Opacity; unset is the config's `fit_alpha`. */
  readonly opacity?: number | null;
}

/** A 3D scatter chart's data: the bound columns and the surfaces the host computed. */
export interface Scatter3DData extends TabularData {
  readonly surfaces?: readonly Surface3D[];
}

/** Theme values the builder needs. */
export interface Scatter3DAppearance {
  /** The host's categorical palette; unset leaves colors to the theme. */
  readonly palette?: readonly string[];
}

/** Name of the one group of an ungrouped chart. */
export const OBSERVATIONS = "Observations";

const AXIS_KEYS = ["xAxis3D", "yAxis3D", "zAxis3D"] as const;

const isMissing = (v: unknown) =>
  v === null || v === undefined || (typeof v === "number" && Number.isNaN(v));

/** A numeric coordinate column, `null` for a missing cell; text or infinity stops with an error. */
function coordinates(data: TabularData, name: string, member: string): (number | null)[] {
  return column(data.columns, name, member).map((v, i) => {
    if (isMissing(v)) return null;
    if (typeof v !== "number" || !Number.isFinite(v)) {
      throw new Error(
        `Column '${name}' (config member '${member}') must hold finite numbers or missing values; row ${i + 1} holds '${String(v)}'.`,
      );
    }
    return v;
  });
}

/**
 * The ECharts-GL series of a surface: the grid sorted by x and by y, x
 * varying fastest, flat color shading, and no wireframe.
 */
export function surfaceSeries(surface: Surface3D, opacity = 0.35): Record<string, unknown> {
  const ix = surface.x
    .map((_, i) => i)
    .sort((a, b) => (surface.x[a] as number) - (surface.x[b] as number));
  const iy = surface.y
    .map((_, j) => j)
    .sort((a, b) => (surface.y[a] as number) - (surface.y[b] as number));
  const data = iy.flatMap((j) =>
    ix.map((i) => {
      const z = surface.z[i]?.[j];
      return [surface.x[i], surface.y[j], typeof z === "number" && Number.isFinite(z) ? z : null];
    }),
  );
  return {
    type: "surface",
    name: surface.name,
    shading: "color",
    wireframe: { show: false },
    dataShape: [iy.length, ix.length],
    itemStyle: {
      ...(surface.color ? { color: surface.color } : {}),
      opacity: surface.opacity ?? opacity,
    },
    data,
  };
}

/**
 * An axis range as `[min, max]`, widened around a single value by 5% of its
 * magnitude, at least 1; a range that is not finite stops with an error.
 */
function axisRange(values: readonly number[]): [number, number] {
  let lo = Number.POSITIVE_INFINITY;
  let hi = Number.NEGATIVE_INFINITY;
  for (const v of values) {
    if (v < lo) lo = v;
    if (v > hi) hi = v;
  }
  if (lo === hi) {
    const r = Math.max(1, Math.abs(lo) * 0.05);
    lo -= r;
    hi += r;
  }
  if (!Number.isFinite(lo) || !Number.isFinite(hi) || !Number.isFinite(hi - lo) || hi - lo <= 0) {
    throw new Error("Rescale 3D coordinates to a finite nonzero range.");
  }
  return [lo, hi];
}

/**
 * The tick label formatter of an axis: {@link tickLabel} at the interval's
 * decimals. The z axis leaves its minimum unlabeled, since that tick sits at
 * the corner where the x axis's labels begin.
 */
function axisTicks(nice: NiceAxis, skipMin: boolean): (value: unknown) => string {
  const label = tickLabel(nice.decimals);
  return (value) =>
    skipMin && Math.abs(Number(value) - nice.min) <= nice.interval * 1e-6 ? "" : label(value);
}

/** An axis's limits and interval: {@link niceAxis} of about four intervals. */
function valueAxis(min: number, max: number): { min: number; max: number; interval: number } {
  const nice = niceAxis(min, max);
  return { min: nice.min, max: nice.max, interval: nice.interval };
}

/**
 * The ECharts-GL option of a 3D scatter chart: one `scatter3D` series of
 * points per group, one `line3D` path per run of consecutive complete
 * observations of a group, and one `surface` per surface of the data; axes
 * spanning the complete observations and the surfaces at nice limits
 * ({@link niceAxis}, about four intervals), each tick labeled at the
 * interval's decimals; a 100-unit box under the configured camera; a tooltip
 * of the coordinates to three significant digits. Groups follow the
 * group column's level order, otherwise first appearance; rows missing a
 * coordinate or the group are left out, and break paths. The legend lists
 * the groups and the surfaces not named after one, when there are two or more.
 * spec: draw/shared-builders#scatter3d
 */
export function buildScatter3DOption(
  data: Scatter3DData,
  config: Scatter3DChartConfig,
  appearance: Scatter3DAppearance = {},
): Record<string, unknown> {
  if (!config.x || !config.y || !config.z) {
    throw new Error("A 3D scatter chart needs the config members 'x', 'y', and 'z' set.");
  }
  const xs = coordinates(data, config.x, "x");
  const ys = coordinates(data, config.y, "y");
  const zs = coordinates(data, config.z, "z");
  const rawGroup = config.group ? column(data.columns, config.group, "group") : null;
  const group = xs.map((_, i) => {
    if (!rawGroup) return OBSERVATIONS;
    const g = rawGroup[i];
    return isMissing(g) ? null : String(g);
  });
  const keep = xs.map(
    (x, i) => x !== null && ys[i] !== null && zs[i] !== null && group[i] !== null,
  );
  const complete = keep.filter(Boolean).length;
  if (complete === 0)
    throw new Error("A 3D scatter chart needs at least one complete observation.");
  const groups = distinct(
    group.filter((g, i): g is string => keep[i] === true && g !== null),
    config.group ? data.levels?.[config.group] : undefined,
  );
  const mode = config.mode ?? "points";
  const opacity = config.opacity ?? (mode === "lines" ? 1 : autoAlpha(complete));
  const series: Record<string, unknown>[] = [];
  for (const g of groups) {
    // Runs of consecutive complete rows of the group; an incomplete row ends a run.
    const runs: number[][] = [];
    let run: number[] = [];
    group.forEach((value, i) => {
      if (value !== g) return;
      if (keep[i]) run.push(i);
      else if (run.length > 0) {
        runs.push(run);
        run = [];
      }
    });
    if (run.length > 0) runs.push(run);
    const points = runs.map((r) => {
      const rows =
        config.order === "x" ? [...r].sort((a, b) => (xs[a] as number) - (xs[b] as number)) : r;
      return rows.map((i) => [xs[i], ys[i], zs[i]]);
    });
    if (mode === "points" || mode === "both") {
      series.push({
        type: "scatter3D",
        name: g,
        symbolSize: config.point_size ?? 8,
        itemStyle: { opacity },
        data: points.flat(),
      });
    }
    if (mode === "lines" || mode === "both") {
      for (const path of points) {
        if (path.length < 2) continue;
        series.push({
          type: "line3D",
          name: g,
          lineStyle: { width: config.line_width ?? 2, opacity },
          data: path,
        });
      }
    }
  }
  if (series.length === 0) {
    throw new Error("A 3D path needs at least two consecutive complete observations in a group.");
  }
  const surfaces = data.surfaces ?? [];
  // A surface named after a group shares its entry; any other adds one.
  const legendNames = [...new Set([...groups, ...surfaces.map((surface) => surface.name)])];
  for (const surface of surfaces) series.push(surfaceSeries(surface, config.fit_alpha ?? 0.35));
  const finite = (v: unknown): v is number => typeof v === "number" && Number.isFinite(v);
  const values = [xs, ys, zs].map((col, d) => [
    ...col.filter((v, i): v is number => keep[i] === true && v !== null),
    ...surfaces.flatMap((s) =>
      d === 0 ? s.x.filter(finite) : d === 1 ? s.y.filter(finite) : s.z.flat().filter(finite),
    ),
  ]);
  const labels = [config.xlab ?? config.x, config.ylab ?? config.y, config.zlab ?? config.z];
  const axes = values.map((v, d) => {
    const [min, max] = axisRange(v);
    return {
      type: "value",
      ...valueAxis(min, max),
      name: labels[d],
      nameGap: 25,
      axisLine: { lineStyle: { color: "#888888" } },
      axisLabel: { color: "#888888", formatter: axisTicks(niceAxis(min, max), d === 2) },
      nameTextStyle: { color: "#888888" },
    };
  });
  const memberPalette = asArray(config.palette);
  const palette = memberPalette.length > 0 ? memberPalette : appearance.palette;
  const viewSize = config.view_size ?? 190;
  const distance = config.distance ?? 250;
  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    ...(palette && palette.length > 0 ? { color: [...palette] } : {}),
    tooltip: {
      trigger: "item",
      formatter: (p: { marker?: string; seriesName?: string; value?: unknown }) => {
        const v = Array.isArray(p.value) ? p.value : [];
        const lines = labels.map((name, d) => `${name}: ${formatNumber(v[d])}`);
        return [`${p.marker ?? ""}${p.seriesName ?? ""}`, ...lines].join("<br/>");
      },
    },
    legend: legendNames.length > 1 ? { data: legendNames } : { show: false },
    ...Object.fromEntries(AXIS_KEYS.map((key, d) => [key, axes[d]])),
    grid3D: {
      boxWidth: 100,
      boxDepth: 100,
      boxHeight: 100,
      top: 60,
      bottom: 20,
      left: 12,
      right: 12,
      viewControl: {
        projection: config.projection ?? "perspective",
        distance,
        minDistance: 100,
        maxDistance: 1000,
        alpha: config.alpha ?? 20,
        beta: config.beta ?? 40,
        orthographicSize: viewSize - 1,
        minOrthographicSize: 20,
        maxOrthographicSize: 1000,
        animation: false,
        autoRotate: false,
        panMouseButton: "right",
      },
      // rtemis.draw's widget scales the camera from these at each width.
      rtemisViewSize: viewSize,
      rtemisDistance: distance,
    },
    series,
  };
}

/**
 * The 3D axes of `option` widened to the coordinates of `surfaces`, or, with
 * `expand` false, an error when a coordinate lies outside them.
 */
export function fitSurfaceAxes(
  option: Record<string, unknown>,
  surfaces: readonly Record<string, unknown>[],
  expand = true,
): Record<string, unknown> {
  if (surfaces.length === 0) return option;
  const out = { ...option };
  AXIS_KEYS.forEach((key, d) => {
    const axis = option[key] as Record<string, unknown> | undefined;
    const min = axis?.min;
    const max = axis?.max;
    if (!axis || typeof min !== "number" || typeof max !== "number") {
      throw new Error("A surface layer needs a 3D chart with numeric axis limits.");
    }
    const coords = surfaces.flatMap((s) =>
      ((s.data as unknown[][] | undefined) ?? [])
        .map((p) => p[d])
        .filter((v): v is number => typeof v === "number" && Number.isFinite(v)),
    );
    const [lo, hi] = axisRange([min, max, ...coords]);
    if (!expand && (lo < min || hi > max)) {
      throw new Error(
        `A surface layer drawn without expanding the axes must lie within them; its ${"xyz"[d]} coordinates span beyond [${min}, ${max}].`,
      );
    }
    if (lo < min || hi > max) {
      const nice = niceAxis(lo, hi);
      out[key] = {
        ...axis,
        ...valueAxis(lo, hi),
        axisLabel: { ...(axis.axisLabel as object), formatter: axisTicks(nice, d === 2) },
      };
    }
  });
  return out;
}
