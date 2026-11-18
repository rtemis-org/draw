import { formatNumber } from "../../shared/format.ts";
import { type MapResolution, normalizeLocation } from "./locations.ts";

/** A corner of the map an overlay is anchored to. */
export type MapCorner = "top-left" | "top-right" | "bottom-left" | "bottom-right";

/** How values are binned into classes: equal counts, equal intervals, or Jenks natural breaks. */
export type MapClassification = "quantile" | "equal" | "jenks";

/** The color schemes of a choropleth: five sequential, then four diverging. */
export const CHOROPLETH_COLORMAPS = Object.freeze([
  "blues",
  "viridis",
  "ylorrd",
  "greens",
  "magma",
  "rdbu",
  "rdylgn",
  "spectral",
  "brbg",
] as const);

/** A color scheme of a choropleth. */
export type ChoroplethColormap = (typeof CHOROPLETH_COLORMAPS)[number];

/**
 * An instance of the `chart/choropleth/v1` schema, written by rtemis.draw's
 * `ChoroplethConfig` class. Every member is optional. `location`, `value`,
 * and `tooltip` name the columns the host builds the map model from.
 * spec: draw/shared-builders#choropleth
 */
export interface ChoroplethChartConfig {
  readonly type?: "choropleth";
  readonly title?: string | null;
  readonly location?: string | null;
  readonly value?: string | null;
  readonly tooltip?: string | null;
  /** Default `"country"`. */
  readonly resolution?: MapResolution;
  /** Default `"quantile"`. */
  readonly classification?: MapClassification;
  /** Number of classes, rounded to a whole number within [2, 12]. Default 5. */
  readonly num_classes?: number;
  /** Default `"blues"`. */
  readonly colormap?: ChoroplethColormap;
  /** Region fill opacity. Default 1. */
  readonly opacity?: number;
  /** Draw region outlines. Default `true`. */
  readonly show_boundaries?: boolean;
  /** Outline width in pixels. Default 0.2. */
  readonly outline_width?: number;
  /** Draw the legend. Default `true`. */
  readonly show_legend?: boolean;
  /** Default `"bottom-right"`. */
  readonly legend_position?: MapCorner;
  /** Default `"top-right"`. */
  readonly tooltip_position?: MapCorner;
  /** Heading of the legend and label of the tooltip value; unset uses the model's. */
  readonly value_label?: string | null;
}

/** Every member of {@link ChoroplethChartConfig}, checked against `chart/choropleth/v1`. */
export const CHOROPLETH_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "location",
  "value",
  "tooltip",
  "resolution",
  "classification",
  "num_classes",
  "colormap",
  "opacity",
  "show_boundaries",
  "outline_width",
  "show_legend",
  "legend_position",
  "tooltip_position",
  "value_label",
] as const satisfies readonly (keyof ChoroplethChartConfig)[]);

type Missing = Exclude<
  keyof ChoroplethChartConfig,
  (typeof CHOROPLETH_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** One region's datum: its location key as the data holds it, its value, and tooltip fields. */
export interface MapRow {
  readonly location: string;
  readonly value: number | null;
  readonly extras?: Readonly<Record<string, string | number | null>> | null;
}

/** A choropleth as the host builds it from its data. */
export interface MapModel {
  readonly rows: readonly MapRow[];
  readonly resolution?: MapResolution | null;
  /** Label of the value; the legend's heading and the tooltip's value label. */
  readonly valueLabel?: string | null;
  /** The `extras` keys the tooltip lists, in order. */
  readonly tooltipFields?: readonly string[] | null;
}

/** The TopoJSON object, initial center (longitude, latitude), and zoom of each resolution. */
export const CHOROPLETH_GEOMETRY: Readonly<
  Record<
    MapResolution,
    { readonly object: string; readonly center: readonly [number, number]; readonly zoom: number }
  >
> = Object.freeze({
  country: { object: "countries", center: [0, 20], zoom: 0.4 },
  state: { object: "states", center: [-96, 38], zoom: 2.6 },
  county: { object: "counties", center: [-96, 38], zoom: 2.6 },
});

/** A GeoJSON feature of the map, its geometry id copied to `properties.joinId`. */
export interface MapFeature {
  readonly type: "Feature";
  readonly id?: string | number;
  properties: Record<string, unknown> & { joinId: string };
  readonly geometry: unknown;
}

/** The map's regions and the ids that have one. */
export interface MapFeatures {
  readonly features: { readonly type: "FeatureCollection"; readonly features: MapFeature[] };
  readonly ids: ReadonlySet<string>;
}

/**
 * The regions of a resolution from a TopoJSON topology, decoded by the host's
 * `feature` from `topojson-client`. Each feature carries its geometry id as
 * `properties.joinId`, the key a MapLibre source promotes and a join matches.
 */
export function mapFeatures(
  topology: { readonly objects: Readonly<Record<string, unknown>> },
  resolution: MapResolution,
  feature: (topology: never, object: never) => unknown,
): MapFeatures {
  const name = CHOROPLETH_GEOMETRY[resolution].object;
  const object = topology.objects[name];
  if (!object) throw new Error(`The map geometry has no object '${name}'.`);
  const collection = feature(topology as never, object as never) as {
    type: "FeatureCollection";
    features: MapFeature[];
  };
  const ids = new Set<string>();
  for (const f of collection.features) {
    const id = f.id === undefined || f.id === null ? "" : String(f.id);
    f.properties = { ...(f.properties ?? {}), joinId: id };
    if (id) ids.add(id);
  }
  return { features: collection, ids };
}

/** A tooltip line: a field's name and its value as text. */
export interface MapTooltipField {
  readonly label: string;
  readonly value: string;
}

/** The rows of a model joined to the regions. */
export interface ChoroplethJoin {
  /** The value of each region that has one. */
  readonly values: ReadonlyMap<string, number>;
  /** The tooltip fields of each region, in `tooltipFields` order. */
  readonly extras: ReadonlyMap<string, readonly MapTooltipField[]>;
  readonly matched: number;
  readonly unmatched: number;
  /** The first 12 location keys without a region. */
  readonly unmatchedKeys: readonly string[];
}

const fieldText = (v: string | number) => (typeof v === "number" ? String(formatNumber(v)) : v);

/**
 * The rows of a model joined to the regions by {@link normalizeLocation}: a
 * row whose key names a region sets that region's value and tooltip fields,
 * a row with a missing or non-finite value leaving the region without one;
 * other rows count as unmatched. Two rows naming one region stop with an
 * error naming both rows. Numeric tooltip fields take {@link formatNumber}.
 */
export function joinChoropleth(model: MapModel, ids: ReadonlySet<string>): ChoroplethJoin {
  const resolution = model.resolution ?? "country";
  const fields = model.tooltipFields ?? [];
  const values = new Map<string, number>();
  const extras = new Map<string, MapTooltipField[]>();
  const rowOf = new Map<string, number>();
  const unmatchedKeys: string[] = [];
  let unmatched = 0;
  model.rows.forEach((row, i) => {
    const id = normalizeLocation(row.location, resolution);
    if (!id || !ids.has(id)) {
      unmatched++;
      if (unmatchedKeys.length < 12) unmatchedKeys.push(row.location);
      return;
    }
    const first = rowOf.get(id);
    if (first !== undefined) {
      throw new Error(
        `A region takes one value; rows ${first + 1} and ${i + 1} both name region '${id}' ('${model.rows[first]?.location}', '${row.location}').`,
      );
    }
    rowOf.set(id, i);
    if (typeof row.value === "number" && Number.isFinite(row.value)) values.set(id, row.value);
    const lines: MapTooltipField[] = [];
    for (const f of fields) {
      const v = row.extras?.[f];
      if (v !== null && v !== undefined) lines.push({ label: f, value: fieldText(v) });
    }
    if (lines.length > 0) extras.set(id, lines);
  });
  return { values, extras, matched: rowOf.size, unmatched, unmatchedKeys };
}

/** A color interpolator over [0, 1]. */
export type ColorInterpolator = (t: number) => string;

/** The `d3-scale-chromatic` interpolators of the color schemes, as the host loads them. */
export interface ChoroplethInterpolators {
  readonly interpolateBlues: ColorInterpolator;
  readonly interpolateViridis: ColorInterpolator;
  readonly interpolateYlOrRd: ColorInterpolator;
  readonly interpolateGreens: ColorInterpolator;
  readonly interpolateMagma: ColorInterpolator;
  readonly interpolateRdBu: ColorInterpolator;
  readonly interpolateRdYlGn: ColorInterpolator;
  readonly interpolateSpectral: ColorInterpolator;
  readonly interpolateBrBG: ColorInterpolator;
}

const INTERPOLATOR: Readonly<Record<ChoroplethColormap, keyof ChoroplethInterpolators>> = {
  blues: "interpolateBlues",
  viridis: "interpolateViridis",
  ylorrd: "interpolateYlOrRd",
  greens: "interpolateGreens",
  magma: "interpolateMagma",
  rdbu: "interpolateRdBu",
  rdylgn: "interpolateRdYlGn",
  spectral: "interpolateSpectral",
  brbg: "interpolateBrBG",
};

const DIVERGING = new Set<ChoroplethColormap>(["rdbu", "rdylgn", "spectral", "brbg"]);

/** Theme values the scale needs. */
export interface ChoroplethAppearance {
  /** Default `"light"`. */
  readonly scheme?: "light" | "dark";
}

/** Fill of a region without a value, and outline colors, by scheme. */
export const CHOROPLETH_COLORS = Object.freeze({
  missing: Object.freeze({ light: "#e4e4e7", dark: "#3f3f46" }),
  outline: Object.freeze({ light: "#cbd5e1", dark: "#3f3f46" }),
});

/** A choropleth's classes: interior breaks, a color per class, and their legend. */
export interface ChoroplethScale {
  /** Interior breaks, ascending, strictly between `min` and `max`. */
  readonly thresholds: readonly number[];
  /** One color per class, `thresholds.length + 1` of them. */
  readonly colors: readonly string[];
  readonly missingColor: string;
  readonly min: number;
  readonly max: number;
  /** One entry per class, labeled with its bounds. */
  readonly legend: readonly { readonly color: string; readonly label: string }[];
}

/** `n` colors sampled evenly from a scheme; a sequential scheme skips its lightest and darkest ends. */
function rampColors(interp: ColorInterpolator, n: number, dark: boolean, diverging: boolean) {
  // A sequential scheme skips its near-white end, and in the dark scheme more
  // of it, so the lowest class stays visible on the background.
  const t0 = diverging ? 0 : dark ? 0.3 : 0.12;
  const t1 = diverging ? 1 : dark ? 1 : 0.95;
  return Array.from({ length: n }, (_, i) => interp(t0 + ((t1 - t0) * i) / (n - 1)));
}

/** Breaks at the i/classes quantiles, by linear interpolation between order statistics. */
function quantileBreaks(sorted: readonly number[], classes: number): number[] {
  const breaks: number[] = [];
  for (let i = 1; i < classes; i++) {
    const q = (sorted.length - 1) * (i / classes);
    const lo = sorted[Math.floor(q)] as number;
    const hi = sorted[Math.ceil(q)] as number;
    breaks.push(lo + (hi - lo) * (q - Math.floor(q)));
  }
  return breaks;
}

function equalBreaks(min: number, max: number, classes: number): number[] {
  const step = (max - min) / classes;
  return Array.from({ length: classes - 1 }, (_, i) => min + step * (i + 1));
}

/** The largest number of values the Jenks dynamic program runs on; larger inputs are sampled evenly. */
const JENKS_CAP = 1500;

/**
 * Jenks natural breaks by the Fisher–Jenks dynamic program over sorted
 * values, each break the lowest value of a class; at most
 * {@link JENKS_CAP} values, sampled evenly from larger inputs, and quantile
 * breaks for no more values than classes. Tied values can leave classes
 * empty, which gives fewer breaks.
 */
function jenksBreaks(values: readonly number[], classes: number): number[] {
  let data = values;
  if (data.length > JENKS_CAP) {
    const step = data.length / JENKS_CAP;
    data = Array.from({ length: JENKS_CAP }, (_, i) => values[Math.floor(i * step)] as number);
  }
  const n = data.length;
  if (n <= classes) return quantileBreaks(values, classes);
  const lower: number[][] = Array.from({ length: n + 1 }, () => new Array(classes + 1).fill(0));
  const cost: number[][] = Array.from({ length: n + 1 }, () => new Array(classes + 1).fill(0));
  for (let j = 1; j <= classes; j++) {
    (lower[1] as number[])[j] = 1;
    for (let i = 2; i <= n; i++) (cost[i] as number[])[j] = Number.POSITIVE_INFINITY;
  }
  for (let l = 2; l <= n; l++) {
    const lowerL = lower[l] as number[];
    const costL = cost[l] as number[];
    let s1 = 0;
    let s2 = 0;
    for (let m = 1; m <= l; m++) {
      const i3 = l - m + 1;
      const val = data[i3 - 1] as number;
      s1 += val;
      s2 += val * val;
      const variance = s2 - (s1 * s1) / m;
      const i4 = i3 - 1;
      if (i4 === 0) continue;
      const costI4 = cost[i4] as number[];
      for (let j = 2; j <= classes; j++) {
        const candidate = variance + (costI4[j - 1] as number);
        if ((costL[j] as number) >= candidate) {
          lowerL[j] = i3;
          costL[j] = candidate;
        }
      }
    }
    lowerL[1] = 1;
    costL[1] = s2 - (s1 * s1) / l;
  }
  const breaks: number[] = [];
  let k = n;
  // With fewer distinct values than classes, the walk reaches the first value
  // before the last class; the remaining classes are empty.
  for (let j = classes; j >= 2 && k >= 1; j--) {
    const start = ((lower[k] as number[])[j] as number) - 1;
    breaks.push(data[start] as number);
    k = start;
  }
  return breaks.reverse();
}

const boundText = (v: number) => String(formatNumber(v));

/**
 * The classes of a choropleth over the values of its joined regions:
 * `num_classes` (default 5, rounded, within [2, 12]) classes by
 * `classification`, repeated breaks and breaks outside (min, max) dropped,
 * so ties can leave fewer classes; colors sampled from `colormap` (default
 * `"blues"`), a sequential scheme over [0.12, 0.95], or [0.3, 1] in the dark
 * scheme, a diverging one over [0, 1]; the missing color of the scheme; and
 * one legend entry per class labeled "low – high", bounds in
 * {@link formatNumber}. No finite value gives no breaks and no legend.
 * spec: draw/shared-builders#choropleth
 */
export function buildChoroplethScale(
  values: Iterable<number | null | undefined>,
  config: ChoroplethChartConfig,
  appearance: ChoroplethAppearance,
  interpolators: ChoroplethInterpolators,
): ChoroplethScale {
  const dark = appearance.scheme === "dark";
  const missingColor = CHOROPLETH_COLORS.missing[dark ? "dark" : "light"];
  const colormap = config.colormap ?? "blues";
  const interp = interpolators[INTERPOLATOR[colormap] ?? "interpolateBlues"];
  const classes = Math.max(2, Math.min(12, Math.round(config.num_classes ?? 5)));
  const colors = rampColors(interp, classes, dark, DIVERGING.has(colormap));
  const sorted = [...values]
    .filter((v): v is number => typeof v === "number" && Number.isFinite(v))
    .sort((a, b) => a - b);
  if (sorted.length === 0) {
    return { thresholds: [], colors: colors.slice(0, 1), missingColor, min: 0, max: 0, legend: [] };
  }
  const min = sorted[0] as number;
  const max = sorted[sorted.length - 1] as number;
  const method = config.classification ?? "quantile";
  const raw =
    min === max
      ? []
      : method === "equal"
        ? equalBreaks(min, max, classes)
        : method === "jenks"
          ? jenksBreaks(sorted, classes)
          : quantileBreaks(sorted, classes);
  const thresholds = raw
    .filter((t, i, a) => i === 0 || t > (a[i - 1] as number))
    .filter((t) => t > min && t < max);
  const bounds = [min, ...thresholds, max];
  const used = colors.slice(0, thresholds.length + 1);
  return {
    thresholds,
    colors: used,
    missingColor,
    min,
    max,
    legend: used.map((color, i) => ({
      color,
      label: `${boundText(bounds[i] as number)} – ${boundText(bounds[i + 1] as number)}`,
    })),
  };
}

/** The fill of a value: its class's color, the missing color for a missing or non-finite value. */
export function choroplethColor(scale: ChoroplethScale, value: number | null | undefined): string {
  if (typeof value !== "number" || !Number.isFinite(value)) return scale.missingColor;
  let i = 0;
  while (i < scale.thresholds.length && value >= (scale.thresholds[i] as number)) i++;
  return scale.colors[Math.min(i, scale.colors.length - 1)] ?? scale.missingColor;
}

/**
 * The MapLibre `fill-color` expression of a scale over the feature state `v`:
 * a step over the breaks, or the one color without breaks, and the missing
 * color where a region has no value.
 */
export function choroplethFillExpression(scale: ChoroplethScale): unknown[] {
  const matched =
    scale.thresholds.length === 0
      ? (scale.colors[0] ?? scale.missingColor)
      : [
          "step",
          ["to-number", ["feature-state", "v"]],
          scale.colors[0],
          ...scale.thresholds.flatMap((t, i) => [t, scale.colors[i + 1]]),
        ];
  return ["case", ["!=", ["feature-state", "v"], null], matched, scale.missingColor];
}
