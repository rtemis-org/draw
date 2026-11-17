/**
 * Kernel density estimates by R's `stats::density()` rules, for a host that
 * estimates outside R from binned counts: the `bw.nrd0()` and `bw.nrd()`
 * bandwidths, `adjust`, every `stats::density()` kernel (each scaled so that
 * the bandwidth is its standard deviation), and a curve extending `cut`
 * bandwidths past the sample. rtemislive bins the values in DuckDB.
 * spec: draw/shared-builders#density
 */

/** The summary of a sample the bandwidth rules read. */
export interface DensitySummary {
  /** Number of values. */
  readonly n: number;
  /** Smallest value. */
  readonly min: number;
  /** Largest value. */
  readonly max: number;
  /** Sample standard deviation. */
  readonly sd?: number | null;
  /** Lower and upper quartiles (type 7). */
  readonly q1?: number | null;
  readonly q3?: number | null;
}

/** A smoothing kernel, as `stats::density()` names it. */
export type DensityKernel =
  | "gaussian"
  | "epanechnikov"
  | "rectangular"
  | "triangular"
  | "biweight"
  | "cosine"
  | "optcosine";

/** A bandwidth rule, as `stats::density()`'s `bw` names it. */
export type BandwidthRule = "nrd0" | "nrd" | "ucv" | "bcv" | "SJ" | "SJ-ste" | "SJ-dpi";

/** How a density is estimated. */
export interface DensityOptions {
  /** Bandwidth rule, used when `bandwidth` is unset. Default `"nrd0"`. */
  readonly bw?: BandwidthRule;
  /** Bandwidth in measurement units, before `adjust`. */
  readonly bandwidth?: number | null;
  /** Bandwidth multiplier. Default 1. */
  readonly adjust?: number;
  /** Smoothing kernel. Default `"gaussian"`. */
  readonly kernel?: DensityKernel;
  /** Points at which the density is evaluated. Default 512. */
  readonly n?: number;
  /** Bandwidths the curve extends past each end of the sample. Default 3. */
  readonly cut?: number;
}

/**
 * A bandwidth rule's bandwidth for a summary. `nrd0`: 0.9 × min(sd, IQR /
 * 1.34) × n^-1/5, the sd when that minimum is 0, then |min|, then 1. `nrd`:
 * 1.06 × min(sd, IQR / 1.34) × n^-1/5. The cross-validated and plug-in rules
 * need R; a host without it stops.
 */
export function ruleBandwidth(summary: DensitySummary, rule: BandwidthRule = "nrd0"): number {
  const sd = summary.sd ?? 0;
  const iqr = ((summary.q3 ?? 0) - (summary.q1 ?? 0)) / 1.34;
  const scale = summary.n ** -0.2;
  switch (rule) {
    case "nrd0": {
      const lo = Math.min(sd, iqr) || sd || Math.abs(summary.min) || 1;
      return 0.9 * lo * scale;
    }
    case "nrd":
      return 1.06 * Math.min(sd, iqr) * scale;
    default:
      throw new Error(
        `The bandwidth rule '${rule}' is computed in R; set \`bw\` to "nrd0" or "nrd", or set \`bandwidth\`.`,
      );
  }
}

/** The kernel density at distance `u` for a kernel whose standard deviation is `bw`. */
function kernelAt(kernel: DensityKernel, u: number, bw: number): number {
  const au = Math.abs(u);
  switch (kernel) {
    case "gaussian":
      return Math.exp(-0.5 * (u / bw) ** 2) / (bw * Math.sqrt(2 * Math.PI));
    case "rectangular": {
      const a = bw * Math.sqrt(3);
      return au < a ? 0.5 / a : 0;
    }
    case "triangular": {
      const a = bw * Math.sqrt(6);
      return au < a ? (1 - au / a) / a : 0;
    }
    case "epanechnikov": {
      const a = bw * Math.sqrt(5);
      return au < a ? (0.75 * (1 - (au / a) ** 2)) / a : 0;
    }
    case "biweight": {
      const a = bw * Math.sqrt(7);
      return au < a ? ((15 / 16) * (1 - (au / a) ** 2) ** 2) / a : 0;
    }
    case "cosine": {
      const a = bw / Math.sqrt(1 / 3 - 2 / Math.PI ** 2);
      return au < a ? (1 + Math.cos((Math.PI * u) / a)) / (2 * a) : 0;
    }
    case "optcosine": {
      const a = bw / Math.sqrt(1 - 8 / Math.PI ** 2);
      return au < a ? ((Math.PI / 4) * Math.cos((Math.PI * u) / (2 * a))) / a : 0;
    }
  }
}

/**
 * A kernel density in density units from a sample's counts in equal bins
 * between its minimum and maximum, each bin's count placed at its center:
 * the bandwidth (`bandwidth`, or the rule's) times `adjust`, evaluated at `n`
 * points from `cut` bandwidths below the minimum to `cut` above the maximum,
 * as `stats::density()` evaluates it. A sample of fewer than two values, or
 * of one repeated value, has no rule bandwidth: without `bandwidth` it has no
 * curve.
 */
export function kernelDensity(
  summary: DensitySummary,
  counts: ArrayLike<number>,
  options: DensityOptions = {},
): { x: number[]; y: number[]; bw: number } | null {
  const { n: size, min, max } = summary;
  const explicit = options.bandwidth ?? null;
  if (!(size > 0) || (explicit === null && (size < 2 || max === min))) return null;
  const bw = (explicit ?? ruleBandwidth(summary, options.bw)) * (options.adjust ?? 1);
  if (!(bw > 0) || !Number.isFinite(bw)) return null;
  const kernel = options.kernel ?? "gaussian";
  const points = options.n ?? 512;
  const cut = options.cut ?? 3;
  const lo = min - cut * bw;
  const hi = max + cut * bw;
  const bins = counts.length;
  const step = (max - min) / bins;
  const centers = Array.from({ length: bins }, (_, b) =>
    max === min ? min : min + (b + 0.5) * step,
  );
  const x = Array.from({ length: points }, (_, i) => lo + ((hi - lo) * i) / (points - 1));
  const y = x.map((at) => {
    let sum = 0;
    for (let b = 0; b < bins; b++) {
      const count = counts[b] as number;
      if (count !== 0) sum += count * kernelAt(kernel, at - (centers[b] as number), bw);
    }
    return sum / size;
  });
  return { x, y, bw };
}
