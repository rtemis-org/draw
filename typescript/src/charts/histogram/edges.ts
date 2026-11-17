/**
 * Histogram bin edges by R's rules, for a host that bins outside R: the class
 * count of `nclass.Sturges()`, `nclass.scott()`, and `nclass.FD()`, and the
 * edges `graphics::hist()` takes from `pretty()` over the range. rtemislive
 * computes the summary in DuckDB and counts the bins there.
 * spec: draw/shared-builders#histogram
 */

/** The summary of the values to bin that the class-count rules read. */
export interface HistogramSummary {
  /** Number of values. */
  readonly n: number;
  /** Smallest value. */
  readonly min: number;
  /** Largest value. */
  readonly max: number;
  /** Sample standard deviation, for Scott's rule. */
  readonly sd?: number | null;
  /** Lower and upper quartiles (type 7), for the Freedman–Diaconis rule. */
  readonly q1?: number | null;
  readonly q3?: number | null;
}

/** A rule for the number of classes, as `graphics::hist()` names it. */
export type BreaksRule = "Sturges" | "Scott" | "FD" | "Freedman-Diaconis";

/** The largest class count `graphics::hist()` accepts. */
const MAX_CLASSES = 1e6;

/**
 * The class count a rule gives for a summary: `ceiling(log2(n) + 1)` for
 * Sturges, the range over `3.5 * sd * n^(-1/3)` for Scott, and the range over
 * `2 * IQR * n^(-1/3)` for Freedman–Diaconis, each at least 1. A zero spread
 * gives 1. R's `nclass.FD()` replaces a zero IQR by a wider quantile range;
 * a summary carries the quartiles only, so a zero IQR gives 1 here.
 */
export function classCount(summary: HistogramSummary, breaks: BreaksRule): number {
  const { n } = summary;
  const range = summary.max - summary.min;
  switch (breaks) {
    case "Sturges":
      return Math.ceil(Math.log2(n) + 1);
    case "Scott": {
      const h = 3.5 * (summary.sd ?? 0) * n ** (-1 / 3);
      return h > 0 ? Math.max(1, Math.ceil(range / h)) : 1;
    }
    case "FD":
    case "Freedman-Diaconis": {
      const iqr = (summary.q3 ?? 0) - (summary.q1 ?? 0);
      const h = 2 * iqr * n ** (-1 / 3);
      return h > 0 ? Math.max(1, Math.ceil(range / h)) : 1;
    }
  }
}

/**
 * Equally spaced round values covering `[lo, hi]` in about `n` intervals: R's
 * `pretty.default()` with its default biases (`high.u.bias` 1.5, `u5.bias`
 * 2.75, `shrink.sml` 0.75, `eps.correct` 0, `bounds` TRUE), ported from
 * `R_pretty()` in R's `src/appl/pretty.c`.
 */
export function pretty(lo: number, hi: number, n = 5, minN = Math.floor(n / 3)): number[] {
  const h = 1.5;
  const h5 = 0.5 + 1.5 * h;
  const shrink = 0.75;
  const roundingEps = 1e-10;
  const dx = hi - lo;
  let cell: number;
  let small: boolean;
  if (dx === 0 && hi === 0) {
    cell = 1;
    small = true;
  } else {
    cell = Math.max(Math.abs(lo), Math.abs(hi));
    const u = (1 + (h5 >= 1.5 * h + 0.5 ? 1 / (1 + h) : 1.5 / (1 + h5))) * Math.max(1, n);
    small = dx < cell * u * Number.EPSILON * 3;
  }
  if (small) {
    if (cell > 10) cell = 9 + cell / 10;
    cell *= shrink;
    if (minN > 1) cell /= minN;
  } else {
    cell = dx;
    if (n > 1) cell /= n;
  }
  const tiny = 2 ** -20 * Number.MIN_VALUE;
  if (cell < (tiny === 0 ? Number.MIN_VALUE : tiny)) cell = Number.MIN_VALUE;
  else if (cell > Number.MAX_VALUE / 1.25) cell = 0.1 * Number.MAX_VALUE;
  const base = 10 ** Math.floor(Math.log10(cell));
  let unit = base;
  if (2 * base - cell < h * (cell - unit)) {
    unit = 2 * base;
    if (5 * base - cell < h5 * (cell - unit)) {
      unit = 5 * base;
      if (10 * base - cell < h * (cell - unit)) unit = 10 * base;
    }
  }
  let ns = Math.floor(lo / unit + roundingEps);
  let nu = Math.ceil(hi / unit - roundingEps);
  while (ns * unit > lo + roundingEps * unit) ns--;
  while (nu * unit < hi - roundingEps * unit) nu++;
  let k = Math.trunc(0.5 + nu - ns);
  if (k < minN) {
    const d = minN - k;
    if (ns >= 0) {
      nu += Math.trunc(d / 2);
      ns -= Math.trunc(d / 2) + (d % 2);
    } else {
      ns -= Math.trunc(d / 2);
      nu += Math.trunc(d / 2) + (d % 2);
    }
    k = minN;
  }
  // The bounds cover the range: a round bound inside it by the rounding
  // tolerance gives way to the range's own end, as in R.
  const l = Math.min(lo, ns * unit);
  const u = Math.max(hi, nu * unit);
  // seq.int(l, u, length.out = k + 1), with values within 1e-14 of an
  // interval of zero set to zero.
  const by = (u - l) / k;
  return Array.from({ length: k + 1 }, (_, i) => {
    const v = i === k ? u : l + i * by;
    return Math.abs(v) < 1e-14 * by ? 0 : v;
  });
}

/**
 * Histogram edges: `bins`, a suggested class count, or else the class count
 * of `breaks` (Sturges by default), rounded to `pretty()` values over the
 * range with at least one class, as `graphics::hist()` computes them.
 */
export function histogramEdges(
  summary: HistogramSummary,
  options: { readonly breaks?: BreaksRule | null; readonly bins?: number | null } = {},
): number[] {
  if (!(summary.n > 0) || !Number.isFinite(summary.min) || !Number.isFinite(summary.max)) {
    throw new Error("A histogram needs at least one finite value to bin.");
  }
  const count = Math.min(
    options.bins ?? classCount(summary, options.breaks ?? "Sturges"),
    MAX_CLASSES,
  );
  if (!(count >= 1)) {
    throw new Error(`\`bins\` must be at least 1; it is ${count}.`);
  }
  return pretty(summary.min, summary.max, count, 1);
}
