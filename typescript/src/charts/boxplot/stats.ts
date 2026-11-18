/**
 * Box statistics of one sample, as rtemis.draw's `boxplot_summary()` computes
 * them, for a host that summarizes values in memory.
 * spec: draw/shared-builders#boxplot
 */

/** The quartile convention and the whisker fence multiplier. */
export interface BoxplotStatsOptions {
  /** `"linear"`: `stats::quantile(type = 7)`; `"hinges"`: Tukey's hinges, `stats::fivenum()`. Default `"linear"`. */
  readonly quartiles?: "linear" | "hinges";
  /** IQR multiplier for the whisker fences; 0 spans the full range. Default 1.5. */
  readonly whisker?: number;
}

/** The five plotting statistics of a sample and which values lie outside the fences. */
export interface BoxplotStats {
  /** Lower whisker end, lower quartile, median, upper quartile, upper whisker end. */
  readonly stats: readonly [number, number, number, number, number];
  /** Per value, in input order: outside the whisker fences. */
  readonly outlier: readonly boolean[];
}

/**
 * The box statistics of the finite `values`, or `null` without one. The
 * quartiles follow `quartiles`; each whisker ends at the most extreme value
 * within `whisker` times the interquartile range of its quartile, and reaches
 * at least the quartile, so the five statistics stay ordered.
 */
export function boxplotStats(
  values: readonly number[],
  options: BoxplotStatsOptions = {},
): BoxplotStats | null {
  const finite = values.filter((v) => Number.isFinite(v));
  if (finite.length === 0) return null;
  const sorted = [...finite].sort((a, b) => a - b);
  const [q1, q2, q3] =
    (options.quartiles ?? "linear") === "hinges" ? hinges(sorted) : linear(sorted);
  const whisker = options.whisker ?? 1.5;
  const iqr = q3 - q1;
  const inside = (v: number) =>
    whisker === 0 || (v >= q1 - whisker * iqr && v <= q3 + whisker * iqr);
  let low = Math.min(q1, q3);
  let high = Math.max(q1, q3);
  for (const v of finite) {
    if (!inside(v)) continue;
    if (v < low) low = v;
    if (v > high) high = v;
  }
  return {
    stats: [low, q1, q2, q3, high],
    outlier: values.map((v) => Number.isFinite(v) && !inside(v)),
  };
}

/** Quartiles by linear interpolation, `stats::quantile(type = 7)`. */
function linear(sorted: readonly number[]): [number, number, number] {
  const at = (p: number) => {
    const h = (sorted.length - 1) * p;
    const lo = Math.floor(h);
    const a = sorted[lo] as number;
    const b = sorted[Math.min(lo + 1, sorted.length - 1)] as number;
    return a + (h - lo) * (b - a);
  };
  return [at(0.25), at(0.5), at(0.75)];
}

/** Tukey's hinges and the median, `stats::fivenum()[2:4]`. */
function hinges(sorted: readonly number[]): [number, number, number] {
  const n = sorted.length;
  const n4 = Math.floor((n + 3) / 2) / 2;
  const at = (d: number) =>
    0.5 * ((sorted[Math.floor(d) - 1] as number) + (sorted[Math.ceil(d) - 1] as number));
  return [at(n4), at((n + 1) / 2), at(n + 1 - n4)];
}
