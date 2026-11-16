/**
 * Axis limits derived from data, shared by the cartesian builders. They equal
 * rtemis.draw's `calc_limits()` and `equal_axis_limits()`, which `resolve()`
 * runs to record the limits in a config. spec: draw/shared-builders#line
 */

/** An axis's `[min, max]`. */
export type AxisLimits = readonly [number, number];

/** Fraction of the data range each axis extends by when its limits are unset. */
export const DEFAULT_PAD = 0.04;

/**
 * The range of the finite values extended by `pad` times the range on each
 * side, or `null` without a finite value. A range of zero extends by `pad`
 * times a tenth of the value's magnitude.
 */
export function calcLimits(values: readonly unknown[], pad = DEFAULT_PAD): AxisLimits | null {
  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;
  for (const v of values) {
    if (typeof v !== "number" || !Number.isFinite(v)) continue;
    if (v < min) min = v;
    if (v > max) max = v;
  }
  if (min > max) return null;
  const span = max - min === 0 ? Math.abs(min) * 0.1 : max - min;
  return [min - pad * span, max + pad * span];
}

/**
 * The limits of an axis that shows magnitudes from zero, such as a filled
 * area's: the range of the finite values and zero, extended by `pad` times
 * that range on each side away from zero, so zero is an end of the axis when
 * the values do not cross it. `null` without a finite value.
 */
export function calcLimitsFromZero(
  values: readonly unknown[],
  pad = DEFAULT_PAD,
): AxisLimits | null {
  const range = calcLimits(values, 0);
  if (!range) return null;
  const min = Math.min(range[0], 0);
  const max = Math.max(range[1], 0);
  const span = max - min;
  return [min === 0 ? 0 : min - pad * span, max === 0 ? 0 : max + pad * span];
}

/**
 * The limits of a plot that is both square and equally scaled, whose two axes
 * span one interval: the limits given for either axis, or the padded range of
 * both axes' values. Either request alone leaves the limits as given.
 */
export function equalAxisLimits(
  x: readonly unknown[] | null,
  y: readonly unknown[],
  xlim: AxisLimits | null,
  ylim: AxisLimits | null,
  pad: number,
  square: boolean,
  equalAxes: boolean,
): { xlim: AxisLimits | null; ylim: AxisLimits | null } {
  if (!square || !equalAxes) return { xlim, ylim };
  if (xlim && ylim) {
    const same = xlim.every(
      (v, i) => Math.abs(v - (ylim[i] as number)) <= 1.5e-8 * Math.max(1, Math.abs(v)),
    );
    if (!same) {
      throw new Error(
        `\`square\` and \`equal_axes\` together require both axes to span the same interval, but \`xlim\` is [${xlim.join(", ")}] and \`ylim\` is [${ylim.join(", ")}]. Give them the same limits, or unset one of the two.`,
      );
    }
    return { xlim, ylim };
  }
  const common = xlim ?? ylim ?? calcLimits([...(x ?? []), ...y], pad);
  return common ? { xlim: common, ylim: common } : { xlim, ylim };
}
