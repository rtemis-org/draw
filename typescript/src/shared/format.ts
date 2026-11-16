/**
 * Number formatting shared by the builders' tooltips and labels.
 * spec: draw/shared-builders#numbers
 */

/** Significant digits a non-integer is shown with. */
export const SIGNIFICANT_DIGITS = 3;

/**
 * A number as text with `digits` significant digits: an integer as it is; a
 * non-integer rounded to `digits` significant digits, keeping every digit of
 * its integer part, in exponent form below 1e-3 or from 1e15; anything that
 * is not a finite number as it is.
 */
export function formatNumber(value: unknown, digits = SIGNIFICANT_DIGITS): unknown {
  if (typeof value !== "number" || !Number.isFinite(value)) return value;
  if (Number.isInteger(value)) return String(value);
  const magnitude = Math.abs(value);
  if (magnitude < 1e-3 || magnitude >= 1e15) return value.toExponential(digits - 1);
  if (Math.floor(Math.log10(magnitude)) + 1 >= digits) return value.toFixed(0);
  const rounded = Number(value.toPrecision(digits));
  const decimals = Math.max(0, digits - 1 - Math.floor(Math.log10(Math.abs(rounded))));
  return rounded.toFixed(decimals);
}
