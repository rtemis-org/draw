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

/**
 * A number as text with `decimals` decimal places, as rtemis.core's `ddSci()`
 * writes it: exact zero with `decimals` zeros; a magnitude from `hi` in
 * scientific notation with up to `decimals` significant digits; a nonzero
 * value that rounds to zero at `decimals` places in scientific notation with up
 * to two significant digits ("4.6e-09"), so it does not read as zero; any other
 * value fixed to `decimals` places. A value that is not a finite number reads
 * "NA".
 */
export function ddSci(value: number | null | undefined, decimals = 2, hi = 1e6): string {
  if (value === null || value === undefined || !Number.isFinite(value)) return "NA";
  if (value === 0) return (0).toFixed(decimals);
  if (Math.abs(value) >= hi) return scientific(Number(value.toFixed(decimals)), decimals);
  if (Number(value.toFixed(decimals)) === 0) return scientific(value, 2);
  return value.toFixed(decimals);
}

/**
 * Scientific notation as R's `format(x, scientific = TRUE, digits)` writes one
 * number: up to `digits` significant digits, trailing zeros dropped, and a
 * signed exponent of at least two digits.
 */
function scientific(value: number, digits: number): string {
  const [mantissa = "", exponent = "0"] = value.toExponential(Math.max(0, digits - 1)).split("e");
  const trimmed = mantissa.includes(".") ? mantissa.replace(/\.?0+$/, "") : mantissa;
  const e = Number(exponent);
  return `${trimmed}e${e < 0 ? "-" : "+"}${String(Math.abs(e)).padStart(2, "0")}`;
}
