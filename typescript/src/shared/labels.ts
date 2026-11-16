/**
 * Axis names a host derives from a config when the author sets none.
 * spec: draw/shared-builders#bar
 */
import { asArray, type ColumnNames } from "./source.ts";

/** The value-axis members a measure is named from. */
export interface MeasureMembers {
  readonly y?: ColumnNames | null;
  readonly aggregate?: "none" | "sum" | "mean" | "median";
}

/**
 * The name of a value axis: the measure the chart shows. With `y` unset,
 * "Count" (a bar chart counting rows); one `y` column gives its name, prefixed
 * by the aggregate ("Mean X", "Median X", "Sum of X"); several give the
 * aggregate alone, or `null` without one, since the legend names each
 * series. `name` maps a column name to its display text. Hosts set it as the
 * value axis's name unless the author set one. rtemis.draw's `resolve()`
 * derives the same text.
 */
export function valueLabel(
  config: MeasureMembers,
  name: (column: string) => string = (column) => column,
): string | null {
  const ys = asArray(config.y);
  if (ys.length === 0) return "Count";
  const only = ys.length === 1 ? name(ys[0] as string) : null;
  switch (config.aggregate ?? "none") {
    case "mean":
      return only === null ? "Mean" : `Mean ${only}`;
    case "median":
      return only === null ? "Median" : `Median ${only}`;
    case "sum":
      return only === null ? "Sum" : `Sum of ${only}`;
    default:
      return only;
  }
}
