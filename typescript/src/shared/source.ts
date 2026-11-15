/**
 * Columns by name: an object from column name to the column's values, as an
 * array or a typed array (rtemislive reads numeric columns from Arrow as
 * `Float64Array`, with missing values as `NaN`).
 */
export type ColumnarSource = Readonly<Record<string, ArrayLike<unknown>>>;

/**
 * A tabular chart's data: the columns the config binds, and the level order of
 * any categorical column among them. spec: draw/shared-builders#tabular-data
 */
export interface TabularData {
  readonly columns: ColumnarSource;
  /** Level order of categorical columns, by column name. */
  readonly levels?: Readonly<Record<string, readonly string[]>>;
}

/**
 * The distinct values of a column in level order: the levels that occur, then
 * values not among the levels in order of appearance.
 */
export function distinct(values: readonly string[], levels?: readonly string[]): string[] {
  const seen = new Set(values);
  const ordered = (levels ?? []).filter((level) => seen.has(level));
  const known = new Set(ordered);
  return [...ordered, ...[...seen].filter((v) => !known.has(v))];
}

/** A config member that names one or more columns, as JSON may carry it. */
export type ColumnNames = string | readonly string[];

/**
 * The values of a column as an array, or an error naming the column and the
 * member. A typed array is copied, so mapping it to text keeps the text.
 */
export function column(source: ColumnarSource, name: string, member: string): readonly unknown[] {
  const values = source[name];
  if (values === undefined) {
    throw new Error(`Column '${name}' (config member '${member}') is not in the data.`);
  }
  return Array.isArray(values) ? (values as readonly unknown[]) : Array.from(values);
}

/** A one-or-many member as an array; a JSON scalar is one element. */
export function asArray<T>(value: T | readonly T[] | null | undefined): readonly T[] {
  if (value === null || value === undefined) return [];
  return Array.isArray(value) ? (value as readonly T[]) : [value as T];
}

/** A numeric cell, or `null` for a missing or non-finite one. */
export function numberOrNull(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) ? value : null;
}
