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
  /** The type of a column whose values are not read as stored, by column name. */
  readonly types?: Readonly<Record<string, ColumnType>>;
  /**
   * Column holding the number of rows each row stands for, when the host has
   * combined rows. A chart that counts rows sums it; unset, each row is one.
   */
  readonly frequency?: string;
}

/**
 * A column type the data declares. `"time"`: a date or date-time, held as
 * epoch milliseconds of its wall-clock reading in UTC, or as ISO 8601 text
 * whose wall-clock fields are read and whose offset is ignored.
 */
export type ColumnType = "time";

/** Whether the data declares a column as a time. */
export function isTime(data: TabularData, name: string): boolean {
  return data.types?.[name] === "time";
}

const ISO_TIME = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2})(?:\.(\d+))?)?)?/;

/**
 * A time cell as epoch milliseconds of its wall-clock reading in UTC, or
 * `null` for a missing or unreadable one. Charts place and label times in UTC,
 * so a time reads the same wherever it is viewed.
 */
export function timeMs(value: unknown): number | null {
  if (typeof value === "number") return Number.isFinite(value) ? value : null;
  if (typeof value !== "string") return null;
  const m = ISO_TIME.exec(value);
  if (!m) return null;
  const [, y, mo, d, h, mi, s, frac] = m;
  const ms = frac ? Math.round(Number(`0.${frac}`) * 1000) : 0;
  return Date.UTC(
    Number(y),
    Number(mo) - 1,
    Number(d),
    Number(h ?? 0),
    Number(mi ?? 0),
    Number(s ?? 0),
    ms,
  );
}

/** A time as text in UTC: the date at midnight, otherwise the date and the time. */
export function timeLabel(ms: number): string {
  const iso = new Date(ms).toISOString();
  if (ms % 86_400_000 === 0) return iso.slice(0, 10);
  return `${iso.slice(0, 10)} ${iso.slice(11, ms % 1000 === 0 ? 19 : 23)}`;
}

/** The values of a column as text; a time column's as {@link timeLabel}. */
export function textColumn(data: TabularData, name: string, member: string): string[] {
  const values = column(data.columns, name, member);
  if (!isTime(data, name)) return values.map((v) => String(v));
  return values.map((v) => {
    const ms = timeMs(v);
    return ms === null ? String(v) : timeLabel(ms);
  });
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
