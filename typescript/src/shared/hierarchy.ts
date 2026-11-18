import { type ColumnNames, column, distinct, type TabularData, textColumn } from "./source.ts";

/**
 * One node of a tree: a leaf carries its value, an inner node its children,
 * whose values ECharts sums.
 * spec: draw/shared-builders#treemap
 */
export interface HierarchyNode {
  readonly name: string;
  readonly value?: number;
  readonly children?: readonly HierarchyNode[];
}

/** A tree the host built, as its top-level nodes. */
export interface HierarchyData {
  readonly tree: readonly HierarchyNode[];
}

/** The members of a tree chart's config that bind its table. */
export interface HierarchyMembers {
  /** Columns naming each row's node at each depth, outermost first. */
  readonly levels?: ColumnNames | null;
  /** Column of each row's value; unset counts each row as 1. */
  readonly value?: string | null;
}

interface Building {
  readonly name: string;
  value: number;
  /** Rows ending at this node, 1-based. */
  readonly rows: number[];
  readonly children: Map<string, Building>;
}

const isMissing = (v: unknown) =>
  v === null || v === undefined || v === "" || (typeof v === "number" && Number.isNaN(v));

/**
 * The tree of a table: each row's path is its values of `levels`, outermost
 * first, ending at its first missing level, and the rows ending at one node
 * sum their values. Siblings take their level column's level order,
 * otherwise their order of first appearance. A row missing its first level,
 * a value that is missing, not finite, or negative, a node that both ends a
 * row and has children, and a total of zero stop with an error naming the
 * cause; `chart` begins the errors that name the chart ("A treemap").
 */
export function hierarchyFromTable(
  data: TabularData,
  members: HierarchyMembers,
  chart: string,
): HierarchyNode[] {
  const levels =
    typeof members.levels === "string" ? [members.levels] : [...(members.levels ?? [])];
  if (levels.length === 0) throw new Error(`${chart} needs the config member 'levels' set.`);
  const raw = levels.map((name) => column(data.columns, name, "levels"));
  const text = levels.map((name) => textColumn(data, name, "levels"));
  const valueName = members.value;
  const values = valueName ? column(data.columns, valueName, "value") : null;
  const n = raw[0]?.length ?? 0;
  const root: Building = { name: "", value: 0, rows: [], children: new Map() };
  let total = 0;
  for (let i = 0; i < n; i++) {
    if (isMissing(raw[0]?.[i])) {
      throw new Error(
        `Column '${levels[0]}' (config member 'levels') must name a node in every row; row ${i + 1} is missing.`,
      );
    }
    const v = values ? values[i] : 1;
    if (typeof v !== "number" || !Number.isFinite(v) || v < 0) {
      throw new Error(
        `Column '${valueName}' (config member 'value') must hold a finite, nonnegative number in every row; row ${i + 1} holds '${String(v)}'.`,
      );
    }
    let node = root;
    for (let d = 0; d < levels.length && !isMissing(raw[d]?.[i]); d++) {
      const name = text[d]?.[i] as string;
      let child = node.children.get(name);
      if (!child) {
        child = { name, value: 0, rows: [], children: new Map() };
        node.children.set(name, child);
      }
      node = child;
    }
    node.value += v;
    node.rows.push(i + 1);
    total += v;
  }
  if (!(total > 0)) {
    throw new Error(
      valueName
        ? `${chart} needs a positive total; column '${valueName}' sums to 0.`
        : `${chart} needs at least one row.`,
    );
  }
  const finish = (node: Building, depth: number, path: readonly string[]): HierarchyNode[] => {
    const level = levels[depth] as string;
    const names = distinct([...node.children.keys()], data.levels?.[level]);
    return names.map((name) => {
      const child = node.children.get(name) as Building;
      const at = [...path, name];
      if (child.children.size === 0) return { name, value: child.value };
      if (child.rows.length > 0) {
        throw new Error(
          `Node '${at.join(" / ")}' has children, which give its value; row ${child.rows[0]} ends at it. Give the row its deeper levels, or leave it out.`,
        );
      }
      return { name, children: finish(child, depth + 1, at) };
    });
  };
  return finish(root, 0, []);
}

/** The tree of a chart's data: a host's tree as it is, or the tree of a table. */
export function hierarchyOf(
  data: TabularData | HierarchyData,
  members: HierarchyMembers,
  chart: string,
): readonly HierarchyNode[] {
  return "tree" in data ? data.tree : hierarchyFromTable(data, members, chart);
}

/** The number of levels of a tree: 1 for a list of leaves. */
export function hierarchyDepth(nodes: readonly HierarchyNode[]): number {
  let depth = 0;
  for (const node of nodes) {
    depth = Math.max(depth, 1 + (node.children ? hierarchyDepth(node.children) : 0));
  }
  return depth;
}
