import { ddSci } from "../../shared/format.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import type { AspectHint } from "../line/config.ts";

/** The members ROC and precision-recall configs share. */
interface CurveMembers extends LegendMembers {
  /** Column of class labels, for one-vs-rest curves. */
  readonly class_label?: string | null;
  /** Column of sample or model labels. */
  readonly split?: string | null;
  /** Column of resample labels; `aggregate` marks pooled curves. */
  readonly fold?: string | null;
  /** Column of omitted-pair counts, constant within each curve. */
  readonly omitted?: string | null;
  /** Pooled curves or one curve per resample. Default `"aggregate"`. */
  readonly variant?: "aggregate" | "per_resample";
  /** Decimals of summaries in the legend and of tooltip values (`ddSci()`). Default 2. */
  readonly digits?: number;
  /** Group colors, by class for multiclass curves and by split otherwise. */
  readonly palette?: ColumnNames | null;
  /** Show the legend. Default `true`. */
  readonly legend?: boolean;
  /** Keep the plotting box square. Default `true`. */
  readonly square?: boolean;
  /** Curve stroke width in pixels. Default 2. */
  readonly line_width?: number;
  /** Opacity of per-resample curves. Default 0.45. */
  readonly fold_opacity?: number;
  /** Horizontal axis name. */
  readonly xlab?: string | null;
  /** Vertical axis name. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/**
 * An instance of the `chart/roc/v1` schema, written by rtemis.draw's
 * `ROCConfig` class. Every member is optional. The members that bind and
 * group the records are applied by the host, which computes {@link CurveData}.
 * spec: draw/shared-builders#curves
 */
export interface RocChartConfig extends CurveMembers {
  readonly type?: "roc";
  readonly fpr?: string;
  readonly tpr?: string;
  readonly auc?: string | null;
  /** Draw the chance diagonal. Default `true`. */
  readonly diagonal?: boolean;
  /** Chance-line color. Default `"#888888"`. */
  readonly diagonal_color?: string;
}

/**
 * An instance of the `chart/pr/v1` schema, written by rtemis.draw's
 * `PRConfig` class. Every member is optional. The members that bind and group
 * the records are applied by the host, which computes {@link CurveData}.
 * spec: draw/shared-builders#curves
 */
export interface PrChartConfig extends CurveMembers {
  readonly type?: "pr";
  readonly recall?: string;
  readonly precision?: string;
  readonly ap?: string | null;
  readonly prevalence?: string | null;
  /** Draw each group's prevalence baseline. Default `true`. */
  readonly baseline?: boolean;
}

const SHARED_MEMBERS = [
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "class_label",
  "split",
  "fold",
  "omitted",
  "variant",
  "digits",
  "palette",
  "legend",
  "square",
  "line_width",
  "fold_opacity",
  "xlab",
  "ylab",
] as const;

/** Every member of {@link RocChartConfig}, checked against `chart/roc/v1`. */
export const ROC_CHART_CONFIG_MEMBERS = Object.freeze([
  ...SHARED_MEMBERS,
  "fpr",
  "tpr",
  "auc",
  "diagonal",
  "diagonal_color",
] as const satisfies readonly (keyof RocChartConfig)[]);

/** Every member of {@link PrChartConfig}, checked against `chart/pr/v1`. */
export const PR_CHART_CONFIG_MEMBERS = Object.freeze([
  ...SHARED_MEMBERS,
  "recall",
  "precision",
  "ap",
  "prevalence",
  "baseline",
] as const satisfies readonly (keyof PrChartConfig)[]);

type MissingRoc = Exclude<keyof RocChartConfig, (typeof ROC_CHART_CONFIG_MEMBERS)[number]>;
type MissingPr = Exclude<keyof PrChartConfig, (typeof PR_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: [MissingRoc, MissingPr] extends [never, never] ? true : never = true;
void _everyMember;

/** One curve group: a split and class, whose resample curves share a legend entry. */
export interface CurveGroup {
  readonly split: string;
  readonly class: string;
  /** The group's summary: the pooled curve's, or the mean over defined resamples; `null` when unknown. */
  readonly mean: number | null;
  /** The sample SD of the resample summaries; `null` for fewer than two. */
  readonly sd?: number | null;
  /** The chance level (PR: the mean prevalence of the group's curves); `null` without one. */
  readonly reference?: number | null;
}

/** One curve: its group's 0-based index, its resample, and its ordered vertices. */
export interface Curve {
  readonly group: number;
  readonly fold: string;
  readonly x: readonly number[];
  readonly y: readonly number[];
  /** The curve's AUC or AP; `null` when unknown. */
  readonly summary: number | null;
  /** Pairs left out of this curve for a missing value. */
  readonly omitted?: number;
}

/**
 * A curve chart's data: the groups in legend order and the curves to draw,
 * validated, ordered, and grouped by the host. rtemis.draw computes them with
 * `curve_data()`; rtemislive from rtemis's long curve table.
 */
export interface CurveData {
  readonly groups: readonly CurveGroup[];
  readonly curves: readonly Curve[];
}

/** What is not chart config: the theme palette and animation. */
export interface CurveAppearance {
  /** The theme's categorical palette, used when the config sets none. */
  readonly palette?: readonly string[];
  /** Animate the first render; unset leaves the ECharts default. */
  readonly animation?: boolean;
}

/** What tells one threshold-curve chart from another. */
interface CurveKind {
  /** Name of a class when a record has none. */
  readonly name: string;
  readonly summary: string;
  readonly x: string;
  readonly y: string;
  readonly xlab: string;
  readonly ylab: string;
  readonly step: false | "start";
}

const ROC: CurveKind = {
  name: "ROC",
  summary: "AUC",
  x: "FPR",
  y: "TPR",
  xlab: "False positive rate",
  ylab: "True positive rate",
  step: false,
};

const PR: CurveKind = {
  name: "PR",
  summary: "AP",
  x: "Recall",
  y: "Precision",
  xlab: "Recall",
  ylab: "Precision",
  // Precision at a vertex holds back to the previous recall, so the drawn area
  // is the average precision; precision is not linear in recall in between.
  step: "start",
};

/** `name` once, then `name.1`, `name.2`, ..., as R's `make.unique()` writes repeats. */
function makeUnique(names: readonly string[]): string[] {
  const seen = new Map<string, number>();
  const used = new Set(names);
  return names.map((name) => {
    if (!seen.has(name)) {
      seen.set(name, 0);
      return name;
    }
    let k = seen.get(name) as number;
    let candidate: string;
    do {
      k += 1;
      candidate = `${name}.${k}`;
    } while (used.has(candidate));
    seen.set(name, k);
    used.add(candidate);
    return candidate;
  });
}

const capitalize = (s: string) => (s ? s.charAt(0).toUpperCase() + s.slice(1) : s);

/**
 * A group's legend entry: the class for a multiclass chart, the split
 * (capitalized) when there are several splits or one class and the split is
 * named, else the class; then its summary at `digits` decimals by `ddSci()`,
 * "Test 0.85" for a pooled curve or "Test 0.77 (0.07)", the mean and SD, over
 * resamples. An SD that rounds to zero but is not zero reads in scientific
 * notation, "Training 1.00 (4e-04)". The legend title names the measure
 * ({@link curveLegendTitle}).
 */
export function curveLabel(
  group: CurveGroup,
  context: {
    readonly classes: readonly string[];
    readonly splits: readonly string[];
    readonly perResample: boolean;
    readonly digits: number;
  },
): string {
  const fmt = (v: number | null | undefined) => ddSci(v, context.digits);
  const multiclass = context.classes.length > 1;
  const parts: string[] = [];
  if (multiclass) parts.push(group.class);
  if ((context.splits.length > 1 || !multiclass) && group.split !== "Sample") {
    parts.push(capitalize(group.split));
  }
  if (parts.length === 0) parts.push(group.class);
  const summary = context.perResample ? `${fmt(group.mean)} (${fmt(group.sd)})` : fmt(group.mean);
  return `${parts.join(" · ")} ${summary}`;
}

/**
 * The legend title a host draws before a curve chart's entries, naming what
 * their values are: `legend_title` when set, else the summary ("AUC", "AP"),
 * or over resamples "AUC: mean (SD)". rtemis.draw's `curve_legend_title()`
 * writes the same text.
 */
export function curveLegendTitle(
  type: "roc" | "pr",
  config: Pick<CurveMembers, "legend_title" | "variant">,
): string {
  if (config.legend_title) return config.legend_title;
  const summary = type === "roc" ? ROC.summary : PR.summary;
  return (config.variant ?? "aggregate") === "per_resample" ? `${summary}: mean (SD)` : summary;
}

function buildCurveOption(
  kind: CurveKind,
  data: CurveData,
  config: RocChartConfig | PrChartConfig,
  appearance: CurveAppearance,
  before: Record<string, unknown>[],
  baselines: boolean,
): Record<string, unknown> {
  const { groups, curves } = data;
  if (curves.length === 0) throw new Error(`A ${kind.name} chart needs at least one curve.`);
  for (const c of curves) {
    if (!groups[c.group]) throw new Error(`Curve group ${c.group} is not among the groups.`);
    if (c.x.length !== c.y.length) {
      throw new Error(`A ${kind.name} curve needs one ${kind.y} per ${kind.x}.`);
    }
  }
  const digits = config.digits ?? 2;
  const perResample = (config.variant ?? "aggregate") === "per_resample";
  const fmt = (v: number | null) => ddSci(v, digits);
  const classes = [...new Set(groups.map((g) => g.class))];
  const splits = [...new Set(groups.map((g) => g.split))];
  const labels = makeUnique(
    groups.map((g) => curveLabel(g, { classes, splits, perResample, digits })),
  );
  const palette = asArray(config.palette);
  const colors = palette.length > 0 ? palette : (appearance.palette ?? []);
  const colorOf = (g: CurveGroup): string | undefined => {
    const id = classes.length > 1 ? classes.indexOf(g.class) : splits.indexOf(g.split);
    return colors.length > 0 ? colors[id % colors.length] : undefined;
  };
  const lineTypes = ["solid", "dashed", "dotted"] as const;
  const display = [kind.x, kind.y, kind.summary];
  const series: Record<string, unknown>[] = [...before];
  for (const c of curves) {
    const g = groups[c.group] as CurveGroup;
    const color = colorOf(g);
    series.push({
      type: "line",
      name: labels[c.group],
      data: c.x.map((x, j) => [
        x,
        c.y[j],
        c.summary,
        c.fold,
        c.omitted ?? 0,
        fmt(x),
        fmt(c.y[j] as number),
        fmt(c.summary),
      ]),
      smooth: false,
      step: kind.step,
      // Transparent points are the hover targets of the point tooltip; only
      // the hovered point shows, so the drawn chart and SVG keep the line.
      showSymbol: true,
      symbol: "circle",
      symbolSize: 8,
      connectNulls: false,
      clip: true,
      lineStyle: {
        ...(color ? { color } : {}),
        width: config.line_width ?? 2,
        opacity: perResample ? (config.fold_opacity ?? 0.45) : 1,
        type: lineTypes[splits.indexOf(g.split) % 3],
      },
      itemStyle: { ...(color ? { color } : {}), opacity: 0 },
      z: 3,
      emphasis: { itemStyle: { opacity: 1 } },
      // A display name and an ordinal type, so the tooltip reads each value's
      // `ddSci()` text without a formatter (DimensionDefinition).
      dimensions: [
        ...display,
        "Resample",
        "Missing pairs",
        ...display.map((label) => ({
          name: `${label} label`,
          displayName: label,
          type: "ordinal",
        })),
      ],
      encode: { x: 0, y: 1, tooltip: [5, 6, 7, 3] },
    });
  }
  const active = [...new Set(curves.map((c) => c.group))];
  if (baselines) {
    for (const id of active) {
      const g = groups[id] as CurveGroup;
      const level = g.reference;
      if (level === null || level === undefined || !Number.isFinite(level)) continue;
      const color = colorOf(g);
      // The baseline takes its group's name, so it toggles with the curves.
      series.push({
        type: "line",
        name: labels[id],
        data: [
          [0, level],
          [1, level],
        ],
        showSymbol: false,
        silent: true,
        legendHoverLink: false,
        lineStyle: { ...(color ? { color } : {}), type: "dotted", width: 1 },
        z: 1,
      });
    }
  }
  const xlab = config.xlab === undefined ? kind.xlab : config.xlab;
  const ylab = config.ylab === undefined ? kind.ylab : config.ylab;
  return {
    ...(config.title ? { title: { text: config.title, left: "center", textAlign: "center" } } : {}),
    grid: {
      left: 70,
      right: 24,
      top: 25 + (config.title ? 25 : 0),
      bottom: 65,
      containLabel: false,
      outerBoundsMode: "none",
    },
    xAxis: {
      type: "value",
      min: 0,
      max: 1,
      ...(xlab ? { name: xlab } : {}),
      nameLocation: "middle",
      nameGap: 30,
    },
    yAxis: {
      type: "value",
      min: 0,
      max: 1,
      ...(ylab ? { name: ylab } : {}),
      nameLocation: "middle",
      nameGap: 45,
    },
    legend: {
      show: config.legend ?? true,
      orient: "horizontal",
      padding: 0,
      itemGap: 6,
      icon: "roundRect",
      itemWidth: 20,
      itemHeight: 3,
      // Legend keys stay visible whatever the transparent hover targets.
      itemStyle: { opacity: 1 },
      data: active.map((id) => labels[id]),
      textStyle: { fontSize: 12, lineHeight: 14 },
    },
    tooltip: { trigger: "item", confine: true },
    series,
    ...(appearance.animation === undefined ? {} : { animation: appearance.animation }),
  };
}

/**
 * Build the ECharts option of a ROC chart from host-grouped curves and its
 * `chart/roc/v1` config: each curve's vertices joined straight, colored by
 * class or split, its line type by split, per-resample curves thinner and
 * translucent, and a dashed chance diagonal outside the legend.
 */
export function buildRocOption(
  data: CurveData,
  config: RocChartConfig,
  appearance: CurveAppearance = {},
): Record<string, unknown> {
  const color = config.diagonal_color ?? "#888888";
  const diagonal =
    (config.diagonal ?? true)
      ? [
          {
            type: "line",
            data: [
              [0, 0],
              [1, 1],
            ],
            showSymbol: false,
            silent: true,
            legendHoverLink: false,
            // A reference line takes no palette color from the groups.
            itemStyle: { color },
            lineStyle: { color, type: "dashed", width: 1 },
            z: 1,
          },
        ]
      : [];
  return buildCurveOption(ROC, data, config, appearance, diagonal, false);
}

/**
 * Build the ECharts option of a precision-recall chart from host-grouped
 * curves and its `chart/pr/v1` config: each curve joined by steps that hold
 * precision back to the previous recall, so the drawn area is the average
 * precision, and with `baseline`, each group's prevalence as a dotted line in
 * the group's color that toggles with its curves.
 */
export function buildPrOption(
  data: CurveData,
  config: PrChartConfig,
  appearance: CurveAppearance = {},
): Record<string, unknown> {
  return buildCurveOption(PR, data, config, appearance, [], config.baseline ?? true);
}

/** The square plotting box a host fits for a curve chart, or `null` without `square`. */
export function curveAspect(
  option: Record<string, unknown>,
  config: Pick<RocChartConfig, "square">,
): AspectHint | null {
  if (!(config.square ?? true)) return null;
  const grid = option.grid as Record<string, number>;
  return {
    ratio: 1,
    leftPx: grid.left as number,
    rightPx: grid.right as number,
    topPx: grid.top as number,
    botPx: grid.bottom as number,
  };
}
