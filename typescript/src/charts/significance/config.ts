import { formatNumber } from "../../shared/format.ts";
import { axisZoom, xyZoom } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { calcLimits } from "../../shared/limits.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import { SIGN_COLORS } from "../../theme/tokens.ts";

/** A method of R's `stats::p.adjust()`. */
export type PAdjustMethod =
  | "holm"
  | "hochberg"
  | "hommel"
  | "bonferroni"
  | "BH"
  | "BY"
  | "fdr"
  | "none";

/**
 * An instance of the `chart/significance/v1` schema, written by rtemis.draw's
 * `SignificanceConfig` class. Every member is optional. The members that bind
 * columns, `p_adjust_method` and `n_tests` are applied by the host, which
 * adjusts the p-values over the whole family ({@link SignificanceData}).
 * spec: draw/shared-builders#significance
 */
export interface SignificanceChartConfig extends LegendMembers {
  readonly type?: "significance";
  /** A volcano scatter or categorical Manhattan bars. Default `"volcano"`. */
  readonly view?: "volcano" | "manhattan";
  readonly estimate?: string;
  readonly p_value?: string;
  readonly label?: string | null;
  readonly group?: string | null;
  /** Display-group colors in group order; unset uses the theme. */
  readonly palette?: ColumnNames | null;
  /** The adjustment the host applied; names the value axis. Default `"holm"`. */
  readonly p_adjust_method?: PAdjustMethod;
  readonly n_tests?: number | null;
  /** Display transform of adjusted p-values. Default `"neg_log10"`. */
  readonly p_transform?: "neg_log10" | "identity" | "one_minus";
  /** Significance threshold on adjusted p-values (strict). Default 0.05. */
  readonly p_thresh?: number;
  /** Effect threshold separating low and high significant results. Default 0. */
  readonly x_thresh?: number;
  /** Display height of a zero p-value under `neg_log10`. */
  readonly zero_cap?: number | null;
  /** Labeled significant points per side of a volcano. Default 7. */
  readonly annotate_n?: number;
  /** Draw the threshold lines. Default `true`. */
  readonly reference?: boolean;
  /** Show the legend. Default `true`. */
  readonly legend?: boolean;
  /** Zoom controls. Default `false`. */
  readonly zoom?: boolean;
  readonly negative_color?: string | null;
  readonly neutral_color?: string | null;
  readonly positive_color?: string | null;
  /** Mark opacity. Default 0.8. */
  readonly alpha?: number;
  /** Symbol size in pixels. Default 8. */
  readonly point_size?: number;
  readonly xlab?: string | null;
  readonly ylab?: string | null;
  readonly xlim?: readonly number[] | null;
  readonly ylim?: readonly number[] | null;
  readonly margin_top?: number | null;
  readonly margin_right?: number | null;
  readonly margin_bottom?: number | null;
  readonly margin_left?: number | null;
  readonly title?: string | null;
}

/** Every member of {@link SignificanceChartConfig}, checked against `chart/significance/v1`. */
export const SIGNIFICANCE_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "view",
  "estimate",
  "p_value",
  "label",
  "group",
  "palette",
  "p_adjust_method",
  "n_tests",
  "p_transform",
  "p_thresh",
  "x_thresh",
  "zero_cap",
  "annotate_n",
  "reference",
  "legend",
  "zoom",
  "negative_color",
  "neutral_color",
  "positive_color",
  "alpha",
  "point_size",
  "xlab",
  "ylab",
  "xlim",
  "ylim",
  "margin_top",
  "margin_right",
  "margin_bottom",
  "margin_left",
] as const satisfies readonly (keyof SignificanceChartConfig)[]);

type Missing = Exclude<
  keyof SignificanceChartConfig,
  (typeof SIGNIFICANCE_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * One family of hypotheses, in input order, as the host adjusted it: per
 * hypothesis its label, its estimate, its raw and adjusted p-values (`null`
 * when missing), and its display group when the config binds `group`.
 * `groups` orders the display groups; unset, they take first appearance.
 */
export interface SignificanceData {
  readonly label: readonly string[];
  readonly estimate: readonly (number | null)[];
  readonly p_value: readonly (number | null)[];
  readonly p_adjusted: readonly (number | null)[];
  readonly group?: readonly (string | null)[] | null;
  readonly groups?: readonly string[] | null;
}

/** Theme colors a host passes the builder. */
export interface SignificanceAppearance {
  readonly signColors?: {
    readonly negative: string;
    readonly neutral: string;
    readonly positive: string;
  };
}

const METHOD_LABELS: Record<PAdjustMethod, string> = {
  holm: "Holm",
  hochberg: "Hochberg",
  hommel: "Hommel",
  bonferroni: "Bonferroni",
  BH: "BH",
  BY: "BY",
  fdr: "FDR",
  none: "",
};

/**
 * The axis names of a significance chart when the author sets none. x:
 * "Coefficient" for a volcano, "Outcome" for Manhattan bars; a host that
 * knows the coefficient sets `xlab` ("Coefficient of x1"). y: the transformed
 * adjusted p-value, "-log10(Holm-adjusted p)", "Holm-adjusted p", or
 * "1 - Holm-adjusted p", with "p" alone for `none`. rtemis.draw's
 * `significance_axis_names()` writes the same text.
 */
export function significanceAxisNames(
  config: Pick<SignificanceChartConfig, "view" | "p_adjust_method" | "p_transform">,
): { readonly x: string; readonly y: string } {
  const method = METHOD_LABELS[config.p_adjust_method ?? "holm"];
  const p = method ? `${method}-adjusted p` : "p";
  const transform = config.p_transform ?? "neg_log10";
  const y = transform === "neg_log10" ? `-log10(${p})` : transform === "one_minus" ? `1 - ${p}` : p;
  return { x: (config.view ?? "volcano") === "volcano" ? "Coefficient" : "Outcome", y };
}

/**
 * The legend names of the three significance groups: "Significant negative",
 * "Not significant", and "Significant positive", or "Significant low" and
 * "Significant high" when `x_thresh` is not zero.
 */
export function significanceGroupNames(x_thresh = 0): readonly [string, string, string] {
  return x_thresh === 0
    ? ["Significant negative", "Not significant", "Significant positive"]
    : ["Significant low", "Not significant", "Significant high"];
}

/** Right margin that holds the threshold label past the grid's right edge. */
function referenceLabelMargin(label: string): number {
  // ECharts places a reference-line label 5 px from the line end in 12 px
  // sans-serif; 0.6 em per character overestimates its glyphs.
  return Math.max(36, Math.ceil(label.length * 12 * 0.6) + 5 + 8);
}

const finite = (v: number | null | undefined): v is number =>
  typeof v === "number" && Number.isFinite(v);

/**
 * The ECharts option of a volcano plot or categorical Manhattan bars over a
 * host-adjusted family of hypotheses.
 *
 * A hypothesis is drawn when its estimate and raw p-value are known, and its
 * display group when the config binds `group`. Adjusted p-values take the
 * `p_transform`; under `neg_log10` a zero p-value is drawn at `zero_cap`, or
 * one unit above the rounded-up highest finite height and the threshold, as
 * a triangle, with a caption. A hypothesis is significant when its adjusted
 * p-value is below `p_thresh`, and then negative (low) when its estimate is
 * below `x_thresh` and positive (high) otherwise; the rest are "Not
 * significant". Without `group`, the groups take the theme's sign colors;
 * with it, the display groups take `palette` or the theme's palette. A
 * volcano labels the `annotate_n` significant points of each side with the
 * smallest raw p-values (ties in input order); the labels carry
 * `rtemisRepelLabels` for {@link applyRepelLabels}. Manhattan bars above 40
 * outcomes leave the outcome axis unlabeled; hover names each bar.
 * spec: draw/shared-builders#significance
 */
export function buildSignificanceOption(
  data: SignificanceData,
  config: SignificanceChartConfig,
  appearance: SignificanceAppearance = {},
): Record<string, unknown> {
  const n = data.p_value.length;
  for (const [name, values] of [
    ["label", data.label],
    ["estimate", data.estimate],
    ["p_adjusted", data.p_adjusted],
  ] as const) {
    if (values.length !== n) {
      throw new Error(`Significance data needs one ${name} per p-value.`);
    }
  }
  const grouped = Boolean(config.group) && Array.isArray(data.group);
  if (grouped && (data.group as readonly unknown[]).length !== n) {
    throw new Error("Significance data needs one group per p-value.");
  }
  const volcano = (config.view ?? "volcano") === "volcano";
  const transform = config.p_transform ?? "neg_log10";
  const pThresh = config.p_thresh ?? 0.05;
  const xThresh = config.x_thresh ?? 0;
  const annotateN = config.annotate_n ?? 7;
  const reference = config.reference ?? true;
  const legend = config.legend ?? true;
  const alpha = config.alpha ?? 0.8;
  const pointSize = config.point_size ?? 8;
  const apply = (p: number) =>
    transform === "neg_log10" ? -Math.log10(p) : transform === "one_minus" ? 1 - p : p;
  const threshold = apply(pThresh);

  const keep = data.p_value.map(
    (p, i) => finite(p) && finite(data.estimate[i]) && (!grouped || data.group?.[i] != null),
  );
  if (!keep.some(Boolean)) {
    throw new Error(
      "A significance chart needs at least one complete estimate and p-value, with a display group when grouping.",
    );
  }
  const adjusted = data.p_adjusted;
  const values = adjusted.map((p) => (finite(p) ? apply(p) : Number.NaN));
  const capped = adjusted.map((p) => p === 0 && transform === "neg_log10");
  let cap: number | null = null;
  if (capped.some((c, i) => c && keep[i])) {
    let highest = Math.max(0, threshold);
    values.forEach((v, i) => {
      if (keep[i] && Number.isFinite(v) && v > highest) highest = v;
    });
    cap = config.zero_cap ?? Math.ceil(highest) + 1;
    if (cap <= highest) {
      throw new Error(
        "Set zero_cap above all finite displayed heights and the significance threshold.",
      );
    }
    capped.forEach((c, i) => {
      if (c) values[i] = cap as number;
    });
  }

  // Significance groups: 0 negative (low), 1 not significant, 2 positive (high).
  const sig = data.p_value.map((_, i) => {
    const p = adjusted[i];
    if (!keep[i] || !finite(p) || p >= pThresh) return 1;
    return (data.estimate[i] as number) < xThresh ? 0 : 2;
  });
  const annotate = new Array<boolean>(n).fill(false);
  if (volcano) {
    for (const side of [0, 2]) {
      const at = sig.flatMap((g, i) => (keep[i] && g === side ? [i] : []));
      at.sort((a, b) => (data.p_value[a] as number) - (data.p_value[b] as number) || a - b);
      for (const i of at.slice(0, annotateN)) annotate[i] = true;
    }
  }

  const sign = appearance.signColors ?? SIGN_COLORS;
  let names: readonly string[] = significanceGroupNames(xThresh);
  let colors: readonly (string | undefined)[] = [
    config.negative_color ?? sign.negative,
    config.neutral_color ?? sign.neutral,
    config.positive_color ?? sign.positive,
  ];
  let groupOf: readonly number[] = sig;
  if (grouped) {
    const labels = data.group as readonly (string | null)[];
    const order = [...(data.groups ?? [])];
    labels.forEach((g, i) => {
      if (keep[i] && g != null && !order.includes(g)) order.push(g);
    });
    const present = order.filter((g) => labels.some((l, i) => keep[i] && l === g));
    names = present;
    groupOf = labels.map((g) => (g == null ? -1 : present.indexOf(g)));
    const palette = asArray(config.palette);
    colors = present.map((_, k) => (palette.length > 0 ? palette[k % palette.length] : undefined));
  }
  const shown = [...new Set(groupOf.filter((g, i) => keep[i] && g >= 0))].sort((a, b) => a - b);

  const dimensions = [
    volcano ? "Estimate" : "Position",
    "Display value",
    { name: "Estimate label", displayName: "Estimate", type: "ordinal" },
    { name: "Raw p label", displayName: "Raw p-value", type: "ordinal" },
    { name: "Adjusted p label", displayName: "Adjusted p-value", type: "ordinal" },
  ];
  const text = (v: number | null | undefined) => (finite(v) ? String(formatNumber(v)) : "NA");
  const series: Record<string, unknown>[] = shown.map((g) => {
    const color = colors[g];
    const marks = groupOf.flatMap((gi, i) => {
      if (!keep[i] || gi !== g) return [];
      const item: Record<string, unknown> = {
        name: data.label[i],
        value: [
          volcano ? data.estimate[i] : i,
          values[i],
          text(data.estimate[i]),
          text(data.p_value[i]),
          text(adjusted[i]),
        ],
      };
      if (volcano) {
        item.symbol = capped[i] ? "triangle" : "circle";
        if (annotate[i]) {
          item.label = { show: true, position: "top", formatter: "{b}", fontSize: 11 };
        }
      }
      return [item];
    });
    const itemStyle = { ...(color ? { color } : {}), opacity: alpha };
    const common = {
      name: names[g],
      data: marks,
      itemStyle,
      dimensions,
      encode: { x: 0, y: 1, tooltip: [2, 3, 4] },
    };
    return volcano
      ? {
          type: "scatter",
          ...common,
          symbolSize: pointSize,
          rtemisRepelLabels: true,
          labelLine: { show: true, ...(color ? { lineStyle: { color } } : {}) },
        }
      : // Disjoint groups share one category position: each outcome has one bar.
        { type: "bar", ...common, barGap: "-100%" };
  });

  const thresholdLabel = `p < ${pThresh}`;
  if (reference) {
    const refs: Record<string, unknown>[] = [
      // Past the grid's right edge, where no mark sits under it.
      { yAxis: threshold, label: { formatter: thresholdLabel, position: "end" } },
    ];
    if (volcano) refs.push({ xAxis: xThresh, label: { show: false } });
    // The thresholds belong to an unnamed series, so a legend toggle leaves them.
    series.push({
      type: "scatter",
      data: [],
      silent: true,
      markLine: {
        silent: true,
        animation: false,
        symbol: "none",
        lineStyle: { color: "#888888", type: "dashed", width: 1 },
        data: refs,
      },
    });
  }
  const cappedShown = capped.flatMap((c, i) => (c && keep[i] ? [i] : []));
  if (!volcano && cappedShown.length > 0) {
    // A triangle over each capped bar, under its group's legend name.
    for (const g of [...new Set(cappedShown.map((i) => groupOf[i] as number))].sort(
      (a, b) => a - b,
    )) {
      const color = colors[g];
      series.push({
        type: "scatter",
        name: names[g],
        data: cappedShown.filter((i) => groupOf[i] === g).map((i) => ({ value: [i, values[i]] })),
        itemStyle: { ...(color ? { color } : {}), opacity: alpha },
        symbol: "triangle",
        symbolSize: pointSize,
        silent: true,
        z: 5,
      });
    }
  }
  const note = cap !== null && cappedShown.length > 0 ? `p = 0 shown at ${cap} (triangles)` : null;

  const kept = (v: readonly (number | null)[]) => v.filter((_, i) => keep[i]);
  const xRange = volcano
    ? ((config.xlim as [number, number] | null | undefined) ??
      calcLimits([...kept(data.estimate), ...(reference ? [xThresh] : [])], 0.12))
    : null;
  const yRange =
    (config.ylim as [number, number] | null | undefined) ??
    calcLimits([0, ...kept(values), ...(reference ? [threshold] : [])], 0.12);
  if (cap !== null && cappedShown.length > 0 && yRange && (cap < yRange[0] || cap > yRange[1])) {
    throw new Error("Expand ylim to include zero_cap, or change the display transform.");
  }

  const grid: Record<string, unknown> = {
    left: config.margin_left ?? 60,
    right: config.margin_right ?? (reference ? referenceLabelMargin(thresholdLabel) : 36),
    top: config.margin_top ?? (note ? 70 : 42),
    bottom: config.margin_bottom ?? (legend ? 68 : 40),
  };
  const zoom = config.zoom ?? false;
  const xyZoomed = zoom && volcano ? xyZoom(true) : null;
  const dataZoom = zoom ? (volcano ? xyZoomed?.dataZoom : axisZoom("x", grid)) : undefined;

  const axisNames = significanceAxisNames(config);
  const xlab = config.xlab === undefined ? axisNames.x : config.xlab;
  const ylab = config.ylab === undefined ? axisNames.y : config.ylab;
  const splitLine = { showMinLine: false, showMaxLine: false };
  const palette = asArray(config.palette);
  const dense = !volcano && n > 40;
  return {
    ...(palette.length > 0 ? { color: [...palette] } : {}),
    ...(config.title || note
      ? {
          title: {
            ...(config.title ? { text: config.title } : {}),
            ...(note ? { subtext: note } : {}),
            left: "center",
          },
        }
      : {}),
    ...(legend ? { legend: { data: shown.map((g) => names[g]), bottom: 0, itemGap: 20 } } : {}),
    tooltip: { trigger: "item" },
    xAxis: volcano
      ? {
          type: "value",
          ...(xlab ? { name: xlab } : {}),
          nameLocation: "middle",
          scale: true,
          splitLine,
          ...(xRange ? { min: xRange[0], max: xRange[1] } : {}),
        }
      : {
          type: "category",
          ...(xlab ? { name: xlab } : {}),
          nameLocation: "middle",
          data: [...data.label],
          ...(dense ? { axisLabel: { show: false }, axisTick: { show: false } } : {}),
        },
    yAxis: {
      type: "value",
      ...(ylab ? { name: ylab } : {}),
      nameLocation: "middle",
      splitLine,
      ...(yRange ? { min: yRange[0], max: yRange[1] } : {}),
    },
    grid,
    ...(dataZoom ? { dataZoom } : {}),
    ...(xyZoomed ? { toolbox: xyZoomed.toolbox } : {}),
    series,
  };
}
