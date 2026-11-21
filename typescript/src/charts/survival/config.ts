import { ddSci, formatNumber } from "../../shared/format.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import {
  darkThemeTokens,
  lightThemeTokens,
  SIGN_COLORS,
  type ThemeTokens,
  themeFontSizes,
} from "../../theme/tokens.ts";
import { type RiskPayload, SURVIVAL_RISK } from "./renderers.ts";

/**
 * An instance of the `chart/survival/v1` schema, written by rtemis.draw's
 * `SurvivalConfig` class. Every member is optional. The members that bind
 * columns are applied by the host, which computes {@link SurvivalData}.
 * spec: draw/shared-builders#survival
 */
export interface SurvivalChartConfig extends LegendMembers {
  readonly type?: "survival";
  readonly time?: string;
  readonly survival?: string;
  readonly group?: string | null;
  readonly lower?: string | null;
  readonly upper?: string | null;
  readonly n_censor?: string | null;
  readonly n_risk?: string | null;
  /** Draw the pointwise confidence bands. Default `true`. */
  readonly show_ci?: boolean;
  /** Draw a tick at each censoring time. Default `true`. */
  readonly show_censors?: boolean;
  /** Draw each curve's median survival dropline, labeled. Default `true`. */
  readonly show_median?: boolean;
  /** Times at which each curve's survival is marked and labeled as a percent. */
  readonly landmarks?: readonly number[] | null;
  /** Draw the number-at-risk strip from the host's risk records. Default `false`. */
  readonly risk_table?: boolean;
  /** Confidence band opacity. Default 0.15. */
  readonly ci_opacity?: number;
  /** Curve stroke width in pixels. Default 2. */
  readonly line_width?: number;
  /** Censor tick height in pixels. Default 8. */
  readonly censor_size?: number;
  /** Decimals of tooltip probabilities (`ddSci()`). Default 3. */
  readonly digits?: number;
  /** Curve colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Show the legend of several curves. Default `true`. */
  readonly legend?: boolean;
  /** Horizontal axis name. Default `"Time"`. */
  readonly xlab?: string;
  /** Vertical axis name. Default `"Survival probability"`. */
  readonly ylab?: string;
  readonly title?: string | null;
}

/** Every member of {@link SurvivalChartConfig}, checked against `chart/survival/v1`. */
export const SURVIVAL_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "legend_position",
  "legend_placement",
  "legend_title",
  "time",
  "survival",
  "group",
  "lower",
  "upper",
  "n_censor",
  "n_risk",
  "show_ci",
  "show_censors",
  "show_median",
  "landmarks",
  "risk_table",
  "ci_opacity",
  "line_width",
  "censor_size",
  "digits",
  "palette",
  "legend",
  "xlab",
  "ylab",
] as const satisfies readonly (keyof SurvivalChartConfig)[]);

type Missing = Exclude<keyof SurvivalChartConfig, (typeof SURVIVAL_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * One survival curve as the host recorded it, sorted by time with unique
 * times and nonincreasing survival, its first record the origin: the curve
 * starts at its first record and the builder adds none. `lower`, `upper`,
 * `n_censor`, and `n_risk` align with `time`; `null` marks an unknown value.
 */
export interface SurvivalCurve {
  /** The curve's legend name; `null` for the only curve of an ungrouped chart. */
  readonly name: string | null;
  readonly time: readonly number[];
  readonly survival: readonly number[];
  readonly lower?: readonly (number | null)[] | null;
  readonly upper?: readonly (number | null)[] | null;
  /** Censored observations at each time; a tick marks a positive count. */
  readonly n_censor?: readonly (number | null)[] | null;
  /** Number at risk immediately before each time, for the tooltip. */
  readonly n_risk?: readonly (number | null)[] | null;
}

/** The number at risk of each curve at shared times, for `risk_table`. */
export interface SurvivalRisk {
  readonly time: readonly number[];
  /** Per curve, in curve order, the count at each time; `null` when unknown. */
  readonly counts: readonly (readonly (number | null)[])[];
}

/** A survival chart's data: the curves in legend order and the risk records. */
export interface SurvivalData {
  readonly curves: readonly SurvivalCurve[];
  readonly risk?: SurvivalRisk | null;
}

/** Theme values the builder needs. */
export interface SurvivalAppearance {
  /** The theme's categorical palette; unset leaves each curve's color to the chart theme. */
  readonly palette?: readonly string[];
  /** Color of the risk strip's caption; unset is the theme's text color. */
  readonly textColor?: string;
  /** The chart is drawn on a dark background. */
  readonly dark?: boolean;
  /** The theme's neutral color (`neutral_color`); unset is rtemis.draw's, `SIGN_COLORS.neutral`. */
  readonly neutral?: string;
}

/**
 * Median survival as R's survival package reports it: the first time the
 * curve reaches 0.5 or less, or, where it holds exactly 0.5, the midpoint of
 * that plateau and the next time it falls below, the last time when the
 * plateau ends the curve; comparisons to 0.5 use a tolerance of
 * √(machine ε). `null` when the curve stays above 0.5.
 */
export function survivalMedian(curve: Pick<SurvivalCurve, "time" | "survival">): number | null {
  const tol = Math.sqrt(Number.EPSILON);
  const s = curve.survival;
  const t = curve.time;
  const i = s.findIndex((v) => v <= 0.5 + tol);
  if (i < 0) return null;
  if (Math.abs((s[i] as number) - 0.5) > tol) return t[i] as number;
  let end = t[t.length - 1] as number;
  for (let k = i + 1; k < s.length; k++) {
    if ((s[k] as number) < 0.5 - tol) {
      end = t[k] as number;
      break;
    }
  }
  return ((t[i] as number) + end) / 2;
}

// The strip's space: a gap under the time axis name, rows 1.6 times the
// axis label font, and a pad at the canvas bottom.
const RISK_GAP = 12;
/** The builder's gap between the plot's left edge and the y axis name. */
const Y_NAME_GAP = 45;
const RISK_PAD = 10;

/**
 * The ECharts option of a Kaplan–Meier chart from host-recorded curves:
 * right-continuous steps from each curve's first record to its last, with,
 * as the config says, the pointwise confidence band, a tick at each time with
 * censored observations, each curve's median dropline (from S = 0.5 across to
 * the median and down to the axis, labeled "Median" and its time), each
 * landmark time's survival as a percent, and the number-at-risk strip under
 * the time axis, which then labels exactly the risk times. Hovering a curve
 * shows the point's survival, bounds, number at risk, and censored count.
 * One unnamed curve draws no legend.
 * spec: draw/shared-builders#survival
 */
export function buildSurvivalOption(
  data: SurvivalData,
  config: SurvivalChartConfig,
  appearance: SurvivalAppearance = {},
): Record<string, unknown> {
  const curves = data.curves.filter((c) => c.time.length > 0);
  if (curves.length === 0) throw new Error("A survival chart needs at least one recorded curve.");
  for (const c of curves) {
    if (c.survival.length !== c.time.length) {
      throw new Error("A survival curve needs one survival value per time.");
    }
  }
  const digits = config.digits ?? 3;
  const fmt = (v: number | null | undefined) => ddSci(v, digits);
  // Counts as recorded: whole numbers, or weighted sums such as 2.5.
  const count = (v: number | null | undefined) =>
    typeof v === "number" && Number.isFinite(v) ? String(v) : "NA";
  // Unset colors leave each curve to the theme palette, which ECharts assigns
  // by series name, so every layer of a curve takes the curve's color.
  const palette = asArray(config.palette);
  const colors = palette.length > 0 ? palette : (appearance.palette ?? []);
  const colorOf = (i: number) =>
    colors.length > 0 ? (colors[i % colors.length] as string) : undefined;
  const styled = (color: string | undefined) => (color ? { color } : {});
  const names = curves.map((c, i) => c.name ?? (curves.length > 1 ? `Curve ${i + 1}` : "Survival"));
  let lo = Number.POSITIVE_INFINITY;
  let hi = Number.NEGATIVE_INFINITY;
  for (const c of curves) {
    lo = Math.min(lo, c.time[0] as number);
    hi = Math.max(hi, c.time[c.time.length - 1] as number);
  }
  // A one-time record still needs a visible time range.
  const domain: [number, number] = hi > lo ? [lo, hi] : [lo - 0.5, hi + 0.5];
  const dimensions = [
    "time",
    "survival",
    ...["Survival", "Lower", "Upper", "At risk", "Censored"].map((name) => ({
      name,
      type: "ordinal",
    })),
  ];
  const encode = { x: 0, y: 1, tooltip: [0, 2, 3, 4, 5, 6] };
  const series: Record<string, unknown>[] = [];
  const showMedian = config.show_median ?? true;
  const landmarks = [...new Set(asArray(config.landmarks))].sort((a, b) => a - b);

  curves.forEach((c, i) => {
    const name = names[i] as string;
    const color = colorOf(i);
    const values = c.time.map((t, j) => [
      t,
      c.survival[j],
      fmt(c.survival[j]),
      fmt(c.lower?.[j]),
      fmt(c.upper?.[j]),
      count(c.n_risk?.[j]),
      count(c.n_censor?.[j]),
    ]);
    series.push({
      type: "line",
      name,
      data: values,
      step: "end",
      smooth: false,
      showSymbol: true,
      symbol: "circle",
      symbolSize: 6,
      clip: false,
      lineStyle: { ...styled(color), width: config.line_width ?? 2 },
      // Transparent symbols are the hover targets of each step.
      itemStyle: { ...styled(color), opacity: c.time.length === 1 ? 1 : 0 },
      emphasis: { itemStyle: { opacity: 1 } },
      dimensions,
      encode,
      z: 3,
    });
    if ((config.show_ci ?? true) && c.lower && c.upper) {
      const lower = c.lower;
      const upper = c.upper;
      const known = c.time.map((_, j) => lower[j] != null && upper[j] != null);
      for (const band of ["lower", "width"] as const) {
        const at = (j: number) =>
          band === "lower" ? (lower[j] as number) : (upper[j] as number) - (lower[j] as number);
        const points: (number | null)[][] = [];
        c.time.forEach((t, j) => {
          // The preceding known interval runs up to a missing step; a null row
          // alone would erase that horizontal segment.
          if (j > 0 && known[j - 1] && !known[j]) points.push([t, at(j - 1)]);
          points.push([t, known[j] ? at(j) : null]);
        });
        series.push({
          type: "line",
          symbol: "circle",
          name,
          step: "end",
          stack: `ci-${i}`,
          stackStrategy: "all",
          data: points,
          showSymbol: false,
          connectNulls: false,
          silent: true,
          legendHoverLink: false,
          itemStyle: styled(color),
          lineStyle: { opacity: 0 },
          areaStyle: {
            ...styled(color),
            opacity: band === "lower" ? 0 : (config.ci_opacity ?? 0.15),
          },
          z: 1,
        });
      }
    }
    if ((config.show_censors ?? true) && c.n_censor) {
      const censored = values.filter((_, j) => ((c.n_censor?.[j] as number) ?? 0) > 0);
      if (censored.length > 0) {
        series.push({
          type: "scatter",
          name,
          data: censored,
          symbol: "rect",
          symbolSize: [1.5, config.censor_size ?? 8],
          clip: false,
          itemStyle: styled(color),
          dimensions,
          encode,
          z: 4,
        });
      }
    }
    const median = showMedian ? survivalMedian(c) : null;
    if (median !== null) {
      series.push({
        type: "line",
        symbol: "circle",
        name,
        data: [
          [domain[0], 0.5],
          [median, 0.5],
          [median, 0],
        ],
        showSymbol: false,
        silent: true,
        legendHoverLink: false,
        itemStyle: styled(color),
        lineStyle: { ...styled(color), type: "dashed", opacity: 0.7 },
        z: 2,
      });
      series.push({
        type: "scatter",
        name,
        data: [{ value: [median, 0], name: `Median ${formatNumber(median)}` }],
        symbolSize: 0,
        silent: true,
        legendHoverLink: false,
        clip: false,
        itemStyle: styled(color),
        // Upright beside the vertical dropline, rising from the time axis, so
        // the labels of different medians stay apart.
        label: {
          show: true,
          position: "top",
          distance: 4,
          rotate: 90,
          align: "left",
          verticalAlign: "bottom",
          formatter: "{b}",
          color: "inherit",
          fontSize: 11,
        },
        z: 5,
      });
    }
    const shown = landmarks.filter(
      (t) => t >= (c.time[0] as number) && t <= (c.time[c.time.length - 1] as number),
    );
    if (shown.length > 0) {
      series.push({
        type: "scatter",
        name,
        data: shown.map((t) => {
          let s = c.survival[0] as number;
          c.time.forEach((u, j) => {
            if (u <= t) s = c.survival[j] as number;
          });
          return { value: [t, s], name: `${Math.round(s * 100)}%` };
        }),
        symbolSize: 6,
        clip: false,
        itemStyle: styled(color),
        label: { show: true, position: "top", formatter: "{b}", color: "inherit" },
        z: 5,
      });
    }
  });
  const references = landmarks.filter((t) => t >= domain[0] && t <= domain[1]);
  if (references.length > 0) {
    // Landmark times as one unnamed reference series, outside the legend.
    series.push({
      type: "line",
      data: [],
      silent: true,
      markLine: {
        silent: true,
        animation: false,
        symbol: "none",
        lineStyle: {
          color: appearance.neutral ?? SIGN_COLORS.neutral,
          type: "dashed",
          width: 1,
          opacity: 0.7,
        },
        label: { show: false },
        data: references.map((t) => ({ xAxis: t })),
      },
    });
  }

  const risk = config.risk_table ? data.risk : null;
  if (config.risk_table && !risk) {
    throw new Error("Supply risk records to draw the number-at-risk table.");
  }
  const grid = {
    left: 70,
    right: 30,
    top: config.title ? 50 : 25,
    bottom: 60,
    containLabel: false,
  };
  if (risk) {
    if (
      risk.counts.length !== curves.length ||
      risk.counts.some((r) => r.length !== risk.time.length)
    ) {
      throw new Error("Supply one risk count per curve and risk time.");
    }
    // `layoutSurvivalRisk()` sets the offset, row height, and font size from
    // the time axis below.
    const base = {
      offset: 0,
      lineHeight: 0,
      rows: curves.length,
      fontSize: 0,
      caption: "At risk",
      // `layoutSurvivalRisk()` sets it from the y axis name below.
      captionOffset: 0,
      textColor:
        appearance.textColor ?? (appearance.dark ? darkThemeTokens() : lightThemeTokens()).fg_color,
    };
    // One row per curve, under the curve's name, so its legend entry hides
    // the row with the curve and the row takes the curve's color.
    curves.forEach((_, i) => {
      const payload: RiskPayload = {
        ...base,
        labels: risk.time.map((_, k) => count(risk.counts[i]?.[k])),
      };
      series.push({
        type: "custom",
        name: names[i],
        renderItem: SURVIVAL_RISK,
        clip: false,
        silent: true,
        legendHoverLink: false,
        tooltip: { show: false },
        itemStyle: styled(colorOf(i)),
        itemPayload: payload,
        data: risk.time.map((t) => [t, i]),
        z: 5,
      });
    });
    series.push({
      type: "custom",
      renderItem: SURVIVAL_RISK,
      clip: false,
      silent: true,
      tooltip: { show: false },
      itemPayload: { ...base, labels: [] } satisfies RiskPayload,
      data: [[domain[0], -1]],
      z: 5,
    });
  }

  const legendShown = (config.legend ?? true) && !(curves.length === 1 && curves[0]?.name == null);
  const riskTimes = risk ? [...risk.time] : null;
  return layoutSurvivalRisk({
    ...(config.title ? { title: { text: config.title, left: "center", textAlign: "center" } } : {}),
    grid,
    xAxis: {
      type: "value",
      min: domain[0],
      max: domain[1],
      name: config.xlab ?? "Time",
      nameGap: 30,
      // With the strip, the axis labels exactly the risk times.
      ...(riskTimes
        ? { axisLabel: { customValues: riskTimes }, axisTick: { customValues: riskTimes } }
        : {}),
    },
    yAxis: {
      type: "value",
      min: 0,
      max: 1,
      name: config.ylab ?? "Survival probability",
      nameGap: Y_NAME_GAP,
    },
    legend: {
      show: legendShown,
      data: names,
      padding: 0,
      itemStyle: { opacity: 1 },
    },
    tooltip: { trigger: "item", confine: true },
    series,
  });
}

/** The first side of a text margin, one number or `[top, right, bottom, left]`. */
function firstMargin(margin: unknown, fallback: number): number {
  if (Array.isArray(margin)) return Number(margin[0]) || 0;
  return typeof margin === "number" ? margin : fallback;
}

/**
 * Places the number-at-risk strip of a built survival option under its time
 * axis name, at the axis label font size: each row is 1.6 times that size,
 * the strip starts 12 pixels below the name (`nameGap`, the name's font
 * size, and any `textMargin` above and below it),
 * and the grid's bottom margin holds the axis band and the rows. The caption
 * "At risk" is centered where a centered y axis name is: `nameGap` plus half
 * the name's font size left of the plot, the gap at least the builder's 45
 * pixels, since ECharts moves a name with a smaller gap clear of the tick
 * labels, and with a name text margin at least the labels' estimated extent
 * plus that margin; with the name at an end of the axis, the caption keeps
 * the builder's place. A host that restyles either axis after the build
 * calls it again; a host that registers a theme passes its
 * `tokens`, whose text sizes and name margin stand where the option sets
 * none. Mutates and returns `option`; an option without the strip is returned
 * as it is.
 */
export function layoutSurvivalRisk<T extends Record<string, unknown>>(
  option: T,
  tokens?: ThemeTokens,
): T {
  const series = Array.isArray(option.series) ? (option.series as Record<string, unknown>[]) : [];
  const strips = series.filter((s) => s?.renderItem === SURVIVAL_RISK);
  if (strips.length === 0) return option;
  const x = (Array.isArray(option.xAxis) ? option.xAxis[0] : option.xAxis) as
    | Record<string, unknown>
    | undefined;
  // Text sizes and the name margin the option leaves to a registered theme
  // come from its tokens; without tokens, ECharts' 12 pixels and no margin.
  const sizes = tokens ? themeFontSizes(tokens) : null;
  const size = (style: unknown, fallback = 12) => {
    const v = (style as { fontSize?: unknown } | undefined)?.fontSize;
    return typeof v === "number" ? v : fallback;
  };
  const labelSize = sizes?.axisLabel ?? 12;
  const nameSize = sizes?.base ?? 12;
  const themeMargin =
    tokens?.axis_name_gap === undefined || tokens.axis_name_gap === null
      ? 0
      : tokens.axis_name_gap * nameSize;
  const nameGap = typeof x?.nameGap === "number" ? x.nameGap : 30;
  const fontSize = size(x?.axisLabel, labelSize);
  // A host's text margin around the axis name, [top, right, bottom, left] or
  // one number, widens the name's band.
  const margin = (x?.nameTextStyle as { textMargin?: unknown } | undefined)?.textMargin;
  const [above, below] = Array.isArray(margin)
    ? [Number(margin[0]) || 0, Number(margin[2] ?? margin[0]) || 0]
    : typeof margin === "number"
      ? [margin, margin]
      : [themeMargin, themeMargin];
  const offset =
    nameGap + above + Math.ceil(size(x?.nameTextStyle, nameSize) * 1.3) + below + RISK_GAP;
  const lineHeight = Math.round(fontSize * 1.6);
  const y = (Array.isArray(option.yAxis) ? option.yAxis[0] : option.yAxis) as
    | Record<string, unknown>
    | undefined;
  // The name's place along the axis: the option's, else the theme's, which
  // centers it in rtemis.draw's themes.
  const location = y?.nameLocation ?? tokens?.axis_name_location ?? "middle";
  const centered = location === "middle" || location === "center";
  const yGap = centered && typeof y?.nameGap === "number" ? y.nameGap : Y_NAME_GAP;
  // A centered name with a text margin keeps it from the tick labels, which
  // ECharts moves the name clear of. The labels' extent is estimated as their
  // margin plus the widest survival label, "1.00", at 0.6 em per character.
  const yMargin = firstMargin(
    (y?.nameTextStyle as { textMargin?: unknown } | undefined)?.textMargin,
    themeMargin,
  );
  const yLabel = y?.axisLabel as { margin?: unknown } | undefined;
  const labelsExtent =
    (typeof yLabel?.margin === "number" ? yLabel.margin : 8) +
    4 * 0.6 * size(y?.axisLabel, labelSize);
  const nameEdge = Math.max(yGap, Y_NAME_GAP, centered && yMargin > 0 ? labelsExtent + yMargin : 0);
  // ECharts puts a centered name's edge nearest the axis at that distance, so
  // the name's center lies half its font size further out.
  const captionOffset = nameEdge + size(y?.nameTextStyle, nameSize) / 2;
  let rows = 0;
  for (const s of strips) {
    const payload = s.itemPayload as RiskPayload;
    rows = payload.rows;
    s.itemPayload = { ...payload, offset, lineHeight, fontSize, captionOffset };
  }
  // Each risk time is a labeled column, the first and last included, whatever
  // a host's restyling hides at the axis ends.
  if (x) x.axisLabel = { ...(x.axisLabel as object), showMinLabel: true, showMaxLabel: true };
  const grid = (Array.isArray(option.grid) ? option.grid[0] : option.grid) as
    | Record<string, unknown>
    | undefined;
  if (grid) grid.bottom = offset + rows * lineHeight + RISK_PAD;
  return option;
}
