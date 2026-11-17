import { ddSci } from "../../shared/format.ts";
import { SIGN_COLORS } from "../../theme/tokens.ts";

/**
 * An instance of the `chart/confusion/v1` schema, written by rtemis.draw's
 * `ConfusionConfig` class. Every member is optional. The members that bind
 * and order the records (`reference`, `predicted`, `count`, `panel`,
 * `classes`) are applied by the host, which computes {@link ConfusionData}.
 * spec: draw/shared-builders#confusion
 */
export interface ConfusionChartConfig {
  readonly type?: "confusion";
  /** Column of reference class labels. */
  readonly reference?: string;
  /** Column of predicted class labels. */
  readonly predicted?: string;
  /** Column of nonnegative integer frequencies. */
  readonly count?: string;
  /** Column of panel labels; unset draws one matrix. */
  readonly panel?: string | null;
  /** Class order shared by every panel; unset keeps first appearance. */
  readonly classes?: readonly string[] | string | null;
  /** Show per-class rates, accuracy, and balanced accuracy. Default `true`. */
  readonly show_metrics?: boolean;
  /** Decimal places of rates (`ddSci()`). Default 2. */
  readonly digits?: number;
  /** Most panels per row. Default 2. */
  readonly ncol?: number;
  /** Fill of a correct cell at a row fraction of 1; unset uses the theme's negative sign color. */
  readonly correct_color?: string | null;
  /** Fill of an error cell at a row fraction of 1; unset uses the theme's positive sign color. */
  readonly incorrect_color?: string | null;
  /** Fill at a row fraction of 0; unset uses the theme background. */
  readonly low_color?: string | null;
  /** Fill of metric cells; unset uses a faint neutral tint of the theme background. */
  readonly summary_color?: string | null;
  /** Cell-label font size in pixels. Default 12. */
  readonly font_size?: number;
  /** Predicted-class axis name. Default `"Predicted"`. */
  readonly xlab?: string | null;
  /** Reference-class axis name. Default `"Reference"`. */
  readonly ylab?: string | null;
  /** Chart title. */
  readonly title?: string | null;
}

/** Every member of {@link ConfusionChartConfig}, checked against `chart/confusion/v1`. */
export const CONFUSION_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "reference",
  "predicted",
  "count",
  "panel",
  "classes",
  "show_metrics",
  "digits",
  "ncol",
  "correct_color",
  "incorrect_color",
  "low_color",
  "summary_color",
  "font_size",
  "xlab",
  "ylab",
  "title",
] as const satisfies readonly (keyof ConfusionChartConfig)[]);

type MissingMember = Exclude<
  keyof ConfusionChartConfig,
  (typeof CONFUSION_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: MissingMember extends never ? true : never = true;
void _everyMember;

/** One panel's counts: `counts[r][c]` is the number of reference class `r` predicted as class `c`. */
export interface ConfusionPanel {
  readonly name: string;
  readonly counts: readonly (readonly number[])[];
}

/**
 * A confusion chart's data: the class order every panel shares and each
 * panel's counts, computed by the host from frequency records. rtemis.draw
 * computes them in R; rtemislive reads rtemis's `confusion_long`.
 */
export interface ConfusionData {
  readonly classes: readonly string[];
  readonly panels: readonly ConfusionPanel[];
}

/** What is not chart config: the theme's sign colors. */
export interface ConfusionAppearance {
  /** The theme's sign colors; unset uses rtemis.draw's. */
  readonly signColors?: { readonly negative: string; readonly positive: string };
}

/** The rates of one panel, `null` where a denominator is zero. */
export interface ConfusionRates {
  readonly total: number;
  /** Each count over its reference class's total; `null` for a class without support. */
  readonly fraction: readonly (readonly (number | null)[])[];
  readonly sensitivity: readonly (number | null)[];
  readonly specificity: readonly (number | null)[];
  readonly ppv: readonly (number | null)[];
  readonly npv: readonly (number | null)[];
  readonly accuracy: number | null;
  readonly balancedAccuracy: number | null;
}

const ratio = (a: number, b: number) => (b > 0 ? a / b : null);

/**
 * A panel's rates over the complete class set, as rtemis.draw's
 * `confusion_data()` computes them: sensitivity (recall), specificity, PPV,
 * and NPV per class, accuracy, and balanced accuracy (the mean recall,
 * `null` when a class has no support).
 */
export function confusionRates(counts: readonly (readonly number[])[]): ConfusionRates {
  const k = counts.length;
  const at = (r: number, c: number) => counts[r]?.[c] ?? 0;
  const actual = counts.map((row) => row.reduce((a, b) => a + b, 0));
  const predicted = counts[0]?.map((_, c) => counts.reduce((a, row) => a + (row[c] ?? 0), 0)) ?? [];
  const total = actual.reduce((a, b) => a + b, 0);
  const hits = Array.from({ length: k }, (_, i) => at(i, i));
  const tn = hits.map((h, i) => total - (actual[i] ?? 0) - (predicted[i] ?? 0) + h);
  const sensitivity = hits.map((h, i) => ratio(h, actual[i] ?? 0));
  return {
    total,
    fraction: counts.map((row, r) => row.map((v) => ratio(v, actual[r] ?? 0))),
    sensitivity,
    specificity: tn.map((t, i) => ratio(t, total - (actual[i] ?? 0))),
    ppv: hits.map((h, i) => ratio(h, predicted[i] ?? 0)),
    npv: tn.map((t, i) => ratio(t, total - (predicted[i] ?? 0))),
    accuracy: ratio(
      hits.reduce((a, b) => a + b, 0),
      total,
    ),
    balancedAccuracy: sensitivity.some((s) => s === null)
      ? null
      : (sensitivity as number[]).reduce((a, b) => a + b, 0) / k,
  };
}

/** The render hints a host passes to `fitConfusion()` and `confusionHeight()`. */
export interface ConfusionLayout {
  readonly metrics: boolean;
  readonly ncol: number;
  readonly fontSize: number;
  readonly lowColor?: string;
  readonly summaryColor?: string;
}

/** The render hints of a config. */
export function confusionLayout(config: ConfusionChartConfig): ConfusionLayout {
  return {
    metrics: config.show_metrics ?? true,
    ncol: config.ncol ?? 2,
    fontSize: config.font_size ?? 12,
    ...(config.low_color ? { lowColor: config.low_color } : {}),
    ...(config.summary_color ? { summaryColor: config.summary_color } : {}),
  };
}

/** A percentage to 15 significant digits, as R writes it. */
function percent(value: number): string {
  return `${+value.toPrecision(15)}%`;
}

/** `#rrggbb` as its three channels. */
function channels(hex: string): [number, number, number] {
  const m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex);
  if (!m) return [255, 255, 255];
  return [m[1], m[2], m[3]].map((h) => Number.parseInt(h as string, 16)) as [
    number,
    number,
    number,
  ];
}

/**
 * The `#rrggbb` color a fraction of the way from `low` to `high`, each channel
 * truncated, as R's `grDevices::rgb()` writes a `colorRamp()` value.
 */
function ramp(low: string, high: string, fraction: number): string {
  const a = channels(low);
  const b = channels(high);
  return `#${a
    .map((v, i) =>
      Math.trunc(v + ((b[i] as number) - v) * fraction)
        .toString(16)
        .padStart(2, "0"),
    )
    .join("")}`;
}

/** Black or white, whichever reads on `color`, by its linear sRGB luminance. */
function contrast(color: string): string {
  const luminance = channels(color)
    .map((v) => {
      const c = v / 255;
      return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    })
    .reduce((sum, v, i) => sum + v * ([0.2126, 0.7152, 0.0722][i] as number), 0);
  return luminance > 0.179 ? "#000000" : "#FFFFFF";
}

/**
 * Build the ECharts option of a confusion chart from host-computed counts and
 * its `chart/confusion/v1` config: per panel, a heatmap of counts filled by
 * each row's fraction (correct cells toward the correct color, errors toward
 * the incorrect color), Predicted labeling the columns above it, and with
 * `show_metrics`, strips of sensitivity and specificity beside the rows, PPV
 * and NPV under the columns, and accuracy and balanced accuracy. The option
 * places panels in percentages; `fitConfusion()` fits square cells and
 * aligned strips to the canvas and resolves the theme's colors.
 */
export function buildConfusionOption(
  data: ConfusionData,
  config: ConfusionChartConfig,
  appearance: ConfusionAppearance = {},
): Record<string, unknown> {
  const { classes, panels } = data;
  const k = classes.length;
  if (k === 0) throw new Error("A confusion chart needs at least one class.");
  if (panels.length === 0) throw new Error("A confusion chart needs at least one panel.");
  for (const p of panels) {
    if (p.counts.length !== k || p.counts.some((row) => row.length !== k)) {
      throw new Error(`Panel '${p.name}' needs a ${k} by ${k} count matrix.`);
    }
  }
  const metrics = config.show_metrics ?? true;
  const digits = config.digits ?? 2;
  const fontSize = config.font_size ?? 12;
  const sign = appearance.signColors ?? SIGN_COLORS;
  const correctColor = config.correct_color ?? sign.negative;
  const incorrectColor = config.incorrect_color ?? sign.positive;
  // A light fallback; `fitConfusion()` resolves both against the render theme.
  const low = config.low_color ?? "#FFFFFF";
  const summary = config.summary_color ?? "#EFEFEF";
  const xlab = config.xlab === undefined ? "Predicted" : config.xlab;
  const ylab = config.ylab === undefined ? "Reference" : config.ylab;
  const rate = (v: number | null) => ddSci(v, digits);
  const label = (text: string, color: string) => ({
    show: true,
    position: "inside",
    formatter: text,
    fontSize,
    color: contrast(color),
  });
  const cols = Math.min(config.ncol ?? 2, panels.length);
  const rows = Math.ceil(panels.length / cols);
  const topOffset = config.title ? 7 : 0;
  const titles: Record<string, unknown>[] = config.title
    ? [{ text: config.title, left: "center", top: 0 }]
    : [];
  const grids: Record<string, unknown>[] = [];
  const xAxes: Record<string, unknown>[] = [];
  const yAxes: Record<string, unknown>[] = [];
  const maps: Record<string, unknown>[] = [];
  const series: Record<string, unknown>[] = [];
  const named = panels.length > 1 || Boolean(config.panel);

  panels.forEach((p, i) => {
    const r = confusionRates(p.counts);
    const left = (100 * (i % cols)) / cols;
    const top = topOffset + ((100 - topOffset) * Math.floor(i / cols)) / rows;
    const pw = 100 / cols;
    const ph = (100 - topOffset) / rows;
    titles.push({
      ...(named ? { text: p.name } : {}),
      textStyle: { fontSize: fontSize + 2 },
      left: percent(left + pw / 2),
      top: percent(top),
      textAlign: "center",
    });
    const base = grids.length;
    // The count matrix, then the strips beside and under it and the summary.
    // Percentages place them before `fitConfusion()` fits them to the canvas.
    const layouts: [number, number, number, number][] = [
      [0.18, 0.2, metrics ? 0.56 : 0.76, metrics ? 0.6 : 0.74],
    ];
    const xLabels: string[][] = [[...classes]];
    const yLabels: string[][] = [[...classes]];
    if (metrics) {
      layouts.push([0.75, 0.2, 0.22, 0.6], [0.18, 0.82, 0.56, 0.15], [0.75, 0.82, 0.22, 0.15]);
      xLabels.push(["Sens.", "Spec."], [...classes], ["Summary"]);
      yLabels.push([...classes], ["PPV", "NPV"], ["Accuracy", "BA"]);
    }
    layouts.forEach((box, j) => {
      const index = base + j;
      // Every strip keeps the category geometry of the matrix it annotates.
      grids.push({
        left: percent(left + pw * box[0]),
        top: percent(top + ph * box[1]),
        width: percent(pw * box[2]),
        height: percent(ph * box[3]),
        containLabel: false,
        outerBoundsMode: "none",
      });
      xAxes.push({
        type: "category",
        gridIndex: index,
        data: xLabels[j],
        position: "top",
        boundaryGap: true,
        ...(j === 0 && xlab ? { name: xlab } : {}),
        nameLocation: "middle",
        nameGap: 30,
        axisLine: { show: false },
        splitLine: { show: false },
        axisLabel: { show: j === 0 || j === 1, interval: 0, fontSize },
      });
      yAxes.push({
        type: "category",
        gridIndex: index,
        data: yLabels[j],
        inverse: true,
        boundaryGap: true,
        ...(j === 0 && ylab ? { name: ylab } : {}),
        nameLocation: "middle",
        nameGap: 50,
        axisLine: { show: false },
        splitLine: { show: false },
        axisLabel: { show: j === 0 || j === 2, interval: 0, fontSize },
      });
    });
    for (const correct of [true, false]) {
      const hue = correct ? correctColor : incorrectColor;
      const marks: Record<string, unknown>[] = [];
      // Column by column, as R's `which(arr.ind = TRUE)` orders cells.
      for (let col = 0; col < k; col++) {
        for (let row = 0; row < k; row++) {
          if ((row === col) !== correct) continue;
          const fraction = r.fraction[row]?.[col] ?? null;
          const count = p.counts[row]?.[col] ?? 0;
          marks.push({
            value: [
              col,
              row,
              fraction ?? 0,
              count,
              classes[row],
              classes[col],
              fraction,
              rate(fraction),
            ],
            label: label(
              Number.isInteger(count) ? count.toFixed(0) : String(count),
              ramp(low, hue, fraction ?? 0),
            ),
          });
        }
      }
      maps.push({
        type: "continuous",
        show: false,
        min: 0,
        max: 1,
        inRange: { color: [low, hue] },
        dimension: 2,
        seriesIndex: series.length,
      });
      series.push({
        type: "heatmap",
        name: p.name,
        data: marks,
        xAxisIndex: base,
        yAxisIndex: base,
        itemStyle: { borderWidth: 1, borderColor: low },
        dimensions: [
          "Column",
          "Row",
          "Color fraction",
          "Count",
          "Reference",
          "Predicted",
          "Row fraction",
          // A display name and an ordinal type, so the tooltip reads the
          // fixed-decimal text without a formatter (DimensionDefinition).
          { name: "Row fraction label", displayName: "Row fraction", type: "ordinal" },
        ],
        encode: { x: 0, y: 1, tooltip: [4, 5, 3, 7] },
      });
    }
    if (metrics) {
      const right: Record<string, unknown>[] = [];
      const bottom: Record<string, unknown>[] = [];
      for (let j = 0; j < k; j++) {
        [r.sensitivity, r.specificity].forEach((values, m) => {
          right.push({ value: [m, j, 0], label: label(rate(values[j] ?? null), summary) });
        });
        [r.ppv, r.npv].forEach((values, m) => {
          bottom.push({ value: [j, m, 0], label: label(rate(values[j] ?? null), summary) });
        });
      }
      // The two overall rates stack, each with its name above its value.
      const overall = [
        { value: [0, 0, 0], label: label(`Accuracy\n${rate(r.accuracy)}`, summary) },
        { value: [0, 1, 0], label: label(`BA\n${rate(r.balancedAccuracy)}`, summary) },
      ];
      [right, bottom, overall].forEach((strip, j) => {
        maps.push({
          type: "continuous",
          show: false,
          min: 0,
          max: 1,
          inRange: { color: [summary, summary] },
          dimension: 2,
          seriesIndex: series.length,
        });
        series.push({
          type: "heatmap",
          data: strip,
          xAxisIndex: base + j + 1,
          yAxisIndex: base + j + 1,
          silent: true,
          itemStyle: { borderWidth: 1, borderColor: low },
        });
      });
    }
  });

  return {
    title: titles,
    grid: grids,
    xAxis: xAxes,
    yAxis: yAxes,
    visualMap: maps,
    series,
    legend: { show: false },
    tooltip: { trigger: "item", confine: true },
  };
}
