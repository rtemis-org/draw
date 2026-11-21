import { ddSci } from "../../shared/format.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { asArray, type ColumnNames } from "../../shared/source.ts";
import { SIGN_COLORS } from "../../theme/tokens.ts";

/** The host's input to the calibration builder beyond the data and config. */
export interface CalibrationAppearance {
  /** The theme's neutral color (`neutral_color`); unset is rtemis.draw's, `SIGN_COLORS.neutral`. */
  readonly neutral?: string;
}

/**
 * An instance of the `chart/calibration/v1` schema, written by rtemis.draw's
 * `CalibrationConfig` class. Every member is optional. `observed`,
 * `probability`, `group`, `n_bins`, `bin_method`, and `na_rm` describe the
 * binning the host computes; the builder draws the bins.
 * spec: draw/shared-builders#calibration
 */
export interface CalibrationChartConfig extends LegendMembers {
  readonly type?: "calibration";
  readonly observed?: string;
  readonly probability?: string;
  readonly group?: string | null;
  readonly n_bins?: number;
  readonly bin_method?: "quantile" | "equidistant";
  readonly na_rm?: boolean;
  /** Curve display; a single bin keeps its marker. Default `"lines+markers"`. */
  readonly mode?: "lines" | "markers" | "lines+markers";
  /** Add each group's Brier score to its legend label. Default `true`. */
  readonly show_brier?: boolean;
  /** Mark each probability along the lower edge. Default `true`. */
  readonly rug?: boolean;
  /** Rug tick height in pixels. Default 8. */
  readonly rug_size?: number;
  /** Rug tick opacity. Default 0.3. */
  readonly rug_opacity?: number;
  /** Marker diameter in pixels. Default 7. */
  readonly point_size?: number;
  /** Curve width in pixels. Default 2. */
  readonly line_width?: number;
  /** Decimal places of probabilities and Brier scores. Default 3. */
  readonly digits?: number;
  /** Draw the perfect-calibration diagonal. Default `true`. */
  readonly diagonal?: boolean;
  /** Diagonal color; unset is the theme's neutral color. */
  readonly diagonal_color?: string;
  /** Group colors; unset uses the theme palette. */
  readonly palette?: ColumnNames | null;
  /** Show the legend. Default `true`. */
  readonly legend?: boolean;
  /** Keep the plotting area square. Default `true`. */
  readonly square?: boolean;
  /** Default `"Mean predicted probability"`. */
  readonly xlab?: string;
  /** Default `"Observed proportion"`. */
  readonly ylab?: string;
  readonly title?: string | null;
}

/** Every member of {@link CalibrationChartConfig}, checked against `chart/calibration/v1`. */
export const CALIBRATION_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "observed",
  "probability",
  "group",
  "n_bins",
  "bin_method",
  "na_rm",
  "mode",
  "show_brier",
  "rug",
  "rug_size",
  "rug_opacity",
  "point_size",
  "line_width",
  "digits",
  "diagonal",
  "diagonal_color",
  "palette",
  "legend",
  "square",
  "xlab",
  "ylab",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof CalibrationChartConfig)[]);

type Missing = Exclude<
  keyof CalibrationChartConfig,
  (typeof CALIBRATION_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** The bins of one group: each bin's mean probability, observed proportion, size, and bounds. */
export interface CalibrationBins {
  readonly probability: readonly number[];
  readonly observed: readonly number[];
  readonly n: readonly number[];
  readonly lower: readonly number[];
  readonly upper: readonly number[];
}

/** One sample or model: its bins, its probabilities, its Brier score, and its omitted pairs. */
export interface CalibrationGroup {
  readonly name: string;
  readonly bins: CalibrationBins;
  /** The probabilities binned, for the rug. */
  readonly probability: readonly number[];
  readonly brier: number;
  /** Incomplete observation and probability pairs left out. */
  readonly omitted: number;
}

/** A calibration chart's data: the groups the host binned, in legend order. */
export interface CalibrationData {
  readonly groups: readonly CalibrationGroup[];
}

const TOOLTIP_DIMENSIONS = [
  "Mean probability",
  "Observed proportion",
  "Bin lower",
  "Bin upper",
  "Brier",
];

/**
 * The ECharts option of a calibration chart: per group, a curve through the
 * bins (mean predicted probability against observed proportion) named with
 * its Brier score, and a rug of its probabilities along the lower edge; the
 * dashed diagonal of perfect calibration; both axes on [0, 1]. Tooltips show
 * the bin's values to `digits` places, its size, its bounds, and the omitted
 * pairs, as text data dimensions.
 * spec: draw/shared-builders#calibration
 */
export function buildCalibrationOption(
  data: CalibrationData,
  config: CalibrationChartConfig,
  appearance: CalibrationAppearance = {},
): Record<string, unknown> {
  const digits = config.digits ?? 3;
  const fmt = (x: number) => ddSci(x, digits);
  const mode = config.mode ?? "lines+markers";
  const palette = asArray(config.palette);
  const color = (i: number) => (palette.length > 0 ? palette[i % palette.length] : undefined);
  const labels = data.groups.map(
    (g) => `${g.name}${(config.show_brier ?? true) ? ` (Brier ${fmt(g.brier)})` : ""}`,
  );
  const series: Record<string, unknown>[] = [];
  if (config.diagonal ?? true) {
    const diagonal = config.diagonal_color ?? appearance.neutral ?? SIGN_COLORS.neutral;
    series.push({
      data: [
        [0, 0],
        [1, 1],
      ],
      silent: true,
      legendHoverLink: false,
      z: 1,
      showSymbol: false,
      // A set color keeps the diagonal from taking a group's palette color.
      lineStyle: { color: diagonal, width: 1, type: "dashed" },
      itemStyle: { color: diagonal },
      type: "line",
    });
  }
  data.groups.forEach((g, i) => {
    const c = color(i);
    const b = g.bins;
    const nBins = b.probability.length;
    series.push({
      name: labels[i],
      data: b.probability.map((p, j) => [
        p,
        b.observed[j],
        b.n[j],
        fmt(p),
        fmt(b.observed[j] as number),
        fmt(b.lower[j] as number),
        fmt(b.upper[j] as number),
        fmt(g.brier),
        g.omitted,
      ]),
      z: 3,
      smooth: false,
      connectNulls: false,
      clip: false,
      showSymbol: true,
      symbol: "circle",
      symbolSize: config.point_size ?? 7,
      lineStyle: {
        ...(c ? { color: c } : {}),
        width: config.line_width ?? 2,
        opacity: mode === "markers" ? 0 : 1,
      },
      itemStyle: {
        ...(c ? { color: c } : {}),
        opacity: mode === "lines" && nBins > 1 ? 0 : 1,
      },
      type: "line",
      emphasis: { itemStyle: { opacity: 1 } },
      dimensions: [
        "probability",
        "observed",
        "Count",
        ...TOOLTIP_DIMENSIONS.map((name) => ({ name, displayName: name, type: "ordinal" })),
        "Missing pairs",
      ],
      encode: { x: 0, y: 1, tooltip: [3, 4, 2, 5, 6, 7, 8] },
    });
    if (config.rug ?? true) {
      const size = config.rug_size ?? 8;
      // Pixel offsets keep the ticks inside the lower edge at every size.
      series.push({
        name: labels[i],
        data: g.probability.map((p) => [p, 0, fmt(p)]),
        z: 2,
        clip: false,
        symbol: "rect",
        symbolSize: [1, size],
        symbolOffset: [0, -size / 2],
        itemStyle: { ...(c ? { color: c } : {}), opacity: config.rug_opacity ?? 0.3 },
        type: "scatter",
        dimensions: ["probability", "baseline", { name: "Probability", type: "ordinal" }],
        encode: { x: 0, y: 1, tooltip: [2] },
      });
    }
  });
  return {
    ...(config.title ? { title: { text: config.title, left: "center" } } : {}),
    legend: {
      show: config.legend ?? true,
      orient: "horizontal",
      padding: 0,
      itemStyle: { opacity: 1 },
      data: labels,
    },
    grid: {
      left: 70,
      right: 24,
      top: config.title ? 50 : 25,
      bottom: 65,
      containLabel: false,
      outerBoundsMode: "none",
    },
    xAxis: {
      type: "value",
      name: config.xlab ?? "Mean predicted probability",
      nameGap: 30,
      min: 0,
      max: 1,
    },
    yAxis: {
      type: "value",
      name: config.ylab ?? "Observed proportion",
      nameGap: 45,
      min: 0,
      max: 1,
    },
    tooltip: { trigger: "item", confine: true },
    series,
  };
}
