import { colorRamp, divergingRamp } from "../../shared/color.ts";
import { COLORMAPS, type ColormapName } from "../../shared/colormaps.ts";
import { ddSci } from "../../shared/format.ts";
import type { MarginMembers } from "../../shared/layout.ts";
import type { LegendMembers } from "../../shared/legend.ts";
import { niceAxis } from "../../shared/limits.ts";
import { darkThemeTokens, lightThemeTokens, SIGN_COLORS } from "../../theme/tokens.ts";

/**
 * An instance of the `chart/spectrogram/v1` schema, written by rtemis.draw's
 * `SpectrogramConfig` class. Every member is optional. `sample_rate`,
 * `n_fft`, `window`, `overlap`, `power`, `db`, and `db_range` describe the
 * transform the host computes; the builder reads `power` and `db` only to
 * name and round the values.
 * spec: draw/shared-builders#spectrogram
 */
export interface SpectrogramChartConfig extends MarginMembers, LegendMembers {
  readonly type?: "spectrogram";
  readonly sample_rate?: number | null;
  readonly n_fft?: number;
  readonly window?: string;
  readonly overlap?: number | null;
  /** The values are a power spectrum, not magnitudes. Default `true`. */
  readonly power?: boolean;
  /** The values are in decibels. Default `true`. */
  readonly db?: boolean;
  readonly db_range?: number;
  /** Frequency axis scale. Default `"linear"`. */
  readonly freq_scale?: "linear" | "log" | "mel";
  /** Frequency axis limits in Hz; bins outside are dropped. */
  readonly freq_range?: readonly number[] | null;
  /** Time axis limits in seconds; frames outside are dropped. */
  readonly time_range?: readonly number[] | null;
  /** Frequency axis unit. Default `"Hz"`. */
  readonly freq_unit?: string;
  /** Time axis unit. Default `"s"`. */
  readonly time_unit?: string;
  /**
   * A color scheme (magma, inferno, plasma, viridis, cividis, rocket, mako,
   * turbo, or diverging), or two or more colors defining one. Default `"magma"`.
   */
  readonly colormap?: string | readonly string[];
  readonly colormap_reverse?: boolean;
  /** Number of colors of the scale. Default 256. */
  readonly n_colors?: number;
  /** Color scale limits; unset is the range of the values. */
  readonly zlim?: readonly number[] | null;
  readonly show_colorbar?: boolean;
  /** Color bar title; unset is "dB", "Power", or "Amplitude". */
  readonly colorbar_title?: string | null;
  readonly xlab?: string | null;
  readonly ylab?: string | null;
  readonly title?: string | null;
}

/** Every member of {@link SpectrogramChartConfig}, checked against `chart/spectrogram/v1`. */
export const SPECTROGRAM_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "sample_rate",
  "n_fft",
  "window",
  "overlap",
  "power",
  "db",
  "db_range",
  "freq_scale",
  "freq_range",
  "time_range",
  "freq_unit",
  "time_unit",
  "colormap",
  "colormap_reverse",
  "n_colors",
  "zlim",
  "show_colorbar",
  "colorbar_title",
  "xlab",
  "ylab",
  "margin_top",
  "margin_right",
  "margin_bottom",
  "margin_left",
  "legend_position",
  "legend_placement",
  "legend_title",
] as const satisfies readonly (keyof SpectrogramChartConfig)[]);

type Missing = Exclude<
  keyof SpectrogramChartConfig,
  (typeof SPECTROGRAM_CHART_CONFIG_MEMBERS)[number]
>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/**
 * A spectrogram's values: `values[i][j]` at `frequency[i]` (Hz) and `time[j]`
 * (seconds), `null` where missing, in the scale the chart draws.
 */
export interface SpectrogramData {
  readonly time: readonly number[];
  readonly frequency: readonly number[];
  readonly values: readonly (readonly (number | null)[])[];
}

/** Theme values the diverging color scheme needs. */
export interface SpectrogramAppearance {
  /** The chart is drawn on a dark background; the diverging scale's midpoint is it. */
  readonly dark?: boolean;
  /** Background of the diverging scale's midpoint; unset is the theme's. */
  readonly background?: string;
  /** Sign colors of the diverging scale; unset is {@link SIGN_COLORS}. */
  readonly signColors?: { readonly negative: string; readonly positive: string };
}

/**
 * `n` colors of a color scheme, low to high, reversed when `reverse` is set:
 * a named scheme's 256 colors, interpolated in RGB to another `n`; a diverging
 * scale between the sign colors with the background at zero; or a ramp
 * through two or more given colors.
 */
export function spectrogramColors(
  colormap: string | readonly string[],
  n: number,
  reverse: boolean,
  zlim: readonly [number, number],
  appearance: SpectrogramAppearance = {},
): string[] {
  let colors: string[];
  const name = typeof colormap === "string" ? colormap : colormap.length === 1 ? colormap[0] : null;
  if (name !== null && name !== undefined) {
    if (name === "diverging") {
      const sym: [number, number] =
        zlim[0] < 0 && zlim[1] > 0
          ? [
              -Math.max(Math.abs(zlim[0]), Math.abs(zlim[1])),
              Math.max(Math.abs(zlim[0]), Math.abs(zlim[1])),
            ]
          : zlim[0] === zlim[1]
            ? [-1, 1]
            : [zlim[0], zlim[1]];
      const sign = appearance.signColors ?? SIGN_COLORS;
      const bg =
        appearance.background ??
        (appearance.dark ? darkThemeTokens() : lightThemeTokens()).bg_color;
      colors = divergingRamp(sign.negative, bg, sign.positive, sym, n);
    } else {
      const table = COLORMAPS[name as ColormapName];
      if (!table) {
        throw new Error(
          `Colormap '${name}' is not recognized; use one of ${Object.keys(COLORMAPS).join(", ")}, diverging, or two or more colors.`,
        );
      }
      colors = n === table.length ? [...table] : colorRamp(table, n);
    }
  } else {
    colors = colorRamp(colormap as readonly string[], n);
  }
  return reverse ? colors.reverse() : colors;
}

/** Values at `xout` interpolated linearly from finite (x, y) pairs, constant beyond the ends. */
function approx(
  x: readonly number[],
  y: readonly (number | null)[],
  xout: readonly number[],
): (number | null)[] {
  const pts = x.flatMap((xi, i) => {
    const yi = y[i];
    return typeof yi === "number" && Number.isFinite(yi) ? [[xi, yi] as const] : [];
  });
  if (pts.length === 0) return xout.map(() => null);
  return xout.map((v) => {
    const first = pts[0] as readonly [number, number];
    const last = pts[pts.length - 1] as readonly [number, number];
    if (v <= first[0]) return first[1];
    if (v >= last[0]) return last[1];
    let k = 0;
    while (k < pts.length - 2 && v > (pts[k + 1] as readonly [number, number])[0]) k++;
    const [x0, y0] = pts[k] as readonly [number, number];
    const [x1, y1] = pts[k + 1] as readonly [number, number];
    return y0 + ((y1 - y0) * (v - x0)) / (x1 - x0);
  });
}

const range2 = (r: readonly number[] | null | undefined): [number, number] | null =>
  r && r.length >= 2 ? [r[0] as number, r[1] as number] : null;

/**
 * The ECharts option of a spectrogram: one heatmap cell per frequency bin and
 * time frame on category axes, low frequencies at the bottom. A log frequency
 * scale drops bins at or below 0 Hz and resamples the values onto as many
 * log-spaced bins; `freq_range` and `time_range` keep the bins and frames
 * within them; the axes read in `freq_unit` and `time_unit`. The continuous
 * color scale spans `zlim`, or the range of the finite values.
 * spec: draw/shared-builders#spectrogram
 */
export function buildSpectrogramOption(
  data: SpectrogramData,
  config: SpectrogramChartConfig,
  appearance: SpectrogramAppearance = {},
): Record<string, unknown> {
  let freq = [...data.frequency];
  let time = [...data.time];
  let rows = data.values.map((row) => row.map((v) => (typeof v === "number" ? v : null)));
  if (rows.length !== freq.length || rows.some((row) => row.length !== time.length)) {
    throw new Error(
      "A spectrogram needs one row of values per frequency and one value per time in each row.",
    );
  }
  const log = config.freq_scale === "log";
  const keepRows = (keep: boolean[]) => {
    freq = freq.filter((_, i) => keep[i]);
    rows = rows.filter((_, i) => keep[i]);
  };
  if (log) {
    const keep = freq.map((f) => f > 0);
    if (!keep.some(Boolean)) throw new Error("A log frequency scale needs a frequency above 0 Hz.");
    keepRows(keep);
  }
  const fr = range2(config.freq_range);
  if (fr) {
    const keep = freq.map((f) => f >= fr[0] && f <= fr[1]);
    if (!keep.some(Boolean)) {
      throw new Error(`No frequency bins fall within freq_range [${fr[0]}, ${fr[1]}] Hz.`);
    }
    keepRows(keep);
  }
  const tr = range2(config.time_range);
  if (tr) {
    const keep = time.map((t) => t >= tr[0] && t <= tr[1]);
    if (!keep.some(Boolean)) {
      throw new Error(`No time frames fall within time_range [${tr[0]}, ${tr[1]}] s.`);
    }
    time = time.filter((_, j) => keep[j]);
    rows = rows.map((row) => row.filter((_, j) => keep[j]));
  }
  if (log) {
    // Rows at log-spaced frequencies, so each row spans the same ratio.
    const n = freq.length;
    const lo = Math.log(freq[0] as number);
    const hi = Math.log(freq[n - 1] as number);
    const logFreq = Array.from({ length: n }, (_, i) =>
      Math.exp(n === 1 ? lo : lo + ((hi - lo) * i) / (n - 1)),
    );
    const cols = time.map((_, j) =>
      approx(
        freq,
        rows.map((row) => row[j] ?? null),
        logFreq,
      ),
    );
    rows = logFreq.map((_, i) => cols.map((col) => col[i] ?? null));
    freq = logFreq;
  }
  const timeUnit = config.time_unit ?? "s";
  const freqUnit = config.freq_unit ?? "Hz";
  const timeShown = time.map((t) => (timeUnit === "ms" ? t * 1000 : t));
  const freqShown = freq.map((f) => (freqUnit === "kHz" ? f / 1000 : f));
  let zlim = range2(config.zlim);
  if (!zlim) {
    let lo = Number.POSITIVE_INFINITY;
    let hi = Number.NEGATIVE_INFINITY;
    for (const row of rows) {
      for (const v of row) {
        if (v !== null && Number.isFinite(v)) {
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
    }
    if (!Number.isFinite(lo)) {
      throw new Error("A spectrogram needs a finite value, or zlim, for its color scale.");
    }
    zlim = [lo, hi];
  }
  const db = config.db ?? true;
  const power = config.power ?? true;
  const colors = spectrogramColors(
    config.colormap ?? "magma",
    config.n_colors ?? 256,
    config.colormap_reverse ?? false,
    zlim,
    appearance,
  );
  const timeDigits = timeUnit === "ms" ? 1 : 3;
  const freqDigits = freqUnit === "kHz" ? 3 : 1;
  // dB values take one decimal; other values one more than the decimals of a
  // nice interval of the color scale (numbers: draw/shared-builders#numbers).
  const valueDigits = db ? 1 : niceAxis(zlim[0], zlim[1]).decimals + 1;
  const xlab = config.xlab ?? `Time (${timeUnit})`;
  const ylab = config.ylab ?? `Frequency (${freqUnit})`;
  const valueName = config.colorbar_title ?? (db ? "dB" : power ? "Power" : "Amplitude");
  const timeLabels = timeShown.map((t) => t.toFixed(timeDigits));
  const freqLabels = freqShown.map((f) => f.toFixed(freqDigits));
  // [time index, frequency index, value], time by time.
  const cells = time.flatMap((_, j) =>
    freq.map((_, i) => {
      const v = rows[i]?.[j];
      return [j, i, v !== null && v !== undefined && Number.isFinite(v) ? v : null];
    }),
  );
  const set = (side: "top" | "right" | "bottom" | "left") => config[`margin_${side}`];
  const anyMargin = (["top", "right", "bottom", "left"] as const).some(
    (side) => typeof set(side) === "number",
  );
  const grid = anyMargin
    ? Object.fromEntries(
        (["top", "right", "bottom", "left"] as const).flatMap((side) => {
          const v = set(side);
          return typeof v === "number" ? [[side, v]] : [];
        }),
      )
    : { top: config.title ? 50 : 36, right: 90, bottom: 36, left: 36 };
  const axis = (name: string, labels: string[]) => ({
    type: "category",
    data: labels,
    name,
    nameLocation: "middle",
    boundaryGap: true,
    splitArea: { show: false },
    axisLine: { show: false },
  });
  return {
    ...(config.title ? { title: { text: config.title } } : {}),
    tooltip: {
      trigger: "item",
      formatter: (p: { value?: unknown[] }) => {
        const [t, f, v] = p.value ?? [];
        if (typeof v !== "number") return "N/A";
        return `${xlab}: ${timeLabels[t as number]}<br/>${ylab}: ${freqLabels[f as number]}<br/>${valueName}: ${ddSci(v, valueDigits)}`;
      },
    },
    grid,
    xAxis: axis(xlab, timeLabels),
    yAxis: axis(ylab, freqLabels),
    visualMap: {
      type: "continuous",
      min: zlim[0],
      max: zlim[1],
      precision: valueDigits,
      calculable: true,
      show: config.show_colorbar ?? true,
      orient: "vertical",
      // The labels sit right of the bar (`align` names the bar's side), away
      // from the cells.
      align: "left",
      right: "right",
      top: "middle",
      inRange: { color: colors },
    },
    series: [{ type: "heatmap", data: cells }],
  };
}
