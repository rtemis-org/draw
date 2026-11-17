/**
 * Fitted curves drawn over the points of a scatter chart and the lines of a
 * line chart. The host fits each model and hands the builder the curves; the
 * builder draws them in the color and under the legend entry of the series
 * each belongs to. spec: draw/shared-builders#fits
 */

/**
 * The fit members of a config. `fit`, `fit_params`, `fit_per_group`, `se`,
 * `se_times`, and `n_fit` tell the host what to fit; the builder reads
 * `fit_per_group`, `rsq`, `fit_alpha`, and `fit_name` to draw the curves.
 */
export interface FitMembers {
  /**
   * The model fitted to each series: `"glm"`, `"polynomial"`, `"loess"`,
   * `"gam"`, or, in rtemis.draw, an rtemis learner name. Unset draws no fit.
   */
  readonly fit?: string | null;
  /**
   * Arguments of the model: `degree` for `polynomial`, `span` for `loess`,
   * `k` and `engine` (`"mgcv"`, `"pygam"`, `"statsmodels"`) for `gam`, or the
   * learner's arguments.
   */
  readonly fit_params?: Readonly<Record<string, unknown>> | null;
  /** One fit per series; `false` fits one curve over every group. Default `true`. */
  readonly fit_per_group?: boolean;
  /** Shade the interval around each fit. Default `true`. */
  readonly se?: boolean;
  /** Multiplier of the standard error that bounds the interval. Default `1.96`. */
  readonly se_times?: number;
  /** Add each fit's R² to its legend entry. Default `false`. */
  readonly rsq?: boolean;
  /** Points each curve is evaluated at. Default `200`. */
  readonly n_fit?: number;
  /** Opacity of the interval, `[0, 1]`. Default `0.25`. */
  readonly fit_alpha?: number;
  /** Text added to the legend entry of each fitted series. */
  readonly fit_name?: string | null;
}

/** Every member of {@link FitMembers}, as the config member lists include them. */
export const FIT_MEMBERS = Object.freeze([
  "fit",
  "fit_params",
  "fit_per_group",
  "se",
  "se_times",
  "rsq",
  "n_fit",
  "fit_alpha",
  "fit_name",
] as const satisfies readonly (keyof FitMembers)[]);

/** A curve the host fitted to one series, or to every group when pooled. */
export interface FitCurve {
  /** The `y` column fitted; unset with one `y` column. */
  readonly y?: string | null;
  /** The group level fitted; unset without a group and for a pooled fit. */
  readonly group?: string | null;
  /** Evaluation points, increasing. */
  readonly x: readonly number[];
  /** Fitted values at `x`. */
  readonly fitted: readonly number[];
  /** Lower interval bound at `x`; unset draws no interval. */
  readonly lower?: readonly number[] | null;
  /** Upper interval bound at `x`. */
  readonly upper?: readonly number[] | null;
  /** The fit's R² on the data it was fitted to; `null` where undefined. */
  readonly rsq?: number | null;
}

/** A series a curve may belong to: its `y` column, group level, legend name, and color. */
export interface FitTarget {
  readonly y: string;
  readonly group: string | null;
  readonly name: string | null;
  readonly color: string;
}

/** Color of a pooled fit, which belongs to no one group. */
export const POOLED_FIT_COLOR = "#808080";

/**
 * The legend entry of a fitted series: its name, joined to `fit_name`, with
 * R² appended when `rsq` is set. A series without a name and a pooled fit are
 * named "Fit" when `rsq` asks for a label.
 */
export function fitLabel(
  base: string | null,
  config: FitMembers,
  rsq: number | null | undefined,
): string | null {
  let name = base;
  if (config.fit_name) name = [base, config.fit_name].filter(Boolean).join(" - ");
  if (config.rsq) {
    const value = typeof rsq === "number" && Number.isFinite(rsq) ? rsq.toFixed(3) : "NA";
    name = `${name ?? "Fit"} (R² = ${value})`;
  }
  return name;
}

/** Whether fits change the legend names of the series they belong to. */
export function fitRenames(config: FitMembers): boolean {
  return Boolean(config.fit) && Boolean(config.fit_name || config.rsq);
}

/**
 * Check each curve's shape: equal lengths, and both interval bounds or
 * neither, ordered at every point.
 */
function checkCurve(curve: FitCurve, at: number): void {
  const n = curve.x.length;
  const where = `fits[${at}]`;
  if (curve.fitted.length !== n) {
    throw new Error(`${where}: \`x\` and \`fitted\` must have the same length.`);
  }
  const lower = curve.lower ?? null;
  const upper = curve.upper ?? null;
  if ((lower === null) !== (upper === null)) {
    throw new Error(`${where}: give both interval bounds, \`lower\` and \`upper\`, or neither.`);
  }
  if (lower && upper && (lower.length !== n || upper.length !== n)) {
    throw new Error(`${where}: \`lower\` and \`upper\` must have the length of \`x\`.`);
  }
}

/**
 * The series of the fitted curves: per curve, its interval as a filled
 * polygon (upper bound left to right, lower bound right to left) beneath, and
 * its line. A curve takes the color and the legend name of the series it
 * belongs to, so one legend entry shows and hides the points, the line, and
 * the interval together; a pooled curve takes `fitLabel(null)` and a neutral
 * color. `lineType` distinguishes the fit from the lines of a line chart.
 */
export function fitSeries(
  curves: readonly FitCurve[],
  targets: readonly FitTarget[],
  config: FitMembers,
  lineType: "solid" | "dashed" = "solid",
): { series: Record<string, unknown>[]; names: Map<FitTarget, string | null> } {
  const pooled = config.fit_per_group === false;
  const names = new Map<FitTarget, string | null>();
  const series: Record<string, unknown>[] = [];
  const multiY = new Set(targets.map((t) => t.y)).size > 1;
  curves.forEach((curve, at) => {
    checkCurve(curve, at);
    if (curve.x.length === 0) return;
    const target = targets.find(
      (t) =>
        (!multiY || t.y === curve.y) &&
        (pooled || (curve.group ?? null) === t.group || t.group === null),
    );
    if (!target) {
      throw new Error(
        `fits[${at}] (y = ${String(curve.y ?? null)}, group = ${String(curve.group ?? null)}) matches no series of the chart.`,
      );
    }
    const owned = !pooled || targets.every((t) => t.group === null);
    const name = owned
      ? fitLabel(target.name, config, curve.rsq)
      : pooledLabel(multiY ? target.y : null, config, curve.rsq);
    if (owned && fitRenames(config)) names.set(target, name);
    const color = owned ? target.color : POOLED_FIT_COLOR;
    const named = name === null ? {} : { name };
    if (config.se !== false && curve.lower && curve.upper) {
      const upper = curve.x.map((x, i) => [x, curve.upper?.[i] ?? null]);
      const lower = curve.x.map((x, i) => [x, curve.lower?.[i] ?? null]).reverse();
      series.push({
        type: "line",
        ...named,
        data: [...upper, ...lower],
        showSymbol: false,
        lineStyle: { opacity: 0 },
        areaStyle: { color, opacity: config.fit_alpha ?? 0.25 },
        silent: true,
        z: 0,
        legendHoverLink: false,
      });
    }
    series.push({
      type: "line",
      ...named,
      data: curve.x.map((x, i) => [x, curve.fitted[i] ?? null]),
      showSymbol: false,
      lineStyle: { color, ...(lineType === "dashed" ? { type: "dashed" } : {}) },
      itemStyle: { color },
      silent: true,
    });
  });
  return { series, names };
}

/**
 * The legend entry of a pooled fit, which belongs to no one group: "Fit", or
 * "Fit (<y>)" for the `y` column of a chart with several, unless `fit_name`
 * or `rsq` labels it.
 */
function pooledLabel(y: string | null, config: FitMembers, rsq: number | null | undefined): string {
  const label = fitLabel(y, config, rsq);
  if (label !== y && label !== null) return label;
  return y === null ? "Fit" : `Fit (${y})`;
}
