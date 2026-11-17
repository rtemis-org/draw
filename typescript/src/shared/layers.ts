/**
 * Layers a host adds over a built option, such as rtemis.draw's supplied fits
 * (`draw_add_fit()`) and annotations (`draw_annotate()`). A layer travels with
 * the build request and is applied after every build, so it survives a
 * rebuild on a change of color scheme. spec: draw/shared-builders#layers
 */

/** Series appended to a built option, and the legend entry they add. */
export interface Layer {
  /** ECharts series, appended after the builder's. */
  readonly series: readonly Record<string, unknown>[];
  /** Legend entry the series add; unset adds none. */
  readonly legend?: string | null;
}

/**
 * The option with each layer's series appended in order. A layer's legend
 * entry joins the legend's listed names, or shows the legend when the option
 * has none.
 */
export function appendLayers(
  option: Record<string, unknown>,
  layers: readonly Layer[] | null | undefined,
): Record<string, unknown> {
  if (!layers || layers.length === 0) return option;
  const series = option.series === undefined ? [] : [option.series].flat();
  const out: Record<string, unknown> = {
    ...option,
    series: [...series, ...layers.flatMap((layer) => layer.series)],
  };
  const names = layers.flatMap((layer) => (layer.legend ? [layer.legend] : []));
  if (names.length > 0) {
    const legend = (option.legend ?? {}) as Record<string, unknown>;
    out.legend = Array.isArray(legend.data)
      ? { ...legend, data: [...new Set([...(legend.data as unknown[]), ...names])] }
      : { ...legend, show: true };
  }
  return out;
}
