/**
 * The package's custom-series renderers. Each builder that draws a custom
 * series names its renderer, so the option stays JSON; hosts register the
 * renderers once with their ECharts module before drawing.
 * spec: draw/shared-builders#package
 */
import { boxplotRenderers } from "../charts/boxplot/renderers.ts";
import { histogramRenderers } from "../charts/histogram/renderers.ts";
import { scatterRenderers } from "../charts/scatter/renderers.ts";
import { survivalRenderers } from "../charts/survival/renderers.ts";

const renderers: Readonly<Record<string, unknown>> = Object.freeze({
  ...boxplotRenderers,
  ...histogramRenderers,
  ...scatterRenderers,
  ...survivalRenderers,
});

/** Names of the custom-series renderers this package registers. */
export const RENDERER_NAMES: readonly string[] = Object.freeze(Object.keys(renderers));

/**
 * Register the package's custom-series renderers with an ECharts module, once
 * before drawing a chart that uses them.
 */
export function registerRenderers(echarts: {
  registerCustomSeries(name: string, renderItem: never): void;
}): void {
  for (const [name, render] of Object.entries(renderers)) {
    echarts.registerCustomSeries(name, render as never);
  }
}
