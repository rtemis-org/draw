/**
 * The end labels of value axes: an axis extent that is not a multiple of the
 * tick interval (a zoomed window, or limits at padded data values) would be
 * labeled 4.553 or 8.521968997863846, so an end carries a label only when it
 * falls on a tick, unless the option sets `showMinLabel` or `showMaxLabel`
 * itself. The tick interval depends on the axis length in pixels, so this
 * runs after every layout change and zoom. spec: draw/zoom-controls#endpoint-labels
 */
import type { EChartsInstance, LooseOption, OptionRecord } from "./instance.ts";

export function fitEndpointLabels(
  chart: EChartsInstance,
  context: { readonly option?: OptionRecord | null },
): void {
  const source = context?.option as LooseOption | null | undefined;
  // A chart stub without models, as some hosts' tests pass, has nothing to fit.
  if (typeof chart?.getModel !== "function" || !source) return;
  const update: LooseOption = {};
  for (const type of ["xAxis", "yAxis"]) {
    const defined: LooseOption[] = ([] as LooseOption[]).concat(source[type] ?? []);
    const axes: LooseOption[] = [];
    let changed = false;
    chart.getModel().eachComponent(type, (model: EChartsInstance) => {
      const label = defined[model.componentIndex]?.axisLabel || {};
      const scale = model.axis?.scale;
      const next: Record<string, boolean> = {};
      // ECharts 6.1 exposes the tick interval through getConfig().
      const interval = scale?.getConfig?.()?.interval ?? scale?.getInterval?.();
      if (model.axis?.type === "value" && Number.isFinite(interval)) {
        const extent = scale.getExtent();
        const onTick = (v: number) =>
          interval > 0 && Math.abs(v / interval - Math.round(v / interval)) < 1e-6;
        if (label.showMinLabel === undefined) next.showMinLabel = onTick(extent[0]);
        if (label.showMaxLabel === undefined) next.showMaxLabel = onTick(extent[1]);
        const current = model.get("axisLabel");
        if (Object.keys(next).some((k) => current?.[k] !== next[k])) changed = true;
      }
      axes[model.componentIndex] = Object.keys(next).length ? { axisLabel: next } : {};
    });
    if (changed) update[type] = axes;
  }
  if (Object.keys(update).length) chart.setOption(update);
}
