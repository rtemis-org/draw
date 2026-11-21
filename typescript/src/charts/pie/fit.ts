/**
 * The pie chart's fit: edge-aligned labels need a band of text width beside
 * the pie, which ECharts truncates to nothing when the radius and the guide
 * lines take it, so the outer radius shrinks to leave the widest label its
 * band, at least 10 pixels, never beyond the built radius, and the inner
 * radius in proportion. Each pass starts from the built radii, so a narrow
 * surface does not shrink the pie for good. spec: draw/shared-builders#pie
 */
import type {
  EChartsInstance,
  EChartsModule,
  LooseOption,
  OptionRecord,
} from "../../layout/instance.ts";

export function fitPieLabels(
  echarts: EChartsModule,
  chart: EChartsInstance,
  context: { readonly option: OptionRecord },
): void {
  const definitions = (context.option as LooseOption).series;
  const source: LooseOption[] = Array.isArray(definitions) ? definitions : [definitions];
  const updates = source.map((definition, index) => {
    if (definition?.type !== "pie" || definition.label?.alignTo !== "edge") return {};
    const model = chart.getModel().getSeriesByIndex(index);
    const data = model.getData();
    const rect = data.getLayout("viewRect");
    const px = (value: unknown, extent: number) =>
      typeof value === "string" && value.endsWith("%")
        ? (Number.parseFloat(value) * extent) / 100
        : +(value as number) || 0;
    const base = Math.min(rect.width, rect.height) / 2;
    const radii = Array.isArray(definition.radius)
      ? definition.radius
      : [0, definition.radius ?? "75%"];
    const outer = px(radii[1], base);
    const inner = px(radii[0], base);
    let band = 0;
    for (let i = 0; i < data.count(); i++) {
      const label = data.getItemModel(i).getModel("label");
      if (label.get("show") === false || !["outer", "outside"].includes(label.get("position"))) {
        continue;
      }
      const line = data.getItemModel(i).getModel("labelLine");
      const width = echarts.format.getTextRect(data.getName(i), label.getFont()).width;
      band = Math.max(
        band,
        width +
          px(label.get("edgeDistance"), rect.width) +
          px(line.get("length"), rect.width) +
          px(line.get("length2"), rect.width) +
          (label.get("distanceToLabelLine") || 0) +
          4,
      );
    }
    const radius = Math.min(outer, Math.max(10, rect.width / 2 - band));
    return { radius: [outer ? (inner * radius) / outer : 0, radius] };
  });
  if (updates.some((update) => "radius" in update)) chart.setOption({ series: updates });
}
