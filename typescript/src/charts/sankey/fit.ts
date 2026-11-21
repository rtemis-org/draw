/**
 * The vertical Sankey diagram's fit: each node's label breaks to its drawn
 * node width, sources' labels above the nodes and sinks' below, and the
 * series' top and bottom margins hold the tallest label on each side. A
 * label position or width the option sets stays. Reads ECharts'
 * SankeyNodeItemOption and the drawn node geometry (chart/sankey/
 * SankeySeries.ts and SankeyView.ts). spec: draw/shared-builders#sankey
 */
import type {
  EChartsInstance,
  EChartsModule,
  LooseOption,
  OptionRecord,
} from "../../layout/instance.ts";

export function fitSankey(
  echarts: EChartsModule,
  chart: EChartsInstance,
  context: { readonly option: OptionRecord },
): void {
  const option = context.option as LooseOption;
  const sources: LooseOption[] = Array.isArray(option.series) ? option.series : [option.series];
  const updates = sources.map((source, index) => {
    if (
      source?.type !== "sankey" ||
      source.orient !== "vertical" ||
      source.label?.position != null ||
      source.label?.width != null
    ) {
      return {};
    }
    const series = chart.getModel().getSeriesByIndex(index);
    const data = series.getData();
    const fontSize = series.getModel("label").get("fontSize") || 12;
    const fontFamily = series.getModel("label").get("fontFamily") || "sans-serif";
    const targets = new Set(source.links.map((edge: LooseOption) => edge.target));
    const origins = new Set(source.links.map((edge: LooseOption) => edge.source));
    let top = 0;
    let bottom = 0;
    const nodes = (source.data as LooseOption[]).map((node, i) => {
      if (node.label?.position != null || node.label?.width != null) return node;
      const width = Math.max(1, data.getItemLayout(i).dx - 4);
      const position = origins.has(node.name) ? "top" : "bottom";
      const label = {
        ...node.label,
        position,
        width,
        overflow: "break",
        lineHeight: fontSize + 2,
      };
      const height =
        new echarts.graphic.Text({
          style: {
            text: node.name,
            fontSize,
            fontFamily,
            width,
            overflow: "break",
            lineHeight: fontSize + 2,
          },
        }).getBoundingRect().height + 12;
      if (!targets.has(node.name)) top = Math.max(top, height);
      if (!origins.has(node.name)) bottom = Math.max(bottom, height);
      return { ...node, label };
    });
    const px = (value: unknown) =>
      typeof value === "string" && value.endsWith("%")
        ? (Number.parseFloat(value) * chart.getHeight()) / 100
        : Number(value) || 0;
    return {
      data: nodes,
      top: Math.max(px(source.top), top),
      bottom: Math.max(px(source.bottom), bottom),
    };
  });
  if (updates.some((update) => "data" in update)) chart.setOption({ series: updates });
}
