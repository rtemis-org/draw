/**
 * The Gantt chart's fit: row labels as wide as the longest, up to 35% of the
 * chart; the builder's automatic legend moved below the plot when the
 * timeline would be narrower than 240 pixels; the toolbar on its own header
 * row when it would overlap the title; and the time axis divided at most every
 * 80 pixels. Reads ECharts' GridOption, LegendOption, and AxisLabelBaseOption
 * (coord/cartesian/GridModel.ts, component/legend/LegendModel.ts,
 * coord/axisCommonTypes.ts). Data, zoom, and the legend selection stay as
 * they are. spec: draw/first-cran-release#responsive-timelines
 */
import type {
  EChartsInstance,
  EChartsModule,
  LooseOption,
  OptionRecord,
} from "../../layout/instance.ts";
import { GANTT_BAR } from "./renderers.ts";

/** What the Gantt fit reads from its host. */
export interface GanttFitContext {
  /** The option the host set. */
  readonly option: OptionRecord;
  /** The legend position a config member set; unset, the builder's automatic legend. */
  readonly legendPosition?: string | null;
}

export function fitGantt(
  echarts: EChartsModule,
  chart: EChartsInstance,
  context: GanttFitContext,
): void {
  const source = context.option as LooseOption;
  const series = Array.isArray(source?.series) ? source.series : [source?.series];
  if (
    !series.some((s: { renderItem?: unknown } | undefined) => s?.renderItem === GANTT_BAR) ||
    !source.grid ||
    Array.isArray(source.grid) ||
    !source.xAxis ||
    !source.yAxis ||
    Array.isArray(source.xAxis) ||
    Array.isArray(source.yAxis)
  ) {
    return;
  }
  const width = chart.getWidth();
  const model = chart.getModel();
  const y = model.getComponent("yAxis");
  const font = y.getModel("axisLabel").getFont();
  const labelWidth = Math.min(
    width * 0.35,
    (y.get("data") as unknown[]).reduce(
      (max: number, label) =>
        Math.max(max, echarts.format.getTextRect(String(label), font).width + 2),
      0,
    ),
  );
  const update: LooseOption = {
    grid: { ...source.grid },
    yAxis: { axisLabel: { width: labelWidth, overflow: "truncate", ...source.yAxis.axisLabel } },
  };
  // Only the builder's automatic legend adapts; a placement the caller set,
  // a hidden legend included, stays.
  const automatic =
    !context.legendPosition &&
    source.legend?.orient === "vertical" &&
    source.legend.right === 8 &&
    source.legend.top === "middle" &&
    source.legend.show !== false;
  let below = false;
  if (automatic) {
    const legend = model.getComponent("legend");
    const legendFont = legend.getModel("textStyle").getFont();
    const natural = Math.max(
      0,
      ...legend
        .getData()
        .map(
          (item: { get(key: string): string }) =>
            echarts.format.getTextRect(item.get("name"), legendFont).width,
        ),
    );
    const right = Math.max(source.grid.right, natural + legend.get("itemWidth") + 29);
    below = width - labelWidth - right - 32 < 240;
    const available = Math.max(24, width - legend.get("itemWidth") - 31);
    update.legend = {
      ...source.legend,
      selected: legend.get("selected"),
      left: below ? 8 : null,
      right: 8,
      top: below ? null : "middle",
      bottom: below ? 4 : null,
      orient: below ? "horizontal" : "vertical",
      textStyle: {
        ...source.legend.textStyle,
        width: below && natural > available ? available : null,
        overflow: "break",
      },
    };
    update.grid.right = below ? 16 : right;
  }
  // On a narrow surface the toolbar takes a header row of its own, so the
  // title and the controls both show; a chart without a title reserves the
  // row too.
  if (source.toolbox?.show !== false && Number.isFinite(source.toolbox?.top)) {
    const title = model.getComponent("title");
    const toolbox = model.getComponent("toolbox");
    const bounds = (component: unknown) => {
      if (!component) return null;
      const group = chart.getViewOfComponentModel(component).group;
      const rect = group.getBoundingRect().clone();
      rect.applyTransform(group.getComputedTransform());
      return rect as { x: number; y: number; width: number; height: number };
    };
    const titleBox = bounds(title);
    const toolsBox = bounds(toolbox);
    // A centered title can meet a right-anchored toolbar even when their
    // widths together fit; their horizontal extents decide.
    const overlap =
      titleBox &&
      toolsBox &&
      titleBox.x < toolsBox.x + toolsBox.width + 8 &&
      toolsBox.x < titleBox.x + titleBox.width + 8;
    const top =
      overlap && titleBox
        ? Math.max(source.toolbox.top, titleBox.y + titleBox.height + 8)
        : source.toolbox.top;
    update.toolbox = { top };
    update.grid.top = Math.max(source.grid.top, top + (toolsBox?.height || 14) + 12);
  }
  chart.setOption(update);
  if (below) {
    const legend = chart.getModel().getComponent("legend");
    const height = chart.getViewOfComponentModel(legend).group.getBoundingRect().height;
    chart.setOption({ grid: { bottom: source.grid.bottom + height + 12 } });
  }
  const grid = chart.getModel().getComponent("grid").coordinateSystem.getRect();
  chart.setOption({
    xAxis: {
      splitNumber:
        source.xAxis.splitNumber ?? Math.max(2, Math.min(5, Math.floor(grid.width / 80))),
      axisLabel: { hideOverlap: true, ...source.xAxis.axisLabel },
    },
  });
}
