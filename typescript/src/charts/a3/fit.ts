/**
 * The A3 diagram's fit. The legend is one vertical rail in the upper right,
 * so every family's symbols and text share a left edge, laid out with an
 * unlimited height so ECharts never flows entries into a second column; with
 * `autoGrid`, the grid's right margin reserves the rail's measured width and
 * the legend gap, and with `autoHeight` on an unbounded surface the chart
 * takes the height of the residue rows or the rail, whichever is taller.
 * Every glyph dimension (symbol sizes and offsets, line and border widths,
 * label sizes and distances) then scales from its built value to the drawn
 * residue spacing, never beyond it. Reads ECharts' GridOption (coord/cartesian/
 * GridModel.ts), LegendOption (component/legend/LegendModel.ts), and the
 * series symbol and label styles (util/types.ts).
 * spec: draw/first-cran-release#a3-responsive-surface-layout
 */
import type { EChartsInstance, LooseOption, OptionRecord } from "../../layout/instance.ts";
import type { A3SurfaceHints } from "./config.ts";

/** The A3 surface hints with the host's flags for the grid and the height. */
export interface A3FitHints extends Partial<A3SurfaceHints> {
  readonly markerSize: number;
  readonly fontSize: number;
  readonly residueSpacing: number;
  /** The grid's right margin follows the measured legend rail. */
  readonly autoGrid?: boolean;
  /** An unbounded surface takes the diagram's height. */
  readonly autoHeight?: boolean;
}

/** What the A3 fit reads from its host. */
export interface A3FitContext {
  /** The option the host set. */
  readonly option: OptionRecord;
  readonly hints: A3FitHints;
  /** The surface's height is fixed; otherwise `autoHeight` sets it. */
  readonly bounded?: boolean;
}

export function fitA3(chart: EChartsInstance, context: A3FitContext): void {
  const hint = context.hints;
  const bounded = context.bounded ?? true;
  const source = context.option as LooseOption;
  const width = chart.getWidth();
  const xRange = source.xAxis.max - source.xAxis.min;
  const yRange = source.yAxis.max - source.yAxis.min;
  const marker = hint.markerSize;
  const step = hint.residueSpacing;
  const glyph = Math.max(1, marker, hint.fontSize);
  if (
    ![marker, hint.fontSize].every((v) => Number.isFinite(v) && v >= 0) ||
    ![step, xRange, yRange].every((v) => Number.isFinite(v) && v > 0)
  ) {
    throw new Error(
      "Supply nonnegative A3 glyph sizes, positive residue spacing, and increasing axis ranges.",
    );
  }
  const grid: LooseOption = { ...source.grid };
  const selected: Record<string, boolean> = {};
  chart
    .getModel()
    .eachComponent("legend", (model: { get(key: string): Record<string, boolean> }) =>
      Object.assign(selected, model.get("selected")),
    );
  chart.setOption({
    legend: {
      ...source.legend,
      selected,
      orient: "vertical",
      align: "left",
      left: null,
      bottom: null,
      width: null,
      height: 1000000,
    },
  });
  const model = chart.getModel().getComponent("legend");
  const box = chart.getViewOfComponentModel(model).group.getBoundingRect();
  // Each pass starts from the built option, so the gap holds and resizes do
  // not accumulate changes to the sequence or the selection.
  if (hint.autoGrid) grid.right = source.legend.right + box.width + (hint.legendGap ?? 40);
  if (hint.autoGrid && hint.autoHeight && !bounded) {
    grid.height = hint.bodyHeight ?? glyph * 2 * yRange;
    const height = Math.max(
      grid.top + grid.height + grid.bottom,
      source.legend.top + box.height + grid.bottom,
    );
    chart.resize({ width, height: Math.ceil(height) });
  }
  chart.setOption({ grid });
  // The drawn grid also respects margins the caller set.
  const area = chart.getModel().getComponent("grid").coordinateSystem.getRect();
  if (area.width <= 0 || area.height <= 0) {
    throw new Error("Increase A3 figure dimensions to leave room for residues and the legend.");
  }
  const dx = (area.width / xRange) * step;
  const dy = area.height / yRange;
  const turn = Math.hypot((Math.sqrt(3) / 2) * dx, dy / 2);
  const scale = Math.min(1, Math.min(dx, dy, turn) / (glyph + 4));
  // Every glyph dimension scales from its built value; positions, data, and
  // colors stay.
  const scaled = (object: LooseOption | undefined, keys: readonly string[]) => {
    if (!object) return undefined;
    const update: LooseOption = {};
    for (const key of keys) {
      if (typeof object[key] === "number") update[key] = object[key] * scale;
      else if (Array.isArray(object[key])) {
        update[key] = (object[key] as number[]).map((v) => v * scale);
      }
    }
    return update;
  };
  chart.setOption({
    series: (source.series as LooseOption[]).map((series) => ({
      ...scaled(series, ["symbolSize", "symbolOffset"]),
      lineStyle: { ...scaled(series.lineStyle, ["width"]) },
      itemStyle: { ...scaled(series.itemStyle, ["borderWidth"]) },
      data: (series.data as LooseOption[]).map((datum) =>
        datum?.label
          ? {
              ...datum,
              label: { ...datum.label, ...scaled(datum.label, ["fontSize", "distance"]) },
            }
          : datum,
      ),
    })),
  });
}
