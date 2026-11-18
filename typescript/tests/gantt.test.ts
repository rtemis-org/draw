import { describe, expect, it } from "vitest";
import { buildGanttOption, ganttHeight } from "../src/charts/gantt/config.ts";
import { GANTT_BAR, ganttRenderers } from "../src/charts/gantt/renderers.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = {
  type: string;
  name?: string;
  renderItem: string;
  itemPayload: Record<string, unknown>;
  itemStyle?: { color: string };
  data: { value: number[]; name: string }[];
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];
type Axis = Record<string, unknown>;

const tasks: TabularData = {
  columns: {
    label: ["load", "clean", "  fold 1", "load"],
    start: [0, 12, 30, 95],
    end: [12, 30, 95, 100],
    status: ["ok", "ok", "error", "ok"],
    failed: [false, false, true, false],
    tip: ["load: 12 ms", "clean: 18 ms", "fold 1: 65 ms", "load: 5 ms"],
  },
};

describe("buildGanttOption", () => {
  it("places one bar per task on the row of its label", () => {
    const option = buildGanttOption(tasks, {});
    const [series] = seriesOf(option);
    expect(seriesOf(option)).toHaveLength(1);
    expect(series?.renderItem).toBe(GANTT_BAR);
    expect(series?.data.map((d) => d.value)).toEqual([
      [0, 0, 12],
      [1, 12, 30],
      [2, 30, 95],
      [0, 95, 100],
    ]);
    // Repeated labels share a row; leading spaces become no-break spaces.
    expect((option.yAxis as Axis).data).toEqual(["load", "clean", "\u00A0\u00A0fold 1"]);
    expect((option.yAxis as Axis).inverse).toBe(true);
    expect((option.yAxis as Axis).axisLabel).toEqual({ interval: 0 });
    expect(series?.data[2]?.name).toBe("  fold 1");
    expect((option.legend as Axis).show).toBe(false);
  });

  it("draws one series per group with the palette, flags, and tooltip text", () => {
    const option = buildGanttOption(tasks, {
      group: "status",
      border: "failed",
      tooltip: "tip",
      palette: ["#111111", "#222222"],
      bar_height: 0.5,
      bar_radius: 2,
    });
    const series = seriesOf(option);
    expect(series.map((s) => s.name)).toEqual(["ok", "error"]);
    expect(series.map((s) => s.itemStyle?.color)).toEqual(["#111111", "#222222"]);
    expect(series[1]?.data).toEqual([{ value: [2, 30, 95, 1], name: "fold 1: 65 ms" }]);
    expect(series[0]?.data.map((d) => d.value[3])).toEqual([0, 0, 0]);
    expect(series[0]?.itemPayload).toEqual({
      barHeight: 0.5,
      barRadius: 2,
      borderColor: "#E53935",
      borderWidth: 1.5,
    });
    expect(option.legend).toEqual({ show: true, data: ["ok", "error"] });
    expect((option.tooltip as Axis).formatter).toBe("{b}");
  });

  it("orders groups by their levels", () => {
    const option = buildGanttOption(
      { ...tasks, levels: { status: ["error", "ok"] } },
      { group: "status" },
    );
    expect(seriesOf(option).map((s) => s.name)).toEqual(["error", "ok"]);
  });

  it("zooms on Shift+wheel with the toolbox and guides by default", () => {
    const option = buildGanttOption(tasks, { xlab: "Elapsed (ms)" });
    const zooms = option.dataZoom as Record<string, unknown>[];
    expect(zooms.map((z) => [z.type, z.zoomOnMouseWheel, z.filterMode])).toEqual([
      ["inside", "shift", "none"],
      ["inside", "shift", "none"],
    ]);
    expect(option.toolbox).toBeDefined();
    const x = option.xAxis as Axis;
    expect(x.name).toBe("Elapsed (ms)");
    expect((x.axisPointer as Axis).show).toBe(true);
    expect((option.grid as Axis).top).toBe(48);
    const plain = buildGanttOption(tasks, { zoom: false, guides: false });
    expect(plain.dataZoom).toBeUndefined();
    expect(plain.toolbox).toBeUndefined();
    expect((plain.xAxis as Axis).axisPointer).toBeUndefined();
    expect((plain.grid as Axis).top).toBe(12);
  });

  it("reads time columns on a UTC time axis", () => {
    const option = buildGanttOption(
      {
        columns: {
          label: ["a"],
          start: ["2026-01-01T00:00:00Z"],
          end: [Date.UTC(2026, 0, 1, 0, 5)],
        },
        types: { start: "time", end: "time" },
      },
      { axis_type: "time" },
    );
    expect((option.xAxis as Axis).type).toBe("time");
    expect(option.useUTC).toBe(true);
    expect(seriesOf(option)[0]?.data[0]?.value).toEqual([
      0,
      Date.UTC(2026, 0, 1),
      Date.UTC(2026, 0, 1, 0, 5),
    ]);
  });

  it("binds renamed columns and reports missing or non-numeric ones", () => {
    const renamed = {
      columns: { name: ["a", "b"], t0: [0, 1], t1: [1, 3] },
    } satisfies TabularData;
    const option = buildGanttOption(renamed, { label: "name", start: "t0", end: "t1" });
    expect(seriesOf(option)[0]?.data.map((d) => d.value)).toEqual([
      [0, 0, 1],
      [1, 1, 3],
    ]);
    expect(() => buildGanttOption(renamed, {})).toThrow(
      "Column 'label' (config member 'label') is not in the data.",
    );
    expect(() =>
      buildGanttOption({ columns: { label: ["a"], start: [null], end: [1] } }, {}),
    ).toThrow(
      "Column 'start' (config member 'start') must hold a number or time in every row; row 1 holds 'null'.",
    );
    expect(() => buildGanttOption({ columns: { label: [], start: [], end: [] } }, {})).toThrow(
      "at least one task",
    );
  });
});

describe("ganttHeight", () => {
  it("gives each row its pixels above a floor", () => {
    expect(ganttHeight(30)).toBe(30 * 20 + 96);
    expect(ganttHeight(30, 14)).toBe(30 * 14 + 96);
    expect(ganttHeight(2)).toBe(220);
  });
});

describe("the Gantt renderer", () => {
  const api = (values: number[]) => ({
    coord: ([x, y]: number[]) => [(x as number) * 2, (y as number) * 20 + 10],
    size: () => [0, 20],
    value: (d: number) => values[d] as number,
    visual: () => "#123456",
  });
  const itemPayload = { barHeight: 0.5, barRadius: 3, borderColor: "#E53935", borderWidth: 1.5 };
  const render = ganttRenderers[GANTT_BAR] as (
    p: unknown,
    a: unknown,
  ) => {
    shape: Record<string, number>;
    style: Record<string, unknown>;
  };

  it("spans start to end on the row, outlining a flagged bar", () => {
    const plain = render({ itemPayload }, api([1, 5, 10, 0]));
    expect(plain.shape).toEqual({ x: 10, y: 25, width: 10, height: 10, r: 3 });
    expect(plain.style).toEqual({ fill: "#123456" });
    const flagged = render({ itemPayload }, api([1, 5, 5, 1]));
    expect(flagged.shape.width).toBe(1);
    expect(flagged.style).toEqual({ fill: "#123456", stroke: "#E53935", lineWidth: 1.5 });
  });
});
