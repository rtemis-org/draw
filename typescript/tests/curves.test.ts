import { describe, expect, it } from "vitest";
import {
  buildPrOption,
  buildRocOption,
  type CurveData,
  curveAspect,
  curveLabel,
  curveLegendTitle,
} from "../src/charts/curves/config.ts";

type Series = {
  type: string;
  name?: string;
  data: unknown[][];
  step?: unknown;
  lineStyle: Record<string, unknown>;
  encode?: Record<string, unknown>;
  dimensions?: unknown[];
};
const seriesOf = (option: Record<string, unknown>) => option.series as Series[];

const roc: CurveData = {
  groups: [
    { split: "training", class: "yes", mean: 0.9, sd: 0.02 },
    { split: "test", class: "yes", mean: 0.8481, sd: 0.075 },
  ],
  curves: [
    { group: 0, fold: "Fold01", x: [0, 0.5, 1], y: [0, 0.8, 1], summary: 0.9 },
    { group: 0, fold: "Fold02", x: [0, 1], y: [0, 1], summary: 0.88 },
    { group: 1, fold: "Fold01", x: [0, 0.5, 1], y: [0, 0.7, 1], summary: 0.85, omitted: 2 },
  ],
};

describe("curveLabel", () => {
  const context = {
    classes: ["yes"],
    splits: ["training", "test"],
    perResample: false,
    digits: 3,
  };
  it("names a pooled curve by split with its summary", () => {
    expect(curveLabel(roc.groups[1] as never, context)).toBe("Test 0.848");
  });

  it("gives the mean and SD over resamples, which the legend title names", () => {
    expect(curveLabel(roc.groups[1] as never, { ...context, perResample: true })).toBe(
      "Test 0.848 (0.075)",
    );
    expect(
      curveLabel(
        { split: "test", class: "yes", mean: 0.8, sd: null },
        { ...context, perResample: true },
      ),
    ).toBe("Test 0.800 (NA)");
    // An SD that rounds to zero reads in scientific notation; a zero SD is 0.
    const tiny = { split: "training", class: "yes", mean: 1, sd: 0.0004 };
    const two = { ...context, digits: 2, perResample: true };
    expect(curveLabel(tiny, two)).toBe("Training 1.00 (4e-04)");
    expect(curveLabel({ ...tiny, sd: 0 }, two)).toBe("Training 1.00 (0.00)");
    expect(curveLegendTitle("roc", {})).toBe("AUC");
    expect(curveLegendTitle("pr", { variant: "per_resample" })).toBe("AP: mean (SD)");
    expect(curveLegendTitle("roc", { legend_title: "Model" })).toBe("Model");
  });

  it("names the class of a multiclass curve, with the split when there are several", () => {
    const g = { split: "test", class: "setosa", mean: 1 };
    expect(curveLabel(g, { ...context, classes: ["setosa", "virginica"], splits: ["test"] })).toBe(
      "setosa 1.000",
    );
    expect(curveLabel(g, { ...context, classes: ["setosa", "virginica"] })).toBe(
      "setosa · Test 1.000",
    );
    expect(
      curveLabel({ split: "Sample", class: "ROC", mean: 0.5 }, { ...context, splits: ["Sample"] }),
    ).toBe("ROC 0.500");
  });
});

describe("buildRocOption", () => {
  it("draws the diagonal outside the legend and one entry per group", () => {
    const option = buildRocOption(roc, {
      variant: "per_resample",
      palette: ["#6CA3A0", "#BE2E5F"],
    });
    const [diagonal, a, b, c] = seriesOf(option);
    expect(diagonal).toMatchObject({
      silent: true,
      lineStyle: { type: "dashed", color: "#808080" },
    });
    expect(diagonal?.name).toBeUndefined();
    expect(a?.name).toBe(b?.name);
    expect(a?.lineStyle).toMatchObject({ color: "#6CA3A0", type: "solid", opacity: 0.45 });
    expect(c?.lineStyle).toMatchObject({ color: "#BE2E5F", type: "dashed" });
    expect((option.legend as { data: string[] }).data).toEqual([
      "Training 0.90 (0.02)",
      "Test 0.85 (0.07)",
    ]);
    expect(option.xAxis).toMatchObject({ name: "False positive rate", min: 0, max: 1 });
    expect(option.yAxis).toMatchObject({ name: "True positive rate" });
  });

  it("carries each point's labels for a tooltip without a formatter", () => {
    const [, , , c] = seriesOf(buildRocOption(roc, {}));
    expect(c?.data[1]).toEqual([0.5, 0.7, 0.85, "Fold01", 2, "0.50", "0.70", "0.85"]);
    expect(c?.encode).toEqual({ x: 0, y: 1, tooltip: [5, 6, 7, 3] });
    expect(c?.dimensions?.[5]).toEqual({ name: "FPR label", displayName: "FPR", type: "ordinal" });
  });

  it("omits the diagonal on request and gives a square box", () => {
    const option = buildRocOption(roc, { diagonal: false, title: "Model" });
    expect(seriesOf(option)).toHaveLength(3);
    expect(curveAspect(option, {})).toEqual({
      ratio: 1,
      leftPx: 70,
      rightPx: 24,
      topPx: 50,
      botPx: 65,
    });
    expect(curveAspect(option, { square: false })).toBeNull();
  });
});

describe("buildPrOption", () => {
  const pr: CurveData = {
    groups: [{ split: "test", class: "yes", mean: 0.84, reference: 0.47 }],
    curves: [{ group: 0, fold: "aggregate", x: [0, 0.5, 1], y: [1, 0.9, 0.47], summary: 0.84 }],
  };
  it("joins by steps and draws the prevalence in the group color", () => {
    const series = seriesOf(buildPrOption(pr, { palette: ["#BE2E5F"] }));
    expect(series[0]?.step).toBe("start");
    expect(series[1]).toMatchObject({
      name: "Test 0.84",
      data: [
        [0, 0.47],
        [1, 0.47],
      ],
      lineStyle: { color: "#BE2E5F", type: "dotted" },
    });
    expect(seriesOf(buildPrOption(pr, { baseline: false }))).toHaveLength(1);
  });

  it("stops on a curve outside the groups", () => {
    expect(() =>
      buildPrOption(
        { groups: [], curves: [{ group: 0, fold: "a", x: [0], y: [1], summary: null }] },
        {},
      ),
    ).toThrow("not among the groups");
  });
});
