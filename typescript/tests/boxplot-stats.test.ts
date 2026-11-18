import { describe, expect, it } from "vitest";
import { boxplotStats } from "../src/charts/boxplot/stats.ts";

// Expected statistics are rtemis.draw's boxplot_summary() for the same values.
const cases: [number[], number[], number[], number[], number[]][] = [
  // values, linear, outliers (1-based), hinges, whisker = 0
  [
    [3, 1, 4, 1, 5, 9, 2, 6],
    [1, 1.75, 3.5, 5.25, 9],
    [],
    [1, 1.5, 3.5, 5.5, 9],
    [1, 1.75, 3.5, 5.25, 9],
  ],
  [[0, 0, 0, 0, 10], [0, 0, 0, 0, 0], [5], [0, 0, 0, 0, 0], [0, 0, 0, 0, 10]],
  [[2.5], [2.5, 2.5, 2.5, 2.5, 2.5], [], [2.5, 2.5, 2.5, 2.5, 2.5], [2.5, 2.5, 2.5, 2.5, 2.5]],
  [[1, 2], [1, 1.25, 1.5, 1.75, 2], [], [1, 1, 1.5, 2, 2], [1, 1.25, 1.5, 1.75, 2]],
  [[-6, -3, 2, 4, 8, 30], [-6, -1.75, 3, 7, 8], [6], [-6, -3, 3, 8, 8], [-6, -1.75, 3, 7, 30]],
];

describe("boxplotStats", () => {
  it("matches rtemis.draw's statistics for both quartile conventions", () => {
    for (const [values, linear, outliers, hinges, full] of cases) {
      const out = boxplotStats(values);
      expect(out?.stats).toEqual(linear);
      expect(out?.outlier.flatMap((o, i) => (o ? [i + 1] : []))).toEqual(outliers);
      expect(boxplotStats(values, { quartiles: "hinges" })?.stats).toEqual(hinges);
      expect(boxplotStats(values, { whisker: 0 })?.stats).toEqual(full);
    }
  });

  it("skips missing values and returns null without one", () => {
    expect(boxplotStats([Number.NaN, 1, 2])?.stats).toEqual([1, 1.25, 1.5, 1.75, 2]);
    expect(boxplotStats([Number.NaN])).toBeNull();
  });
});
