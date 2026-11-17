import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  type BandwidthRule,
  type DensityKernel,
  type DensitySummary,
  kernelDensity,
  ruleBandwidth,
} from "../src/charts/density/kde.ts";

// Written by rtemis.draw's data-raw/density_estimates.R from stats::density().
interface Fixture {
  samples: Record<string, number[]>;
  cases: {
    sample: string;
    summary: DensitySummary;
    kernel: DensityKernel;
    bw: BandwidthRule;
    adjust: number;
    bandwidth: number;
    x: number[];
    y: number[];
  }[];
}
const fixture = JSON.parse(
  readFileSync(new URL("./fixtures/density-estimates.json", import.meta.url), "utf8"),
) as Fixture;

/** Counts in 512 equal bins between the minimum and maximum, as a host bins. */
function binned(values: readonly number[], summary: DensitySummary): Float64Array {
  const counts = new Float64Array(512);
  for (const v of values) {
    const b = Math.min(511, Math.floor(((v - summary.min) / (summary.max - summary.min)) * 512));
    counts[b] = (counts[b] ?? 0) + 1;
  }
  return counts;
}

describe("kernelDensity", () => {
  for (const c of fixture.cases) {
    it(`matches stats::density() on ${c.sample}, ${c.kernel}, ${c.bw}, adjust ${c.adjust}`, () => {
      expect(ruleBandwidth(c.summary, c.bw) * c.adjust).toBeCloseTo(c.bandwidth, 10);
      const curve = kernelDensity(c.summary, binned(fixture.samples[c.sample] ?? [], c.summary), {
        bw: c.bw,
        adjust: c.adjust,
        kernel: c.kernel,
        n: 64,
      });
      expect(curve?.x).toHaveLength(64);
      curve?.x.forEach((x, i) => {
        expect(x).toBeCloseTo(c.x[i] as number, 8);
      });
      // Both estimate from binned values, R on a grid through a Fourier
      // transform; they agree within 0.5% of the peak, and within 4% for the
      // rectangular kernel, whose edges fall between grid points (3.3% at
      // most on these samples).
      const peak = Math.max(...c.y);
      const tolerance = c.kernel === "rectangular" ? 0.04 : 0.005;
      curve?.y.forEach((y, i) => {
        expect(Math.abs(y - (c.y[i] as number))).toBeLessThan(tolerance * peak);
      });
    });
  }

  it("takes an explicit bandwidth, and has no rule bandwidth for a constant sample", () => {
    const constant = { n: 3, min: 2, max: 2, sd: 0, q1: 2, q3: 2 };
    expect(kernelDensity(constant, [3])).toBeNull();
    const curve = kernelDensity(constant, [3], { bandwidth: 0.5, n: 5 });
    expect(curve?.bw).toBe(0.5);
    expect(curve?.x).toEqual([0.5, 1.25, 2, 2.75, 3.5]);
    expect(curve?.y[2]).toBeCloseTo(1 / (0.5 * Math.sqrt(2 * Math.PI)), 12);
  });

  it("stops on a rule it leaves to R", () => {
    const s = fixture.cases[0]?.summary as DensitySummary;
    expect(() => ruleBandwidth(s, "SJ")).toThrow("computed in R");
  });
});
