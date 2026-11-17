import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  type BreaksRule,
  classCount,
  type HistogramSummary,
  histogramEdges,
  pretty,
} from "../src/charts/histogram/edges.ts";

// Written by rtemis.draw's data-raw/histogram_edges.R from graphics::hist()
// and base::pretty().
interface Fixture {
  cases: {
    name: string;
    summary: HistogramSummary;
    breaks: Record<"Sturges" | "Scott" | "FD", number[]>;
    bins: { bins: number; edges: number[] }[];
  }[];
  pretty: { lo: number; hi: number; n: number; edges: number[] }[];
}
const fixture = JSON.parse(
  readFileSync(new URL("./fixtures/histogram-edges.json", import.meta.url), "utf8"),
) as Fixture;

const close = (actual: number[], expected: number[]) => {
  expect(actual).toHaveLength(expected.length);
  actual.forEach((v, i) => {
    expect(v).toBeCloseTo(expected[i] as number, 12);
  });
};

describe("histogramEdges", () => {
  for (const c of fixture.cases) {
    for (const rule of ["Sturges", "Scott", "FD"] as const) {
      it(`matches graphics::hist() for ${c.name} by ${rule}`, () => {
        close(histogramEdges(c.summary, { breaks: rule as BreaksRule }), c.breaks[rule]);
      });
    }
    it(`matches graphics::hist() for ${c.name} with a suggested bin count`, () => {
      for (const b of c.bins) close(histogramEdges(c.summary, { bins: b.bins }), b.edges);
    });
  }

  it("defaults to Sturges and treats Freedman-Diaconis as FD", () => {
    const c = fixture.cases[0] as Fixture["cases"][number];
    expect(histogramEdges(c.summary)).toEqual(histogramEdges(c.summary, { breaks: "Sturges" }));
    expect(classCount(c.summary, "Freedman-Diaconis")).toBe(classCount(c.summary, "FD"));
  });

  it("stops without a finite value or with a bin count below 1", () => {
    expect(() => histogramEdges({ n: 0, min: Number.NaN, max: Number.NaN })).toThrow(
      "at least one finite value",
    );
    expect(() => histogramEdges({ n: 3, min: 0, max: 1 }, { bins: 0 })).toThrow(
      "`bins` must be at least 1",
    );
  });
});

describe("pretty", () => {
  for (const p of fixture.pretty) {
    it(`matches base::pretty() over [${p.lo}, ${p.hi}] in about ${p.n} intervals`, () => {
      close(pretty(p.lo, p.hi, p.n, 1), p.edges);
    });
  }
});
