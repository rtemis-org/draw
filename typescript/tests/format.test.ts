import { describe, expect, it } from "vitest";
import { ddSci, tooltipValue } from "../src/shared/format.ts";

// Expected text is rtemis.core::ddSci()'s for the same values and decimals.
const values = [
  0.34876549, 0.00000000457823, 0, 0.075, 0.0049, -0.0032, 1234567.8, 0.999, 1, 12.345,
];
const expected: Record<number, string[]> = {
  2: ["0.35", "4.6e-09", "0.00", "0.07", "4.9e-03", "-3.2e-03", "1.2e+06", "1.00", "1.00", "12.35"],
  3: [
    "0.349",
    "4.6e-09",
    "0.000",
    "0.075",
    "0.005",
    "-0.003",
    "1.23e+06",
    "0.999",
    "1.000",
    "12.345",
  ],
  1: ["0.3", "4.6e-09", "0.0", "0.1", "4.9e-03", "-3.2e-03", "1e+06", "1.0", "1.0", "12.3"],
};

describe("ddSci", () => {
  it("writes numbers as rtemis.core's ddSci() does", () => {
    for (const [decimals, text] of Object.entries(expected)) {
      expect(values.map((v) => ddSci(v, Number(decimals)))).toEqual(text);
    }
  });

  it("tests the rounding to zero at the requested decimals", () => {
    expect(ddSci(0.0004, 3)).toBe("4e-04");
    expect(ddSci(0.0049, 3)).toBe("0.005");
  });

  it("writes NA for a missing or non-finite value", () => {
    expect([null, undefined, Number.NaN, Number.POSITIVE_INFINITY].map((v) => ddSci(v))).toEqual([
      "NA",
      "NA",
      "NA",
      "NA",
    ]);
  });
});

describe("tooltipValue", () => {
  it("ignores the data index ECharts passes as its second argument", () => {
    const format = tooltipValue as (value: unknown, dataIndex: number) => unknown;
    expect(format(1.23456, 181)).toBe("1.23");
    expect(format(7, 500)).toBe("7");
  });
});
