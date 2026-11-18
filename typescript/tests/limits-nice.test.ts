import { describe, expect, it } from "vitest";
import { niceAxis } from "../src/shared/limits.ts";

describe("niceAxis", () => {
  it("spans the range with the least room among nice intervals", () => {
    expect(niceAxis(2700, 6300)).toEqual({ min: 2000, max: 7000, interval: 1000, decimals: 0 });
    expect(niceAxis(0.2884351517, 0.7642381465)).toEqual({
      min: 0.2,
      max: 0.8,
      interval: 0.2,
      decimals: 1,
    });
    expect(niceAxis(32.1, 59.6)).toEqual({ min: 30, max: 60, interval: 10, decimals: 0 });
    expect(niceAxis(0, 1)).toEqual({ min: 0, max: 1, interval: 0.2, decimals: 1 });
    expect(niceAxis(0.00123, 0.00456)).toMatchObject({ interval: 0.001, decimals: 3 });
  });

  it("widens a single value", () => {
    expect(niceAxis(2, 2)).toMatchObject({ min: 1, max: 3 });
  });
});
