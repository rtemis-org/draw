import { describe, expect, it } from "vitest";
import { AXIS_LABEL_INSET_EM, axisLabelInset } from "../src/shared/layout.ts";

describe("axisLabelInset", () => {
  it("is 0.09 em of the font size", () => {
    expect(AXIS_LABEL_INSET_EM).toBe(0.09);
    expect(axisLabelInset(16)).toBeCloseTo(1.44, 10);
    expect(axisLabelInset(0)).toBe(0);
  });
});
