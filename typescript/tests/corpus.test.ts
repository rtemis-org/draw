import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { A3 } from "@rtemis/a3";
import { describe, expect, it } from "vitest";
import { builders, type ChartType } from "../src/index.ts";

// Each case is <name>.case.json: { type, data (a fixture file name), config,
// appearance }. Its expected option is <name>.option.json, the builder's
// output serialized as JSON, which drops formatter functions. RTEMIS_DRAW_
// CORPUS_WRITE=1 rewrites the expected files.
const dir = new URL("./corpus/", import.meta.url);
const write = process.env.RTEMIS_DRAW_CORPUS_WRITE === "1";

interface Case {
  type: ChartType;
  data: string;
  config?: Record<string, unknown>;
  appearance?: Record<string, unknown>;
}

function load(name: string): unknown {
  const text = readFileSync(new URL(`../fixtures/${name}`, dir), "utf8");
  return new A3(JSON.parse(text)).toData();
}

describe("option corpus", () => {
  const cases = readdirSync(dir).filter((f) => f.endsWith(".case.json"));
  it("has cases", () => expect(cases.length).toBeGreaterThan(0));
  for (const file of cases) {
    it(file, () => {
      const c = JSON.parse(readFileSync(new URL(file, dir), "utf8")) as Case;
      const build = builders[c.type] as (d: unknown, cf?: unknown, a?: unknown) => unknown;
      const actual = `${JSON.stringify(build(load(c.data), c.config, c.appearance), null, 2)}\n`;
      const expectedUrl = new URL(file.replace(".case.json", ".option.json"), dir);
      if (write) writeFileSync(expectedUrl, actual);
      expect(actual).toBe(readFileSync(expectedUrl, "utf8"));
    });
  }
});
