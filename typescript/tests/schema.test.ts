import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { A3_CHART_CONFIG_MEMBERS } from "../src/charts/a3/config.ts";

// `just ts-schema-check` generates the chart schemas from the R classes into a
// temporary directory and sets RTEMIS_DRAW_SCHEMA_DIR; without it the check is
// skipped. Provenance members describe where a config came from and do not
// reach the builder.
const schemaDir = process.env.RTEMIS_DRAW_SCHEMA_DIR;
const PROVENANCE = new Set(["dat_path", "origin", "writer"]);

describe.skipIf(!schemaDir)("config types against the generated schemas", () => {
  it("A3ChartConfig has every chart/a3/v1 property and no other", () => {
    const schema = JSON.parse(
      readFileSync(join(schemaDir ?? "", "chart/a3/v1/schema.json"), "utf8"),
    ) as { properties: Record<string, unknown> };
    const fromSchema = Object.keys(schema.properties)
      .filter((k) => !PROVENANCE.has(k))
      .sort();
    expect([...A3_CHART_CONFIG_MEMBERS].sort()).toEqual(fromSchema);
  });
});
