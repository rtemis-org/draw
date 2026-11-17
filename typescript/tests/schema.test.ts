import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { A3_CHART_CONFIG_MEMBERS } from "../src/charts/a3/config.ts";
import { BAR_CHART_CONFIG_MEMBERS } from "../src/charts/bar/config.ts";
import { BOXPLOT_CHART_CONFIG_MEMBERS } from "../src/charts/boxplot/config.ts";
import { DENSITY_CHART_CONFIG_MEMBERS } from "../src/charts/density/config.ts";
import { HISTOGRAM_CHART_CONFIG_MEMBERS } from "../src/charts/histogram/config.ts";
import { LINE_CHART_CONFIG_MEMBERS } from "../src/charts/line/config.ts";
import { SCATTER_CHART_CONFIG_MEMBERS } from "../src/charts/scatter/config.ts";

// `just ts-schema-check` generates the chart schemas from the R classes into a
// temporary directory and sets RTEMIS_DRAW_SCHEMA_DIR; without it the check is
// skipped. Provenance members describe where a config came from and do not
// reach the builder.
const schemaDir = process.env.RTEMIS_DRAW_SCHEMA_DIR;
const PROVENANCE = new Set(["dat_path", "origin", "writer"]);

describe.skipIf(!schemaDir)("config types against the generated schemas", () => {
  const cases: [string, readonly string[]][] = [
    ["a3", A3_CHART_CONFIG_MEMBERS],
    ["bar", BAR_CHART_CONFIG_MEMBERS],
    ["boxplot", BOXPLOT_CHART_CONFIG_MEMBERS],
    ["density", DENSITY_CHART_CONFIG_MEMBERS],
    ["histogram", HISTOGRAM_CHART_CONFIG_MEMBERS],
    ["line", LINE_CHART_CONFIG_MEMBERS],
    ["scatter", SCATTER_CHART_CONFIG_MEMBERS],
  ];
  for (const [type, members] of cases) {
    it(`the ${type} config type has every chart/${type}/v1 property and no other`, () => {
      const schema = JSON.parse(
        readFileSync(join(schemaDir ?? "", `chart/${type}/v1/schema.json`), "utf8"),
      ) as { properties: Record<string, unknown> };
      const fromSchema = Object.keys(schema.properties)
        .filter((k) => !PROVENANCE.has(k))
        .sort();
      expect([...members].sort()).toEqual(fromSchema);
    });
  }
});
