import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { A3_CHART_CONFIG_MEMBERS } from "../src/charts/a3/config.ts";
import { BAR_CHART_CONFIG_MEMBERS } from "../src/charts/bar/config.ts";
import { BOXPLOT_CHART_CONFIG_MEMBERS } from "../src/charts/boxplot/config.ts";
import { CALIBRATION_CHART_CONFIG_MEMBERS } from "../src/charts/calibration/config.ts";
import { CHOROPLETH_CHART_CONFIG_MEMBERS } from "../src/charts/choropleth/config.ts";
import { CONFUSION_CHART_CONFIG_MEMBERS } from "../src/charts/confusion/config.ts";
import { PR_CHART_CONFIG_MEMBERS, ROC_CHART_CONFIG_MEMBERS } from "../src/charts/curves/config.ts";
import { DENSITY_CHART_CONFIG_MEMBERS } from "../src/charts/density/config.ts";
import { GANTT_CHART_CONFIG_MEMBERS } from "../src/charts/gantt/config.ts";
import { HEATMAP_CHART_CONFIG_MEMBERS } from "../src/charts/heatmap/config.ts";
import { HISTOGRAM_CHART_CONFIG_MEMBERS } from "../src/charts/histogram/config.ts";
import { LINE_CHART_CONFIG_MEMBERS } from "../src/charts/line/config.ts";
import { NETWORK_CHART_CONFIG_MEMBERS } from "../src/charts/network/config.ts";
import { PIE_CHART_CONFIG_MEMBERS } from "../src/charts/pie/config.ts";
import { SANKEY_CHART_CONFIG_MEMBERS } from "../src/charts/sankey/config.ts";
import { SCATTER_CHART_CONFIG_MEMBERS } from "../src/charts/scatter/config.ts";
import { SCATTER3D_CHART_CONFIG_MEMBERS } from "../src/charts/scatter3d/config.ts";
import { SIGNIFICANCE_CHART_CONFIG_MEMBERS } from "../src/charts/significance/config.ts";
import { SPECTROGRAM_CHART_CONFIG_MEMBERS } from "../src/charts/spectrogram/config.ts";
import { SURVIVAL_CHART_CONFIG_MEMBERS } from "../src/charts/survival/config.ts";
import { TIMESERIES_CHART_CONFIG_MEMBERS } from "../src/charts/timeseries/config.ts";

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
    ["calibration", CALIBRATION_CHART_CONFIG_MEMBERS],
    ["choropleth", CHOROPLETH_CHART_CONFIG_MEMBERS],
    ["confusion", CONFUSION_CHART_CONFIG_MEMBERS],
    ["density", DENSITY_CHART_CONFIG_MEMBERS],
    ["gantt", GANTT_CHART_CONFIG_MEMBERS],
    ["heatmap", HEATMAP_CHART_CONFIG_MEMBERS],
    ["histogram", HISTOGRAM_CHART_CONFIG_MEMBERS],
    ["line", LINE_CHART_CONFIG_MEMBERS],
    ["network", NETWORK_CHART_CONFIG_MEMBERS],
    ["pie", PIE_CHART_CONFIG_MEMBERS],
    ["pr", PR_CHART_CONFIG_MEMBERS],
    ["roc", ROC_CHART_CONFIG_MEMBERS],
    ["sankey", SANKEY_CHART_CONFIG_MEMBERS],
    ["scatter", SCATTER_CHART_CONFIG_MEMBERS],
    ["scatter3d", SCATTER3D_CHART_CONFIG_MEMBERS],
    ["significance", SIGNIFICANCE_CHART_CONFIG_MEMBERS],
    ["spectrogram", SPECTROGRAM_CHART_CONFIG_MEMBERS],
    ["survival", SURVIVAL_CHART_CONFIG_MEMBERS],
    ["timeseries", TIMESERIES_CHART_CONFIG_MEMBERS],
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
