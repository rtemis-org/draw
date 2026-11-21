import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { A3 } from "@rtemis/a3";
import { describe, expect, it } from "vitest";
import { type HeatmapHints, heatmapHints } from "../src/charts/heatmap/config.ts";
import { builders, type ChartType } from "../src/index.ts";
import { fit as fitChart } from "../src/layout/fit.ts";
import { type LayoutRect, measureLayout } from "../src/layout/measure.ts";
import { registerRenderers } from "../src/shared/renderers.ts";
import { lightThemeTokens, themeObject } from "../src/theme/tokens.ts";

// The geometry the fit phase leaves, measured on every corpus case and a few
// charts with keys and square cells, at a wide and a narrow surface: the plot
// rectangles, the legend, the color scale, and the title, after `fit()`. Each
// rectangle matches the recorded one within half a pixel, so a change of the
// fit's geometry fails and a renumbering of the drawn elements does not.
// RTEMIS_DRAW_GEOMETRY_WRITE=1 rewrites the records.
// spec: draw/chart-appearance#build-and-fit

// biome-ignore lint/suspicious/noExplicitAny: the R package's ECharts bundle carries no types.
function loadECharts(): any {
  const path = new URL("../../r/inst/htmlwidgets/lib/echarts/echarts.min.js", import.meta.url)
    .pathname;
  return existsSync(path) ? createRequire(import.meta.url)(path) : null;
}

const echarts = loadECharts();
if (echarts) registerRenderers(echarts);
const write = process.env.RTEMIS_DRAW_GEOMETRY_WRITE === "1";
const records = new URL("./fixtures/fit-geometry/", import.meta.url);
const corpus = new URL("./corpus/", import.meta.url);
const SURFACES = [
  [600, 400],
  [390, 420],
] as const;
const TOLERANCE = 0.5;

interface Case {
  readonly option: Record<string, unknown>;
  readonly type: string;
  readonly hints?: HeatmapHints;
}

function fixture(name: string): unknown {
  const text = readFileSync(new URL(`./fixtures/${name}`, import.meta.url), "utf8");
  return name.endsWith(".a3.json") ? new A3(JSON.parse(text)).toData() : JSON.parse(text);
}

function cases(): Record<string, Case> {
  const out: Record<string, Case> = {};
  for (const file of readdirSync(corpus).filter((f) => f.endsWith(".case.json"))) {
    const c = JSON.parse(readFileSync(new URL(file, corpus), "utf8")) as {
      type: ChartType;
      data: string;
      config?: Record<string, unknown>;
      appearance?: Record<string, unknown>;
    };
    const build = builders[c.type] as (d: unknown, cf?: unknown, a?: unknown) => unknown;
    out[file.replace(".case.json", "")] = {
      type: c.type,
      option: build(fixture(c.data), c.config, c.appearance) as Record<string, unknown>,
    };
  }
  const columns = { x: [1, 2, 3, 4, 5, 6], y: [2, 1, 4, 3, 6, 5], c: [0, 1, 2, 3, 4, 5] };
  out["scatter-keys"] = {
    option: builders.scatter(
      { columns: { ...columns, s: [1, 3, 2, 6, 4, 5] } },
      {
        x: "x",
        y: "y",
        color: "c",
        size: "s",
      },
    ) as Record<string, unknown>,
    type: "scatter",
  };
  const parity = fixture("heatmap/r-parity.json") as {
    name: string;
    data: unknown;
    config: Record<string, unknown>;
  }[];
  for (const c of parity.filter((p) => ["plain", "clustered"].includes(p.name))) {
    out[`heatmap-${c.name}`] = {
      type: "heatmap",
      option: builders.heatmap(c.data as never, c.config as never) as Record<string, unknown>,
      hints: heatmapHints(c.data as never, c.config as never),
    };
  }
  return out;
}

type Geometry = Record<string, LayoutRect | readonly LayoutRect[] | null>;

function measure(c: Case, width: number, height: number): Geometry {
  const theme = themeObject(lightThemeTokens());
  const chart = echarts.init(null, theme, { renderer: "svg", ssr: true, width, height });
  try {
    chart.setOption({ ...c.option, animation: false });
    fitChart(echarts, chart, {
      type: c.type,
      option: c.option,
      ...(c.hints ? { hints: { heatmap: c.hints } } : {}),
    });
    const layout = measureLayout(chart);
    return {
      plots: layout.plots,
      legend: layout.legend,
      colorScale: layout.colorScale,
      title: layout.title,
    };
  } finally {
    chart.dispose();
  }
}

function close(actual: unknown, expected: unknown, at: string, out: string[]): void {
  if (typeof expected === "number" && typeof actual === "number") {
    if (Math.abs(actual - expected) > TOLERANCE) out.push(`${at}: ${actual} != ${expected}`);
    return;
  }
  if (expected !== null && typeof expected === "object") {
    if (actual === null || typeof actual !== "object") {
      out.push(`${at}: ${JSON.stringify(actual)} != ${JSON.stringify(expected)}`);
      return;
    }
    const keys = new Set([...Object.keys(expected), ...Object.keys(actual)]);
    for (const k of keys) {
      close(
        (actual as Record<string, unknown>)[k],
        (expected as Record<string, unknown>)[k],
        `${at}.${k}`,
        out,
      );
    }
    return;
  }
  if (actual !== expected)
    out.push(`${at}: ${JSON.stringify(actual)} != ${JSON.stringify(expected)}`);
}

describe.skipIf(!echarts)("fit geometry", () => {
  const all = echarts ? cases() : {};
  if (write) mkdirSync(records, { recursive: true });
  for (const [name, c] of Object.entries(all)) {
    it(`${name} keeps its layout`, () => {
      const geometry = Object.fromEntries(
        SURFACES.map(([w, h]) => [`${w}x${h}`, measure(c, w, h)]),
      );
      const url = new URL(`${name}.json`, records);
      if (write) writeFileSync(url, `${JSON.stringify(geometry, null, 2)}\n`);
      const expected = JSON.parse(readFileSync(url, "utf8"));
      const differences: string[] = [];
      close(geometry, expected, name, differences);
      expect(differences).toEqual([]);
    });
  }
});
