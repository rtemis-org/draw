import { readdirSync, readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { builders } from "../src/index.ts";

// A builder writes no path the theme owns, since any value in the option
// defeats the registered theme beneath it. spec: draw/chart-appearance#rule
const THEME_OWNED = [
  /(^|\.)(textStyle|nameTextStyle|subtextStyle|axisTick|nameLocation|itemGap|backgroundColor)(\.|$)/,
  /^color(\.|$)/,
  /^([xyz]Axis|[xyz]Axis3D)\.(axisLine|splitLine)\.lineStyle(\.|$)/,
  /^(xAxis|yAxis)\.splitLine\.show$/,
  /^([xyz]Axis|[xyz]Axis3D)\.axisLabel\.(color|fontSize|fontFamily|fontWeight|margin)$/,
  /^([xyz]Axis|[xyz]Axis3D)\.axisPointer\.(lineStyle|label\.backgroundColor)(\.|$)/,
];

// Paths the grammar of a chart sets on purpose: a rich-text style the
// builder's own formatter names (A3's legend headings) sets its weight and
// padding, and an axis places its ticks where it places its labels.
const GRAMMAR = [
  /\.textStyle\.rich\.\w+\.(fontWeight|padding)$/,
  /^(xAxis|yAxis)\.axisTick\.(interval|alignWithLabel|customValues)$/,
];

// The significance chart hides the ticks of a dense axis with its labels.
const DENSE_TICKS = /^(xAxis|yAxis)\.axisTick\.show$/;

// Paths a chart type still writes until the theme carries them, fit to the
// surface in milestone T3 of the chart-appearance plan: the confusion
// matrix's text size, fit to its panels, and the heatmap's label margins,
// which clear its dendrograms.
const PENDING: Record<string, RegExp> = {
  confusion: /^(xAxis|yAxis)\.axisLabel\.fontSize$/,
  heatmap: /^(xAxis|yAxis)\.axisLabel\.margin$/,
};

function leafPaths(value: unknown, prefix = ""): string[] {
  if (Array.isArray(value)) return value.flatMap((v) => leafPaths(v, prefix));
  if (value !== null && typeof value === "object") {
    return Object.entries(value).flatMap(([k, v]) => leafPaths(v, prefix ? `${prefix}.${k}` : k));
  }
  return [prefix];
}

/**
 * The theme-owned paths an option writes. A config's `palette` member
 * replaces the theme's palette, so the option's `color` follows it.
 */
function themeOwned(
  type: string,
  option: unknown,
  config: unknown = {},
  extra: readonly RegExp[] = [],
): string[] {
  const pending = PENDING[type];
  const palette = (config as { palette?: unknown } | null)?.palette;
  const allowed = [
    ...GRAMMAR,
    ...extra,
    ...(pending ? [pending] : []),
    ...(palette === undefined || palette === null ? [] : [/^color$/]),
  ];
  return [...new Set(leafPaths(option))].filter(
    (path) => THEME_OWNED.some((re) => re.test(path)) && !allowed.some((re) => re.test(path)),
  );
}

interface ParityCase {
  name: string;
  config: unknown;
  data?: unknown;
  surfaces?: unknown;
}

// The R parity cases of a chart type: an array of cases, or `cases` with the
// data shared at the top (scatter3d, whose cases add their fitted surfaces).
function parityCases(type: string): { name: string; config: unknown; data: unknown }[] {
  const url = new URL(`./fixtures/${type}/r-parity.json`, import.meta.url);
  const parity = JSON.parse(readFileSync(url, "utf8")) as
    | ParityCase[]
    | { data?: Record<string, unknown>; cases: ParityCase[] };
  const list = Array.isArray(parity) ? parity : parity.cases;
  const shared = Array.isArray(parity) ? undefined : parity.data;
  return list.map((c) => ({
    name: c.name,
    config: Array.isArray(c.config) ? {} : c.config,
    data: shared ? { ...shared, ...(c.surfaces ? { surfaces: c.surfaces } : {}) } : c.data,
  }));
}

type Build = (data: unknown, config?: unknown, appearance?: unknown) => unknown;
const build = (type: keyof typeof builders): Build => builders[type] as Build;

describe("disjoint layers", () => {
  const dir = new URL("./corpus/", import.meta.url);
  const cases = readdirSync(dir).filter((f) => f.endsWith(".case.json"));
  for (const file of cases) {
    const { type, config } = JSON.parse(readFileSync(new URL(file, dir), "utf8")) as {
      type: string;
      config?: unknown;
    };
    it(`${file} writes no theme-owned path`, () => {
      const option = JSON.parse(
        readFileSync(new URL(file.replace(".case.json", ".option.json"), dir), "utf8"),
      );
      expect(themeOwned(type, option, config)).toEqual([]);
    });
  }

  // Every other chart type, from the inputs of its own tests.
  const tabular = (columns: Record<string, unknown[]>) => ({ columns });
  const inputs: [keyof typeof builders, unknown, unknown?][] = [
    [
      "roc",
      {
        groups: [{ split: "test", class: "yes", mean: 0.85, sd: 0.07 }],
        curves: [{ group: 0, fold: "Fold01", x: [0, 0.5, 1], y: [0, 0.7, 1], summary: 0.85 }],
      },
      {},
    ],
    [
      "pr",
      {
        groups: [{ split: "test", class: "yes", mean: 0.85, sd: 0.07, prevalence: 0.4 }],
        curves: [{ group: 0, fold: "Fold01", x: [0, 0.5, 1], y: [1, 0.7, 0.4], summary: 0.85 }],
      },
      {},
    ],
    [
      "survival",
      {
        curves: [
          {
            name: "A",
            time: [0, 2, 5],
            survival: [1, 0.6, 0.2],
            lower: [1, 0.4, null],
            upper: [1, 0.8, null],
            n_censor: [0, 2, 1],
            n_risk: [10, 8, 1],
          },
        ],
      },
      { landmarks: [1, 3] },
    ],
    [
      "gantt",
      tabular({
        label: ["load", "clean", "fold 1"],
        start: [0, 12, 30],
        end: [12, 30, 95],
        status: ["completed", "completed", "failed"],
      }),
      { label: "label", start: "start", end: "end", group: "status", xlab: "Elapsed (ms)" },
    ],
    [
      "significance",
      {
        label: ["A", "B", "C", "D", "E", "F"],
        estimate: [-2, -1, 0, 1, 2, 3],
        p_value: [0.001, 0.002, 0.003, 0.05, 0.004, 0.005],
        p_adjusted: [0.006, 0.01, 0.012, 0.05, 0.012, 0.012],
      },
      {},
    ],
    [
      "timeseries",
      tabular({ t: [1, 2, 3, 4], a: [3, 1, 2, 5], zt: [0, 6, 12, 18], dark: [0, 1, 1, 0] }),
      { x: "t", y: ["a"], zt: "zt", shade_bin: "dark" },
    ],
    [
      "confusion",
      {
        classes: ["a", "b"],
        panels: [
          {
            name: "Test",
            counts: [
              [40, 10],
              [5, 30],
            ],
          },
        ],
      },
      {},
    ],
    [
      "density",
      {
        samples: [
          { name: "a", n: 10, x: [0, 1, 2], y: [0.1, 0.5, 0.1] },
          { name: "b", n: 5, x: [1, 2, 3], y: [0.2, 0.3, 0.2] },
        ],
      },
      { mode: "ridge" },
    ],
    [
      "histogram",
      {
        edges: [0, 2, 4, 6],
        samples: [
          { name: "a", n: 10, counts: [2, 5, 3] },
          { name: "b", n: 4, counts: [0, 1, 3] },
        ],
      },
      {},
    ],
    ["pie", tabular({ kind: ["b", "a", "c"], n: [1, 2, 4] }), { values: "n", labels: "kind" }],
    [
      "radar",
      tabular({ team: ["a", "b"], speed: [2, 4], power: [-1, 3] }),
      { indicators: ["speed", "power"], group: "team" },
    ],
    [
      "funnel",
      tabular({ step: ["visit", "cart", "buy"], n: [60, 30, 10] }),
      { values: "n", labels: "step" },
    ],
    ["sankey", tabular({ source: ["a", "a", "b"], target: ["b", "c", "c"], value: [1, 2, 3] }), {}],
    ["treemap", tabular({ kind: ["a", "b"], n: [1, 2] }), { levels: "kind", value: "n" }],
    ["sunburst", tabular({ kind: ["a", "b"], n: [1, 2] }), { levels: "kind", value: "n" }],
  ];
  for (const [type, data, config] of inputs) {
    it(`${type} writes no theme-owned path`, () => {
      expect(themeOwned(type, build(type)(data, config), config)).toEqual([]);
    });
  }
  for (const type of ["calibration", "heatmap", "scatter3d", "spectrogram"] as const) {
    for (const c of parityCases(type)) {
      it(`${type} ${c.name} writes no theme-owned path`, () => {
        expect(themeOwned(type, build(type)(c.data, c.config), c.config)).toEqual([]);
      });
    }
  }
  it("significance hides a dense axis's ticks with its labels", () => {
    const n = 200;
    const option = build("significance")(
      {
        label: Array.from({ length: n }, (_, i) => `g${i}`),
        estimate: Array.from({ length: n }, (_, i) => i - n / 2),
        p_value: Array.from({ length: n }, () => 0.01),
        p_adjusted: Array.from({ length: n }, () => 0.02),
      },
      { view: "manhattan" },
    );
    expect(themeOwned("significance", option, {}, [DENSE_TICKS])).toEqual([]);
  });
});
