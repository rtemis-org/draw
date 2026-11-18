import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import * as chromatic from "d3-scale-chromatic";
import { feature } from "topojson-client";
import { describe, expect, it } from "vitest";
import {
  buildChoroplethScale,
  CHOROPLETH_COLORS,
  type ChoroplethColormap,
  choroplethColor,
  choroplethFillExpression,
  joinChoropleth,
  type MapClassification,
  type MapModel,
  mapFeatures,
} from "../src/charts/choropleth/config.ts";
import { type MapResolution, normalizeLocation } from "../src/charts/choropleth/locations.ts";

// rtemis.draw's map scene (map_scene.js) on every key of its tables and on
// six value sets, written before the package functions replaced it.
const parity = JSON.parse(
  readFileSync(join(__dirname, "fixtures/choropleth/r-parity.json"), "utf8"),
) as {
  locations: { raw: string | number | null; resolution: MapResolution; id: string | null }[];
  sets: Record<string, (number | null | "NaN")[]>;
  scales: {
    values: string;
    classification: MapClassification;
    colormap: ChoroplethColormap;
    num_classes: number;
    dark: boolean;
    error?: string;
    thresholds?: number[];
    colors?: string[];
    missingColor?: string;
    min?: number;
    max?: number;
  }[];
};

const valuesOf = (name: string) =>
  (parity.sets[name] ?? []).map((v) => (v === "NaN" ? Number.NaN : v));

describe("normalizeLocation", () => {
  it("reproduces rtemis.draw's keys at every resolution", () => {
    for (const { raw, resolution, id } of parity.locations) {
      expect(normalizeLocation(raw, resolution), `${String(raw)} ${resolution}`).toBe(id);
    }
  });
});

describe("buildChoroplethScale", () => {
  it("reproduces rtemis.draw's breaks and colors", () => {
    for (const c of parity.scales) {
      if (c.error) continue;
      const scale = buildChoroplethScale(
        valuesOf(c.values),
        { classification: c.classification, colormap: c.colormap, num_classes: c.num_classes },
        { scheme: c.dark ? "dark" : "light" },
        chromatic,
      );
      const name = `${c.values} ${c.classification} ${c.colormap} ${c.num_classes}`;
      expect(scale.thresholds, name).toEqual(c.thresholds);
      expect(scale.colors, name).toEqual(c.colors);
      expect([scale.missingColor, scale.min, scale.max], name).toEqual([
        c.missingColor,
        c.min,
        c.max,
      ]);
    }
  });

  it("classifies tied values into fewer classes where rtemis.draw stopped", () => {
    const crashed = parity.scales.filter((c) => c.error);
    expect(crashed.length).toBeGreaterThan(0);
    for (const c of crashed) {
      const scale = buildChoroplethScale(
        valuesOf(c.values),
        { classification: c.classification, colormap: c.colormap, num_classes: c.num_classes },
        {},
        chromatic,
      );
      expect(scale.thresholds.length).toBeLessThan(c.num_classes - 1);
      expect(scale.colors).toHaveLength(scale.thresholds.length + 1);
    }
  });

  it("labels the classes with their bounds to three significant digits", () => {
    const scale = buildChoroplethScale(
      [0.123456, 1.5, 2.75, 1234.5],
      { classification: "equal", num_classes: 2 },
      {},
      chromatic,
    );
    expect(scale.legend.map((e) => e.label)).toEqual(["0.123 – 617", "617 – 1235"]);
    expect(scale.legend.map((e) => e.color)).toEqual(scale.colors);
  });

  it("gives no breaks and no legend without a finite value", () => {
    const scale = buildChoroplethScale([null, Number.NaN], {}, { scheme: "dark" }, chromatic);
    expect(scale).toMatchObject({ thresholds: [], legend: [], missingColor: "#3f3f46" });
  });
});

describe("choroplethColor and choroplethFillExpression", () => {
  const scale = buildChoroplethScale([1, 2, 3, 4], { num_classes: 2 }, {}, chromatic);

  it("colors a value by its class and a missing value with the missing color", () => {
    expect(scale.thresholds).toEqual([2.5]);
    expect(choroplethColor(scale, 1)).toBe(scale.colors[0]);
    expect(choroplethColor(scale, 2.5)).toBe(scale.colors[1]);
    expect(choroplethColor(scale, null)).toBe(CHOROPLETH_COLORS.missing.light);
  });

  it("steps over the breaks in MapLibre's expression", () => {
    expect(choroplethFillExpression(scale)).toEqual([
      "case",
      ["!=", ["feature-state", "v"], null],
      ["step", ["to-number", ["feature-state", "v"]], scale.colors[0], 2.5, scale.colors[1]],
      scale.missingColor,
    ]);
    const flat = buildChoroplethScale([5, 5], {}, {}, chromatic);
    expect(choroplethFillExpression(flat)[2]).toBe(flat.colors[0]);
  });
});

describe("joinChoropleth", () => {
  const ids = new Set(["06", "48", "36"]);
  const model: MapModel = {
    rows: [
      { location: "CA", value: 1.23456, extras: { name: "California", pop: 39538223.5 } },
      { location: "48", value: null },
      { location: "Atlantis", value: 3 },
      { location: "new york", value: 2 },
    ],
    resolution: "state",
    tooltipFields: ["name", "pop"],
  };

  it("joins rows to regions and reports the rows without one", () => {
    const join = joinChoropleth(model, ids);
    expect([...join.values]).toEqual([
      ["06", 1.23456],
      ["36", 2],
    ]);
    expect(join.extras.get("06")).toEqual([
      { label: "name", value: "California" },
      { label: "pop", value: "39538224" },
    ]);
    expect([join.matched, join.unmatched, join.unmatchedKeys]).toEqual([3, 1, ["Atlantis"]]);
  });

  it("stops on two rows naming one region", () => {
    const twice = { ...model, rows: [...model.rows, { location: "California", value: 9 }] };
    expect(() => joinChoropleth(twice, ids)).toThrow(
      "A region takes one value; rows 1 and 5 both name region '06' ('CA', 'California').",
    );
  });
});

const usAtlas = join(__dirname, "../../r/inst/htmlwidgets/lib/geo/us-10m.topo.json");

describe.skipIf(!existsSync(usAtlas))("mapFeatures", () => {
  it("decodes a resolution's regions with their join ids", () => {
    const topology = JSON.parse(readFileSync(usAtlas, "utf8"));
    const states = mapFeatures(topology, "state", feature);
    expect(states.ids.has("06")).toBe(true);
    const ca = states.features.features.find((f) => f.properties.joinId === "06");
    expect(ca?.properties.name).toBe("California");
    expect(() => mapFeatures(topology, "country", feature)).toThrow(
      "The map geometry has no object 'countries'.",
    );
  });
});
