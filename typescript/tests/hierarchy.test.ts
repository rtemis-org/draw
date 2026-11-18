import { describe, expect, it } from "vitest";
import { buildSunburstOption } from "../src/charts/sunburst/config.ts";
import { buildTreemapOption } from "../src/charts/treemap/config.ts";
import { hierarchyDepth, hierarchyFromTable } from "../src/shared/hierarchy.ts";
import type { TabularData } from "../src/shared/source.ts";

type Series = Record<string, unknown> & { data: unknown };
const seriesOf = (option: Record<string, unknown>) => (option.series as Series[])[0] as Series;

const places: TabularData = {
  columns: {
    region: ["Europe", "Europe", "Asia", "Europe", "Asia"],
    country: ["France", "Spain", "Japan", "France", null],
    n: [3, 2, 5, 1, 4],
  },
};
const bound = { levels: ["region", "country"], value: "n" } as const;

describe("hierarchyFromTable", () => {
  it("nests rows by level and sums the rows of a path", () => {
    expect(
      hierarchyFromTable(
        {
          ...places,
          columns: { ...places.columns, country: ["France", "Spain", "Japan", "France", "China"] },
        },
        bound,
        "A treemap",
      ),
    ).toEqual([
      {
        name: "Europe",
        children: [
          { name: "France", value: 4 },
          { name: "Spain", value: 2 },
        ],
      },
      {
        name: "Asia",
        children: [
          { name: "Japan", value: 5 },
          { name: "China", value: 4 },
        ],
      },
    ]);
  });

  it("ends a path at its first missing level and orders siblings by level", () => {
    const flat = { columns: { region: ["b", "a", "b"] }, levels: { region: ["a", "b"] } };
    expect(hierarchyFromTable(flat, { levels: "region" }, "A treemap")).toEqual([
      { name: "a", value: 1 },
      { name: "b", value: 2 },
    ]);
    const ragged = { columns: { region: ["Asia", "Asia"], country: [null, ""], n: [1, 2] } };
    expect(hierarchyFromTable(ragged, bound, "A treemap")).toEqual([{ name: "Asia", value: 3 }]);
  });

  it("stops on a node that ends a row and has children", () => {
    expect(() => hierarchyFromTable(places, bound, "A treemap")).toThrow(
      "Node 'Asia' has children, which give its value; row 5 ends at it. Give the row its deeper levels, or leave it out.",
    );
  });

  it("stops on missing levels, bad values, and a zero total", () => {
    expect(() => hierarchyFromTable(places, { value: "n" }, "A treemap")).toThrow(
      "A treemap needs the config member 'levels' set.",
    );
    expect(() =>
      hierarchyFromTable({ columns: { region: ["a", null] } }, { levels: "region" }, "A sunburst"),
    ).toThrow(
      "Column 'region' (config member 'levels') must name a node in every row; row 2 is missing.",
    );
    expect(() =>
      hierarchyFromTable(
        { columns: { region: ["a"], n: [-1] } },
        { levels: "region", value: "n" },
        "A treemap",
      ),
    ).toThrow(
      "Column 'n' (config member 'value') must hold a finite, nonnegative number in every row; row 1 holds '-1'.",
    );
    expect(() =>
      hierarchyFromTable(
        { columns: { region: ["a"], n: [0] } },
        { levels: "region", value: "n" },
        "A treemap",
      ),
    ).toThrow("A treemap needs a positive total; column 'n' sums to 0.");
  });

  it("measures a tree's depth", () => {
    expect(hierarchyDepth([{ name: "a", value: 1 }])).toBe(1);
    expect(
      hierarchyDepth([
        { name: "a", children: [{ name: "b", children: [{ name: "c", value: 1 }] }] },
      ]),
    ).toBe(3);
  });
});

describe("buildTreemapOption", () => {
  const flat = { columns: { kind: ["a", "b"], n: [1, 2] } };

  it("draws a table's tree with borders in the background color", () => {
    const option = buildTreemapOption(
      flat,
      { levels: "kind", value: "n" },
      { background: "#181818" },
    );
    const series = seriesOf(option);
    expect(series).toMatchObject({
      type: "treemap",
      roam: false,
      nodeClick: false,
      breadcrumb: { show: false },
      top: 8,
      bottom: 8,
      label: { show: true, formatter: "{b}" },
      itemStyle: { borderColor: "#181818", borderWidth: 1, gapWidth: 0 },
    });
    expect(series.data).toEqual([
      { name: "a", value: 1 },
      { name: "b", value: 2 },
    ]);
    expect(option.color).toBeUndefined();
    expect(option.title).toBeUndefined();
    const dark = seriesOf(buildTreemapOption(flat, { levels: "kind" }, { dark: true }));
    expect((dark.itemStyle as { borderColor: string }).borderColor).toBe("#181818");
  });

  it("draws a host's tree, the palette, labels, and title", () => {
    const tree = [{ name: "x", children: [{ name: "y", value: 3 }] }];
    const option = buildTreemapOption(
      { tree },
      { show_labels: false, border_width: 2, title: "T", palette: ["#111111"] },
      { palette: ["#222222"] },
    );
    const series = seriesOf(option);
    expect(series.data).toBe(tree);
    expect(series).toMatchObject({
      label: { show: false },
      itemStyle: { borderWidth: 2 },
      top: 40,
    });
    expect(option.color).toEqual(["#111111"]);
    expect(option.title).toEqual({ text: "T", left: "center" });
    expect(buildTreemapOption({ tree }, {}, { palette: ["#222222"] }).color).toEqual(["#222222"]);
  });
});

describe("buildSunburstOption", () => {
  const tree = [
    { name: "a", children: [{ name: "b", children: [{ name: "c", value: 1 }] }] },
    { name: "d", value: 2 },
  ];

  it("labels the innermost rings up to the label depth", () => {
    const series = seriesOf(buildSunburstOption({ tree }, {}));
    expect(series).toMatchObject({
      type: "sunburst",
      radius: ["18%", "85%"],
      nodeClick: false,
      label: { rotate: "radial", minAngle: 0 },
      itemStyle: { borderColor: "#ffffff", borderWidth: 1 },
    });
    expect((series.levels as { label: { show: boolean } }[]).map((l) => l.label.show)).toEqual([
      false,
      true,
      true,
      false,
    ]);
  });

  it("maps rotation, angle, radii, and depth", () => {
    const series = seriesOf(
      buildSunburstOption(
        { tree },
        {
          label_depth: 0,
          label_rotate: "horizontal",
          min_angle: 5,
          radius: "90%",
          inner_radius: "0%",
        },
      ),
    );
    expect(series.radius).toEqual(["0%", "90%"]);
    expect(series.label).toEqual({ rotate: 0, minAngle: 5 });
    expect((series.levels as { label: { show: boolean } }[]).every((l) => !l.label.show)).toBe(
      true,
    );
  });

  it("builds a table's tree", () => {
    const series = seriesOf(
      buildSunburstOption({ columns: { r: ["x", "x"], c: ["p", "q"] } }, { levels: ["r", "c"] }),
    );
    expect(series.data).toEqual([
      {
        name: "x",
        children: [
          { name: "p", value: 1 },
          { name: "q", value: 1 },
        ],
      },
    ]);
  });
});
