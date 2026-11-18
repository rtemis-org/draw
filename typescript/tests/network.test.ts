import { readFileSync } from "node:fs";
import { join } from "node:path";
import GraphologyModule from "graphology";
import louvainModule from "graphology-communities-louvain";
import { circlepack, random } from "graphology-layout";
import forceAtlas2Module from "graphology-layout-forceatlas2";
import { describe, expect, it } from "vitest";
import {
  buildNetworkGraph,
  type GraphModel,
  NEUTRAL_EDGE_COLOR,
  type NetworkChartConfig,
  networkReducers,
} from "../src/charts/network/config.ts";
import { RTEMIS_PALETTE, SIGN_COLORS } from "../src/theme/tokens.ts";

// Under NodeNext, TypeScript types a CommonJS package's default import as
// the module; at run time the default import is the default export.
const cjs = <T>(m: T) => m as unknown as T extends { default: infer D } ? D : T;
const libraries = {
  Graph: cjs(GraphologyModule),
  louvain: cjs(louvainModule),
  forceAtlas2: cjs(forceAtlas2Module),
  circlepack,
  random,
};

// rtemis.draw's graph scene (graph_scene.js) for each case, written before
// the package functions replaced it: the R models of cor(mtcars) at
// |r| >= 0.5 and of a directed edge list.
const parity = JSON.parse(
  readFileSync(join(__dirname, "fixtures/network/r-parity.json"), "utf8"),
) as {
  models: Record<string, GraphModel>;
  cases: {
    model: string;
    config: NetworkChartConfig;
    nodes: Record<string, unknown>[];
    edges: Record<string, unknown>[];
  }[];
};

const triangle: GraphModel = {
  nodes: [{ id: "a", value: 2 }, { id: "b", label: "B", value: 4 }, { id: "c" }],
  edges: [
    { source: "a", target: "b", weight: 2, sign: 1 },
    { source: "b", target: "c", weight: -1, sign: -1 },
    { source: "a", target: "c" },
    { source: "b", target: "a", weight: 5 },
    { source: "a", target: "zz" },
  ],
  directed: false,
};

describe("buildNetworkGraph", () => {
  it("reproduces rtemis.draw's communities, styles, and layouts other than force", () => {
    // The force layout starts from seeded random positions, where rtemis.draw
    // started it from a circle; its positions and label sides are left out.
    const placed = (c: (typeof parity.cases)[number], rows: Record<string, unknown>[]) =>
      c.config.layout === "force"
        ? rows.map(({ x: _x, y: _y, labelPosition: _l, ...rest }) => rest)
        : rows;
    for (const c of parity.cases) {
      const graph = buildNetworkGraph(parity.models[c.model] as GraphModel, c.config, libraries);
      const reducers = networkReducers(graph, c.config);
      const nodes = graph.mapNodes((id, a) => {
        const r = reducers.nodeReducer(id, a);
        return {
          id,
          x: a.x,
          y: a.y,
          community: a.community,
          size: r.size,
          color: r.color,
          opacity: r.opacity,
          labelPosition: r.labelPosition,
        };
      });
      const edges = graph.mapEdges((id, a, source, target) => {
        const r = reducers.edgeReducer(id, a);
        return { id, source, target, size: r.size, color: r.color, opacity: r.opacity };
      });
      expect(placed(c, nodes), `${c.model} ${c.config.layout}`).toEqual(placed(c, c.nodes));
      expect(edges, `${c.model} ${c.config.layout}`).toEqual(c.edges);
    }
  });

  it("keeps the model's nodes, one edge per pair, and edges between known nodes", () => {
    const graph = buildNetworkGraph(triangle, {}, libraries);
    expect(graph.type).toBe("undirected");
    expect(graph.nodes()).toEqual(["a", "b", "c"]);
    expect(graph.edges()).toEqual(["0", "1", "2"]);
    expect(graph.getNodeAttributes("b")).toMatchObject({ label: "B", value: 4, valueNorm: 1 });
    expect(graph.getNodeAttributes("a")).toMatchObject({ label: "a", valueNorm: 0.5, group: null });
    expect(graph.getEdgeAttributes("0")).toEqual({ weightNorm: 1, sign: 1, louvainWeight: 1 });
    expect(graph.getEdgeAttributes("1")).toEqual({ weightNorm: 0.5, sign: -1, louvainWeight: 0 });
    expect(graph.getEdgeAttributes("2")).toEqual({ weightNorm: 0.5, sign: 0, louvainWeight: 0.5 });
  });

  it("gives the same positions for a seed, and others for another seed", () => {
    for (const layout of ["force", "random"] as const) {
      const place = (seed?: number) =>
        buildNetworkGraph(
          parity.models.cor as GraphModel,
          seed === undefined ? { layout } : { layout, seed },
          libraries,
        ).mapNodes((_, a) => [a.x, a.y]);
      expect(place(), layout).toEqual(place(1));
      expect(place(7), layout).toEqual(place(7));
      expect(place(7), layout).not.toEqual(place(1));
    }
  });

  it("builds a directed graph and an empty one", () => {
    expect(buildNetworkGraph({ ...triangle, directed: true }, {}, libraries).type).toBe("directed");
    expect(buildNetworkGraph({ nodes: [], edges: [] }, {}, libraries).order).toBe(0);
  });
});

describe("networkReducers", () => {
  const graph = buildNetworkGraph(triangle, { layout: "circular" }, libraries);

  it("takes the host's palette and sign colors when the config sets none", () => {
    const r = networkReducers(graph, {}, {});
    expect(r.nodeReducer("a", {}).color).toBe(RTEMIS_PALETTE[0]);
    expect(r.edgeReducer("0", {}).color).toBe(SIGN_COLORS.positive);
    expect(r.edgeReducer("1", {}).color).toBe(SIGN_COLORS.negative);
    expect(r.edgeReducer("2", {}).color).toBe(NEUTRAL_EDGE_COLOR);
    const host = networkReducers(
      graph,
      {},
      { palette: ["#111111"], positive_color: "#222222", negative_color: "#333333" },
    );
    expect(host.nodeReducer("a", {}).color).toBe("#111111");
    expect(host.edgeReducer("0", {}).color).toBe("#222222");
    expect(host.edgeReducer("1", {}).color).toBe("#333333");
  });

  it("sizes nodes by value and edges by weight", () => {
    const r = networkReducers(graph, { node_size: 10, edge_scale: 4 });
    expect(r.nodeReducer("b", {}).size).toBe(15);
    expect(r.nodeReducer("c", {}).size).toBe(5);
    expect(r.edgeReducer("1", {}).size).toBe(2);
    const flat = networkReducers(graph, { node_size: 7, scale_by_degree: false });
    expect(flat.nodeReducer("b", {}).size).toBe(7);
  });

  it("labels nodes in the scheme's color and font", () => {
    const light = networkReducers(graph, {}).nodeReducer("a", { label: "a" });
    expect(light).toMatchObject({
      label: "a",
      labelColor: "#1a1a1a",
      backdropVisibility: "hidden",
    });
    expect(light.labelFont).toBeUndefined();
    const dark = networkReducers(graph, {}, { scheme: "dark", font_family: "Inter" });
    expect(dark.nodeReducer("a", {})).toMatchObject({ labelColor: "#e6e6e6", labelFont: "Inter" });
    const own = networkReducers(graph, {}, { label_color: "#123456" });
    expect(own.nodeReducer("a", {}).labelColor).toBe("#123456");
  });

  it("fades what lies outside the hovered node's neighborhood", () => {
    const path: GraphModel = {
      nodes: [{ id: "a" }, { id: "b" }, { id: "c" }],
      edges: [
        { source: "a", target: "b" },
        { source: "b", target: "c" },
      ],
    };
    const g = buildNetworkGraph(path, { layout: "circular" }, libraries);
    const r = networkReducers(g, { edge_opacity: 0.5 }, { scheme: "dark" }, () => "a");
    expect(r.nodeReducer("a", { label: "a" })).toMatchObject({
      label: "a",
      backdropVisibility: "visible",
      backdropColor: "#505050",
    });
    expect(r.nodeReducer("b", { label: "b" })).toMatchObject({ label: "b", opacity: 0.95 });
    expect(r.nodeReducer("c", { label: "c" })).toMatchObject({
      label: "",
      opacity: 0.15,
      color: `${RTEMIS_PALETTE[0]}28`,
    });
    expect(r.edgeReducer("0", {}).opacity).toBe(0.5);
    expect(r.edgeReducer("1", {}).opacity).toBe(0.06);
  });

  it("blends edge colors from hex colors and keeps a named color", () => {
    const g = buildNetworkGraph(triangle, {}, libraries);
    const blend = networkReducers(g, { blend_edges: true, node_color: "#000000" });
    expect(blend.edgeReducer("0", {}).color).toBe("#000000");
    const named = networkReducers(g, { blend_edges: true, node_color: "red" });
    expect(named.edgeReducer("0", {}).color).toBe("red");
    expect(named.nodeReducer("a", {}).color).toBe("red");
  });
});
