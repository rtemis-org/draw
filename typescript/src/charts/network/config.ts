import { asArray, type ColumnNames } from "../../shared/source.ts";
import { RTEMIS_PALETTE, SIGN_COLORS } from "../../theme/tokens.ts";

/** A network layout: ForceAtlas2, a ring ordered by community, nested community circles, or random. */
export type NetworkLayout = "force" | "circular" | "circlepack" | "random";

/**
 * An instance of the `chart/network/v1` schema, written by rtemis.draw's
 * `NetworkConfig` class. Every member is optional. `directed`, `threshold`,
 * and `self_loops` describe the graph the host builds from its data; the
 * other members describe its layout and its look.
 * spec: draw/shared-builders#network
 */
export interface NetworkChartConfig {
  readonly type?: "network";
  readonly title?: string | null;
  readonly directed?: boolean;
  readonly threshold?: number | null;
  readonly self_loops?: boolean;
  /** Default `"force"`. */
  readonly layout?: NetworkLayout;
  /**
   * Seed of the random number generator behind community detection, the
   * force layout's starting positions, and the random layout. Default 1.
   */
  readonly seed?: number;
  /** Louvain resolution: higher values find more, smaller communities. Default 1. */
  readonly cluster_resolution?: number;
  /** Size each node by its value, the weighted degree. Default `true`. */
  readonly scale_by_degree?: boolean;
  /** Color each node by its community. Default `false`. */
  readonly color_by_group?: boolean;
  /** Color each edge as the mean of its two nodes' colors. Default `false`. */
  readonly blend_edges?: boolean;
  /** Draw node labels. Default `true`. */
  readonly show_labels?: boolean;
  /** Base node radius in screen pixels. Default 10. */
  readonly node_size?: number;
  /** Edge width at the largest weight, in pixels. Default 3. */
  readonly edge_scale?: number;
  /** Default 0.95. */
  readonly node_opacity?: number;
  /** Default 0.4. */
  readonly edge_opacity?: number;
  /** Community colors; unset uses the host's palette. */
  readonly palette?: ColumnNames | null;
  /** Node color when nodes are not colored by community; unset uses the palette's first color. */
  readonly node_color?: string | null;
  /** Color of an edge of positive sign; unset uses the host's positive color. */
  readonly positive_color?: string | null;
  /** Color of an edge of negative sign; unset uses the host's negative color. */
  readonly negative_color?: string | null;
}

/** Every member of {@link NetworkChartConfig}, checked against `chart/network/v1`. */
export const NETWORK_CHART_CONFIG_MEMBERS = Object.freeze([
  "type",
  "title",
  "directed",
  "threshold",
  "self_loops",
  "layout",
  "seed",
  "cluster_resolution",
  "scale_by_degree",
  "color_by_group",
  "blend_edges",
  "show_labels",
  "node_size",
  "edge_scale",
  "node_opacity",
  "edge_opacity",
  "palette",
  "node_color",
  "positive_color",
  "negative_color",
] as const satisfies readonly (keyof NetworkChartConfig)[]);

type Missing = Exclude<keyof NetworkChartConfig, (typeof NETWORK_CHART_CONFIG_MEMBERS)[number]>;
const _everyMember: Missing extends never ? true : never = true;
void _everyMember;

/** One node of a network. */
export interface GraphNode {
  readonly id: string;
  /** Display label; unset uses the id. */
  readonly label?: string | null;
  /** Size weight, such as the weighted degree. */
  readonly value?: number | null;
  /** Categorical key carried on the node. */
  readonly group?: string | null;
}

/** One edge of a network. */
export interface GraphEdge {
  readonly source: string;
  readonly target: string;
  /** Edge magnitude, which sets its width; unset counts as 1. */
  readonly weight?: number | null;
  /** Sign of the underlying value, such as a correlation's, which sets its color. */
  readonly sign?: 1 | -1 | null;
}

/** A network as the host builds it from its data. */
export interface GraphModel {
  readonly nodes: readonly GraphNode[];
  readonly edges: readonly GraphEdge[];
  readonly directed?: boolean | null;
}

/** The attributes {@link buildNetworkGraph} gives each node. */
export interface NetworkNodeAttributes {
  label: string;
  x: number;
  y: number;
  value: number;
  /** The node's value over the largest value, or over 1 when every value is below 1. */
  valueNorm: number;
  group: string | null;
  community: number;
}

/** The attributes {@link buildNetworkGraph} gives each edge. */
export interface NetworkEdgeAttributes {
  /** The edge's absolute weight over the largest absolute weight. */
  weightNorm: number;
  sign: number;
  /** The weight Louvain uses: `weightNorm`, or 0 for an edge of negative sign. */
  louvainWeight: number;
}

/** The members of a Graphology graph the network functions use. */
export interface NetworkGraph {
  readonly order: number;
  readonly size: number;
  addNode(node: string, attributes: NetworkNodeAttributes): unknown;
  addEdgeWithKey(
    edge: string,
    source: string,
    target: string,
    attributes: NetworkEdgeAttributes,
  ): unknown;
  hasNode(node: string): boolean;
  hasEdge(source: string, target: string): boolean;
  nodes(): string[];
  getNodeAttributes(node: string): object;
  getEdgeAttributes(edge: string): object;
  setNodeAttribute(node: string, name: "x" | "y", value: number): unknown;
  extremities(edge: string): [string, string];
  areNeighbors(node: string, neighbor: string): boolean;
}

/**
 * The Graphology constructor and algorithms the network functions run, as
 * the host loads them: `graphology`, `graphology-communities-louvain`,
 * `graphology-layout-forceatlas2`, and `circlepack` and `random` from
 * `graphology-layout`. The package ships none of them.
 */
export interface NetworkLibraries<G extends NetworkGraph = NetworkGraph> {
  readonly Graph: new (options: { type: "directed" | "undirected" }) => G;
  readonly louvain: {
    assign(
      graph: G,
      options: {
        nodeCommunityAttribute: string;
        getEdgeWeight: string;
        resolution: number;
        rng: () => number;
      },
    ): void;
  };
  readonly forceAtlas2: {
    assign(graph: G, options: { iterations: number; settings: object }): void;
    inferSettings(graph: G): object;
  };
  readonly circlepack: {
    assign(graph: G, options: { hierarchyAttributes: string[]; rng: () => number }): void;
  };
  readonly random: { assign(graph: G, options: { rng: () => number }): void };
}

/** A linear congruential generator of numbers in [0, 1) from an integer seed. */
function seededRandom(start: number): () => number {
  let seed = Math.trunc(start) >>> 0;
  return () => {
    seed = (Math.imul(1664525, seed) + 1013904223) >>> 0;
    return seed / 4294967296;
  };
}

const absWeight = (e: GraphEdge) => Math.abs(e.weight ?? 1);

/**
 * The Graphology graph of a network, laid out: nodes in model order with
 * their label, value, and normalized value; the first edge of each node
 * pair, in model order, keyed by its index, skipping an edge whose node is
 * missing, with its absolute weight over the largest among the edges kept;
 * Louvain communities at `cluster_resolution`, with edges of
 * negative sign weighted 0; and node positions from `layout`, ForceAtlas2
 * running 200 iterations from random starting positions. One generator,
 * seeded by `seed`, drives Louvain, the starting positions, and the
 * circlepack and random layouts, so the same model and config give the same
 * graph.
 * spec: draw/shared-builders#network
 */
export function buildNetworkGraph<G extends NetworkGraph>(
  model: GraphModel,
  config: NetworkChartConfig,
  libraries: NetworkLibraries<G>,
): G {
  const { Graph, louvain, forceAtlas2, circlepack, random } = libraries;
  const rng = seededRandom(config.seed ?? 1);
  const graph = new Graph({ type: model.directed ? "directed" : "undirected" });
  const nodes = model.nodes;
  const maxValue = nodes.reduce((m, n) => Math.max(m, n.value ?? 0), 1);
  nodes.forEach((node, i) => {
    const angle = (2 * Math.PI * i) / nodes.length;
    graph.addNode(node.id, {
      label: node.label ?? node.id,
      x: Math.cos(angle),
      y: Math.sin(angle),
      value: node.value ?? 0,
      valueNorm: (node.value ?? 0) / maxValue,
      group: node.group ?? null,
      community: 0,
    });
  });
  const kept: GraphEdge[] = [];
  const pairs = new Set<string>();
  for (const e of model.edges) {
    if (!graph.hasNode(e.source) || !graph.hasNode(e.target)) continue;
    const pair = JSON.stringify(
      model.directed || e.source <= e.target ? [e.source, e.target] : [e.target, e.source],
    );
    if (pairs.has(pair)) continue;
    pairs.add(pair);
    kept.push(e);
  }
  const maxWeight = kept.reduce((m, e) => Math.max(m, absWeight(e)), 1e-9);
  for (const e of kept) {
    const sign = e.sign ?? 0;
    const weightNorm = absWeight(e) / maxWeight;
    // Louvain takes nonnegative weights; an edge of negative sign weighs 0 so
    // that it does not pull its nodes into one community.
    graph.addEdgeWithKey(String(graph.size), e.source, e.target, {
      weightNorm,
      sign,
      louvainWeight: sign < 0 ? 0 : weightNorm,
    });
  }
  if (graph.size > 0) {
    louvain.assign(graph, {
      nodeCommunityAttribute: "community",
      getEdgeWeight: "louvainWeight",
      resolution: config.cluster_resolution ?? 1,
      rng,
    });
  }
  const layout = config.layout ?? "force";
  if (layout === "circular") {
    const community = (id: string) => nodeAttributes(graph, id).community;
    const ids = graph.nodes().sort((a, b) => community(a) - community(b));
    ids.forEach((id, i) => {
      const angle = (2 * Math.PI * i) / ids.length;
      graph.setNodeAttribute(id, "x", Math.cos(angle));
      graph.setNodeAttribute(id, "y", Math.sin(angle));
    });
  } else if (layout === "circlepack") {
    circlepack.assign(graph, { hierarchyAttributes: ["community"], rng });
  } else if (layout === "random") {
    random.assign(graph, { rng });
  } else if (graph.order > 0) {
    // ForceAtlas2 is deterministic; the seed sets where it starts.
    for (const id of graph.nodes()) {
      graph.setNodeAttribute(id, "x", 2 * rng() - 1);
      graph.setNodeAttribute(id, "y", 2 * rng() - 1);
    }
    forceAtlas2.assign(graph, {
      iterations: 200,
      settings: { ...forceAtlas2.inferSettings(graph), scalingRatio: 10 },
    });
  }
  return graph;
}

const nodeAttributes = (graph: NetworkGraph, id: string) =>
  graph.getNodeAttributes(id) as NetworkNodeAttributes;
const edgeAttributes = (graph: NetworkGraph, id: string) =>
  graph.getEdgeAttributes(id) as NetworkEdgeAttributes;

/** Theme values the network reducers need. */
export interface NetworkAppearance {
  /** The host's categorical palette; unset uses rtemis' palette. */
  readonly palette?: readonly string[];
  /** Colors of edges of positive and negative sign; unset uses the sign colors. */
  readonly positive_color?: string;
  readonly negative_color?: string;
  /** Label color; unset follows the scheme. */
  readonly label_color?: string;
  readonly font_family?: string;
  /** Default `"light"`. */
  readonly scheme?: "light" | "dark";
}

/** Alpha channel, of 255, of a node outside the hovered node's neighborhood: 30% of its color. */
const FADED_ALPHA = 77;

/** Color of an edge of neither sign. */
export const NEUTRAL_EDGE_COLOR = "#9aa0a6";

const HEX6 = /^#[0-9a-f]{6}$/i;

/** A `#rrggbb` color with an alpha channel of 0 to 255; another color unchanged. */
function withAlpha(color: string, alpha: number): string {
  if (!HEX6.test(color)) return color;
  const a = Math.max(0, Math.min(255, Math.round(alpha)));
  return `${color}${a.toString(16).padStart(2, "0")}`;
}

/** The channel-wise mean of two `#rrggbb` colors; the first color when either is not `#rrggbb`. */
function blendHex(a: string, b: string): string {
  if (!HEX6.test(a) || !HEX6.test(b)) return a;
  const pa = Number.parseInt(a.slice(1), 16);
  const pb = Number.parseInt(b.slice(1), 16);
  const r = ((pa >> 16) + (pb >> 16)) >> 1;
  const g = (((pa >> 8) & 0xff) + ((pb >> 8) & 0xff)) >> 1;
  const bl = ((pa & 0xff) + (pb & 0xff)) >> 1;
  return `#${((1 << 24) | (r << 16) | (g << 8) | bl).toString(16).slice(1)}`;
}

/** Sigma node and edge reducers: the display attributes of each node and edge. */
export interface NetworkReducers {
  nodeReducer(node: string, data: object): Record<string, unknown>;
  edgeReducer(edge: string, data: object): Record<string, unknown>;
}

/**
 * The display attributes of a laid-out network, as Sigma reducers that the
 * SVG export also calls. A node's radius is `node_size`, scaled by
 * 0.5 plus its normalized value when `scale_by_degree` is set; its color is
 * its community's palette color or `node_color`; its label faces the
 * graph's center. An edge's width is its normalized weight times
 * `edge_scale`, at least 0.5 pixels; its color is the mean of its nodes'
 * colors when `blend_edges` is set, else its sign's color. While `hovered()`
 * names a node, the node shows its label on a backdrop, every node outside
 * its neighborhood keeps 30% of its color without a label, and every edge
 * not touching it fades.
 * spec: draw/shared-builders#network
 */
export function networkReducers(
  graph: NetworkGraph,
  config: NetworkChartConfig,
  appearance: NetworkAppearance = {},
  hovered: () => string | null = () => null,
): NetworkReducers {
  const memberPalette = asArray(config.palette);
  const palette =
    memberPalette.length > 0
      ? memberPalette
      : appearance.palette && appearance.palette.length > 0
        ? appearance.palette
        : RTEMIS_PALETTE;
  const nodeColor = config.node_color ?? (palette[0] as string);
  const byGroup = config.color_by_group ?? false;
  const colorOf = (community: number) =>
    byGroup
      ? (palette[((community % palette.length) + palette.length) % palette.length] as string)
      : nodeColor;
  const positive = config.positive_color ?? appearance.positive_color ?? SIGN_COLORS.positive;
  const negative = config.negative_color ?? appearance.negative_color ?? SIGN_COLORS.negative;
  const dark = appearance.scheme === "dark";
  const labelColor = appearance.label_color ?? (dark ? "#e6e6e6" : "#1a1a1a");
  const nodeSize = config.node_size ?? 10;
  const byDegree = config.scale_by_degree ?? true;
  const nodeOpacity = config.node_opacity ?? 0.95;
  const edgeScale = config.edge_scale ?? 3;
  const edgeOpacity = config.edge_opacity ?? 0.4;
  let minX = Number.POSITIVE_INFINITY;
  let maxX = Number.NEGATIVE_INFINITY;
  for (const id of graph.nodes()) {
    const x = nodeAttributes(graph, id).x;
    minX = Math.min(minX, x);
    maxX = Math.max(maxX, x);
  }
  const centerX = graph.order > 0 ? (minX + maxX) / 2 : 0;
  return {
    nodeReducer(node, data) {
      const attrs = nodeAttributes(graph, node);
      const out: Record<string, unknown> = {
        ...data,
        size: byDegree ? Math.max(1, nodeSize * (0.5 + (attrs.valueNorm || 0))) : nodeSize,
        color: colorOf(attrs.community || 0),
        opacity: nodeOpacity,
        labelPosition: attrs.x > centerX ? "left" : "right",
        labelColor,
        ...(appearance.font_family ? { labelFont: appearance.font_family } : {}),
        backdropVisibility: "hidden",
      };
      const over = hovered();
      if (over === node) {
        out.backdropVisibility = "visible";
        out.backdropColor = dark ? "#505050" : "#cbcbcb";
        out.backdropShadowColor = "transparent";
        out.backdropShadowBlur = 0;
      } else if (over && !graph.areNeighbors(over, node)) {
        out.color = withAlpha(out.color as string, FADED_ALPHA);
        out.opacity = FADED_ALPHA / 255;
        out.label = "";
      }
      return out;
    },
    edgeReducer(edge, data) {
      const attrs = edgeAttributes(graph, edge);
      const [source, target] = graph.extremities(edge);
      const color = config.blend_edges
        ? blendHex(
            colorOf(nodeAttributes(graph, source).community),
            colorOf(nodeAttributes(graph, target).community),
          )
        : attrs.sign > 0
          ? positive
          : attrs.sign < 0
            ? negative
            : NEUTRAL_EDGE_COLOR;
      const over = hovered();
      return {
        ...data,
        size: Math.max(0.5, (attrs.weightNorm ?? 0.5) * edgeScale),
        color,
        opacity:
          over && source !== over && target !== over ? Math.min(edgeOpacity, 0.06) : edgeOpacity,
      };
    },
  };
}
