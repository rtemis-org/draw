// rtemis-graph htmlwidget binding.
//
// Sigma.js renderer for the network / graph plot type. This is the first
// non-ECharts rendering surface in rtemis.draw; it consumes a renderer-agnostic
// graph model ({ nodes, edges, directed }) — not an EChartsOption — and manages
// the sigma instance lifecycle. It is a vanilla-JS port of rtemislive's
// GraphCanvas.tsx (~/Code/live/src/components/chart/GraphCanvas.tsx): same
// graphology build, Louvain community detection, ForceAtlas2 / circular /
// circlepack / random layouts, and node/edge reducers.
//
// graphology + sigma come from the vendored bundle (window.RtemisGraph), built
// from r/tools/graph-bundle. Styling options arrive once on the payload (no live
// sliders, unlike the React app) under payload.style.

HTMLWidgets.widget({
  name: "rtemis-graph",
  type: "output",

  factory: (el, width, height) => {
    let sigma = null;
    let resizeObserver = null;

    const { Graph, Sigma, louvain, forceAtlas2, circlepack, random } =
      window.RtemisGraph || {};

    // Detect dark mode from VS Code, RStudio, or browser preference.
    // (Mirrors rtemis-draw.js so both renderers theme consistently.)
    const isDarkMode = () => {
      const body = document.body;
      if (
        body.classList.contains("vscode-dark") ||
        body.classList.contains("vscode-high-contrast")
      ) {
        return true;
      }
      if (body.classList.contains("vscode-light")) return false;
      if (body.classList.contains("rstudio-themes-dark-menus")) return true;
      if (window.matchMedia) {
        return window.matchMedia("(prefers-color-scheme: dark)").matches;
      }
      return false;
    };

    // Resolve the active theme list (echarts-shaped) -> the few colors sigma
    // needs: page background, label color, font family.
    const resolveTheme = (x) => {
      let theme = null;
      if (x.autoTheme) {
        theme = isDarkMode() ? x.themeDark : x.theme;
      } else if (x.theme) {
        theme = x.theme;
      }
      const dark = x.autoTheme ? isDarkMode() : false;
      const bg = (theme && theme.backgroundColor) || (dark ? "#1a1a1a" : "#ffffff");
      const fg =
        (theme && theme.textStyle && theme.textStyle.color) ||
        (dark ? "#e6e6e6" : "#1a1a1a");
      const fontFamily =
        theme && theme.textStyle && theme.textStyle.fontFamily;
      return { bg, fg, fontFamily, dark };
    };

    // ── small color helpers (ported from GraphCanvas) ──────────────────────
    const paletteColor = (palette, i) => {
      const n = palette.length;
      return palette[((i % n) + n) % n];
    };
    const NEUTRAL_EDGE = "#9aa0a6";
    const withAlpha = (hex, alpha255) => {
      const a = Math.max(0, Math.min(255, Math.round(alpha255)));
      return `${hex}${a.toString(16).padStart(2, "0")}`;
    };
    const blendHex = (a, b) => {
      const pa = parseInt(a.slice(1), 16);
      const pb = parseInt(b.slice(1), 16);
      const r = ((pa >> 16) + (pb >> 16)) >> 1;
      const g = (((pa >> 8) & 0xff) + ((pb >> 8) & 0xff)) >> 1;
      const bl = ((pa & 0xff) + (pb & 0xff)) >> 1;
      return `#${((1 << 24) | (r << 16) | (g << 8) | bl).toString(16).slice(1)}`;
    };

    // Containers: a graph surface plus a hover tooltip + optional title overlay,
    // pinned absolutely so they never displace the canvas.
    el.style.position = "relative";
    const surface = document.createElement("div");
    surface.style.position = "absolute";
    surface.style.inset = "0";
    el.appendChild(surface);

    const tooltip = document.createElement("div");
    tooltip.style.cssText =
      "position:absolute;right:12px;top:12px;z-index:20;pointer-events:none;" +
      "border-radius:6px;padding:6px 10px;font-size:12px;display:none;" +
      "backdrop-filter:blur(4px);";
    el.appendChild(tooltip);

    const titleEl = document.createElement("div");
    titleEl.style.cssText =
      "position:absolute;left:12px;top:10px;z-index:20;pointer-events:none;" +
      "font-weight:600;font-size:14px;display:none;";
    el.appendChild(titleEl);

    const renderGraph = (x) => {
      if (!Graph || !Sigma) {
        surface.innerHTML =
          '<div style="padding:1rem;color:#b00">rtemis-graph bundle failed to load (window.RtemisGraph missing).</div>';
        return;
      }
      if (sigma) {
        sigma.kill();
        sigma = null;
      }

      const model = x.model || {};
      const s = x.style || {};
      const nodes = model.nodes || [];
      const edges = model.edges || [];
      const palette = s.palette || ["#6CA3A0"];
      const theme = resolveTheme(x);

      el.style.backgroundColor = theme.bg;

      // Title
      if (x.title) {
        titleEl.textContent = x.title;
        titleEl.style.color = theme.fg;
        titleEl.style.display = "block";
      } else {
        titleEl.style.display = "none";
      }

      if (nodes.length === 0) {
        surface.innerHTML =
          '<div style="position:absolute;inset:0;display:flex;align-items:center;' +
          'justify-content:center;color:' +
          theme.fg +
          ';opacity:0.6">No nodes to display.</div>';
        return;
      }
      surface.innerHTML = "";

      const graph = new Graph({ type: model.directed ? "directed" : "undirected" });

      const maxValue = nodes.reduce((m, n) => Math.max(m, n.value || 0), 1);
      const nn = nodes.length;
      nodes.forEach((node, i) => {
        const angle = (2 * Math.PI * i) / nn;
        graph.addNode(node.id, {
          label: node.label != null ? node.label : node.id,
          x: Math.cos(angle),
          y: Math.sin(angle),
          value: node.value || 0,
          valueNorm: (node.value || 0) / maxValue,
          group: node.group != null ? node.group : null,
          community: 0,
        });
      });

      const maxWeight = edges.reduce(
        (m, e) => Math.max(m, Math.abs(e.weight != null ? e.weight : 1)),
        1e-9,
      );
      for (const e of edges) {
        if (!graph.hasNode(e.source) || !graph.hasNode(e.target)) continue;
        if (graph.hasEdge(e.source, e.target)) continue;
        const sign = e.sign != null ? e.sign : 0;
        const weightNorm = Math.abs(e.weight != null ? e.weight : 1) / maxWeight;
        graph.addEdge(e.source, e.target, {
          weightNorm,
          sign,
          // Louvain only supports non-negative weights; clamp negatives to 0 so
          // anti-correlated nodes are not pulled together (same compromise as
          // rtemislive's GraphCanvas).
          louvainWeight: sign < 0 ? 0 : weightNorm,
        });
      }

      // Community detection (always computed; coloring decides whether to use).
      if (graph.size > 0 && louvain) {
        louvain.assign(graph, {
          nodeCommunityAttribute: "community",
          getEdgeWeight: "louvainWeight",
          resolution: s.resolution != null ? s.resolution : 1,
        });
      }

      // ── Layout ──────────────────────────────────────────────────────────
      const layout = s.layout || "force";
      if (layout === "circular") {
        const ids = graph.nodes();
        ids.sort(
          (a, b) =>
            graph.getNodeAttribute(a, "community") -
            graph.getNodeAttribute(b, "community"),
        );
        ids.forEach((id, i) => {
          const angle = (2 * Math.PI * i) / ids.length;
          graph.setNodeAttribute(id, "x", Math.cos(angle));
          graph.setNodeAttribute(id, "y", Math.sin(angle));
        });
      } else if (layout === "circlepack" && circlepack) {
        circlepack.assign(graph, { hierarchyAttributes: ["community"] });
      } else if (layout === "random" && random) {
        random.assign(graph);
      } else if (graph.order > 0 && forceAtlas2) {
        forceAtlas2.assign(graph, {
          iterations: 200,
          settings: { ...forceAtlas2.inferSettings(graph), scalingRatio: 10 },
        });
      }

      const colorByGroup = !!s.colorByGroup;
      const nodeColorFor = (community) =>
        colorByGroup ? paletteColor(palette, community) : s.nodeColor || palette[0];

      let hovered = null;

      sigma = new Sigma(graph, surface, {
        // Sigma v4 nests renderer settings under `settings`; passing them flat
        // silently drops them (e.g. itemSizesReference defaulted to "positions",
        // sizing nodes in unit-circle layout coords -> giant nodes).
        settings: {
          allowInvalidContainer: true,
          renderLabels: s.showLabels !== false,
          renderEdgeLabels: false,
          enableEdgeEvents: false,
          // Node size N means N screen pixels regardless of layout coordinate
          // scale (without this, unit-circle layouts render giant nodes).
          itemSizesReference: "screen",
        },
        nodeReducer: (key, data) => {
          const attrs = graph.getNodeAttributes(key);
          const base = s.nodeSize != null ? s.nodeSize : 10;
          const size =
            s.scaleByDegree !== false
              ? Math.max(1, base * (0.5 + (attrs.valueNorm || 0)))
              : base;
          const res = {
            ...data,
            size,
            color: nodeColorFor(attrs.community || 0),
            opacity: s.nodeOpacity != null ? s.nodeOpacity : 0.95,
            labelColor: theme.fg,
            labelFont: theme.fontFamily,
          };
          if (hovered && hovered !== key && !graph.areNeighbors(hovered, key)) {
            res.color = withAlpha(palette[0], 40);
            res.opacity = 0.15;
            res.label = "";
          }
          return res;
        },
        edgeReducer: (key, data) => {
          const attrs = graph.getEdgeAttributes(key);
          const size = Math.max(
            0.5,
            (attrs.weightNorm != null ? attrs.weightNorm : 0.5) *
              (s.edgeScale != null ? s.edgeScale : 3),
          );
          let color;
          if (s.blendEdges) {
            const ext = graph.extremities(key);
            color = blendHex(
              nodeColorFor(graph.getNodeAttribute(ext[0], "community")),
              nodeColorFor(graph.getNodeAttribute(ext[1], "community")),
            );
          } else {
            const sign = attrs.sign;
            color =
              sign > 0
                ? s.positiveColor || palette[0]
                : sign < 0
                  ? s.negativeColor || NEUTRAL_EDGE
                  : NEUTRAL_EDGE;
          }
          const res = {
            ...data,
            size,
            color,
            opacity: s.edgeOpacity != null ? s.edgeOpacity : 0.4,
          };
          if (hovered && !graph.extremities(key).includes(hovered)) {
            res.opacity = Math.min(
              s.edgeOpacity != null ? s.edgeOpacity : 0.4,
              0.06,
            );
          }
          return res;
        },
      });

      // Hover: dim the rest, show a themed tooltip with node name + details.
      sigma.on("enterNode", ({ node }) => {
        hovered = node;
        const label = graph.getNodeAttribute(node, "label") || node;
        const degree = graph.degree(node);
        const community = graph.getNodeAttribute(node, "community");
        tooltip.style.color = theme.fg;
        tooltip.style.backgroundColor = theme.dark
          ? "rgba(40,40,40,0.85)"
          : "rgba(255,255,255,0.85)";
        tooltip.innerHTML =
          '<div style="font-weight:500">' +
          label +
          '</div><div style="opacity:0.7">degree ' +
          degree +
          (colorByGroup ? " &middot; community " + community : "") +
          "</div>";
        tooltip.style.display = "block";
        sigma.refresh();
      });
      sigma.on("leaveNode", () => {
        hovered = null;
        tooltip.style.display = "none";
        sigma.refresh();
      });

      // resize() resizes (and clears) the WebGL canvas but does not redraw it,
      // so schedule a render explicitly after each resize.
      if (resizeObserver) resizeObserver.disconnect();
      resizeObserver = new ResizeObserver(() => {
        if (sigma) {
          sigma.resize();
          sigma.scheduleRender();
        }
      });
      resizeObserver.observe(surface);
    };

    return {
      renderValue: (x) => renderGraph(x),
      resize: (newWidth, newHeight) => {
        if (sigma) {
          sigma.resize();
          sigma.scheduleRender();
        }
      },
    };
  },
});
