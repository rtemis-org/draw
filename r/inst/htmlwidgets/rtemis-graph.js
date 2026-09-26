// rtemis-graph htmlwidget binding.
//
// Sigma.js renderer for the network / graph plot type. This is the first
// non-ECharts rendering surface in rtemis.draw; it consumes a renderer-agnostic
// graph model ({ nodes, edges, directed }) — not an EChartsOption — and manages
// the sigma instance lifecycle. It is a vanilla-JS port of rtemislive's
// GraphCanvas.tsx: same
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

    const { Graph, Sigma } =
      window.RtemisGraph || {};

    // Detect dark mode from VS Code, RStudio, Quarto, or browser preference.
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
      if (body.classList.contains("quarto-dark")) return true;
      if (body.classList.contains("quarto-light")) return false;
      if (window.matchMedia) {
        return window.matchMedia("(prefers-color-scheme: dark)").matches;
      }
      return false;
    };

    // Resolve the active theme list (echarts-shaped) -> the few colors sigma
    // needs: page background, label color, font family.
    const resolveTheme = (x) => window.RtemisVectorTheme.resolve(
      x.autoTheme && isDarkMode() ? x.themeDark : x.theme
    );

    // ── small color helpers (ported from GraphCanvas) ──────────────────────
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
      el.style.fontFamily = theme.fontFamily;

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

      const scene = window.RtemisGraphScene.create(model, s, theme);
      const graph = scene.graph;
      const colorByGroup = !!s.colorByGroup;

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
        nodeReducer: scene.nodeReducer,
        edgeReducer: scene.edgeReducer,
      });

      // Hover: dim the rest, show a themed tooltip with node name + details.
      sigma.on("enterNode", ({ node }) => {
        scene.setHovered(node);
        const label = graph.getNodeAttribute(node, "label") || node;
        const degree = graph.degree(node);
        const community = graph.getNodeAttribute(node, "community");
        tooltip.style.color = theme.fg;
        tooltip.style.backgroundColor = theme.dark
          ? "rgba(40,40,40,0.85)"
          : "rgba(255,255,255,0.85)";
        tooltip.innerHTML =
          '<div style="font-weight:500">' +
          window.RtemisVectorTheme.escape(label) +
          '</div><div style="opacity:0.7">degree ' +
          degree +
          (colorByGroup ? " &middot; community " + community : "") +
          "</div>";
        tooltip.style.display = "block";
        sigma.refresh();
      });
      sigma.on("leaveNode", () => {
        scene.setHovered(null);
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
      getRenderer: () => sigma,
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
