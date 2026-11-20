// rtemis-graph htmlwidget binding.
//
// Sigma.js renderer for the network chart. The payload carries a graph model
// ({ nodes, edges, directed }) and its chart/network/v1 config. @rtemis/draw
// (window.RtemisDraw) builds the laid-out Graphology graph and the node and
// edge reducers, which rtemislive's GraphCanvas also uses; this binding owns
// the Sigma instance, the hover tooltip, the title, and theme changes.
// spec: draw/shared-builders#network
//
// graphology, its algorithms, and sigma come from the vendored bundle
// (window.RtemisGraph), built from r/tools/graph-bundle.

HTMLWidgets.widget({
  name: "rtemis-graph",
  type: "output",

  factory: (el, width, height) => {
    let sigma = null;
    let resizeObserver = null;

    const { Graph, Sigma } =
      window.RtemisGraph || {};

    // The payload last rendered, and the color scheme it was rendered in, so a
    // theme change can re-render it (see the watcher below).
    let currentPayload = null;
    let renderedDark = null;

    // The active theme document (light or dark by the page when automatic) and
    // the background, label color, font, and scheme it states. Dark-mode
    // detection is shared with the other bindings (lib/draw/theme_watch.js).
    const resolveTheme = (x) => {
      renderedDark = window.RtemisThemeWatch.isDark();
      const doc = x.autoTheme && renderedDark ? x.themeDark : x.theme;
      return { doc, ...window.RtemisVectorTheme.resolve(doc) };
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

    // `camera` restores the viewer's pan and zoom when re-rendering the same
    // graph in a new theme; a new payload starts from the default view.
    const renderGraph = (x, camera = null) => {
      currentPayload = x;
      if (!Graph || !Sigma || !window.RtemisDraw) {
        surface.innerHTML =
          '<div style="padding:1rem;color:#b00">rtemis-graph bundles failed to load (window.RtemisGraph or window.RtemisDraw missing).</div>';
        return;
      }
      if (sigma) {
        sigma.kill();
        sigma = null;
      }

      const model = x.model || {};
      const config = x.config || {};
      const nodes = model.nodes || [];
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

      const graph = window.RtemisDraw.buildNetworkGraph(
        { ...model, edges: model.edges || [], nodes },
        config,
        window.RtemisGraph
      );
      let hovered = null;
      const reducers = window.RtemisDraw.networkReducers(
        graph,
        config,
        // The reducers' appearance from the theme document (palette, sign
        // colors, label color, font, scheme); without one, neutral defaults.
        theme.doc
          ? window.RtemisDraw.networkTheme(theme.doc)
          : {
              label_color: theme.fg,
              font_family: theme.fontFamily,
              scheme: "light",
            },
        () => hovered
      );
      const colorByGroup = !!config.color_by_group;

      sigma = new Sigma(graph, surface, {
        // Sigma v4 nests renderer settings under `settings`; passing them flat
        // silently drops them (e.g. itemSizesReference defaulted to "positions",
        // sizing nodes in unit-circle layout coords -> giant nodes).
        settings: {
          allowInvalidContainer: true,
          renderLabels: config.show_labels !== false,
          renderEdgeLabels: false,
          enableEdgeEvents: false,
          // Node size N means N screen pixels regardless of layout coordinate
          // scale (without this, unit-circle layouts render giant nodes).
          itemSizesReference: "screen",
        },
        nodeReducer: reducers.nodeReducer,
        edgeReducer: reducers.edgeReducer,
      });
      if (camera) sigma.getCamera().setState(camera);

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
          window.RtemisVectorTheme.escape(label) +
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

    // Re-render in the new theme when the host switches between light and
    // dark, keeping the current pan and zoom. The layout is deterministic, so
    // nodes stay where they were.
    window.RtemisThemeWatch.watch(el, () => {
      if (!currentPayload?.autoTheme) return;
      if (window.RtemisThemeWatch.isDark() === renderedDark) return;
      const camera = sigma ? sigma.getCamera().getState() : null;
      renderGraph(currentPayload, camera);
    }, () => {
      if (resizeObserver) resizeObserver.disconnect();
      if (sigma) sigma.kill();
      sigma = null;
    });

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
