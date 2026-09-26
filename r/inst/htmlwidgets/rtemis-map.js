// rtemis-map htmlwidget binding.
//
// MapLibre GL renderer for the choropleth map plot type. This is the third
// rendering surface in rtemis.draw (after ECharts and Sigma.js); it consumes a
// renderer-agnostic map model ({ rows, resolution, valueLabel, tooltipFields })
// plus an embedded TopoJSON geometry -- not an EChartsOption -- and manages the
// MapLibre instance lifecycle.
//
// It is a vanilla-JS port of rtemislive's MapCanvas.tsx
// together with the scale
// (choroplethScale.ts) and location-resolver (locationResolver.ts) logic that
// in rtemislive live in separate modules. No basemap tiles: admin boundaries
// render on the themed app background, exactly like rtemislive.
//
// maplibregl + topojson-client + d3-scale-chromatic come from the vendored
// bundle (window.RtemisMap), built from r/tools/map-bundle. Styling options
// arrive once on the payload (no live sliders, unlike the React app) under
// payload.style.

HTMLWidgets.widget({
  name: "rtemis-map",
  type: "output",

  factory: (el, width, height) => {
    let map = null;
    let resizeObserver = null;

    const { maplibregl, topojsonFeature, chromatic } = window.RtemisMap || {};

    const SOURCE_ID = "regions";
    const FILL_LAYER = "regions-fill";
    const LINE_LAYER = "regions-outline";
    const BG_LAYER = "bg";

    // Theme colors (mirror MapCanvas.tsx). Used when the rtemis theme does not
    // supply an explicit background.
    const OUTLINE_LIGHT = "#cbd5e1"; // slate-300
    const OUTLINE_DARK = "#3f3f46"; // zinc-700

    // ── Dark-mode detection (identical to rtemis-graph.js / rtemis-draw.js) ──
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

    // Resolve the active theme (echarts-shaped list) -> the few colors the map
    // needs. For an explicit theme we infer dark/light from the background
    // luminance; for auto mode we detect it from the host.
    const resolveTheme = (x) => window.RtemisVectorTheme.resolve(
      x.autoTheme && isDarkMode() ? x.themeDark : x.theme
    );

    const {normalizeKey, fillColorExpression} = window.RtemisMapScene;

    // ── Overlays ─────────────────────────────────────────────────────────────
    el.style.position = "relative";

    const container = document.createElement("div");
    container.style.cssText = "position:absolute;inset:0;";
    el.appendChild(container);

    // Corner anchor -> inline CSS for an overlay box.
    const cornerStyle = (corner) => {
      const c = corner || "bottom-right";
      const v = c.indexOf("top") === 0 ? "top:12px;" : "bottom:12px;";
      const h = c.indexOf("left") >= 0 ? "left:12px;" : "right:12px;";
      return v + h;
    };

    const makeOverlay = () => {
      const d = document.createElement("div");
      d.style.cssText =
        "position:absolute;z-index:20;border-radius:6px;padding:6px 10px;" +
        "font-size:12px;line-height:1.4;backdrop-filter:blur(4px);display:none;";
      el.appendChild(d);
      return d;
    };
    const legendEl = makeOverlay();
    const tooltipEl = makeOverlay();
    tooltipEl.style.pointerEvents = "none";
    const titleEl = makeOverlay();
    titleEl.style.cssText += "left:12px;top:10px;font-size:14px;font-weight:600;padding:0;pointer-events:none;";

    const overlayColors = (theme) => ({
      bg: theme.dark ? "rgba(40,40,40,0.85)" : "rgba(255,255,255,0.85)",
      fg: theme.fg,
      muted: theme.dark ? "rgba(230,230,230,0.7)" : "rgba(26,26,26,0.65)",
    });

    const escapeHtml = (s) =>
      String(s).replace(/[&<>"]/g, (c) => ({
        "&": "&amp;",
        "<": "&lt;",
        ">": "&gt;",
        '"': "&quot;",
      })[c]);

    // ── Render ────────────────────────────────────────────────────────────────
    let valueById = new Map();
    let extrasById = new Map();

    const renderMap = (x) => {
      if (!maplibregl || !topojsonFeature || !chromatic) {
        container.innerHTML =
          '<div style="padding:1rem;color:#b00">rtemis-map bundle failed to load (window.RtemisMap missing).</div>';
        return;
      }
      if (map) {
        map.remove();
        map = null;
      }
      legendEl.style.display = "none";
      tooltipEl.style.display = "none";
      titleEl.style.display = "none";

      const model = x.model || {};
      const s = x.style || {};
      const geo = x.geo || {};
      const rows = model.rows || [];
      const resolution = model.resolution || "country";
      const theme = resolveTheme(x);
      const oc = overlayColors(theme);

      el.style.backgroundColor = theme.bg;
      el.style.fontFamily = theme.fontFamily;

      let scene;
      try { scene = window.RtemisMapScene.create(x, theme.dark); }
      catch (err) { container.textContent = "rtemis-map: failed to parse geometry."; throw err; }
      const fc = scene.fc, idSet = scene.ids;


      map = new maplibregl.Map({
        container: container,
        center: geo.center || [0, 20],
        zoom: geo.zoom != null ? geo.zoom : 0.4,
        attributionControl: false,
        dragRotate: false,
        canvasContextAttributes: { preserveDrawingBuffer: true },
        style: {
          version: 8,
          sources: {},
          layers: [
            {
              id: BG_LAYER,
              type: "background",
              paint: { "background-color": theme.bg },
            },
          ],
        },
      });

      map.on("load", () => {
        map.addSource(SOURCE_ID, {
          type: "geojson",
          data: fc,
          promoteId: "joinId",
        });
        map.addLayer({
          id: FILL_LAYER,
          type: "fill",
          source: SOURCE_ID,
          paint: { "fill-color": "#ccc", "fill-opacity": s.opacity != null ? s.opacity : 1 },
        });
        map.addLayer({
          id: LINE_LAYER,
          type: "line",
          source: SOURCE_ID,
          paint: {
            "line-color": theme.dark ? OUTLINE_DARK : OUTLINE_LIGHT,
            "line-width": s.outlineWidth != null ? s.outlineWidth : 0.2,
          },
        });

        const scale = scene.scale;
        map.removeFeatureState({ source: SOURCE_ID });
        valueById = scene.values;
        extrasById = new Map();
        const tooltipFields = model.tooltipFields || [];
        let matched = 0;
        let unmatched = 0;
        const unmatchedKeys = [];
        for (const row of rows) {
          const id = normalizeKey(row.location, resolution);
          if (id && idSet.has(id)) {
            valueById.set(id, row.value);
            if (tooltipFields.length > 0 && row.extras) {
              extrasById.set(
                id,
                tooltipFields
                  .filter((f) => row.extras[f] != null)
                  .map((f) => {
                    const v = row.extras[f];
                    return {
                      label: f,
                      value:
                        typeof v === "number" ? v.toLocaleString() : String(v),
                    };
                  }),
              );
            }
            matched++;
          } else {
            unmatched++;
            if (unmatchedKeys.length < 12) unmatchedKeys.push(row.location);
          }
        }
        for (const [id, v] of valueById) {
          map.setFeatureState({ source: SOURCE_ID, id: id }, { v: v });
        }

        // Paint.
        map.setPaintProperty(FILL_LAYER, "fill-color", fillColorExpression(scale));
        map.setPaintProperty(FILL_LAYER, "fill-opacity", s.opacity != null ? s.opacity : 1);
        map.setLayoutProperty(
          LINE_LAYER,
          "visibility",
          s.showBoundaries === false ? "none" : "visible",
        );

        renderLegend(x, scale, oc);
        // Processing diagnostics belong in the console, not on the figure.
        if (unmatched) console.warn("rtemis-map: unmatched locations:", unmatchedKeys);
        if (x.title) { titleEl.textContent = x.title; titleEl.style.color = theme.fg; titleEl.style.display = "block"; }
      });

      // Hover tooltip.
      map.on("mousemove", FILL_LAYER, (e) => {
        const f = e.features && e.features[0];
        if (!f) return;
        map.getCanvas().style.cursor = "pointer";
        const id = String((f.properties && f.properties.joinId) || "");
        const name = String((f.properties && f.properties.name) || id);
        const value = valueById.has(id) ? valueById.get(id) : null;
        const extras = extrasById.get(id) || [];
        let html = '<div style="font-weight:500">' + escapeHtml(name) + "</div>";
        const vlabel = model.valueLabel
          ? '<span style="color:' + oc.muted + '">' + escapeHtml(model.valueLabel) + ": </span>"
          : "";
        html +=
          '<div style="color:' +
          oc.muted +
          '">' +
          vlabel +
          '<span style="font-variant-numeric:tabular-nums;color:' +
          oc.fg +
          '">' +
          (value == null ? "no data" : value.toLocaleString()) +
          "</span></div>";
        for (const ex of extras) {
          html +=
            '<div style="color:' +
            oc.muted +
            '">' +
            escapeHtml(ex.label) +
            ': <span style="color:' +
            oc.fg +
            '">' +
            escapeHtml(ex.value) +
            "</span></div>";
        }
        tooltipEl.style.cssText =
          tooltipEl.style.cssText.replace(/top:[^;]*;|bottom:[^;]*;|left:[^;]*;|right:[^;]*;/g, "") +
          cornerStyle(s.tooltipPosition || "top-right");
        tooltipEl.style.backgroundColor = oc.bg;
        tooltipEl.style.color = oc.fg;
        tooltipEl.innerHTML = html;
        tooltipEl.style.display = "block";
      });
      map.on("mouseleave", FILL_LAYER, () => {
        map.getCanvas().style.cursor = "";
        tooltipEl.style.display = "none";
      });

      if (resizeObserver) resizeObserver.disconnect();
      resizeObserver = new ResizeObserver(() => {
        if (map) map.resize();
      });
      resizeObserver.observe(container);
    };

    const renderLegend = (x, scale, oc) => {
      const s = x.style || {};
      const model = x.model || {};
      if (s.showLegend === false || !scale.legend || scale.legend.length === 0) {
        legendEl.style.display = "none";
        return;
      }
      let html = "";
      if (model.valueLabel) {
        html +=
          '<div style="font-weight:500;margin-bottom:4px">' +
          escapeHtml(model.valueLabel) +
          "</div>";
      }
      const swatch = (color) =>
        '<span style="display:inline-block;height:12px;width:12px;border-radius:2px;background:' +
        color +
        '"></span>';
      for (const entry of scale.legend) {
        html +=
          '<div style="display:flex;align-items:center;gap:8px">' +
          swatch(entry.color) +
          '<span style="font-variant-numeric:tabular-nums;color:' +
          oc.muted +
          '">' +
          escapeHtml(entry.label) +
          "</span></div>";
      }
      html +=
        '<div style="display:flex;align-items:center;gap:8px;margin-top:4px;color:' +
        oc.muted +
        '">' +
        swatch(scale.missingColor) +
        "<span>No data</span></div>";
      legendEl.style.cssText =
        legendEl.style.cssText.replace(/top:[^;]*;|bottom:[^;]*;|left:[^;]*;|right:[^;]*;/g, "") +
        cornerStyle(s.legendPosition || "bottom-right");
      legendEl.style.backgroundColor = oc.bg;
      legendEl.style.color = oc.fg;
      legendEl.innerHTML = html;
      legendEl.style.display = "block";
    };

    return {
      getRenderer: () => map,
      renderValue: (x) => renderMap(x),
      resize: () => {
        if (map) map.resize();
      },
    };
  },
});
