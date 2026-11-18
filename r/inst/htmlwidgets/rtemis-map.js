// rtemis-map htmlwidget binding.
//
// MapLibre GL renderer for the choropleth map. The payload carries a map model
// ({ rows, resolution, valueLabel, tooltipFields }), its chart/choropleth/v1
// config, and the TopoJSON geometry of its resolution. @rtemis/draw
// (window.RtemisDraw) decodes the regions, joins the rows, and builds the
// classes and the fill expression, as for rtemislive's MapCanvas; this binding
// owns the MapLibre instance, the legend, tooltip, and title overlays, and
// theme changes. No basemap tiles: boundaries render on the theme background.
// spec: draw/shared-builders#choropleth
//
// maplibregl, topojson-client, and d3-scale-chromatic come from the vendored
// bundle (window.RtemisMap), built from r/tools/map-bundle.

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


    // The payload last rendered, and the color scheme it was rendered in, so a
    // theme change can re-render it (see the watcher below).
    let currentPayload = null;
    let renderedDark = null;

    // Resolves the active theme (an ECharts theme list) to the colors the map
    // reads. An explicit theme is dark or light by its background luminance; in
    // auto mode the host's scheme decides, by the detection every binding shares
    // (lib/draw/theme_watch.js).
    const resolveTheme = (x) => {
      renderedDark = window.RtemisThemeWatch.isDark();
      return window.RtemisVectorTheme.resolve(
        x.autoTheme && renderedDark ? x.themeDark : x.theme
      );
    };


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

    // `view` ({center, zoom}) restores the viewer's position when re-rendering
    // the same map in a new theme; a new payload starts from its own view.
    const renderMap = (x, view = null) => {
      currentPayload = x;
      const draw = window.RtemisDraw;
      if (!maplibregl || !topojsonFeature || !chromatic || !draw) {
        container.innerHTML =
          '<div style="padding:1rem;color:#b00">rtemis-map bundles failed to load (window.RtemisMap or window.RtemisDraw missing).</div>';
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
      const s = x.config || {};
      const geo = x.geo || {};
      const resolution = model.resolution || s.resolution || "country";
      const theme = resolveTheme(x);
      const oc = overlayColors(theme);
      const valueLabel = s.value_label || model.valueLabel;

      el.style.backgroundColor = theme.bg;
      el.style.fontFamily = theme.fontFamily;

      // A geometry or join error replaces the map with its message.
      let regions, joined;
      try {
        regions = draw.mapFeatures(JSON.parse(geo.topojson), resolution, topojsonFeature);
        joined = draw.joinChoropleth({ ...model, resolution }, regions.ids, s.aggregate);
      } catch (err) {
        container.textContent = "rtemis-map: " + err.message;
        throw err;
      }
      const scale = draw.buildChoroplethScale(
        joined.values.values(), s, { scheme: theme.dark ? "dark" : "light" }, chromatic
      );
      valueById = joined.values;
      extrasById = joined.extras;
      const outline = draw.CHOROPLETH_COLORS.outline[theme.dark ? "dark" : "light"];

      map = new maplibregl.Map({
        container: container,
        center: view ? view.center : (geo.center || [0, 20]),
        zoom: view ? view.zoom : (geo.zoom != null ? geo.zoom : 0.4),
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
          data: regions.features,
          promoteId: "joinId",
        });
        map.addLayer({
          id: FILL_LAYER,
          type: "fill",
          source: SOURCE_ID,
          paint: {
            "fill-color": draw.choroplethFillExpression(scale),
            "fill-opacity": s.opacity != null ? s.opacity : 1,
          },
        });
        map.addLayer({
          id: LINE_LAYER,
          type: "line",
          source: SOURCE_ID,
          paint: {
            "line-color": outline,
            "line-width": s.outline_width != null ? s.outline_width : 0.2,
          },
          layout: { visibility: s.show_boundaries === false ? "none" : "visible" },
        });
        for (const [id, v] of valueById) {
          map.setFeatureState({ source: SOURCE_ID, id: id }, { v: v });
        }

        renderLegend(s, valueLabel, scale, oc);
        // Processing diagnostics belong in the console, not on the figure.
        if (joined.unmatched) console.warn("rtemis-map: unmatched locations:", joined.unmatchedKeys);
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
        const vlabel = valueLabel
          ? '<span style="color:' + oc.muted + '">' + escapeHtml(valueLabel) + ": </span>"
          : "";
        html +=
          '<div style="color:' +
          oc.muted +
          '">' +
          vlabel +
          '<span style="font-variant-numeric:tabular-nums;color:' +
          oc.fg +
          '">' +
          (value == null ? "no data" : escapeHtml(String(draw.formatNumber(value)))) +
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
          cornerStyle(s.tooltip_position || "top-right");
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

    const renderLegend = (s, valueLabel, scale, oc) => {
      if (s.show_legend === false || !scale.legend || scale.legend.length === 0) {
        legendEl.style.display = "none";
        return;
      }
      let html = "";
      if (valueLabel) {
        html +=
          '<div style="font-weight:500;margin-bottom:4px">' +
          escapeHtml(valueLabel) +
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
        cornerStyle(s.legend_position || "bottom-right");
      legendEl.style.backgroundColor = oc.bg;
      legendEl.style.color = oc.fg;
      legendEl.innerHTML = html;
      legendEl.style.display = "block";
    };

    // Re-render in the new theme when the host switches between light and
    // dark, keeping the current center and zoom.
    window.RtemisThemeWatch.watch(el, () => {
      if (!currentPayload?.autoTheme) return;
      if (window.RtemisThemeWatch.isDark() === renderedDark) return;
      const view = map ? {center: map.getCenter(), zoom: map.getZoom()} : null;
      renderMap(currentPayload, view);
    }, () => {
      if (resizeObserver) resizeObserver.disconnect();
      if (map) map.remove();
      map = null;
    });

    return {
      getRenderer: () => map,
      renderValue: (x) => renderMap(x),
      resize: () => {
        if (map) map.resize();
      },
    };
  },
});
