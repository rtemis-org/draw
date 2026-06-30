// rtemis-map htmlwidget binding.
//
// MapLibre GL renderer for the choropleth map plot type. This is the third
// rendering surface in rtemis.draw (after ECharts and Sigma.js); it consumes a
// renderer-agnostic map model ({ rows, resolution, valueLabel, tooltipFields })
// plus an embedded TopoJSON geometry -- not an EChartsOption -- and manages the
// MapLibre instance lifecycle.
//
// It is a vanilla-JS port of rtemislive's MapCanvas.tsx
// (~/Code/live/src/components/chart/MapCanvas.tsx) together with the scale
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
    const BG_LIGHT = "#f8fafc"; // slate-50
    const BG_DARK = "#09090b"; // zinc-950
    const OUTLINE_LIGHT = "#cbd5e1"; // slate-300
    const OUTLINE_DARK = "#3f3f46"; // zinc-700
    const MISSING_LIGHT = "#e4e4e7"; // zinc-200
    const MISSING_DARK = "#3f3f46"; // zinc-700

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
      if (window.matchMedia) {
        return window.matchMedia("(prefers-color-scheme: dark)").matches;
      }
      return false;
    };

    // Resolve the active theme (echarts-shaped list) -> the few colors the map
    // needs. For an explicit theme we infer dark/light from the background
    // luminance; for auto mode we detect it from the host.
    const resolveTheme = (x) => {
      let theme = null;
      if (x.autoTheme) {
        theme = isDarkMode() ? x.themeDark : x.theme;
      } else if (x.theme) {
        theme = x.theme;
      }
      let dark;
      if (x.autoTheme) {
        dark = isDarkMode();
      } else if (theme && theme.backgroundColor) {
        dark = isColorDark(theme.backgroundColor);
      } else {
        dark = false;
      }
      const bg =
        (theme && theme.backgroundColor) || (dark ? BG_DARK : BG_LIGHT);
      const fg =
        (theme && theme.textStyle && theme.textStyle.color) ||
        (dark ? "#e6e6e6" : "#1a1a1a");
      return { bg, fg, dark };
    };

    // Rough relative-luminance test for a #rrggbb / #rgb color.
    const isColorDark = (hex) => {
      const m = String(hex).trim().replace("#", "");
      if (m.length !== 6 && m.length !== 3) return false;
      const full =
        m.length === 3
          ? m
              .split("")
              .map((c) => c + c)
              .join("")
          : m;
      const r = parseInt(full.slice(0, 2), 16);
      const g = parseInt(full.slice(2, 4), 16);
      const b = parseInt(full.slice(4, 6), 16);
      // Perceived luminance (ITU-R BT.601).
      return (0.299 * r + 0.587 * g + 0.114 * b) / 255 < 0.5;
    };

    // ── Color scale (port of choroplethScale.ts) ────────────────────────────
    const sequentialScheme = (name) => {
      const map = {
        blues: "interpolateBlues",
        viridis: "interpolateViridis",
        ylorrd: "interpolateYlOrRd",
        greens: "interpolateGreens",
        magma: "interpolateMagma",
      };
      return map[name] ? chromatic[map[name]] : null;
    };
    const divergingScheme = (name) => {
      const map = {
        rdbu: "interpolateRdBu",
        rdylgn: "interpolateRdYlGn",
        spectral: "interpolateSpectral",
        brbg: "interpolateBrBG",
      };
      return map[name] ? chromatic[map[name]] : null;
    };

    // Sample n colors from an interpolator, biasing brighter in dark mode.
    const rampColors = (interp, n, dark, diverging) => {
      if (n <= 1) return [interp(0.5)];
      const t0 = diverging ? 0 : dark ? 0.3 : 0.12;
      const t1 = diverging ? 1 : dark ? 1 : 0.95;
      return Array.from({ length: n }, (_, i) =>
        interp(t0 + ((t1 - t0) * i) / (n - 1)),
      );
    };

    const quantileBreaks = (sorted, classes) => {
      const breaks = [];
      for (let i = 1; i < classes; i++) {
        const q = (sorted.length - 1) * (i / classes);
        const lo = Math.floor(q);
        const hi = Math.ceil(q);
        breaks.push(sorted[lo] + (sorted[hi] - sorted[lo]) * (q - lo));
      }
      return breaks;
    };

    const equalBreaks = (min, max, classes) => {
      const breaks = [];
      const step = (max - min) / classes;
      for (let i = 1; i < classes; i++) breaks.push(min + step * i);
      return breaks;
    };

    // Jenks natural breaks (Fisher-Jenks DP), capped by sampling for large n.
    const jenksBreaks = (values, classes) => {
      let data = values;
      const CAP = 1500;
      if (data.length > CAP) {
        const step = data.length / CAP;
        data = Array.from(
          { length: CAP },
          (_, i) => values[Math.floor(i * step)],
        );
      }
      const n = data.length;
      if (n <= classes) return quantileBreaks(values, classes);

      const mat1 = Array.from({ length: n + 1 }, () =>
        new Array(classes + 1).fill(0),
      );
      const mat2 = Array.from({ length: n + 1 }, () =>
        new Array(classes + 1).fill(0),
      );
      for (let j = 1; j <= classes; j++) {
        mat1[1][j] = 1;
        mat2[1][j] = 0;
        for (let i = 2; i <= n; i++) mat2[i][j] = Number.POSITIVE_INFINITY;
      }
      for (let l = 2; l <= n; l++) {
        let s1 = 0;
        let s2 = 0;
        let w = 0;
        for (let m = 1; m <= l; m++) {
          const i3 = l - m + 1;
          const val = data[i3 - 1];
          w++;
          s1 += val;
          s2 += val * val;
          const variance = s2 - (s1 * s1) / w;
          const i4 = i3 - 1;
          if (i4 !== 0) {
            for (let j = 2; j <= classes; j++) {
              if (mat2[l][j] >= variance + mat2[i4][j - 1]) {
                mat1[l][j] = i3;
                mat2[l][j] = variance + mat2[i4][j - 1];
              }
            }
          }
        }
        mat1[l][1] = 1;
        mat2[l][1] = s2 - (s1 * s1) / w;
      }

      const breaks = [];
      let k = n;
      for (let j = classes; j >= 2; j--) {
        const id = mat1[k][j] - 1;
        breaks.push(data[id]);
        k = mat1[k][j] - 1;
      }
      return breaks.reverse();
    };

    const fmt = (v) =>
      Math.abs(v) >= 100 || Number.isInteger(v)
        ? Math.round(v).toLocaleString()
        : v.toFixed(1);

    const buildScale = (rawValues, opts) => {
      const missingColor = opts.dark ? MISSING_DARK : MISSING_LIGHT;
      const diverging = divergingScheme(opts.scheme) !== null;
      const interp =
        sequentialScheme(opts.scheme) ||
        divergingScheme(opts.scheme) ||
        chromatic.interpolateBlues;

      const finite = rawValues.filter(
        (v) => typeof v === "number" && Number.isFinite(v),
      );
      const classes = Math.max(2, Math.min(12, Math.round(opts.classes)));
      const colors = rampColors(interp, classes, opts.dark, diverging);

      if (finite.length === 0) {
        return {
          classes,
          thresholds: [],
          colors,
          missingColor,
          min: 0,
          max: 0,
          legend: [],
        };
      }

      const sorted = [...finite].sort((a, b) => a - b);
      const min = sorted[0];
      const max = sorted[sorted.length - 1];

      let thresholds;
      if (min === max) {
        thresholds = [];
      } else if (opts.classification === "equal") {
        thresholds = equalBreaks(min, max, classes);
      } else if (opts.classification === "jenks") {
        thresholds = jenksBreaks(sorted, classes);
      } else {
        thresholds = quantileBreaks(sorted, classes);
      }
      thresholds = thresholds
        .filter((t, i, a) => i === 0 || t > a[i - 1])
        .filter((t) => t > min && t < max);

      const bounds = [min, ...thresholds, max];
      const legend = colors.slice(0, bounds.length - 1).map((color, i) => ({
        color,
        label: `${fmt(bounds[i])} – ${fmt(bounds[i + 1])}`,
      }));

      return {
        classes,
        thresholds,
        colors: colors.slice(0, thresholds.length + 1),
        missingColor,
        min,
        max,
        legend,
      };
    };

    // MapLibre fill-color expression: missing color when no joined value, else a
    // step over the classification thresholds.
    const fillColorExpression = (scale) => {
      const value = ["to-number", ["feature-state", "v"]];
      let matched;
      if (scale.thresholds.length === 0) {
        matched = scale.colors[0] || scale.missingColor;
      } else {
        const step = ["step", value, scale.colors[0]];
        scale.thresholds.forEach((t, i) => {
          step.push(t, scale.colors[i + 1]);
        });
        matched = step;
      }
      return [
        "case",
        ["!=", ["feature-state", "v"], null],
        matched,
        scale.missingColor,
      ];
    };

    // ── Location resolver (port of locationResolver.ts) ──────────────────────
    const STATE_ABBR_TO_FIPS = {
      AL: "01", AK: "02", AZ: "04", AR: "05", CA: "06", CO: "08", CT: "09",
      DE: "10", DC: "11", FL: "12", GA: "13", HI: "15", ID: "16", IL: "17",
      IN: "18", IA: "19", KS: "20", KY: "21", LA: "22", ME: "23", MD: "24",
      MA: "25", MI: "26", MN: "27", MS: "28", MO: "29", MT: "30", NE: "31",
      NV: "32", NH: "33", NJ: "34", NM: "35", NY: "36", NC: "37", ND: "38",
      OH: "39", OK: "40", OR: "41", PA: "42", RI: "44", SC: "45", SD: "46",
      TN: "47", TX: "48", UT: "49", VT: "50", VA: "51", WA: "53", WV: "54",
      WI: "55", WY: "56", PR: "72", VI: "78", GU: "66", AS: "60", MP: "69",
    };
    const STATE_NAME_TO_FIPS = {
      alabama: "01", alaska: "02", "american samoa": "60", arizona: "04",
      arkansas: "05", california: "06", colorado: "08",
      "commonwealth of the northern mariana islands": "69", connecticut: "09",
      delaware: "10", "district of columbia": "11", florida: "12",
      georgia: "13", guam: "66", hawaii: "15", idaho: "16", illinois: "17",
      indiana: "18", iowa: "19", kansas: "20", kentucky: "21", louisiana: "22",
      maine: "23", maryland: "24", massachusetts: "25", michigan: "26",
      minnesota: "27", mississippi: "28", missouri: "29", montana: "30",
      nebraska: "31", nevada: "32", "new hampshire": "33", "new jersey": "34",
      "new mexico": "35", "new york": "36", "north carolina": "37",
      "north dakota": "38", ohio: "39", oklahoma: "40", oregon: "41",
      pennsylvania: "42", "puerto rico": "72", "rhode island": "44",
      "south carolina": "45", "south dakota": "46", tennessee: "47",
      texas: "48", "united states virgin islands": "78", utah: "49",
      vermont: "50", virginia: "51", washington: "53", "west virginia": "54",
      wisconsin: "55", wyoming: "56",
    };
    const ISO2_TO_ISO3 = {
      AE: "ARE", AF: "AFG", AL: "ALB", AM: "ARM", AO: "AGO", AQ: "ATA",
      AR: "ARG", AT: "AUT", AU: "AUS", AZ: "AZE", BA: "BIH", BD: "BGD",
      BE: "BEL", BF: "BFA", BG: "BGR", BI: "BDI", BJ: "BEN", BN: "BRN",
      BO: "BOL", BR: "BRA", BS: "BHS", BT: "BTN", BW: "BWA", BY: "BLR",
      BZ: "BLZ", CA: "CAN", CD: "COD", CF: "CAF", CG: "COG", CH: "CHE",
      CI: "CIV", CL: "CHL", CM: "CMR", CN: "CHN", CO: "COL", CR: "CRI",
      CU: "CUB", CY: "CYP", CZ: "CZE", DE: "DEU", DJ: "DJI", DK: "DNK",
      DO: "DOM", DZ: "DZA", EC: "ECU", EE: "EST", EG: "EGY", EH: "ESH",
      ER: "ERI", ES: "ESP", ET: "ETH", FI: "FIN", FJ: "FJI", FK: "FLK",
      FR: "FRA", GA: "GAB", GB: "GBR", GE: "GEO", GH: "GHA", GL: "GRL",
      GM: "GMB", GN: "GIN", GQ: "GNQ", GR: "GRC", GT: "GTM", GW: "GNB",
      GY: "GUY", HN: "HND", HR: "HRV", HT: "HTI", HU: "HUN", ID: "IDN",
      IE: "IRL", IL: "ISR", IN: "IND", IQ: "IRQ", IR: "IRN", IS: "ISL",
      IT: "ITA", JM: "JAM", JO: "JOR", JP: "JPN", KE: "KEN", KG: "KGZ",
      KH: "KHM", KP: "PRK", KR: "KOR", KW: "KWT", KZ: "KAZ", LA: "LAO",
      LB: "LBN", LK: "LKA", LR: "LBR", LS: "LSO", LT: "LTU", LU: "LUX",
      LV: "LVA", LY: "LBY", MA: "MAR", MD: "MDA", ME: "MNE", MG: "MDG",
      MK: "MKD", ML: "MLI", MM: "MMR", MN: "MNG", MR: "MRT", MW: "MWI",
      MX: "MEX", MY: "MYS", MZ: "MOZ", NA: "NAM", NC: "NCL", NE: "NER",
      NG: "NGA", NI: "NIC", NL: "NLD", NO: "NOR", NP: "NPL", NZ: "NZL",
      OM: "OMN", PA: "PAN", PE: "PER", PG: "PNG", PH: "PHL", PK: "PAK",
      PL: "POL", PR: "PRI", PS: "PSE", PT: "PRT", PY: "PRY", QA: "QAT",
      RO: "ROU", RS: "SRB", RU: "RUS", RW: "RWA", SA: "SAU", SB: "SLB",
      SD: "SDN", SE: "SWE", SI: "SVN", SK: "SVK", SL: "SLE", SN: "SEN",
      SO: "SOM", SR: "SUR", SS: "SSD", SV: "SLV", SY: "SYR", SZ: "SWZ",
      TD: "TCD", TF: "ATF", TG: "TGO", TH: "THA", TJ: "TJK", TL: "TLS",
      TM: "TKM", TN: "TUN", TR: "TUR", TT: "TTO", TW: "TWN", TZ: "TZA",
      UA: "UKR", UG: "UGA", US: "USA", UY: "URY", UZ: "UZB", VE: "VEN",
      VN: "VNM", VU: "VUT", YE: "YEM", ZA: "ZAF", ZM: "ZMB", ZW: "ZWE",
    };

    const digits = (raw) => raw.replace(/[^0-9]/g, "");

    // Normalize one location value to the canonical TopoJSON join id.
    const normalizeKey = (raw, resolution) => {
      if (raw == null) return null;
      const s = String(raw).trim();
      if (!s) return null;

      if (resolution === "county") {
        const d = digits(s);
        if (!d || d.length > 5) return null;
        return d.padStart(5, "0");
      }
      if (resolution === "state") {
        const d = digits(s);
        if (d) return d.length <= 2 ? d.padStart(2, "0") : null;
        const up = s.toUpperCase();
        if (STATE_ABBR_TO_FIPS[up]) return STATE_ABBR_TO_FIPS[up];
        return STATE_NAME_TO_FIPS[s.toLowerCase()] || null;
      }
      // country
      const up = s.toUpperCase();
      if (up.length === 3) return up;
      if (up.length === 2) return ISO2_TO_ISO3[up] || null;
      return null;
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
    const reportEl = makeOverlay();

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
      reportEl.style.display = "none";

      const model = x.model || {};
      const s = x.style || {};
      const geo = x.geo || {};
      const rows = model.rows || [];
      const resolution = model.resolution || "country";
      const theme = resolveTheme(x);
      const oc = overlayColors(theme);

      el.style.backgroundColor = theme.bg;

      // Parse the embedded TopoJSON once and convert to GeoJSON.
      let fc;
      const idSet = new Set();
      try {
        const topo = JSON.parse(geo.topojson);
        const obj = topo.objects[geo.object];
        fc = topojsonFeature(topo, obj);
        for (const f of fc.features) {
          const id = f.id == null ? "" : String(f.id);
          f.properties = Object.assign({}, f.properties || {}, { joinId: id });
          if (id) idSet.add(id);
        }
      } catch (err) {
        container.innerHTML =
          '<div style="padding:1rem;color:#b00">rtemis-map: failed to parse geometry.</div>';
        return;
      }

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

        // Build the scale from the model values.
        const scale = buildScale(
          rows.map((r) => r.value),
          {
            classification: s.classification || "quantile",
            scheme: s.colorScheme || "blues",
            classes: s.numClasses != null ? s.numClasses : 5,
            dark: theme.dark,
          },
        );

        // Join: normalize each row key, set feature-state, build join report.
        map.removeFeatureState({ source: SOURCE_ID });
        valueById = new Map();
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
        renderReport(x, { matched, unmatched, unmatchedKeys }, oc);
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

    const renderReport = (x, report, oc) => {
      const s = x.style || {};
      if (!report || (report.matched === 0 && report.unmatched === 0)) {
        reportEl.style.display = "none";
        return;
      }
      let html =
        '<span style="font-variant-numeric:tabular-nums">' +
        report.matched.toLocaleString() +
        " matched</span>";
      if (report.unmatched > 0) {
        const more =
          report.unmatched > report.unmatchedKeys.length ? " …" : "";
        html +=
          ' <span style="font-variant-numeric:tabular-nums;color:#d97706" title="Unmatched keys: ' +
          escapeHtml(report.unmatchedKeys.join(", ") + more) +
          '">· ' +
          report.unmatched.toLocaleString() +
          " unmatched</span>";
      }
      // Default the report to the corner opposite the legend's left/right to
      // reduce overlap; bottom-left mirrors rtemislive's default.
      reportEl.style.cssText =
        reportEl.style.cssText.replace(/top:[^;]*;|bottom:[^;]*;|left:[^;]*;|right:[^;]*;/g, "") +
        cornerStyle(s.reportPosition || "bottom-left");
      reportEl.style.backgroundColor = oc.bg;
      reportEl.style.color = oc.fg;
      reportEl.innerHTML = html;
      reportEl.style.display = "block";
    };

    return {
      renderValue: (x) => renderMap(x),
      resize: () => {
        if (map) map.resize();
      },
    };
  },
});
