# map-bundle

Build input for the vendored browser bundle that powers the `rtemis-map`
htmlwidget (choropleth maps via MapLibre GL).

MapLibre ships a UMD, but `topojson-client` and `d3-scale-chromatic` do not
expose a single convenient browser global, so we bundle all three here with
esbuild into one IIFE that attaches a `window.RtemisMap` global. The dependency
versions are pinned to match rtemislive (`~/Code/live`) so both renderers
behave identically.

## Regenerate the bundle

```sh
cd r/tools/map-bundle
npm install
npm run build
```

This (re)writes the committed artifact:

```
r/inst/htmlwidgets/lib/maplibre/rtemis-map-deps.js
```

`node_modules/` and `package-lock.json` are gitignored; only `package.json`,
`entry.mjs`, and the built artifact are tracked.

## What it exposes

`window.RtemisMap` = `{ maplibregl, topojsonFeature, chromatic }`, consumed by
`r/inst/htmlwidgets/rtemis-map.js`:

- `maplibregl` — the MapLibre GL JS module (default export).
- `topojsonFeature` — `topojson-client`'s `feature()` (TopoJSON -> GeoJSON).
- `chromatic` — the `d3-scale-chromatic` namespace (the `interpolate*` color
  ramps sampled to build the choropleth scale).

## Geometry

The admin-boundary TopoJSON files live separately in
`r/inst/htmlwidgets/lib/geo/` (`countries.topo.json`, `us-10m.topo.json`) and
are embedded into each widget's payload by `draw_choropleth()` at render time --
they are not part of this bundle.
