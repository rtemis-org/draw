// Bundle entry for the rtemis-map htmlwidget dependency.
//
// htmlwidgets loads plain <script> files that must attach their API to a
// browser global (there is no module loader at runtime, unlike rtemislive's
// bundler). MapLibre ships a UMD, but topojson-client and d3-scale-chromatic do
// not expose a single convenient global, so we bundle all three here into one
// IIFE. esbuild's `--global-name=RtemisMap` assigns this module's exports to
// `window.RtemisMap`, which rtemis-map.js then reads.
//
// Keep these versions in sync with rtemislive's MapCanvas.tsx / choroplethScale.ts
// (~/Code/live) so both renderers behave identically.

import maplibregl from "maplibre-gl";
import { feature as topojsonFeature } from "topojson-client";
import * as chromatic from "d3-scale-chromatic";

export { maplibregl, topojsonFeature, chromatic };
