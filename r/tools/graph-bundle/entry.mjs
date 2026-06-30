// Bundle entry for the rtemis-graph htmlwidget dependency.
//
// htmlwidgets loads plain <script> files that must attach their API to a
// browser global (there is no module loader at runtime, unlike rtemislive's
// bundler). echarts ships a prebuilt UMD; sigma 4 + graphology do not, so we
// bundle them here into a single IIFE. esbuild's `--global-name=RtemisGraph`
// assigns this module's exports to `window.RtemisGraph`, which
// rtemis-graph.js then reads.
//
// Keep these imports in sync with rtemislive's GraphCanvas.tsx so both
// renderers behave identically.

export { default as Graph } from "graphology";
export { default as Sigma } from "sigma";
export { default as louvain } from "graphology-communities-louvain";
export { default as forceAtlas2 } from "graphology-layout-forceatlas2";
export { circlepack, random } from "graphology-layout";
