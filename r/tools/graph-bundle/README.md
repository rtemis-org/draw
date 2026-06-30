# graph-bundle

Build input for the vendored browser bundle that powers the `rtemis-graph`
htmlwidget (network / graph plots via Sigma.js).

Unlike echarts (which ships a prebuilt UMD), sigma 4 and graphology have no
single browser global, so we bundle them here with esbuild into one IIFE that
attaches a `window.RtemisGraph` global. The dependency versions are pinned to
match rtemislive (`~/Code/live`) so both renderers behave identically.

## Regenerate the bundle

```sh
cd r/tools/graph-bundle
npm install
npm run build
```

This (re)writes the committed artifact:

```
r/inst/htmlwidgets/lib/sigma/rtemis-graph-deps.js
```

`node_modules/` and `package-lock.json` are gitignored; only `package.json`,
`entry.mjs`, and the built artifact are tracked.

## What it exposes

`window.RtemisGraph` = `{ Graph, Sigma, louvain, forceAtlas2, circlepack, random }`,
consumed by `r/inst/htmlwidgets/rtemis-graph.js`.
