# Bundled JavaScript sources and licenses

The package ships local JavaScript assets; drawing does not fetch library code
from a CDN. The notices below apply to the vendored libraries, independently
of the R package license. Upstream LICENSE and NOTICE files are retained verbatim.
ECharts-GL 2.1.0 declares MIT in its npm metadata but ships a BSD license file;
the distributed upstream license file is included without alteration.

## Rebuilding

The source archive includes `tools/graph-bundle` and `tools/map-bundle`, with
entry modules and npm lockfiles. In either directory, run `npm ci`, then
`npm run build` and `npm run build:vector` to rebuild browser and Node bundles.
Node.js 18 or later is required for SVG export and the build tooling; browsers
render ordinary widgets without Node.js.

ECharts 6.1.0 and ECharts-GL 2.1.0 are unmodified upstream distribution files.
Obtain the complete source and build files with `npm pack echarts@6.1.0` and
`npm pack echarts-gl@2.1.0`; copy `dist/echarts.min.js` and
`dist/echarts-gl.min.js`, respectively, to `inst/htmlwidgets/lib/echarts` and
`inst/htmlwidgets/lib/echarts-gl`. Preserve the latter's `.LICENSE.txt` companion.
The extracted packages include source directories and their upstream build
scripts. Run the package's `npm install` and `npm run build` to build from source.

All dependency sources are available as the versioned npm distributions linked
below. The graph/map lockfiles fix the complete build dependency graph. Shared
rtemis renderers and bindings are shipped as readable source in
`inst/htmlwidgets` and `inst/node` and require no compilation.

## Dependency notices

| Package | Version | Source | License notices |
| --- | --- | --- | --- |
| claygl | 1.3.0 | [npm](https://www.npmjs.com/package/claygl/v/1.3.0) | [claygl@1.3.0.txt](claygl@1.3.0.txt) |
| d3-array | 3.2.4 | [npm](https://www.npmjs.com/package/d3-array/v/3.2.4) | [d3-array@3.2.4.txt](d3-array@3.2.4.txt) |
| d3-color | 3.1.0 | [npm](https://www.npmjs.com/package/d3-color/v/3.1.0) | [d3-color@3.1.0.txt](d3-color@3.1.0.txt) |
| d3-geo | 3.1.1 | [npm](https://www.npmjs.com/package/d3-geo/v/3.1.1) | [d3-geo@3.1.1.txt](d3-geo@3.1.1.txt) |
| d3-interpolate | 3.0.1 | [npm](https://www.npmjs.com/package/d3-interpolate/v/3.0.1) | [d3-interpolate@3.0.1.txt](d3-interpolate@3.0.1.txt) |
| d3-scale-chromatic | 3.1.0 | [npm](https://www.npmjs.com/package/d3-scale-chromatic/v/3.1.0) | [d3-scale-chromatic@3.1.0.txt](d3-scale-chromatic@3.1.0.txt) |
| echarts-gl | 2.1.0 | [npm](https://www.npmjs.com/package/echarts-gl/v/2.1.0) | [echarts-gl@2.1.0.txt](echarts-gl@2.1.0.txt) |
| echarts | 6.1.0 | [npm](https://www.npmjs.com/package/echarts/v/6.1.0) | [echarts@6.1.0.txt](echarts@6.1.0.txt) |
| events | 3.3.0 | [npm](https://www.npmjs.com/package/events/v/3.3.0) | [events@3.3.0.txt](events@3.3.0.txt) |
| graphology-communities-louvain | 2.0.2 | [npm](https://www.npmjs.com/package/graphology-communities-louvain/v/2.0.2) | [graphology-communities-louvain@2.0.2.txt](graphology-communities-louvain@2.0.2.txt) |
| graphology-indices | 0.17.0 | [npm](https://www.npmjs.com/package/graphology-indices/v/0.17.0) | [graphology-indices@0.17.0.txt](graphology-indices@0.17.0.txt) |
| graphology-layout-forceatlas2 | 0.10.1 | [npm](https://www.npmjs.com/package/graphology-layout-forceatlas2/v/0.10.1) | [graphology-layout-forceatlas2@0.10.1.txt](graphology-layout-forceatlas2@0.10.1.txt) |
| graphology-layout | 0.6.1 | [npm](https://www.npmjs.com/package/graphology-layout/v/0.6.1) | [graphology-layout@0.6.1.txt](graphology-layout@0.6.1.txt) |
| graphology-utils | 2.5.2 | [npm](https://www.npmjs.com/package/graphology-utils/v/2.5.2) | [graphology-utils@2.5.2.txt](graphology-utils@2.5.2.txt) |
| graphology | 0.26.0 | [npm](https://www.npmjs.com/package/graphology/v/0.26.0) | [graphology@0.26.0.txt](graphology@0.26.0.txt) |
| internmap | 2.0.3 | [npm](https://www.npmjs.com/package/internmap/v/2.0.3) | [internmap@2.0.3.txt](internmap@2.0.3.txt) |
| maplibre-gl | 5.24.0 | [npm](https://www.npmjs.com/package/maplibre-gl/v/5.24.0) | [maplibre-gl@5.24.0.txt](maplibre-gl@5.24.0.txt) |
| mnemonist | 0.39.8 | [npm](https://www.npmjs.com/package/mnemonist/v/0.39.8) | [mnemonist@0.39.8.txt](mnemonist@0.39.8.txt) |
| obliterator | 2.0.5 | [npm](https://www.npmjs.com/package/obliterator/v/2.0.5) | [obliterator@2.0.5.txt](obliterator@2.0.5.txt) |
| pandemonium | 2.4.1 | [npm](https://www.npmjs.com/package/pandemonium/v/2.4.1) | [pandemonium@2.4.1.txt](pandemonium@2.4.1.txt) |
| sigma | 4.0.0-alpha.6 | [npm](https://www.npmjs.com/package/sigma/v/4.0.0-alpha.6) | [sigma@4.0.0-alpha.6.txt](sigma@4.0.0-alpha.6.txt) |
| topojson-client | 3.1.0 | [npm](https://www.npmjs.com/package/topojson-client/v/3.1.0) | [topojson-client@3.1.0.txt](topojson-client@3.1.0.txt) |
| zrender | 6.1.0 | [npm](https://www.npmjs.com/package/zrender/v/6.1.0) | [zrender@6.1.0.txt](zrender@6.1.0.txt) |
