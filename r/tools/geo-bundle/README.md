# Country boundary build

This developer tool rebuilds the bundled country topology and provenance
manifest from Natural Earth 5.1.2. It is not used when installing or running
the R package. Building requires Node.js 20.11 or later; ordinary SVG export
continues to require Node.js 18 or later.

From this directory:

```sh
npm ci --ignore-scripts
npm run build
npm run build -- --check
```

The build downloads a versioned GeoJSON and verifies its SHA-256 before
processing it with the pinned mapshaper version. To use an already downloaded
copy, append its path to either build command. `--check` compares regenerated
bytes with both bundled files and does not modify them.

The conversion retains 174 uniquely keyed `ISO_A3_EH` country features,
excluding the three source features with the sentinel `-99`. It preserves the
existing country ID set, retains the source's 1:110m geometry without further
simplification, and quantizes coordinates to 100,000 units. Only country names
and ISO keys are retained as properties. The generated manifest contains all
conversion arguments, source and output hashes, and the sorted country-ID hash.
No timestamps or machine-specific paths enter the generated files.
