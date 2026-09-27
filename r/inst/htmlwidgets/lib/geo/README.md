# Bundled map boundaries

`us-10m.topo.json` is an unmodified copy of `counties-10m.json` from
[us-atlas 3.0.1](https://github.com/topojson/us-atlas/tree/v3.0.1).
Download it from the [versioned distribution](https://cdn.jsdelivr.net/npm/us-atlas@3.0.1/counties-10m.json).
It contains counties, states and nation objects with FIPS identifiers. Its
SHA-256 is `145aaf5d1433352a6a1d8e86b5f149c7c653f9171baf14aaf75ee66575def1b0`.
The upstream ISC notice is retained in `us-atlas-LICENSE.txt`.

`countries.topo.json` contains 174 country features keyed by `ISO_A3_EH`,
with geometry identifiers copied to the feature `id`. It is a simplified
Natural Earth-derived topology, used without remote basemap tiles.
[Natural Earth terms](https://www.naturalearthdata.com/about/terms-of-use/)
place its vector and raster map data in the public domain.
The bundled artifact's SHA-256 is
`a958f331a9815d7833ec6f82b1443fe08f569db67b833098488a3136982e2827`.
The original Natural Earth revision and mapshaper simplification settings are
not recorded; this artifact is retained as supplied and exact regeneration
has not been established.
