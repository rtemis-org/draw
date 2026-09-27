# Bundled map boundaries

`us-10m.topo.json` is an unmodified copy of `counties-10m.json` from
[us-atlas 3.0.1](https://github.com/topojson/us-atlas/tree/v3.0.1).
Download it from the [versioned distribution](https://cdn.jsdelivr.net/npm/us-atlas@3.0.1/counties-10m.json).
It contains counties, states and nation objects with FIPS identifiers. Its
SHA-256 is `145aaf5d1433352a6a1d8e86b5f149c7c653f9171baf14aaf75ee66575def1b0`.
The upstream ISC notice is retained in `us-atlas-LICENSE.txt`.

`countries.topo.json` contains 174 country features keyed by `ISO_A3_EH`,
with geometry identifiers copied to the feature `id`. It is built from
[Natural Earth 5.1.2, 1:110m admin-0 countries](https://github.com/nvkelso/natural-earth-vector/blob/v5.1.2/geojson/ne_110m_admin_0_countries.geojson),
without additional geometry simplification or remote basemap tiles. The three
source features without an `ISO_A3_EH` code are excluded from the keyed join.
[Natural Earth terms](https://www.naturalearthdata.com/about/terms-of-use/)
place its vector and raster map data in the public domain.

`countries.source.json` records the versioned source URL, source and artifact
SHA-256 hashes, exact mapshaper version and conversion options. The artifact's
SHA-256 is `603f93d7c500ab728ecf0db06f0d6fbe8759ba8035aae8679d31f2a5c37454cc`.
The reproducible developer build lives in `tools/geo-bundle` in the package
source. Its README documents regeneration and byte-for-byte verification.
