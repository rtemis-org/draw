[![R CI](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml) [![rtemis.draw status badge](https://rtemis-org.r-universe.dev/rtemis.draw/badges/version)](https://rtemis-org.r-universe.dev/rtemis.draw)

# rtemis.draw

![rtemis.draw cover](https://docs.rtemis.org/r/draw/assets/cover.avif)

Interface to JS libraries for high performance interactive visualization using type-checked, validated configuration objects.

## Maintaining widget assets

htmlwidgets identifies the ECharts binding by the package version in
`r/DESCRIPTION` and its supporting scripts by the versions in
`r/inst/htmlwidgets/rtemis-draw.yaml`. Cached documents retain those identities.
When changing a script or its dependency list, advance the affected version and
add its script names and MD5 fingerprints (with normalized line endings) to
`r/tests/testthat/fixtures/widget_assets.json`. Keep previous entries unchanged.
The dependency tests detect changed bytes under an existing identity and load
the scripts in the order declared for the browser.

After updating the installed package, execute all chapters of a documentation
book together. Quarto's `freeze: auto` watches document source changes, so it can
reuse execution results from an older package installation. Use `freeze: false`
for documentation that needs to exercise the current package on each full build.

## Visual export QA

After installing the current R package, run `just qa-export <output-directory>`
from this checkout. The developer tool checks square heatmaps, dendrograms,
independent panels, and A3 diagrams in light/dark themes and two viewport sizes.
It writes HTML, SVG, browser screenshots, and a manifest recording the installed
package/dependency versions, asset fingerprints, source revision, and results.
Review the images: geometry assertions alone do not establish visual quality.

This requires Node.js, Chrome, and the R packages `chromote`, `base64enc`, `xml2`,
`htmlwidgets`, `jsonlite`, `rtemis.a3`, and `rtemis.draw`. Optional `rsvg-convert`
also produces SVG previews. The command covers these families only; the broader
visual and interaction audit remains separate.
