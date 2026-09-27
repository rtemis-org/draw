# Visual release checks

Install the current checkout, then run `just qa-targeted <output-directory>` from
the repository root. The developer harness requires chromote, htmlwidgets,
jsonlite, base64enc, xml2, rtemis.a3, Chrome, and Node.js. Its renderer checksum
check rejects an installed package whose JavaScript differs from the checkout.
All visual-QA tools and generated evidence stay outside the CRAN source archive.

The targeted matrix covers long category labels, six long legend labels, faint
box outlines, vertical Sankey flow, four-class confusion matrices, three
validation panels, a 300-residue annotated protein with overlapping PTMs and
variants, an explicit A3 grid, and a mixed three-chart composition. Each scene
uses light/dark themes and 1200, 760, and 390 pixel host widths. The browser
preference is deliberately opposite the requested theme. Native legend clicks,
A3 hover/zoom, browser exceptions, vector XML, screenshots, and measured text
bounds are recorded in `manifest.json`. Inspect every browser/SVG pair; a
successful script does not establish visual correctness by itself.

`DRAW_QA_CASES` optionally selects comma-separated builder names for a focused
rerun. Earlier evidence must retain its original source and asset provenance.

## Deliberate surface constraints

- A3's explicit residues-per-row and custom grid settings are caller-owned.
  The 300-residue phone rendering is an overview, not a readable sequence at
  ordinary scale. Use a wider surface or fewer residues per row for reading.
  The custom grid reserves 300 pixels for the annotation rail.
- `PanelLayout$ncol` is fixed. The mixed-panel fixture explicitly uses one
  column and a 1500-pixel height at 390 pixels, with two columns at wider sizes;
  this is not automatic reflow. A horizontal heatmap colorbar and 35-pixel
  dendrogram strips reserve room for cell values in the narrow composition.
- Confusion panels reflow, but bounded SVG output cannot grow vertically.
  The three-panel fixture uses a 1500-pixel canvas. An undersized confusion
  surface raises a size error instead of overlapping labels and metric values.

The matrix does not certify every browser, IDE viewer, arbitrary low-level
option combination, or biological annotation density.
