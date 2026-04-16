# draw_a3 — A3 Amino Acid Sequence Visualization

Documents the design and implementation of the A3 sequence viewer in both:

- **R**: `r/R/draw_a3.R` in `rtemis.draw`
- **TypeScript**: `src/lib/a3/visualization/echarts.ts` in `rtemislive-draw`

Both implementations must produce identical ECharts options and visual output.

---

## Layout algorithm

Translated from `createSequenceCoordinates()` in `layout.ts`.

The sequence is wrapped in a meander (serpentine) path:

- **X** oscillates: `1, 2, …, nPerRow, nPerRow−1, …, 2` (period `p = 2·nPerRow − 2`)
- **Y** increments by band: each group of `nPerRow − 1` residues shares the same integer Y
- At the **right-turn** (1-based index `nPerRow`, repeated every `p`):
  `y += 0.5`, `x -= borderOffset` where `borderOffset = 1 − √3/2`
- At the **left-turn** (1-based index `2·nPerRow − 1`, repeated every `p`):
  `y += 0.5`, `x += borderOffset`
- X is scaled: `x_scaled = 1 + (x − 1) · residueSpacing`

**Bounds** (`minY`, `maxY`, etc.) are computed from the final coordinates after turn
adjustments. `verticalSpan = maxY − minY`.

---

## Auto-computed height

```
height = ceil(titleMarginTop + 24 + markerSize · (2 · verticalSpan + 1))
```

The grid spans `verticalSpan + 2` Y-axis units over that pixel height.

---

## Series layers (z-order)

| z  | Series type | Purpose |
|----|-------------|---------|
| 10 | `line`      | Backbone (primary structure) |
| 15 | `line`      | Regions (thick semi-transparent band) |
| 20 | `scatter`   | Sites (hollow circles) |
| 30 | `scatter`   | PTMs (small filled circles, radially offset) |
| 31 | `scatter`   | Processing (inverted triangles, radially offset) |
| 40 | `scatter`   | Residue labels (`symbolSize = 0`, label only) |
| 41 | `scatter`   | Position labels every N residues (`symbolSize = 0`) |

Heading placeholder series (`symbolSize = 0`, `silent = true`, `data = []`) sit
between annotation groups to provide visual section headers in the legend via the
rich-text `formatter`.

---

## Legend design

### Type and position

`type: "plain"` is used instead of `"scroll"`. The scroll legend reserves space for
navigation arrows even when all items fit (confirmed from
`ScrollableLegendView.ts` line 265:
`const showController = contentRect[wh] > maxSize[wh]`), causing items to start
below `legend.top`. With `"plain"`, items start exactly at `legend.top`.

`legend.top` is computed to align the first legend item with the first sequence row:

```
gridHeight   = markerSize · (2 · verticalSpan + 1)
yAxisSpan    = verticalSpan + 2          // [minY−1 , maxY+1]
legendTop    = titleMarginTop + round(gridHeight / yAxisSpan)
```

This is the pixel offset of `y = minY = 1` within the y-axis range, giving exact
alignment between the legend's first item and the first residue row regardless of
sequence length.

### Icons

| Entry type | `icon` field | Notes |
|---|---|---|
| Primary structure | *(none, string entry)* | ECharts uses series line style |
| Site | `"circle"` + per-item `itemStyle` | Hollow ring: `color: "rgba(0,0,0,0)"`, `borderColor`, `borderWidth: 2` |
| Region | `"path://M2 5 H22 V11 H2 Z"` | Horizontal band; `path://` is CSP-safe |
| PTM | *(none)* | ECharts uses series symbol (filled circle) |
| Processing | *(none)* | ECharts uses series symbol (triangle) |
| Heading placeholder | `"none"` | Hides the icon slot; rich-text formatter renders the section label |

**Why not `image://data:image/svg+xml,...`**: these URIs are blocked by the
Content Security Policy in VSCode and RStudio webviews, producing blank icons.

**Why not `path://` ring via non-zero winding**: ZRender's handling of
multi-subpath `path://` strings in legend icons is unreliable and a degenerate
`path://M0,0Z` for heading entries caused the legend layout to collapse by
producing a zero-area bounding box.

The per-item `itemStyle` override in `legend.data` entries (ECharts 5 feature)
is the clean solution: the built-in `"circle"` symbol is styled hollow for the
legend icon independently of how the actual scatter markers are styled on the chart.

### Section headings

Three sentinel series names mark section boundaries:
`__legend_heading_regions__`, `__legend_heading_ptms__`,
`__legend_heading_processing__`. The JS/R `formatter` function replaces them with
rich-text `{heading|Regions}`, `{heading|PTMs}`, `{heading|Processing}`.

---

## Color palettes

Identical between R and TypeScript (light and dark themes use the same values):

| Annotation | Colors |
|---|---|
| Region  | `#0F766E #2563EB #D97706 #7C3AED #DC2626 #0891B2` |
| Site    | `#0EA5E9 #14B8A6 #E11D48 #A855F7 #F97316 #16A34A` |
| PTM     | `#0284C7 #F59E0B #10B981 #8B5CF6 #EF4444 #14B8A6` |
| Processing | `#D97706 #DC2626 #0EA5E9 #9333EA #16A34A #DB2777` |
