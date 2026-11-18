# @rtemis/draw

ECharts option builders shared by [rtemis.draw](../r) and rtemislive. Each builder takes a chart's data and its config, an instance of the published `chart/<type>/v1` schema, and returns the ECharts option. rtemis.draw's widgets and rtemislive call the same builder, so a chart has one definition in R and in the browser. The network functions lay out and style a Sigma.js graph, and the choropleth functions join, classify, and color a MapLibre map, the same way.

The package has no runtime dependencies and runs in browsers and in Node.js.

## Usage

```ts
import { buildA3Option } from "@rtemis/draw"
import * as echarts from "echarts"

const option = buildA3Option(a3Document, { n_per_row: 30, ptm_placement: "outerRadial" }, { dark: true })
echarts.init(element).setOption(option)
```

`builders` maps each chart type to its builder:

```ts
import { builders } from "@rtemis/draw"

const option = builders.a3(a3Document, config)
```

## Charts

| Type | Builder | Data | Config schema |
|---|---|---|---|
| `a3` | `buildA3Option(document, config, appearance)` | an A3 document (`@rtemis/a3` `A3Data`) | `https://schema.rtemis.org/chart/a3/v1/schema.json` |
| `bar` | `buildBarOption(data, config, appearance)` | tabular data, `{columns, levels, types}` | `https://schema.rtemis.org/chart/bar/v1/schema.json` |
| `line` | `buildLineOption(data, config, appearance)` | tabular data, `{columns, levels, types}` | `https://schema.rtemis.org/chart/line/v1/schema.json` |
| `boxplot` | `buildBoxplotOption(data, config, appearance)` | box summaries, `{variables, levels, cells, comparisons}` | `https://schema.rtemis.org/chart/boxplot/v1/schema.json` |
| `confusion` | `buildConfusionOption(data, config, appearance)` | counts, `{classes, panels}` | `https://schema.rtemis.org/chart/confusion/v1/schema.json` |
| `roc`, `pr` | `buildRocOption(data, config, appearance)`, `buildPrOption(data, config, appearance)` | grouped curves, `{groups, curves}` | `https://schema.rtemis.org/chart/roc/v1/schema.json`, `https://schema.rtemis.org/chart/pr/v1/schema.json` |
| `density` | `buildDensityOption(data, config, appearance)` | curves, `{samples}` | `https://schema.rtemis.org/chart/density/v1/schema.json` |
| `histogram` | `buildHistogramOption(data, config, appearance)` | bins, `{edges, samples}` | `https://schema.rtemis.org/chart/histogram/v1/schema.json` |

Tabular data holds the columns the config binds, each an array or typed array, optionally the level order of categorical columns, and optionally the type of a column whose values are not read as stored: `"time"` declares a date or date-time column, held as epoch milliseconds of its wall-clock reading in UTC or as ISO 8601 text, whose offset is ignored. A bar chart labels a time category by its date, or its date and time. `frequency` names a column holding the number of rows each row stands for, when the host has combined rows. A bar chart draws one bar per row; with `group`, one bar per distinct category and group, in level order. When `group` or `aggregate` is set, the data must hold one row per category and group: the host combines rows as `aggregate` says (rtemislive in DuckDB, rtemis.draw in R) before building. With `y` unset, each bar is the number of rows of its category and group: the sum of the `frequency` column, or one per row. `barValueLabel(config, name)` gives the value axis name a host derives when the author sets none: "Count" with `y` unset; for one `y` column its name, as "Mean X", "Median X", or "Sum of X" under an aggregate.

`appearance` holds what every chart type shares and the config does not: the light or dark theme, the font family, the title inset, the legend gap, and animation. Every member of `config` and `appearance` is optional.

A line chart draws one line per `y` column, or per `y` column and group, through the rows in data order: a numeric `x` gets a value axis, a time `x` a time axis in UTC, and any other `x` a category axis. Unset axis limits extend the data range by `pad` on each side, as rtemis.draw's `resolve()` derives them; a stacked chart's y range is that of the sums.

A boxplot takes summaries the host computed: per value column, or per column and group level, the five plotting statistics (whisker ends, quartiles, and median), the count, the range, the points to draw, and, for violins, a density profile; rtemis.draw computes them in R and rtemislive in DuckDB. The builder draws points, violins, paired lines, and comparison brackets as custom series whose renderers `registerRenderers(echarts)` registers by name, once, before drawing.

A histogram takes bins the host computed: one set of edges, and per sample (an `x` column, or a column and group level) its size, the count in each bin `(lower, upper]` with the lowest edge included, the `bin_stat` of the values in each bin, and, for `density`, the kernel density curve; rtemis.draw bins with `graphics::hist()` and rtemislive in DuckDB. `histogramEdges(summary, {breaks, bins})` gives the edges `graphics::hist()` computes from a summary of the values (count, range, standard deviation, quartiles): the class count of Sturges', Scott's, or the Freedman–Diaconis rule, or a suggested count, rounded to `pretty()` values. The builder applies `normalization`, overlays, dodges, or stacks the samples (`bar_mode`), or gives each a row (`mode` ridge), and draws each bin between its edges on a numeric axis through the `rtemis.histogram.v1` renderer. `histogramValueLabel(config)` gives the value axis name a host derives when the author sets none.

A density chart takes curves the host computed: per sample, its size and the density at each evaluation point; rtemis.draw estimates with `stats::density()`. `kernelDensity(summary, counts, options)` estimates from a sample's counts in equal bins between its minimum and maximum, as rtemislive bins them in DuckDB: R's `bw.nrd0` or `bw.nrd` bandwidth (`ruleBandwidth()`) or a given one, times `adjust`, any `stats::density()` kernel, at `n` points from `cut` bandwidths below the minimum to `cut` above the maximum. The builder draws each curve with a filled area; `mode` overlays the curves or gives each a ridge row; `DENSITY_VALUE_LABEL` is the value axis name a host derives.

A confusion chart takes the class order and each panel's count matrix (`counts[r][c]`, reference class `r` predicted as class `c`); the builder computes the rates (`confusionRates()`), fills each count by its reference row's fraction toward the theme's sign colors, and adds strips of sensitivity, specificity, PPV, NPV, accuracy, and balanced accuracy with `show_metrics`. The option places panels in percentages: before drawing and on every resize, a host fits it to its canvas and theme with `fitConfusion(echarts, {option, confusion: confusionLayout(config)}, theme, width, height)`, which measures labels and sizes square cells, and `confusionHeight()` gives the height a width needs.

ROC and precision-recall charts take curves the host validated, ordered, and grouped: per group (a split and class) its summary (the pooled AUC or AP, or the mean and SD over resamples) and its chance level, and per curve its group, resample, vertices, and summary. The builders color each group by class or split, set the line type by split, draw per-resample curves thinner and translucent, and add the ROC chance diagonal or each PR group's prevalence baseline in its color; `curveLabel()` names a legend entry and `curveLegendTitle()` the title a host draws on its own row above the entries, and a point's tooltip reads its coordinates, summary, and resample without a formatter, each written by `ddSci()`, a port of rtemis.core's number format. `curveAspect()` gives the square plotting box.

Kaplan–Meier curves (`buildSurvivalOption()`) take curves the host recorded, each starting at its origin, with optional bounds and censored and risk counts. The builder draws right-continuous steps, the confidence band, censor ticks, each curve's labeled median dropline (`survivalMedian()`, as R's survival package defines it), landmark survival as a percent, and the number-at-risk strip, a custom series whose renderer `registerRenderers()` registers.

Gantt charts (`buildGanttOption()`) take a task table: one bar per row from its start to its end on the row of its label, colored by an optional group, with flagged bars outlined and each bar's tooltip text. The bars are a custom series whose renderer `registerRenderers()` registers; `ganttHeight()` gives the chart's height from its number of rows.

Sankey diagrams (`buildSankeyOption()`) take a link table. Rows with the same source and target sum into one link (`sankeyLinks()`); a missing or negative value, or a cycle, stops with an error naming it. Nodes take the palette in order of first appearance, and links run in a gradient from source to target.

Pie charts (`buildPieOption()`) take a table of labels and values. Rows with the same label sum into one slice (`pieSlices()`); a missing label, a missing or negative value, or a zero total stops with an error naming it. `inner_radius` draws a donut, `rose_type` a Nightingale rose, and `label_format` sets each slice label to the name, the value, the percentage, or the name with either.

Time series (`buildTimeSeriesOption()`) take a time column and value columns, sort each series by time, and draw its observations with an optional rolling summary (`rollingValues()`: mean, median, minimum, maximum, or sum over a window of observations), values on a right axis, zeitgeber labels, and shading of flagged runs. The zeitgeber labels are a custom series whose renderer `registerRenderers()` registers.

3D scatters (`buildScatter3DOption()`) take numeric x, y, and z columns and draw ECharts-GL points, paths through consecutive rows of each group, or both, with the surfaces the host fitted (`surfaces`); `appendLayers()` widens the 3D axes to a surface layer. The option needs the echarts-gl extension.

Spectrograms (`buildSpectrogramOption()`) take a spectrum matrix with its times and frequencies, the transform computed by the host, and apply the log frequency scale, ranges, units, and a color scale from the stored viridisLite schemes (`COLORMAPS`), a diverging scale, or given colors.

Calibration charts (`buildCalibrationOption()`) take the bins the host computed per group and draw each group's curve, named with its Brier score, its probability rug, and the diagonal on a square plotting area.

Heatmaps (`buildHeatmapOption()`) take a matrix the host prepared, with its labels, the trees the host computed in R's `hclust` form, cell notes, and color tracks. The builder orders the cells by the trees and draws them on a continuous color scale, with dendrogram panels, branch colors by cluster (`branchColors()`), and color tracks, whose renderers `registerRenderers()` registers. `heatmapHints()` gives the cells' layout for a host that fits square cells. `colorRamp()`, `divergingRamp()`, and `dark3()` reproduce R's `colorRampPalette()`, rtemis.draw's diverging scale, and `hcl.colors(n, "Dark 3")`.

`boxplotStats(values, options)` computes one box's statistics as rtemis.draw does (type-7 quartiles or Tukey's hinges, whiskers to the last value within `whisker` IQR), for a host that summarizes values in memory; with `mean_marker`, the boxplot builder marks each cell's `mean` with a diamond.

Volcano plots and categorical Manhattan bars (`buildSignificanceOption()`) take a family of hypotheses the host adjusted: per hypothesis its label, estimate, and raw and adjusted p-values. The builder transforms the adjusted p-values, draws a zero p-value at a marked cap, colors significant negative and positive results with the theme's sign colors, labels the most significant points of each side, and draws the threshold lines; `significanceAxisNames()` names the axes. The labels carry `rtemisRepelLabels`: `applyRepelLabels(option)` installs `repelLabels()`, a label layout that keeps them clear of each other and of the labeled points, before `setOption()`.

Networks are drawn by Sigma.js rather than ECharts. `buildNetworkGraph(model, config, libraries)` takes a graph model the host built (nodes, edges, and whether they are directed) and a `chart/network/v1` config and returns the Graphology graph with Louvain communities and node positions from the config's layout, one generator seeded by `seed` making every layout reproducible. `networkReducers(graph, config, appearance, hovered)` returns the Sigma node and edge reducers: node sizes by value, colors by community or one color, edge widths by weight, edge colors by sign or blended from their nodes, labels facing the graph's center, and the fading outside a hovered node's neighborhood. The host passes Graphology, Louvain, ForceAtlas2, and the circlepack and random layouts as `libraries`, so the package keeps no runtime dependency on them.

Choropleth maps are drawn by MapLibre GL. `mapFeatures(topology, resolution, feature)` decodes a resolution's regions with topojson-client's `feature`, `normalizeLocation()` maps a location key to its region id (ISO alpha-3 for countries, FIPS for US states and counties), and `joinChoropleth(model, ids)` joins a map model's rows to the regions, stopping on two rows for one region. `buildChoroplethScale(values, config, appearance, interpolators)` classifies the joined values by quantiles, equal intervals, or Jenks natural breaks and samples the config's d3-scale-chromatic scheme; `choroplethColor()` and `choroplethFillExpression()` give a region's fill and MapLibre's `fill-color` expression.

`lineAspect(option, config)` returns the ratio and pixel margins of the plotting box a host fits for a line chart with `square` or `equal_axes`; `aspectHints` maps each chart type that has such a box to its function.

`a3SurfaceHints(document, config, appearance)` returns the height that keeps the layout's row spacing, the plot body height, and the glyph sizes, for a renderer that sizes its surface before drawing.

`ddSci(value, decimals = 2, hi = 1e6)` writes a number as rtemis.core's `ddSci()` does: fixed to `decimals` places, with a nonzero value that rounds to zero at `decimals` places in scientific notation ("4e-04"). The curve, confusion, and fit labels use it; `formatNumber()` writes other tooltip values to three significant digits.

## Themes

`themeOption(tokens)` projects a theme's tokens (fonts, text sizes, surface colors, the palette, sign colors, axis colors, and gridline colors) to the ECharts theme object a host registers with `echarts.registerTheme()` or `chart.setTheme()`. `lightThemeTokens()` and `darkThemeTokens()` give rtemis.draw's light and dark themes, whose projections equal the objects `theme_light()` and `theme_dark()` register. Builders write no path the theme owns.

## In rtemis.draw

rtemis.draw ships the builders as one script, `r/inst/htmlwidgets/lib/draw/rtemis-draw-builders.js`, which defines the global `RtemisDraw` in a browser and is a CommonJS module in Node.js. The `rtemis-draw` widget declares it as a dependency and builds the option of a `BuildRequest` with it; `save_drawing()` builds the same request in Node.js. The `rtemis-graph` widget and the network SVG export build and style the graph with the network functions and the vendored Graphology libraries; the `rtemis-map` widget and the map SVG export join and color the map with the choropleth functions and the vendored topojson-client and d3-scale-chromatic. From the repository root:

```sh
just ts-test          # type check, Biome, Vitest
just ts-bundle        # build, then copy the bundle into the R package
just ts-bundle-check  # fail when the R package's copy differs from a fresh build
```

## Tests

`tests/corpus/` holds cases of (type, data, config, appearance) and the option each produces, serialized as JSON. `RTEMIS_DRAW_CORPUS_WRITE=1 pnpm vitest run` rewrites the expected options after an intended change.

## License

[BSD 3-Clause](LICENSE.txt)
