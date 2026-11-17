# @rtemis/draw

ECharts option builders shared by [rtemis.draw](../r) and rtemislive. Each builder takes a chart's data and its config, an instance of the published `chart/<type>/v1` schema, and returns the ECharts option. rtemis.draw's widgets and rtemislive call the same builder, so a chart has one definition in R and in the browser.

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
| `density` | `buildDensityOption(data, config, appearance)` | curves, `{samples}` | `https://schema.rtemis.org/chart/density/v1/schema.json` |
| `histogram` | `buildHistogramOption(data, config, appearance)` | bins, `{edges, samples}` | `https://schema.rtemis.org/chart/histogram/v1/schema.json` |

Tabular data holds the columns the config binds, each an array or typed array, optionally the level order of categorical columns, and optionally the type of a column whose values are not read as stored: `"time"` declares a date or date-time column, held as epoch milliseconds of its wall-clock reading in UTC or as ISO 8601 text, whose offset is ignored. A bar chart labels a time category by its date, or its date and time. `frequency` names a column holding the number of rows each row stands for, when the host has combined rows. A bar chart draws one bar per row; with `group`, one bar per distinct category and group, in level order. When `group` or `aggregate` is set, the data must hold one row per category and group: the host combines rows as `aggregate` says (rtemislive in DuckDB, rtemis.draw in R) before building. With `y` unset, each bar is the number of rows of its category and group: the sum of the `frequency` column, or one per row. `barValueLabel(config, name)` gives the value axis name a host derives when the author sets none: "Count" with `y` unset; for one `y` column its name, as "Mean X", "Median X", or "Sum of X" under an aggregate.

`appearance` holds what every chart type shares and the config does not: the light or dark theme, the font family, the title inset, the legend gap, and animation. Every member of `config` and `appearance` is optional.

A line chart draws one line per `y` column, or per `y` column and group, through the rows in data order: a numeric `x` gets a value axis, a time `x` a time axis in UTC, and any other `x` a category axis. Unset axis limits extend the data range by `pad` on each side, as rtemis.draw's `resolve()` derives them; a stacked chart's y range is that of the sums.

A boxplot takes summaries the host computed: per value column, or per column and group level, the five plotting statistics (whisker ends, quartiles, and median), the count, the range, the points to draw, and, for violins, a density profile; rtemis.draw computes them in R and rtemislive in DuckDB. The builder draws points, violins, paired lines, and comparison brackets as custom series whose renderers `registerRenderers(echarts)` registers by name, once, before drawing.

A histogram takes bins the host computed: one set of edges, and per sample (an `x` column, or a column and group level) its size, the count in each bin `(lower, upper]` with the lowest edge included, the `bin_stat` of the values in each bin, and, for `density`, the kernel density curve; rtemis.draw bins with `graphics::hist()` and rtemislive in DuckDB. `histogramEdges(summary, {breaks, bins})` gives the edges `graphics::hist()` computes from a summary of the values (count, range, standard deviation, quartiles): the class count of Sturges', Scott's, or the Freedman–Diaconis rule, or a suggested count, rounded to `pretty()` values. The builder applies `normalization`, overlays, dodges, or stacks the samples (`bar_mode`), or gives each a row (`mode` ridge), and draws each bin between its edges on a numeric axis through the `rtemis.histogram.v1` renderer. `histogramValueLabel(config)` gives the value axis name a host derives when the author sets none.

A density chart takes curves the host computed: per sample, its size and the density at each evaluation point; rtemis.draw estimates with `stats::density()`. `kernelDensity(summary, counts, options)` estimates from a sample's counts in equal bins between its minimum and maximum, as rtemislive bins them in DuckDB: R's `bw.nrd0` or `bw.nrd` bandwidth (`ruleBandwidth()`) or a given one, times `adjust`, any `stats::density()` kernel, at `n` points from `cut` bandwidths below the minimum to `cut` above the maximum. The builder draws each curve with a filled area; `mode` overlays the curves or gives each a ridge row; `DENSITY_VALUE_LABEL` is the value axis name a host derives.

`lineAspect(option, config)` returns the ratio and pixel margins of the plotting box a host fits for a line chart with `square` or `equal_axes`; `aspectHints` maps each chart type that has such a box to its function.

`a3SurfaceHints(document, config, appearance)` returns the height that keeps the layout's row spacing, the plot body height, and the glyph sizes, for a renderer that sizes its surface before drawing.

## Themes

`themeOption(tokens)` projects a theme's tokens (fonts, text sizes, surface colors, the palette, sign colors, axis colors, and gridline colors) to the ECharts theme object a host registers with `echarts.registerTheme()` or `chart.setTheme()`. `lightThemeTokens()` and `darkThemeTokens()` give rtemis.draw's light and dark themes, whose projections equal the objects `theme_light()` and `theme_dark()` register. Builders write no path the theme owns.

## In rtemis.draw

rtemis.draw ships the builders as one script, `r/inst/htmlwidgets/lib/draw/rtemis-draw-builders.js`, which defines the global `RtemisDraw` in a browser and is a CommonJS module in Node.js. The `rtemis-draw` widget declares it as a dependency and builds the option of a `BuildRequest` with it; `save_drawing()` builds the same request in Node.js. From the repository root:

```sh
just ts-test          # type check, Biome, Vitest
just ts-bundle        # build, then copy the bundle into the R package
just ts-bundle-check  # fail when the R package's copy differs from a fresh build
```

## Tests

`tests/corpus/` holds cases of (type, data, config, appearance) and the option each produces, serialized as JSON. `RTEMIS_DRAW_CORPUS_WRITE=1 pnpm vitest run` rewrites the expected options after an intended change.

## License

[BSD 3-Clause](LICENSE.txt)
