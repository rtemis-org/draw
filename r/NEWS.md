# rtemis.draw 0.5.0

First release with the **config layer**: a chart can now be described as a
JSON document, validated against a published schema, and rendered from that
document by any interface.

## Chart configs

* New `ChartConfig` classes and `setup_*()` constructors for all 14 chart
  types: `setup_ScatterConfig()`, `setup_BarConfig()`, `setup_LineConfig()`,
  `setup_DensityConfig()`, `setup_HistogramConfig()`, `setup_PieConfig()`,
  `setup_BoxplotConfig()`, `setup_SankeyConfig()`, `setup_GanttConfig()`,
  `setup_HeatmapConfig()`, `setup_NetworkConfig()`, `setup_ChoroplethConfig()`,
  `setup_SpectrogramConfig()`, `setup_A3Config()`.
* A config names its columns; the data is supplied at draw time, either as
  `draw(config, data = df)` or through the config's `dat_path`. `dat_path`
  reads `.csv` (with column names kept exactly as written) and `.rds` (for the
  charts that bind a matrix or an object rather than a table).
* `resolve()` fills in what the data determines -- axis labels from the bound
  column names, limits from the values -- and records each one in the config's
  `origin` map. It is idempotent, never overwrites a value the author set, and
  derives what it can even with no data at all.
* `compile()` turns a config into the render option. It materializes the data
  and resolves the config before dispatching, so no chart type can skip either
  step.

## Reading and writing

* `write_chart_config()` / `read_chart_config()` round-trip a config through
  JSON. `write_chart_config(complete = TRUE)` writes an **output config**:
  every property, unset ones as explicit nulls, with provenance attached --
  the form one interface hands to another, with nothing left to infer.
* `chart_config_to_list()` converts a config to a plain list, shaping each
  value by its declared container so a one-element array stays an array and a
  map stays an object.
* `chart_registry()` is the single list of chart types, read both by
  `read_chart_config()` and by schema generation.

## Schemas

* `chart_schema()` and `chart_dispatcher_schema()` generate the JSON Schemas
  published at `schema.rtemis.org`, from the classes themselves. Each chart type
  publishes a `schema.json` (input config) and a `record.json` (output config),
  the two names the registry already uses, plus a dispatcher of each kind. No
  `default` is ever emitted: a default is what an interface chooses to fill in,
  not a fact about the document.
* `origin` and `writer` are emitted as closed objects -- one entry per settable
  property, all required, no others allowed -- matching the `origin` block every
  other rtemis `record.json` carries. The class validates the same shape.
* `just schemas` writes them to a schema-repo checkout; `just schemas-check`
  verifies they build.

## Square and equally-scaled plots

* `draw_scatter()` and `draw_line()` (and their configs) gained `square` and
  `equal_axes`. `square` makes the plotting box itself square -- excluding axis
  labels and margins. `equal_axes` gives one data unit the same size in pixels
  on both axes.
* Set together they state something about the limits, since the only square box
  with equal scaling is one whose axes span the same interval: both axes are put
  on one common interval, derived from all the values or from whichever limit
  you gave. Giving `xlim` and `ylim` as different intervals is an error rather
  than a silent override. This is the ROC / true-versus-predicted case, where
  the identity line has to run at 45 degrees.
* The box is solved in the browser, which is the only side that knows the
  container width, and re-solved on every resize.

## draw()

* `elementId` is now `element_id`, and the per-chart `color` argument is now
  `palette`, matching the rest of the package's snake_case API.
* `draw()` gained `animation`, which disables ECharts animation for charts
  where redrawing every interactive update is expensive.
* `draw()` now errors on an argument it does not recognize, instead of
  silently dropping it.

## Other

* A named color palette reached the browser as a JSON object where ECharts
  expects an array, and the chart rendered with no colors at all. Every
  palette entering an option is now unnamed, including one the caller supplied.
* `rtemis.draw` no longer defines its own `rtemis_colors`, which masked
  `rtemis.core`'s and made a positional lookup mean different colors depending
  on which package resolved it. It is re-exported from `rtemis.core`.
* `draw_line()` now pads its axes by `pad` like every other value axis. It
  previously used the exact data range, putting the first and last points hard
  against the plot edges while a scatter of the same data got 4% of room.
* Charts re-theme with Quarto's light/dark toggle, which only rewrites body
  classes and fires no event.
* `LineSeries` and `ScatterSeries` gained `symbol_rotate` and `symbol_offset`.

## Dependencies

* `rtemis` moved from Imports to Suggests. The `plot()` and `plot_session()`
  methods for its session classes are registered at load time when it is
  installed.
