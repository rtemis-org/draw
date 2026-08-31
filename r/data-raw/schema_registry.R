# schema_registry.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Which class backs which published chart schema, plus the descriptions that
# are not derivable from the class. Read by `generate_schemas.R`.
#
# No entry here restates a property: every one is generated from its
# `PropertySpec` by `chart_schema()`. What lives here is the one-line
# description of what each chart *is*, which nothing in the class knows.
#
# The discriminator is `type`, matching the four rtemis families that already
# use it (resampler, tuner, explainer, conformal) rather than `algorithm`,
# which is the registry default there.
#
# Requires the package to be loaded first: the entries reference class objects.

# %% chart_family ----
# Title and description for the family dispatcher.
chart_family <- list(
  title = "rtemis chart",
  description = paste0(
    "Language-independent config for an rtemis chart. A chart type plus its ",
    "type-specific data binding, semantics and appearance. The same config ",
    "drives rtemis.draw (R) and rtemislive (TypeScript) to the same chart."
  )
)


# %% chart_descriptions ----
# What each chart is, keyed by `type`. The class list itself is NOT repeated
# here: it comes from `chart_registry()`, the same one `read_chart_config()`
# uses, so a chart type cannot exist for reading and not for publishing.
chart_descriptions <- c(
  scatter = paste0(
    "Scatter chart: points at (x, y), optionally sized, grouped, and ",
    "overlaid with a fit and its standard-error band."
  ),
  bar = paste0(
    "Bar chart: one bar per category, with one series per bound value column, ",
    "grouped or stacked, vertical or horizontal."
  ),
  density = paste0(
    "Kernel density chart: the estimated distribution of one column, ",
    "optionally split into one curve per level of a grouping column. ",
    ""
  ),
  histogram = paste0(
    "Histogram: binned counts of one column, optionally split into one ",
    "series per level of a grouping column."
  ),
  line = paste0(
    "Line chart: one line per bound value column against a shared x column, ",
    "optionally smoothed, filled, and shaded with background bands. ",
    ""
  ),
  pie = paste0(
    "Pie chart: one slice per row, sized by a value column and named by a ",
    "label column, optionally drawn as a Nightingale rose."
  ),
  boxplot = paste0(
    "Boxplot: one box per bound column, optionally split into one box per ",
    "level of a grouping column."
  ),
  gantt = paste0(
    "Gantt chart: one bar per task, bound to a table with a label, a start ",
    "and an end, optionally colored by group and outlined by a flag column. ",
    ""
  ),
  heatmap = paste0(
    "Heatmap: a numeric matrix shaded by a continuous color scale, optionally ",
    "reordered by hierarchical clustering with dendrograms. ",
    ""
  ),
  a3 = paste0(
    "Annotated protein diagram: a sequence laid out in rows with regions, ",
    "modifications and variants marked. Bound to an A3 object from rtemis.a3. ",
    ""
  ),
  spectrogram = paste0(
    "Spectrogram: a signal's frequency content over time, from a raw signal ",
    "via a short-time Fourier transform or from a precomputed matrix. ",
    ""
  ),
  network = paste0(
    "Network graph: nodes and edges bound to an adjacency matrix or edge ",
    "list, laid out and optionally colored by detected community. Rendered ",
    "with Sigma.js."
  ),
  choropleth = paste0(
    "Choropleth map: regions shaded by a bound value column, keyed by a ",
    "location column. Rendered with MapLibre."
  ),
  sankey = paste0(
    "Sankey diagram: flows between nodes, bound to a table of links with a ",
    "source, a target and a magnitude."
  )
)
