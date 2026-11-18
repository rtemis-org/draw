# rtemis.draw 0.5.5

* `draw_treemap()`, `draw_sunburst()`, `draw_radar()`, and `draw_funnel()` draw treemaps, sunburst charts, radar charts, and funnel charts, with the configurations `TreemapConfig`, `SunburstConfig`, `RadarConfig`, and `FunnelConfig`.

* `draw_scatter()`, `draw_fit()`, and `draw()` on a `ScatterConfig` build the chart with the `@rtemis/draw` scatter builder, the one rtemislive uses; `compile()` on a `ScatterConfig` returns a `BuildRequest`. `ScatterConfig` `y` takes several columns.

* `fit` takes `"polynomial"` (`fit_params` `degree`) and `"loess"` (`span`) besides `"glm"`, `"gam"`, and rtemis learners; `fit_per_group = FALSE` fits one curve over every group.

* `draw_line()` and `LineConfig` take the fit arguments of `draw_scatter()` and draw each fit dashed in its line's color, with `aggregate = "none"` and `area = FALSE`.

* `draw_scatter()` gains `color`, `color_scale`, `colormap`, and `color_title`, a continuous color scale for one `y` column, and `point_size`; `size` sets each symbol's area in proportion to its value, the largest at five times `point_size` across, and adds a size key titled by `size_title`. The color scale and the size key sit beside the plot on the side `legend_position` names, or opposite a categorical legend; with a color scale, fits are gray.

* Fit legend entries write R-squared as `R²`.

* `xlab` and `ylab` accept `""`, which draws no axis name, in `draw_scatter()`, `draw_line()`, `draw_bar()`, `draw_boxplot()`, `draw_histogram()`, `draw_density()`, and their configs, `TimeSeriesConfig` included; `NULL` derives the label from the data.

* `draw_add_fit()` accepts named coordinate and bound vectors, such as the columns of a `predict(..., interval = "confidence")` matrix, and drops their names.

* `draw_bar()` and `draw()` on a `BarConfig` build the chart with the `@rtemis/draw` bar builder, the one rtemislive uses; `compile()` on a `BarConfig` returns a `BuildRequest`. `BarConfig` gains `group`, which splits each value column into one series per group, `aggregate` (`"none"`, `"sum"`, `"mean"`, `"median"`, `"count"`), which combines rows that share a category and group, and `ylim`, the value axis limits. A factor group or category keeps its level order. `draw_bar()` takes `zoom` as `TRUE` or `FALSE` and `margins` in pixels.

* `draw_histogram()` and `draw()` on a `HistogramConfig` build the chart with the `@rtemis/draw` histogram builder, the one rtemislive uses; `compile()` on a `HistogramConfig` returns a `BuildRequest`. `draw_histogram()` takes `zoom` as `TRUE` or `FALSE` and `margins` in pixels. The y axis is named after a bar's height unless `ylab` is given: the normalization ("Count", "Percent", ...) or the bin statistic of the column ("Mean X").

* `draw_survival()`, `draw_survfit()`, and `draw()` on a `SurvivalConfig` build the chart with the `@rtemis/draw` survival builder, the one rtemislive uses; `compile()` on a `SurvivalConfig` returns a `BuildRequest`. `show_median` defaults to `TRUE` and labels the median; landmarks read as percents; the risk table draws one row per group in its color under a time axis that labels the risk times; one ungrouped curve draws no legend.

* `draw_gantt()` and `draw()` on a `GanttConfig` build the chart with the `@rtemis/draw` Gantt builder, the one rtemislive's execution timeline uses; `compile()` on a `GanttConfig` returns a `BuildRequest`. With `height` unset, the chart takes 20 pixels per task row plus 96, at least 220, and every row is labeled.

* `draw_sankey()` and `draw()` on a `SankeyConfig` build the chart with the `@rtemis/draw` Sankey builder, the one rtemislive uses; `compile()` on a `SankeyConfig` returns a `BuildRequest`. Links default to a source-to-target gradient (`link_color = "gradient"`, `link_opacity = 0.2`) and highlight a node's links on hover; rows with the same source and target are summed; a missing, infinite, or negative value, or a cycle, stops with an error. `SankeyConfig` gains `link_color`, `link_opacity`, `label_font_size`, and the four `margin_*` members.

* `draw_ts()`, `draw_xt()`, and `draw()` on a `TimeSeriesConfig` build the chart with the `@rtemis/draw` time series builder, the one rtemislive's Time series chart uses; `compile()` on a `TimeSeriesConfig` returns a `BuildRequest`. Shading from `shade_bin` spans each run's first and last observation.

* `draw_heatmap()` and `draw()` on a `HeatmapConfig` build the chart with the `@rtemis/draw` heatmap builder; `compile()` on a `HeatmapConfig` returns a `BuildRequest`. R validates the matrix, masks the triangle, and clusters; the color scale derived from the theme follows the light or dark scheme the widget draws in.

* The choropleth widget and its SVG export join, classify, and color the map with the `@rtemis/draw` choropleth functions, the ones rtemislive uses. `draw_choropleth()`, `draw_map()`, `MapLibreOption`, and `ChoroplethConfig` gain `aggregate` (`"none"`, `"sum"`, `"mean"`, `"median"`), which combines the rows naming one region; with `"none"`, two such rows stop the map with an error. The classes cover the values of the regions the rows name; legend bounds take three significant digits.

* A network node outside the hovered node's neighborhood keeps its own color at 30% opacity.

* The network widget and its SVG export lay out and style the graph with the `@rtemis/draw` network functions, the ones rtemislive uses. `draw_network()`, `draw_graph()`, `SigmaOption`, and `NetworkConfig` gain `seed`, which sets community detection, the force layout's random starting positions, and the random layout.

* `draw_pie()` and `draw()` on a `PieConfig` build the chart with the `@rtemis/draw` pie builder, the one rtemislive uses; `compile()` on a `PieConfig` returns a `BuildRequest`. Values that share a label sum into one slice. `PieConfig` gains `inner_radius` and `show_labels`; `draw_pie()` gains `show_labels`, and its two-element `radius` sets the inner and outer radius.

* `draw_scatter3d()`, `draw_spectrogram()`, `draw_calibration()`, and `draw()` on their configs build the chart with the `@rtemis/draw` builders; `compile()` returns a `BuildRequest`. `draw_add_surface(expand = FALSE)` on a 3D scatter stops when the chart is built if the surface lies outside the axes. The spectrogram's `margins` take pixels, `SpectrogramConfig`'s `overlap` is a fraction of `n_fft`, and its tooltip names each value after its axis. The calibration chart's title is centered. viridisLite is no longer a dependency. The 3D scatter's axes take nice limits and ticks labeled at the interval's decimals.

* `draw_fit()` names the x axis "Observed" by default.

* `draw_boxplot()` and `BoxplotConfig` gain `mean_marker`, a diamond at each box's mean; `draw_varimp(type = "boxplot")` sets it.

* `draw_volcano()`, `draw_manhattan()`, `draw_pvals()`, and `draw()` on a `SignificanceConfig` build the chart with the `@rtemis/draw` significance builder, the one rtemislive uses; `compile()` on a `SignificanceConfig` returns a `BuildRequest`. The middle group reads "Not significant", a significant estimate equal to `x_thresh` is positive, the y axis reads "-log10(Holm-adjusted p)", hover shows the estimate and p-values to three significant digits, and a Manhattan plot of more than 40 outcomes leaves the outcome axis unlabeled.

* `draw_roc()`, `draw_pr()`, and `draw()` on a `ROCConfig` or `PRConfig` build the chart with the `@rtemis/draw` curve builders, the ones rtemislive uses; `compile()` on either returns a `BuildRequest`. A legend title, on its own row above the entries, names the measure ("AUC", or per resample "AUC: mean (SD)"), and each entry names the split, or the class for a multiclass chart, and its value: "Test 0.85", or "Test 0.77 (0.07)" per resample. `digits` defaults to 2, and values are written as `rtemis.core::ddSci()` writes them, so a nonzero SD below 0.005 reads "4e-04".

* `draw_confusion()` and `draw()` on a `ConfusionConfig` build the chart with the `@rtemis/draw` confusion builder, the one rtemislive uses; `compile()` on a `ConfusionConfig` returns a `BuildRequest`. `correct_color` and `incorrect_color` default to the theme's negative (blue) and positive (orange) sign colors.

* `draw_density()` and `draw()` on a `DensityConfig` build the chart with the `@rtemis/draw` density builder, the one rtemislive uses; `compile()` on a `DensityConfig` returns a `BuildRequest`. The y axis is named "Density" unless `ylab` is given. `draw_density()` takes `zoom` as `TRUE` or `FALSE` and `margins` in pixels.

* `draw_a3()` and `draw()` on an `A3Config` build the diagram with the `@rtemis/draw` A3 builder, the one rtemislive uses, in the widget and in `save_drawing()`; the widget rebuilds it when the viewer switches between light and dark. Its tooltip groups annotations under family headings, as in rtemislive.

* `compile()` on an `A3Config` returns a `BuildRequest` (chart type, the A3 document as JSON, the config, and the appearance) in place of an `EChartsOption`.

* `A3Config` requires `n_per_row` of at least 2 and a positive `residue_spacing`, the values the layout accepts.

* `draw_panels()` accepts A3 drawings.

* `A3Config` and `draw_a3()` take `annotation_gap` and `position_label_gap`, the pixel gaps to processing overlays and position labels; `A3Config` accepts `position_every = NULL` for no position labels, and the `chart/a3/v1` schema lists the three `ptm_placement` values.

* `draw_a3()` labels each variant in the residue tooltip as `<from><position><to>`, or `<from><position>del` for a deletion, once per distinct change, with the sources of every record that carries it (`P301L (ClinVar, UniProtKB)`).

* `draw_a3()` and `draw_protein()` require rtemis.a3 0.7.0, whose position and range indices are integer vectors and matrices and whose variants are named lists.

* `draw_panels()`, `draw_confusion()`, `draw_roc()`, and `draw_pr()` follow the width of the viewer or browser window with `width` unset, as other charts do; `draw_panels()`, `draw_roc()`, and `draw_pr()` also fill its height. `save_drawing()` keeps their default pixel sizes.

* A chart with `height` set and `width` unset spans its container. Square and equal-axis charts and square-cell heatmaps fit within a filled window, and a numeric `width` sizes the canvas to that width.

* `draw_pr()` draws precision-recall curves from labels and probabilities or from curve records (pooled, or one curve per resample) as step functions whose area is the average precision, with a prevalence baseline per curve group; `PRConfig`, `setup_PRConfig()`, and the `pr` chart schema describe the chart, and `pr_vertices()` computes its records.

* `draw_roc()` curves start at the first color of the theme palette, as `draw_pr()` curves do, so the two charts color the same groups alike; the chance line keeps `diagonal_color`.

* Grouped charts order a factor `group` by its levels, in series, colors, and legends, and drop levels without drawn rows; other vectors keep first-appearance order. `true_pred_data()` returns a factor `group` as a factor `sample`.

# rtemis.draw 0.5.4

* `draw_mermaid()` renders Mermaid diagram text with 'Pele' as a scrolling SVG widget that follows the light or dark theme and the palette, highlights selected nodes with `node_color` and `fill_alpha`, and exports it to SVG; `MermaidConfig` and `setup_MermaidConfig()` describe a diagram for `draw()` and the chart schemas, reading `.mmd` files through `dat_path`.

* Compatible with S7 releases after 0.2.2. `FitLayer()` and `SurfaceLayer()` declare their coordinate defaults as quoted expressions, which requires rtemis.core 0.4.7 or later. Load, unload, and build hooks use `S7_on_load()`, `S7_on_unload()`, and `S7_on_build()` when S7 provides them, with `methods_register()` for earlier S7 releases. The minimum S7 version is unchanged.

* `draw_harmonograph()` draws harmonograph curves from random or supplied pendulum frequencies, damping rates and phases.

* `draw_scatter3d()` uses perspective projection by default, with configurable camera distance and an orthographic option; SVG export follows the configured camera.

* 3D hover guides use dotted lines and follow the active theme with their value labels, preserving explicit color settings.

* The `rtemis.font_family` option sets the font of every visualization, e.g. `options(rtemis.font_family = "Geist")`; `theme_light()` and `theme_dark()` follow a font family with the system UI font as the fallback. SVG export of diagrams measures labels in the theme font when it is installed and 'systemfonts' is available.

* `Theme`, `theme_light()` and `theme_dark()` gain `negative_color`, `neutral_color` and `positive_color`, used by every chart that colors by sign unless the chart sets its own colors.
* `compile()` gains `theme`, so colors computed before drawing follow the theme the chart will be drawn with; `draw()` passes its theme.
* Significance plots, network edges, and heatmap and spectrogram scales share one colorblind-safe sign pair by default: blue for negative, orange for positive, gray for neither.
* Heatmap and spectrogram color scales fade to the theme's background, including a custom theme's.
* Histograms gain `border_alpha` for a solid bar outline (default 1); unset `fill_alpha` is 0.5 where overlaid groups overlap and 0.75 otherwise.
* Volcano plot annotations are placed clear of each other and of annotated points, in the browser and in SVG export; the significance threshold label sits outside the plotting area.
* `summarize_varimp()` returns the per-variable summary across folds and the ranking that `draw_varimp()` draws.
* `draw_varimp()` treats an omitted variable row as an unavailable score; the `absent` argument is removed, and a producer supplies zero scores explicitly.
* `draw_scatter()`, `draw_bar()`, `draw_histogram()`, `draw_density()`, `draw_boxplot()`, `draw_volcano()`, and `draw_manhattan()` gain `zoom`, as do their chart configurations: a slider with drag panning on the x-axis (the category axis for bars and boxplots), or box-zoom, undo, and reset controls with drag panning on both axes for scatter and volcano plots. Square and equal-axes scatter plots zoom both axes together and omit box zoom.
* Mouse-wheel zoom requires the Shift key in every chart, including `draw_line()`, `draw_ts()`, and `draw_gantt()`; the plain wheel and a one-finger touch drag scroll the page, and two-finger pinch zooms.
* `draw_a3()` takes `zoom`; `enable_zoom` is deprecated and sets `zoom` with a warning.
* Histograms update without transition, so hovering a bar during a legend toggle or zoom leaves the y-axis at the correct scale; `EChartsOption` gains `animation_duration_update`.
* Value axes label their ends only where they fall on a tick, in every chart and after zoom, so axes show rounded labels, such as 2 to 12, rather than 1.8 or 11.47; line, scatter, boxplot, and volcano axes label tick-valued ends.
* SVG export omits zoom sliders and toolbox buttons and returns the slider's space to the plot.
* Charts with a legend or color scale gain `legend_title`, as do their chart configurations: a bold title above left/right legends and vertical color bars, and before the first row of top/bottom legends and horizontal color bars. The default `NULL` draws no title.

# rtemis.draw 0.5.3

* Initial CRAN submission.
