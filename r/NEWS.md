# rtemis.draw news

## rtemis.draw 0.5.4

* `Theme`, `theme_light()` and `theme_dark()` gain `negative_color`, `neutral_color` and `positive_color`, used by every chart that colors by sign unless the chart sets its own colors.
* `compile()` gains `theme`, so colors computed before drawing follow the theme the chart will be drawn with; `draw()` passes its theme.
* Significance plots, network edges, and heatmap and spectrogram scales share one colorblind-safe sign pair by default: blue for negative, orange for positive, gray for neither.
* Heatmap and spectrogram color scales fade to the theme's background, including a custom theme's.
* Histograms gain `border_alpha` for a solid bar outline (default 1); unset `fill_alpha` is 0.5 where overlaid groups overlap and 0.75 otherwise.
* Volcano plot annotations are placed clear of each other and of annotated points, in the browser and in SVG export; the significance threshold label sits outside the plotting area.

## rtemis.draw 0.5.3

* Initial CRAN submission.
