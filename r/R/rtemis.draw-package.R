#' @name rtemis.draw-package
#'
#' @title rtemis.draw: Interactive Visualization
#'
#' @description
#' Create interactive statistical charts, network graphs and choropleth maps.
#' Charts are rendered with 'ECharts', networks with 'Sigma.js' and maps with
#' 'MapLibre GL JS'. Every visualization is an 'htmlwidgets' widget that
#' automatically follows the light or dark theme of wherever it is shown, and
#' can be exported to a Scalable Vector Graphics (SVG) file.
#'
#' @section Statistical charts:
#' - Distributions and comparisons: [draw_scatter()], [draw_line()],
#'   [draw_bar()], [draw_boxplot()], [draw_violin()], [draw_histogram()],
#'   [draw_density()], [draw_pie()], [draw_heatmap()], [draw_sankey()].
#' - Time: [draw_ts()], [draw_xt()], [draw_gantt()], [draw_spectrogram()].
#' - Three dimensions: [draw_scatter3d()], with [draw_add_surface()].
#' - Model evaluation: [draw_roc()], [draw_calibration()], [draw_confusion()],
#'   [draw_fit()], [draw_varimp()], [draw_metric()], [draw_learning_curve()].
#' - Time-to-event: [draw_survival()], [draw_survfit()].
#' - Significance testing: [draw_volcano()], [draw_manhattan()],
#'   [draw_pvals()].
#' - Sequences: [draw_protein()], [draw_a3()].
#'
#' Add fitted lines, reference lines, bands and labels with [draw_add_fit()]
#' and [draw_annotate()], and arrange several charts into one figure with
#' [draw_panels()].
#'
#' @section Network graphs:
#' [draw_network()] draws a network from an adjacency or correlation matrix, or
#' from an edge list, with 'Sigma.js'.
#'
#' @section Choropleth maps:
#' [draw_choropleth()] colors countries, U.S. states or U.S. counties by a
#' value, with 'MapLibre GL JS'. Boundaries are bundled with the package, so
#' maps need no network access.
#'
#' @section Configuration:
#' Each `draw_*()` function builds a validated 'S7' configuration object,
#' e.g. from [setup_ScatterConfig()], and renders it. Configurations can be
#' written to and read from 'JSON' files with [write_chart_config()] and
#' [read_chart_config()], and [chart_schema()] returns the 'JSON' Schema that
#' such files validate against. [draw()] renders a complete low-level option
#' object directly.
#'
#' @section Themes:
#' By default, visualizations detect whether they are shown on a light or dark
#' background in 'RStudio', 'VS Code', 'Quarto' and the browser, style
#' themselves to match, and switch when the viewer changes theme, e.g. with the
#' 'Quarto' light/dark toggle or the operating system setting. Networks and
#' maps keep their current pan and zoom when they switch. Pass [theme_light()]
#' or [theme_dark()] as `theme` to fix a theme.
#'
#' @section Output:
#' Visualizations display in the 'RStudio' and 'VS Code' viewers, 'R Markdown'
#' and 'Quarto' documents, and 'shiny' apps (see [drawOutput()] and
#' [renderDraw()]). All 'JavaScript' is bundled with the package, so HTML
#' output works offline. [save_drawing()] exports a visualization to an SVG
#' file using 'Node.js'.
#'
#' @import utils S7 rtemis.core
#' @importFrom stats approx
"_PACKAGE"

NULL
