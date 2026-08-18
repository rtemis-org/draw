# test-palette-serialization.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# ECharts takes a color palette as a JSON **array**. htmlwidgets serializes with
# `keep_vec_names = TRUE`, so a *named* palette reaches the browser as a JSON
# object and the chart renders with no colors at all -- with nothing but a
# jsonlite deprecation warning to show for it.
#
# `rtemis.core::rtemis_colors` is named on purpose, so this is a live hazard
# every time a palette is passed through, and it is invisible to any test that
# compares R objects rather than the serialized payload. These tests look at the
# JSON.

widget_json <- function(w) {
  as.character(suppressWarnings(htmlwidgets:::toJSON(w[["x"]])))
}

charts <- function() {
  set.seed(1)
  list(
    line = draw_line(1:10, (1:10)^2),
    bar = draw_bar(letters[1:3], c(1, 2, 3)),
    scatter = draw_scatter(1:20, (1:20)^1.2, group = rep(c("a", "b"), 10)),
    density = draw_density(rnorm(50)),
    histogram = draw_histogram(rnorm(50)),
    boxplot = draw_boxplot(iris[["Sepal.Length"]]),
    pie = draw_pie(c(3, 5, 2), c("a", "b", "c")),
    gantt = draw_gantt(data.frame(
      label = c("s1", "s2"),
      start = c(0, 5),
      end = c(5, 9),
      kind = c("k", "k")
    ))
  )
}

test_that("the theme palette serializes as a JSON array, not an object", {
  for (nm in names(charts())) {
    json <- widget_json(charts()[[nm]])
    idx <- regexpr('"theme":{"color":', json, fixed = TRUE)
    expect_gt(idx, 0L)
    expect_identical(substr(json, idx + 17L, idx + 17L), "[", label = nm)
  }
})

test_that("serializing a widget emits no named-vector warning", {
  for (nm in names(charts())) {
    n <- 0L
    withCallingHandlers(
      htmlwidgets:::toJSON(charts()[[nm]][["x"]]),
      warning = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(n, 0L, label = nm)
  }
})

test_that("a user-supplied named palette is stripped too", {
  # A caller passing rtemis_colors directly, or any named subset of it, hits
  # exactly the same failure as the default did.
  w <- draw_bar(letters[1:3], c(1, 2, 3), palette = rtemis_colors[1:3])
  json <- widget_json(w)
  expect_false(grepl('"color":{"teal"', json, fixed = TRUE))
})

test_that("palette_colors strips names and leaves values alone", {
  expect_null(names(palette_colors(rtemis_colors)))
  expect_identical(unname(rtemis_colors), palette_colors(rtemis_colors))
  expect_identical(palette_colors(c("#FFFFFF")), "#FFFFFF")
})


# %% the color vocabulary ----

test_that("every chart taking a categorical palette calls it `palette`", {
  # One concept, one name. `color` used to mean this on most functions and
  # `palette` on the network ones; `palette` is the accurate name, since the
  # value is a vector of colors cycled across series.
  categorical <- c(
    "draw_line",
    "draw_bar",
    "draw_scatter",
    "draw_pie",
    "draw_density",
    "draw_histogram",
    "draw_boxplot",
    "draw_sankey",
    "draw_gantt",
    "draw_graph",
    "draw_network"
  )
  for (fn in categorical) {
    args <- names(formals(get(fn)))
    expect_true("palette" %in% args, label = fn)
    expect_false("color" %in% args, label = fn)
  }
})

test_that("every chart taking a continuous scale calls it `colormap`", {
  # Distinct concept, distinct name: a continuous scale, not a cycled vector.
  for (fn in c(
    "draw_heatmap",
    "draw_spectrogram",
    "draw_map",
    "draw_choropleth"
  )) {
    args <- names(formals(get(fn)))
    expect_true("colormap" %in% args, label = fn)
    expect_false("color_scheme" %in% args, label = fn)
    expect_false("palette" %in% args, label = fn)
  }
})

test_that("role colors keep their own names", {
  # `*_color` names a specific mark rather than the palette, so it is untouched.
  expect_true("node_color" %in% names(formals(draw_network)))
  expect_true("border_color" %in% names(formals(draw_gantt)))
  expect_true("block_color" %in% names(formals(draw_line)))
})

test_that("palette reaches the option for density and histogram", {
  # Both previously had no way to override the theme palette at all.
  pal <- c("#111111", "#222222")
  d <- draw_density(rnorm(50), group = rep(c("a", "b"), 25), palette = pal)
  expect_identical(d[["x"]][["option"]][["color"]], pal)
  h <- draw_histogram(rnorm(50), group = rep(c("a", "b"), 25), palette = pal)
  expect_identical(h[["x"]][["option"]][["color"]], pal)
})
