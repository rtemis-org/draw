# test-sign_colors.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Every chart that colors by sign reads the same pair, so a hue means the same
# sign in a volcano plot, a network's edges, and a heatmap's scale. The theme
# owns the pair; a chart's own color arguments override it.

custom_sign <- c(
  negative = "#112233",
  neutral = "#445566",
  positive = "#778899"
)
custom_theme <- function(...) {
  Theme(
    negative_color = custom_sign[["negative"]],
    neutral_color = custom_sign[["neutral"]],
    positive_color = custom_sign[["positive"]],
    ...
  )
}


test_that("built-in themes carry the sign colors and keep them out of ECharts", {
  for (theme in list(theme_light(), theme_dark())) {
    expect_identical(
      c(
        negative = theme@negative_color,
        neutral = theme@neutral_color,
        positive = theme@positive_color
      ),
      SIGN_COLORS
    )
    expect_false(any(
      c("negativeColor", "neutralColor", "positiveColor") %in%
        names(to_list(theme))
    ))
  }
})


test_that("theme_sign_colors falls back to SIGN_COLORS for anything unset", {
  expect_identical(theme_sign_colors(NULL), SIGN_COLORS)
  expect_identical(theme_sign_colors(NA), SIGN_COLORS)
  expect_identical(
    theme_sign_colors(list(backgroundColor = "#000")),
    SIGN_COLORS
  )
  expect_identical(theme_sign_colors(custom_theme()), custom_sign)
  partial <- theme_sign_colors(Theme(positive_color = "#000000"))
  expect_identical(partial[["positive"]], "#000000")
  expect_identical(partial[["negative"]], SIGN_COLORS[["negative"]])
  expect_identical(partial[["neutral"]], SIGN_COLORS[["neutral"]])
})


test_that("configs leave sign colors to the theme unless the author sets them", {
  significance <- setup_SignificanceConfig()
  expect_null(significance@negative_color)
  expect_null(significance@neutral_color)
  expect_null(significance@positive_color)
  network <- setup_NetworkConfig()
  expect_null(network@negative_color)
  expect_null(network@positive_color)
  for (f in list(
    setup_SignificanceConfig,
    setup_NetworkConfig,
    graph_option,
    draw_graph,
    draw_network
  )) {
    defaults <- formals(f)
    expect_null(defaults[["negative_color"]])
    expect_null(defaults[["positive_color"]])
  }
  expect_null(formals(setup_SignificanceConfig)[["neutral_color"]])
})


test_that("significance marks take config, then theme, then SIGN_COLORS", {
  data <- data.frame(
    estimate = c(-2, 0.1, 2),
    p_value = c(.001, .5, .001)
  )
  # Mark colors; reference-line series carry none.
  mark_colors <- function(series) {
    unlist(lapply(series, function(s) s[["itemStyle"]][["color"]]))
  }
  colors <- function(config, theme = NULL) {
    option <- to_list(compile(config, data = data, theme = theme))
    mark_colors(option[["series"]])
  }
  config <- setup_SignificanceConfig(p_adjust_method = "none")
  expect_setequal(colors(config), unname(SIGN_COLORS))
  expect_setequal(colors(config, theme_dark()), unname(SIGN_COLORS))
  expect_setequal(colors(config, custom_theme()), unname(custom_sign))
  authored <- setup_SignificanceConfig(
    p_adjust_method = "none",
    positive_color = "#ABCDEF"
  )
  expect_setequal(
    colors(authored, custom_theme()),
    unname(c(custom_sign[c("negative", "neutral")], "#ABCDEF"))
  )
  # The theme reaches compile() through draw().
  widget <- draw(config, data = data, theme = custom_theme())
  expect_setequal(
    mark_colors(widget[["x"]][["option"]][["series"]]),
    unname(custom_sign)
  )
})


test_that("network edges take config, then theme, then SIGN_COLORS", {
  m <- matrix(
    c(0, 1, -1, 1, 0, 1, -1, 1, 0),
    nrow = 3L,
    dimnames = list(letters[1:3], letters[1:3])
  )
  model <- graph_from_matrix(m)
  sigma <- SigmaOption(model = model)
  expect_identical(sigma@negative_color, SIGN_COLORS[["negative"]])
  expect_identical(sigma@positive_color, SIGN_COLORS[["positive"]])
  expect_identical(
    graph_option(model)@positive_color,
    SIGN_COLORS[["positive"]]
  )
  themed <- graph_option(model, theme = custom_theme())
  expect_identical(themed@negative_color, custom_sign[["negative"]])
  expect_identical(themed@positive_color, custom_sign[["positive"]])
  authored <- graph_option(
    model,
    negative_color = "#ABCDEF",
    theme = custom_theme()
  )
  expect_identical(authored@negative_color, "#ABCDEF")
  expect_identical(authored@positive_color, custom_sign[["positive"]])

  compiled <- compile(setup_NetworkConfig(), data = m, theme = custom_theme())
  expect_identical(compiled@negative_color, custom_sign[["negative"]])
  style <- draw_network(m, theme = custom_theme())[["x"]][["style"]]
  expect_identical(style[["negativeColor"]], custom_sign[["negative"]])
  expect_identical(style[["positiveColor"]], custom_sign[["positive"]])
})


test_that("heatmap scales put the theme's sign colors at the same ends", {
  ends <- function(widget, which = "colorLight") {
    colors <- toupper(unlist(widget[["x"]][[which]]))
    c(colors[[1L]], colors[[length(colors)]])
  }
  m <- matrix(c(-2, -1, 1, 2), 2L)
  hm <- function(x, ...) {
    draw_heatmap(x, cluster_rows = FALSE, cluster_cols = FALSE, ...)
  }
  expect_identical(
    ends(hm(m)),
    unname(SIGN_COLORS[c("negative", "positive")])
  )
  # One-sided data keeps the diverging scale's half for its sign, rather than
  # swapping hues.
  expect_identical(ends(hm(-abs(m)))[[1L]], SIGN_COLORS[["negative"]])
  expect_identical(ends(hm(abs(m)))[[2L]], SIGN_COLORS[["positive"]])
  expect_identical(
    ends(hm(m, theme = custom_theme())),
    toupper(unname(custom_sign[c("negative", "positive")]))
  )
  # A config's scale reads the theme draw() is given.
  widget <- draw(
    setup_HeatmapConfig(cluster_rows = FALSE, cluster_cols = FALSE),
    data = m,
    theme = custom_theme()
  )
  expect_identical(
    ends(widget),
    toupper(unname(custom_sign[c("negative", "positive")]))
  )
})


test_that("heatmap and spectrogram scales fade to the theme's background", {
  midpoint <- function(colors) toupper(colors[[(length(colors) + 1L) / 2L]])
  m <- matrix(c(-1, -0.5, 0.5, 1), 2L)
  meta <- draw_heatmap(
    m,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    zlim = c(-1, 1)
  )[["x"]]
  expect_identical(midpoint(unlist(meta[["colorLight"]])), "#FFFFFF")
  expect_identical(midpoint(unlist(meta[["colorDark"]])), "#181818")
  # A theme's own background is the midpoint in both variants, so the browser's
  # light/dark choice cannot pick a ramp built for a different background.
  meta <- draw_heatmap(
    m,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    zlim = c(-1, 1),
    theme = Theme(background_color = "#202830")
  )[["x"]]
  expect_identical(midpoint(unlist(meta[["colorLight"]])), "#202830")
  expect_identical(midpoint(unlist(meta[["colorDark"]])), "#202830")

  pal <- toupper(.spectrogram_palette("diverging", 11L, FALSE, c(-1, 1)))
  expect_identical(pal[[1L]], SIGN_COLORS[["negative"]])
  expect_identical(pal[[11L]], SIGN_COLORS[["positive"]])
  pal <- .spectrogram_palette(
    "diverging",
    11L,
    FALSE,
    c(-1, 1),
    theme = custom_theme(background_color = "#202830")
  )
  expect_identical(
    toupper(c(pal[[1L]], pal[[6L]], pal[[11L]])),
    toupper(unname(c(
      custom_sign[["negative"]],
      "#202830",
      custom_sign[["positive"]]
    )))
  )
  expect_identical(toupper(attr(pal, "dark_variant")[[6L]]), "#202830")
})
