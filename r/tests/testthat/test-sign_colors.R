# test-sign_colors.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Every chart that colors by sign reads the same pair, so a hue means the same
# sign in a volcano plot, a network's edges, and a heatmap's scale.

test_that("significance and network defaults use the sign colors", {
  significance <- setup_SignificanceConfig()
  expect_identical(
    c(
      negative = significance@negative_color,
      neutral = significance@neutral_color,
      positive = significance@positive_color
    ),
    SIGN_COLORS
  )
  for (f in list(
    setup_SignificanceConfig,
    setup_NetworkConfig,
    graph_option,
    draw_graph,
    draw_network
  )) {
    defaults <- formals(f)
    expect_identical(
      eval(defaults[["negative_color"]]),
      SIGN_COLORS[["negative"]]
    )
    expect_identical(
      eval(defaults[["positive_color"]]),
      SIGN_COLORS[["positive"]]
    )
  }
  expect_identical(
    eval(formals(setup_SignificanceConfig)[["neutral_color"]]),
    SIGN_COLORS[["neutral"]]
  )
  network <- setup_NetworkConfig()
  expect_identical(network@negative_color, SIGN_COLORS[["negative"]])
  expect_identical(network@positive_color, SIGN_COLORS[["positive"]])
  sigma <- SigmaOption(model = list(nodes = list()))
  expect_identical(sigma@negative_color, SIGN_COLORS[["negative"]])
  expect_identical(sigma@positive_color, SIGN_COLORS[["positive"]])
})


test_that("heatmap scales put the same sign at the same end", {
  ends <- function(widget) {
    meta <- widget[["x"]]
    colors <- toupper(unlist(meta[["colorLight"]]))
    c(colors[[1L]], colors[[length(colors)]])
  }
  m <- matrix(c(-2, -1, 1, 2), 2L)
  expect_identical(
    ends(draw_heatmap(m, cluster_rows = FALSE, cluster_cols = FALSE)),
    c(SIGN_COLORS[["negative"]], SIGN_COLORS[["positive"]])
  )
  # One-sided data keeps the diverging scale's half for its sign, rather than
  # swapping hues.
  expect_identical(
    ends(draw_heatmap(-abs(m), cluster_rows = FALSE, cluster_cols = FALSE))[[
      1L
    ]],
    SIGN_COLORS[["negative"]]
  )
  expect_identical(
    ends(draw_heatmap(abs(m), cluster_rows = FALSE, cluster_cols = FALSE))[[
      2L
    ]],
    SIGN_COLORS[["positive"]]
  )
  spectrogram <- toupper(.spectrogram_palette(
    "diverging",
    11L,
    FALSE,
    c(-1, 1)
  ))
  expect_identical(spectrogram[[1L]], SIGN_COLORS[["negative"]])
  expect_identical(spectrogram[[11L]], SIGN_COLORS[["positive"]])
})
