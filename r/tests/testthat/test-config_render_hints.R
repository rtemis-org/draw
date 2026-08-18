# test-config_render_hints.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# HeatmapConfig, SpectrogramConfig and A3Config -- the three charts that derive
# *render hints* from their own content.
#
# Their builders return `list(option =, render =)`. The option is the chart and
# is what `compile()` returns; `render` is what the interface needs to size or
# theme its surface, is handed straight to `draw()`, and is **never serialized**
# -- a height or container geometry computed for one surface is wrong on
# another.

hm <- function() matrix(1:20, nrow = 4)
spec_m <- function() {
  set.seed(1)
  matrix(abs(rnorm(64 * 20)) + 1, nrow = 64)
}


# %% construction ----

test_that("each config carries its own type constant", {
  expect_identical(HeatmapConfig()@type, "heatmap")
  expect_identical(SpectrogramConfig()@type, "spectrogram")
  expect_identical(A3Config()@type, "a3")
})

test_that("no setup function has a mandatory argument", {
  for (fn in list(
    setup_HeatmapConfig,
    setup_SpectrogramConfig,
    setup_A3Config
  )) {
    no_default <- vapply(
      formals(fn),
      function(f) identical(f, quote(expr = )),
      logical(1L)
    )
    expect_false(any(no_default))
  }
})

test_that("enums are declared, so a bad value fails where it is set", {
  expect_error(HeatmapConfig(triangle = "sideways"))
  expect_error(HeatmapConfig(dendro_row_side = "top"))
  expect_error(SpectrogramConfig(freq_scale = "bogus"))
})


# %% compile returns the option only ----

test_that("compile returns the option, matching the vector path", {
  expect_identical(
    compile(setup_HeatmapConfig(), data = hm()),
    heatmap_option(x = hm())[["option"]]
  )
  expect_identical(
    compile(setup_SpectrogramConfig(), data = spec_m()),
    spectrogram_option(x = spec_m())[["option"]]
  )
})

test_that("the whole widget matches the vector path", {
  expect_identical(
    draw(setup_HeatmapConfig(square_cells = TRUE), data = hm())[["x"]],
    draw_heatmap(hm(), square_cells = TRUE)[["x"]]
  )
  expect_identical(
    draw(setup_SpectrogramConfig(), data = spec_m())[["x"]],
    draw_spectrogram(spec_m())[["x"]]
  )
})


# %% render hints reach draw() but never a document ----

test_that("heatmap render hints reach the widget", {
  # The browser needs the cell counts and pixel offsets to size its container.
  w <- draw(setup_HeatmapConfig(square_cells = TRUE), data = hm())
  expect_true(isTRUE(w[["x"]][["squareCells"]]))
  expect_true(all(c("nRows", "nCols", "leftPx") %in% names(w[["x"]])))
})

test_that("render hints are absent from a written config", {
  # They describe the display surface, which is the interface's business.
  path <- tempfile(fileext = ".json")
  write_chart_config(setup_HeatmapConfig(square_cells = TRUE), path)
  json <- paste(readLines(path), collapse = "")
  for (hint in c("nRows", "nCols", "leftPx", "colorLight", "colorDark")) {
    expect_false(grepl(hint, json, fixed = TRUE))
  }
})

test_that("no render target is a property of these configs", {
  for (cls in list(HeatmapConfig, SpectrogramConfig, A3Config)) {
    props <- names(cls@properties)
    expect_false(any(c("width", "height", "theme", "filename") %in% props))
  }
})


# %% round trip ----

test_that("all three round-trip and draw the same", {
  cases <- list(
    list(
      cfg = setup_HeatmapConfig(
        cluster_rows = TRUE,
        colorbar_orient = "horizontal"
      ),
      data = hm()
    ),
    list(
      cfg = setup_SpectrogramConfig(
        n_fft = 128L,
        colormap = "viridis",
        db = FALSE
      ),
      data = spec_m()
    )
  )
  for (case in cases) {
    path <- tempfile(fileext = ".json")
    write_chart_config(case[["cfg"]], path)
    back <- read_chart_config(path)
    expect_identical(back, case[["cfg"]])
    expect_identical(
      compile(back, data = case[["data"]]),
      compile(case[["cfg"]], data = case[["data"]])
    )
  }
})

test_that("the A3 config round-trips without needing an A3 object", {
  # Its bindings are all styling, so the document stands on its own; only
  # drawing needs rtemis.a3.
  cfg <- setup_A3Config(n_per_row = 30L, marker_size = 20, zoom = FALSE)
  path <- tempfile(fileext = ".json")
  write_chart_config(cfg, path)
  expect_identical(read_chart_config(path), cfg)
})


# %% the registry is complete ----

test_that("every chart type is registered and agrees with its class", {
  registry <- chart_registry()
  expect_length(registry, 14L)
  for (type in names(registry)) {
    entry <- registry[[type]]
    expect_identical(chart_type_of(entry[["cls"]]), type)
    expect_true(is.function(get(
      entry[["setup"]],
      envir = asNamespace("rtemis.draw")
    )))
  }
})

test_that("every registered class produces a contract-clean schema", {
  for (type in names(chart_registry())) {
    cls <- chart_registry()[[type]][["cls"]]
    s <- chart_schema(
      cls,
      id = paste0("https://schema.rtemis.org/chart/", type, "/v1/schema.json"),
      title = paste("rtemis", cls@name),
      description = "x"
    )
    expect_identical(as.character(s[["required"]]), "type", label = type)
    expect_false(s[["additionalProperties"]], label = type)
    # No property may emit a default: that is an interface's choice, not a fact
    # about the document.
    for (p in s[["properties"]]) {
      expect_false("default" %in% names(p), label = type)
    }
  }
})
