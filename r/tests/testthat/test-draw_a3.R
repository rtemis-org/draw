# test-draw_a3.R
# Tests for draw_a3(), A3Config, and the A3 build request. The ECharts option
# is built by @rtemis/draw in the widget; its content is pinned by the
# TypeScript package's corpus, and checked here end to end through Node.

# ── Helpers ───────────────────────────────────────────────────────────────────

a3_fixture <- function() {
  rtemis.a3::create_A3(
    "MAEPRQEFEVMEDHAGTYGLGDRK",
    region = list(
      Domain = rtemis.a3::annotation_range(matrix(c(3L, 10L), ncol = 2))
    ),
    site = list(Site = rtemis.a3::annotation_position(8L)),
    ptm = list(Phosphorylation = rtemis.a3::annotation_position(5L)),
    processing = list(Cleavage = rtemis.a3::annotation_position(17L))
  )
}

bundle_path <- function() {
  system.file(
    "htmlwidgets/lib/draw/rtemis-draw-builders.js",
    package = "rtemis.draw"
  )
}

# Build the option of a widget's build request in Node, as the widget does.
node_build <- function(widget, positions = integer(), dark = FALSE) {
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  jsonlite::write_json(
    list(
      payload = widget[["x"]],
      builders = bundle_path(),
      positions = I(positions),
      dark = dark
    ),
    input,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  out <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "a3_build.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(out, "status"))) {
    stop(paste(out, collapse = "\n"))
  }
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

series_names <- function(option) {
  vapply(option[["series"]], function(s) s[["name"]] %||% "", character(1))
}

# ── Build request ─────────────────────────────────────────────────────────────

test_that("draw_a3 rejects non-A3 input", {
  skip_if_not_installed("rtemis.a3")
  expect_error(draw_a3(LineStyle()), "A3 object")
})

test_that("draw_a3 sends the A3 document, its config, and the appearance", {
  skip_if_not_installed("rtemis.a3")
  a <- a3_fixture()
  w <- draw_a3(a, n_per_row = 12L, ptm_placement = "outerRadial", title = "T")
  expect_s3_class(w, "htmlwidget")
  expect_null(w[["x"]][["option"]])
  build <- w[["x"]][["build"]]
  expect_identical(build[["type"]], "a3")
  document <- jsonlite::fromJSON(build[["data"]], simplifyVector = FALSE)
  expect_identical(document[["sequence"]], "MAEPRQEFEVMEDHAGTYGLGDRK")
  expect_identical(build[["config"]][["n_per_row"]], 12L)
  expect_identical(build[["config"]][["ptm_placement"]], "outerRadial")
  expect_identical(build[["config"]][["title"]], "T")
  expect_identical(build[["appearance"]], list(legendGap = 40))
})

test_that("compile() on an A3Config returns a BuildRequest with every config member", {
  skip_if_not_installed("rtemis.a3")
  request <- compile(setup_A3Config(position_every = NULL), data = a3_fixture())
  expect_true(S7::S7_inherits(request, BuildRequest))
  members <- c(
    "type",
    "n_per_row",
    "position_every",
    "ptm_placement",
    "zoom",
    "residue_spacing",
    "marker_size",
    "font_size",
    "line_width",
    "show_markers",
    "show_labels",
    "annotation_gap",
    "position_label_gap",
    "region_opacity",
    "residue_fill",
    "residue_stroke",
    "label_color",
    "pos_label_color",
    "variant_color",
    "disease_variant_color",
    "title"
  )
  expect_setequal(names(request@config), members)
  expect_identical(request@config[["position_every"]], NA)
  expect_identical(request@config[["residue_fill"]], NA)
})

test_that("A3 surface flags follow height and grid", {
  skip_if_not_installed("rtemis.a3")
  a <- a3_fixture()
  w <- draw_a3(a)
  expect_identical(w[["x"]][["a3"]], list(autoHeight = TRUE, autoGrid = TRUE))
  expect_true(is.numeric(w[["height"]]))
  expect_false(draw_a3(a, height = 400)[["x"]][["a3"]][["autoHeight"]])
  gridded <- draw_a3(a, grid = Grid(left = 40))
  expect_false(gridded[["x"]][["a3"]][["autoGrid"]])
  expect_identical(
    gridded[["x"]][["build"]][["appearance"]][["grid"]][["left"]],
    40
  )
  expect_identical(
    draw(setup_A3Config(), data = a)[["x"]][["a3"]],
    w[["x"]][["a3"]]
  )
  expect_error(draw_a3(a, grid = list(left = 40)), "Grid")
})

test_that("draw_a3 enable_zoom is a deprecated alias of zoom", {
  skip_if_not_installed("rtemis.a3")
  expect_warning(
    w <- draw_a3(a3_fixture(), enable_zoom = FALSE),
    "deprecated"
  )
  expect_false(w[["x"]][["build"]][["config"]][["zoom"]])
})

test_that("A3Config validates the layout members", {
  expect_error(setup_A3Config(n_per_row = 1L))
  expect_error(setup_A3Config(residue_spacing = 0))
  expect_error(setup_A3Config(ptm_placement = "sideways"))
  expect_error(setup_A3Config(annotation_gap = -1))
  config <- setup_A3Config(
    position_every = NULL,
    annotation_gap = 10,
    position_label_gap = 12
  )
  expect_null(config@position_every)
  expect_identical(config@annotation_gap, 10)
  expect_identical(config@position_label_gap, 12)
})

# ── Built option, through Node ────────────────────────────────────────────────

test_that("the built option carries the config", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  a <- a3_fixture()
  built <- node_build(draw_a3(a, title = "Tau"))
  names <- series_names(built[["option"]])
  for (name in c(
    "Primary structure",
    "Domain",
    "Site",
    "Phosphorylation",
    "Cleavage",
    "Position labels"
  )) {
    expect_true(name %in% names, info = name)
  }
  expect_identical(built[["option"]][["title"]][["text"]], "Tau")
  expect_length(built[["option"]][["dataZoom"]], 2L)
  expect_identical(built[["hints"]][["legendGap"]], 40L)

  bare <- node_build(draw_a3(
    a,
    show_markers = FALSE,
    position_every = NULL,
    zoom = FALSE
  ))
  names <- series_names(bare[["option"]])
  expect_false("Primary structure" %in% names)
  expect_false("Position labels" %in% names)
  expect_null(bare[["option"]][["dataZoom"]])

  gap <- node_build(draw_a3(a, position_label_gap = 12))
  positions <- gap[["option"]][["series"]][[which(
    series_names(gap[["option"]]) == "Position labels"
  )]]
  expect_identical(positions[["data"]][[1]][["label"]][["distance"]], 12L)
})

test_that("config colors replace theme colors in the built option", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  built <- node_build(draw_a3(
    a3_fixture(),
    residue_fill = "#123456",
    residue_stroke = "#654321"
  ))
  backbone <- built[["option"]][["series"]][[which(
    series_names(built[["option"]]) == "Primary structure"
  )]]
  expect_identical(backbone[["itemStyle"]][["color"]], "#123456")
  expect_identical(backbone[["itemStyle"]][["borderColor"]], "#654321")
})

test_that("residue tooltips label each variant change once with its sources", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  a <- rtemis.a3::create_A3(
    "MAEPRQEFEV",
    variant = list(
      rtemis.a3::annotation_variant(
        5L,
        info = list(from = "R", to = "L", source = "ClinVar")
      ),
      rtemis.a3::annotation_variant(
        5L,
        info = list(from = "R", to = "L", source = "UniProtKB")
      ),
      rtemis.a3::annotation_variant(
        5L,
        info = list(from = "R", to = "H", source = "UniProtKB")
      ),
      rtemis.a3::annotation_variant(
        8L,
        info = list(
          from = "F",
          type = "deletion",
          source = c("ClinVar", "UniProtKB")
        )
      )
    )
  )
  tips <- node_build(draw_a3(a), positions = c(5L, 8L))[["tooltips"]]
  expect_identical(
    lengths(regmatches(
      tips[["5"]],
      gregexpr("R5L", tips[["5"]], fixed = TRUE)
    )),
    1L
  )
  expect_match(tips[["5"]], "R5L ClinVar, UniProtKB", fixed = TRUE)
  expect_match(tips[["5"]], "R5H UniProtKB", fixed = TRUE)
  expect_match(tips[["8"]], "F8del ClinVar, UniProtKB", fixed = TRUE)
})

# ── Widget and SVG ────────────────────────────────────────────────────────────

test_that("A3 builds in the widget binding with fitted geometry across surfaces", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  w <- draw_a3(a3_fixture())
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  jsonlite::write_json(
    list(
      payload = w[["x"]],
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      ),
      builders = bundle_path(),
      binding = system.file(
        "htmlwidgets/rtemis-draw.js",
        package = "rtemis.draw"
      )
    ),
    input,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  out <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "a3_geometry.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  expect_match(paste(out, collapse = "\n"), "A3 spacing")
})

test_that("A3 legend headings and annotations survive SVG export", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  w <- draw_a3(a3_fixture(), n_per_row = 12L, font_size = 12, marker_size = 16)
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  for (widget in list(w, draw_panels(list(w, w), ncol = 2))) {
    save_drawing(widget, path, width = 1400, height = 600)
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    for (label in c(
      "Regions",
      "PTMs",
      "Processing",
      "Domain",
      "Phosphorylation",
      "Cleavage"
    )) {
      expect_match(svg, paste0(">", label, "</text>"), fixed = TRUE)
    }
    expect_false(grepl("<image|__legend_heading|\\{heading\\|", svg))
  }
})

test_that("A3 SVG export fits its height to the layout unless one is given", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  w <- draw_a3(a3_fixture())
  hints <- node_build(w)[["hints"]]
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  svg_height <- function() {
    head <- paste(readLines(path, n = 1L, warn = FALSE), collapse = "")
    as.numeric(sub('.*height="([0-9.]+)".*', "\\1", head))
  }
  save_drawing(w, path, width = 900)
  expect_gte(svg_height(), ceiling(hints[["height"]]))
  save_drawing(w, path, width = 900, height = 500)
  expect_identical(svg_height(), 500)
})

test_that("A3 empty display choices and zero marker configs still export", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  a <- rtemis.a3::create_A3("MAEPR")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  blank <- draw_a3(a, show_markers = FALSE, show_labels = FALSE)
  expect_length(node_build(blank)[["option"]][["series"]], 0L)
  expect_no_error(save_drawing(blank, path))
  expect_no_error(save_drawing(
    draw(setup_A3Config(marker_size = 0), data = a),
    path
  ))
})

test_that("A3 SVG export takes its colors from the theme's a3 tokens", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  theme <- theme_light(a3 = A3Theme(residue_stroke_color = "#a52a2a"))
  save_drawing(
    draw_a3(a3_fixture(), n_per_row = 12L, theme = theme),
    path,
    width = 1200,
    height = 600
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(svg, "#a52a2a", fixed = TRUE)
  # A config color member replaces the theme's.
  save_drawing(
    draw_a3(
      a3_fixture(),
      n_per_row = 12L,
      residue_stroke = "#2a52a5",
      theme = theme
    ),
    path,
    width = 1200,
    height = 600
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(svg, "#2a52a5", fixed = TRUE)
  expect_false(grepl("#a52a2a", svg, fixed = TRUE))
})
