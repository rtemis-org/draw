# test-mermaid.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Mermaid diagrams rendered with Pele: PeleOption, draw_mermaid(),
# MermaidConfig, the rtemis-diagram widget payload, and SVG export.

flow <- c("flowchart LR", "  A[Data] --> B[Model]")


# %% mermaid_source ----

test_that("mermaid_source joins lines and rejects unusable input", {
  expect_identical(mermaid_source(flow), "flowchart LR\n  A[Data] --> B[Model]")
  expect_identical(mermaid_source("pie\n  \"a\" : 1"), "pie\n  \"a\" : 1")
  expect_error(mermaid_source(1), class = "rtemis_type_error")
  expect_error(mermaid_source(character()), class = "rtemis_type_error")
  expect_error(
    mermaid_source(c("flowchart LR", NA)),
    class = "rtemis_type_error"
  )
  expect_error(mermaid_source(c(" ", "")), class = "rtemis_value_error")
  expect_error(
    mermaid_source(strrep("a", PELE_SOURCE_LIMIT + 1L)),
    class = "rtemis_value_error"
  )
})


# %% mermaid_with_title ----

test_that("mermaid_with_title writes the title into frontmatter", {
  src <- "flowchart LR\n  A --> B"
  expect_identical(mermaid_with_title(src, NULL), src)
  expect_identical(
    mermaid_with_title(src, "Workflow"),
    "---\ntitle: \"Workflow\"\n---\nflowchart LR\n  A --> B"
  )
  # Existing frontmatter keeps its other keys; the argument replaces its title.
  fm <- "---\ntitle: Old\nconfig:\n  look: classic\n---\nflowchart LR\n  A --> B"
  expect_identical(
    mermaid_with_title(fm, "New"),
    paste0(
      "---\ntitle: \"New\"\nconfig:\n  look: classic\n---\n",
      "flowchart LR\n  A --> B"
    )
  )
  # Quotes and backslashes are escaped; line breaks become spaces.
  expect_identical(
    mermaid_with_title(src, "a \"b\" \\ c\nd"),
    "---\ntitle: \"a \\\"b\\\" \\\\ c d\"\n---\nflowchart LR\n  A --> B"
  )
})


# %% PeleOption ----

test_that("PeleOption validates its properties", {
  opt <- PeleOption(source = "flowchart LR\n  A --> B")
  expect_identical(opt@font_size, 14)
  expect_null(opt@padding)
  expect_false(opt@auto_direction)
  expect_false(opt@fit)
  expect_null(opt@palette)
  expect_identical(opt@critical_color, rtemis_colors[["red"]])
  expect_error(PeleOption())
  expect_error(PeleOption(source = "pie", font_size = 0))
  expect_error(PeleOption(source = "pie", padding = -1))
  expect_error(PeleOption(source = "pie", fit = NA))
})

test_that("to_list drops unset settings and keeps the palette an array", {
  lst <- to_list(PeleOption(source = "pie", palette = "#ff0000"))
  expect_identical(lst[["source"]], "pie")
  expect_false("padding" %in% names(lst[["style"]]))
  expect_identical(lst[["style"]][["palette"]], list("#ff0000"))
  expect_identical(lst[["style"]][["fontSize"]], 14)
  expect_identical(lst[["style"]][["criticalColor"]], rtemis_colors[["red"]])
  expect_false(
    "palette" %in% names(to_list(PeleOption(source = "pie"))[["style"]])
  )
})


# %% draw_mermaid ----

test_that("draw_mermaid builds an rtemis-diagram widget", {
  w <- draw_mermaid(flow, title = "Workflow", font_size = 16, padding = 4)
  expect_s3_class(w, "rtemis-diagram")
  expect_s3_class(w, "htmlwidget")
  x <- w[["x"]]
  expect_match(x[["source"]], "^---\ntitle: \"Workflow\"\n---\nflowchart LR")
  expect_identical(x[["style"]][["fontSize"]], 16)
  expect_identical(x[["style"]][["padding"]], 4)
  expect_true(x[["autoHeight"]])
  # NULL theme sends both built-in themes for light/dark detection.
  expect_true(x[["autoTheme"]])
  expect_false(is.null(x[["themeDark"]]))
})

test_that("an explicit height turns off document auto-height", {
  w <- draw_mermaid(flow, height = 300)
  expect_false(w[["x"]][["autoHeight"]])
  expect_identical(w[["height"]], 300)
})

test_that("a named palette reaches the browser as an array", {
  wire <- widget_wire(draw_mermaid(
    flow,
    palette = rtemis_colors[c("teal", "orange")]
  ))
  expect_identical(wire[["style"]][["palette"]], list("#6CA3A0", "#F08904"))
  wire1 <- widget_wire(draw_mermaid(flow, palette = "#ff0000"))
  expect_identical(wire1[["style"]][["palette"]], list("#ff0000"))
})

test_that("node_color reach the browser as an object keyed by node id", {
  wire <- widget_wire(draw_mermaid(
    flow,
    node_color = c(A = "#6CA3A0", B = "#BE2E5F"),
    fill_alpha = 0.2
  ))
  expect_identical(
    wire[["style"]][["nodeColor"]],
    list(A = "#6CA3A0", B = "#BE2E5F")
  )
  expect_identical(wire[["style"]][["fillAlpha"]], 0.2)
  # One color stays an object, not a scalar.
  wire1 <- widget_wire(draw_mermaid(flow, node_color = c(A = "#6CA3A0")))
  expect_identical(wire1[["style"]][["nodeColor"]], list(A = "#6CA3A0"))
  expect_false(
    "nodeColor" %in% names(widget_wire(draw_mermaid(flow))[["style"]])
  )
})

test_that("node_color are checked against the source", {
  expect_error(
    draw_mermaid(flow, node_color = c(Z = "#000000")),
    class = "rtemis_value_error"
  )
  expect_error(
    draw_mermaid(flow, node_color = "#000000"),
    class = "rtemis_type_error"
  )
  expect_error(draw_mermaid(
    flow,
    node_color = c(A = "#000000"),
    fill_alpha = 2
  ))
  expect_error(PeleOption(
    source = "pie",
    node_color = c(A = "#000000", A = "#111111")
  ))
})

test_that("draw_mermaid validates its source before building a widget", {
  expect_error(draw_mermaid(NULL), class = "rtemis_type_error")
  expect_error(draw_mermaid(""), class = "rtemis_value_error")
})

test_that("the diagram widget declares Pele, its theme, and the watcher", {
  dependencies <- htmlwidgets::getDependency("rtemis-diagram", "rtemis.draw")
  names <- vapply(dependencies, `[[`, character(1), "name")
  binding <- which(names == "rtemis-diagram-binding")
  for (name in c("pele", "rtemis-diagram-theme", "rtemis-theme-watch")) {
    expect_length(which(names == name), 1L)
    expect_lt(which(names == name), binding, label = name)
  }
  pele <- dependencies[[which(names == "pele")]]
  expect_identical(pele[["version"]], "1.6.0")
})


# %% MermaidConfig ----

test_that("setup_MermaidConfig records type, defaults, and provenance", {
  cfg <- setup_MermaidConfig(font_size = 16)
  expect_identical(cfg@type, "mermaid")
  expect_identical(cfg@font_size, 16)
  expect_null(cfg@palette)
  expect_identical(cfg@origin[["font_size"]], "user")
  expect_identical(cfg@origin[["fit"]], "default")
  expect_error(setup_MermaidConfig(font_size = -1))
})

test_that("compile turns the bound source into a PeleOption", {
  cfg <- setup_MermaidConfig(title = "Design", palette = "#123456", fit = TRUE)
  opt <- compile(cfg, data = flow)
  expect_true(S7::S7_inherits(opt, PeleOption))
  expect_match(opt@source, "title: \"Design\"", fixed = TRUE)
  expect_identical(opt@palette, "#123456")
  colored <- compile(
    setup_MermaidConfig(node_color = c(A = "#6CA3A0"), fill_alpha = 0.3),
    data = flow
  )
  expect_identical(colored@node_color, c(A = "#6CA3A0"))
  expect_identical(colored@fill_alpha, 0.3)
  expect_true(opt@fit)
  expect_error(compile(cfg, data = mtcars), class = "rtemis_type_error")
  expect_error(compile(setup_MermaidConfig()), class = "rtemis_null_input")
})

test_that("draw renders a MermaidConfig from data or a .mmd file", {
  w <- draw(setup_MermaidConfig(), data = flow)
  expect_s3_class(w, "rtemis-diagram")
  expect_identical(w[["x"]][["source"]], paste(flow, collapse = "\n"))

  path <- tempfile(fileext = ".mmd")
  on.exit(unlink(path), add = TRUE)
  writeLines(flow, path)
  w2 <- draw(setup_MermaidConfig(dat_path = path))
  expect_identical(w2[["x"]][["source"]], paste(flow, collapse = "\n"))
})

test_that("a MermaidConfig round-trips through JSON", {
  cfg <- setup_MermaidConfig(
    palette = c("#111111", "#222222"),
    node_color = c(train = "#6CA3A0", test = "#BE2E5F"),
    fill_alpha = 0.25,
    font_size = 12,
    padding = 2,
    auto_direction = TRUE,
    title = "Design"
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_chart_config(cfg, path)
  back <- read_chart_config(path)
  for (nm in setdiff(names(S7::props(cfg)), "writer")) {
    expect_identical(S7::prop(back, nm), S7::prop(cfg, nm), info = nm)
  }
})


# %% diagram_theme.js ----

test_that("the diagram theme maps draw themes to Pele variables", {
  skip_if_no_node()
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "diagram_theme.js")),
      shQuote(system.file(
        "htmlwidgets/lib/draw/diagram_theme.js",
        package = "rtemis.draw"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "diagram theme checks passed")
})


# %% SVG export ----

test_that("SVG export writes literal theme colors at the natural size", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_mermaid(
    c(flow, "  B --> C[Report]"),
    title = "Workflow",
    filename = path
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("var(", svg, fixed = TRUE))
  expect_match(
    svg,
    "<rect width=\"100%\" height=\"100%\" fill=\"#ffffff\"/>",
    fixed = TRUE
  )
  expect_match(svg, ">Workflow<", fixed = TRUE)
  # Natural size: the 800 x 600 export default does not apply.
  width <- regmatches(svg, regexpr("<svg\\b[^>]*", svg))
  expect_false(grepl("width=\"800\"", width, fixed = TRUE))
})

test_that("SVG export names the fonts of Pele's width table", {
  skip_if_no_node()
  old <- options(rtemis.font_family = NULL)
  on.exit(options(old), add = TRUE)
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_mermaid(flow, theme = theme_dark(), filename = path)
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  root <- regmatches(svg, regexpr("<svg\\b[^>]*", svg))
  expect_match(
    root,
    "font-family:Helvetica, Arial, 'Liberation Sans', Arimo, sans-serif",
    fixed = TRUE
  )
  expect_false(grepl("system-ui", svg, fixed = TRUE))
})

# One row per face of a family whose faces share the file `path`.
font_table <- function(family, path) {
  data.frame(
    path = path,
    index = 0L,
    family = family,
    weight = c("normal", "bold"),
    italic = FALSE,
    variable = FALSE
  )
}

test_that("font_metrics measures the first installed family of the stack", {
  skip_if_not_installed("systemfonts")
  installed <- systemfonts::system_fonts()
  skip_if(nrow(installed) == 0L, "no installed fonts")
  fonts <- font_table("Body Font", installed[["path"]][[1L]])
  m <- font_metrics(
    "Missing Font, 'Body Font', sans-serif",
    "A \u2192 B",
    fonts
  )
  expect_identical(m[["family"]], "Body Font")
  chars <- strsplit(m[["chars"]], "")[[1L]]
  expect_true(all(c("A", " ", "~", "\u2026", "\u2192") %in% chars))
  expect_length(m[["widths"]], 4L)
  for (w in m[["widths"]]) {
    expect_length(w, length(chars))
    expect_true(all(is.finite(w) & w >= 0))
  }
  # The family has no italic faces, so italic takes the regular widths.
  expect_identical(m[["widths"]][[3L]], m[["widths"]][[1L]])
})

test_that("font_metrics stops at a generic family or an uninstalled stack", {
  fonts <- font_table("Body Font", "unused.ttf")
  expect_null(font_metrics("system-ui, 'Body Font'", "A", fonts))
  expect_null(font_metrics("Missing Font", "A", fonts))
  expect_null(font_metrics(NULL, "A", fonts))
})

test_that("SVG export measures labels with the supplied font metrics", {
  skip_if_no_node()
  # A diagram widget with wide and narrow synthetic metrics for one family.
  export <- function(em) {
    widget <- draw_mermaid(flow, theme = theme_light())
    scene <- widget[["x"]]
    chars <- intToUtf8(32:126, multiple = TRUE)
    scene[["fontMetrics"]] <- list(
      family = "Test Sans",
      chars = paste(chars, collapse = ""),
      widths = rep(list(rep(em, length(chars))), 4L)
    )
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path), add = TRUE)
    save_svg_ssr(
      NULL,
      widget[["x"]][["theme"]],
      path,
      NULL,
      NULL,
      backend = "diagram",
      scene = scene
    )
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  svg_width <- function(svg) {
    root <- regmatches(svg, regexpr("<svg\\b[^>]*", svg))
    as.numeric(sub(".*\\bwidth=\"([0-9.]+)\".*", "\\1", root))
  }
  narrow <- export(400)
  wide <- export(1000)
  expect_gt(svg_width(wide), svg_width(narrow))
  expect_match(
    wide,
    "font-family:'Test Sans', Helvetica, Arial, 'Liberation Sans', Arimo, sans-serif",
    fixed = TRUE
  )
})

test_that("SVG export names the theme font when it is installed", {
  skip_if_no_node()
  skip_if_not_installed("systemfonts")
  installed <- setdiff(
    unique(systemfonts::system_fonts()[["family"]]),
    c("Helvetica", "Arial")
  )
  skip_if(length(installed) == 0L, "no installed fonts")
  family <- installed[[1L]]
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_mermaid(
    flow,
    theme = theme_light(font_family = paste0("'", family, "'")),
    filename = path
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(
    svg,
    paste0("font-family:'", family, "', Helvetica"),
    fixed = TRUE
  )
})

test_that("SVG export follows an explicit dark theme and the palette", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_mermaid(
    "pie\n  \"a\" : 2\n  \"b\" : 1",
    palette = c("#123456", "#654321"),
    theme = theme_dark(),
    filename = path
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("var(", svg, fixed = TRUE))
  expect_match(svg, "fill=\"#181818\"", fixed = TRUE)
  expect_match(svg, "#123456", fixed = TRUE)
  expect_match(svg, "#654321", fixed = TRUE)
})

test_that("SVG export colors the named nodes", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  draw_mermaid(
    flow,
    node_color = c(A = "#6CA3A0"),
    fill_alpha = 0.2,
    filename = path
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("var(", svg, fixed = TRUE))
  node_a <- regmatches(svg, regexpr("data-id=\"A\"[^>]*>.*?</g>", svg))
  expect_match(node_a, "fill=\"#6CA3A0\" fill-opacity=\"0.2\"", fixed = TRUE)
  expect_match(node_a, "stroke=\"#6CA3A0\"", fixed = TRUE)
  node_b <- regmatches(svg, regexpr("data-id=\"B\"[^>]*>.*?</g>", svg))
  expect_false(grepl("#6CA3A0", node_b, fixed = TRUE))
})

test_that("SVG export reports Mermaid syntax errors and keeps the file", {
  skip_if_no_node()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  writeLines("previous", path)
  expect_error(
    draw_mermaid("flowchart LR\n  A -->", filename = path),
    class = "rtemis_export_error"
  )
  expect_identical(readLines(path), "previous")
})
