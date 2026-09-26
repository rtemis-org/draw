# Backend contracts exercise shared calculations and real SVG geometry, including
# failed exports preserving an existing destination.
test_that("network and map scene calculations have portable vector contracts", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "vector_geometry.js")),
      shQuote(system.file(package = "rtemis.draw"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "Vector contracts passed")
})

test_that("network and map exports retain vector geometry, themes, and labels", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  edges <- data.frame(
    source = c("A & B", "C", "D"),
    target = c("C", "D", "A & B"),
    weight = c(1, -0.5, 0.7)
  )
  graph <- draw_network(
    edges,
    layout = "circular",
    title = "Network <one>",
    theme = theme_dark()
  )
  map <- draw_choropleth(
    data.frame(region = c("CA", "NY", "TX"), value = c(1, 3, 5)),
    "region",
    "value",
    resolution = "state",
    title = "State values",
    theme = theme_dark()
  )
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  for (widget in list(graph, map)) {
    expect_invisible(save_drawing(widget, path, width = 800, height = 600))
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(svg, "#181818", fixed = TRUE)
    expect_match(svg, "<path", fixed = TRUE)
    expect_false(grepl("<image|<foreignObject|NaN|Infinity", svg))
    if (inherits(widget, "rtemis-graph")) {
      expect_match(svg, 'data-node-id="A &amp; B"', fixed = TRUE)
      expect_match(svg, "Network &lt;one&gt;", fixed = TRUE)
      expect_equal(lengths(regmatches(svg, gregexpr("<circle ", svg))), 3L)
    } else {
      expect_match(svg, 'data-region-id="06"', fixed = TRUE)
      expect_match(svg, "State values", fixed = TRUE)
      expect_false(grepl("matched", svg, fixed = TRUE))
    }
  }
  writeLines("existing figure", path)
  map[["x"]][["geo"]][["topojson"]] <- "invalid JSON"
  expect_error(save_drawing(map, path), class = "rtemis_export_error")
  expect_identical(readLines(path), "existing figure")
})
