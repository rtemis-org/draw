# Extracted from test-surface_layer.R:177

# test -------------------------------------------------------------------------
skip_if_no_node()
plot <- draw_scatter3d(
  c(0, 1, 0, 1),
  c(0, 0, 1, 1),
  c(0, 1, 1, 0),
  mode = "both"
)
plot <- draw_add_surface(plot, c(1, 0), c(1, 0), matrix(c(3, 2, 1, 0), 2))
input <- tempfile(fileext = ".json")
on.exit(unlink(input))
jsonlite::write_json(
  list(
    option = strip_js(raw_option(plot)),
    module = system.file(
      "htmlwidgets/lib/draw/scatter3d.js",
      package = "rtemis.draw"
    )
  ),
  input,
  auto_unbox = TRUE,
  digits = NA
)
result <- system2(
  Sys.which("node"),
  c(shQuote(test_path("fixtures", "surface_geometry.js")), shQuote(input)),
  stdout = TRUE,
  stderr = TRUE
)
expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
expect_match(paste(result, collapse = "\n"), "3D surface visibility passed")
