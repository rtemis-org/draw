# Extracted from test-scatter3d.R:202

# test -------------------------------------------------------------------------
skip_if_no_node()
plot <- draw_scatter3d(iris[1:3], group = iris[["Species"]])
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
output <- system2(
  Sys.which("node"),
  c(
    shQuote(test_path("fixtures", "scatter3d_projection.js")),
    shQuote(input)
  ),
  stdout = TRUE,
  stderr = TRUE
)
expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
