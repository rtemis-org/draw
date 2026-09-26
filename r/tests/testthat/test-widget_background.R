test_that("standalone viewer backgrounds follow chart themes without changing embedded pages", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "widget_background.js")),
      shQuote(system.file(
        "htmlwidgets/rtemis-draw.js",
        package = "rtemis.draw"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})
