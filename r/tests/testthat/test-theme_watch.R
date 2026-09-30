# test-theme_watch.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# lib/draw/theme_watch.js is the one light/dark detector and theme-change
# watcher behind the ECharts, Sigma.js and MapLibre bindings.

test_that("theme detection and watching behave the same for every binding", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "theme_watch.js")),
      shQuote(system.file(
        "htmlwidgets/lib/draw/theme_watch.js",
        package = "rtemis.draw"
      ))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "theme watch checks passed")
})

test_that("every widget binding declares the shared theme watcher", {
  for (name in c("rtemis-draw", "rtemis-graph", "rtemis-map")) {
    dependencies <- htmlwidgets::getDependency(name, "rtemis.draw")
    names <- vapply(dependencies, `[[`, character(1), "name")
    watcher <- which(names == "rtemis-theme-watch")
    binding <- which(names == paste0(name, "-binding"))
    expect_length(watcher, 1L)
    # Dependencies load in order, so the watcher must precede the binding.
    expect_lt(watcher, binding, label = name)
  }
})
