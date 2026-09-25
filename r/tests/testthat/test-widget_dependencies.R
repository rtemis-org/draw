test_that("changed widget assets do not overwrite prior frozen dependencies", {
  site_libs <- tempfile("widget-assets-")
  dir.create(site_libs)
  on.exit(unlink(site_libs, recursive = TRUE), add = TRUE)
  # These names are the previously shared cache identities. A newly rendered
  # chapter must not replace the bytes used by an older frozen chapter.
  old_files <- file.path(
    site_libs,
    c(
      "rtemis-draw-binding-0.5.2/rtemis-draw.js",
      "rtemis-panel-layout-1.0.0/panels.js"
    )
  )
  for (path in old_files) {
    dir.create(dirname(path))
    writeLines("previously cached asset", path)
  }
  dependencies <- htmlwidgets::getDependency("rtemis-draw", "rtemis.draw")
  copied <- lapply(
    dependencies,
    htmltools::copyDependencyToDir,
    outputDir = site_libs
  )
  for (path in old_files) {
    expect_identical(readLines(path), "previously cached asset")
  }
  headers <- htmltools::renderDependencies(copied)
  expect_match(headers, "/confusion.js", fixed = TRUE)
  expect_false(grepl("rtemis-draw-binding-0.5.2/", headers, fixed = TRUE))
  expect_false(grepl("rtemis-panel-layout-1.0.0/", headers, fixed = TRUE))
})

test_that("declared browser dependencies initialize an ordinary chart", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  dependencies <- htmlwidgets::getDependency("rtemis-draw", "rtemis.draw")
  scripts <- unlist(
    lapply(dependencies, function(dependency) {
      if (!startsWith(dependency[["name"]], "rtemis-")) {
        return(NULL)
      }
      file.path(
        system.file(dependency[["src"]][["file"]], package = "rtemis.draw"),
        dependency[["script"]]
      )
    }),
    use.names = FALSE
  )
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  jsonlite::write_json(scripts, input, auto_unbox = FALSE)
  output <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "widget_dependencies.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "Declared dependencies render")
})

test_that("versioned widget scripts match their immutable recorded contents", {
  registry <- jsonlite::read_json(
    test_path("fixtures", "widget_assets.json"),
    simplifyVector = TRUE
  )
  dependencies <- htmlwidgets::getDependency("rtemis-draw", "rtemis.draw")
  for (dependency in dependencies) {
    if (!startsWith(dependency[["name"]], "rtemis-")) {
      next
    }
    identity <- paste(dependency[["name"]], dependency[["version"]], sep = "-")
    recorded <- registry[[identity]]
    expect_false(
      is.null(recorded),
      info = paste("Record the new asset identity:", identity)
    )
    if (is.null(recorded)) {
      next
    }
    scripts <- dependency[["script"]]
    expect_identical(scripts, recorded[["scripts"]], info = identity)
    paths <- file.path(
      system.file(dependency[["src"]][["file"]], package = "rtemis.draw"),
      scripts
    )
    fingerprints <- vapply(
      paths,
      function(path) {
        # Normalize text line endings so Windows checkouts use the same record.
        text <- paste(
          readLines(path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n"
        )
        normalized <- tempfile()
        on.exit(unlink(normalized), add = TRUE)
        writeBin(charToRaw(enc2utf8(text)), normalized)
        unname(tools::md5sum(normalized))
      },
      character(1),
      USE.NAMES = FALSE
    )
    expect_identical(
      fingerprints,
      recorded[["md5"]],
      info = paste(
        identity,
        "changed: advance the dependency version and add a new registry entry;",
        "do not rewrite the previous identity."
      )
    )
  }
})
