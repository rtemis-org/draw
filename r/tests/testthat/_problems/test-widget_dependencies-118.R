# Extracted from test-widget_dependencies.R:118

# test -------------------------------------------------------------------------
registry <- jsonlite::read_json(
  test_path("fixtures", "widget_assets.json"),
  simplifyVector = TRUE
)
dependencies <- unlist(
  lapply(
    c("rtemis-draw", "rtemis-graph", "rtemis-map", "rtemis-diagram"),
    function(name) {
      htmlwidgets::getDependency(name, "rtemis.draw")
    }
  ),
  recursive = FALSE
)
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
      "changed: advance published dependency versions; for an unpublished",
      "build, update its fingerprint and rebuild documentation from clean artifacts."
    )
  )
}
