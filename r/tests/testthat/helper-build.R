# Build the ECharts option of a drawing or BuildRequest whose option
# @rtemis/draw builds, in Node with the bundled builders, as the widget does.
# Requires Node.js; callers skip with skip_if_no_node().
built_option <- function(widget, dark = FALSE) {
  payload <- if (S7::S7_inherits(widget, BuildRequest)) {
    list(build = build_payload(widget))
  } else if (inherits(widget, "htmlwidget")) {
    widget[["x"]]
  } else {
    widget
  }
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  jsonlite::write_json(
    list(
      payload = payload,
      builders = system.file(
        "htmlwidgets/lib/draw/rtemis-draw-builders.js",
        package = "rtemis.draw"
      ),
      dark = dark
    ),
    input,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  # A builder error exits Node nonzero; it is raised below with its message.
  out <- suppressWarnings(system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "build_option.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(attr(out, "status"))) {
    stop(paste(out, collapse = "\n"))
  }
  jsonlite::fromJSON(
    paste(out, collapse = ""),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}
