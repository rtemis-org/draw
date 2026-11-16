# Build the ECharts option of a drawing or BuildRequest whose option
# @rtemis/draw builds, in Node with the bundled builders, as the widget does.
# Requires Node.js; callers skip with skip_if_no_node().
# `what = "aspect"` returns the fixed-aspect box hint the widget derives from
# the built option instead.
built_option <- function(widget, dark = FALSE, what = c("option", "aspect")) {
  what <- match.arg(what)
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
      dark = dark,
      what = what
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

# The option of a drawing: built in Node when @rtemis/draw builds it, and the
# payload's option otherwise.
node_option <- function(widget) {
  if (
    !S7::S7_inherits(widget, BuildRequest) && is.null(widget[["x"]][["build"]])
  ) {
    return(widget[["x"]][["option"]])
  }
  skip_if_no_node()
  built_option(widget)
}

# A widget payload with its build request replaced by the option and aspect
# hint the widget builds from it, for fixtures that render payloads in Node.
built_payload <- function(widget) {
  x <- widget[["x"]]
  if (!is.null(x[["build"]])) {
    x[["option"]] <- built_option(widget)
    x[["aspect"]] <- built_option(widget, what = "aspect")
    x[["build"]] <- NULL
  }
  x
}
