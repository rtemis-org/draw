# Build the ECharts option of a drawing or BuildRequest whose option
# @rtemis/draw builds, in Node with the bundled builders, as the widget does.
# Requires Node.js; callers skip with skip_if_no_node().
# `what = "aspect"` returns the fixed-aspect box hint the widget derives from
# the built option instead.
# `simplify = FALSE` keeps every JSON array a list, as R option lists are,
# where a record mixes numbers and text.
built_option <- function(
  widget,
  dark = FALSE,
  what = c("option", "aspect"),
  simplify = TRUE
) {
  what <- match.arg(what)
  build <- if (S7::S7_inherits(widget, BuildRequest)) {
    build_payload(widget)
  } else if (inherits(widget, "htmlwidget")) {
    widget[["x"]][["build"]]
  } else {
    widget[["build"]]
  }
  node_build(build, dark = dark, simplify = simplify, what = what)
}

# The option of a drawing: built in Node when @rtemis/draw builds it, and the
# payload's option otherwise.
node_option <- function(widget, simplify = TRUE) {
  if (
    !S7::S7_inherits(widget, BuildRequest) && is.null(widget[["x"]][["build"]])
  ) {
    return(widget[["x"]][["option"]])
  }
  skip_if_no_node()
  built_option(widget, simplify = simplify)
}

# The option of a drawing with every JSON array a list.
raw_option <- function(widget) node_option(widget, simplify = FALSE)

# A widget payload with its build request replaced by the option and aspect
# hint the widget builds from it, for fixtures that render payloads in Node.
# Every JSON array stays a list, so a record that mixes numbers and text (a
# confusion cell) and a one-element array keep their JSON form.
built_payload <- function(widget) {
  x <- widget[["x"]]
  if (!is.null(x[["build"]])) {
    x[["option"]] <- built_option(widget, simplify = FALSE)
    x[["aspect"]] <- built_option(widget, what = "aspect")
    x[["build"]] <- NULL
  }
  x
}
