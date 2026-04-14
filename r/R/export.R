# export.R
# Offline export of draw widgets to static image files.
#
# Currently supports SVG via a small Node.js SSR script that loads the
# same bundled echarts.min.js used by the htmlwidget and calls
# `chart.renderToSVGString()`. PNG / PDF / WEBP formats are planned
# via a chromote fallback.

#' Save a Draw Widget to a File
#'
#' Exports a widget created by [draw()] (or any of the `draw_*`
#' functions) to a static file. Currently supports `.svg` via Node.js
#' server-side rendering. Requires a `node` binary on `PATH`.
#'
#' Interactive features (tooltip formatters, event handlers) are stripped
#' before rendering — static images cannot express them.
#'
#' @param widget htmlwidget: A widget returned by [draw()] or a `draw_*` function.
#' @param filename Character: Output file path. Extension determines the format.
#' @param width Numeric: Image width in pixels.
#' @param height Numeric: Image height in pixels.
#' @return The `filename`, invisibly.
#' @export
save_drawing <- function(widget, filename, width = 800, height = 600) {
  if (!inherits(widget, "htmlwidget")) {
    stop("`widget` must be an htmlwidget returned by draw().", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(filename))
  if (!nzchar(ext)) {
    stop("`filename` must include a file extension (e.g. .svg).", call. = FALSE)
  }

  payload <- widget$x
  option <- strip_js(payload$option)
  # auto_theme payloads expose the light theme under `theme`; use it as the
  # export default. Users can pass theme = theme_dark() at draw() time for
  # a dark export.
  theme <- strip_js(payload$theme)

  if (ext == "svg") {
    save_svg_ssr(option, theme, filename, width, height)
  } else {
    stop(
      "save_drawing() currently only supports .svg (got .",
      ext,
      "). ",
      "PNG/PDF/WEBP support is planned.",
      call. = FALSE
    )
  }

  invisible(filename)
}

# Recursively drop any htmlwidgets::JS()-wrapped values. These are raw
# JavaScript snippets (e.g. custom tooltip formatters) that cannot be
# serialized into a static SVG.
strip_js <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "JS_EVAL")) {
    return(NULL)
  }
  if (is.list(x)) {
    x <- lapply(x, strip_js)
    x <- x[!vapply(x, is.null, logical(1))]
  }
  x
}

save_svg_ssr <- function(option, theme, filename, width, height) {
  node <- Sys.which("node")
  if (!nzchar(node)) {
    stop(
      "SVG export requires Node.js. Install it from https://nodejs.org ",
      "or via your package manager (e.g. `brew install node`).",
      call. = FALSE
    )
  }

  script <- system.file("node", "render_svg.js", package = "rtemis.draw")
  if (!nzchar(script)) {
    stop(
      "Could not locate render_svg.js in the installed package. ",
      "Reinstall rtemis.draw.",
      call. = FALSE
    )
  }

  payload <- list(
    option = option,
    theme = theme,
    width = width,
    height = height
  )
  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    force = TRUE
  )

  tmp_in <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_in), add = TRUE)
  writeLines(json, tmp_in)

  err_file <- tempfile(fileext = ".err")
  on.exit(unlink(err_file), add = TRUE)

  status <- system2(
    node,
    args = shQuote(script),
    stdin = tmp_in,
    stdout = filename,
    stderr = err_file
  )

  if (!identical(status, 0L)) {
    err <- tryCatch(readLines(err_file, warn = FALSE), error = function(e) {
      character()
    })
    stop(
      "SVG export failed (node exit status ",
      status,
      ").",
      if (length(err)) paste0("\n", paste(err, collapse = "\n")) else "",
      call. = FALSE
    )
  }
}
