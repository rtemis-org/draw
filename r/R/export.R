# export.R
# Offline export of draw widgets to static image files.
# spec: draw/first-cran-release#current-static-export-boundary
#
# Currently supports SVG via a small Node.js SSR script that loads the
# same bundled echarts.min.js used by the htmlwidget and calls
# `chart.renderToSVGString()`. PNG / PDF / WEBP formats are planned
# via a chromote fallback.

#' Save a Draw Widget to a File
#'
#' Exports a widget created by [draw()] (or any of the `draw_*`
#' functions) to a static file. Currently supports ECharts `.svg` via Node.js
#' server-side rendering, including complete [draw_panels()] figures. Requires
#' a `node` binary on `PATH`.
#'
#' Callbacks under tooltips, axis pointers, and interactive toolbox controls
#' are omitted. Other JavaScript callbacks are rejected because discarding them
#' could change visible content. Built-in Gantt, dendrogram, and boxplot point renderers are
#' shared with the browser widget and use JSON parameters. Network and map
#' exports are not yet implemented.
#'
#' @param widget htmlwidget: A widget returned by [draw()] or a `draw_*` function.
#' @param filename Character scalar: Nonempty output path with an extension.
#' @param width Optional Numeric scalar `(0, Inf)`: Finite image width in pixels.
#'   Unset uses the widget's numeric width, or 800 for a relative/unspecified width.
#' @param height Optional Numeric scalar `(0, Inf)`: Finite image height in pixels.
#'   Unset uses the widget's numeric height, or 600 for a relative/unspecified height.
#' @return The `filename`, invisibly.
#' @export
#'
#' @examples
#' # SVG export shells out to Node.js, so run it only where node exists.
#' if (nzchar(Sys.which("node"))) {
#'   chart <- draw_bar(x = c("A", "B", "C"), y = c(3, 7, 2))
#'   path <- file.path(tempdir(), "chart.svg")
#'   save_drawing(chart, path)
#'   unlink(path)
#' }
save_drawing <- function(widget, filename, width = NULL, height = NULL) {
  if (!inherits(widget, "htmlwidget")) {
    abort(
      "`widget` must be an htmlwidget returned by draw().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  # Retain numeric widget dimensions for complete panel compositions. Relative
  # CSS dimensions have no standalone pixel size, so they use the fallback.
  width <- width %||%
    if (is.numeric(widget[["width"]])) widget[["width"]] else 800
  height <- height %||%
    if (is.numeric(widget[["height"]])) widget[["height"]] else 600
  if (
    !is.character(filename) ||
      length(filename) != 1L ||
      is.na(filename) ||
      !nzchar(filename)
  ) {
    abort(
      "Supply one nonempty `filename` with an extension.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  for (name in c("width", "height")) {
    value <- if (name == "width") width else height
    if (
      !is.numeric(value) ||
        is.complex(value) ||
        length(value) != 1L ||
        !is.finite(value) ||
        value <= 0
    ) {
      abort(
        "Supply a finite positive numeric scalar for `",
        name,
        "`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }

  ext <- tolower(tools::file_ext(filename))
  if (!nzchar(ext)) {
    abort(
      "`filename` must include a file extension (e.g. .svg).",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (ext != "svg") {
    abort(
      "save_drawing() currently only supports .svg (got .",
      ext,
      "). ",
      "PNG/PDF/WEBP support is planned.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }

  # Reject other backends before looking for an ECharts option. An SVG-shaped
  # file is not proof that the requested network or map was rendered.
  if (!inherits(widget, "rtemis-draw") && !inherits(widget, "rtemis-panels")) {
    abort(
      "SVG export currently supports ECharts widgets only; ",
      "network, map, and other widget exporters are not implemented.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }
  payload <- widget[["x"]]
  if (inherits(widget, "rtemis-panels")) {
    panels <- lapply(seq_along(payload[["panels"]]), function(i) {
      strip_js(payload[["panels"]][[i]], path = paste0("panels[", i, "]"))
    })
    save_svg_ssr(
      NULL,
      NULL,
      filename,
      width,
      height,
      panels = panels,
      layout = payload[["layout"]]
    )
    return(invisible(filename))
  }
  option <- strip_js(payload[["option"]], path = "option")
  option <- static_aspect(option, payload[["aspect"]], width, height)
  # Automatic themes resolve to light for offline export. An explicit theme
  # remains the export theme; static output does not query OS preferences.
  theme <- strip_js(payload[["theme"]], path = "theme")
  save_svg_ssr(
    option,
    theme,
    filename,
    width,
    height,
    aspect = payload[["aspect"]],
    legend_position = payload[["legendPosition"]],
    confusion = payload[["confusion"]],
    meta = payload[intersect(
      names(payload),
      c(
        "squareCells",
        "nRows",
        "nCols",
        "leftPx",
        "rightPx",
        "topPx",
        "botPx",
        "colorLight",
        "colorDark"
      )
    )]
  )

  invisible(filename)
}

#' Fit a fixed-aspect plotting grid into a static image
#'
#' Reserves a centered outer box within both requested canvas dimensions.
#' ECharts first measures axis labels within this box; the shared JavaScript
#' layout then enforces the exact ratio on the remaining plotting area.
#' @param x List: Compiled ECharts option.
#' @param aspect Optional List: Ratio and pixel margins from render_meta().
#' @param width,height Numeric: Validated canvas dimensions.
#' @return List containing the resolved plotting-grid dimensions.
#' @keywords internal
#' @noRd
static_aspect <- new_generic("static_aspect", "x")
method(static_aspect, class_list) <- function(x, aspect, width, height) {
  if (is.null(aspect)) {
    return(x)
  }
  grid <- x[["grid"]]
  if (is.null(grid) || is.null(names(grid))) {
    abort(
      "Fixed-aspect SVG export requires one named plotting grid.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }
  fields <- c("ratio", "leftPx", "rightPx", "topPx", "botPx")
  if (
    !all(vapply(
      aspect[fields],
      function(v) {
        is.numeric(v) &&
          length(v) == 1L &&
          is.finite(v)
      },
      logical(1)
    )) ||
      aspect[["ratio"]] <= 0 ||
      any(unlist(aspect[fields[-1L]]) < 0)
  ) {
    abort(
      "Supply a positive aspect ratio and finite nonnegative pixel margins.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }
  available <- min(
    width - aspect[["leftPx"]] - aspect[["rightPx"]],
    (height - aspect[["topPx"]] - aspect[["botPx"]]) / aspect[["ratio"]]
  )
  preferred <- aspect[["widthPx"]] %||% available
  if (
    !is.numeric(preferred) ||
      length(preferred) != 1L ||
      !is.finite(preferred) ||
      preferred <= 0 ||
      available <= 0
  ) {
    abort(
      "Increase the SVG dimensions to leave room for the plotting grid and margins.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }
  grid[["width"]] <- min(preferred, available)
  grid[["height"]] <- grid[["width"]] * aspect[["ratio"]]
  grid[["left"]] <- aspect[["leftPx"]] +
    (width - aspect[["leftPx"]] - aspect[["rightPx"]] - grid[["width"]]) / 2
  grid[["top"]] <- aspect[["topPx"]] +
    (height - aspect[["topPx"]] - aspect[["botPx"]] - grid[["height"]]) / 2
  # Preserve native label containment until it has actually been measured.
  # Disabling it here clips axis names and lets tick labels overlap the legend.
  x[["grid"]] <- grid
  x
}

#' Prepare an option subtree for static rendering
#'
#' Remove interaction-only callbacks; reject callbacks affecting static content.
#' Preserve positional NULLs in data arrays, including missing heatmap cells.
#'
#' @param x Any: Value in a compiled ECharts option or theme.
#' @param path Character scalar: Diagnostic location in the payload.
#' @param interactive Logical scalar: Whether this subtree only affects interaction.
#' @return The subtree without interaction callbacks.
#' @keywords internal
#' @noRd
strip_js <- new_generic("strip_js", "x")

method(strip_js, class_any) <- function(
  x,
  path = "option",
  interactive = FALSE
) {
  if (inherits(x, "JS_EVAL")) {
    if (interactive) {
      return(NULL)
    }
    abort(
      "SVG export cannot preserve the JavaScript callback at `",
      path,
      "`. Use materialized values or a built-in named renderer.",
      class = c("rtemis_export_error", "rtemis_input_error")
    )
  }
  if (is.list(x)) {
    keys <- names(x)
    for (i in seq_along(x)) {
      key <- if (is.null(keys)) paste0("[", i, "]") else keys[[i]]
      # Single-bracket assignment retains NULL slots in arrays. Dropping a
      # missing coordinate would shift the remaining dimensions of a datum.
      x[i] <- list(strip_js(
        x[[i]],
        path = paste0(path, if (is.null(keys)) "" else ".", key),
        interactive = interactive ||
          key %in% c("tooltip", "axisPointer", "toolbox")
      ))
    }
    if (!is.null(keys)) {
      x <- x[!vapply(x, is.null, logical(1))]
    }
  }
  x
}

#' Render a static ECharts SVG using Node.js
#'
#' Render to a temporary file before replacing the caller's destination.
#'
#' @param option List: Prepared ECharts option.
#' @param theme Optional List: Prepared ECharts theme.
#' @param filename Character scalar: Destination path.
#' @param width,height Numeric scalars: Finite positive image dimensions.
#' @param panels,layout Optional List: Child payloads and resolved panel layout.
#' @param aspect Optional List: Fixed-ratio render metadata for a single chart.
#' @param legend_position Optional Character: Inset ROC legend corner.
#' @param confusion Optional List: Theme and square-cell constraints for confusion plots.
#' @param meta Optional List: Declarative heatmap geometry and palette hints shared
#'   with the browser renderer.
#' @return Logical, invisibly, indicating successful file copy.
#' @keywords internal
#' @noRd
save_svg_ssr <- function(
  option,
  theme,
  filename,
  width,
  height,
  panels = NULL,
  layout = NULL,
  aspect = NULL,
  legend_position = NULL,
  confusion = NULL,
  meta = NULL
) {
  node <- Sys.which("node")
  if (!nzchar(node)) {
    abort(
      "SVG export requires Node.js. Install it from https://nodejs.org ",
      "or via your package manager (e.g. `brew install node`).",
      class = "rtemis_export_error"
    )
  }

  script <- system.file("node", "render_svg.js", package = "rtemis.draw")
  if (!nzchar(script)) {
    abort(
      "Could not locate render_svg.js in the installed package. ",
      "Reinstall rtemis.draw.",
      class = "rtemis_export_error"
    )
  }

  payload <- c(
    meta,
    list(
      option = option,
      panels = panels,
      layout = layout,
      aspect = aspect,
      legendPosition = legend_position,
      confusion = confusion,
      theme = theme,
      width = width,
      height = height,
      creator = paste0(
        "rtemis.draw ",
        utils::packageVersion("rtemis.draw")
      )
    )
  )
  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    force = TRUE
  )

  tmp_in <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_in), add = TRUE)
  writeLines(json, tmp_in)

  err_file <- tempfile(fileext = ".err")
  on.exit(unlink(err_file), add = TRUE)

  # Render into a temp file and move it into place only once node has exited
  # cleanly: a failed export must not leave an empty or truncated file behind
  # at the caller's path.
  tmp_out <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp_out), add = TRUE)

  status <- system2(
    node,
    args = shQuote(script),
    stdin = tmp_in,
    stdout = tmp_out,
    stderr = err_file
  )

  if (!identical(status, 0L)) {
    err <- tryCatch(readLines(err_file, warn = FALSE), error = function(e) {
      character()
    })
    abort(
      "SVG export failed (node exit status ",
      status,
      ").",
      if (length(err)) paste0("\n", paste(err, collapse = "\n")) else "",
      class = "rtemis_export_error"
    )
  }

  # file.copy rather than file.rename: the temp directory and the destination
  # may sit on different filesystems, where a rename fails.
  if (!file.copy(tmp_out, filename, overwrite = TRUE)) {
    abort(
      "Could not write the SVG to '",
      filename,
      "'. Check the output directory.",
      class = "rtemis_export_error"
    )
  }
  invisible(TRUE)
}
