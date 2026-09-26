# Installed-package Sigma/MapLibre browser, interaction, and vector QA.
# Run from the repository root: just qa-backends <output-directory>.
library(rtemis.draw)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1L]])
stopifnot(
  as.character(packageVersion("rtemis.draw")) ==
    read.dcf("r/DESCRIPTION", fields = "Version")[[1L]]
)
# A small trait network and real state data make the geometry easy to inspect.
matrix <- stats::cor(
  penguins[c("bill_len", "bill_dep", "flipper_len", "body_mass")],
  use = "complete.obs"
)
edges <- data.frame(
  source = c("Bill", "Bill", "Flipper", "Mass"),
  target = c("Flipper", "Mass", "Mass", "Depth"),
  weight = c(0.8, 0.6, 0.9, -0.4)
)
states <- data.frame(location = state.abb, value = state.x77[, "Life Exp"])
counties <- data.frame(
  location = c(
    "06001",
    "06013",
    "06041",
    "06055",
    "06075",
    "06081",
    "06085",
    "06095",
    "06097"
  ),
  value = seq_len(9)
)
countries <- data.frame(
  location = c("US", "CA", "MX", "GB", "FR", "DE", "JP", "AU"),
  value = c(10, 8, 6, 9, 7, 5, 4, 3)
)
builders <- list()
for (layout in c("force", "circular", "circlepack", "random")) {
  builders[[paste0("graph-", layout)]] <- local({
    chosen <- layout
    function(theme) {
      draw_network(
        edges,
        layout = chosen,
        color_by_group = TRUE,
        title = "Trait network",
        theme = theme
      )
    }
  })
}
for (resolution in c("country", "state", "county")) {
  builders[[paste0("map-", resolution)]] <- local({
    chosen <- resolution
    data <- switch(
      chosen,
      country = countries,
      state = states,
      county = counties
    )
    function(theme) {
      draw_choropleth(
        data,
        "location",
        "value",
        resolution = chosen,
        colormap = "viridis",
        value_label = if (chosen == "state") {
          "Life expectancy (years)"
        } else {
          "Value"
        },
        theme = theme
      )
    }
  })
}
manifest <- list(
  complete = FALSE,
  expected_cases = length(builders) * 4L,
  source_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  source_status = system2("git", c("status", "--porcelain"), stdout = TRUE),
  generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  cases = list()
)
manifest[["r_version"]] <- R.version.string
manifest[["node_version"]] <- system2(
  Sys.which("node"),
  "--version",
  stdout = TRUE
)
manifest[["dependencies"]] <- vapply(
  c("rtemis.draw", "htmlwidgets", "chromote"),
  function(p) as.character(packageVersion(p)),
  character(1)
)
manifest[["assets"]] <- tools::md5sum(system.file(
  c(
    "htmlwidgets/rtemis-graph.js",
    "htmlwidgets/rtemis-map.js",
    "htmlwidgets/lib/draw/graph_scene.js",
    "htmlwidgets/lib/draw/map_scene.js",
    "htmlwidgets/lib/draw/vector_theme.js",
    "node/vector.js",
    "node/graph-deps.js",
    "node/map-deps.js"
  ),
  package = "rtemis.draw"
))
jsonlite::write_json(
  manifest,
  file.path(out, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
b <- chromote::ChromoteSession$new(width = 1100, height = 650)
manifest[["browser"]] <- b$Browser$getVersion()
b$Page$addScriptToEvaluateOnNewDocument(
  source = paste(readLines("r/tools/visual-qa/backends.js"), collapse = "\n")
)
#' Evaluate a browser observation and fail on JavaScript exceptions.
#' @param code Character: JavaScript expression to evaluate.
#' @return JSON-compatible value returned by the expression.
#' @keywords internal
#' @noRd
evaluate <- function(code) {
  result <- b$Runtime$evaluate(
    expression = code,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  if (!is.null(result[["exceptionDetails"]])) {
    stop(jsonlite::toJSON(result[["exceptionDetails"]]))
  }
  result[["result"]][["value"]]
}

#' Wait for observable rendering or interaction completion.
#' @param code Character: JavaScript expression returning a boolean.
#' @return NULL, invisibly, after the expression becomes true.
#' @keywords internal
#' @noRd
wait_for <- function(code) {
  for (attempt in seq_len(100L)) {
    if (isTRUE(evaluate(code))) {
      return(invisible(NULL))
    }
    Sys.sleep(0.1)
  }
  stop("Browser condition did not settle: ", code)
}

#' Send a native mouse click at an observed page position.
#' @param point List: Page x and y coordinates.
#' @param count Integer: Native click count, including two for a double-click.
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
click <- function(point, count = 1L) {
  b$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = point[["x"]],
    y = point[["y"]]
  )
  for (type in c("mousePressed", "mouseReleased")) {
    b$Input$dispatchMouseEvent(
      type = type,
      x = point[["x"]],
      y = point[["y"]],
      button = "left",
      clickCount = count
    )
  }
  invisible(NULL)
}


tryCatch(
  {
    for (mode in c("light", "dark")) {
      theme <- if (mode == "light") theme_light() else theme_dark()
      b$Emulation$setEmulatedMedia(
        features = list(list(
          name = "prefers-color-scheme",
          value = if (mode == "light") "dark" else "light"
        ))
      )
      for (name in names(builders)) {
        widget <- builders[[name]](theme)
        widget[["width"]] <- "100%"
        widget[["height"]] <- 600L
        html <- file.path(out, paste0(name, "-", mode, ".html"))
        htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
        for (width in c(1100L, 390L)) {
          key <- paste(name, mode, width, sep = "-")
          b$Emulation$setDeviceMetricsOverride(
            width = width,
            height = 650L,
            deviceScaleFactor = 1,
            mobile = FALSE
          )
          b$go_to(paste0("file://", html))
          wait_for("backendQA.ready()")
          evaluate(
            "new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))"
          )
          scene <- evaluate("backendQA.scene()")
          stopifnot(
            length(scene[["errors"]]) == 0L,
            scene[["width"]] > 300,
            scene[["count"]] > 0
          )
          screenshot <- b$Page$captureScreenshot(
            format = "png",
            captureBeyondViewport = FALSE
          )
          writeBin(
            base64enc::base64decode(screenshot[["data"]]),
            file.path(out, paste0(key, ".png"))
          )
          svg_path <- file.path(out, paste0(key, ".svg"))
          save_drawing(widget, svg_path, width = width, height = 600)
          svg <- xml2::read_xml(svg_path)
          stopifnot(
            length(xml2::xml_find_all(svg, './/*[local-name()="path"]')) > 0L,
            length(xml2::xml_find_all(svg, './/*[local-name()="image"]')) == 0L
          )
          stopifnot(
            system2(
              Sys.which("rsvg-convert"),
              c(
                shQuote(svg_path),
                "-o",
                shQuote(file.path(out, paste0(key, "-svg.png")))
              )
            ) ==
              0
          )
          # Verify SVG node centers against Sigma's actual initialized camera.
          if (startsWith(name, "graph")) {
            nodes <- xml2::xml_find_all(svg, './/*[local-name()="circle"]')
            for (node in nodes) {
              id <- xml2::xml_attr(node, "data-node-id")
              point <- scene[["nodes"]][[id]]
              stopifnot(
                abs(as.numeric(xml2::xml_attr(node, "cx")) - point[["x"]]) <
                  0.01,
                abs(as.numeric(xml2::xml_attr(node, "cy")) - point[["y"]]) <
                  0.01
              )
            }
          }
          point <- evaluate("backendQA.hoverPoint()")
          b$Input$dispatchMouseEvent(
            type = "mouseMoved",
            x = point[["x"]],
            y = point[["y"]]
          )
          wait_for("backendQA.tooltip().length>0")
          scene[["tooltip"]] <- evaluate("backendQA.tooltip()")
          screenshot <- b$Page$captureScreenshot(
            format = "png",
            captureBeyondViewport = FALSE
          )
          writeBin(
            base64enc::base64decode(screenshot[["data"]]),
            file.path(out, paste0(key, "-hover.png"))
          )
          # Both viewers expose native wheel zoom. Compare observed camera values.
          before <- evaluate("backendQA.zoom()")
          b$Input$dispatchMouseEvent(
            type = "mouseWheel",
            x = point[["x"]],
            y = point[["y"]],
            deltaX = 0,
            deltaY = -240
          )
          wait_for(paste0("backendQA.zoom() !== ", before))
          scene[["zoom"]] <- evaluate("backendQA.zoom()")
          stopifnot(length(evaluate("window.__qaErrors")) == 0L)
          manifest[["cases"]][[key]] <- scene
          jsonlite::write_json(
            manifest,
            file.path(out, "manifest.json"),
            auto_unbox = TRUE,
            pretty = TRUE
          )
          cat(key, "passed\n")
        }
      }
    }
  },
  finally = b$close()
)
stopifnot(length(manifest[["cases"]]) == manifest[["expected_cases"]])
manifest[["complete"]] <- TRUE
jsonlite::write_json(
  manifest,
  file.path(out, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
