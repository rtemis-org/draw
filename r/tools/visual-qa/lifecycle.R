# Installed-widget lifecycle, dense-data and fallback-font qualification.
# Run from the repository root with an output directory as the only argument.
library(rtemis.draw)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1L]])
# Deterministic data exercise substantial geometry without a timing assertion.
x <- seq(0, 20, length.out = 10000L)
edges <- data.frame(
  source = paste0("N", 1:300),
  target = paste0("N", c(2:300, 1))
)
plots <- list(
  line = draw_line(x, sin(x), title = "10,000 observations"),
  graph = draw_network(edges, layout = "circular"),
  map = draw_choropleth(
    data.frame(location = c("US", "CA", "MX"), value = 1:3),
    "location",
    "value",
    resolution = "country"
  ),
  surface = draw_add_surface(
    draw_scatter3d(c(-1, 1), c(-1, 1), c(-1, 1)),
    seq(-1, 1, length.out = 25),
    seq(-1, 1, length.out = 25),
    outer(
      seq(-1, 1, length.out = 25),
      seq(-1, 1, length.out = 25),
      function(x, y) sin(x * y)
    )
  )
)
plots$panels <- draw_panels(list(plots$line, draw_bar(letters[1:4], 1:4)))
themes <- lapply(list(theme_light(), theme_dark()), function(theme) {
  theme@text_style@font_family <- "Unavailable QA Font, serif"
  to_list(theme)
})
b <- chromote::ChromoteSession$new(width = 900, height = 650)
b$Page$addScriptToEvaluateOnNewDocument(
  source = "window.qaErrors=[];addEventListener('error',e=>qaErrors.push(e.message));"
)
#' Evaluate browser JavaScript and reject exceptions.
#' @param code Character: JavaScript expression.
#' @return Any: JSON-compatible browser result.
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
#' Wait until native geometry is ready.
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
wait_ready <- function() {
  for (i in seq_len(150L)) {
    if (
      isTRUE(evaluate(
        "(()=>{let w=HTMLWidgets.find('.html-widget');if(!w)return false;let r=w.getRenderer?.();return r?(r.loaded?r.loaded()&&!!r.getSource('regions'):r.getGraph().order>0):w.getCharts().every(c=>c.getModel().getSeries().length>0)})()"
      ))
    ) {
      evaluate(
        "new Promise(r=>setTimeout(()=>requestAnimationFrame(()=>r(true)),150))"
      )
      return(invisible(NULL))
    }
    Sys.sleep(.1)
  }
  stop("Wait for native chart geometry before observing lifecycle behavior.")
}
manifest <- list(
  complete = FALSE,
  commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  browser = b$Browser$getVersion(),
  cases = list()
)
tryCatch(
  {
    for (name in names(plots)) {
      widget <- plots[[name]]
      widget[["width"]] <- "100%"
      widget[["height"]] <- 600
      html <- file.path(out, paste0(name, ".html"))
      htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
      b$go_to(paste0("file://", html))
      wait_ready()
      evaluate(paste0(
        "window.qaThemes=",
        jsonlite::toJSON(themes, auto_unbox = TRUE),
        ";true"
      ))
      evaluate(
        "window.qaEl=document.querySelector('.html-widget');window.qaWidget=HTMLWidgets.find('#'+qaEl.id);window.qaPayload=JSON.parse(document.querySelector('script[data-for=\"'+qaEl.id+'\"]').textContent);window.qaCanvasCount=qaEl.querySelectorAll('canvas').length;true"
      )
      for (cycle in seq_len(6L)) {
        width <- if (cycle %% 2L) 390L else 900L
        b$Emulation$setDeviceMetricsOverride(
          width = width,
          height = 650L,
          deviceScaleFactor = 1,
          mobile = FALSE
        )
        evaluate(paste0(
          "(()=>{window.qaOldCharts=qaWidget.getCharts?.()||[];let p=JSON.parse(JSON.stringify(qaPayload));for(let key of p.evals||[])HTMLWidgets.evaluateStringMember(p.x,key);function theme(x){x.autoTheme=false;x.theme=qaThemes[",
          cycle %% 2L,
          "];if(x.panels)x.panels.forEach(theme);}theme(p.x);qaWidget.renderValue(p.x);qaWidget.resize(",
          width,
          ",600);return true})()"
        ))
        wait_ready()
        result <- evaluate(
          "(()=>{let charts=qaWidget.getCharts?.()||[];return {errors:qaErrors,canvases:qaEl.querySelectorAll('canvas').length,expectedCanvases:qaCanvasCount,oldDisposed:qaOldCharts.every(c=>c.isDisposed()),width:qaEl.getBoundingClientRect().width,charts:charts.map(c=>({width:c.getWidth(),height:c.getHeight(),series:c.getModel().getSeries().length})),font:getComputedStyle(qaEl).fontFamily}})()"
        )
        stopifnot(
          length(result[["errors"]]) == 0L,
          result[["canvases"]] == result[["expectedCanvases"]],
          result[["oldDisposed"]],
          result[["width"]] > 300
        )
        manifest[["cases"]][[paste(name, cycle, sep = "-")]] <- result
      }
      screenshot <- b$Page$captureScreenshot(
        format = "png",
        captureBeyondViewport = FALSE
      )
      writeBin(
        base64enc::base64decode(screenshot[["data"]]),
        file.path(out, paste0(name, ".png"))
      )
      svg <- file.path(out, paste0(name, ".svg"))
      save_drawing(widget, svg, width = 900, height = 600)
      doc <- xml2::read_xml(svg)
      stopifnot(
        length(xml2::xml_find_all(doc, './/*[local-name()="image"]')) == 0L,
        length(xml2::xml_find_all(
          doc,
          './/*[local-name()="path" or local-name()="circle"]'
        )) >
          0L
      )
      cat(name, "lifecycle passed\n")
    }
  },
  finally = b$close()
)
manifest[["complete"]] <- TRUE
jsonlite::write_json(
  manifest,
  file.path(out, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
