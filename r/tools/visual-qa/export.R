# Reproducible installed-package visual QA for square heatmaps and A3 exports.
# Run from the repository root: just qa-export <output-directory>.
# This developer tool is excluded from the source package, and never modifies
# public examples. Inspect the images after the automated checks complete.
required <- c(
  "rtemis.draw",
  "rtemis.a3",
  "htmlwidgets",
  "jsonlite",
  "chromote",
  "base64enc",
  "xml2"
)
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Install the QA dependencies: ", paste(missing, collapse = ", "))
}
if (!nzchar(Sys.which("node"))) {
  stop("Install Node.js for vector export.")
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply one output directory.")
}
dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1L]], mustWork = TRUE)
library(rtemis.draw)

source_version <- read.dcf("r/DESCRIPTION", fields = "Version")[[1L]]
if (as.character(packageVersion("rtemis.draw")) != source_version) {
  stop("Install the current rtemis.draw checkout before running visual QA.")
}

# Real correlations and a compact annotated sequence exercise useful scenes;
# malformed inputs and extreme geometry belong in testthat fixtures.
m <- cor(mtcars[c("mpg", "disp", "hp", "wt")])
rect <- m[1:3, , drop = FALSE]
a3 <- rtemis.a3::create_A3(
  "MAEPRQEFEVMEDHAGTYGLGDRK",
  region = list(
    Domain = rtemis.a3::annotation_range(matrix(c(3L, 10L), ncol = 2))
  ),
  site = list(Site = rtemis.a3::annotation_position(8L)),
  ptm = list(Phosphorylation = rtemis.a3::annotation_position(5L)),
  processing = list(Cleavage = rtemis.a3::annotation_position(17L))
)
# Each builder receives an explicit theme. Browser preference is deliberately
# the opposite, exercising explicit-theme precedence as well as native layout.
builders <- list(
  heatmap = function(theme) {
    draw_heatmap(
      m,
      show_values = TRUE,
      theme = theme,
      title = "Vehicle characteristics"
    )
  },
  rectangular = function(theme) {
    draw_heatmap(rect, square_cells = TRUE, show_values = TRUE, theme = theme)
  },
  clustered = function(theme) {
    draw_heatmap(
      m,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      show_values = TRUE,
      theme = theme
    )
  },
  reversed = function(theme) {
    draw_heatmap(
      m,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      dendro_row_side = "left",
      dendro_col_side = "bottom",
      show_values = TRUE,
      theme = theme
    )
  },
  panels = function(theme) {
    draw_panels(
      list(
        draw_heatmap(
          m,
          show_values = TRUE,
          theme = theme,
          title = "Correlations"
        ),
        draw_heatmap(
          rect,
          square_cells = TRUE,
          show_values = TRUE,
          theme = theme,
          title = "Selected rows"
        )
      ),
      ncol = 2,
      height = 550
    )
  },
  a3 = function(theme) {
    draw_a3(
      a3,
      theme = theme,
      title = "Annotated sequence",
      height = 500
    )
  }
)
manifest <- list(
  r_version = R.version.string,
  node_version = system2(Sys.which("node"), "--version", stdout = TRUE),
  widget_dependencies = lapply(
    htmlwidgets::getDependency("rtemis-draw", "rtemis.draw"),
    function(d) d[c("name", "version", "script")]
  ),
  package_version = as.character(packageVersion("rtemis.draw")),
  dependencies = vapply(
    required,
    function(p) as.character(packageVersion(p)),
    character(1)
  ),
  assets = tools::md5sum(system.file(
    c(
      "htmlwidgets/rtemis-draw.js",
      "htmlwidgets/lib/draw/panels.js",
      "htmlwidgets/lib/draw/a3.js",
      "htmlwidgets/lib/draw/panel_widget.js",
      "node/render_svg.js"
    ),
    package = "rtemis.draw"
  )),
  source_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  source_status = system2("git", c("status", "--porcelain"), stdout = TRUE),
  generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  cases = list()
)
b <- chromote::ChromoteSession$new(width = 1100, height = 900)
manifest[["browser"]] <- b$Browser$getVersion()
b$Page$addScriptToEvaluateOnNewDocument(
  source = "window.__qaErrors=[];window.addEventListener('error',e=>window.__qaErrors.push(e.message));"
)
# Fail on JavaScript exceptions rather than accepting an empty screenshot.
evaluate <- function(code) {
  value <- b$Runtime$evaluate(
    expression = code,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  if (!is.null(value[["exceptionDetails"]])) {
    stop(jsonlite::toJSON(value[["exceptionDetails"]]))
  }
  value[["result"]][["value"]]
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
        stem <- paste(name, mode, sep = "-")
        html <- file.path(out, paste0(stem, ".html"))
        htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
        svg_path <- file.path(out, paste0(stem, ".svg"))
        save_drawing(widget, svg_path, width = 1100, height = 550)
        svg <- xml2::read_xml(svg_path)
        stopifnot(
          length(xml2::xml_find_all(svg, './/*[local-name()="path"]')) > 0L
        )
        stopifnot(
          length(xml2::xml_find_all(svg, './/*[local-name()="image"]')) == 0L
        )
        converter <- Sys.which("rsvg-convert")
        if (nzchar(converter)) {
          status <- system2(
            converter,
            c(
              shQuote(svg_path),
              "-o",
              shQuote(file.path(out, paste0(stem, "-svg.png")))
            )
          )
          stopifnot(status == 0)
        }
        widths <- if (name == "panels") {
          c(1100L, 900L)
        } else if (name == "a3") {
          c(1100L, 560L, 390L)
        } else {
          c(1100L, 560L)
        }
        for (width in widths) {
          b$Emulation$setDeviceMetricsOverride(
            width = width,
            height = 900L,
            deviceScaleFactor = 1,
            mobile = FALSE
          )
          b$go_to(paste0("file://", html), delay = 0.2)
          # Wait for actual chart scenes and final animation, not a nonempty HTML
          # container. Fresh navigation for each capture avoids shared-page state.
          ready <- FALSE
          for (attempt in 1:40) {
            ready <- isTRUE(evaluate(
              "(()=>{let el=document.querySelector('.rtemis-draw');let w=el&&window.HTMLWidgets?.find('#'+el.id);let cs=w?.getCharts?.();window.qaCharts=cs;return !!cs?.length&&cs.every(c=>c&&c.getZr().storage.getDisplayList(true).length>0)})()"
            ))
            if (ready) {
              break
            }
            Sys.sleep(0.1)
          }
          stopifnot(ready)
          settled <- FALSE
          for (attempt in 1:100) {
            settled <- isTRUE(evaluate(
              "qaCharts.every(c=>c.getZr().animation.isFinished())"
            ))
            if (settled) {
              break
            }
            Sys.sleep(0.1)
          }
          stopifnot(settled)
          evaluate(
            "new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(()=>resolve(true))))"
          )
          result <- evaluate(
            "(()=>{let el=document.querySelector('.rtemis-draw');window.qaCharts=HTMLWidgets.find('#'+el.id).getCharts();return {errors:window.__qaErrors,charts:qaCharts.map(c=>({width:c.getWidth(),height:c.getHeight(),background:c.getModel().get('backgroundColor'),heatmaps:c.getModel().getSeries().filter(s=>s.subType==='heatmap').map(s=>{let a=s.coordinateSystem.getArea(),d=s.getData(),e=Array.from({length:d.count()},(_,i)=>d.getItemGraphicEl(i)).find(Boolean);return {x:a.x,y:a.y,width:a.width,height:a.height,cellWidth:e.shape.width,cellHeight:e.shape.height}})}))}})()"
          )
          stopifnot(length(result[["errors"]]) == 0L)
          for (chart in result[["charts"]]) {
            stopifnot(chart[["width"]] > 0, chart[["height"]] > 0)
            stopifnot(
              chart[["background"]] ==
                if (mode == "light") "#ffffff" else "#181818"
            )
            for (heat in chart[["heatmaps"]]) {
              stopifnot(abs(heat[["cellWidth"]] - heat[["cellHeight"]]) < 1e-6)
            }
            if (name == "panels") stopifnot(chart[["height"]] <= 550)
          }
          if (name == "panels") {
            expected <- if (mode == "light") {
              "rgb(255, 255, 255)"
            } else {
              "rgb(24, 24, 24)"
            }
            surface <- evaluate(
              "({widget:getComputedStyle(document.querySelector('.rtemis-draw')).backgroundColor,body:getComputedStyle(document.body).backgroundColor})"
            )
            stopifnot(
              surface[["widget"]] == expected,
              surface[["body"]] == expected
            )
          }
          if (name == "a3") {
            geometry <- evaluate(
              "(()=>{let c=qaCharts[0],s=c.getModel().getSeries().find(s=>s.name==='Primary structure'),d=s.getData(),points=Array.from({length:d.count()},(_,i)=>s.coordinateSystem.dataToPoint([d.get('x',i),d.get('y',i)])),minimum=Math.min(...points.slice(1).map((p,i)=>Math.hypot(p[0]-points[i][0],p[1]-points[i][1]))),boxes=c.getModel().findComponents({mainType:'legend'}).map(l=>{let g=c.getViewOfComponentModel(l).group,b=g.getBoundingRect().clone();b.applyTransform(g.getComputedTransform());return b}),b={x:Math.min(...boxes.map(b=>b.x)),y:Math.min(...boxes.map(b=>b.y))};b.width=Math.max(...boxes.map(b=>b.x+b.width))-b.x;b.height=Math.max(...boxes.map(b=>b.y+b.height))-b.y;return {minimum,diameter:s.get('symbolSize'),legend:{x:b.x,y:b.y,width:b.width,height:b.height},width:c.getWidth(),height:c.getHeight()}})()"
            )
            stopifnot(geometry[["minimum"]] > geometry[["diameter"]])
            box <- geometry[["legend"]]
            stopifnot(
              box[["x"]] >= -1,
              box[["y"]] >= -1,
              box[["x"]] + box[["width"]] <= geometry[["width"]] + 1,
              box[["y"]] + box[["height"]] <= geometry[["height"]] + 1
            )
            result[["a3"]] <- geometry
            if (width < 1100) {
              narrow_svg <- file.path(out, paste0(stem, "-", width, ".svg"))
              save_drawing(widget, narrow_svg, width = width, height = 550)
              stopifnot(
                length(xml2::xml_find_all(
                  xml2::read_xml(narrow_svg),
                  './/*[local-name()="image"]'
                )) ==
                  0L
              )
              if (nzchar(converter)) {
                stopifnot(
                  system2(
                    converter,
                    c(
                      shQuote(narrow_svg),
                      "-o",
                      shQuote(file.path(
                        out,
                        paste0(stem, "-", width, "-svg.png")
                      ))
                    )
                  ) ==
                    0
                )
              }
            }
          }
          key <- paste(stem, width, sep = "-")
          manifest[["cases"]][[key]] <- result
          screenshot <- b$Page$captureScreenshot(
            format = "png",
            captureBeyondViewport = FALSE
          )
          writeBin(
            base64enc::base64decode(screenshot[["data"]]),
            file.path(out, paste0(key, ".png"))
          )
          # A real pointer click toggles the region legend and restores it.
          if (name == "a3") {
            point <- evaluate(
              "(()=>{let c=qaCharts[0],m=c.getModel().findComponents({mainType:'legend'}).find(m=>m.getData().some(d=>d.get('name')==='Domain')),i=m.getData().findIndex(d=>d.get('name')==='Domain'),g=c.getViewOfComponentModel(m).getContentGroup().children().find(g=>g.__legendDataIndex===i),r=g.getBoundingRect().clone();r.applyTransform(g.getComputedTransform());let d=c.getDom().getBoundingClientRect();return {x:d.x+r.x+r.width/2,y:d.y+r.y+r.height/2}})()"
            )
            for (selected in c(FALSE, TRUE)) {
              b$Input$dispatchMouseEvent(
                type = "mouseMoved",
                x = point[["x"]],
                y = point[["y"]]
              )
              b$Input$dispatchMouseEvent(
                type = "mousePressed",
                x = point[["x"]],
                y = point[["y"]],
                button = "left",
                clickCount = 1
              )
              b$Input$dispatchMouseEvent(
                type = "mouseReleased",
                x = point[["x"]],
                y = point[["y"]],
                button = "left",
                clickCount = 1
              )
              stopifnot(identical(
                evaluate(
                  "qaCharts[0].getModel().getComponent('legend').isSelected('Domain')"
                ),
                selected
              ))
            }
          }
          cat(key, "passed\n")
        }
      }
    }
  },
  finally = b$close()
)
stopifnot(length(manifest[["cases"]]) == 26L)
jsonlite::write_json(
  manifest,
  file.path(out, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
cat("Inspect the PNG and SVG files in", out, "\n")
