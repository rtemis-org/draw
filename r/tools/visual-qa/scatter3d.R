library(rtemis.draw)
library(chromote)
set_chrome_args(c(default_chrome_args(), "--enable-unsafe-swiftshader"))
# Installed-package browser and SVG QA. Run: just qa-scatter3d <output-directory>.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply one output directory.")
}
dir.create(args[[1]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1]], mustWork = TRUE)
b <- ChromoteSession$new(width = 800, height = 650)
on.exit(b$close())
eval_js <- function(code) {
  r <- b$Runtime$evaluate(
    expression = code,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  if (!is.null(r$exceptionDetails)) {
    stop(jsonlite::toJSON(r$exceptionDetails))
  }
  r$result$value
}
for (dark in c(FALSE, TRUE)) {
  for (width in c(800, 390)) {
    key <- paste(if (dark) 'dark' else 'light', width, sep = '-')
    theme <- if (dark) theme_dark() else theme_light()
    w <- draw_scatter3d(
      iris[1:3],
      group = iris$Species,
      theme = theme,
      width = width,
      height = 650,
      xlab = 'Sepal length',
      ylab = 'Sepal width',
      zlab = 'Petal length'
    )
    html <- file.path(out, paste0(key, '.html'))
    htmlwidgets::saveWidget(w, html, selfcontained = FALSE)
    save_drawing(
      w,
      file.path(out, paste0(key, '.svg')),
      width = width,
      height = 650
    )
    b$Emulation$setDeviceMetricsOverride(
      width = width,
      height = 650,
      deviceScaleFactor = 1,
      mobile = FALSE
    )
    b$Page$navigate(paste0('file://', html))
    for (i in 1:100) {
      ready <- try(
        eval_js(
          '!!(window.HTMLWidgets && HTMLWidgets.find(".rtemis-draw")?.getChart()?.getModel()?.getComponent("grid3D")?.coordinateSystem?.viewGL?.camera)'
        ),
        silent = TRUE
      )
      if (isTRUE(ready)) {
        break
      }
      Sys.sleep(.1)
    }
    if (!isTRUE(ready)) {
      stop('3D failed to initialize')
    }
    Sys.sleep(1)
    result <- eval_js(
      '(() => {
 const chart=HTMLWidgets.find(".rtemis-draw").getChart();
 const option=chart.getOption();
 const coord=chart.getModel().getComponent("grid3D").coordinateSystem;
 const camera=coord.viewGL.camera; camera.update();
 const vp=coord.viewGL.viewport;
 const raw={grid3D:option.grid3D[0],xAxis3D:option.xAxis3D[0],yAxis3D:option.yAxis3D[0],zAxis3D:option.zAxis3D[0],series:option.series};
 const geometry=rtemisScatter3D.geometry(raw,chart.getWidth(),chart.getHeight());
 const mult=(m,p)=>[0,1,2,3].map(r=>m[r]*p[0]+m[r+4]*p[1]+m[r+8]*p[2]+m[r+12]*p[3]);
 let maxError=0;
 for(const p of geometry.points){
  const world=coord.dataToPoint(p.value);
  const eye=mult(camera.viewMatrix.array,[...world,1]);
  const clip=mult(camera.projectionMatrix.array,eye);
  const screen=[vp.x+(clip[0]/clip[3]+1)*vp.width/2,chart.getHeight()-vp.y-(clip[1]/clip[3]+1)*vp.height/2];
  maxError=Math.max(maxError,Math.hypot(screen[0]-p.point[0],screen[1]-p.point[1]));
 }
 return {maxError,points:geometry.points.length,viewport:vp,alpha:raw.grid3D.viewControl.alpha,canvas:document.querySelectorAll("canvas").length};
})()'
    )
    print(list(case = key, result = result))
    stopifnot(result$points == 150, result$maxError < .01, result$canvas > 0)
    b$Page$captureScreenshot()$data |>
      base64enc::base64decode() |>
      writeBin(file.path(out, paste0(key, '.png')))
    b$Input$dispatchMouseEvent(
      type = 'mousePressed',
      x = 200,
      y = 280,
      button = 'left',
      clickCount = 1
    )
    for (dx in seq(210, 290, 20)) {
      b$Input$dispatchMouseEvent(
        type = 'mouseMoved',
        x = dx,
        y = 310,
        button = 'left',
        buttons = 1
      )
    }
    b$Input$dispatchMouseEvent(
      type = 'mouseReleased',
      x = 290,
      y = 310,
      button = 'left',
      clickCount = 1
    )
    Sys.sleep(.5)
    changed <- eval_js(
      'HTMLWidgets.find(".rtemis-draw").getChart().getOption().grid3D[0].viewControl.alpha'
    )
    stopifnot(abs(changed - result$alpha) > .1)
    # Inspect the actual vector artifact, including every grouped observation.
    svg <- file.path(out, paste0(key, '.svg'))
    doc <- xml2::read_xml(svg)
    ns <- c(s = "http://www.w3.org/2000/svg")
    stopifnot(
      length(xml2::xml_find_all(doc, ".//s:circle", ns)) == 150L,
      length(xml2::xml_find_all(doc, ".//s:image", ns)) == 0L
    )
    b$Page$navigate(paste0('file://', svg))
    Sys.sleep(.3)
    b$Page$captureScreenshot()$data |>
      base64enc::base64decode() |>
      writeBin(file.path(out, paste0(key, '-svg.png')))
  }
}
# A 3D child must keep its optional GL dependency inside a mixed figure.
panel <- draw_panels(
  list(
    draw_scatter3d(iris[1:3], group = iris$Species),
    draw_bar(c(A = 2, B = 3))
  ),
  ncol = 2,
  width = 1000,
  height = 650
)
html <- file.path(out, "panels.html")
htmlwidgets::saveWidget(panel, html, selfcontained = FALSE)
save_drawing(panel, file.path(out, "panels.svg"), width = 1000, height = 650)
b$Emulation$setDeviceMetricsOverride(
  width = 1000,
  height = 650,
  deviceScaleFactor = 1,
  mobile = FALSE
)
b$Page$navigate(paste0("file://", html))
ready <- FALSE
for (i in 1:100) {
  ready <- try(
    eval_js(
      '(() => {const cs=window.HTMLWidgets?.find(".rtemis-draw")?.getCharts?.();return cs?.length===2&&!!cs[0].getModel()?.getComponent("grid3D")?.coordinateSystem?.viewGL?.camera;})()'
    ),
    silent = TRUE
  )
  if (isTRUE(ready)) {
    break
  }
  Sys.sleep(.1)
}
stopifnot(isTRUE(ready))
doc <- xml2::read_xml(file.path(out, "panels.svg"))
stopifnot(
  length(xml2::xml_find_all(doc, './/*[local-name()="circle"]')) == 150L,
  length(xml2::xml_find_all(doc, './/*[local-name()="image"]')) == 0L
)
cat("Mixed 3D panel browser and SVG passed\n")
b$close()
