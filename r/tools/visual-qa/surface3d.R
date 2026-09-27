# Installed GL/SVG QA for paths, surfaces, intersections and mixed panels.
# Run from the repository root: just qa-surface3d <output-directory>.
library(rtemis.draw)
library(chromote)
set_chrome_args(c(default_chrome_args(), "--enable-unsafe-swiftshader"))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1L]], mustWork = TRUE)
model <- lm(mpg ~ wt + hp, data = mtcars)
gx <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 9)
gy <- seq(min(mtcars$hp), max(mtcars$hp), length.out = 8)
predicted <- matrix(predict(model, expand.grid(wt = gx, hp = gy)), length(gx))
builders <- list(
  fitted = function(theme) {
    draw_add_surface(
      draw_scatter3d(
        mtcars$wt,
        mtcars$hp,
        mtcars$mpg,
        theme = theme,
        xlab = "Weight (1000 lb)",
        ylab = "Horsepower",
        zlab = "MPG",
        palette = "#4078A6",
        title = "Observed and predicted fuel economy"
      ),
      gx,
      gy,
      predicted,
      color = "#EF8A00",
      name = "Linear prediction"
    )
  },
  crossings = function(theme) {
    t <- seq(0, 4 * pi, length.out = 31)
    plot <- draw_scatter3d(
      cos(t),
      sin(t),
      t / (4 * pi),
      mode = "both",
      point_size = 5,
      line_width = 3,
      opacity = 1,
      theme = theme,
      title = "Paths crossing two surfaces"
    )
    grid <- seq(-1, 1, length.out = 6)
    plot <- draw_add_surface(
      plot,
      grid,
      grid,
      outer(grid, grid, function(x, y) .5 + .3 * x),
      color = "#EF8A00",
      name = "Rising",
      opacity = .65
    )
    draw_add_surface(
      plot,
      grid,
      grid,
      outer(grid, grid, function(x, y) .5 - .3 * x),
      color = "#80558C",
      name = "Falling",
      opacity = .65
    )
  }
)
b <- ChromoteSession$new(width = 800, height = 650)
evaluate <- function(code) {
  result <- b$Runtime$evaluate(
    expression = code,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  if (!is.null(result$exceptionDetails)) {
    stop(jsonlite::toJSON(result$exceptionDetails))
  }
  result$result$value
}
manifest <- list(
  complete = FALSE,
  commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  browser = b$Browser$getVersion(),
  cases = list()
)
tryCatch(
  {
    for (mode in c("light", "dark")) {
      for (width in c(800L, 390L)) {
        for (family in names(builders)) {
          theme <- if (mode == "light") theme_light() else theme_dark()
          plot <- builders[[family]](theme)
          plot$width <- width
          plot$height <- 650
          key <- paste(family, mode, width, sep = "-")
          html <- file.path(out, paste0(key, ".html"))
          svg <- file.path(out, paste0(key, ".svg"))
          htmlwidgets::saveWidget(plot, html, selfcontained = FALSE)
          save_drawing(plot, svg, width = width, height = 650)
          doc <- xml2::read_xml(svg)
          stopifnot(
            length(xml2::xml_find_all(doc, './/*[local-name()="image"]')) == 0L,
            length(xml2::xml_find_all(doc, './/*[@data-kind="surface"]')) > 0L
          )
          if (family == "crossings") {
            stopifnot(
              length(xml2::xml_find_all(doc, './/*[@data-kind="line"]')) > 0L
            )
          }
          if (family == "fitted") {
            stopifnot(
              length(xml2::xml_find_all(
                doc,
                './/*[local-name()="circle" and @fill="#4078A6"]'
              )) >=
                32L
            )
          }
          b$Emulation$setDeviceMetricsOverride(
            width = width,
            height = 650,
            deviceScaleFactor = 1,
            mobile = FALSE
          )
          b$Page$navigate(paste0("file://", html))
          ready <- FALSE
          for (i in 1:100) {
            ready <- try(
              evaluate(
                '!!window.HTMLWidgets?.find(".rtemis-draw")?.getChart()?.getModel()?.getComponent("grid3D")?.coordinateSystem?.viewGL?.camera'
              ),
              silent = TRUE
            )
            if (isTRUE(ready)) {
              break
            }
            Sys.sleep(.1)
          }
          stopifnot(isTRUE(ready))
          Sys.sleep(.7)
          result <- evaluate(
            '(() => {
    const chart=HTMLWidgets.find(".rtemis-draw").getChart(),option=chart.getOption();
    const coord=chart.getModel().getComponent("grid3D").coordinateSystem,camera=coord.viewGL.camera;
    camera.update();const vp=coord.viewGL.viewport;
    const raw={grid3D:option.grid3D[0],xAxis3D:option.xAxis3D[0],yAxis3D:option.yAxis3D[0],zAxis3D:option.zAxis3D[0],series:option.series};
    const g=rtemisScatter3D.geometry(raw,chart.getWidth(),chart.getHeight());
    const mult=(m,p)=>[0,1,2,3].map(r=>m[r]*p[0]+m[r+4]*p[1]+m[r+8]*p[2]+m[r+12]*p[3]);
    let maxError=0,topology=true,triangleCount=0;
    for(const triangle of g.triangles)for(let i=0;i<3;i++) {
      const world=coord.dataToPoint(triangle.values[i]),eye=mult(camera.viewMatrix.array,[...world,1]);
      const clip=mult(camera.projectionMatrix.array,eye);
      const p=[vp.x+(clip[0]/clip[3]+1)*vp.width/2,chart.getHeight()-vp.y-(clip[1]/clip[3]+1)*vp.height/2];
      maxError=Math.max(maxError,Math.hypot(p[0]-triangle.vertices[i][0],p[1]-triangle.vertices[i][1]));
    }
    chart.getModel().getSeries().filter(s=>s.subType==="surface").forEach(series=>{
      const mesh=chart.getViewOfSeriesModel(series)._surfaceMesh;
      const geometry=mesh.geometry,indices=geometry.indices,position=geometry.attributes.position;
      const projected=g.triangles.filter(t=>t.series===series.seriesIndex);
      triangleCount+=indices.length/3;
      topology=topology&&projected.length===indices.length/3;
      const raw=series.getData(),data=Array.from({length:raw.count()},(_,i)=>raw.getRawDataItem(i));
      const indexOf=new Map(data.map((v,i)=>[JSON.stringify(v),i]));
      const key=indices=>indices.slice().sort((a,b)=>a-b).join(",");
      const actualFaces=[];
      for(let i=0;i<indices.length;i+=3)actualFaces.push(key(Array.from(indices.slice(i,i+3))));
      const expectedFaces=projected.map(t=>key(t.values.map(v=>indexOf.get(JSON.stringify(v)))));
      topology=topology&&JSON.stringify(actualFaces.sort())===JSON.stringify(expectedFaces.sort());
      data.forEach((v,i)=>{
        const actual=[];position.get(i,actual);const expected=coord.dataToPoint(v);
        topology=topology&&actual.every((n,k)=>Math.abs(n-expected[k])<1e-4);
      });
    });
    const expectedNames=[...new Set(option.series.map(s=>s.name))];
    const legendNames=chart.getModel().getComponent("legend").getData().map(d=>d.get("name"));
    return {maxError,topology,triangleCount,points:g.points.length,lines:g.lines.length,
      legendComplete:JSON.stringify(expectedNames)===JSON.stringify(legendNames)};
  })()'
          )
          print(list(case = key, geometry = result))
          stopifnot(
            result$maxError < .01,
            isTRUE(result$topology),
            isTRUE(result$legendComplete),
            result$triangleCount > 0
          )
          if (family == "fitted") {
            stopifnot(identical(
              evaluate(
                'HTMLWidgets.find(".rtemis-draw").getChart().getOption().series[0].itemStyle.color'
              ),
              "#4078A6"
            ))
          }
          screenshot <- b$Page$captureScreenshot()
          writeBin(
            base64enc::base64decode(screenshot$data),
            file.path(out, paste0(key, ".png"))
          )
          # Native legend clicks hide the surface, then restore it. Locate its actual
          # rendered hit target rather than relying on a fixed pixel coordinate.
          point <- evaluate(
            '(() => {
    const c=HTMLWidgets.find(".rtemis-draw").getChart(),m=c.getModel().getComponent("legend");
    const last=m.getData().length-1,g=c.getViewOfComponentModel(m).getContentGroup().children().find(g=>g.__legendDataIndex===last);
    const r=g.getBoundingRect().clone();r.applyTransform(g.getComputedTransform());
    const host=c.getDom().getBoundingClientRect();return {x:host.x+r.x+r.width/2,y:host.y+r.y+r.height/2,name:m.getData()[last].get("name")};
  })()'
          )
          for (selected in c(FALSE, TRUE)) {
            b$Input$dispatchMouseEvent(
              type = "mouseMoved",
              x = point$x,
              y = point$y
            )
            for (type in c("mousePressed", "mouseReleased")) {
              b$Input$dispatchMouseEvent(
                type = type,
                x = point$x,
                y = point$y,
                button = "left",
                clickCount = 1
              )
            }
            actual <- evaluate(paste0(
              'HTMLWidgets.find(".rtemis-draw").getChart().getModel().getComponent("legend").isSelected(',
              jsonlite::toJSON(point$name, auto_unbox = TRUE),
              ')'
            ))
            stopifnot(identical(actual, selected))
          }
          b$Page$navigate(paste0("file://", svg))
          Sys.sleep(.2)
          screenshot <- b$Page$captureScreenshot()
          writeBin(
            base64enc::base64decode(screenshot$data),
            file.path(out, paste0(key, "-svg.png"))
          )
          manifest$cases[[key]] <- result
          message(key, " passed")
        }
      }
    }
    # Combined surface exports must keep clip identifiers unique across children.
    panel <- draw_panels(
      list(builders$fitted(theme_light()), builders$crossings(theme_light())),
      ncol = 2
    )
    save_drawing(
      panel,
      file.path(out, "panels.svg"),
      width = 1400,
      height = 650
    )
    doc <- xml2::read_xml(file.path(out, "panels.svg"))
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, './/*[@id]'), "id")
    stopifnot(!anyDuplicated(ids))
    manifest$complete <- TRUE
  },
  finally = {
    jsonlite::write_json(
      manifest,
      file.path(out, "manifest.json"),
      pretty = TRUE,
      auto_unbox = TRUE
    )
    b$close()
  }
)
