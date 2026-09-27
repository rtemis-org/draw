# Targeted release QA: dense labels, annotated proteins and unusual layouts.
# Run with the current checkout installed: just qa-targeted <output-directory>.
# Saves native browser and SVG screenshots plus measured geometry. Review every
# image; geometric flags are diagnostics, not substitutes for visual judgment.
library(rtemis.draw)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[1L]])
# Verify installed renderer bytes, not merely a possibly reused package version.
assets <- list.files('r/inst', recursive = TRUE, full.names = FALSE)
assets <- assets[grepl('\\.(js|yaml)$', assets)]
stopifnot(
  unname(tools::md5sum(file.path('r/inst', assets))) ==
    unname(tools::md5sum(system.file(assets, package = 'rtemis.draw')))
)
set.seed(531)
labels <- paste(
  c('Anterior', 'Posterior', 'Lateral', 'Medial', 'Central', 'Peripheral'),
  'response after treatment'
)
group <- rep(labels, each = 20L)
x <- rep(seq_len(20L), 6)
y <- sin(x / 3) + rep(seq_len(6L), each = 20L) + rnorm(120, sd = .15)
sequence <- paste(rep('ACDEFGHIKLMNPQRSTVWY', 15), collapse = '')
protein <- rtemis.a3::create_A3(
  sequence,
  region = list(
    `Catalytic domain` = rtemis.a3::annotation_range(matrix(
      c(10L, 75L, 150L, 220L),
      ncol = 2,
      byrow = TRUE
    )),
    `Regulatory region` = rtemis.a3::annotation_range(matrix(
      c(85L, 135L),
      ncol = 2
    ))
  ),
  site = list(
    `Ligand-binding site` = rtemis.a3::annotation_position(c(35L, 98L, 170L)),
    `Disease-Associated Variant` = rtemis.a3::annotation_position(62L)
  ),
  ptm = list(
    Phosphorylation = rtemis.a3::annotation_position(c(20L, 35L, 160L)),
    Acetylation = rtemis.a3::annotation_position(c(20L, 35L, 90L)),
    Methylation = rtemis.a3::annotation_position(c(20L, 100L))
  ),
  processing = list(
    `Signal peptide cleavage` = rtemis.a3::annotation_position(25L)
  ),
  variant = list(
    rtemis.a3::annotation_variant(40L, info = list(mutation = 'Y40F')),
    rtemis.a3::annotation_variant(62L, info = list(mutation = 'C62R'))
  )
)
links <- data.frame(
  source = c(
    'Screened cohort',
    'Screened cohort',
    'Eligible participants',
    'Eligible participants'
  ),
  target = c(
    'Eligible participants',
    'Excluded participants',
    'Treatment arm',
    'Control arm'
  ),
  value = c(80, 20, 40, 40)
)
cm <- matrix(
  c(36, 2, 1, 0, 3, 29, 4, 1, 1, 2, 31, 3, 0, 2, 3, 27),
  4,
  dimnames = list(labels[1:4], labels[1:4])
)
# Deliberately long labels and explicit grids remain QA fixtures, not tutorials.
builders <- list(
  dense_legend = function(theme) {
    draw_scatter(
      x,
      y,
      group = group,
      theme = theme,
      title = 'Treatment response by anatomical region'
    )
  },
  long_bar = function(theme) {
    draw_bar(
      labels,
      seq(18, 48, length.out = 6),
      horizontal = TRUE,
      theme = theme
    )
  },
  faint_box = function(theme) {
    draw_boxplot(
      y,
      group = group,
      horizontal = TRUE,
      fill_alpha = .06,
      boxpoints = 'all',
      theme = theme
    )
  },
  sankey_vertical = function(theme) {
    draw_sankey(links, orient = 'vertical', label_font_size = 14, theme = theme)
  },
  confusion = function(theme) draw_confusion(cm, font_size = 15, theme = theme),
  confusion_panels = function(theme) {
    draw_confusion(
      list(
        `Internal validation` = cm,
        `External validation` = cm,
        `Temporal validation` = cm
      ),
      ncol = 3L,
      theme = theme
    )
  },
  a3_long = function(theme) {
    draw_a3(
      protein,
      n_per_row = 31L,
      position_every = 25L,
      theme = theme,
      height = 900
    )
  },
  a3_grid = function(theme) {
    draw_a3(
      protein,
      n_per_row = 41L,
      ptm_placement = 'outerRadial',
      grid = Grid(left = 30, right = 300, top = 55, bottom = 45),
      theme = theme,
      height = 900
    )
  },
  panels = function(theme) {
    draw_panels(
      list(
        draw_sankey(links, orient = 'vertical', theme = theme),
        draw_heatmap(
          cor(mtcars[1:5]),
          cluster_rows = TRUE,
          cluster_cols = TRUE,
          dendro_row_side = 'left',
          dendro_col_side = 'bottom',
          dendro_row_width = 35,
          dendro_col_height = 35,
          colorbar_orient = 'horizontal',
          show_values = TRUE,
          theme = theme
        ),
        draw_scatter(x, y, group = group, theme = theme)
      ),
      ncol = 2,
      gap = 24,
      padding = 16,
      height = 900
    )
  }
)
#' Evaluate JavaScript in the QA browser, surfacing native exceptions.
#' @param code Character JavaScript expression.
#' @return JSON-compatible browser value.
#' @keywords internal
#' @noRd
evaluate <- function(code) {
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
b <- chromote::ChromoteSession$new(width = 1200, height = 1000)
b$Page$addScriptToEvaluateOnNewDocument(
  source = "window.qaErrors=[];addEventListener('error',e=>qaErrors.push(e.message));"
)
manifest <- list(
  commit = system2('git', c('rev-parse', 'HEAD'), stdout = TRUE),
  status = system2('git', c('status', '--porcelain'), stdout = TRUE),
  generated_utc = format(Sys.time(), tz = 'UTC', usetz = TRUE),
  R = R.version.string,
  browser = b$Browser$getVersion(),
  assets = as.list(tools::md5sum(system.file(assets, package = 'rtemis.draw'))),
  cases = list()
)
selected_cases <- Sys.getenv('DRAW_QA_CASES')
if (nzchar(selected_cases)) {
  builders <- builders[strsplit(selected_cases, ',', fixed = TRUE)[[1L]]]
}
js <- paste(readLines('r/tools/visual-qa/targeted.js'), collapse = '\n')
tryCatch(
  {
    for (mode in c('light', 'dark')) {
      for (name in names(builders)) {
        theme <- if (mode == 'light') theme_light() else theme_dark()
        widget <- builders[[name]](theme)
        widget$width <- '100%'
        height <- if (
          name %in% c('a3_long', 'a3_grid', 'panels', 'confusion_panels')
        ) {
          900L
        } else {
          650L
        }
        if (name == 'confusion_panels') {
          height <- 1500L
        }
        widget$height <- height
        html <- file.path(out, paste0(name, '-', mode, '.html'))
        htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
        for (width in c(1200L, 760L, 390L)) {
          # A fixed two-column layout is intentionally not responsive. Supply one
          # column on phones so each child has a usable drawing surface.
          if (name == 'panels') {
            widget$x$layout$ncol <- if (width < 760L) 1L else 2L
            height <- if (width < 760L) 1500L else 900L
            widget$height <- height
            htmlwidgets::saveWidget(widget, html, selfcontained = FALSE)
          }
          key <- paste(name, mode, width, sep = '-')
          cat(key, '\n')
          result <- tryCatch(
            {
              b$Emulation$setDeviceMetricsOverride(
                width = width,
                height = height + 30L,
                deviceScaleFactor = 1,
                mobile = FALSE
              )
              b$Emulation$setEmulatedMedia(
                features = list(list(
                  name = 'prefers-color-scheme',
                  value = if (mode == 'light') 'dark' else 'light'
                ))
              )
              b$go_to(paste0('file://', html), delay = .1)
              evaluate(js)
              ready <- FALSE
              for (i in 1:120) {
                ready <- isTRUE(evaluate('targetedQA.ready()'))
                if (ready) {
                  break
                }
                Sys.sleep(.1)
              }
              stopifnot(ready)
              evaluate(
                'new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))'
              )
              scene <- evaluate('targetedQA.scene()')
              scene$layout <- widget$x$layout
              if (length(scene$errors)) {
                stop(paste(scene$errors, collapse = "; "))
              }
              # Standalone confusion widgets may grow beyond the viewport.
              # Capture the complete page, including all metric/footer rows.
              capture_height <- ceiling(evaluate(
                'Math.max(innerHeight, document.documentElement.scrollHeight, document.body?.scrollHeight || 0)'
              ))
              # Resize the viewport first, then wait for repaint. Capturing a
              # larger clip directly can catch ECharts mid-resize with missing
              # animated text even though its prior scene was settled.
              b$Emulation$setDeviceMetricsOverride(
                width = width,
                height = capture_height,
                deviceScaleFactor = 1,
                mobile = FALSE
              )
              Sys.sleep(.25)
              for (attempt in seq_len(100L)) {
                if (isTRUE(evaluate('targetedQA.ready()'))) {
                  break
                }
                Sys.sleep(.05)
              }
              evaluate(
                'new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))'
              )
              scene <- evaluate('targetedQA.scene()')
              scene$layout <- widget$x$layout
              shot <- b$Page$captureScreenshot(
                format = 'png',
                captureBeyondViewport = FALSE
              )
              writeBin(
                base64enc::base64decode(shot$data),
                file.path(out, paste0(key, '-browser.png'))
              )
              # Full-page screenshots can schedule a transient Chrome viewport
              # resize. Let htmlwidgets finish its resulting layout first.
              Sys.sleep(.25)
              evaluate(
                'new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))'
              )
              # Native pointer interaction on the first legend entry, followed by restore.
              point <- evaluate('targetedQA.legend()')
              if (!is.null(point)) {
                for (selected in c(FALSE, TRUE)) {
                  b$Input$dispatchMouseEvent(
                    type = 'mousePressed',
                    x = point$x,
                    y = point$y,
                    button = 'left',
                    clickCount = 1
                  )
                  b$Input$dispatchMouseEvent(
                    type = 'mouseReleased',
                    x = point$x,
                    y = point$y,
                    button = 'left',
                    clickCount = 1
                  )
                  # Wait for native legend updates before another pointer event.
                  for (attempt in seq_len(30L)) {
                    if (
                      identical(evaluate('targetedQA.selected()'), selected)
                    ) {
                      break
                    }
                    Sys.sleep(.05)
                  }
                  stopifnot(identical(
                    evaluate('targetedQA.selected()'),
                    selected
                  ))
                }
                scene$legend_toggle <- TRUE
              }
              for (attempt in seq_len(100L)) {
                if (isTRUE(evaluate('targetedQA.ready()'))) {
                  break
                }
                Sys.sleep(.05)
              }
              if (startsWith(name, 'a3') && width >= 760L) {
                point <- evaluate('targetedQA.a3Point()')
                b$Input$dispatchMouseEvent(
                  type = 'mouseMoved',
                  x = point$x,
                  y = point$y
                )
                Sys.sleep(.2)
                scene$tooltip <- evaluate('targetedQA.tooltip()')
                stopifnot(nzchar(scene$tooltip))
                before <- evaluate('targetedQA.zoom()')
                b$Input$dispatchMouseEvent(
                  type = 'mouseWheel',
                  modifiers = 8L, # A3 uses Shift + wheel for zoom.
                  x = point$x,
                  y = point$y,
                  deltaX = 0,
                  deltaY = -200
                )
                Sys.sleep(.2)
                scene$zoom <- evaluate('targetedQA.zoom()')
                stopifnot(!identical(before, scene$zoom))
              }
              svg <- file.path(out, paste0(key, '.svg'))
              save_drawing(widget, svg, width = width, height = height)
              doc <- xml2::read_xml(svg)
              stopifnot(
                length(xml2::xml_find_all(doc, './/*[local-name()="image"]')) ==
                  0L,
                length(xml2::xml_find_all(
                  doc,
                  './/*[local-name()="path" or local-name()="circle" or local-name()="rect"]'
                )) >
                  0L
              )
              b$go_to(paste0('file://', svg), delay = .1)
              scene$svg <- evaluate(
                "(()=>{const s=document.documentElement,r=s.getBoundingClientRect();return {text:[...document.querySelectorAll('text')].map(e=>{const b=e.getBoundingClientRect();return {text:e.textContent,x:b.x-r.x,y:b.y-r.y,width:b.width,height:b.height}})}})()"
              )
              # Standalone confusion widgets may grow beyond the viewport.
              # Capture the complete page, including all metric/footer rows.
              capture_height <- ceiling(evaluate(
                'Math.max(innerHeight, document.documentElement.scrollHeight, document.body?.scrollHeight || 0)'
              ))
              shot <- b$Page$captureScreenshot(
                format = 'png',
                captureBeyondViewport = TRUE,
                clip = list(
                  x = 0,
                  y = 0,
                  width = width,
                  height = capture_height,
                  scale = 1
                )
              )
              writeBin(
                base64enc::base64decode(shot$data),
                file.path(out, paste0(key, '-svg.png'))
              )
              scene
            },
            error = function(e) list(error = conditionMessage(e))
          )
          manifest$cases[[key]] <- result
          jsonlite::write_json(
            manifest,
            file.path(out, 'manifest.json'),
            pretty = TRUE,
            auto_unbox = TRUE,
            null = 'null'
          )
        }
      }
    }
  },
  finally = b$close()
)
failed <- vapply(manifest$cases, function(x) !is.null(x[["error"]]), logical(1))
if (any(failed)) {
  stop('QA cases failed: ', paste(names(failed)[failed], collapse = ', '))
}
cat(
  'Completed ',
  length(manifest$cases),
  ' browser/SVG pairs. Inspect screenshots and manifest geometry.\n',
  sep = ''
)
