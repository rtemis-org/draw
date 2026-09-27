# Installed-package visual/interaction QA for scatter, line, and bar charts.
# Run from the repository root: just qa-foundation <output-directory>.
# Assertions and stress layouts stay here; public examples remain curated.
required <- c(
  "rtemis.draw",
  "mgcv",
  "survival",
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
  stop("Install QA dependencies: ", paste(missing, collapse = ", "))
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
stopifnot(
  as.character(packageVersion("rtemis.draw")) ==
    read.dcf("r/DESCRIPTION", fields = "Version")[[1L]]
)

# Use the same penguins workflows as the high-level chapter, with explicit
# themes and axis labels to check publication surfaces and fit layers.
birds <- penguins[
  complete.cases(penguins[c("bill_len", "flipper_len", "species")]),
]
counts <- with(penguins, table(species, sex))
by_sex <- setNames(
  lapply(seq_len(ncol(counts)), function(i) as.numeric(counts[, i])),
  colnames(counts)
)
annual <- with(penguins, table(species, year))
by_species <- setNames(
  lapply(seq_len(nrow(annual)), function(i) as.numeric(annual[i, ])),
  rownames(annual)
)
builders <- list(
  scatter = function(theme) {
    draw_scatter(
      birds[["bill_len"]],
      birds[["flipper_len"]],
      group = birds["species"],
      fit = "gam",
      xlab = "Bill length (mm)",
      ylab = "Flipper length (mm)",
      theme = theme
    )
  },
  line = function(theme) {
    draw_line(
      colnames(annual),
      by_species,
      xlab = "Year",
      ylab = "Count",
      zoom = TRUE,
      theme = theme
    )
  },
  area = function(theme) {
    draw_line(
      colnames(annual),
      by_species,
      area = TRUE,
      xlab = "Year",
      ylab = "Count",
      zoom = TRUE,
      theme = theme
    )
  },
  bar = function(theme) {
    draw_bar(rownames(counts), by_sex, ylab = "Count", theme = theme)
  },
  stacked = function(theme) {
    draw_bar(
      rownames(counts),
      by_sex,
      stack = TRUE,
      horizontal = TRUE,
      xlab = "Count",
      theme = theme
    )
  }
)
# Reproduce the grouped-boxplot layout and inspect outside/inset alignments.
for (anchor in c("top", "top-right", "bottom", "inside")) {
  builders[[paste0("boxplot_", anchor)]] <- local({
    position <- if (anchor == "inside") "top-right" else anchor
    placement <- if (anchor == "inside") "inside" else "outside"
    function(theme) {
      draw_boxplot(
        birds[c("bill_len", "bill_dep")],
        group = birds["species"],
        labels = c("Bill length (mm)", "Bill depth (mm)"),
        legend_position = position,
        legend_placement = placement,
        theme = theme
      )
    }
  })
}
# Distribution, flow, time-frequency, and independent-panel families.
species_counts <- table(birds[["species"]])
links <- as.data.frame(
  table(source = birds[["species"]], target = birds[["sex"]]),
  responseName = "value"
)
builders[["histogram"]] <- function(theme) {
  draw_histogram(
    birds[["bill_len"]],
    group = birds["species"],
    xlab = "Bill length (mm)",
    theme = theme
  )
}
builders[["density"]] <- function(theme) {
  draw_density(
    birds[["bill_len"]],
    group = birds["species"],
    xlab = "Bill length (mm)",
    theme = theme
  )
}
builders[["histogram_density"]] <- function(theme) {
  draw_histogram(
    birds[["bill_len"]],
    group = birds[["species"]],
    normalization = "density",
    density = TRUE,
    xlab = "Bill length (mm)",
    ylab = "Density",
    theme = theme
  )
}
builders[["histogram_unequal"]] <- function(theme) {
  draw_histogram(
    birds[["bill_len"]],
    group = birds[["species"]],
    bin_edges = c(30, 35, 40, 50, 60),
    normalization = "density",
    density = TRUE,
    xlab = "Bill length (mm)",
    ylab = "Density",
    theme = theme
  )
}
builders[["density_kernel"]] <- function(theme) {
  draw_density(
    birds[["bill_len"]],
    group = birds[["species"]],
    bandwidth = 2,
    kernel = "epanechnikov",
    xlab = "Bill length (mm)",
    ylab = "Density",
    theme = theme
  )
}
builders[["pie"]] <- function(theme) {
  draw_pie(as.numeric(species_counts), names(species_counts), theme = theme)
}
builders[["rose"]] <- function(theme) {
  draw_pie(
    as.numeric(species_counts),
    names(species_counts),
    rose_type = "radius",
    theme = theme
  )
}
builders[["sankey"]] <- function(theme) {
  draw_sankey(links, title = "Penguins by species and sex", theme = theme)
}
builders[["spectrogram"]] <- function(theme) {
  time <- seq(0, 1, by = 1 / 2000)
  draw_spectrogram(
    sin(2 * pi * (100 * time + 350 * time^2)),
    sample_rate = 2000,
    n_fft = 128L,
    theme = theme
  )
}
builders[["panels"]] <- function(theme) {
  draw_panels(
    list(
      draw_bar(rownames(counts), by_sex, theme = theme),
      draw_density(birds[["bill_len"]], group = birds["species"], theme = theme)
    ),
    ncol = 2L,
    height = 600
  )
}
# An optional family filter keeps iterative QA bounded; the unfiltered run is
# the complete foundation matrix recorded in the manifest.
set.seed(42)
training_rows <- unlist(lapply(
  split(seq_len(nrow(iris)), iris[["Species"]]),
  sample,
  size = 35L
))
training <- iris[training_rows, ]
testing <- iris[-training_rows, ]
training[["virginica"]] <- training[["Species"]] == "virginica"
sepal_model <- glm(
  virginica ~ Sepal.Length + Sepal.Width,
  data = training,
  family = binomial()
)
petal_model <- glm(
  virginica ~ Petal.Width,
  data = training,
  family = binomial()
)
reference <- factor(ifelse(
  testing[["Species"]] == "virginica",
  "Virginica",
  "Other"
))
builders[["calibration"]] <- function(theme) {
  draw_calibration(
    list(Sepal = reference, Petal = reference),
    list(
      Sepal = predict(sepal_model, testing, type = "response"),
      Petal = predict(petal_model, testing, type = "response")
    ),
    positive = "Virginica",
    n_bins = 5L,
    theme = theme
  )
}
# Real survival fits exercise all optional native layers together.
builders[["survival"]] <- function(theme) {
  patients <- survival::lung
  patients[["sex"]] <- factor(
    patients[["sex"]],
    levels = c(1, 2),
    labels = c("Male", "Female")
  )
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = patients)
  draw_survfit(
    fit,
    risk_times = c(0, 250, 500, 750, 1000),
    show_median = TRUE,
    landmarks = 365,
    xlab = "Days",
    theme = theme
  )
}
# Distribution layers use real repeated measurements and grouped morphology.
for (inset in c(FALSE, TRUE)) {
  builders[[if (inset) "violin_box" else "violin"]] <- local({
    show_box <- inset
    function(theme) {
      draw_violin(
        birds[c("bill_len", "bill_dep")],
        group = birds[["species"]],
        labels = c("Length", "Depth"),
        show_box = show_box,
        boxpoints = "all",
        ylab = "Bill dimensions (mm)",
        theme = theme
      )
    }
  })
}
for (horizontal in c(FALSE, TRUE)) {
  builders[[if (horizontal) "paired_horizontal" else "paired"]] <- local({
    orientation <- horizontal
    function(theme) {
      draw_violin(
        datasets::sleep[["extra"]],
        group = paste("Drug", datasets::sleep[["group"]]),
        observation = datasets::sleep[["ID"]],
        paired = TRUE,
        show_box = TRUE,
        boxpoints = "all",
        horizontal = orientation,
        comparisons = data.frame(
          from = "Drug 1",
          to = "Drug 2",
          label = "Paired p = 0.003"
        ),
        theme = theme
      )
    }
  })
}
# Remaining distribution, time-series and supplied-overlay workflows.
builders[["ridge"]] <- function(theme) {
  draw_density(
    birds[["bill_len"]],
    group = birds[["species"]],
    mode = "ridge",
    order = "mean",
    theme = theme
  )
}
builders[["histogram_stack"]] <- function(theme) {
  draw_histogram(
    birds[["bill_len"]],
    group = birds[["species"]],
    bar_mode = "stack",
    theme = theme
  )
}
builders[["histogram_group"]] <- function(theme) {
  draw_histogram(
    birds[["bill_len"]],
    group = birds[["species"]],
    bar_mode = "group",
    theme = theme
  )
}
builders[["timeseries"]] <- function(theme) {
  draw_ts(
    list(Activity = c(2, 4, 3, 5, 4, 7, 5, 3, 2)),
    time = seq(0, 48, length.out = 9),
    window = 3L,
    theme = theme,
    zoom = TRUE
  )
}
builders[["zeitgeber"]] <- function(theme) {
  draw_xt(
    seq(0, 48, length.out = 9),
    list(Activity = c(2, 4, 3, 5, 4, 7, 5, 3, 2)),
    zt = seq(0, 48, length.out = 9) %% 24,
    show_zt_every = 2L,
    shade_bin = c(0, 0, 1, 1, 0, 0, 1, 1, 0),
    theme = theme
  )
}
builders[["supplied_fit"]] <- function(theme) {
  plot <- draw_scatter(1:5, c(2, 3, 2, 5, 4), rug = TRUE, theme = theme)
  plot <- draw_add_fit(
    plot,
    1:5,
    c(2, 2.5, 3, 3.5, 4),
    lower = c(1.5, 2, 2.5, 3, 3.5),
    upper = c(2.5, 3, 3.5, 4, 4.5),
    name = "Predicted"
  )
  draw_annotate(
    plot,
    hline = 3,
    x = 3,
    y = 4.5,
    text = "Reference",
    bands = list(c(2, 3))
  )
}
builders[["heatmap_annotations"]] <- function(theme) {
  x <- cor(mtcars[c("mpg", "disp", "hp", "wt")])
  draw_heatmap(
    x,
    row_tree = hclust(dist(x)),
    col_tree = hclust(dist(t(x))),
    cell_notes = matrix(c("{b}", rep("Measured note {b}", 15)), 4, 4),
    show_notes = TRUE,
    row_colors = c("#18A3AC", "#F48024", "#F48024", "#18A3AC"),
    col_colors = c("#18A3AC", "#F48024", "#F48024", "#18A3AC"),
    row_cut = 2L,
    col_cut = 2L,
    dendro_col_side = "bottom",
    theme = theme,
    square_cells = TRUE,
    title = "Vehicle correlations"
  )
}
extra_labels <- list(
  heatmap_annotations = "{b}",
  ridge = unique(birds[["species"]]),
  histogram_stack = unique(birds[["species"]]),
  histogram_group = unique(birds[["species"]]),
  timeseries = "Activity",
  zeitgeber = "Activity",
  supplied_fit = "Reference"
)
selected <- Sys.getenv("DRAW_QA_FAMILIES")
if (nzchar(selected)) {
  builders <- builders[strsplit(selected, ",", fixed = TRUE)[[1L]]]
}
manifest <- list(
  complete = FALSE,
  expected_cases = length(builders) * 4L,
  generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  source_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  source_status = system2("git", c("status", "--porcelain"), stdout = TRUE),
  r_version = R.version.string,
  node_version = system2(Sys.which("node"), "--version", stdout = TRUE),
  dependencies = vapply(
    required,
    function(p) as.character(packageVersion(p)),
    character(1)
  ),
  widget_dependencies = lapply(
    htmlwidgets::getDependency("rtemis-draw", "rtemis.draw"),
    function(d) d[c("name", "version", "script")]
  ),
  assets = tools::md5sum(system.file(
    c(
      "htmlwidgets/rtemis-draw.js",
      "htmlwidgets/lib/draw/panels.js",
      "node/render_svg.js"
    ),
    package = "rtemis.draw"
  )),
  cases = list()
)
# Overwrite the manifest before doing work so an interrupted rerun cannot leave
# a previous run's successful manifest alongside partially replaced artifacts.
jsonlite::write_json(
  manifest,
  file.path(out, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
# Chrome requires a nondefault profile for remote debugging. Keep this QA run
# isolated unless the caller already supplied an explicit profile.
chrome_args <- chromote::get_chrome_args()
if (!any(grepl("^--user-data-dir(?:=|$)", chrome_args, perl = TRUE))) {
  chromote::set_chrome_args(c(
    chrome_args,
    paste0("--user-data-dir=", tempfile("draw-foundation-chrome-"))
  ))
}
b <- chromote::ChromoteSession$new(width = 1100, height = 650)
manifest[["browser"]] <- b$Browser$getVersion()
b$Page$addScriptToEvaluateOnNewDocument(
  source = paste(
    "window.__qaErrors=[];window.addEventListener('error',e=>window.__qaErrors.push(e.message));",
    paste(readLines("r/tools/visual-qa/foundation.js"), collapse = "\n")
  )
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
        for (width in c(1100L, if (name == "panels") 760L else 390L)) {
          key <- paste(name, mode, width, sep = "-")
          b$Emulation$setDeviceMetricsOverride(
            width = width,
            height = 650L,
            deviceScaleFactor = 1,
            mobile = FALSE
          )
          b$go_to(paste0("file://", html))
          wait_for(
            "!!foundationQA.chart()?.getZr().storage.getDisplayList(true).length"
          )
          wait_for(
            "foundationQA.charts().every(c=>c.getZr().animation.isFinished())"
          )
          # Fixed-aspect widgets may grow taller than their requested height.
          # Keep the complete chart in the native pointer/capture viewport.
          capture_height <- max(
            650L,
            evaluate(
              "Math.ceil(Math.max(...foundationQA.charts().map(c=>c.getDom().getBoundingClientRect().bottom)) + 20)"
            )
          )
          if (capture_height > 650L) {
            b$Emulation$setDeviceMetricsOverride(
              width = width,
              height = capture_height,
              deviceScaleFactor = 1,
              mobile = FALSE
            )
            wait_for(
              "foundationQA.charts().every(c=>c.getZr().animation.isFinished())"
            )
          }
          evaluate(
            "new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))"
          )
          if (name == "calibration") {
            wait_for("foundationQA.linePointsAligned()")
            wait_for("foundationQA.chart().getZr().animation.isFinished()")
          }
          if (name == "heatmap_annotations") {
            stopifnot(isTRUE(evaluate("foundationQA.heatmapTracksAligned()")))
          }
          result <- evaluate("foundationQA.scene()")
          if (name == "panels") {
            result[["children"]] <- evaluate(
              "foundationQA.charts().map(c=>({width:c.getWidth(),height:c.getHeight(),series:c.getModel().getSeries().length}))"
            )
            stopifnot(
              length(result[["children"]]) == 2L,
              all(vapply(
                result[["children"]],
                function(x) {
                  x[["width"]] > 100 && x[["height"]] > 100 && x[["series"]] > 0
                },
                logical(1)
              ))
            )
          }
          expected_series <- if (name %in% names(extra_labels)) {
            length(widget[["x"]][["option"]][["series"]])
          } else if (name %in% c("pie", "rose", "sankey", "spectrogram")) {
            1L
          } else if (name %in% c("histogram", "density", "density_kernel")) {
            3L
          } else if (name %in% c("histogram_density", "histogram_unequal")) {
            6L
          } else if (name == "scatter") {
            9L
          } else if (name %in% c("violin", "violin_box")) {
            9L
          } else if (startsWith(name, "paired")) {
            5L
          } else if (name == "survival") {
            17L
          } else if (name == "calibration") {
            5L
          } else if (
            name %in% c("line", "area") || startsWith(name, "boxplot_")
          ) {
            3L
          } else {
            2L
          }
          stopifnot(
            length(result[["errors"]]) == 0L,
            length(result[["series"]]) == expected_series,
            all(vapply(
              result[["series"]],
              function(series) series[["count"]] > 0,
              logical(1)
            )),
            result[["grid"]][["width"]] > 100,
            result[["background"]] ==
              if (mode == "light") "#ffffff" else "#181818"
          )
          screenshot <- b$Page$captureScreenshot(
            format = "png",
            captureBeyondViewport = FALSE
          )
          writeBin(
            base64enc::base64decode(screenshot[["data"]]),
            file.path(out, paste0(key, ".png"))
          )

          # Check actual vector content and group labels at both export widths.
          svg_path <- file.path(out, paste0(key, ".svg"))
          save_drawing(widget, svg_path, width = width, height = 600)
          svg <- xml2::read_xml(svg_path)
          labels <- xml2::xml_text(xml2::xml_find_all(
            svg,
            './/*[local-name()="text"]'
          ))
          expected <- if (name %in% c("bar", "stacked", "panels")) {
            names(by_sex)
          } else {
            names(by_species)
          }
          if (startsWith(name, "paired")) {
            expected <- c("Drug 1", "Drug 2", "Paired p = 0.003")
          } else if (name == "spectrogram") {
            expected <- character()
          } else if (name %in% c("calibration", "survival")) {
            expected <- unlist(widget[["x"]][["option"]][["legend"]][["data"]])
          }
          if (name %in% names(extra_labels)) {
            expected <- extra_labels[[name]]
          }
          stopifnot(
            all(expected %in% labels),
            length(xml2::xml_find_all(svg, './/*[local-name()="path"]')) > 0L,
            length(xml2::xml_find_all(svg, './/*[local-name()="image"]')) == 0L
          )
          converter <- Sys.which("rsvg-convert")
          if (nzchar(converter)) {
            stopifnot(
              system2(
                converter,
                c(
                  shQuote(svg_path),
                  "-o",
                  shQuote(file.path(out, paste0(key, "-svg.png")))
                )
              ) ==
                0
            )
          }

          # Hide every layer belonging to the first legend group, then restore it.
          legend <- evaluate("foundationQA.legend()")
          if (!is.null(legend)) {
            result[["legend"]] <- list(name = legend[["name"]], states = list())
            for (selected in c(FALSE, TRUE)) {
              click(legend[["point"]])
              state <- evaluate(paste0(
                "foundationQA.selection(",
                jsonlite::toJSON(legend[["name"]], auto_unbox = TRUE),
                ")"
              ))
              stopifnot(
                identical(state[["selected"]], selected),
                length(state[["layers"]]) ==
                  if (name == "scatter") {
                    3L
                  } else if (name %in% c("violin", "violin_box")) {
                    3L
                  } else if (name == "survival") {
                    8L
                  } else if (
                    name %in%
                      c(
                        "calibration",
                        "histogram_density",
                        "histogram_unequal",
                        "timeseries",
                        "supplied_fit"
                      )
                  ) {
                    2L
                  } else {
                    1L
                  },
                all(vapply(
                  state[["layers"]],
                  function(layer) identical(layer[["filtered"]], !selected),
                  logical(1)
                ))
              )
              result[["legend"]][["states"]][[
                if (selected) "restored" else "hidden"
              ]] <- state
            }
          }
          wait_for(
            "foundationQA.charts().every(c=>c.getZr().animation.isFinished())"
          )
          # Legend actions schedule a fresh render on the next frame. Observe
          # that render before measuring its hit target (zero-height entering
          # bars are not yet hoverable even if the previous frame was idle).
          evaluate(
            "new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(()=>r(true))))"
          )
          wait_for(
            "foundationQA.charts().every(c=>c.getZr().animation.isFinished())"
          )
          point <- evaluate("foundationQA.hoverPoint()")
          b$Input$dispatchMouseEvent(
            type = "mouseMoved",
            x = point[["x"]],
            y = point[["y"]]
          )
          wait_for("foundationQA.tooltip().length > 0")
          result[["tooltip"]] <- evaluate("foundationQA.tooltip()")
          screenshot <- b$Page$captureScreenshot(
            format = "png",
            captureBeyondViewport = FALSE
          )
          writeBin(
            base64enc::base64decode(screenshot[["data"]]),
            file.path(out, paste0(key, "-hover.png"))
          )

          # Density overlays expose evaluated values through native transparent
          # line symbols, as ROC does. Test a real pointer hover on the curve.
          if (name %in% c("histogram_density", "histogram_unequal")) {
            point <- evaluate(
              "(()=>{const c=foundationQA.chart(),s=c.getModel().getSeries().find(s=>s.subType==='line'),d=s.getData();let i=0;for(let j=1;j<d.count();j++){if(d.getRawDataItem(j)[1]>d.getRawDataItem(i)[1])i=j;}const p=s.coordinateSystem.dataToPoint(d.getRawDataItem(i));return foundationQA.pagePoint(p[0],p[1]);})()"
            )
            b$Input$dispatchMouseEvent(
              type = "mouseMoved",
              x = point[["x"]],
              y = point[["y"]]
            )
            wait_for("foundationQA.tooltip().length > 0")
            result[["curve_tooltip"]] <- evaluate("foundationQA.tooltip()")
          }

          # Only line/area expose zoom here; wheel and double-click use real input.
          if (name %in% c("line", "area")) {
            point <- evaluate("foundationQA.center()")
            b$Input$dispatchMouseEvent(
              type = "mouseWheel",
              x = point[["x"]],
              y = point[["y"]],
              deltaX = 0,
              deltaY = -240
            )
            wait_for("foundationQA.zoom().some(r=>r[1]-r[0] < 99)")
            result[["zoomed"]] <- evaluate("foundationQA.zoom()")
            click(point, 2L)
            wait_for("foundationQA.zoom().every(r=>r[0]===0&&r[1]===100)")
            result[["reset"]] <- evaluate("foundationQA.zoom()")
          }
          stopifnot(length(evaluate("window.__qaErrors")) == 0L)
          manifest[["cases"]][[key]] <- result
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
cat("Inspect the browser and SVG PNGs in", out, "\n")
