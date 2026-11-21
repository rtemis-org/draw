# test-export_baseline.R
# The SVG export of a fixed set of charts, held as snapshots: the layout the
# shared fit computes (legend placement, the aspect box, square cells, keys,
# A3, confusion panels, panel compositions) is visible in them. zrender's
# element ids and the package version are normalized out, so a change in the
# number of setOption passes leaves a snapshot equal and a change of geometry
# does not. spec: draw/chart-appearance#build-and-fit

# The SVG with zrender's ids renumbered in order of appearance and the
# version comment removed.
normalize_svg <- function(svg) {
  svg <- sub("<!-- Created by rtemis.draw [^>]* -->", "", svg)
  for (pattern in c(
    "zr\\d+-cls-\\d+",
    "zr\\d+-c\\d+",
    "zr\\d+-g\\d+",
    "zr\\d+-p\\d+"
  )) {
    ids <- unique(regmatches(svg, gregexpr(pattern, svg))[[1L]])
    if (length(ids) == 0L) {
      next
    }
    prefix <- sub("\\d+$", "", sub("^zr\\d+", "zr", ids[[1L]]))
    keys <- paste0("\001", seq_along(ids), "\002")
    for (i in seq_along(ids)) {
      svg <- gsub(paste0(ids[[i]], "(?![0-9])"), keys[[i]], svg, perl = TRUE)
    }
    for (i in seq_along(ids)) {
      svg <- gsub(keys[[i]], paste0(prefix, i - 1L), svg, fixed = TRUE)
    }
  }
  svg
}

# Writes the normalized SVG of `widget` and returns its path.
baseline_svg <- function(widget, name, width = 640, height = 420) {
  raw <- tempfile(fileext = ".svg")
  on.exit(unlink(raw), add = TRUE)
  save_drawing(widget, raw, width = width, height = height)
  svg <- paste(
    readLines(raw, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  path <- file.path(tempdir(), paste0(name, ".svg"))
  writeLines(normalize_svg(svg), path, useBytes = TRUE)
  path
}

baseline_cases <- function() {
  set.seed(2026)
  n <- 60
  d <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    g = rep(c("Adelie", "Chinstrap", "Gentoo"), length.out = n),
    s = runif(n, 1, 10)
  )
  scatter <- function(position, placement, title = NULL) {
    # The loop below reuses its variables; each case keeps its own values.
    force(position)
    force(placement)
    force(title)
    function() {
      draw_scatter(
        d$x,
        d$y,
        group = d$g,
        xlab = "Bill length",
        ylab = "Bill depth",
        legend_position = position,
        legend_placement = placement,
        legend_title = title
      )
    }
  }
  cases <- list()
  for (position in c("top", "bottom", "left", "right")) {
    for (placement in c("outside", "inside")) {
      cases[[paste("scatter-legend", position, placement, sep = "-")]] <-
        scatter(position, placement)
    }
  }
  cases[["scatter-legend-title-top"]] <- scatter("top", "outside", "Species")
  cases[["scatter-legend-title-right"]] <- scatter(
    "right",
    "outside",
    "Species"
  )
  cases[["scatter-keys"]] <- function() {
    draw_scatter(
      d$x,
      d$y,
      color = d$y,
      size = d$s,
      xlab = "X",
      ylab = "Y"
    )
  }
  labels <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  probability <- plogis(2 * (as.integer(labels) - 1.5) + rnorm(n))
  cases[["roc"]] <- function() draw_roc(labels, probability)
  cases[["calibration"]] <- function() {
    draw_calibration(labels, probability, n_bins = 5L)
  }
  predicted <- factor(
    ifelse(probability > 0.5, "yes", "no"),
    levels = c("no", "yes")
  )
  cases[["confusion"]] <- function() draw_confusion(labels, predicted)
  m <- cor(mtcars[c("mpg", "disp", "hp", "wt", "qsec")])
  cases[["heatmap-square"]] <- function() {
    draw_heatmap(m, square_cells = TRUE, show_values = TRUE)
  }
  cases[["heatmap-clustered"]] <- function() {
    draw_heatmap(m, cluster_rows = TRUE, cluster_cols = TRUE)
  }
  cases[["pie"]] <- function() {
    draw_pie(c(12, 30, 8, 20), labels = c("A", "B", "C", "D"))
  }
  cases[["sankey"]] <- function() {
    draw_sankey(data.frame(
      source = c("a", "a", "b"),
      target = c("b", "c", "c"),
      value = c(4, 2, 3)
    ))
  }
  cases[["gantt"]] <- function() {
    draw_gantt(
      data.frame(
        label = c("load", "clean", "train", "predict"),
        start = c(0, 12, 30, 95),
        end = c(12, 30, 95, 100),
        status = c("ok", "ok", "ok", "error")
      ),
      group = "status"
    )
  }
  cases[["boxplot-zoom-bottom"]] <- function() {
    draw_boxplot(
      split(d$y, d$g),
      zoom = TRUE,
      legend_position = "bottom"
    )
  }
  cases[["scatter-dark"]] <- function() {
    draw_scatter(d$x, d$y, group = d$g, theme = theme_dark())
  }
  cases[["panels"]] <- function() {
    draw_panels(
      list(
        draw_scatter(d$x, d$y, group = d$g),
        draw_roc(labels, probability),
        draw_heatmap(m, square_cells = TRUE)
      ),
      ncol = 3L
    )
  }
  cases
}

test_that("SVG exports keep their layout", {
  skip_if_no_node()
  cases <- baseline_cases()
  for (name in names(cases)) {
    expect_snapshot_file(
      baseline_svg(cases[[name]](), name),
      paste0(name, ".svg")
    )
  }
  # A narrow surface wraps the legend.
  expect_snapshot_file(
    baseline_svg(
      cases[["scatter-legend-title-top"]](),
      "narrow-legend",
      390,
      520
    ),
    "narrow-legend.svg"
  )
  # Three panels side by side at 1200 pixels; the `panels` case draws them at 640.
  expect_snapshot_file(
    baseline_svg(cases[["panels"]](), "panels-wide", 1200, 420),
    "panels-wide.svg"
  )
})

test_that("A3 SVG exports keep their layout, bounded and unbounded", {
  skip_if_not_installed("rtemis.a3")
  skip_if_no_node()
  a3 <- rtemis.a3::create_A3(
    "MAEPRQEFEVMEDHAGTYGLGDRKDQGGYTMHQDQEGDTDAGLK",
    region = list(
      Domain = rtemis.a3::annotation_range(matrix(c(3L, 10L), ncol = 2))
    ),
    site = list(Site = rtemis.a3::annotation_position(8L)),
    ptm = list(Phosphorylation = rtemis.a3::annotation_position(c(5L, 18L))),
    processing = list(Cleavage = rtemis.a3::annotation_position(17L))
  )
  widget <- draw_a3(a3, n_per_row = 12L)
  raw <- tempfile(fileext = ".svg")
  on.exit(unlink(raw), add = TRUE)
  # Unbounded: the export takes the layout's height.
  save_drawing(widget, raw, width = 900)
  svg <- paste(
    readLines(raw, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  path <- file.path(tempdir(), "a3-unbounded.svg")
  writeLines(normalize_svg(svg), path, useBytes = TRUE)
  expect_snapshot_file(path, "a3-unbounded.svg")
  expect_snapshot_file(
    baseline_svg(widget, "a3-bounded", 900, 360),
    "a3-bounded.svg"
  )
})

test_that("3D scatter SVG exports keep their layout, wide and narrow", {
  skip_if_no_node()
  set.seed(7)
  widget <- draw_scatter3d(
    rnorm(40),
    rnorm(40),
    rnorm(40),
    group = rep(c("a", "b"), 20)
  )
  expect_snapshot_file(baseline_svg(widget, "scatter3d"), "scatter3d.svg")
  expect_snapshot_file(
    baseline_svg(widget, "scatter3d-narrow", 390, 420),
    "scatter3d-narrow.svg"
  )
})

test_that("normalize_svg renumbers zrender ids in order of appearance", {
  svg <- paste(
    '<!-- Created by rtemis.draw 9.9.9 -->',
    '<g clip-path="url(#zr3-c7)"><path class="zr3-cls-4"></path>',
    '<path class="zr3-cls-12"></path><clipPath id="zr3-c7"></clipPath></g>'
  )
  expect_identical(
    normalize_svg(svg),
    paste(
      "",
      '<g clip-path="url(#zr-c0)"><path class="zr-cls-0"></path>',
      '<path class="zr-cls-1"></path><clipPath id="zr-c0"></clipPath></g>'
    )
  )
})
