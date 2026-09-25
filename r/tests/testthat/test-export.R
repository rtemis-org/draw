# test-export.R
# Tests for save_drawing(). The SVG path itself needs a `node` binary, so the
# node-free tests here cover the guards and, importantly, that a rejected
# export leaves nothing behind at the caller's path.

test_that("save_drawing rejects a non-widget", {
  expect_error(save_drawing(list(), tempfile(fileext = ".svg")), "htmlwidget")
})


test_that("save_drawing requires an extension", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  expect_error(save_drawing(w, file.path(tempdir(), "chart")), "extension")
})


test_that("an unsupported format errors without creating the file", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".png")
  expect_error(save_drawing(w, path), "only supports")
  expect_false(file.exists(path))
})


test_that("a failed SVG export leaves no file at the destination", {
  skip_if(
    nzchar(Sys.which("node")),
    "node is installed, so the export succeeds"
  )
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".svg")
  expect_error(save_drawing(w, path), "Node.js")
  expect_false(file.exists(path))
})


test_that("save_drawing writes the SVG and returns its path invisibly", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  expect_invisible(save_drawing(w, path))
  expect_true(file.exists(path))
  expect_match(readLines(path, n = 1L, warn = FALSE), "svg")
})


test_that("static preparation drops only interaction callbacks", {
  callback <- htmlwidgets::JS("function(p) { return p.value; }")
  x <- list(
    tooltip = list(formatter = callback, trigger = "item"),
    xAxis = list(axisPointer = list(label = list(formatter = callback))),
    toolbox = list(feature = list(custom = list(onclick = callback)))
  )
  out <- strip_js(x)
  expect_null(out[["tooltip"]][["formatter"]])
  expect_identical(out[["tooltip"]][["trigger"]], "item")
  expect_null(out[["xAxis"]][["axisPointer"]][["label"]][["formatter"]])
  expect_null(out[["toolbox"]][["feature"]][["custom"]][["onclick"]])
  expect_error(
    strip_js(list(label = list(formatter = callback))),
    "option.label.formatter",
    class = "rtemis_export_error"
  )
  expect_error(
    strip_js(list(renderItem = callback)),
    "option.renderItem",
    class = "rtemis_export_error"
  )
})


test_that("static preparation preserves missing data dimensions and positions", {
  x <- list(
    data = list(list(0L, 0L, NULL), NULL, list(2L, 0L, 1.23456789)),
    absent = NULL
  )
  out <- strip_js(x)
  expect_identical(out[["data"]], x[["data"]])
  expect_false("absent" %in% names(out))
})


test_that("unsupported callbacks and backends leave existing output untouched", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  writeLines("existing output", path)
  w[["x"]][["option"]][["series"]][[1L]][["label"]] <- list(
    formatter = htmlwidgets::JS("function(p) { return p.name; }")
  )
  expect_error(
    save_drawing(w, path),
    "series.*label.formatter",
    class = "rtemis_export_error"
  )
  expect_identical(readLines(path), "existing output")
  class(w) <- c("rtemis-network", "htmlwidget")
  expect_error(
    save_drawing(w, path),
    "ECharts widgets only",
    class = "rtemis_export_error"
  )
  expect_identical(readLines(path), "existing output")
})


test_that("export validates paths and image dimensions", {
  w <- draw_bar(x = c("A", "B"), y = c(1, 2))
  for (path in list(NULL, NA_character_, "", c("a.svg", "b.svg"))) {
    expect_error(
      save_drawing(w, path),
      "filename",
      class = "rtemis_input_error"
    )
  }
  for (size in list(NA_real_, Inf, 0, -1, c(100, 200), "800", 1i)) {
    expect_error(
      save_drawing(w, tempfile(fileext = ".svg"), width = size),
      "width",
      class = "rtemis_input_error"
    )
    expect_error(
      save_drawing(w, tempfile(fileext = ".svg"), height = size),
      "height",
      class = "rtemis_input_error"
    )
  }
})


test_that("Gantt SVG contains proportional bars, failure outlines, and labels", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  tasks <- data.frame(
    label = c("Load", "Fit", "Predict"),
    start = c(0, 2, 4),
    end = c(2, 6, 10),
    failed = c(FALSE, TRUE, FALSE)
  )
  w <- draw_gantt(
    tasks,
    palette = "#126789",
    border = "failed",
    border_color = "#ed3456",
    border_width = 2,
    zoom = FALSE
  )
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(w, path, width = 800, height = 600)
  svg <- readLines(path, warn = FALSE)
  bars <- grep('<path .*fill="#126789"', svg, value = TRUE)
  expect_length(bars, nrow(tasks))
  expect_equal(sum(grepl('stroke="#ed3456" stroke-width="2"', bars)), 1L)
  # The SVG rectangle path contains an initial move followed by its width.
  # Durations 2, 4, 6 must remain proportional, not collapse during SSR.
  widths <- as.numeric(sub('.*d="M[^l]+l([-0-9.]+).*', '\\1', bars))
  expect_true(all(is.finite(widths) & widths > 0))
  expect_equal(widths / widths[[1L]], c(1, 2, 3), tolerance = 0.002)
  for (label in tasks[["label"]]) {
    expect_true(any(grepl(paste0(">", label, "</text>"), svg, fixed = TRUE)))
  }
  expect_false(any(grepl("<image|@keyframes|<animate", svg)))

  # The renderer identity and its parameters survive a plain JSON round trip.
  # No executable JavaScript is needed to reconstruct the static scene.
  w[["x"]][["option"]] <- jsonlite::fromJSON(
    jsonlite::toJSON(
      strip_js(w[["x"]][["option"]]),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    simplifyVector = FALSE
  )
  save_drawing(w, path)
  expect_identical(readLines(path, warn = FALSE), svg)
})


test_that("Gantt SVG preserves rounded corners and a zero-duration task", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw_gantt(
    data.frame(label = c("Milestone", "Run"), start = c(0, 0), end = c(0, 5)),
    palette = "#126789",
    bar_radius = 3,
    zoom = FALSE
  )
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(w, path)
  svg <- readLines(path, warn = FALSE)
  bars <- grep('<path .*fill="#126789"', svg, value = TRUE)
  expect_length(bars, 2L)
  expect_match(bars[[2L]], "A3 3", fixed = TRUE)
  expect_match(bars[[1L]], "A0.5 0.5", fixed = TRUE)
})


test_that("heatmap SVG retains cells, labels, and clipped dendrogram merges", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  m <- matrix(
    c(1.23456, 2, 3, 4, 5, 6),
    nrow = 2L,
    dimnames = list(c("rowA", "rowB"), c("colA", "colB", "colC"))
  )
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  # Both opposite placements exercise the shared row/column renderer with
  # normal and reversed height axes. Single-panel cases are checked too.
  cases <- list(
    list(cluster_rows = TRUE, cluster_cols = TRUE),
    list(
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      dendro_row_side = "left",
      dendro_col_side = "bottom"
    ),
    list(cluster_rows = TRUE),
    list(cluster_cols = TRUE)
  )
  for (args in cases) {
    w <- do.call(
      draw_heatmap,
      c(
        list(
          x = m,
          show_values = TRUE,
          value_digits = 4L,
          dendro_color = "#ed3456",
          square_cells = FALSE
        ),
        args
      )
    )
    save_drawing(w, path)
    svg <- readLines(path, warn = FALSE)
    merges <- grep('<polyline .*stroke="#ed3456"', svg, value = TRUE)
    expected <- if (isTRUE(args[["cluster_rows"]])) 1L else 0L
    expected <- expected + if (isTRUE(args[["cluster_cols"]])) 2L else 0L
    expect_length(merges, expected)
    points <- sub('.*points="([^"]+)".*', '\\1', merges)
    expect_true(all(lengths(strsplit(points, " ", fixed = TRUE)) == 8L))
    expect_true(any(grepl('clip-path="url\\(#', svg)))
    expect_true(any(grepl("<clipPath", svg, fixed = TRUE)))
    expect_equal(sum(grepl('<path .*ecmeta_series_index=', svg)), length(m))
    for (label in c(rownames(m), colnames(m), "1.2346", "6.0000")) {
      expect_true(any(grepl(paste0(">", label, "</text>"), svg, fixed = TRUE)))
    }
    expect_false(any(grepl("<image|@keyframes|<animate", svg)))
  }
})


test_that("missing heatmap cells remain missing with materialized value labels", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw_heatmap(
    matrix(c(1.23456, NA, 2, 3), 2L),
    show_values = TRUE,
    value_digits = 3L,
    show_colorbar = FALSE
  )
  data <- w[["x"]][["option"]][["series"]][[1L]][["data"]]
  expect_identical(data[[1L]][["label"]][["formatter"]], "1.235")
  expect_length(data[[3L]][["value"]], 3L)
  expect_null(data[[3L]][["value"]][[3L]])
  expect_identical(data[[3L]][["label"]][["formatter"]], "")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(w, path)
  svg <- readLines(path, warn = FALSE)
  expect_equal(sum(grepl('<path .*ecmeta_series_index=', svg)), 3L)
  expect_true(any(grepl(">1.235</text>", svg, fixed = TRUE)))
})


test_that("unknown custom renderers fail without replacing an existing file", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw_gantt(data.frame(label = "Task", start = 0, end = 1))
  w[["x"]][["option"]][["series"]][[1L]][["renderItem"]] <- "unregistered"
  # An empty series must still be rejected, rather than silently skipped.
  w[["x"]][["option"]][["series"]][[1L]][["data"]] <- list()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  writeLines("existing output", path)
  expect_error(
    save_drawing(w, path),
    "Unsupported custom-series renderer",
    class = "rtemis_export_error"
  )
  expect_identical(readLines(path), "existing output")
})


test_that("SVG export retains numeric precision in point positions and limits", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  w <- draw(list(
    grid = list(
      left = 100,
      right = 100,
      top = 100,
      bottom = 100,
      containLabel = FALSE
    ),
    xAxis = list(type = "value", min = 0, max = 1),
    yAxis = list(type = "value", min = 1, max = 1.000004),
    series = list(list(
      type = "scatter",
      data = list(
        list(0.25, 1.000001),
        list(0.75, 1.000003)
      )
    ))
  ))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(w, path, width = 800, height = 600)
  svg <- readLines(path, warn = FALSE)
  points <- grep('<path .*ecmeta_series_index=', svg, value = TRUE)
  expect_length(points, 2L)
  y <- as.numeric(sub(
    '.*transform="matrix\\([^,]+,[^,]+,[^,]+,[^,]+,[^,]+,([-0-9.]+)\\)".*',
    '\\1',
    points
  ))
  expect_equal(y, c(400, 200), tolerance = 0.1)
})


test_that("SVG dimensions follow numeric widget dimensions unless overridden", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  w <- draw_bar(c("A", "B"), c(1, 2), width = 950, height = 450)
  save_drawing(w, path)
  expect_match(
    readLines(path, n = 1L),
    'width="950" height="450"',
    fixed = TRUE
  )
  save_drawing(w, path, width = 640, height = 360)
  expect_match(
    readLines(path, n = 1L),
    'width="640" height="360"',
    fixed = TRUE
  )
  w[["width"]] <- "100%"
  w[["height"]] <- NULL
  save_drawing(w, path)
  expect_match(
    readLines(path, n = 1L),
    'width="800" height="600"',
    fixed = TRUE
  )
})
