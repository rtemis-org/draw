test_that("legend properties share validation, schema, and provenance", {
  expect_named(legend_properties(), c("legend_position", "legend_placement"))
  expect_identical(
    to_list(LegendLayout()),
    list(legend_position = "top", legend_placement = "outside")
  )
  expect_error(legend_meta("outside"))
  expect_error(legend_meta("top", "beside"))
  expect_error(legend_meta(NA_character_))
  expect_error(legend_meta(c("top", "left")))
  classes <- list(
    LineConfig,
    BarConfig,
    ScatterConfig,
    BoxplotConfig,
    DensityConfig,
    HistogramConfig,
    PieConfig,
    GanttConfig,
    ROCConfig,
    SignificanceConfig,
    HeatmapConfig,
    SpectrogramConfig
  )
  for (Class in classes) {
    setup <- get(paste0("setup_", Class@name))
    config <- setup(legend_position = "top-right", legend_placement = "inside")
    expect_identical(config@origin[["legend_position"]], "user")
    expect_identical(config@origin[["legend_placement"]], "user")
    schema <- chart_schema(
      Class,
      id = "https://example.org/chart.json",
      title = "Chart",
      description = "Chart config."
    )
    expect_identical(
      as.character(schema[["properties"]][["legend_position"]][["enum"]]),
      LEGEND_POSITIONS
    )
    expect_identical(
      as.character(schema[["properties"]][["legend_placement"]][["enum"]]),
      c("outside", "inside")
    )
    for (complete in c(FALSE, TRUE)) {
      path <- tempfile(fileext = ".json")
      write_chart_config(config, path, complete = complete)
      back <- read_chart_config(path)
      expect_identical(back@legend_position, config@legend_position)
      expect_identical(back@legend_placement, config@legend_placement)
      unlink(path)
    }
  }
  expect_identical(setup_HeatmapConfig()@legend_position, "right")
  expect_identical(
    setup_HeatmapConfig(colorbar_orient = "horizontal")@legend_position,
    "bottom"
  )
  expect_identical(setup_ChoroplethConfig()@legend_position, "bottom-right")
})

test_that("high-level and config legend hints agree without adding legends", {
  direct <- draw_boxplot(
    iris[1:2],
    group = iris[["Species"]],
    legend_position = "bottom-left",
    legend_placement = "inside"
  )
  configured <- draw(
    setup_BoxplotConfig(
      x = names(iris)[1:2],
      group = "Species",
      legend_position = "bottom-left",
      legend_placement = "inside"
    ),
    data = iris
  )
  for (field in c("legendPosition", "legendPlacement")) {
    expect_identical(direct[["x"]][[field]], configured[["x"]][[field]])
  }
  expect_identical(direct[["x"]][["option"]], configured[["x"]][["option"]])
  expect_null(draw_boxplot(iris[1], group = iris[["Species"]])[["x"]][[
    "option"
  ]][["legend"]])
  expect_null(draw_line(1:3, 2:4)[["x"]][["option"]][["legend"]])
  expect_identical(
    draw_roc(data.frame(fpr = c(0, 1), tpr = c(0, 1)))[["x"]][[
      "legendPlacement"
    ]],
    "outside"
  )
  panels <- draw_panels(list(direct, configured))
  expect_identical(
    panels[["x"]][["panels"]][[1]][["legendPosition"]],
    "bottom-left"
  )
  # Native low-level placement stays caller-owned, without semantic hints.
  expect_null(draw(EChartsOption(legend = Legend(right = 5)))[["x"]][[
    "legendPosition"
  ]])
})

test_that("native legends reserve space across families, anchors, and resizes", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  charts <- list(
    boxplot = draw_boxplot(
      iris[1:2],
      group = iris[["Species"]],
      title = "Iris measurements"
    ),
    line = draw_line(
      1:3,
      list(Training = c(3, 2, 1), Validation = c(4, 3, 2)),
      zoom = TRUE,
      title = "Learning"
    ),
    scatter = draw_scatter(
      iris[[1]],
      iris[[2]],
      group = iris[["Species"]],
      title = "Iris"
    ),
    pie = draw_pie(
      c(10, 20, 30),
      labels = c("A", "B", "C"),
      title = "Composition"
    ),
    roc = draw_roc(
      data.frame(
        fpr = rep(c(0, .2, 1), 3),
        tpr = rep(c(0, .8, 1), 3),
        class = rep(c("A", "B", "C"), each = 3)
      ),
      title = "Validation"
    ),
    heatmap = draw_heatmap(
      cor(mtcars[1:4]),
      show_values = TRUE,
      title = "Correlation"
    )
  )
  long <- charts[["boxplot"]]
  for (i in seq_along(long[["x"]][["option"]][["series"]])) {
    long[["x"]][["option"]][["series"]][[i]][["name"]] <- paste(
      "A longer group label",
      i
    )
  }
  long[["x"]][["option"]][["legend"]][["data"]] <- lapply(1:3, function(i) {
    paste("A longer group label", i)
  })
  charts[["long"]] <- long
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = lapply(charts, function(x) strip_js(x[["x"]])),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      ),
      positions = as.list(LEGEND_POSITIONS)
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  result <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "legend_layout.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "Shared legend layout passed")
})
