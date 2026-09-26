test_that("boxplot settings and bindings survive schema and config conversion", {
  cfg <- setup_BoxplotConfig(
    x = "score",
    observation = "fold",
    quartiles = "hinges",
    whisker = 0,
    boxpoints = "all",
    point_spread = 0
  )
  expect_s7_class(cfg, BoxplotConfig)
  expect_identical(cfg@origin[["boxpoints"]], "user")
  expect_false("group" %in% names(to_list(cfg)))
  for (args in list(
    list(quartiles = "bad"),
    list(whisker = -1),
    list(whisker = Inf),
    list(boxpoints = TRUE),
    list(point_spread = 2),
    list(point_size = 0),
    list(point_alpha = NA),
    list(observation = "")
  )) {
    expect_error(do.call(setup_BoxplotConfig, args))
  }
  data <- data.frame(score = c(1, 2, 3, 7), fold = letters[1:4])
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    back <- read_chart_config(path)
    expect_equal(back@whisker, 0)
    expect_identical(to_list(compile(back, data)), to_list(compile(cfg, data)))
  }
  schema <- chart_schema(
    BoxplotConfig,
    id = "https://example.org/box.json",
    title = "Boxplot",
    description = "Boxplot."
  )[["properties"]]
  expect_equal(
    as.character(schema[["quartiles"]][["enum"]]),
    c("linear", "hinges")
  )
  expect_equal(schema[["point_spread"]][["maximum"]], 1)
  expect_equal(schema[["point_size"]][["exclusiveMinimum"]], 0)
  expect_identical(
    compile(cfg, data),
    boxplot_option(
      list(score = data[["score"]]),
      observation = data[["fold"]],
      quartiles = "hinges",
      whisker = 0,
      boxpoints = "all",
      point_spread = 0
    )
  )
})

test_that("boxplot omission diagnostics do not affect labels or margins", {
  values <- c(32.1, 38.5, 39.2, 44.4, 48.8, 59.6)
  for (horizontal in c(FALSE, TRUE)) {
    for (title in list(NULL, "Bill length")) {
      for (margins in list(
        NULL,
        c(top = 12, right = 20, bottom = 30, left = 40)
      )) {
        expect_message(
          missing <- boxplot_option(
            c(values, NA, NA),
            horizontal = horizontal,
            title = title,
            margins = margins
          ),
          "Removed 2 NA values"
        )
        complete <- boxplot_option(
          values,
          horizontal = horizontal,
          title = title,
          margins = margins
        )
        # The entire compiled option, including statistics and geometry, agrees.
        expect_identical(to_list(missing), to_list(complete))
        expect_null(to_list(missing)[["title"]][["subtext"]])
      }
    }
  }
  expect_message(boxplot_option(c(values, NA), verbosity = 0), NA)
})

test_that("boxplot baselines emphasize zero and never a nonzero range edge", {
  for (horizontal in c(FALSE, TRUE)) {
    axis <- if (horizontal) "yAxis" else "xAxis"
    for (points in c("none", "all", "outliers")) {
      for (values in list(c(32, 39, 45, 49, 60), c(-60, -49, -45, -39, -32))) {
        opt <- to_list(boxplot_option(
          values,
          horizontal = horizontal,
          boxpoints = points
        ))
        expect_identical(
          opt[[axis]][["axisLine"]],
          list(show = FALSE, onZero = TRUE)
        )
      }
      for (values in list(c(-5, -2, 0, 2, 5), c(0, 1, 2, 3))) {
        opt <- to_list(boxplot_option(
          values,
          horizontal = horizontal,
          boxpoints = points
        ))
        expect_identical(
          opt[[axis]][["axisLine"]],
          list(show = TRUE, onZero = TRUE)
        )
      }
    }
  }
  # A hidden extreme does not give the visible box a spurious zero baseline.
  values <- c(-100, 30:50)
  hidden <- to_list(boxplot_option(values, boxpoints = "none"))
  shown <- to_list(boxplot_option(values, boxpoints = "all"))
  expect_false(hidden[["xAxis"]][["axisLine"]][["show"]])
  expect_true(shown[["xAxis"]][["axisLine"]][["show"]])
})

test_that("native boxplot SVG emphasizes zero in both orientations and themes", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  charts <- list()
  for (theme in list(theme_light(), theme_dark())) {
    for (horizontal in c(FALSE, TRUE)) {
      for (values in list(
        c(32, 39, 45, 49, 60),
        c(-60, -49, -45, -39, -32),
        c(-5, 0, 5)
      )) {
        charts[[length(charts) + 1L]] <- list(
          option = to_list(boxplot_option(
            c(values, NA, NA),
            horizontal = horizontal,
            verbosity = 0
          )),
          theme = to_list(theme),
          horizontal = horizontal,
          zero = min(values) <= 0 && max(values) >= 0
        )
      }
    }
  }
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = charts,
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    digits = NA
  )
  output <- system2(
    "node",
    c(shQuote(test_path("fixtures", "axis_baseline.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})
