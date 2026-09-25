metric_records_fixture <- function() {
  data.frame(
    fold = rep(c("Fold1", "Fold2", "Fold3"), 2),
    split = rep(c("Training", "Test"), each = 3),
    metric = "rsq",
    value = c(.8, .9, .7, .6, NA, -.5)
  )
}

test_that("boxplot statistics state their quartile and whisker conventions", {
  values <- c(0, 1, 2, 3, 4, 20)
  s <- boxplot_summary(values)
  expect_equal(s[["stats"]], c(0, 1.25, 2.5, 3.75, 4))
  expect_identical(s[["outlier"]], c(rep(FALSE, 5), TRUE))
  expect_equal(
    boxplot_summary(values, "hinges")[["stats"]],
    grDevices::boxplot.stats(values)[["stats"]]
  )
  expect_equal(
    boxplot_summary(values, whisker = 0)[["stats"]],
    c(0, 1.25, 2.5, 3.75, 20)
  )
  expect_equal(
    boxplot_summary(1:4, whisker = .01)[["stats"]],
    c(1.75, 1.75, 2.5, 3.25, 3.25)
  )
  expect_equal(boxplot_summary(7)[["stats"]], rep(7, 5))
  expect_equal(boxplot_summary(rep(7, 4))[["stats"]], rep(7, 5))
  expect_true(all(is.na(boxplot_summary(numeric())[["stats"]])))
  expect_equal(boxplot_summary(c(1, NA, 3))[["at"]], c(1L, 3L))
  expect_error(boxplot_summary(c(1, NA), na_rm = FALSE), "na_rm")
  for (bad in list(c(1, Inf), "1", matrix(1), complex(1))) {
    expect_error(boxplot_summary(bad), "numeric boxplot")
  }
})

test_that("boxplot points preserve every selected row and stable identities without RNG", {
  set.seed(41)
  state <- .Random.seed
  w <- draw_boxplot(
    list(A = c(0:4, 20), B = c(NA, 1:5)),
    boxpoints = "all",
    observation = letters[1:6],
    verbosity = 0L
  )
  expect_identical(.Random.seed, state)
  points <- w[["x"]][["option"]][["series"]][[2]][["data"]]
  expect_length(points, 11)
  expect_equal(
    vapply(points[1:6], function(p) p[["value"]][[2]], 0),
    c(0:4, 20)
  )
  expect_identical(
    vapply(points[7:11], function(p) p[["value"]][[3]], ""),
    letters[2:6]
  )
  out <- draw_boxplot(c(0:4, 20), boxpoints = "outliers")[["x"]][["option"]][[
    "series"
  ]][[2]][["data"]]
  expect_length(out, 1)
  # IDs differ (automatic row ID vs explicit letter); coordinates/offset do not.
  expect_equal(
    out[[1]][["value"]][c(1, 2, 4)],
    points[[6]][["value"]][c(1, 2, 4)]
  )
  expect_equal(boxplot_offsets(as.numeric(1:4)), c(0, -.25, .25, -.375))
  centered <- draw_boxplot(1:3, boxpoints = "all", point_spread = 0)
  expect_true(all(vapply(
    centered[["x"]][["option"]][["series"]][[2]][["data"]],
    function(p) p[["value"]][[4]] == 0,
    logical(1)
  )))
  expect_error(
    draw_boxplot(list(1:3, 1:2), observation = letters[1:3]),
    "identifier"
  )
  expect_error(draw_boxplot(1:3, group = 1:2), "group value")
  expect_error(draw_boxplot(list(1:2, 3:4), labels = "A"), "label")
  expect_error(draw_boxplot(c(NA, NA)), "available")
})

test_that("grouped points share the box legend and retain horizontal mappings", {
  w <- draw_boxplot(
    list(A = 1:6, B = 7:12),
    group = c("x", "y", "x", NA, "y", "y"),
    boxpoints = "all",
    horizontal = TRUE
  )
  o <- w[["x"]][["option"]]
  expect_length(o[["series"]], 4)
  expect_equal(vapply(o[["series"]], `[[`, "", "name"), c("x", "y", "x", "y"))
  expect_equal(lengths(lapply(o[["series"]][3:4], `[[`, "data")), c(4, 6))
  expect_equal(o[["series"]][[3]][["itemPayload"]][["boxSeries"]], list(0L, 1L))
  expect_equal(o[["series"]][[3]][["encode"]][["x"]], 1)
  expect_equal(o[["series"]][[3]][["encode"]][["y"]], 0)
  expect_equal(o[["legend"]][["data"]], list("x", "y"))
})

test_that("metric records align folds and disclose missing values without zero fill", {
  d <- metric_records_fixture()
  prepared <- metric_data(d)
  expect_equal(prepared[["data"]][["Test"]], c(.6, NA, -.5))
  expect_equal(metric_data(d[-5, ])[["data"]], prepared[["data"]])
  w <- suppressMessages(draw_metric(d))[["x"]][["option"]]
  expect_equal(w[["yAxis"]][["name"]], "Rsq")
  expect_lt(w[["yAxis"]][["min"]], -.5)
  expect_match(w[["title"]][["subtext"]], "1 missing")
  expect_length(w[["series"]][[2]][["data"]], 5)
  h <- suppressMessages(draw_metric(d, horizontal = TRUE, boxpoints = "none"))[[
    "x"
  ]][["option"]]
  expect_equal(h[["xAxis"]][["name"]], "Rsq")
  expect_length(h[["series"]], 1)
  expect_null(suppressMessages(draw_metric(d, ylab = NULL))[["x"]][["option"]][[
    "yAxis"
  ]][["name"]])
  d[["value"]][4:6] <- NA_real_
  w <- suppressMessages(draw_metric(d))[["x"]][["option"]]
  expect_equal(w[["xAxis"]][["data"]], c("Training", "Test"))
  expect_true(all(is.na(w[["series"]][[1]][["data"]][[2]])))
  expect_error(draw_metric(transform(d, value = NA_real_)), "available")
  expect_error(draw_metric(rbind(d, d[1, ])), "exactly one")
  expect_error(draw_metric(transform(d, value = Inf)), "finite")
  expect_error(draw_metric(transform(d, fold = NA_character_)), "identities")
  more <- rbind(d, transform(d, metric = "rmse"))
  expect_error(draw_metric(more), "Choose one")
  expect_equal(metric_data(more, "rmse")[["metric"]], "rmse")
  expect_error(draw_metric(d, metric = "bad"), "available")
  expect_error(draw_metric(d[FALSE, ]), "nonempty")
  collision <- transform(
    d,
    split = ifelse(split == "Training", ".observation", split)
  )
  expect_equal(metric_data(collision)[["observation"]], ".observation_")
})

test_that("boxplot and observation marks export as vector geometry", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  for (horizontal in c(FALSE, TRUE)) {
    w <- draw_boxplot(
      list(A = 1:4, B = 5:8),
      group = rep(c("Train", "Test"), 2),
      boxpoints = "all",
      horizontal = horizontal,
      title = "Metric distributions"
    )
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path), add = TRUE)
    save_drawing(w, path)
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(svg, "Metric distributions")
    expect_match(svg, "Train")
    expect_match(svg, "Test")
    expect_false(grepl("<image", svg, fixed = TRUE))
    # Point circles are genuine SVG geometry, separate from box paths.
    expect_equal(
      lengths(regmatches(svg, gregexpr('<circle ', svg, fixed = TRUE))),
      8
    )
  }
})

test_that("point layout matches native boxes after legend filtering and resize", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  for (horizontal in c(FALSE, TRUE)) {
    w <- draw_boxplot(
      list(A = 1:4, B = 5:8),
      group = rep(c("Train", "Test"), 2),
      boxpoints = "all",
      horizontal = horizontal
    )
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    jsonlite::write_json(
      list(
        option = w[["x"]][["option"]],
        phone_option = draw_boxplot(
          list(Training = 1:4, Test = 2:5, Validation = 3:6),
          horizontal = horizontal
        )[["x"]][["option"]],
        horizontal = horizontal,
        echarts = system.file(
          "htmlwidgets/lib/echarts/echarts.min.js",
          package = "rtemis.draw"
        ),
        renderers = system.file(
          "htmlwidgets/lib/draw/renderers.js",
          package = "rtemis.draw"
        )
      ),
      path,
      auto_unbox = TRUE,
      null = "null"
    )
    output <- system2(
      Sys.which("node"),
      c(shQuote(test_path("fixtures", "boxplot_geometry.js")), shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    )
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    expect_match(paste(output, collapse = "\n"), "passed")
  }
})
