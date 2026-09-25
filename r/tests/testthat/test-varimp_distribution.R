# Fixtures distinguish observed zero, sparse zero, unknown value, missing
# measure, and absent fold; none of these are interchangeable.
distribution_fixture <- function() {
  varimp_data(data.frame(
    variable = c("a", "b", "a", "b", "c", "c", "a"),
    fold = c("A", "A", "B", "B", "B", "C", "D"),
    gain = c(8, 2, NA, 4, -6, -3, NA)
  ))
}

test_that("fold distribution materialization agrees with the summary contract", {
  data <- distribution_fixture()
  before <- data
  folds <- LETTERS[1:5]
  info <- varimp_fold_info(data, "gain", folds)
  expect_identical(info, list(ids = folds, scored = c("A", "B", "C")))
  expect_null(varimp_fold_info(varimp_data(c(a = 1)), "importance")[["ids"]])
  for (policy in c("missing", "zero")) {
    selected <- summarize_varimp(data, absent = policy, folds = folds)
    records <- varimp_distribution(data, selected, "gain", policy, folds)
    wide <- records[["data"]]
    expect_identical(wide[[records[["observation"]]]], folds)
    expect_equal(wide[["a"]], c(8, NA, if (policy == "zero") 0 else NA, NA, NA))
    expect_equal(wide[["b"]], c(2, 4, if (policy == "zero") 0 else NA, NA, NA))
    expect_equal(
      wide[["c"]],
      c(if (policy == "zero") 0 else NA, -6, -3, NA, NA)
    )
    expect_identical(records[["columns"]], c("a", "c", "b"))
    for (i in seq_len(nrow(selected))) {
      values <- wide[[selected[["variable"]][[i]]]]
      expect_equal(mean(values, na.rm = TRUE), selected[["importance"]][[i]])
      expect_equal(sum(!is.na(values)), selected[["n_available"]][[i]])
    }
  }
  expect_identical(data, before)
  # C reports only an unselected variable. It still establishes a sparse zero.
  selected <- summarize_varimp(data, top_n = 1, absent = "zero", folds = folds)
  out <- varimp_distribution(data, selected, "gain", "zero", folds)
  expect_equal(out[["data"]][["a"]], c(8, NA, 0, NA, NA))
  expect_equal(ncol(out[["data"]]), 2)
  expect_equal(nrow(out[["data"]]), 5)
  reverse <- varimp_distribution(data, selected, "gain", "zero", rev(folds))
  expect_identical(reverse[["data"]][[".fold"]], rev(folds))
  expect_equal(reverse[["data"]][["a"]], rev(out[["data"]][["a"]]))
})

test_that("importance boxes retain ranking, fold IDs, missing counts and signed values", {
  data <- distribution_fixture()
  for (horizontal in c(TRUE, FALSE)) {
    w <- suppressMessages(draw_varimp(
      data,
      type = "boxplot",
      absent = "zero",
      folds = LETTERS[1:5],
      horizontal = horizontal
    ))
    opt <- w[["x"]][["option"]]
    axis <- if (horizontal) "yAxis" else "xAxis"
    value_axis <- if (horizontal) "xAxis" else "yAxis"
    labels <- c("a (2/5 folds)", "c (3/5 folds)", "b (3/5 folds)")
    expect_equal(opt[[axis]][["data"]], if (horizontal) rev(labels) else labels)
    expect_identical(opt[[value_axis]][["name"]], "Gain (available folds)")
    expect_identical(opt[[value_axis]][["nameLocation"]], "middle")
    expect_identical(opt[[axis]][["nameLocation"]], "middle")
    expect_equal(opt[["grid"]][["top"]], 48)
    expect_false(opt[[value_axis]][["axisLabel"]][["showMinLabel"]])
    expect_false(opt[[value_axis]][["axisLabel"]][["showMaxLabel"]])
    expect_match(opt[["title"]][["subtext"]], "7 missing")
    expect_identical(opt[["series"]][[1]][["type"]], "boxplot")
    points <- opt[["series"]][[2]][["data"]]
    expect_length(points, 8)
    values <- vapply(points, function(p) p[["value"]][[2]], numeric(1))
    expect_equal(sort(values), c(-6, -3, 0, 0, 0, 2, 4, 8))
    ids <- vapply(points, function(p) p[["value"]][[3]], "")
    expect_setequal(ids, c("A", "B", "C"))
    expect_lt(opt[[value_axis]][["min"]], -6)
    expect_gt(opt[[value_axis]][["max"]], 8)
  }
  expect_error(draw_varimp(c(a = 1), type = "boxplot"), "fold.*column")
  expect_error(draw_varimp(data, type = "violin"))
  expect_error(draw_varimp(data, type = NA_character_))
  expect_error(draw_varimp(data, type = "boxplot", whisker = -1))
  expect_error(draw_varimp(data, type = "boxplot", boxpoints = "bad"))
  expect_error(draw_varimp(data, type = "boxplot", group = "fold"))
  for (margin in c(0L, 90L)) {
    opt <- suppressMessages(draw_varimp(
      data,
      type = "boxplot",
      title = "VI",
      margin_top = margin
    ))[["x"]][["option"]]
    expect_equal(opt[["grid"]][["top"]], max(64, margin))
  }
})

test_that("summary ranks distributions without replacing fold scores", {
  data <- data.frame(
    variable = rep(c("a", "b", "zero"), each = 3),
    fold = rep(letters[1:3], 3),
    score = c(0, 0, 9, 2, 2, 2, 0, 0, 0)
  )
  for (summary in c("mean", "median")) {
    opt <- draw_varimp(
      data,
      type = "boxplot",
      top_n = 1,
      summary = summary,
      whisker = 0,
      point_spread = 0
    )[["x"]][["option"]]
    expect_equal(
      opt[["yAxis"]][["data"]],
      list(if (summary == "mean") "a" else "b")
    )
    expect_equal(
      unlist(opt[["series"]][[1]][["data"]][[1]]),
      if (summary == "mean") c(0, 0, 0, 4.5, 9) else rep(2, 5)
    )
    expect_length(opt[["series"]][[2]][["data"]], 3)
  }
  all <- draw_varimp(data, type = "boxplot", top_n = NULL)[["x"]][["option"]]
  expect_equal(all[["yAxis"]][["data"]], c("zero", "b", "a"))
  none <- draw_varimp(data, type = "boxplot", boxpoints = "none")[["x"]][[
    "option"
  ]]
  expect_length(none[["series"]], 1)
  outliers <- data.frame(
    variable = "x",
    fold = as.character(1:5),
    score = c(0, 0, 0, 0, 10)
  )
  opt <- draw_varimp(outliers, type = "boxplot", boxpoints = "outliers")[[
    "x"
  ]][["option"]]
  expect_length(opt[["series"]][[2]][["data"]], 1)
  expect_equal(opt[["series"]][[2]][["data"]][[1]][["value"]][[2]], 10)
})

test_that("portable distribution data and config round trips reproduce the view", {
  data <- varimp_data(data.frame(
    variable = c(".fold", ".fold", "other"),
    fold = c("first", "second", "first"),
    score = c(-2.125, NA, 1)
  ))
  folds <- c("first", "second", "empty")
  selected <- summarize_varimp(data, folds = folds)
  selected <- selected[rev(seq_len(nrow(selected))), ]
  records <- varimp_distribution(data, selected, "score", "missing", folds)
  expect_identical(records[["observation"]], ".fold_")
  config <- setup_BoxplotConfig(
    x = records[["columns"]],
    observation = records[["observation"]],
    horizontal = TRUE,
    boxpoints = "all",
    labels = paste0(
      selected[["variable"]],
      " (",
      selected[["n_available"]],
      "/3 folds)"
    ),
    xlab = "Score (available folds)",
    ylab = "Variable"
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  for (complete in c(FALSE, TRUE)) {
    write_chart_config(config, path, complete = complete)
    restored <- read_chart_config(path)
    rows <- jsonlite::fromJSON(jsonlite::toJSON(
      records[["data"]],
      dataframe = "rows",
      na = "null",
      digits = I(17)
    ))
    expected <- suppressMessages(draw_varimp(
      data,
      type = "boxplot",
      folds = folds
    ))
    actual <- suppressMessages(draw(restored, data = rows))
    expect_equal(actual[["x"]], expected[["x"]])
    expect_identical(
      jsonlite::toJSON(actual[["x"]], auto_unbox = TRUE, digits = I(17)),
      jsonlite::toJSON(expected[["x"]], auto_unbox = TRUE, digits = I(17))
    )
  }
})

test_that("importance distribution SVG retains known zeros and all available scores", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  for (horizontal in c(TRUE, FALSE)) {
    suppressMessages(draw_varimp(
      distribution_fixture(),
      type = "boxplot",
      absent = "zero",
      folds = LETTERS[1:5],
      horizontal = horizontal,
      palette = "#123456",
      title = "Importance distribution",
      filename = path
    ))
    svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(svg, "Importance distribution")
    expect_match(svg, "a (2/5 folds)", fixed = TRUE)
    expect_match(svg, "7 missing")
    expect_match(svg, "#123456", fixed = TRUE)
    expect_false(grepl("<image", svg, fixed = TRUE))
    expect_equal(
      lengths(regmatches(svg, gregexpr('<circle ', svg, fixed = TRUE))),
      8
    )
  }
})
