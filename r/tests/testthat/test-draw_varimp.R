# test-draw_varimp.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

test_that("importance input preserves names, measures, and observed fold records", {
  expected <- data.frame(variable = c("b", "a"), importance = c(3, -2))
  expect_identical(varimp_data(c(b = 3L, a = -2L)), expected)
  expect_identical(
    varimp_data(matrix(c(3, -2), ncol = 1, dimnames = list(c("b", "a"), "x"))),
    expected
  )
  expect_identical(varimp_data(c(3, -2))[["variable"]], c("1", "2"))
  data <- data.frame(
    variable = factor(c("b", "a", "b")),
    fold = c(2, 2, 1),
    gain = c(1, NA, 3),
    other = c(-2, 2, 4)
  )
  normalized <- varimp_data(data)
  expect_identical(names(normalized), names(data))
  expect_identical(normalized[["fold"]], c("2", "2", "1"))
  expect_identical(normalized[["variable"]], c("b", "a", "b"))
  expect_equal(normalized[["gain"]], c(1, NA, 3))
  expect_identical(varimp_measure(normalized), "gain")
  expect_identical(varimp_measure(normalized, "other"), "other")
  expect_error(varimp_measure(normalized, "fold"), class = "rtemis_input_error")
  expect_error(varimp_measure(normalized, c("gain", "other")))
  data[["gain"]] <- NA
  expect_type(varimp_data(data)[["gain"]], "double")
})

test_that("importance rejects malformed records and nonfinite scores", {
  for (x in list(
    NULL,
    numeric(),
    TRUE,
    "2",
    1 + 2i,
    matrix(1:4, 2),
    array(1:8, c(2, 2, 2))
  )) {
    expect_error(varimp_data(x), class = "rtemis_input_error")
  }
  bad_tables <- list(
    data.frame(variable = "a"),
    data.frame(name = "a", score = 1),
    data.frame(variable = c("a", "a"), score = 1:2),
    data.frame(variable = c("a", "a"), fold = "A", score = 1:2),
    data.frame(variable = NA_character_, score = 1),
    data.frame(variable = " ", score = 1),
    data.frame(variable = 1, score = 1),
    data.frame(variable = "a", fold = NA_character_, score = 1),
    data.frame(variable = "a", fold = "", score = 1),
    data.frame(variable = "a", score = "1"),
    data.frame(variable = "a", score = Inf),
    data.frame(variable = "a", score = 1 + 2i),
    data.frame(variable = "a", score = I(matrix(1:2, 1)))
  )
  duplicate <- data.frame(variable = "a", score = 1, other = 2)
  names(duplicate)[[3L]] <- "score"
  bad_tables <- c(bad_tables, list(duplicate))
  for (data in bad_tables) {
    expect_error(varimp_data(data), class = "rtemis_input_error")
  }
  expect_error(varimp_data(c(a = 1, a = 2)), "one row")
  expect_error(draw_varimp(c(a = NA_real_)), "no available scores")
})

test_that("ranking selects absolute summaries or signed summaries with stable ties", {
  data <- varimp_data(c(negative = -8, positive = 3, tied = -3, zero = 0))
  expect_identical(
    summarize_varimp(data, top_n = 2)[["variable"]],
    c("negative", "positive")
  )
  expect_identical(
    summarize_varimp(data, rank_by = "signed", top_n = 2)[["variable"]],
    c("positive", "zero")
  )
  expect_equal(nrow(summarize_varimp(data, top_n = 1)), 1)
  expect_equal(nrow(summarize_varimp(data, top_n = 100)), 4)
  expect_equal(nrow(summarize_varimp(data, top_n = NULL)), 4)
  expect_equal(nrow(summarize_varimp(varimp_data(seq_len(30)))), 20)
  for (n in list(0, -1, 0.5, 1.9, NA, Inf, "2", c(1, 2))) {
    expect_error(summarize_varimp(data, top_n = n))
  }
  for (arg in c("rank_by", "summary", "absent")) {
    expect_error(do.call(
      summarize_varimp,
      c(list(data), stats::setNames(list("invalid"), arg))
    ))
    expect_error(do.call(
      summarize_varimp,
      c(list(data), stats::setNames(list(c("a", "b")), arg))
    ))
  }
  zeros <- summarize_varimp(varimp_data(c(a = 0, b = 0)))
  expect_identical(zeros[["variable"]], c("a", "b"))
  expect_equal(zeros[["importance"]], c(0, 0))
})

test_that("known zero omissions differ from explicit unknown scores and missing folds", {
  data <- varimp_data(data.frame(
    variable = c("a", "b", "a", "b", "c", "c", "a"),
    fold = c("A", "A", "B", "B", "B", "C", "D"),
    gain = c(8, 2, NA, 4, -6, -3, NA)
  ))
  # Fold D reports no gain; E has no table. Both are unavailable. In fold C,
  # absent a/b are known zeros only when the sparse convention is declared.
  folds <- LETTERS[1:5]
  available <- summarize_varimp(data, folds = folds)
  expect_identical(available[["variable"]], c("a", "c", "b"))
  expect_equal(available[["importance"]], c(8, -4.5, 3))
  expect_equal(available[["n_available"]], c(1, 2, 2))
  sparse <- summarize_varimp(data, folds = folds, absent = "zero")
  expect_identical(sparse[["variable"]], c("a", "c", "b"))
  expect_equal(sparse[["importance"]], c(4, -3, 2))
  expect_equal(sparse[["n_available"]], c(2, 3, 3))
  expect_equal(sparse[["n_zero"]], c(1, 1, 1))
  expect_equal(sparse[["n_folds"]], rep(5, 3))
  expect_identical(nrow(data), 7L)
  for (ids in list(character(), c("A", "A"), c("A", NA), "A", 1:5)) {
    expect_error(summarize_varimp(data, folds = ids), "unique nonempty names")
  }
  expect_error(
    summarize_varimp(varimp_data(c(a = 1)), folds = "A"),
    "fold.*column"
  )
})

test_that("median with implicit zeros agrees with independently completed samples", {
  cases <- list(
    c(-8, -2),
    c(2, 8),
    c(-2, 8),
    c(-8, -2, 4),
    c(-8, 0, 4),
    c(0, 0)
  )
  for (values in cases) {
    for (n in seq.int(length(values), length(values) + 3L)) {
      data <- varimp_data(data.frame(
        variable = c(rep("a", length(values)), rep("anchor", n)),
        fold = c(as.character(seq_along(values)), as.character(seq_len(n))),
        importance = c(values, rep(1, n))
      ))
      out <- summarize_varimp(data, absent = "zero", summary = "median")
      expect_equal(
        out[["importance"]][out[["variable"]] == "a"],
        stats::median(c(values, rep(0, n - length(values))))
      )
    }
  }
  data <- varimp_data(data.frame(
    variable = c("a", "a", "a", "b", "b", "b"),
    fold = rep(1:3, 2),
    score = c(-10, 10, 0, 2, 2, 2)
  ))
  # Rank absolute summary, not mean absolute fold score.
  expect_identical(summarize_varimp(data, top_n = 1)[["variable"]], "b")
})

test_that("bars preserve signed values, physical axes, coverage labels, and overrides", {
  data <- c(a = -8, b = 3, c = 1)
  option <- draw_varimp(data, top_n = 2)[["x"]][["option"]]
  expect_identical(option[["yAxis"]][["data"]], c("b", "a"))
  expect_equal(unlist(option[["series"]][[1L]][["data"]]), c(3, -8))
  expect_identical(option[["xAxis"]][["name"]], "Importance")
  vertical <- draw_varimp(
    data,
    horizontal = FALSE,
    xlab = "Predictor",
    ylab = "Score",
    title = "VI",
    width = 700,
    height = 500,
    palette = "#123456"
  )
  opt <- vertical[["x"]][["option"]]
  expect_identical(opt[["xAxis"]][["data"]], c("a", "b", "c"))
  expect_identical(opt[["xAxis"]][["name"]], "Predictor")
  expect_identical(opt[["yAxis"]][["name"]], "Score")
  expect_identical(opt[["xAxis"]][["nameLocation"]], "middle")
  expect_identical(opt[["yAxis"]][["nameLocation"]], "middle")
  expect_identical(opt[["title"]][["text"]], "VI")
  expect_equal(vertical[["width"]], 700)
  expect_equal(vertical[["height"]], 500)
  expect_identical(opt[["series"]][[1L]][["color"]], "#123456")
  sparse <- data.frame(
    variable = c("a", "b", "a"),
    fold = c("A", "A", "B"),
    score = c(2, 1, 4)
  )
  opt <- draw_varimp(sparse)[["x"]][["option"]]
  expect_identical(
    opt[["yAxis"]][["data"]],
    c("b (1/2 folds)", "a (2/2 folds)")
  )
  expect_identical(opt[["xAxis"]][["name"]], "Mean Score (available folds)")
  expect_error(draw_varimp(data, horizontal = NA))
  expect_error(draw_varimp(data, margin_top = -1L))
})

test_that("materialized importance bars survive config and data JSON round trips", {
  values <- data.frame(label = "one", importance = -3.125)
  config <- setup_BarConfig(
    x = "label",
    y = "importance",
    horizontal = TRUE,
    xlab = "Importance",
    ylab = "Variable"
  )
  path <- tempfile(fileext = ".json")
  write_chart_config(config, path)
  restored <- read_chart_config(path)
  rows <- jsonlite::fromJSON(jsonlite::toJSON(
    values,
    dataframe = "rows",
    digits = I(17)
  ))
  expect_identical(
    draw(config, data = values)[["x"]],
    draw(restored, data = rows)[["x"]]
  )
  expect_identical(
    draw_varimp(c(one = -3.125))[["x"]],
    draw(restored, data = rows)[["x"]]
  )
  option <- draw(restored, data = rows)[["x"]][["option"]]
  expect_null(option[["title"]])
  parsed <- jsonlite::fromJSON(
    jsonlite::toJSON(option, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_length(parsed[["series"]][[1L]][["data"]], 1L)
  expect_true(is.list(parsed[["yAxis"]][["data"]]))
})
