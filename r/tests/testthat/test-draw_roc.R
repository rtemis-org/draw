roc_test_labels <- function() {
  factor(c("no", "yes", "no", "yes"), levels = c("no", "yes"))
}
roc_test_records <- function() roc_input(roc_test_labels(), c(.1, .8, .5, .5))
roc_test_config <- function(...) {
  setup_ROCConfig(
    auc = "auc",
    class_label = "class",
    split = "split",
    fold = "fold",
    omitted = "omitted",
    ...
  )
}

test_that("empirical ROC uses fixed direction, ties, and independent AUC values", {
  y <- roc_test_labels()
  score <- c(.1, .8, .5, .5)
  out <- roc_input(y, score)
  expect_equal(out[["fpr"]], c(0, 0, .5, 1))
  expect_equal(out[["tpr"]], c(0, .5, 1, 1))
  expect_equal(unique(out[["auc"]]), .875)
  # Independent pairwise definition gives half credit for equal scores.
  pairwise <- outer(score[y == "yes"], score[y == "no"], "-")
  expect_equal(
    unique(out[["auc"]]),
    mean((pairwise > 0) + .5 * (pairwise == 0))
  )
  expect_equal(unique(roc_input(y, 1 - score)[["auc"]]), .125)
  expect_equal(unique(roc_input(y, rep(.5, 4))[["auc"]]), .5)
  expect_equal(nrow(roc_input(y, rep(.5, 4))), 2)
  expect_identical(
    unique(roc_input(y, score, positive = "no")[["class"]]),
    "no"
  )
  named <- matrix(score, ncol = 1, dimnames = list(NULL, "yes"))
  expect_equal(unique(roc_input(y, named, positive = "no")[["auc"]]), .875)
  missing <- roc_input(
    factor(c("no", "yes", "yes", NA), levels = levels(y)),
    c(.1, .8, NA, .9)
  )
  expect_equal(unique(missing[["omitted"]]), 2)
  expect_equal(unique(missing[["auc"]]), 1)
  undefined <- roc_input(factor(rep("yes", 4), levels = levels(y)), score)
  expect_true(all(is.na(undefined[["auc"]])))
  expect_error(draw_roc(undefined), "No defined ROC")
})

test_that("ROC normalizes class columns and pairs input sets by name", {
  y <- factor(c("A", "B", "C", "A", "B", "C"), levels = c("B", "A", "C"))
  p <- rbind(
    c(.2, .7, .1),
    c(.8, .1, .1),
    c(.2, .1, .7),
    c(.1, .6, .3),
    c(.7, .2, .1),
    c(.2, .3, .5)
  )
  colnames(p) <- levels(y)
  a <- roc_input(y, p)
  expect_identical(a, roc_input(y, p[, c(3, 1, 2)]))
  expect_identical(unique(a[["class"]]), levels(y))
  expect_equal(unique(a[["auc"]]), 1)
  binary <- roc_test_labels()
  d <- roc_input(
    list(Train = binary, Test = binary),
    list(Test = c(.9, .2, .5, .5), Train = c(.1, .8, .5, .5))
  )
  expect_equal(unique(d[["auc"]][d[["split"]] == "Train"]), .875)
  expect_equal(unique(d[["auc"]][d[["split"]] == "Test"]), .125)
  for (args in list(
    list(binary, c(.1, .2)),
    list(binary, c(1, 2, 3, 4)),
    list(binary, c(0, Inf, .5, 1)),
    list(binary, letters[1:4]),
    list(y, p[, 1]),
    list(y, p, positive = "B"),
    list(binary, matrix(.5, 4, 3)),
    list(binary, array(.5, c(4, 1, 1))),
    list(list(a = binary), list(b = rep(.5, 4))),
    list(list(binary), rep(.5, 4)),
    list(binary, rep(.5, 4), positive = "bad")
  )) {
    expect_error(do.call(roc_input, args))
  }
  bad <- p
  colnames(bad) <- c("A", "A", "C")
  expect_error(roc_input(y, bad), "column names")
  expect_error(
    roc_input(data.frame(fpr = 0, tpr = 0), positive = "yes"),
    "Omit"
  )
  expect_error(roc_input(binary), "Supply probabilities")
  expect_error(
    roc_input(list(a = binary, a = binary), list(rep(.5, 4), rep(.5, 4))),
    "unique"
  )
})

test_that("ROC table validation preserves vertical vertices and supplied full-resolution AUC", {
  d <- roc_test_records()
  out <- roc_data(roc_test_config(), d[nrow(d):1, ])
  expect_equal(out[["curves"]][[1]][["fpr"]], d[["fpr"]])
  expect_equal(out[["curves"]][[1]][["tpr"]], d[["tpr"]])
  coarse <- data.frame(fpr = c(0, 1), tpr = c(0, 1), auc = c(.875, .875))
  expect_equal(
    roc_data(setup_ROCConfig(auc = "auc"), coarse)[["curves"]][[1]][["auc"]],
    .875
  )
  expect_equal(
    roc_data(setup_ROCConfig(), coarse)[["curves"]][[1]][["auc"]],
    .5
  )
  for (edit in list(
    function(x) {
      x[["auc"]][1] <- .2
      x
    },
    function(x) {
      x[["tpr"]][2] <- NA
      x
    },
    function(x) {
      x[["fpr"]][1] <- .1
      x
    },
    function(x) {
      x[["tpr"]] <- c(0, .8, .5, 1)
      x
    },
    function(x) {
      x[["fpr"]][2] <- Inf
      x
    },
    function(x) {
      x[["class"]][1] <- ""
      x
    },
    function(x) {
      x[["omitted"]][1] <- .5
      x
    }
  )) {
    expect_error(roc_data(roc_test_config(), edit(d)))
  }
  expect_error(
    roc_data(roc_test_config(variant = "per_resample"), d),
    "No curves match"
  )
})

test_that("fold AUC summaries count each fold once and keep undefined uncertainty", {
  d <- roc_test_records()
  d[["fold"]] <- "one"
  second <- d[c(1, 3, 4), ]
  second[["fold"]] <- "two"
  second[["auc"]] <- .5
  unavailable <- d[1, ]
  unavailable[["fold"]] <- "three"
  unavailable[c("fpr", "tpr", "auc")] <- NA_real_
  records <- rbind(d, second, unavailable)
  out <- roc_data(roc_test_config(variant = "per_resample"), records)
  g <- out[["groups"]][[1]]
  expect_equal(g[["mean"]], (.875 + .5) / 2)
  expect_equal(g[["sd"]], sqrt((.875 - .5)^2 / 2))
  expect_equal(g[["available"]], 2)
  expect_equal(g[["total"]], 3)
  expect_equal(out[["undefined"]], 1)
  one <- draw_roc(d, variant = "per_resample")[["x"]][["option"]]
  expect_match(one[["legend"]][["data"]][[1]], "SD NA")
  expect_message(
    widget <- draw_roc(records, variant = "per_resample", palette = "#123456"),
    "1 undefined curve"
  )
  option <- widget[["x"]][["option"]]
  expect_length(option[["legend"]][["data"]], 1)
  expect_null(option[["series"]][[1]][["name"]])
  expect_identical(
    option[["series"]][[2]][["name"]],
    option[["series"]][[3]][["name"]]
  )
  expect_identical(option[["series"]][[3]][["lineStyle"]][["color"]], "#123456")
  expect_null(option[["title"]][["subtext"]])
  expect_identical(
    widget[["x"]],
    draw(
      roc_test_config(variant = "per_resample", palette = "#123456"),
      data = records
    )[["x"]]
  )
  expect_false(grepl(
    "function\\s*\\(",
    jsonlite::toJSON(option, auto_unbox = TRUE)
  ))
  wire <- jsonlite::fromJSON(
    htmlwidgets:::toJSON(widget[["x"]]),
    simplifyVector = FALSE
  )
  expect_type(wire[["option"]][["series"]][[2]][["data"]][[1]][[1]], "integer")
})

test_that("ROC SVG contains curves, chance line, full labels, and a square plotting grid", {
  skip_if(!nzchar(Sys.which("node")))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  widget <- draw_roc(roc_test_records(), palette = "#123456", filename = path)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(text, "yes \\(AUC 0.875\\)")
  expect_match(text, "AUC 0.875")
  expect_match(text, 'stroke="#123456"')
  expect_match(text, 'stroke="#888888"')
  expect_false(grepl("<image", text, fixed = TRUE))
  grid <- static_aspect(
    widget[["x"]][["option"]],
    widget[["x"]][["aspect"]],
    800,
    500
  )[["grid"]]
  expect_equal(grid[["width"]], grid[["height"]])
  expect_equal(grid[["height"]], 500 - 25 - 65)
})

test_that("classification labels and square ROC axes fit phone and SVG layouts", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  folds <- roc_test_records()
  folds[["fold"]] <- "Fold 1"
  undefined <- folds[1L, ]
  undefined[["fold"]] <- "Fold 2"
  undefined[c("fpr", "tpr", "auc")] <- NA_real_
  folds <- rbind(folds, undefined)
  folds[["omitted"]] <- 1
  widgets <- list(
    draw_roc(
      list(Training = roc_test_labels(), Test = roc_test_labels()),
      list(Training = c(.1, .8, .5, .5), Test = c(.5, .5, .5, .5))
    ),
    draw_roc(folds, variant = "per_resample"),
    draw_roc(roc_test_records(), legend = FALSE),
    draw_confusion(matrix(
      c(8, 2, 1, 9),
      2,
      dimnames = list(c("yes", "no"), c("yes", "no"))
    ))
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = lapply(widgets, `[[`, "x"),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      confusion = system.file(
        "htmlwidgets/lib/draw/confusion.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "classification_geometry.js")),
      shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})

test_that("empirical ROC agrees with the existing rtemis statistical engine", {
  skip_if_not_installed("rtemis")
  skip_if_not_installed("pROC")
  y <- roc_test_labels()
  p <- c(.1, .8, .5, .5)
  old <- rtemis::roc_curve(y, p)
  new <- roc_input(y, p)
  expect_equal(new[c("class", "fpr", "tpr", "auc")], old)
})

test_that("ROC legends use compact labels and portable corner placement", {
  records <- roc_test_records()
  default <- draw_roc(records)
  expect_identical(default[["x"]][["legendPosition"]], "bottom-right")
  expect_identical(
    default[["x"]][["option"]][["legend"]][["data"]],
    list("yes (AUC 0.875)")
  )
  records[["split"]] <- "Validation"
  named <- draw_roc(records, legend_position = "top-left")
  expect_identical(named[["x"]][["legendPosition"]], "top-left")
  expect_identical(
    named[["x"]][["option"]][["legend"]][["data"]],
    list("Validation: yes (AUC 0.875)")
  )
  expect_equal(named[["height"]], default[["height"]])
  expect_identical(
    draw_roc(records, square = FALSE, legend_position = "bottom-left")[["x"]][[
      "legendPosition"
    ]],
    "bottom-left"
  )
})

test_that("ROC legend anchors and complete labels survive resize and selection", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  multiclass <- do.call(
    rbind,
    lapply(c("setosa", "versicolor", "virginica"), function(cl) {
      records <- roc_test_records()
      records[["class"]] <- cl
      records
    })
  )
  folds <- rbind(roc_test_records(), roc_test_records())
  folds[["fold"]] <- rep(c("one", "two"), each = nrow(folds) / 2)
  folds[["split"]] <- "Validation sample"
  corners <- c("bottom-right", "top-right", "top-left", "bottom-left")
  widgets <- unlist(
    lapply(corners, function(corner) {
      list(
        draw_roc(multiclass, legend_position = corner),
        draw_roc(
          folds,
          variant = "per_resample",
          legend_position = corner,
          theme = theme_dark(),
          square = FALSE
        )
      )
    }),
    recursive = FALSE
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = lapply(widgets, `[[`, "x"),
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      layout = system.file(
        "htmlwidgets/lib/draw/panels.js",
        package = "rtemis.draw"
      )
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "roc_legend.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})

test_that("nonfactor binary labels use factor order, independent of row order", {
  d <- roc_input(c(1, 0, 1, 0), c(.8, .2, .9, .1))
  expect_identical(unique(d[["class"]]), "1")
  expect_equal(unique(d[["auc"]]), 1)
})


test_that("ROC hover labels round without changing numeric vertices or AUC", {
  records <- data.frame(
    fpr = c(0, 1 / 3, 1),
    tpr = c(0, 2 / 3, 1),
    auc = 2 / 3,
    class = "yes",
    split = "Test",
    fold = "Fold 1",
    omitted = 0L
  )
  for (digits in c(0L, 2L, 3L)) {
    widget <- draw_roc(records, variant = "per_resample", digits = digits)
    option <- widget[["x"]][["option"]]
    expect_identical(option[["tooltip"]][["trigger"]], "item")
    expect_identical(option[["legend"]][["itemStyle"]][["opacity"]], 1)
    curve <- option[["series"]][[2L]]
    expect_true(curve[["showSymbol"]])
    expect_identical(curve[["itemStyle"]][["opacity"]], 0)
    expect_identical(curve[["emphasis"]][["itemStyle"]][["opacity"]], 1)
    value <- curve[["data"]][[2L]]
    expect_equal(unlist(value[1:3]), c(1 / 3, 2 / 3, 2 / 3))
    expect_identical(
      unlist(value[6:8]),
      formatC(
        c(1 / 3, 2 / 3, 2 / 3),
        format = "f",
        digits = digits,
        decimal.mark = "."
      )
    )
    expect_identical(
      curve[["encode"]][["tooltip"]],
      as.list(c(5L, 6L, 7L, 3L))
    )
    expect_identical(
      curve[["dimensions"]][[8L]],
      list(
        name = "AUC label",
        displayName = "AUC",
        type = "ordinal"
      )
    )
    # Numeric geometry and ordinal display text must both survive the wire.
    wire <- jsonlite::fromJSON(
      htmlwidgets:::toJSON(widget[["x"]]),
      simplifyVector = FALSE
    )
    expect_identical(wire[["option"]][["series"]][[2L]][["data"]][[2L]], value)
  }
})


test_that("static ROC export omits invisible hover symbols and retains its color key", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  save_drawing(draw_roc(roc_test_records(), palette = "#123456"), path)
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl('fill-opacity="0"[^>]*ecmeta_ssr_type="chart"', svg))
  expect_match(svg, 'fill="#123456"', fixed = TRUE)
  expect_match(svg, 'stroke="#123456"', fixed = TRUE)
  expect_match(svg, "AUC 0.875", fixed = TRUE)
})
