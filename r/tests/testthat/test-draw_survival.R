test_that("survival records sort by time without inventing origins or changing probabilities", {
  d <- data.frame(time = c(5, 2, 8), survival = c(.7, 1, .2), group = "A")
  cfg <- setup_SurvivalConfig(group = "group")
  g <- survival_data(cfg, d)[["groups"]][[1L]]
  expect_equal(g[["time"]], c(2, 5, 8))
  expect_equal(g[["survival"]], c(1, .7, .2))
  expect_identical(draw_survival(d)[["x"]], draw(cfg, data = d)[["x"]])
  for (bad in list(
    data.frame(),
    transform(d, survival = c(1, .7, .2)),
    transform(d, time = c(1, 1, 2)),
    transform(d, time = c(NA, 1, 2)),
    transform(d, survival = c(NA, 1, .2)),
    transform(d, group = NA_character_),
    transform(d, survival = c(1.1, 1, .2)),
    transform(d, survival = as.character(survival))
  )) {
    expect_error(survival_data(cfg, bad))
  }
  expect_no_error(draw_survival(data.frame(time = 2, survival = .7)))
  s <- draw_survival(data.frame(time = 2, survival = .7))[["x"]][["option"]][[
    "series"
  ]][[1L]]
  expect_equal(s[["itemStyle"]][["opacity"]], 1)
})

test_that("median survival matches crossings and midpoints without extrapolation", {
  expect_equal(
    survival_median(data.frame(time = c(0, 1, 3), survival = c(1, .7, .4))),
    3
  )
  expect_equal(
    survival_median(data.frame(
      time = c(0, 1, 2, 4),
      survival = c(1, .5, .5, .2)
    )),
    2.5
  )
  expect_equal(
    survival_median(data.frame(
      time = c(0, 1, 2, 5),
      survival = c(1, .5, .5, .5)
    )),
    3
  )
  expect_true(is.na(survival_median(data.frame(
    time = c(0, 1, 3),
    survival = c(1, .7, .6)
  ))))
})

test_that("confidence, censor and annotation layers preserve their records", {
  d <- data.frame(
    time = c(0, 1, 3),
    survival = c(1, .75, .25),
    lower = c(1, .5, NA),
    upper = c(1, .9, NA),
    n_censor = c(0, 2.5, 0),
    n_risk = c(4, 4, 1)
  )
  widget <- draw_survival(d, show_median = TRUE, landmarks = c(-1, 1, 2, 9))
  s <- widget[["x"]][["option"]][["series"]]
  expect_length(s, 6L)
  expect_identical(s[[1L]][["step"]], "end")
  expect_identical(s[[2L]][["step"]], "end")
  expect_equal(s[[3L]][["data"]][[2L]], list(1, .4))
  expect_equal(s[[3L]][["data"]][[3L]], list(3, .4))
  expect_null(s[[3L]][["data"]][[4L]][[2L]])
  expect_equal(s[[4L]][["data"]][[1L]][[7L]], "2.5")
  expect_equal(s[[5L]][["data"]], list(c(0, .5), c(3, .5), c(3, 0)))
  expect_equal(
    lapply(s[[6L]][["data"]], `[[`, "value"),
    list(c(1, .75), c(2, .75))
  )
  expect_length(
    draw_survival(d, show_ci = FALSE, show_censors = FALSE)[["x"]][["option"]][[
      "series"
    ]],
    1L
  )
  expect_error(draw_survival(transform(d, lower = c(1, .8, .2))), "enclose")
  expect_error(
    draw_survival(transform(d, n_censor = c(0, -1, 0))),
    "nonnegative"
  )
  expect_false(grepl("function\\s*\\(", widget_payload_json(widget)))
})

test_that("risk records are explicit and share the curve groups", {
  d <- data.frame(time = c(0, 1, 3), survival = c(1, .75, .25))
  risk <- data.frame(
    time = c(0, 2, 3),
    group = "Survival",
    n_risk = c(4, NA, 1.5)
  )
  w <- draw_survival(list(curves = d, risk = risk), risk_table = TRUE)
  s <- w[["x"]][["option"]][["series"]]
  expect_length(s, 4L)
  expect_identical(s[[3L]][["data"]][[2L]][["name"]], "NA")
  expect_identical(s[[3L]][["data"]][[3L]][["name"]], "1.5")
  expect_identical(s[[2L]][["name"]], s[[1L]][["name"]])
  expect_error(draw_survival(d, risk_table = TRUE), "explicit")
  for (bad in list(
    transform(risk, time = c(0, 2, Inf)),
    transform(risk, n_risk = -1),
    transform(risk, group = "other"),
    rbind(risk, risk[1, ]),
    transform(risk, time = c(0, 2, 9))
  )) {
    expect_error(draw_survival(list(curves = d, risk = bad), risk_table = TRUE))
  }
})

test_that("survival SVG retains correct step, band, censor, landmark and risk geometry", {
  skip_if_not(nzchar(Sys.which("node")), "node not installed")
  d <- data.frame(
    time = c(0, 2, 5),
    survival = c(1, .6, .2),
    lower = c(1, .4, .1),
    upper = c(1, .8, .5),
    n_censor = c(0, 2, 1),
    group = "A"
  )
  risk <- data.frame(time = c(0, 1, 5), group = "A", n_risk = c(10, NA, 1))
  charts <- lapply(list(theme_light(), theme_dark()), function(theme) {
    draw_survival(
      list(curves = d, risk = risk),
      show_median = TRUE,
      landmarks = c(1, 4),
      risk_table = TRUE,
      theme = theme
    )[["x"]]
  })
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      charts = charts,
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
  result <- suppressWarnings(system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "survival_geometry.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "Survival vector geometry")
  # Check the public exporter too, including a one-time-point curve and missing
  # confidence bounds, whose geometry must remain finite and vector-based.
  for (data in list(
    d,
    d[1L, ],
    transform(d, lower = c(1, .4, NA), upper = c(1, .8, NA))
  )) {
    svg_path <- tempfile(fileext = ".svg")
    save_drawing(draw_survival(data), svg_path, width = 390, height = 600)
    svg <- paste(readLines(svg_path, warn = FALSE), collapse = "\n")
    expect_match(svg, "<path", fixed = TRUE)
    expect_false(grepl("<image|NaN|Infinity", svg))
    unlink(svg_path)
  }
})
