test_that("supplied fits preserve paired coordinates and interval bounds", {
  layer <- FitLayer(
    x = c(3, 1, 2),
    y = c(5, 2, 4),
    lower = c(4, 1, 3),
    upper = c(6, 3, 5),
    name = "Model"
  )
  series <- to_list(layer)
  expect_equal(series[[2]][["data"]], list(c(1, 2), c(2, 4), c(3, 5)))
  expect_equal(
    series[[1]][["itemPayload"]][["vertices"]],
    list(c(1, 3), c(2, 5), c(3, 6), c(3, 4), c(2, 3), c(1, 1))
  )
  expect_equal(strip_js(series), series)
  expect_error(FitLayer(x = c(1, 1), y = 1:2))
  expect_error(FitLayer(x = 1:2, y = 1:2, lower = 1:2))
  expect_error(FitLayer(x = 1:2, y = 1:2, lower = 2:3, upper = 1:2))
  expect_error(FitLayer(x = c(1, Inf), y = 1:2))
  w <- draw_add_fit(draw_scatter(1:3, 2:4), layer = layer)
  expect_length(w[["x"]][["option"]][["series"]], 3L)
  expect_error(draw_add_fit(draw_pie(c(A = 2, B = 3)), layer = layer))
  expect_error(draw_add_fit(
    draw_scatter(1:3, 2:4),
    layer = layer,
    name = "Other"
  ))
})

test_that("fits, rugs, references and phase labels retain vector geometry", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  charts <- list()
  for (theme in list(theme_light(), theme_dark())) {
    plot <- draw_scatter(1:5, c(2, 3, 2, 5, 4), rug = TRUE, theme = theme)
    plot <- draw_add_fit(
      plot,
      c(5, 1, 3),
      c(4, 2, 3),
      lower = c(3.5, 1.5, 2.5),
      upper = c(4.5, 2.5, 3.5)
    )
    plot <- draw_annotate(
      plot,
      hline = 3,
      vline = 2,
      x = 3,
      y = 4,
      text = "Peak {b}",
      bands = list(c(2, 3))
    )
    charts[[length(charts) + 1L]] <- list(
      option = strip_js(plot[["x"]][["option"]]),
      theme = to_list(theme),
      labels = list("Peak {b}")
    )
    plot <- draw_xt(
      c(0, 2, 5, 8),
      list(Activity = c(2, 4, 3, 1)),
      zt = c(0, 6, 12, 18),
      shade_bin = c(0, 1, 1, 0),
      theme = theme
    )
    charts[[length(charts) + 1L]] <- list(
      option = strip_js(plot[["x"]][["option"]]),
      theme = to_list(theme),
      labels = list("Activity", "18")
    )
  }
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input))
  jsonlite::write_json(
    list(
      charts = charts,
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      renderers = system.file(
        "htmlwidgets/lib/draw/renderers.js",
        package = "rtemis.draw"
      )
    ),
    input,
    auto_unbox = TRUE,
    digits = NA
  )
  result <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "overlay_geometry.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "8 overlay geometry cases passed"
  )
})
