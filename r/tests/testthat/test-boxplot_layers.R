test_that("violin profiles match explicit Gaussian density and handle degenerate samples", {
  values <- c(-2, -1, 0, .5, 1, 4, 8)
  set.seed(123)
  state <- .Random.seed
  for (bandwidth in list(NULL, .7)) {
    profile <- boxplot_density(
      values,
      bandwidth,
      adjust = 1.4,
      density_points = 64L
    )
    expected <- stats::density(
      values,
      bw = (bandwidth %||% stats::bw.nrd0(values)) * 1.4,
      n = 64,
      from = min(values),
      to = max(values)
    )
    expect_equal(unlist(profile[["position"]]), expected[["x"]])
    expect_equal(
      unlist(profile[["width"]]),
      expected[["y"]] / max(expected[["y"]])
    )
    expect_equal(range(unlist(profile[["position"]])), range(values))
  }
  expect_identical(.Random.seed, state)
  expect_identical(boxplot_density(numeric()), list())
  expect_identical(boxplot_density(c(2, 2)), boxplot_density(2))
  expect_identical(
    boxplot_density(2),
    list(position = list(2), width = list(1))
  )
  expect_error(boxplot_density(values, 1e308, adjust = 10), "bandwidth")
})

test_that("violin settings validate and compile through portable config bindings", {
  data <- list(
    A = 1:4,
    B = c(2, 3, 4, 8),
    id = letters[1:4],
    tests = data.frame(from = "A", to = "B", label = "paired p = 0.1")
  )
  cfg <- setup_BoxplotConfig(
    x = c("A", "B"),
    observation = "id",
    paired = TRUE,
    comparisons = "tests",
    geometry = "both",
    density_points = 64L,
    bandwidth = .5
  )
  expected <- boxplot_option(
    data[c("A", "B")],
    observation = data[["id"]],
    paired = TRUE,
    comparisons = data[["tests"]],
    geometry = "both",
    density_points = 64L,
    bandwidth = .5
  )
  expect_identical(to_list(compile(cfg, data)), to_list(expected))
  for (complete in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    write_chart_config(cfg, path, complete = complete)
    expect_identical(
      to_list(compile(read_chart_config(path), data)),
      to_list(expected)
    )
  }
  schema <- chart_schema(
    BoxplotConfig,
    id = "https://example.org/box.json",
    title = "Boxplot",
    description = "Boxplot."
  )[["properties"]]
  expect_equal(
    as.character(schema[["geometry"]][["enum"]]),
    c("box", "violin", "both")
  )
  expect_equal(schema[["density_points"]][["maximum"]], 4096)
  expect_false("comparisons" %in% names(to_list(setup_BoxplotConfig())))
  for (args in list(
    list(geometry = "bad"),
    list(bandwidth = 0),
    list(adjust = Inf),
    list(density_points = 15L),
    list(density_points = 64.5),
    list(paired = NA),
    list(pair_alpha = 2),
    list(pair_width = 0),
    list(comparisons = "")
  )) {
    expect_error(do.call(setup_BoxplotConfig, args))
  }
  expect_error(draw_violin(1:5, show_box = NA))
  expect_identical(
    draw_violin(1:5)[["x"]],
    draw_boxplot(1:5, geometry = "violin")[["x"]]
  )
  expect_identical(
    draw_violin(1:5, show_box = TRUE)[["x"]],
    draw_boxplot(1:5, geometry = "both")[["x"]]
  )
})

test_that("paired lines match explicit IDs and preserve missing intermediate measurements", {
  expect_error(draw_boxplot(1:3, paired = TRUE), "explicit")
  expect_error(
    draw_boxplot(1:3, paired = TRUE, observation = c("a", "a", "b")),
    "unique"
  )
  # Shuffled long-format rows match by ID, not by position in each group.
  opt <- to_list(boxplot_option(
    c(1, 2, 12, 11),
    group = c("A", "A", "B", "B"),
    paired = TRUE,
    observation = c("one", "two", "two", "one"),
    boxpoints = "all"
  ))
  pairs <- opt[["series"]][[3L]][["data"]]
  expect_equal(vapply(pairs, function(x) x[[5L]], numeric(1)), c(11, 12))
  expect_identical(
    vapply(pairs, function(x) x[[7L]], character(1)),
    c("one", "two")
  )
  # Missing middle values cannot create a line from A directly to C.
  opt <- to_list(boxplot_option(
    list(A = c(1, 2), B = c(NA, 3), C = c(4, 5)),
    paired = TRUE,
    observation = c("one", "two"),
    verbosity = 0
  ))
  pairs <- opt[["series"]][[2L]][["data"]]
  expect_length(pairs, 2)
  expect_true(all(vapply(pairs, function(x) x[[7L]] == "two", logical(1))))
  expect_equal(
    vapply(pairs, function(x) x[[4L]] - x[[1L]], numeric(1)),
    c(1, 1)
  )
  expect_true(all(vapply(
    pairs,
    function(x) x[[3L]] == 0 && x[[6L]] == 0,
    logical(1)
  )))
  # Duplicate IDs across different groups are valid; groups stay separate.
  opt <- to_list(boxplot_option(
    list(A = c(1, 2), B = c(10, 20)),
    group = c("G1", "G2"),
    observation = c("same", "same"),
    paired = TRUE
  ))
  expect_equal(opt[["series"]][[3L]][["data"]][[1L]][[5L]], 10)
  expect_equal(opt[["series"]][[4L]][["data"]][[1L]][[5L]], 20)
})

test_that("comparisons resolve names without running tests or inventing empty endpoints", {
  values <- list(A = c(1, 2), B = c(2, 3))
  comparisons <- data.frame(
    from = "A",
    to = "B",
    label = "supplied label",
    position = 8
  )
  opt <- to_list(boxplot_option(values, comparisons = comparisons))
  record <- opt[["series"]][[2L]][["data"]][[1L]]
  expect_identical(record, list(0L, 1L, 8, 0L, 0L, "supplied label"))
  expect_gt(opt[["yAxis"]][["max"]], 8)
  for (bad in list(
    list(),
    data.frame(from = "A"),
    transform(comparisons, from = "Z"),
    transform(comparisons, to = "A"),
    transform(comparisons, label = NA_character_),
    transform(comparisons, position = Inf),
    transform(comparisons, position = "8"),
    transform(comparisons, from_group = "G")
  )) {
    expect_error(boxplot_option(values, comparisons = bad))
  }
  expect_error(
    boxplot_option(list(A = 1:2, B = numeric()), comparisons = comparisons),
    "available"
  )
  expect_error(
    boxplot_option(values, labels = c("A", "A"), comparisons = comparisons),
    "unique"
  )
  expect_error(
    boxplot_option(values, group = c("G1", "G2"), comparisons = comparisons),
    "from_group"
  )
  grouped <- transform(comparisons, from_group = "G1", to_group = "G2")
  opt <- to_list(boxplot_option(
    values,
    group = c("G1", "G2"),
    comparisons = grouped
  ))
  expect_identical(opt[["series"]][[3L]][["data"]][[1L]][[5L]], 1L)
})

test_that("native SVG distribution layers retain geometry through orientation, legend, and resize", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  charts <- list()
  for (horizontal in c(FALSE, TRUE)) {
    for (geometry in c("box", "violin", "both")) {
      for (theme in list(theme_light(), theme_dark())) {
        option <- boxplot_option(
          list(A = c(1, 2, 3, 4), B = c(2, 3, 4, 6)),
          group = c("G1", "G1", "G2", "G2"),
          observation = c("a", "b", "a", "b"),
          paired = TRUE,
          boxpoints = "all",
          geometry = geometry,
          horizontal = horizontal,
          comparisons = data.frame(
            from = "A",
            to = "B",
            from_group = "G1",
            to_group = "G2",
            label = "Comparison"
          )
        )
        charts[[length(charts) + 1L]] <- list(
          option = to_list(option),
          theme = to_list(theme),
          horizontal = horizontal,
          geometry = geometry
        )
      }
    }
    # One item, constant samples and an unavailable cell need distinct marks.
    for (values in list(
      list(A = 2),
      list(A = rep(2, 4)),
      list(A = 1:4, B = numeric())
    )) {
      charts[[length(charts) + 1L]] <- list(
        option = to_list(boxplot_option(
          values,
          geometry = "violin",
          horizontal = horizontal
        )),
        horizontal = horizontal,
        geometry = "violin"
      )
    }
  }
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
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
  output <- system2(
    "node",
    c(shQuote(test_path("fixtures", "boxplot_layers.js")), shQuote(input)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "passed")
})
