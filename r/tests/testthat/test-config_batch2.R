# test-config_batch2.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# line, pie, boxplot, sankey and gantt. Between them they cover the two binding
# shapes the first four charts did not: a chart with no axes at all (pie,
# sankey), and a **table-bound** chart, where the properties name columns within
# one bound table rather than one column per role (sankey, gantt).

links_df <- function() {
  data.frame(
    source = c("a", "b"),
    target = c("c", "c"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
}
tasks_df2 <- function() {
  data.frame(
    label = c("s1", "s2"),
    start = c(0, 5),
    end = c(5, 9),
    kind = c("a", "b"),
    stringsAsFactors = FALSE
  )
}


# %% every config in the batch ----

test_that("each config carries its own type constant", {
  expect_identical(LineConfig()@type, "line")
  expect_identical(PieConfig()@type, "pie")
  expect_identical(BoxplotConfig()@type, "boxplot")
  expect_identical(SankeyConfig()@type, "sankey")
  expect_identical(GanttConfig()@type, "gantt")
})

test_that("no setup function in the batch has a mandatory argument", {
  setups <- list(
    setup_LineConfig,
    setup_PieConfig,
    setup_BoxplotConfig,
    setup_SankeyConfig,
    setup_GanttConfig
  )
  for (fn in setups) {
    no_default <- vapply(
      formals(fn),
      function(f) identical(f, quote(expr = )),
      logical(1L)
    )
    expect_false(any(no_default))
  }
})

test_that("every registered type resolves to a class that agrees with it", {
  for (type in names(chart_registry())) {
    entry <- chart_registry()[[type]]
    expect_identical(chart_type_of(entry[["cls"]]), type)
  }
})


# %% compile matches the vector path ----

test_that("line compiles to the same option as the vector path", {
  d <- mtcars[order(mtcars[["wt"]]), ]
  expect_identical(
    compile(setup_LineConfig(x = "wt", y = "mpg"), data = d),
    line_option(x = d[["wt"]], y = d[["mpg"]], xlab = "wt", ylab = "mpg")
  )
  expect_identical(
    compile(setup_LineConfig(x = "wt", y = c("mpg", "hp")), data = d),
    line_option(
      x = d[["wt"]],
      y = list(mpg = d[["mpg"]], hp = d[["hp"]]),
      xlab = "wt"
    )
  )
})

test_that("pie compiles to the same option as the vector path", {
  counts <- data.frame(kind = c("a", "b", "c"), n = c(3, 5, 2))
  expect_identical(
    compile(setup_PieConfig(values = "n", labels = "kind"), data = counts),
    pie_option(values = counts[["n"]], labels = counts[["kind"]])
  )
})

test_that("boxplot compiles to the same option as the vector path", {
  expect_identical(
    compile(setup_BoxplotConfig(x = c("mpg", "hp")), data = mtcars),
    boxplot_option(x = list(mpg = mtcars[["mpg"]], hp = mtcars[["hp"]]))
  )
})

test_that("sankey and gantt compile to the same options as the vector path", {
  expect_identical(
    compile(setup_SankeyConfig(), data = links_df()),
    sankey_option(links = links_df())
  )
  expect_identical(
    compile(setup_GanttConfig(group = "kind"), data = tasks_df2()),
    gantt_option(tasks = tasks_df2(), group = "kind")
  )
})


# %% table-bound charts name their columns ----

test_that("a table-bound config binds columns that are not conventionally named", {
  # The point of a binding layer: a table calling its columns something else is
  # plotted without the caller reshaping it first.
  renamed <- data.frame(
    from = c("a", "b"),
    to = c("c", "c"),
    n = c(1, 2),
    stringsAsFactors = FALSE
  )
  expect_identical(
    compile(
      setup_SankeyConfig(source = "from", target = "to", value = "n"),
      data = renamed
    ),
    sankey_option(links = links_df())
  )
})

test_that("gantt binds renamed task columns", {
  renamed <- tasks_df2()
  names(renamed)[1:3] <- c("name", "t0", "t1")
  expect_identical(
    compile(
      setup_GanttConfig(
        label = "name",
        start = "t0",
        end = "t1",
        group = "kind"
      ),
      data = renamed
    ),
    gantt_option(tasks = tasks_df2(), group = "kind")
  )
})

test_that("a missing bound column is reported by name", {
  expect_error(
    compile(setup_SankeyConfig(source = "nope"), data = links_df()),
    "nope"
  )
})


# %% resolve ----

test_that("line derives labels and limits like scatter", {
  d <- mtcars[order(mtcars[["wt"]]), ]
  r <- resolve(setup_LineConfig(x = "wt", y = "mpg"), data = d)
  expect_identical(r@xlab, "wt")
  expect_identical(r@ylab, "mpg")
  expect_length(r@ylim, 2L)
  expect_identical(r@origin[["ylim"]], "derived")
})

test_that("line derives no value label for several columns", {
  d <- mtcars[order(mtcars[["wt"]]), ]
  r <- resolve(setup_LineConfig(x = "wt", y = c("mpg", "hp")), data = d)
  expect_null(r@ylab)
})

test_that("charts with nothing to derive resolve to themselves", {
  # Every type resolves, so `draw()` never has to know which have a derivation.
  expect_identical(
    resolve(
      setup_PieConfig(values = "n", labels = "k"),
      data = data.frame(n = 1, k = "a")
    ),
    setup_PieConfig(values = "n", labels = "k")
  )
  expect_identical(
    resolve(setup_SankeyConfig(), data = links_df()),
    setup_SankeyConfig()
  )
})


# %% round trip ----

test_that("every config in the batch round-trips and draws the same", {
  cases <- list(
    list(
      cfg = setup_LineConfig(x = "wt", y = c("mpg", "hp"), smooth = TRUE),
      data = mtcars[order(mtcars[["wt"]]), ]
    ),
    list(
      cfg = setup_PieConfig(
        values = "n",
        labels = "kind",
        rose_type = "radius"
      ),
      data = data.frame(kind = c("a", "b"), n = c(1, 2))
    ),
    list(
      cfg = setup_BoxplotConfig(x = c("mpg", "hp"), horizontal = TRUE),
      data = mtcars
    ),
    list(
      cfg = setup_SankeyConfig(orient = "vertical", node_width = 12),
      data = links_df()
    ),
    list(
      cfg = setup_GanttConfig(
        group = "kind",
        axis_type = "value",
        zoom = FALSE
      ),
      data = tasks_df2()
    )
  )
  for (case in cases) {
    path <- tempfile(fileext = ".json")
    write_chart_config(case[["cfg"]], path)
    back <- read_chart_config(path)
    expect_identical(back, case[["cfg"]])
    expect_identical(
      compile(back, data = case[["data"]]),
      compile(case[["cfg"]], data = case[["data"]])
    )
  }
})
