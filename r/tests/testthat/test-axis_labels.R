# test-axis_labels.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Axis labels: `NULL` derives the label from the data, and `""` draws no axis
# name, on every chart whose config derives its labels.

label_data <- function() {
  data.frame(
    g = letters[1:20],
    u = seq_len(20),
    v = seq(2, 40, by = 2)
  )
}

# Each config whose labels derive from the data, set up with empty labels.
blank_configs <- function() {
  list(
    scatter = setup_ScatterConfig(x = "u", y = "v", xlab = "", ylab = ""),
    line = setup_LineConfig(x = "u", y = "v", xlab = "", ylab = ""),
    bar = setup_BarConfig(x = "g", y = "u", xlab = "", ylab = ""),
    histogram = setup_HistogramConfig(x = "u", xlab = "", ylab = ""),
    density = setup_DensityConfig(x = "u", xlab = "", ylab = "")
  )
}


# %% configs ----

test_that("an empty label is valid and resolve() keeps it", {
  d <- label_data()
  for (config in blank_configs()) {
    r <- resolve(config, data = d)
    expect_identical(r@xlab, "")
    expect_identical(r@ylab, "")
  }
  expect_identical(setup_BoxplotConfig(x = "u", xlab = "")@xlab, "")
})

test_that("an unset label still derives from the data", {
  d <- label_data()
  expect_identical(
    resolve(setup_ScatterConfig(x = "u", y = "v"), data = d)@xlab,
    "u"
  )
  expect_identical(
    resolve(setup_HistogramConfig(x = "u"), data = d)@xlab,
    "u"
  )
})

test_that("each schema accepts an empty label: no minLength", {
  classes <- list(
    ScatterConfig,
    LineConfig,
    BarConfig,
    HistogramConfig,
    DensityConfig,
    BoxplotConfig,
    TimeSeriesConfig
  )
  for (cls in classes) {
    schema <- chart_schema(
      cls,
      id = "https://schema.rtemis.org/chart/test/v1/schema.json",
      title = "Test",
      description = "Test."
    )
    for (k in c("xlab", "ylab")) {
      expect_null(schema[["properties"]][[k]][["minLength"]])
    }
  }
})


# %% drawn axes ----

test_that("an empty label draws no axis name", {
  d <- label_data()
  configs <- blank_configs()
  skip_if_no_node()
  ts <- built_option(
    compile(
      setup_TimeSeriesConfig(x = "u", y = "v", xlab = "", ylab = ""),
      data = d
    ),
    simplify = FALSE
  )
  expect_null(ts[["xAxis"]][["name"]])
  expect_null(ts[["yAxis"]][["name"]])
  for (type in c("scatter", "line", "bar", "histogram", "density")) {
    option <- built_option(compile(configs[[type]], data = d))
    expect_null(option[["xAxis"]][["name"]])
    expect_null(option[["yAxis"]][["name"]])
  }
  option <- built_option(draw_boxplot(
    list(a = d[["u"]], b = d[["v"]]),
    xlab = "",
    ylab = ""
  ))
  expect_null(option[["xAxis"]][["name"]])
  expect_null(option[["yAxis"]][["name"]])
})

test_that("axis_title() draws a label and drops NULL or an empty string", {
  expect_identical(axis_title("Age"), "Age")
  expect_null(axis_title(""))
  expect_null(axis_title(NULL))
})
