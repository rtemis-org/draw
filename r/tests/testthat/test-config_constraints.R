test_that("structural constraints use the same declaration in R and JSON Schema", {
  skip_if_not_installed("jsonvalidate", "1.5.0")
  cases <- list(
    list(
      setup = setup_LineConfig,
      cls = LineConfig,
      good = list(
        list(group = "g", y = "a"),
        list(y = c("a", "b")),
        list(xlim = c(0, 1))
      ),
      bad = list(
        list(group = "g", y = c("a", "b")),
        list(group = "g", blocks = "b"),
        list(xlim = 1:3)
      )
    ),
    list(
      setup = setup_ScatterConfig,
      cls = ScatterConfig,
      good = list(list(xlim = NULL), list(ylim = c(-1, 1))),
      bad = list(list(ylim = 1:3))
    ),
    list(
      setup = setup_SignificanceConfig,
      cls = SignificanceConfig,
      good = list(
        list(group = "g", palette = "red"),
        list(view = "manhattan"),
        list(zero_cap = 4)
      ),
      bad = list(
        list(palette = "red"),
        list(view = "manhattan", xlim = c(0, 1)),
        list(p_transform = "identity", zero_cap = 4),
        list(xlim = 1:3)
      )
    ),
    list(
      setup = setup_HistogramConfig,
      cls = HistogramConfig,
      good = list(
        list(bins = 4),
        list(bin_edges = 0:3),
        list(bin_stat = "mean"),
        list(mode = "ridge")
      ),
      bad = list(
        list(bins = 4, bin_edges = 0:3),
        list(bin_stat = "mean", density = TRUE),
        list(bin_stat = "sum", normalization = "density"),
        list(bar_mode = "stack", mode = "ridge"),
        list(bar_mode = "group", density = TRUE)
      )
    ),
    list(
      setup = setup_SurvivalConfig,
      cls = SurvivalConfig,
      good = list(
        list(lower = "lo", upper = "hi"),
        list(lower = NULL, upper = NULL)
      ),
      bad = list(list(lower = "lo"), list(upper = "hi"))
    ),
    list(
      setup = setup_ConfusionConfig,
      cls = ConfusionConfig,
      good = list(list(low_color = NULL), list(correct_color = "#a1B2c3")),
      bad = list(list(correct_color = "red"), list(summary_color = "#123"))
    )
  )
  for (case in cases) {
    schema <- chart_schema(
      case[["cls"]],
      "https://example.org/config",
      "Config",
      "Config"
    )
    validate <- jsonvalidate::json_validator(
      jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
      engine = "ajv"
    )
    for (kind in c("good", "bad")) {
      for (args in case[[kind]]) {
        expected <- kind == "good"
        if (expected) {
          expect_no_error(do.call(case[["setup"]], args))
        } else {
          expect_error(do.call(case[["setup"]], args))
        }
        values <- chart_config_to_list(case[["setup"]](), complete = TRUE)
        for (name in names(args)) {
          value <- args[[name]]
          spec <- prop_spec(case[["cls"]]@properties[[name]])
          if (!is.null(value) && identical(spec[["container"]], "array")) {
            value <- as.list(value)
          }
          values[name] <- list(value)
        }
        expect_identical(
          validate(jsonlite::toJSON(values, auto_unbox = TRUE, null = "null")),
          expected,
          info = paste(case[["cls"]]@name, paste(names(args), collapse = ","))
        )
      }
    }
    # Partial inputs do not acquire guessed defaults in schema constraints.
    expect_true(validate(jsonlite::toJSON(
      list(type = chart_type_of(case[["cls"]])),
      auto_unbox = TRUE
    )))
  }
})

test_that("relational limits and data rules remain runtime checks", {
  for (setup in list(
    setup_LineConfig,
    setup_ScatterConfig,
    setup_SignificanceConfig
  )) {
    expect_error(setup(xlim = c(2, 1)), "increasing")
    expect_error(setup(ylim = c(1, 1)), "increasing")
    expect_error(setup(xlim = c(0, Inf)))
  }
  expect_error(setup_HistogramConfig(bin_edges = c(0, 2, 1)), "increasing")
  expect_null(config_ordered_limits(setup_ScatterConfig(xlim = c(-1, 1))))
})

test_that("structural matching handles conditionals, missing fields and unsupported rules", {
  expect_true(config_matches(NULL, list(type = "null")))
  expect_false(config_matches(1, list(type = "null")))
  expect_true(config_matches(1, list(enum = list(1L, 2L))))
  expect_false(config_matches(3, list(enum = list(1L, 2L))))
  expect_true(config_matches(
    2,
    list(allOf = list(list(not = list(const = 1)), list(const = 2)))
  ))
  expect_false(config_matches(
    1,
    list(anyOf = list(list(const = 2), list(const = 3)))
  ))
  expect_true(config_matches(
    list(),
    list(properties = list(a = list(const = 2)))
  ))
  expect_false(config_matches(list(), list(required = list("a"))))
  expect_false(config_matches(1:2, list(minItems = 3L)))
  expect_error(config_matches(1, list(minimum = 0)), "keywords")
  expect_error(config_matches(1, list(type = "number")), "prop_\\*")
})

test_that("schema constraints follow S7 inheritance", {
  Child <- new_class("ConstrainedLine", parent = LineConfig)
  schema <- chart_schema(Child, "https://example.org/child", "Child", "Child")
  expect_equal(
    schema[["allOf"]],
    chart_schema(LineConfig, "https://example.org/line", "Line", "Line")[[
      "allOf"
    ]]
  )
  expect_error(Child(xlim = c(1, 2, 3)), "two")
})


test_that("all registered input and complete records validate independently", {
  skip_if_not_installed("jsonvalidate", "1.5.0")
  for (entry in chart_registry()) {
    for (complete in c(FALSE, TRUE)) {
      schema <- chart_schema(
        entry[["cls"]],
        "https://example.org/chart",
        "Chart",
        "Chart",
        complete = complete
      )
      config <- get(entry[["setup"]])()
      document <- chart_config_to_list(config, complete = complete)
      expect_true(
        jsonvalidate::json_validate(
          jsonlite::toJSON(document, auto_unbox = TRUE, null = "null"),
          jsonlite::toJSON(schema, auto_unbox = TRUE, null = "null"),
          engine = "ajv"
        ),
        info = paste(entry[["cls"]]@name, complete)
      )
      if (!complete) {
        expect_no_error(rtemis.core::assert_config_contract(
          schema,
          entry[["cls"]]@name,
          structural = "type"
        ))
      }
    }
  }
  # An authored partial document can await resolution, but explicit incompatible
  # values must fail. This distinction is part of the shared input contract.
  schema <- chart_schema(
    SurvivalConfig,
    "https://example.org/survival",
    "Survival",
    "Survival"
  )
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(schema, auto_unbox = TRUE),
    engine = "ajv"
  )
  expect_true(validate('{"type":"survival","lower":"lo"}'))
  expect_false(validate('{"type":"survival","lower":"lo","upper":null}'))
})


test_that("partial provenance and writer validation agree with their schemas", {
  skip_if_not_installed("jsonvalidate", "1.5.0")
  cfg <- ScatterConfig(origin = c(x = "user"))
  schema <- chart_schema(
    ScatterConfig,
    "https://example.org/partial",
    "Partial",
    "Partial"
  )
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(schema, auto_unbox = TRUE),
    engine = "ajv"
  )
  expect_true(validate(jsonlite::toJSON(
    chart_config_to_list(cfg),
    auto_unbox = TRUE
  )))
  expect_error(chart_config_to_list(cfg, complete = TRUE), "every property")
  expect_false(validate('{"type":"scatter","origin":{}}'))
  expect_false(validate('{"type":"scatter","origin":{"unknown":"user"}}'))
  expect_error(ScatterConfig(writer = c(name = "R")), "both name and version")
  expect_false(validate('{"type":"scatter","writer":{"name":"R"}}'))
  expect_true(validate(
    '{"type":"scatter","writer":{"name":"R","version":"1"}}'
  ))
})
