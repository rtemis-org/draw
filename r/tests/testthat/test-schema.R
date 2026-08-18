# test-schema.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The generated chart schemas. These check the contract structurally, from what
# the package itself can see: `data-raw/` is .Rbuildignore'd, so a test cannot
# reach the registry in a built package, but the emitter is package code and the
# classes are the source of truth for everything it emits.
#
# The load-bearing one is "no `default` is emitted anywhere". A default is what
# an interface chooses to fill in, and interfaces are expected to differ; a
# schema stating one would make a chart pane and a web canvas wrong about each
# other.

SCATTER_ID <- "https://schema.rtemis.org/chart/scatter/v1/schema.json"

scatter_schema <- function(complete = FALSE) {
  chart_schema(
    ScatterConfig,
    id = SCATTER_ID,
    title = "rtemis ScatterConfig",
    description = "Scatter chart.",
    complete = complete
  )
}

# Walk every subschema, so a violation nested inside `items` or
# `additionalProperties` is found rather than only a top-level one.
walk_subschemas <- function(x, path = "") {
  if (!is.list(x)) {
    return(list())
  }
  found <- list(stats::setNames(list(x), path))
  for (key in c("properties", "items", "additionalProperties", "oneOf")) {
    child <- x[[key]]
    if (is.list(child)) {
      nms <- names(child)
      if (is.null(nms)) {
        for (i in seq_along(child)) {
          found <- c(
            found,
            walk_subschemas(child[[i]], paste0(path, "/", key, "[", i, "]"))
          )
        }
      } else if (key %in% c("items", "additionalProperties")) {
        found <- c(found, walk_subschemas(child, paste0(path, "/", key)))
      } else {
        for (nm in nms) {
          found <- c(
            found,
            walk_subschemas(child[[nm]], paste0(path, "/", key, "/", nm))
          )
        }
      }
    }
  }
  unlist(found, recursive = FALSE)
}


# %% shape ----

test_that("the schema declares the 2020-12 dialect and its own id", {
  s <- scatter_schema()
  expect_identical(s[["$schema"]], JSON_SCHEMA_DIALECT)
  expect_identical(s[["$id"]], SCATTER_ID)
  expect_identical(s[["type"]], "object")
  expect_false(s[["additionalProperties"]])
})

test_that("every class property appears in the schema", {
  s <- scatter_schema()
  expect_setequal(names(s[["properties"]]), names(ScatterConfig@properties))
})

test_that("the discriminator is emitted as a constant", {
  s <- scatter_schema()
  expect_identical(s[["properties"]][["type"]][["const"]], "scatter")
})


# %% the contract ----

test_that("no default is emitted anywhere in the document", {
  for (complete in c(FALSE, TRUE)) {
    subs <- walk_subschemas(scatter_schema(complete))
    offenders <- names(subs)[vapply(
      subs,
      function(sub) "default" %in% names(sub),
      logical(1L)
    )]
    expect_identical(offenders, character())
  }
})

test_that("an input config requires only the discriminator", {
  expect_identical(as.character(scatter_schema()[["required"]]), "type")
})

test_that("an output config requires every property", {
  s <- scatter_schema(complete = TRUE)
  expect_setequal(
    as.character(s[["required"]]),
    names(ScatterConfig@properties)
  )
})

test_that("required is an array even when it holds one key", {
  # `auto_unbox = TRUE` would otherwise emit a bare string, which is not a
  # valid `required`.
  json <- jsonlite::toJSON(scatter_schema(), auto_unbox = TRUE, null = "null")
  expect_match(as.character(json), '"required":\\["type"\\]', fixed = FALSE)
})


# %% property translation ----

test_that("a nullable property admits null in its type", {
  p <- scatter_schema()[["properties"]][["x"]]
  expect_setequal(p[["type"]], c("string", "null"))
})

test_that("a nullable enum lists null as well", {
  # `type` and `enum` must both admit the value, so an enum that omits null
  # would declare a null it then rejects.
  p <- scatter_schema()[["properties"]][["fit"]]
  expect_true("null" %in% p[["type"]])
  expect_true(any(vapply(p[["enum"]], is.null, logical(1L))))
  expect_setequal(
    unlist(Filter(Negate(is.null), p[["enum"]])),
    c("glm", "gam")
  )
})

test_that("bounds are carried across", {
  props <- scatter_schema()[["properties"]]
  expect_identical(props[["n_fit"]][["minimum"]], 2L)
  expect_identical(props[["fit_alpha"]][["minimum"]], 0)
  expect_identical(props[["fit_alpha"]][["maximum"]], 1)
  expect_identical(props[["margin_left"]][["minimum"]], 0L)
})

test_that("a vector property becomes an array with its element type", {
  props <- scatter_schema()[["properties"]]
  expect_true("array" %in% props[["palette"]][["type"]])
  expect_identical(props[["palette"]][["items"]][["type"]], "string")
  expect_identical(props[["xlim"]][["minItems"]], 2L)
})

test_that("every property carries a description", {
  props <- scatter_schema()[["properties"]]
  described <- vapply(
    props,
    function(p) is.character(p[["description"]]) && nzchar(p[["description"]]),
    logical(1L)
  )
  expect_true(all(described))
})


# %% dispatcher ----

test_that("the dispatcher enumerates every chart type and refs every leaf", {
  classes <- list(ScatterConfig)
  ids <- SCATTER_ID
  d <- chart_dispatcher_schema(
    classes = classes,
    id = "https://schema.rtemis.org/chart/v1/schema.json",
    leaf_ids = ids,
    title = "rtemis chart",
    description = "Chart family."
  )
  expect_setequal(
    as.character(d[["properties"]][["type"]][["enum"]]),
    "scatter"
  )
  expect_identical(as.character(d[["required"]]), "type")
  expect_length(d[["oneOf"]], length(classes))
  expect_identical(d[["oneOf"]][[1L]][["$ref"]], SCATTER_ID)
})

test_that("the dispatcher rejects mismatched classes and leaf ids", {
  expect_error(
    chart_dispatcher_schema(
      classes = list(ScatterConfig),
      id = "https://schema.rtemis.org/chart/v1/schema.json",
      leaf_ids = character(),
      title = "t",
      description = "d"
    ),
    "same length"
  )
})


# %% failure modes ----

test_that("chart_schema rejects a non-class", {
  expect_error(
    chart_schema(list(), id = SCATTER_ID, title = "t", description = "d"),
    "S7 class"
  )
})

test_that("a class without a constant type cannot be dispatched on", {
  Bare <- S7::new_class(
    "Bare",
    parent = ChartConfig,
    properties = list(dummy = prop_string(NULL, nullable = TRUE))
  )
  expect_error(chart_type_of(Bare), "prop_chart_type")
})

test_that("a property with no spec fails generation loudly", {
  # A bare S7 property has no wire form and no way to describe one, so the
  # emitter must refuse rather than silently drop it.
  Undeclared <- S7::new_class(
    "Undeclared",
    parent = ChartConfig,
    properties = list(
      type = prop_chart_type("undeclared"),
      loose = S7::new_property(S7::class_character)
    )
  )
  expect_error(
    chart_schema(Undeclared, id = SCATTER_ID, title = "t", description = "d"),
    "prop_\\* factory"
  )
})


# %% writing ----

test_that("write_chart_schema writes parseable JSON and creates its directory", {
  path <- file.path(tempfile(), "chart", "scatter", "v1", "schema.json")
  on.exit(unlink(dirname(dirname(dirname(dirname(path)))), recursive = TRUE))
  write_chart_schema(scatter_schema(), path)
  expect_true(file.exists(path))
  round_tripped <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(round_tripped[["$id"]], SCATTER_ID)
  expect_identical(round_tripped[["required"]], list("type"))
})


# %% provenance ----

test_that("origin values are constrained to the three the description names", {
  origin <- scatter_schema()[["properties"]][["origin"]]
  expect_identical(
    as.character(origin[["additionalProperties"]][["enum"]]),
    c("user", "default", "derived")
  )
})

test_that("an output config's provenance is required and non-null", {
  # Everything else may be null in a complete document -- "this chart has no
  # title" is a fact worth recording -- but a document that cannot say where
  # its values came from is not complete, whatever it claims.
  complete <- chart_schema(
    ScatterConfig,
    id = SCATTER_ID,
    title = "t",
    description = "d",
    complete = TRUE
  )
  for (nm in c("origin", "writer")) {
    expect_identical(complete[["properties"]][[nm]][["type"]], "object")
  }
  # The input config leaves both nullable: an authored config has neither.
  input <- scatter_schema()
  for (nm in c("origin", "writer")) {
    expect_setequal(input[["properties"]][[nm]][["type"]], c("object", "null"))
  }
})

test_that("a chart's own properties stay nullable in an output config", {
  complete <- chart_schema(
    ScatterConfig,
    id = SCATTER_ID,
    title = "t",
    description = "d",
    complete = TRUE
  )
  expect_setequal(
    complete[["properties"]][["title"]][["type"]],
    c("string", "null")
  )
})
