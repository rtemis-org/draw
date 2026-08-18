# test-chart_io.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Reading and writing chart configs as JSON, and the provenance that travels
# with them.
#
# The load-bearing test is the round trip: a config written in one interface and
# read in another must produce the same chart. That is the whole point of the
# schema, and it is the property most easily broken by a serialization detail
# (JSON has one number type; jsonlite drops names from atomic vectors; a
# one-element array unboxes to a scalar).

# Files land in the session temp directory, which R clears on exit. Registering
# per-test cleanup would need withr, which is not a declared dependency.
tmp_json <- function() {
  tempfile(fileext = ".json")
}


# %% registry ----

test_that("every registered chart type maps to a class and a setup function", {
  registry <- chart_registry()
  expect_gt(length(registry), 0L)
  for (type in names(registry)) {
    entry <- registry[[type]]
    expect_true(inherits(entry[["cls"]], "S7_class"))
    expect_identical(chart_type_of(entry[["cls"]]), type)
    expect_true(is.function(get(
      entry[["setup"]],
      envir = asNamespace("rtemis.draw")
    )))
  }
})


# %% origin ----

test_that("origin marks supplied values user and the rest default", {
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm")
  expect_identical(cfg@origin[["x"]], "user")
  expect_identical(cfg@origin[["y"]], "user")
  expect_identical(cfg@origin[["fit"]], "user")
  expect_identical(cfg@origin[["se"]], "default")
  expect_identical(cfg@origin[["n_fit"]], "default")
})

test_that("origin covers every settable property", {
  cfg <- setup_ScatterConfig()
  expect_setequal(
    names(cfg@origin),
    setdiff(names(ScatterConfig@properties), c("type", "origin", "writer"))
  )
})

test_that("a value set to its default value is still user-supplied", {
  # Origin records what the author *did*, not whether the value differs from
  # the default -- otherwise an explicit choice that happens to match would be
  # silently re-resolvable by another interface.
  cfg <- setup_ScatterConfig(se = TRUE)
  expect_identical(cfg@origin[["se"]], "user")
})


# %% serialization ----

test_that("an authored config serializes only what was set", {
  lst <- chart_config_to_list(setup_ScatterConfig(x = "wt", y = "mpg"))
  expect_true(all(c("type", "x", "y") %in% names(lst)))
  # Unset properties are dropped, so an authored document stays as small as it
  # was authored.
  expect_false("dat_path" %in% names(lst))
  expect_false("xlim" %in% names(lst))
})

test_that("the type discriminator is always present", {
  expect_identical(
    chart_config_to_list(setup_ScatterConfig())[["type"]],
    "scatter"
  )
})

test_that("a map property serializes as a JSON object, keeping its keys", {
  path <- tmp_json()
  write_chart_config(setup_ScatterConfig(x = "wt"), path)
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(is.list(parsed[["origin"]]))
  expect_identical(parsed[["origin"]][["x"]], "user")
})

test_that("a one-element array property stays an array", {
  # `auto_unbox` would otherwise emit a bare string, which the schema declares
  # as an array and a reader would reject.
  path <- tmp_json()
  write_chart_config(setup_ScatterConfig(palette = "#FF0000"), path)
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(is.list(parsed[["palette"]]))
  expect_length(parsed[["palette"]], 1L)
})

test_that("chart_config_to_list rejects a non-config", {
  expect_error(chart_config_to_list(list()), "ChartConfig")
})


# %% round trip ----

test_that("a config round-trips through JSON identically", {
  cfg <- setup_ScatterConfig(
    x = "wt",
    y = "mpg",
    group = "cyl",
    fit = "glm",
    se = FALSE,
    n_fit = 50L,
    fit_alpha = 0.4,
    palette = c("#FF0000", "#00FF00"),
    xlim = c(0, 6),
    ylim = c(10, 35),
    xlab = "Weight",
    title = "Cars",
    margin_left = 40L
  )
  path <- tmp_json()
  write_chart_config(cfg, path)
  expect_identical(read_chart_config(path), cfg)
})

test_that("a round-tripped config draws the same chart", {
  # The property the whole design exists for: write in one interface, recreate
  # in another.
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm", title = "Cars")
  path <- tmp_json()
  write_chart_config(cfg, path)
  expect_identical(
    compile(read_chart_config(path), data = mtcars),
    compile(cfg, data = mtcars)
  )
})

test_that("provenance is carried through a round trip, not recomputed", {
  # If a read collapsed everything to "user", a value an interface merely
  # defaulted would harden into a choice on the first hop, and no later
  # interface could re-resolve it for its own display.
  cfg <- setup_ScatterConfig(x = "wt", y = "mpg")
  path <- tmp_json()
  write_chart_config(cfg, path)
  back <- read_chart_config(path)
  expect_identical(back@origin, cfg@origin)
  expect_identical(back@origin[["se"]], "default")
})

test_that("a hand-authored document reads without provenance", {
  path <- tmp_json()
  writeLines('{"type": "scatter", "x": "wt", "y": "mpg"}', path)
  cfg <- read_chart_config(path)
  expect_identical(cfg@x, "wt")
  # Nothing claimed an origin, so the keys the document carried are the
  # author's and everything else is this interface's.
  expect_identical(cfg@origin[["x"]], "user")
  expect_identical(cfg@origin[["se"]], "default")
})

test_that("numeric type is taken from the class, not from the parser", {
  # JSON has one number type, so `[0, 6]` parses back as integer; the property
  # is a double and the declared type is the authority.
  path <- tmp_json()
  writeLines('{"type": "scatter", "x": "wt", "y": "mpg", "xlim": [0, 6]}', path)
  expect_identical(read_chart_config(path)@xlim, c(0, 6))
})

test_that("a writer block round-trips", {
  cfg <- setup_ScatterConfig(x = "wt", writer = chart_writer())
  path <- tmp_json()
  write_chart_config(cfg, path)
  back <- read_chart_config(path)
  expect_identical(back@writer[["name"]], "rtemis.draw")
  expect_identical(back@writer, cfg@writer)
})


# %% reading failures ----

test_that("a document with no type is rejected", {
  path <- tmp_json()
  writeLines('{"x": "wt", "y": "mpg"}', path)
  expect_error(read_chart_config(path), "no `type` key")
})

test_that("an unknown chart type names the known ones", {
  path <- tmp_json()
  writeLines('{"type": "nope", "x": "wt"}', path)
  expect_error(read_chart_config(path), "Unknown chart type")
  expect_error(read_chart_config(path), "scatter")
})

test_that("an invalid value is rejected on read, through the same validator", {
  # Reading goes through setup_*, so a document cannot construct an object a
  # direct call could not.
  path <- tmp_json()
  writeLines('{"type": "scatter", "x": "wt", "fit": "bogus"}', path)
  expect_error(read_chart_config(path))
})
