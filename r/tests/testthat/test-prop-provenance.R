# test-prop-provenance.R
# Every option-class property is declared by a `prop_*` factory, not by one of
# rtemis.core's hand-written properties.
#
# Those hand-written properties carry no `PropertySpec`: `prop_spec()` returns
# NULL for them, so a class built on one and its published schema are free to
# disagree. They are on their way out of rtemis.core, and this is what keeps
# them from coming back here.
#
# This does *not* assert that every property carries a spec, because many still
# do not: `numeric_or_null_property()`, `numeric_or_string_property()` and
# `class_or_null_property()` are this package's own hand-written properties,
# for shapes rtemis.core has no factory for (a number-or-string union, a nested
# option class, an untyped `class_any`), and `prop_chart_type()` is
# deliberately spec-less -- `chart_schema()` emits the discriminator from its
# own branch before it ever asks for a spec. Retiring those is separate work;
# what is settled here is that none of *rtemis.core's* legacy properties is
# used any more, which is what lets that package remove them.

test_that("no property is one of rtemis.core's spec-less hand-written ones", {
  # The bare property objects rtemis.core exports and this package no longer
  # uses. Compared by identity: they are singletons, so a property built from
  # one *is* one.
  legacy <- c(
    "character_scalar",
    "optional_character_scalar",
    "double_scalar",
    "optional_double_scalar",
    "integer_scalar",
    "optional_integer_scalar",
    "nonneg_integer_scalar",
    "optional_nonneg_integer_scalar",
    "pos_integer_scalar",
    "optional_pos_integer_scalar",
    "logical_scalar",
    "optional_logical_scalar",
    "prob_scalar",
    "optional_prob_scalar",
    "unit_open_scalar",
    "optional_unit_open_scalar",
    "pos_double_scalar",
    "optional_pos_double_scalar",
    "nonneg_double_scalar",
    "optional_nonneg_double_scalar",
    "prob_vector",
    "optional_prob_vector",
    "unit_open_vector",
    "optional_unit_open_vector",
    "pos_double_vector",
    "optional_pos_double_vector",
    "nonneg_double_vector",
    "optional_nonneg_double_vector"
  )
  core <- asNamespace("rtemis.core")
  legacy <- legacy[vapply(legacy, exists, logical(1), envir = core)]
  # If rtemis.core has retired them all, there is nothing left to guard against.
  skip_if(length(legacy) == 0L, "rtemis.core no longer exports them")
  legacy_props <- lapply(legacy, get, envir = core)

  ns <- asNamespace("rtemis.draw")
  classes <- Filter(
    function(nm) inherits(get(nm, envir = ns), "S7_class"),
    ls(ns, all.names = TRUE)
  )
  expect_gt(length(classes), 0L)

  found <- character()
  for (cn in classes) {
    props <- get(cn, envir = ns)@properties
    for (pn in names(props)) {
      for (i in seq_along(legacy_props)) {
        if (identical(props[[pn]], legacy_props[[i]])) {
          found <- c(found, paste0(cn, "@", pn, " = ", legacy[[i]]))
        }
      }
    }
  }
  expect_identical(found, character())
})
