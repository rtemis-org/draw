# schema.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# JSON Schema generation for the chart config classes.
#
# A chart schema is a set of typed properties with descriptions -- nothing
# more. `rtemis.core`'s `prop_spec()` is already a JSON Schema property fragment
# in all but spelling (`minimum`, `maximum`, `enum`, `const`, `nullable`,
# `container`, `min_items`, `unique_items`, `allow_empty`, `description`), so
# the emitter below is largely a field rename plus a wrapper.
#
# Two rules it enforces:
#
# 1. **No `default` is ever emitted.** A default is what an interface chooses
#    to fill in, not a fact about the document, and interfaces are expected to
#    differ -- a chart pane in an IDE and a large web canvas should not be
#    forced to agree. Round-trip fidelity comes from writing *resolved*
#    documents, not from sharing defaults.
# 2. **An input config requires nothing** beyond the discriminator, which
#    carries the document's shape. A config is partial by nature: the author
#    sets a subset and the interface fills in the rest. The `complete` variant
#    requires every property, which is a claim about a *written* document
#    rather than a constraint on what an author has to type.
#
# Each leaf is self-contained: it declares its own `type` constant and closes
# with `additionalProperties: false`, so it validates standalone rather than
# only through the dispatcher.

# %% JSON_SCHEMA_DIALECT ----
# The dialect every emitted document declares.
JSON_SCHEMA_DIALECT <- "https://json-schema.org/draft/2020-12/schema"


# %% schema_leaf_type ----
#' JSON Schema fragment for a property's leaf value
#'
#' The element type, before any container wrapping and before nullability is
#' applied. Split out because an array's `items` needs exactly this and nothing
#' else.
#'
#' @param spec Named list: A property spec, from `rtemis.core::prop_spec()`.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_leaf_type <- function(spec) {
  out <- list(type = spec[["type"]])
  if (!is.null(spec[["enum"]])) {
    # I(): `enum` is an array even with one member, and auto_unbox would
    # otherwise collapse a single value to a bare string.
    out[["enum"]] <- I(spec[["enum"]])
  }
  if (!is.null(spec[["const"]])) {
    out[["const"]] <- spec[["const"]]
  }
  for (pair in list(
    c("minimum", "minimum"),
    c("maximum", "maximum"),
    c("exclusive_minimum", "exclusiveMinimum"),
    c("exclusive_maximum", "exclusiveMaximum")
  )) {
    value <- spec[[pair[[1L]]]]
    if (!is.null(value)) {
      out[[pair[[2L]]]] <- value
    }
  }
  # A string property rejects "" unless it opts in, so say so on the wire.
  if (identical(spec[["type"]], "string") && !isTRUE(spec[["allow_empty"]])) {
    out[["minLength"]] <- 1L
  }
  out
} # /rtemis.draw::schema_leaf_type


# %% schema_property ----
#' JSON Schema fragment for one property
#'
#' Applies the container (array or string-keyed map), then nullability, then the
#' description. Never emits a `default`.
#'
#' Nullability is applied to the `enum` as well as to `type`: a value must
#' satisfy both keywords, so a nullable enum that does not list `null` declares
#' a null it then rejects.
#'
#' @param spec Named list: A property spec, from `rtemis.core::prop_spec()`.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_property <- function(spec) {
  container <- spec[["container"]]
  out <- if (identical(container, "array")) {
    items <- list(type = "array", items = schema_leaf_type(spec))
    if (!is.null(spec[["min_items"]])) {
      items[["minItems"]] <- as.integer(spec[["min_items"]])
    }
    if (isTRUE(spec[["unique_items"]])) {
      items[["uniqueItems"]] <- TRUE
    }
    items
  } else if (identical(container, "map")) {
    list(type = "object", additionalProperties = schema_leaf_type(spec))
  } else {
    schema_leaf_type(spec)
  }

  if (isTRUE(spec[["nullable"]])) {
    out[["type"]] <- c(out[["type"]], "null")
    if (!is.null(out[["enum"]])) {
      out[["enum"]] <- I(c(as.list(out[["enum"]]), list(NULL)))
    }
  }
  if (nzchar(spec[["description"]])) {
    out[["description"]] <- spec[["description"]]
  }
  out
} # /rtemis.draw::schema_property


# %% origin_property ----
#' JSON Schema fragment for a document's `origin` map
#'
#' A closed object with one entry per settable property, all required, rather
#' than an open string-keyed map. That is what makes "this document says where
#' every one of its values came from" checkable by a validator instead of only
#' by the writer: a map missing an entry, or naming a property the chart does
#' not have, fails here.
#'
#' Matches the `origin` block every `record.json` in the rtemis registry
#' carries. The chart vocabulary is the three of `ORIGIN_VALUES` a chart can
#' actually produce -- `"tuned"` needs a tuner and `"unset"` means a run never
#' reached the value, neither of which a chart has.
#'
#' @param names Character: The settable property names.
#' @param nullable Logical: Whether the map itself may be null, which it may be
#'   on an input config and may not be on a record.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
origin_property <- function(names, nullable) {
  entries <- lapply(names, function(nm) {
    list(type = "string", enum = I(ORIGIN_VALUES))
  })
  names(entries) <- names
  list(
    type = if (nullable) c("object", "null") else "object",
    description = paste(
      "Where each value came from: 'user' if the author set it, 'default' if",
      "the interface filled it in, 'derived' if it was computed from the data.",
      "Carried through a re-write, never recomputed: a value the author chose",
      "must stay honored, while a defaulted one may be re-resolved for a",
      "different display."
    ),
    properties = entries,
    required = I(names),
    additionalProperties = FALSE
  )
} # /rtemis.draw::origin_property


# %% writer_property ----
#' JSON Schema fragment for a document's `writer`
#'
#' Closed for the same reason as `origin`: the two keys are the whole contract,
#' so a third one is a mistake worth catching rather than a field to ignore.
#'
#' @param nullable Logical: Whether the block itself may be null.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writer_property <- function(nullable) {
  list(
    type = if (nullable) c("object", "null") else "object",
    description = paste(
      "Which interface wrote this document, as `name` and `version`.",
      "Absent on an authored config."
    ),
    properties = list(
      name = list(
        type = "string",
        minLength = 1L,
        description = "The interface that wrote the document."
      ),
      version = list(
        type = "string",
        minLength = 1L,
        description = "Its version."
      )
    ),
    required = I(c("name", "version")),
    additionalProperties = FALSE
  )
} # /rtemis.draw::writer_property


# %% chart_schema ----
#' Generate a JSON Schema for a chart config class
#'
#' Emits the schema for one [ChartConfig] subclass: every property it declares
#' or inherits, typed from its `PropertySpec`, plus a constant for the `type`
#' discriminator. The result is self-contained -- it validates a chart document
#' on its own, without the family dispatcher.
#'
#' @details
#' Two kinds, differing only in what they require:
#'
#' - `complete = FALSE` (an **input config**) requires only `type`. The author
#'   sets a subset and the interface fills in the rest.
#' - `complete = TRUE` (an **output config**) requires every property, so a
#'   document that claims to be complete can be checked rather than trusted. The
#'   chart's own properties may still be null -- "this chart has no title" is a
#'   fact worth recording -- but the provenance maps may not: a document that
#'   cannot say where its values came from is not complete, whatever it claims.
#'
#' Neither emits a `default`: what an interface fills in is not a fact about
#' the document.
#'
#' @param cls S7 class: A [ChartConfig] subclass.
#' @param id Character: The schema's `$id` URL.
#' @param title Character: Human-readable schema title.
#' @param description Character: What this chart is.
#' @param complete Logical: If TRUE, emit the output-config kind, requiring
#'   every property.
#'
#' @return Named list: The JSON Schema, ready for [jsonlite::toJSON()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' schema <- chart_schema(
#'   ScatterConfig,
#'   id = "https://schema.rtemis.org/chart/scatter/v1/schema.json",
#'   title = "rtemis ScatterConfig",
#'   description = "Scatter chart."
#' )
#' names(schema[["properties"]])
chart_schema <- function(
  cls,
  id,
  title,
  description,
  complete = FALSE
) {
  check_character_scalar(id)
  check_logical_scalar(complete)
  if (!inherits(cls, "S7_class")) {
    abort(
      "`cls` must be an S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  properties <- cls@properties
  type_value <- chart_type_of(cls)
  # The chart's own properties, which are also exactly the keys an `origin` map
  # covers -- so the map's schema is built from the same list the document is.
  settable <- setdiff(names(properties), c("type", PROVENANCE_PROPS))

  out <- list()
  for (nm in names(properties)) {
    if (identical(nm, "type")) {
      # The discriminator is a computed constant, so it carries no spec.
      out[[nm]] <- list(
        const = type_value,
        description = "Chart type; the schema discriminator."
      )
      next
    }
    if (nm %in% PROVENANCE_PROPS) {
      # Provenance is structured, not a free map, and a record must state it:
      # see @details above.
      out[[nm]] <- if (identical(nm, "origin")) {
        origin_property(settable, nullable = !complete)
      } else {
        writer_property(nullable = !complete)
      }
      next
    }
    spec <- prop_spec(properties[[nm]])
    if (is.null(spec)) {
      # A property with no spec has no wire form and no way to describe one.
      # Failing here beats emitting a schema that quietly omits it.
      abort(
        "Property `",
        nm,
        "` of ",
        cls@name,
        " was not built by a prop_* factory, so it has no schema. Declare it ",
        "with one, or exclude it from the published class.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    out[[nm]] <- schema_property(spec)
  }

  list(
    `$schema` = JSON_SCHEMA_DIALECT,
    `$id` = id,
    title = title,
    description = description,
    type = "object",
    properties = out,
    # An input config requires only the key carrying the document's shape.
    # I(): `required` is an array even when it holds one key.
    required = I(if (complete) names(out) else "type"),
    additionalProperties = FALSE
  )
} # /rtemis.draw::chart_schema


# %% chart_type_of ----
#' Read a chart class's constant type without constructing it
#'
#' The discriminator is a computed property whose getter ignores `self`, so it
#' can be read from the class definition. That avoids default-constructing a
#' class whose defaults may not form a drawable chart.
#'
#' @param cls S7 class: A [ChartConfig] subclass.
#'
#' @return Character: The chart type.
#'
#' @author EDG
#' @keywords internal
#' @noRd
chart_type_of <- function(cls) {
  getter <- cls@properties[["type"]][["getter"]]
  if (is.null(getter)) {
    abort(
      cls@name,
      " does not override `type` with a constant, so it cannot be dispatched ",
      "on. Declare it with prop_chart_type().",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  getter(NULL)
} # /rtemis.draw::chart_type_of


# %% chart_dispatcher_schema ----
#' Generate the chart family dispatcher schema
#'
#' The family's entry point: a document is a chart if it matches exactly one of
#' the leaf schemas, selected by its `type`.
#'
#' @param classes List: [ChartConfig] subclasses.
#' @param id Character: The dispatcher's `$id` URL.
#' @param leaf_ids Character: `$id` of each leaf schema, in the same order as
#'   `classes`.
#' @param title Character: Human-readable schema title.
#' @param description Character: What the family is.
#'
#' @return Named list: The JSON Schema, ready for [jsonlite::toJSON()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' schema <- chart_dispatcher_schema(
#'   classes = list(ScatterConfig, BarConfig),
#'   id = "https://schemas.rtemis.org/draw/chart.schema.json",
#'   leaf_ids = c(
#'     "https://schemas.rtemis.org/draw/scatter.schema.json",
#'     "https://schemas.rtemis.org/draw/bar.schema.json"
#'   ),
#'   title = "Chart",
#'   description = "Any rtemis.draw chart."
#' )
#' schema[["properties"]][["type"]][["enum"]]
chart_dispatcher_schema <- function(
  classes,
  id,
  leaf_ids,
  title,
  description
) {
  check_character_scalar(id)
  if (length(classes) != length(leaf_ids)) {
    abort(
      "`classes` and `leaf_ids` must be the same length.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  types <- vapply(classes, chart_type_of, character(1L))
  list(
    `$schema` = JSON_SCHEMA_DIALECT,
    `$id` = id,
    title = title,
    description = description,
    type = "object",
    properties = list(
      type = list(
        type = "string",
        enum = I(types),
        description = "Chart type; selects which chart schema applies."
      )
    ),
    required = I("type"),
    oneOf = lapply(leaf_ids, function(ref) list(`$ref` = ref))
  )
} # /rtemis.draw::chart_dispatcher_schema


# %% write_chart_schema ----
#' Write a generated schema to disk
#'
#' @param schema Named list: A schema from [chart_schema()] or
#'   [chart_dispatcher_schema()].
#' @param path Character: Destination file.
#'
#' @return `path`, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' schema <- chart_schema(
#'   ScatterConfig,
#'   id = "https://schemas.rtemis.org/draw/scatter.schema.json",
#'   title = "Scatter chart",
#'   description = "A scatter chart configuration."
#' )
#' # Write to the session temp directory; use a real path in your own code.
#' path <- file.path(tempdir(), "scatter.schema.json")
#' write_chart_schema(schema, path)
#' unlink(path)
write_chart_schema <- function(schema, path) {
  check_character_scalar(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(
    schema,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    # 17 significant digits is what round-trips an IEEE 754 double exactly.
    # jsonlite defaults to 4 *decimal places*, which silently rounds a resolved
    # axis limit, and even `digits = NA` loses the last bits -- so a document
    # would draw a *nearly* identical chart, which is worse than an obviously
    # broken one. Verified: I(15) and I(16) still lose them.
    digits = I(17)
  )
  writeLines(as.character(json), path)
  invisible(path)
} # /rtemis.draw::write_chart_schema
