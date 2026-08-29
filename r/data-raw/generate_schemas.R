# generate_schemas.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Single source of truth for the schema.rtemis.org chart schemas. Generates one
# leaf schema per chart type plus the `chart/v1` dispatcher, and writes them to
# the schema repo in the same `<family>/v1` + `<family>/<type>/v1` layout rtemis
# uses.
#
# Run with: Rscript data-raw/generate_schemas.R [SCHEMA_REPO]
#
# **Regenerate, never hand-edit.** A hand-edited schema detaches from its source
# and the next regeneration silently reverts it.

suppressMessages(pkgload::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"
family <- "chart"

source(file.path("data-raw", "schema_registry.R"))

# The registry is the package's, so publishing and reading cannot disagree.
charts <- chart_registry()
missing <- setdiff(names(charts), names(chart_descriptions))
if (length(missing) > 0L) {
  stop(
    "No description for chart type(s): ",
    paste(missing, collapse = ", "),
    ". Add one to data-raw/schema_registry.R.",
    call. = FALSE
  )
}

leaf_url <- function(type, kind) {
  paste0(base_url, "/", family, "/", type, "/v1/", kind, ".json")
}

# Every document in the registry is written by one function, so the keyword
# order and the serialization are the registry's rather than this package's.
# `digits = I(17)` is the one thing a chart schema needs that the default does
# not give: 17 significant digits is what round-trips an IEEE 754 double
# exactly, and jsonlite's own default of 4 *decimal places* would silently round
# a resolved axis limit -- a document that draws a nearly identical chart, which
# is worse than one that obviously fails.
write_schema <- function(schema, path) {
  rtemis.core::write_JSONSchema(
    schema,
    path,
    overwrite = TRUE,
    digits = I(17),
    verbosity = 0L
  )
}

# Leaves. Each is written twice, under the two names the registry publishes:
#
#   schema.json  the **input config** -- requires only the discriminator.
#   record.json  the **output config** -- every property, provenance included.
#
# `record.json` is the registry's own name for "the same field vocabulary, but
# every value resolved and annotated with where it came from" (see
# `scripts/build-index.mjs` there), and it is the name `build-index.mjs`
# collects. A document under any other name serves fine at its URL and is
# invisible to everything that syncs by the manifest -- which is every consumer.
leaf_ids <- character(length(charts))
record_ids <- character(length(charts))
for (i in seq_along(charts)) {
  type <- names(charts)[[i]]
  chart <- charts[[i]]
  leaf_ids[[i]] <- leaf_url(type, "schema")
  record_ids[[i]] <- leaf_url(type, "record")
  dir <- file.path(schema_repo, family, type, "v1")
  for (kind in c("schema", "record")) {
    schema <- chart_schema(
      chart[["cls"]],
      id = leaf_url(type, kind),
      title = paste0("rtemis ", chart[["cls"]]@name),
      description = chart_descriptions[[type]],
      complete = kind == "record"
    )
    write_schema(schema, file.path(dir, paste0(kind, ".json")))
  }
  cat(sprintf("%-14s schema + record\n", type))
}

# Dispatchers, one per kind: a document is a chart if it matches exactly one
# leaf, selected by its `type`. The record dispatcher answers the same question
# of a written document -- "is this a complete chart of any type" -- and every
# other family publishes a family-level record beside its family-level schema.
for (kind in c("schema", "record")) {
  dispatcher <- chart_dispatcher_schema(
    classes = lapply(charts, `[[`, "cls"),
    id = paste0(base_url, "/", family, "/v1/", kind, ".json"),
    leaf_ids = if (kind == "record") record_ids else leaf_ids,
    title = chart_family[["title"]],
    description = chart_family[["description"]]
  )
  write_schema(
    dispatcher,
    file.path(schema_repo, family, "v1", paste0(kind, ".json"))
  )
}
cat(sprintf(
  "%-14s schema + record dispatcher over %d chart(s)\n",
  family,
  length(charts)
))
