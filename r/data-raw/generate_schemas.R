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

# Leaves. Each is written twice: `schema.json`, the **input config**, which
# requires only the discriminator; and `complete.json`, the **output config**,
# which requires every property. The two differ in `required` and in nothing
# else -- an input and an output config are the same kind of document, differing
# only in how much has been filled in.
leaf_ids <- character(length(charts))
for (i in seq_along(charts)) {
  type <- names(charts)[[i]]
  chart <- charts[[i]]
  leaf_ids[[i]] <- leaf_url(type, "schema")
  dir <- file.path(schema_repo, family, type, "v1")
  for (kind in c("schema", "complete")) {
    schema <- chart_schema(
      chart[["cls"]],
      id = leaf_url(type, kind),
      title = paste0("rtemis ", chart[["cls"]]@name),
      description = chart_descriptions[[type]],
      complete = kind == "complete"
    )
    write_chart_schema(schema, file.path(dir, paste0(kind, ".json")))
  }
  cat(sprintf("%-14s schema + complete\n", type))
}

# Dispatcher: a document is a chart if it matches exactly one leaf, selected by
# its `type`.
dispatcher <- chart_dispatcher_schema(
  classes = lapply(charts, `[[`, "cls"),
  id = paste0(base_url, "/", family, "/v1/schema.json"),
  leaf_ids = leaf_ids,
  title = chart_family[["title"]],
  description = chart_family[["description"]]
)
write_chart_schema(
  dispatcher,
  file.path(schema_repo, family, "v1", "schema.json")
)
cat(sprintf("%-14s dispatcher over %d chart(s)\n", family, length(charts)))
