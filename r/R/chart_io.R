# chart_io.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Reading and writing chart configs as JSON. This is the point of the schema:
# write a config in one interface, recreate the chart in another.
#
# The model:
#
# - An **input config** is what an author writes: a subset of the properties,
#   with no `origin` and no `writer`. This is what `write_chart_config()`
#   produces by default.
# - An **output config** is what an interface writes back: every property,
#   present even where its value is null, plus an `origin` map saying where each
#   came from and a `writer` saying who produced it. This is
#   `write_chart_config(complete = TRUE)`.
#
# Both are the same kind of document and differ only in how much is filled in;
# they validate against the registry's `schema.json` and `record.json`, which is
# the name every rtemis family uses for a document whose values are all resolved
# and annotated with where each came from. Reading is `do.call(setup_*, x)`, so a
# document from any source arrives fully resolved through the same seam a
# hand-written call goes through.

# %% chart_registry ----
#' Chart type registry
#'
#' Maps each chart `type` to the class that models it and the `setup_*` that
#' builds one. Read at run time by [read_chart_config()], and at generation time
#' by `data-raw/generate_schemas.R`, so the two cannot disagree about which
#' chart types exist.
#'
#' @return Named list, one entry per chart type, each with `cls` and `setup`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' names(chart_registry())
chart_registry <- function() {
  list(
    scatter = list(cls = ScatterConfig, setup = "setup_ScatterConfig"),
    bar = list(cls = BarConfig, setup = "setup_BarConfig"),
    density = list(cls = DensityConfig, setup = "setup_DensityConfig"),
    histogram = list(cls = HistogramConfig, setup = "setup_HistogramConfig"),
    line = list(cls = LineConfig, setup = "setup_LineConfig"),
    pie = list(cls = PieConfig, setup = "setup_PieConfig"),
    boxplot = list(cls = BoxplotConfig, setup = "setup_BoxplotConfig"),
    sankey = list(cls = SankeyConfig, setup = "setup_SankeyConfig"),
    gantt = list(cls = GanttConfig, setup = "setup_GanttConfig"),
    network = list(cls = NetworkConfig, setup = "setup_NetworkConfig"),
    choropleth = list(cls = ChoroplethConfig, setup = "setup_ChoroplethConfig"),
    heatmap = list(cls = HeatmapConfig, setup = "setup_HeatmapConfig"),
    spectrogram = list(
      cls = SpectrogramConfig,
      setup = "setup_SpectrogramConfig"
    ),
    a3 = list(cls = A3Config, setup = "setup_A3Config")
  )
} # /rtemis.draw::chart_registry


# %% chart_origin ----
#' Record where each of a setup function's values came from
#'
#' Called by every `setup_*`: the arguments the caller named are `"user"`, the
#' rest are `"default"`. Values a run computes from the data are marked
#' `"derived"` later, when they are resolved.
#'
#' @details
#' The distinction is what lets a document survive moving between interfaces
#' with its intent intact. A margin the author set must be honored anywhere; a
#' margin an IDE pane defaulted may be re-resolved for a large web canvas.
#' Without origins a resolved document cannot tell the two apart, and every
#' default hardens into a choice the moment it is written.
#'
#' @param call Call: The `setup_*` call, from `match.call()`.
#' @param names Character: Every property the config declares an origin for.
#'
#' @return Named character vector, one entry per element of `names`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
chart_origin <- function(call, names) {
  supplied <- setdiff(names(as.list(call))[-1L], "")
  stats::setNames(
    ifelse(names %in% supplied, "user", "default"),
    names
  )
} # /rtemis.draw::chart_origin


# %% chart_writer ----
#' Identify this package as the writer of a config
#'
#' Stamped onto every complete document by [write_chart_config()], overwriting
#' any writer already there: the interface that wrote *this* file is the one
#' that produced it, whoever produced the one it was read from.
#'
#' @return Named character vector: `name` and `version`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
chart_writer <- function() {
  c(
    name = "rtemis.draw",
    version = as.character(utils::packageVersion("rtemis.draw"))
  )
} # /rtemis.draw::chart_writer


# %% settable_props ----
#' The properties a config's author and interface set
#'
#' Everything except the discriminator, which is a class constant, and the two
#' provenance maps, which describe the document rather than the chart.
#'
#' @param config [ChartConfig]: The chart configuration.
#'
#' @return Character: Property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
settable_props <- function(config) {
  setdiff(
    names(S7::S7_class(config)@properties),
    c("type", PROVENANCE_PROPS)
  )
} # /rtemis.draw::settable_props


# %% as_output_config ----
#' Turn a config into the output config a complete document records
#'
#' Stamps this package as the writer and checks that the config can honestly
#' claim to be complete.
#'
#' The check is on `origin`: a complete document has to say where every one of
#' its values came from, and only `setup_*()` builds a map that covers every
#' property. A config from a bare constructor has none, and one from an older
#' writer may cover fewer properties than the class now declares -- in both
#' cases writing it as complete would assert a provenance nobody established.
#'
#' @param config [ChartConfig]: The chart configuration.
#'
#' @return [ChartConfig], with `writer` set.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_output_config <- function(config) {
  absent <- setdiff(settable_props(config), names(config@origin))
  if (length(absent) > 0L) {
    abort(
      "A complete config needs an `origin` entry for every property; ",
      length(absent),
      " are missing: ",
      paste(absent, collapse = ", "),
      ". Build the config with setup_",
      S7::S7_class(config)@name,
      "(), which records one per property.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  config@writer <- chart_writer()
  config
} # /rtemis.draw::as_output_config


# %% chart_config_to_list ----
#' Convert a chart config to a plain list
#'
#' @details
#' Two forms, matching the two the schema publishes:
#'
#' - `complete = FALSE` (default) drops unset properties, so an authored config
#'   stays as small as it was authored. `type` is always kept: it carries the
#'   document's shape and is the one key the schema requires.
#' - `complete = TRUE` keeps every property, unset ones included, so they
#'   serialize as explicit nulls -- and stamps the `writer`. A consumer of such
#'   a document can read every key directly instead of reproducing this
#'   package's defaults. Requires a full `origin` map; see [write_chart_config()].
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param complete Logical: If TRUE, emit the output-config form: every
#'   property, plus provenance.
#'
#' @return Named list, ready for [jsonlite::toJSON()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' chart_config_to_list(setup_ScatterConfig(x = "wt", y = "mpg"))
chart_config_to_list <- function(config, complete = FALSE) {
  if (!S7_inherits(config, ChartConfig)) {
    abort(
      "`config` must be a ChartConfig.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  check_logical_scalar(complete)
  if (complete) {
    config <- as_output_config(config)
  }
  values <- S7::props(config)
  if (!complete) {
    values <- values[!vapply(values, is.null, logical(1L))]
  }
  # jsonlite serializes an atomic vector as an array whatever its names, and
  # `auto_unbox` collapses a length-1 one to a scalar. Neither matches what the
  # schema declares, so shape each value by its *declared container* rather than
  # by what it happens to look like:
  #   map   -> a list, so it serializes as a JSON object with its keys
  #   array -> I(), so a one-element array stays an array
  properties <- S7::S7_class(config)@properties
  for (nm in names(values)) {
    # An unset property is a null on the wire, which has no container to shape
    # -- and assigning NULL into a list would delete the key rather than empty
    # it, dropping the very entry `complete` exists to keep.
    if (is.null(values[[nm]])) {
      next
    }
    spec <- prop_spec(properties[[nm]])
    container <- if (is.null(spec)) "none" else spec[["container"]]
    if (identical(container, "map")) {
      values[[nm]] <- as.list(values[[nm]])
    } else if (identical(container, "array")) {
      values[[nm]] <- I(values[[nm]])
    }
  }
  values
} # /rtemis.draw::chart_config_to_list


# %% write_chart_config ----
#' Write a chart config to a JSON file
#'
#' @details
#' `complete = FALSE` (default) writes an **input config**: only the properties
#' that are set. It validates against the chart's `schema.json`.
#'
#' `complete = TRUE` writes an **output config**: every property, unset ones as
#' explicit nulls, with this package stamped as the `writer`. It validates
#' against `record.json`, and is what an interface hands to another interface
#' -- nothing is left for the reader to infer. Resolve the config first, so that
#' the values the data determines are written as the derived facts they are:
#'
#' ```r
#' write_chart_config(resolve(config, data), path, complete = TRUE)
#' ```
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param path Character: Destination file.
#' @param complete Logical: If TRUE, write the output-config form.
#'
#' @return `path`, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".json")
#' write_chart_config(setup_ScatterConfig(x = "wt", y = "mpg"), path)
#'
#' # An output config: resolved against the data, then written in full.
#' cfg <- resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)
#' write_chart_config(cfg, path, complete = TRUE)
#' unlink(path)
write_chart_config <- function(config, path, complete = FALSE) {
  check_character_scalar(path)
  json <- jsonlite::toJSON(
    chart_config_to_list(config, complete = complete),
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
} # /rtemis.draw::write_chart_config


# %% read_chart_config ----
#' Read a chart config from JSON
#'
#' Reconstructs through the chart's `setup_*` function rather than its
#' constructor, so a document from any source -- another interface, a hand-written
#' file, a language model -- arrives resolved through the same seam a direct call
#' goes through.
#'
#' @param x Character: Path to a JSON file, or a JSON string.
#'
#' @return [ChartConfig] subclass object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".json")
#' write_chart_config(setup_ScatterConfig(x = "wt", y = "mpg", fit = "glm"), path)
#' read_chart_config(path)@fit
#' unlink(path)
read_chart_config <- function(x) {
  check_character_scalar(x)
  parsed <- jsonlite::fromJSON(x, simplifyVector = TRUE)
  if (!is.list(parsed) || is.null(parsed[["type"]])) {
    abort(
      "Not a chart config: no `type` key. Every chart document names its ",
      "type, which is what selects the schema that applies.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  type <- parsed[["type"]]
  registry <- chart_registry()
  entry <- registry[[type]]
  if (is.null(entry)) {
    abort(
      "Unknown chart type '",
      type,
      "'. Known types: ",
      paste(names(registry), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  setup <- get(entry[["setup"]], envir = asNamespace("rtemis.draw"))
  # `type` is a constant on the class, and `origin` / `writer` are provenance a
  # reader restores rather than re-resolves, so none is a setup_* argument.
  args <- parsed[setdiff(names(parsed), c("type", PROVENANCE_PROPS))]
  properties <- entry[["cls"]]@properties
  args <- stats::setNames(
    lapply(names(args), function(nm) {
      coerce_to_spec(unlist_scalar(args[[nm]]), prop_spec(properties[[nm]]))
    }),
    names(args)
  )
  config <- do.call(setup, args)
  for (nm in PROVENANCE_PROPS) {
    if (!is.null(parsed[[nm]])) {
      prop(config, nm) <- unlist(parsed[[nm]])
    }
  }
  config
} # /rtemis.draw::read_chart_config


# %% unlist_scalar ----
#' Flatten a parsed JSON value to the vector an S7 property expects
#'
#' `jsonlite` returns a list for a JSON object and for some arrays; the
#' properties are typed vectors. Named lists keep their names, which is what a
#' map property needs.
#'
#' @param x Parsed JSON value.
#'
#' @return Vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
unlist_scalar <- function(x) {
  if (is.list(x)) unlist(x) else x
} # /rtemis.draw::unlist_scalar


# %% coerce_to_spec ----
#' Coerce a parsed JSON value to its property's declared type
#'
#' JSON has one number type, so `[0, 6]` parses back as integer and would land
#' in a `prop_float()` as the wrong storage mode -- accepted by the validator,
#' but no longer identical to what was written. The declared type is the
#' authority, so the reader coerces to it rather than trusting what the parser
#' guessed.
#'
#' @param value Parsed JSON value.
#' @param spec Optional Named list: The property spec.
#'
#' @return `value`, coerced.
#'
#' @author EDG
#' @keywords internal
#' @noRd
coerce_to_spec <- function(value, spec) {
  if (is.null(spec) || is.null(value)) {
    return(value)
  }
  coerced <- switch(
    spec[["type"]],
    integer = as.integer(value),
    number = as.numeric(value),
    string = as.character(value),
    boolean = as.logical(value),
    value
  )
  # A map property's keys are part of its value.
  if (identical(spec[["container"]], "map")) {
    names(coerced) <- names(value)
  }
  coerced
} # /rtemis.draw::coerce_to_spec
