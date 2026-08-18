# chart_config.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The config layer. A `ChartConfig` says what a chart *is* -- which columns it
# binds, its semantics, its appearance -- in a form that serializes to JSON and
# validates against a published schema. `compile()` turns one into the render IR
# (`EChartsOption` / `SigmaOption` / `MapLibreOption`), which `draw()` then
# renders.
#
#   draw_scatter(x, y, ...)  --------------------------------> option -> widget
#   draw(ScatterConfig, data = df) --compile()--> option -> widget
#
# Two rules decide what is a property here and what is not:
#
# 1. **A config never carries data.** It carries column *names*, plus an
#    optional `dat_path` read at draw time. In-memory data is passed to
#    `draw()`, which is why there is one class per chart rather than a
#    parallel "live" hierarchy.
# 2. **A config carries only what the author supplies.** What the *interface*
#    supplies -- `theme`, `width`, `height`, `element_id`, `filename` -- are
#    arguments to `draw()` and are deliberately not properties. That is what
#    lets one document render correctly in an IDE pane and a web app, adapting
#    its presentation while keeping its meaning.

# %% ORIGIN_VALUES ----
# The three things a value's provenance can be. Named here so the S7 validator,
# the emitted schema's `enum`, and `config_derive()` cannot drift apart.
ORIGIN_VALUES <- c("user", "default", "derived")


# %% PROVENANCE_PROPS ----
# The properties that describe the *document* rather than the chart. They are
# not `setup_*()` values, they carry no origin of their own, and they are the
# two a complete document must state -- so every place that has to treat them
# apart from the chart's own properties reads them from here.
PROVENANCE_PROPS <- c("origin", "writer")


# %% ChartConfig ----
#' Chart Configuration Base Class
#'
#' Abstract base for every chart config. Subclasses override `type` with a
#' constant, which is the discriminator the published schema dispatches on.
#'
#' @param type Character: Chart type. Set by each subclass, not by the user.
#' @param dat_path Optional Character: Path to the data, read at draw time. The
#'   serializable alternative to passing `data` to [draw()].
#' @param title Optional Character: Chart title. Declared here because it is the
#'   one property every chart type has; `palette` is not (four take a `colormap`
#'   instead), and axis labels and margins are cartesian-only.
#' @param origin Optional Named character \{"user", "default", "derived"\}: Where
#'   each value came from, one entry per settable property. Absent on an
#'   authored config; written by the interface that resolved it.
#' @param writer Optional Named character: Which interface wrote the config, as
#'   `name` and `version`. Absent on an authored config.
#'
#' @return `ChartConfig` object.
#'
#' @author EDG
#' @export
ChartConfig <- new_class(
  name = "ChartConfig",
  package = "rtemis.draw",
  abstract = TRUE,
  properties = list(
    type = prop_string(
      NULL,
      nullable = TRUE,
      description = "Chart type; the schema discriminator."
    ),
    dat_path = prop_string(
      NULL,
      nullable = TRUE,
      description = paste(
        "Path to the data to plot, read at draw time. NULL means the data is",
        "supplied to draw() instead."
      )
    ),
    title = prop_string(
      NULL,
      nullable = TRUE,
      description = "Chart title."
    ),
    # Provenance. Absent on an authored config, present on one an interface
    # wrote out -- and a complete `origin` map is what makes "this document is
    # complete" checkable rather than self-reported, since it needs an entry
    # per property.
    origin = prop_string(
      NULL,
      enum = ORIGIN_VALUES,
      nullable = TRUE,
      map = TRUE,
      description = paste(
        "Where each value came from: 'user' if the author set it, 'default'",
        "if the interface filled it in, 'derived' if it was computed from the",
        "data. Carried through a re-write, never recomputed: a value the",
        "author chose must stay honored, while a defaulted one may be",
        "re-resolved for a different display."
      )
    ),
    writer = prop_string(
      NULL,
      nullable = TRUE,
      map = TRUE,
      description = paste(
        "Which interface wrote this document, as `name` and `version`.",
        "Absent on an authored config."
      )
    )
  ),
  # The published schema states both maps as closed objects, so the class holds
  # to the same shape: an origin naming a property the chart does not have, or a
  # writer with a key that is not `name` or `version`, is a mistake worth
  # catching where it is made rather than at the far end of a round trip.
  validator = function(self) {
    origin_extra <- setdiff(names(self@origin), settable_props(self))
    writer_extra <- setdiff(names(self@writer), c("name", "version"))
    c(
      if (length(origin_extra) > 0L) {
        paste0(
          "@origin names properties this chart does not have: ",
          paste(origin_extra, collapse = ", "),
          "."
        )
      },
      if (!is.null(self@writer) && length(writer_extra) > 0L) {
        paste0(
          "@writer takes `name` and `version`; got: ",
          paste(writer_extra, collapse = ", "),
          "."
        )
      }
    )
  }
) # /rtemis.draw::ChartConfig


# %% prop_chart_type ----
#' Constant chart-type discriminator
#'
#' Each subclass overrides the inherited `type` with a computed constant, so the
#' value is always correct and is never stored on the instance. The emitted
#' schema's `const` is read back off the getter, so the property carries no spec
#' and is excluded from the leaf's own properties.
#'
#' @param type Character: The chart type.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_chart_type <- function(type) {
  force(type)
  new_property(class_character, getter = function(self) type)
} # /rtemis.draw::prop_chart_type


# %% compile ----
#' Compile a chart config into a render option
#'
#' Turns a [ChartConfig] into the backend option object `draw()` renders --
#' an [EChartsOption] for ECharts charts. This is where a config's column names
#' are resolved against actual data.
#'
#' @details
#' The generic does the two steps every chart needs before its own translation
#' can start, so that no method can forget one: it materializes the data (from
#' `data`, or from the config's `dat_path`) and [resolve()]s the config against
#' it. Methods therefore always receive a **resolved** config and **non-NULL**
#' data, and are pure translation.
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param data Optional Data frame or named list: The data to plot. When `NULL`,
#'   the config's `dat_path` is read instead.
#' @param ... Passed to methods.
#'
#' @return Render option object, e.g. [EChartsOption].
#'
#' @author EDG
#' @export
compile <- new_generic(
  "compile",
  "config",
  function(config, data = NULL, ...) {
    # Rebinding before S7_dispatch() is what makes "resolved config, real data"
    # an invariant of the generic rather than a convention each method has to
    # remember. `resolve()` is idempotent, so a builder shared with draw() may
    # resolve again without consequence.
    data <- config_data(config, data)
    config <- resolve(config, data = data)
    S7_dispatch()
  }
)


# %% config_data ----
#' Resolve a chart config's data
#'
#' A config names its columns; the values come from the caller at draw time or
#' from `dat_path`. This resolves the two into one table.
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param data Optional Data frame or named list: Data supplied by the caller.
#' @param required Logical: If TRUE, having no data at all is an error. FALSE
#'   returns `NULL` instead, which is what [resolve()] wants: a config with no
#'   data can still derive labels from the column names it states.
#'
#' @return Data frame, matrix, or other object read from `dat_path`; `NULL` when
#'   there is no data and `required` is FALSE.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_data <- function(config, data = NULL, required = TRUE) {
  if (!is.null(data)) {
    return(data)
  }
  path <- config@dat_path
  if (is.null(path)) {
    if (!required) {
      return(NULL)
    }
    abort(
      "No data: pass `data` to draw(), or set `dat_path` on the config.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  read_dat(path)
} # /rtemis.draw::config_data


# %% read_dat ----
#' Read a config's data file
#'
#' Dispatches on the file extension, because a config's binding is not always a
#' table: a heatmap binds a matrix, a network an adjacency matrix, an annotated
#' protein diagram an `A3` object. CSV covers the tabular charts; RDS covers
#' every one of them, since it round-trips any R object exactly.
#'
#' CSV is read with `check.names = FALSE`. R's default would rewrite
#' `"Bill Length"` to `"Bill.Length"`, and a config names its columns as they
#' are written -- so the default would make a config unable to find the column
#' it names.
#'
#' @param path Character: Path to the data file.
#'
#' @return The object read from `path`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_dat <- function(path) {
  if (!file.exists(path)) {
    abort(
      "`dat_path` does not exist: ",
      path,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  switch(
    tolower(tools::file_ext(path)),
    csv = utils::read.csv(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    rds = readRDS(path),
    abort(
      "`dat_path` must name a .csv or .rds file; got '",
      basename(path),
      "'. Use .rds for data that is not a table, such as a heatmap matrix or ",
      "a network adjacency matrix.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
} # /rtemis.draw::read_dat


# %% config_column ----
#' Pull one named column out of a config's data
#'
#' Column names are the config's half of the data binding, so a name that is not
#' in the data is a config error and is reported as one, naming both the missing
#' column and what is available.
#'
#' @param data Optional Data frame or named list: The resolved data. `NULL`
#'   returns `NULL`, which is what [resolve()] wants: with no data there are no
#'   values to derive from, and that is not an error. `compile()` guarantees its
#'   methods non-NULL data, so the lookup below is never skipped where a missing
#'   column would matter.
#' @param column Optional Character: Column name; `NULL` returns `NULL`.
#' @param argument Character: The config property being resolved, for the error
#'   message.
#'
#' @return Vector, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_column <- function(data, column, argument) {
  if (is.null(column) || is.null(data)) {
    return(NULL)
  }
  if (!column %in% names(data)) {
    abort(
      "`",
      argument,
      "` names column '",
      column,
      "', which is not in the data. Available: ",
      paste(names(data), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  data[[column]]
} # /rtemis.draw::config_column


# %% config_margins ----
#' Assemble a margins vector from a config's per-side properties
#'
#' `draw_*()` takes `margins` as one named vector because that is convenient to
#' type. A config declares four scalars instead, because that is what states
#' cleanly in a schema. This converts, dropping unset sides so the chart's own
#' auto-layout still applies to them.
#'
#' @param config [ChartConfig]: The chart configuration.
#'
#' @return Named numeric vector, or `NULL` when no side is set.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_margins <- function(config) {
  sides <- c("top", "right", "bottom", "left")
  values <- vapply(
    sides,
    function(side) {
      v <- prop(config, paste0("margin_", side))
      if (is.null(v)) NA_real_ else as.numeric(v)
    },
    numeric(1L)
  )
  if (all(is.na(values))) {
    return(NULL)
  }
  values[!is.na(values)]
} # /rtemis.draw::config_margins


# %% resolve ----
#' Resolve a partial chart config into a complete one
#'
#' The middle step of the pipeline: **partial config -> complete config ->
#' render**. Fills in every value that can be derived from the data, and stamps
#' each one `"derived"` in the config's `origin` map.
#'
#' @details
#' Resolution is its own step rather than a set of fallbacks inside the option
#' builders, and that is what makes an output config honest. A value derived
#' during rendering would have to be *reconstructed* to write it down; a value
#' derived here is simply part of the document that then gets rendered. Reading
#' a complete config and rendering it become the same operation.
#'
#' Two things are deliberately **not** resolved:
#'
#' - **Anything describing the display surface** -- width, height, aspect ratio,
#'   container geometry. Those belong to the interface, are recomputed by each
#'   one, and are never written into a document: a height computed for an IDE
#'   pane is wrong in a large web canvas.
#' - **Anything with nothing to derive from.** Labels come from column names, so
#'   a config that names no columns gets no labels.
#'
#' Resolving is **idempotent** and never overwrites a value that is already
#' set, so resolving twice is the same as resolving once. [compile()] relies on
#' that: it resolves up front, and a builder it shares with [draw()] may resolve
#' again without changing the result.
#'
#' Data is optional. With none available, the values that need it are simply not
#' derived; the ones that come from the column names still are. That makes
#' `resolve()` total -- it has no failing input -- so a caller can always ask for
#' the most complete config obtainable from what it has.
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param data Optional Data frame or named list: The data to plot. When `NULL`,
#'   the config's `dat_path` is read if it has one.
#' @param ... Passed to methods.
#'
#' @return [ChartConfig] subclass object, with every derivable value filled in.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Axis limits come from the data; the labels come from the column names.
#' resolve(setup_ScatterConfig(x = "wt", y = "mpg"), data = mtcars)@xlim
resolve <- new_generic(
  "resolve",
  "config",
  function(config, data = NULL, ...) {
    # Materialized once here so that no method has to, and `required = FALSE`
    # so that having no data is a smaller resolution rather than an error.
    data <- config_data(config, data, required = FALSE)
    S7_dispatch()
  }
)


# %% config_derive ----
#' Fill in unset config properties and mark them derived
#'
#' Sets each named property only if it is currently unset, so an author's
#' choice is never overwritten, and records `"derived"` for the ones actually
#' filled. Values that resolve to NULL are left alone: there was nothing to
#' derive from, which is not the same as having derived a null.
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param values Named list: Candidate values, one per property name.
#'
#' @return [ChartConfig], updated.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_derive <- function(config, values) {
  origin <- config@origin
  for (nm in names(values)) {
    if (!is.null(prop(config, nm)) || is.null(values[[nm]])) {
      next
    }
    prop(config, nm) <- values[[nm]]
    # A config built by its bare constructor carries no origin map -- provenance
    # is what `setup_*()` adds -- so there is nothing to stamp.
    if (!is.null(origin) && nm %in% names(origin)) {
      origin[[nm]] <- "derived"
    }
  }
  config@origin <- origin
  config
} # /rtemis.draw::config_derive


# %% render_meta ----
#' Render hints a config's chart needs but its document does not carry
#'
#' Some charts need the browser to solve part of their geometry, because it
#' depends on the container width and nothing on this side knows that. Those
#' hints are derived from the compiled option at draw time and passed as `meta`;
#' they are never written into a document, because a box solved for an IDE pane
#' is the wrong box for a large web canvas.
#'
#' Most charts have none, so the base method returns nothing and a chart type
#' opts in by overriding.
#'
#' @param config [ChartConfig]: The chart configuration.
#' @param option The compiled render option.
#'
#' @return Named list, empty when the chart needs no hints.
#'
#' @author EDG
#' @keywords internal
#' @noRd
render_meta <- new_generic(
  "render_meta",
  "config",
  function(config, option) {
    S7_dispatch()
  }
)

method(render_meta, ChartConfig) <- function(config, option) {
  list()
}


# %% draw.ChartConfig ----
# Every config renders the same way: compile to the backend option, then draw
# it. The render targets are arguments here, never properties of the config.
method(draw, ChartConfig) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  data = NULL
) {
  built <- compile(option, data = data)
  meta <- render_meta(option, built)
  # `meta` is the ECharts binding's channel; the Sigma and MapLibre methods have
  # no formal for it. Forwarding an empty one anyway would make draw() an error
  # on every config those backends serve -- while forwarding a *non-empty* one
  # to a backend that cannot carry it should be exactly that, rather than hints
  # silently going nowhere. So it travels only when there is something to send.
  if (length(meta) > 0L) {
    return(draw(
      built,
      theme = theme,
      width = width,
      height = height,
      element_id = element_id,
      filename = filename,
      animation = animation,
      meta = meta,
      ...
    ))
  }
  draw(
    built,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    ...
  )
}
