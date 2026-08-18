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
#
# See `plan/draw-schemas.md`.

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
  )
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
#'
#' @return Data frame or named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_data <- function(config, data = NULL) {
  if (!is.null(data)) {
    return(data)
  }
  path <- config@dat_path
  if (is.null(path)) {
    abort(
      "No data: pass `data` to draw(), or set `dat_path` on the config.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (!file.exists(path)) {
    abort(
      "`dat_path` does not exist: ",
      path,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
} # /rtemis.draw::config_data


# %% config_column ----
#' Pull one named column out of a config's data
#'
#' Column names are the config's half of the data binding, so a name that is not
#' in the data is a config error and is reported as one, naming both the missing
#' column and what is available.
#'
#' @param data Data frame or named list: The resolved data.
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
  if (is.null(column)) {
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
#' @param config [ChartConfig]: The chart configuration.
#' @param data Optional Data frame or named list: The data to plot. When `NULL`,
#'   the config's `dat_path` is read instead.
#' @param ... Passed to methods.
#'
#' @return [ChartConfig] subclass object, with every derivable value filled in.
#'
#' @author EDG
#' @export
resolve <- new_generic(
  "resolve",
  "config",
  function(config, data = NULL, ...) {
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
    if (!is.null(origin) && nm %in% names(origin)) {
      origin[[nm]] <- "derived"
    }
  }
  config@origin <- origin
  config
} # /rtemis.draw::config_derive


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
  draw(
    compile(option, data = data),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    animation = animation,
    ...
  )
}
