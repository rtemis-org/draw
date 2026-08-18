# config_pie.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The pie chart's config. The first non-cartesian one: no axes, so no axis
# labels and no margins -- which is why those live on the leaves rather than on
# `ChartConfig`.

# %% PieConfig ----
#' Pie Chart Configuration
#'
#' A serializable description of a pie chart. Build one with
#' [setup_PieConfig()] rather than calling this constructor directly.
#'
#' @param values Optional Character: Column holding the slice values.
#' @param labels Optional Character: Column holding the slice labels.
#' @param radius Character: Outer radius, as a CSS length or percentage.
#' @param rose_type Optional Character \{"radius", "area"\}: Draw as a
#'   Nightingale rose chart, sizing slices by radius or by area. `NULL` draws a
#'   plain pie.
#' @param palette Optional Character: Slice colors, overriding the theme
#'   palette for this chart. `NULL` uses the theme's.
#' @inheritParams ChartConfig
#'
#' @return `PieConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_PieConfig(values = "n", labels = "kind")@type
PieConfig <- new_class(
  name = "PieConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("pie"),
    # -- data binding ------------------------------------------------------
    values = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding the slice values."
    ),
    labels = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding the slice labels."
    ),
    # -- semantics ---------------------------------------------------------
    rose_type = prop_string(
      NULL,
      enum = c("radius", "area"),
      nullable = TRUE,
      description = "Draw as a Nightingale rose chart. NULL draws a plain pie."
    ),
    # -- appearance --------------------------------------------------------
    radius = prop_string(
      "75%",
      description = "Outer radius, as a CSS length or percentage."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Slice colors, overriding the theme palette. NULL uses the theme's."
    )
  )
) # /rtemis.draw::PieConfig


# %% PIE_ORIGIN_NAMES ----
PIE_ORIGIN_NAMES <- setdiff(
  names(PieConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_PieConfig ----
#' Set up a Pie Chart Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams PieConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [PieConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' counts <- data.frame(kind = c("a", "b", "c"), n = c(3, 5, 2))
#' draw(setup_PieConfig(values = "n", labels = "kind"), data = counts)
setup_PieConfig <- function(
  values = NULL,
  labels = NULL,
  radius = "75%",
  rose_type = NULL,
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), PIE_ORIGIN_NAMES)
  PieConfig(
    values = values,
    labels = labels,
    radius = radius,
    rose_type = rose_type,
    palette = palette,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_PieConfig


# %% resolve.PieConfig ----
# Nothing to derive: a pie has no axes to label and no limits to compute. The
# method exists so every config type resolves through the same call, rather than
# `draw()` having to know which types have one.
method(resolve, PieConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.PieConfig ----
method(compile, PieConfig) <- function(config, data = NULL, ...) {
  values <- config_column(data, config@values, "values")
  labels <- config_column(data, config@labels, "labels")
  if (is.null(values) || is.null(labels)) {
    abort(
      "A PieConfig needs both `values` and `labels` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  pie_option(
    values = values,
    labels = labels,
    radius = config@radius,
    rose_type = config@rose_type,
    palette = config@palette,
    title = config@title
  )
}
