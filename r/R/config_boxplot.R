# config_boxplot.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The boxplot's config. Multi-column like bar and line -- one box per bound
# column -- with an optional grouping column that splits each box into one per
# level.
#
# `labels` is the one property here that names *values* rather than columns: it
# overrides the box labels, which otherwise come from the bound column names.

# %% BoxplotConfig ----
#' Boxplot Configuration
#'
#' A serializable description of a boxplot. Build one with
#' [setup_BoxplotConfig()] rather than calling this constructor directly.
#'
#' `x` names **one or more** columns, one box each. `group` optionally names a
#' column that splits every box into one per level.
#'
#' @param x Optional Character: Columns to summarize, one box each.
#' @param group Optional Character: Column that splits each box by level.
#' @param labels Optional Character: Box labels. `NULL` uses the bound column
#'   names.
#' @param horizontal Logical: Draw the boxes horizontally.
#' @param na_rm Logical: Drop `NA` values before summarizing.
#' @param palette Optional Character: Box colors, overriding the theme palette
#'   for this chart. `NULL` uses the theme's.
#' @param fill_alpha Numeric `[0, 1]`: Box fill opacity.
#' @param xlab,ylab Optional Character: Axis labels.
#' @param margin_top,margin_right,margin_bottom,margin_left Optional Integer
#'   `[0, Inf)`: Plot margins in pixels.
#' @inheritParams ChartConfig
#'
#' @return `BoxplotConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_BoxplotConfig(x = c("mpg", "hp"))@type
BoxplotConfig <- new_class(
  name = "BoxplotConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("boxplot"),
    # -- data binding ------------------------------------------------------
    x = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Columns to summarize, one box each."
    ),
    group = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column that splits each box into one per level."
    ),
    # -- semantics ---------------------------------------------------------
    horizontal = prop_boolean(
      FALSE,
      description = "Draw the boxes horizontally."
    ),
    na_rm = prop_boolean(
      TRUE,
      description = "Drop NA values before summarizing."
    ),
    # -- appearance --------------------------------------------------------
    labels = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Box labels. NULL uses the bound column names."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Box colors, overriding the theme palette. NULL uses the theme's."
    ),
    fill_alpha = prop_float(
      0.25,
      min = 0,
      max = 1,
      description = "Box fill opacity."
    ),
    xlab = prop_string(NULL, nullable = TRUE, description = "X axis label."),
    ylab = prop_string(NULL, nullable = TRUE, description = "Y axis label."),
    margin_top = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Top margin in pixels."
    ),
    margin_right = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Right margin in pixels."
    ),
    margin_bottom = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Bottom margin in pixels."
    ),
    margin_left = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Left margin in pixels."
    )
  )
) # /rtemis.draw::BoxplotConfig


# %% BOXPLOT_ORIGIN_NAMES ----
BOXPLOT_ORIGIN_NAMES <- setdiff(
  names(BoxplotConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_BoxplotConfig ----
#' Set up a Boxplot Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams BoxplotConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [BoxplotConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw(setup_BoxplotConfig(x = c("mpg", "hp")), data = mtcars)
setup_BoxplotConfig <- function(
  x = NULL,
  group = NULL,
  labels = NULL,
  horizontal = FALSE,
  na_rm = TRUE,
  palette = NULL,
  fill_alpha = 0.25,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  margin_top = NULL,
  margin_right = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), BOXPLOT_ORIGIN_NAMES)
  BoxplotConfig(
    x = x,
    group = group,
    labels = labels,
    horizontal = horizontal,
    na_rm = na_rm,
    palette = palette,
    fill_alpha = fill_alpha,
    xlab = xlab,
    ylab = ylab,
    title = title,
    margin_top = margin_top,
    margin_right = margin_right,
    margin_bottom = margin_bottom,
    margin_left = margin_left,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_BoxplotConfig


# %% resolve.BoxplotConfig ----
# The box labels are the bound column names, which the builder already derives
# from the named list it is handed -- so nothing is resolved here beyond
# recording that. Axis labels are not derived: a boxplot's category axis is the
# box names and its value axis has no single bound column to name it.
method(resolve, BoxplotConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.BoxplotConfig ----
# The builder takes a named list, one element per box, and uses the names as
# labels -- so the bound column names carry through without a `labels` argument.
method(compile, BoxplotConfig) <- function(config, data = NULL, ...) {
  if (is.null(config@x)) {
    abort(
      "A BoxplotConfig needs `x` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  values <- lapply(config@x, function(column) config_column(data, column, "x"))
  names(values) <- config@x
  boxplot_option(
    x = values,
    labels = config@labels,
    group = config_column(data, config@group, "group"),
    horizontal = config@horizontal,
    palette = config@palette,
    fill_alpha = config@fill_alpha,
    na_rm = config@na_rm,
    xlab = config@xlab,
    ylab = config@ylab,
    title = config@title,
    margins = config_margins(config) %||% DEFAULT_MARGINS
  )
}
