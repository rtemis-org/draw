# build_request.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The render object of a chart whose ECharts option is built in JavaScript by
# the @rtemis/draw builders that rtemislive also uses. The builders ship as
# inst/htmlwidgets/lib/draw/rtemis-draw-builders.js, a build of the draw
# repository's typescript/ package.
# spec: draw/shared-builders#r-widget

# %% BUILDER_TYPES ----
# Chart types with a builder in the bundled @rtemis/draw.
BUILDER_TYPES <- c("a3", "bar", "boxplot", "histogram", "line", "scatter")


# %% BuildRequest ----
#' Request for an ECharts Option Built by @rtemis/draw
#'
#' The object [compile()] returns for a chart whose ECharts option is built in
#' JavaScript by the `@rtemis/draw` builders, which rtemislive also uses.
#' [draw()] sends it to the widget, and [save_drawing()] sends it to Node.js;
#' each calls the builder for `type` with `data`, `config`, and `appearance`.
#'
#' @param type Character \{"a3", "bar", "boxplot", "histogram", "line",
#'   "scatter"\}: Chart type.
#' @param data Character: The chart data as JSON text.
#' @param config Named list: The chart config, an instance of the
#'   `chart/<type>/v1` schema.
#' @param appearance Named list: The builder's appearance input.
#'
#' @return `BuildRequest` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' if (requireNamespace("rtemis.a3", quietly = TRUE)) {
#'   a <- rtemis.a3::create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
#'   request <- compile(setup_A3Config(), data = a)
#'   request@type
#' }
BuildRequest <- S7::new_class(
  "BuildRequest",
  package = "rtemis.draw",
  properties = list(
    type = prop_string(enum = BUILDER_TYPES),
    data = prop_string(),
    config = S7::class_list,
    appearance = S7::class_list
  )
)


# %% draw.BuildRequest ----
S7::method(draw, BuildRequest) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  animation = NULL,
  ...,
  meta = list()
) {
  check_dots_empty(...)
  widget <- render_widget(
    "rtemis-draw",
    c(list(build = build_payload(option, animation)), meta),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id
  )
  if (!is.null(filename)) {
    save_drawing(widget, filename)
  }
  widget
}


# %% build_payload ----
#' The widget payload of a build request
#'
#' Empty lists serialize as JSON objects, which is what the builders take.
#'
#' @param request [BuildRequest].
#' @param animation Optional Logical: Render-time animation setting, which
#'   replaces the request's.
#'
#' @return Named list: `type`, `data`, `config`, and `appearance`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_payload <- function(request, animation = NULL) {
  appearance <- request@appearance
  if (!is.null(animation)) {
    appearance[["animation"]] <- animation
  }
  as_object <- function(x) {
    if (length(x) == 0L) stats::setNames(list(), character(0)) else x
  }
  list(
    type = request@type,
    data = request@data,
    config = as_object(request@config),
    appearance = as_object(appearance)
  )
}


# %% config_members ----
#' The schema members of a chart config, for a builder
#'
#' Every member except the provenance members (`origin`, `writer`) and
#' `dat_path`, which describe where a config and its data came from. A `NULL`
#' member is written as `NA`, which the widget and SVG serializers both write
#' as JSON `null`, so a builder reads it as set to nothing rather than unset.
#'
#' @param config [ChartConfig].
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_members <- function(config) {
  names <- setdiff(
    names(S7::S7_class(config)@properties),
    c(PROVENANCE_PROPS, "dat_path")
  )
  stats::setNames(
    lapply(names, function(name) S7::prop(config, name) %||% NA),
    names
  )
}


# %% tabular_json ----
#' A tabular chart's data, as @rtemis/draw's tabular builders take it
#'
#' The JSON text of `{columns, levels, types, frequency, fits}`: `columns` maps each
#' named column to an array of values, `levels` maps each factor column to its
#' level order, `types` marks each date or date-time column `"time"`, and
#' `frequency` names the column of row counts, or is `null`; `fits`, present
#' when given, lists the fitted curves. Numbers are
#' written as numbers and missing values as `null`. A factor is written as its
#' labels. A date or date-time is written as epoch milliseconds of its
#' wall-clock reading in UTC (`time_axis_ms()`), which the builders place and
#' label in UTC. Any other value is written as text.
#'
#' @param data Data frame or list: The chart data.
#' @param columns Character: Columns to include.
#' @param frequency Optional Character: Column holding the number of rows each
#'   row stands for, written with the columns and named as `frequency`.
#' @param fits Optional List: Fitted curves from `fit_curves()`, written as
#'   `fits`.
#'
#' @return Character: JSON text.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tabular_json <- function(data, columns, frequency = NULL, fits = NULL) {
  columns <- unique(c(columns, frequency))
  as_values <- function(x) {
    if (is.factor(x)) {
      return(as.character(x))
    }
    if (is_time_axis(x)) {
      return(time_axis_ms(x))
    }
    if (is.numeric(x) || is.logical(x) || is.character(x)) {
      return(unclass(x))
    }
    as.character(x)
  }
  values <- stats::setNames(
    lapply(columns, function(name) as_values(data[[name]])),
    columns
  )
  factors <- Filter(function(name) is.factor(data[[name]]), columns)
  levels <- stats::setNames(
    lapply(factors, function(name) levels(data[[name]])),
    factors
  )
  times <- Filter(function(name) is_time_axis(data[[name]]), columns)
  as_object <- function(x) {
    if (length(x)) x else stats::setNames(list(), character(0))
  }
  as.character(jsonlite::toJSON(
    c(
      list(
        columns = values,
        levels = as_object(levels),
        types = as_object(stats::setNames(
          lapply(times, function(name) jsonlite::unbox("time")),
          times
        )),
        frequency = if (!is.null(frequency)) jsonlite::unbox(frequency)
      ),
      if (!is.null(fits)) list(fits = lapply(fits, fit_json))
    ),
    auto_unbox = FALSE,
    na = "null",
    null = "null",
    digits = NA
  ))
}


# %% fit_json ----
#' One fitted curve as the builders take it
#'
#' @param curve Named list: A curve from `fit_curves()`.
#'
#' @return Named list for `jsonlite::toJSON(auto_unbox = FALSE)`: the column
#'   and group as scalars, the coordinates as arrays.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_json <- function(curve) {
  list(
    y = jsonlite::unbox(curve[["y"]]),
    group = if (!is.null(curve[["group"]])) jsonlite::unbox(curve[["group"]]),
    x = curve[["x"]],
    fitted = curve[["fitted"]],
    lower = curve[["lower"]],
    upper = curve[["upper"]],
    rsq = jsonlite::unbox(curve[["rsq"]])
  )
} # /rtemis.draw::fit_json
