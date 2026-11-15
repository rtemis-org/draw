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
BUILDER_TYPES <- c("a3")


# %% BuildRequest ----
#' Request for an ECharts Option Built by @rtemis/draw
#'
#' The object [compile()] returns for a chart whose ECharts option is built in
#' JavaScript by the `@rtemis/draw` builders, which rtemislive also uses.
#' [draw()] sends it to the widget, and [save_drawing()] sends it to Node.js;
#' each calls the builder for `type` with `data`, `config`, and `appearance`.
#'
#' @param type Character \{"a3"\}: Chart type.
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
