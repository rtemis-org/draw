# draw_panels.R
# spec: draw/first-cran-release#panel-composition

#' Rectangular Panel Layout
#'
#' Package-owned, backend-independent layout settings. Child charts retain
#' separate ECharts options (`EChartsOption` in `src/export/option.ts`); this
#' layout does not merge or renumber their axes, series, or legends.
#' Rows fill from left to right, with equally sized cells. Unused final cells
#' remain empty. Settings serialize as ordinary JSON with [to_list()].
#'
#' @param ncol Integer `[1, Inf)`: Number of columns.
#' @param gap Numeric `[0, Inf)`: Finite gap between cells in pixels.
#' @param padding Numeric `[0, Inf)`: Finite outer padding in pixels.
#' @return PanelLayout: Validated layout settings.
#' @export
#' @examples
#' to_list(setup_PanelLayout(ncol = 2))
PanelLayout <- new_class(
  "PanelLayout",
  package = "rtemis.draw",
  properties = list(
    ncol = prop_integer(
      1L,
      min = 1L,
      description = "Number of equally sized columns."
    ),
    gap = prop_float(
      12,
      min = 0,
      description = "Gap between panels in pixels."
    ),
    padding = prop_float(0, min = 0, description = "Outer padding in pixels.")
  )
)

#' Set Up a Panel Layout
#' @inheritParams PanelLayout
#' @return PanelLayout: Validated settings.
#' @export
#' @examples
#' setup_PanelLayout(ncol = 2, gap = 16)
setup_PanelLayout <- function(ncol = 1L, gap = 12, padding = 0) {
  PanelLayout(ncol = clean_int(ncol), gap = gap, padding = padding)
}
method(to_list, PanelLayout) <- function(x) S7::props(x)

#' JSON Schema for Panel Layout Settings
#'
#' Uses the S7 properties' declarations and omits defaults. This is a layout
#' schema, separate from the chart registry and child chart/data contracts.
#' Resolved layouts emitted by [to_list()] contain every setting.
#' @param complete Logical: Require every setting instead of allowing omissions.
#' @return Named list: Draft 2020-12 JSON Schema.
#' @export
#' @examples
#' panel_layout_schema()[["properties"]][["ncol"]]
panel_layout_schema <- function(complete = FALSE) {
  check_logical_scalar(complete)
  properties <- lapply(PanelLayout@properties, function(p) {
    schema_property(prop_spec(p))
  })
  out <- list(
    `$schema` = JSON_SCHEMA_DIALECT,
    type = "object",
    properties = properties,
    additionalProperties = FALSE
  )
  if (complete) {
    out[["required"]] <- as.list(names(properties))
  }
  out
}

#' Validate and extract an independent chart panel
#' @param x htmlwidget: ECharts drawing.
#' @return List: Rendering payload without widget DOM state.
#' @keywords internal
#' @noRd
panel_payload <- new_generic("panel_payload", "x")
method(panel_payload, class_any) <- function(x) {
  if (!inherits(x, "rtemis-draw") || !is.list(x[["x"]][["option"]])) {
    abort(
      "Supply ECharts drawings from draw_*() to `draw_panels()`; nested layouts and other backends are not yet supported.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (length(x[["jsHooks"]])) {
    abort(
      "Compose drawings before adding widget JavaScript hooks; child hooks cannot be preserved in a panel.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  payload <- x[["x"]]
  payload
}

#' Compose Drawings into One Figure
#'
#' Arrange independent ECharts widgets in a row-major grid. Each panel keeps
#' its own legend, tooltips, zoom, axes, and custom layers. No linked zoom or
#' legend selection is implied. SVG export includes all panels in one vector
#' file and uses the same layout calculation as the browser.
#'
#' Child widget widths and heights are replaced by equally sized layout cells.
#' Fixed-aspect and square-cell heatmap grids fit within both cell dimensions.
#' Dendrograms remain aligned with their heatmap. Give panels
#' enough space for their labels and legends. Explicit child themes are kept;
#' automatic themes follow the browser, and resolve to light for SVG export.
#' Padding and gaps use the common child background when all panels agree;
#' mixed-background figures remain transparent. This also applies to SVG export.
#' Nested layouts, Sigma/MapLibre, and child JavaScript hooks are not supported
#' by this initial composition API.
#'
#' @param plots List: One or more ECharts widgets from draw_*() functions.
#' @param ncol,gap,padding See [PanelLayout].
#' @param layout Optional PanelLayout: Complete layout overriding the three
#'   convenience settings; do not supply both forms.
#' @param width Optional Character or Numeric: Total figure width.
#' @param height Optional Numeric: Total figure height. Unset allocates 360
#'   pixels per row plus padding and gaps.
#' @param element_id Optional Character: Figure DOM identifier.
#' @param filename Optional Character: Export the complete figure with [save_drawing()].
#' @return htmlwidget: A figure containing independent chart panels.
#' @export
#' @examples
#' draw_panels(list(draw_bar(c("A", "B"), c(2, 4)),
#'   draw_boxplot(list(A = 1:5, B = 3:7), boxpoints = "all")), ncol = 2)
draw_panels <- function(
  plots,
  ncol = 1L,
  gap = 12,
  padding = 0,
  layout = NULL,
  width = 900,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  if (!is.list(plots) || inherits(plots, "htmlwidget") || !length(plots)) {
    abort(
      "Supply a nonempty list of drawings to `draw_panels()`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (
    !is.null(layout) && (!missing(ncol) || !missing(gap) || !missing(padding))
  ) {
    abort(
      "Supply either `layout` or ncol/gap/padding settings, not both.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  layout <- layout %||% setup_PanelLayout(ncol, gap, padding)
  if (!S7_inherits(layout, PanelLayout)) {
    abort(
      "Build `layout` with setup_PanelLayout().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  panels <- unname(lapply(plots, panel_payload))
  rows <- ceiling(length(panels) / layout@ncol)
  if (
    !is.null(width) &&
      (length(width) != 1L ||
        anyNA(width) ||
        !(is.numeric(width) || is.character(width)) ||
        is.complex(width) ||
        (is.character(width) && !nzchar(width)))
  ) {
    abort(
      "Supply a numeric pixel width, a nonempty CSS width, or NULL.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  height <- height %||%
    (rows * 360 + (rows - 1) * layout@gap + 2 * layout@padding)
  if (
    !is.numeric(height) ||
      is.complex(height) ||
      length(height) != 1L ||
      !is.finite(height) ||
      height <= 2 * layout@padding + (rows - 1) * layout@gap ||
      (is.numeric(width) &&
        (length(width) != 1L ||
          !is.finite(width) ||
          width <= 2 * layout@padding + (layout@ncol - 1) * layout@gap))
  ) {
    abort(
      "Increase the figure dimensions to leave positive panel space after padding and gaps.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  widget <- render_widget(
    "rtemis-draw",
    list(panels = panels, layout = to_list(layout)),
    theme = NA,
    width = width,
    height = height,
    element_id = element_id
  )
  class(widget) <- c(class(widget), "rtemis-panels")
  if (!is.null(filename)) {
    save_drawing(widget, filename)
  }
  widget
}
