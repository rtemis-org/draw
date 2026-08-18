# map.R
# Choropleth maps rendered with MapLibre GL (not ECharts).
#
# This is the third rendering backend in rtemis.draw, after ECharts and
# Sigma.js. It mirrors rtemislive's MapCanvas (~/Code/live/src/components/chart/
# MapCanvas.tsx) together with its scale (choroplethScale.ts) and location
# resolver (locationResolver.ts). The S7 classes below are the R analog of the
# TypeScript interfaces MapRow / MapModel in ~/Code/live/src/lib/types.ts.
#
# Pipeline:
#   draw_choropleth(x, location, value)      # data frame -> MapModel (S7)
#     -> map_from_data_frame()
#     -> draw_map(model, ...)                # MapModel -> MapLibreOption (S7)
#     -> draw(MapLibreOption)                # dispatch -> htmlwidget ("rtemis-map")
#     -> rtemis-map.js                       # maplibre-gl renders in the browser
#
# Geometry: admin boundaries are vendored as TopoJSON in
# inst/htmlwidgets/lib/geo/ and embedded into the widget payload at render time
# (htmlwidgets has no runtime fetch endpoint). The location -> geometry join,
# classification scale, and FIPS/ISO key normalization all live in the JS
# binding, where the geometry's id set is available.

# Enumerations shared by the constructors and the Tier 1 builder.
map_classifications <- c("quantile", "equal", "jenks")
map_color_schemes <- c(
  "blues",
  "viridis",
  "ylorrd",
  "greens",
  "magma",
  "rdbu",
  "rdylgn",
  "spectral",
  "brbg"
)
map_resolutions <- c("country", "state", "county")
map_corners <- c("top-left", "top-right", "bottom-left", "bottom-right")

# -- MapRow ---------------------------------------------------------------------

#' Map Row
#'
#' One region's datum in a choropleth: a raw location key, the numeric value
#' that colors the region, and optional extra fields to surface in the tooltip.
#' The R analog of the `MapRow` TypeScript interface in rtemislive
#' (`~/Code/live/src/lib/types.ts`).
#'
#' @param location Character: Raw location key (FIPS / ISO / name) before
#'   normalization. Normalization to the canonical geometry id happens in the
#'   renderer.
#' @param value Numeric: The value that colors the region.
#' @param extras Optional named list: Extra column values to show in the
#'   tooltip, keyed by column name.
#' @export
MapRow <- S7::new_class(
  "MapRow",
  properties = list(
    location = character_scalar,
    value = S7::new_property(
      S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1L) {
          return("must be a single number")
        }
        NULL
      }
    ),
    extras = S7::new_property(
      class = S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (!is.list(value) || is.null(names(value))) {
          return("must be NULL or a named list")
        }
        NULL
      }
    )
  )
)

S7::method(to_list, MapRow) <- function(x, ...) {
  out <- list(location = x@location, value = x@value)
  if (!is.null(x@extras)) {
    out[["extras"]] <- x@extras
  }
  out
}

# -- MapModel -------------------------------------------------------------------

#' Map Model
#'
#' A complete, renderer-agnostic choropleth dataset: a list of [MapRow] objects,
#' the administrative `resolution` that selects the geometry, the label for the
#' value column, and the ordered tooltip field names. The R analog of the
#' `MapModel` TypeScript interface in rtemislive (`~/Code/live/src/lib/types.ts`),
#' consumed by the `rtemis-map` htmlwidget.
#'
#' Most users build this implicitly through [draw_choropleth()]; the constructor
#' is exported for power users who assemble rows directly.
#'
#' @param rows List: List of [MapRow] objects.
#' @param resolution Character \{"country", "state", "county"\}: Administrative
#'   resolution. `"country"` joins on ISO-A3, `"state"` on 2-digit FIPS,
#'   `"county"` on 5-digit FIPS.
#' @param value_label Character: Display label for the value column (labels the
#'   legend and tooltip value).
#' @param tooltip_fields Character: Ordered names of the extra fields to show in
#'   the tooltip (keys into each [MapRow]'s `extras`).
#' @export
MapModel <- S7::new_class(
  "MapModel",
  properties = list(
    rows = S7::new_property(class = S7::class_list, default = list()),
    resolution = S7::new_property(
      S7::class_character,
      default = "country",
      validator = function(value) {
        if (length(value) != 1L || !value %in% map_resolutions) {
          return(paste0(
            "must be one of ",
            paste0("\"", map_resolutions, "\"", collapse = ", ")
          ))
        }
        NULL
      }
    ),
    value_label = S7::new_property(S7::class_character, default = "value"),
    tooltip_fields = S7::new_property(
      S7::class_character,
      default = character(0)
    )
  ),
  validator = function(self) {
    if (
      length(self@rows) > 0L &&
        !all(vapply(
          self@rows,
          function(r) S7::S7_inherits(r, MapRow),
          logical(1)
        ))
    ) {
      return("`rows` must be a list of MapRow objects")
    }
    NULL
  }
)

S7::method(to_list, MapModel) <- function(x, ...) {
  list(
    rows = unname(lapply(x@rows, to_list)),
    resolution = x@resolution,
    valueLabel = x@value_label,
    # as.list() forces a JSON array even for length 0 / 1 (auto_unbox would
    # otherwise drop a single field to a scalar, which the JS expects as array).
    tooltipFields = as.list(x@tooltip_fields)
  )
}

# -- MapLibreOption -------------------------------------------------------------

#' MapLibre Render Option
#'
#' The complete, validated render spec for a MapLibre choropleth: a [MapModel]
#' (the data) plus all visual styling and an optional title. This is the
#' MapLibre analog of [EChartsOption] / [SigmaOption] -- the single object
#' [draw()] dispatches on to emit a `rtemis-map` widget. Theme is *not* a
#' property here; like every backend, theming is supplied to [draw()] and
#' resolved uniformly.
#'
#' Most users never touch this directly -- [draw_choropleth()] and [draw_map()]
#' build it -- but power users can construct it for full control:
#' `draw(MapLibreOption(model = MapModel(...), colormap = "viridis"))`.
#'
#' Its [to_list()] produces the `{ model, style, title }` payload consumed by
#' the `rtemis-map` htmlwidget binding (the geometry is added by the [draw()]
#' method, since it depends only on the model's resolution).
#'
#' @param model [MapModel] or named list: The choropleth data (a list must
#'   contain a `rows` element).
#' @param classification Character \{"quantile", "equal", "jenks"\}: Class-break
#'   method. `"quantile"` (equal counts), `"equal"` (equal intervals), or
#'   `"jenks"` (natural breaks).
#' @param colormap Character: Color ramp. One of the sequential schemes
#'   `"blues"`, `"viridis"`, `"ylorrd"`, `"greens"`, `"magma"`, or the diverging
#'   schemes `"rdbu"`, `"rdylgn"`, `"spectral"`, `"brbg"`.
#' @param num_classes Numeric \[2, 12\]: Number of color classes.
#' @param opacity Numeric \[0, 1\]: Region fill opacity.
#' @param show_boundaries Logical: Whether to draw region outlines.
#' @param outline_width Numeric \[0, Inf): Region outline width in pixels.
#' @param show_legend Logical: Whether to show the legend.
#' @param legend_position Character \{"top-left", "top-right", "bottom-left",
#'   "bottom-right"\}: Legend corner.
#' @param tooltip_position Character \{"top-left", "top-right", "bottom-left",
#'   "bottom-right"\}: Hover tooltip corner.
#' @param report_position Character \{"top-left", "top-right", "bottom-left",
#'   "bottom-right"\}: Join-report corner (shows matched / unmatched counts).
#' @param title Optional Character: Title (currently surfaced via the value
#'   label; reserved for future use).
#' @export
MapLibreOption <- S7::new_class(
  "MapLibreOption",
  properties = list(
    model = S7::new_property(
      class = S7::class_any,
      validator = function(value) {
        if (S7::S7_inherits(value, MapModel)) {
          return(NULL)
        }
        if (is.list(value) && !is.null(value[["rows"]])) {
          return(NULL)
        }
        "must be a MapModel or a list with a `rows` element"
      }
    ),
    classification = map_enum_default(map_classifications, "quantile"),
    colormap = map_enum_default(map_color_schemes, "blues"),
    num_classes = S7::new_property(
      S7::class_numeric,
      default = 5,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 2 || value > 12) {
          return("must be a single number in [2, 12]")
        }
        NULL
      }
    ),
    opacity = prob_default(1),
    show_boundaries = logical_default(TRUE),
    outline_width = nonneg_numeric_default(0.2),
    show_legend = logical_default(TRUE),
    legend_position = map_enum_default(map_corners, "bottom-right"),
    tooltip_position = map_enum_default(map_corners, "top-right"),
    report_position = map_enum_default(map_corners, "bottom-left"),
    title = optional_character_scalar
  )
)

S7::method(to_list, MapLibreOption) <- function(x, ...) {
  model <- x@model
  model_list <- if (S7::S7_inherits(model)) to_list(model) else model
  out <- list(
    model = model_list,
    style = list(
      classification = x@classification,
      colorScheme = x@colormap,
      numClasses = x@num_classes,
      opacity = x@opacity,
      showBoundaries = x@show_boundaries,
      outlineWidth = x@outline_width,
      showLegend = x@show_legend,
      legendPosition = x@legend_position,
      tooltipPosition = x@tooltip_position,
      reportPosition = x@report_position
    )
  )
  if (!is.null(x@title)) {
    out[["title"]] <- x@title
  }
  out
}

# -- Geometry loader ------------------------------------------------------------

#' Load a vendored TopoJSON geometry for a resolution
#'
#' Reads the admin-boundary TopoJSON shipped in `inst/htmlwidgets/lib/geo/` for
#' the given resolution and returns it (as a raw JSON string) plus the camera /
#' object metadata the renderer needs. Mirrors `GEO_SOURCES` in rtemislive's
#' `choroplethGeo.ts`.
#'
#' @param resolution Character \{"country", "state", "county"\}: Resolution.
#' @return Named list: `topojson` (string), `object` (string), `center`
#'   (numeric length 2), `zoom` (number).
#' @keywords internal
#' @noRd
load_map_geometry <- function(resolution) {
  sources <- list(
    country = list(
      file = "countries.topo.json",
      object = "countries",
      center = c(0, 20),
      zoom = 0.4
    ),
    state = list(
      file = "us-10m.topo.json",
      object = "states",
      center = c(-96, 38),
      zoom = 2.6
    ),
    county = list(
      file = "us-10m.topo.json",
      object = "counties",
      center = c(-96, 38),
      zoom = 2.6
    )
  )
  src <- sources[[resolution]]
  if (is.null(src)) {
    abort(
      "Unknown map resolution '",
      resolution,
      "'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  path <- system.file(
    "htmlwidgets",
    "lib",
    "geo",
    src[["file"]],
    package = "rtemis.draw"
  )
  if (!nzchar(path) || !file.exists(path)) {
    abort(
      "Vendored geometry '",
      src[["file"]],
      "' not found in the package.",
      class = "rtemis_io_error"
    )
  }
  # Read the whole file as one JSON string; file size in bytes is an upper bound
  # on the character count, so this reads the entire file.
  topojson <- readChar(path, file.info(path)[["size"]], useBytes = TRUE)
  Encoding(topojson) <- "UTF-8"
  list(
    topojson = topojson,
    object = src[["object"]],
    center = src[["center"]],
    zoom = src[["zoom"]]
  )
}

# -- draw() method: MapLibre backend --------------------------------------------

# MapLibre backend: embed the resolution's geometry, then render the choropleth
# spec as a `rtemis-map` widget.
S7::method(draw, MapLibreOption) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  # Accepted to match the draw() generic; this backend has no ECharts
  # animation to disable.
  animation = NULL,
  ...
) {
  if (!is.null(filename)) {
    warn(
      "Static export of map widgets is not yet supported; ignoring `filename`."
    )
  }

  payload <- to_list(option)
  resolution <- payload[["model"]][["resolution"]] %||% "country"
  payload[["geo"]] <- load_map_geometry(resolution)

  render_widget(
    "rtemis-map",
    payload,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id
  )
}

# -- Model builder --------------------------------------------------------------

#' Build a MapModel from a data frame
#'
#' @param data Data frame: One row per region.
#' @param location Character: Name of the column holding the location key.
#' @param value Character: Name of the numeric column to color by.
#' @param resolution Character \{"country", "state", "county"\}: Administrative
#'   resolution.
#' @param tooltip Optional Character: Names of extra columns to show in the
#'   tooltip, in order.
#' @param value_label Optional Character: Label for the value column; defaults
#'   to `value`.
#' @return [MapModel].
#' @keywords internal
#' @noRd
map_from_data_frame <- function(
  data,
  location,
  value,
  resolution,
  tooltip = NULL,
  value_label = NULL
) {
  if (!is.data.frame(data)) {
    abort(
      "`data` must be a data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.character(location) || length(location) != 1L) {
    abort(
      "`location` must be a single column name.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.character(value) || length(value) != 1L) {
    abort(
      "`value` must be a single column name.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  missing_cols <- setdiff(c(location, value, tooltip), names(data))
  if (length(missing_cols) > 0L) {
    abort(
      "Columns not found in `data`: ",
      paste(missing_cols, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  loc <- as.character(data[[location]])
  val <- as.numeric(data[[value]])
  tooltip <- tooltip %||% character(0)

  # Extract tooltip columns once (coercing factors) rather than subsetting the
  # data frame per row inside the loop.
  tooltip_list <- lapply(data[tooltip], function(v) {
    if (is.factor(v)) as.character(v) else v
  })

  rows <- lapply(seq_len(nrow(data)), function(i) {
    extras <- if (length(tooltip) > 0L) {
      lapply(tooltip_list, `[[`, i)
    } else {
      NULL
    }
    MapRow(location = loc[i], value = val[i], extras = extras)
  })

  MapModel(
    rows = rows,
    resolution = resolution,
    value_label = value_label %||% value,
    tooltip_fields = tooltip
  )
}

# -- Widget builder: draw_map ---------------------------------------------------

#' Build the render option for choropleth map
#'
#' The single implementation shared by [draw_map()], which resolves its arguments
#' directly, and `compile()` on the corresponding [ChartConfig], which resolves
#' them from a config. The render targets stay with the caller.
#'
#' @inheritParams draw_map
#'
#' @return [MapLibreOption]: The option object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
map_option <- function(
  model,
  classification = "quantile",
  colormap = "blues",
  num_classes = 5,
  opacity = 1,
  show_boundaries = TRUE,
  outline_width = 0.2,
  show_legend = TRUE,
  legend_position = "bottom-right",
  tooltip_position = "top-right",
  report_position = "bottom-left",
  title = NULL
) {
  classification <- match.arg(classification, map_classifications)
  colormap <- match.arg(colormap, map_color_schemes)
  legend_position <- match.arg(legend_position, map_corners)
  tooltip_position <- match.arg(tooltip_position, map_corners)
  report_position <- match.arg(report_position, map_corners)

  option <- MapLibreOption(
    model = model,
    classification = classification,
    colormap = colormap,
    num_classes = num_classes,
    opacity = opacity,
    show_boundaries = show_boundaries,
    outline_width = outline_width,
    show_legend = show_legend,
    legend_position = legend_position,
    tooltip_position = tooltip_position,
    report_position = report_position,
    title = title
  )

  option
} # /rtemis.draw::map_option


#' Render a MapModel as a MapLibre choropleth htmlwidget
#'
#' Mid-level builder: takes a [MapModel] (or a plain list with `rows`,
#' `resolution`, ...) plus styling, assembles a [MapLibreOption], and dispatches
#' through [draw()]. Most users call [draw_choropleth()] instead.
#'
#' @param model [MapModel] or named list: The choropleth data to render.
#' @inheritParams MapLibreOption
#' @param theme Optional [Theme], list, or `NA`: Theme override. `NULL` enables
#'   light/dark auto-detection (matching [draw()]).
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit element ID.
#' @param filename Optional Character: Currently ignored with a warning (static
#'   export of map widgets is not yet supported); accepted for signature parity
#'   with the other `draw_*` functions.
#' @return htmlwidget.
#' @export
draw_map <- function(
  model,
  classification = "quantile",
  colormap = "blues",
  num_classes = 5,
  opacity = 1,
  show_boundaries = TRUE,
  outline_width = 0.2,
  show_legend = TRUE,
  legend_position = "bottom-right",
  tooltip_position = "top-right",
  report_position = "bottom-left",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  option <- map_option(
    model = model,
    classification = classification,
    colormap = colormap,
    num_classes = num_classes,
    opacity = opacity,
    show_boundaries = show_boundaries,
    outline_width = outline_width,
    show_legend = show_legend,
    legend_position = legend_position,
    tooltip_position = tooltip_position,
    report_position = report_position,
    title = title
  )

  draw(
    option,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}

# -- Tier 1: draw_choropleth ----------------------------------------------------

#' Draw a Choropleth Map
#'
#' Render a choropleth map with MapLibre GL: regions shaded by a value, joined
#' to vendored administrative boundaries (countries by ISO-A3, US states /
#' counties by FIPS). No basemap tiles -- boundaries render on the themed
#' background, with light/dark auto-detection like the rest of `draw_*`.
#'
#' The `location` column is matched to the geometry in the renderer, which
#' accepts ISO-A3 / ISO-A2 / country code for countries, and FIPS / postal
#' abbreviation / full name for US states (5-digit FIPS for counties). A
#' join report in the corner surfaces any unmatched keys.
#'
#' @param x Data frame: One row per region.
#' @param location Character: Name of the column holding the location key
#'   (FIPS / ISO / name).
#' @param value Character: Name of the numeric column to color by.
#' @param resolution Character \{"country", "state", "county"\}: Administrative
#'   resolution selecting the geometry and the join key.
#' @param tooltip Optional Character: Names of extra columns to show in the
#'   hover tooltip, in order.
#' @param value_label Optional Character: Label for the value column in the
#'   legend / tooltip; defaults to `value`.
#' @inheritParams draw_map
#' @return htmlwidget.
#' @examples
#' \dontrun{
#' # Country choropleth from ISO-A3 codes
#' df <- data.frame(
#'   iso = c("USA", "CAN", "MEX", "BRA", "FRA"),
#'   gdp = c(25.5, 2.1, 1.4, 1.9, 2.9)
#' )
#' draw_choropleth(df, location = "iso", value = "gdp")
#'
#' # US states by postal abbreviation, natural-breaks classification
#' states <- data.frame(
#'   st = state.abb,
#'   pop = as.numeric(state.x77[, "Population"])
#' )
#' draw_choropleth(
#'   states,
#'   location = "st",
#'   value = "pop",
#'   resolution = "state",
#'   classification = "jenks",
#'   colormap = "viridis"
#' )
#' }
#' @export
draw_choropleth <- function(
  x,
  location,
  value,
  resolution = c("country", "state", "county"),
  tooltip = NULL,
  value_label = NULL,
  classification = "quantile",
  colormap = "blues",
  num_classes = 5,
  opacity = 1,
  show_boundaries = TRUE,
  outline_width = 0.2,
  show_legend = TRUE,
  legend_position = "bottom-right",
  tooltip_position = "top-right",
  report_position = "bottom-left",
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  resolution <- match.arg(resolution)

  model <- map_from_data_frame(
    x,
    location = location,
    value = value,
    resolution = resolution,
    tooltip = tooltip,
    value_label = value_label
  )

  draw_map(
    model,
    classification = classification,
    colormap = colormap,
    num_classes = num_classes,
    opacity = opacity,
    show_boundaries = show_boundaries,
    outline_width = outline_width,
    show_legend = show_legend,
    legend_position = legend_position,
    tooltip_position = tooltip_position,
    report_position = report_position,
    title = title,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
