# config_treemap.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The treemap and sunburst configs: a table's rows nested by level columns,
# which the @rtemis/draw treemap and sunburst builders turn into a tree
# (spec: draw/shared-builders#treemap, draw/shared-builders#sunburst).

# %% TreemapConfig ----
#' Treemap Configuration
#'
#' A serializable description of a treemap. Build one with
#' [setup_TreemapConfig()] rather than calling this constructor directly.
#'
#' @param levels Optional Character: Columns naming each row's node at each
#'   depth, outermost first. A row's path ends at its first missing level.
#' @param value Optional Character: Column holding each row's value. `NULL`
#'   counts each row as 1.
#' @param show_labels Logical: Whether each tile carries its name.
#' @param border_width Numeric `[0, Inf)`: Width of the border between tiles,
#'   in pixels.
#' @param palette Optional Character: Colors of the top-level nodes, overriding
#'   the theme palette. `NULL` uses the theme's.
#' @inheritParams ChartConfig
#'
#' @return `TreemapConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_TreemapConfig(levels = c("region", "country"), value = "n")@type
TreemapConfig <- new_class(
  name = "TreemapConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("treemap"),
    # -- data binding --------------------------------------------------------
    levels = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Columns naming each row's node at each depth, outermost first."
    ),
    value = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding each row's value. Unset counts each row as 1."
    ),
    # -- appearance ----------------------------------------------------------
    show_labels = prop_boolean(
      TRUE,
      description = "Whether each tile carries its name."
    ),
    border_width = prop_float(
      1,
      min = 0,
      description = "Width of the border between tiles, in pixels."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Colors of the top-level nodes, overriding the theme palette. Unset uses the theme's."
    )
  )
) # /rtemis.draw::TreemapConfig


# %% TREEMAP_ORIGIN_NAMES ----
TREEMAP_ORIGIN_NAMES <- setdiff(
  names(TreemapConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_TreemapConfig ----
#' Set up a Treemap Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams TreemapConfig
#' @param origin Optional Named character: Where each value came from. Normally
#'   computed from which arguments were supplied; pass it only when restoring a
#'   config that already carries provenance.
#' @param writer Optional Named character: Which interface wrote the config.
#'
#' @return [TreemapConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' places <- data.frame(
#'   region = c("Europe", "Europe", "Asia"),
#'   country = c("France", "Spain", "Japan"),
#'   n = c(3, 2, 5)
#' )
#' draw(setup_TreemapConfig(levels = c("region", "country"), value = "n"), data = places)
setup_TreemapConfig <- function(
  levels = NULL,
  value = NULL,
  show_labels = TRUE,
  border_width = 1,
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), TREEMAP_ORIGIN_NAMES)
  TreemapConfig(
    levels = levels,
    value = value,
    show_labels = show_labels,
    border_width = border_width,
    palette = palette,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_TreemapConfig


# %% resolve.TreemapConfig ----
# Nothing to derive: a treemap has no axes to label and no limits to compute.
method(resolve, TreemapConfig) <- function(config, data = NULL, ...) {
  config
}


# %% hierarchy_request ----
#' The build request of a tree chart
#'
#' Checks that a treemap or sunburst config names its level columns and that
#' they, and the value column, are in the data, and returns the request whose
#' builder nests the rows.
#'
#' @param config [TreemapConfig] or [SunburstConfig]: The config.
#' @param data Data frame: The table.
#'
#' @return [BuildRequest].
#'
#' @author EDG
#' @keywords internal
#' @noRd
hierarchy_request <- function(config, data) {
  if (is.null(config@levels)) {
    abort(
      "A ",
      class(config)[[1L]],
      " needs `levels` set to draw.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  for (level in config@levels) {
    config_column(data, level, "levels")
  }
  config_column(data, config@value, "value")
  BuildRequest(
    type = config@type,
    data = tabular_json(data, c(config@levels, config@value)),
    config = config_members(config),
    appearance = list()
  )
} # /rtemis.draw::hierarchy_request


# %% compile.TreemapConfig ----
# The request for the treemap builder, which nests the rows by level, sums the
# rows of each path, and checks the values.
method(compile, TreemapConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  hierarchy_request(config, data)
}


# %% SunburstConfig ----
#' Sunburst Configuration
#'
#' A serializable description of a sunburst chart. Build one with
#' [setup_SunburstConfig()] rather than calling this constructor directly.
#'
#' @inheritParams TreemapConfig
#' @param label_depth Integer `[0, Inf)`: Rings that carry labels, from the
#'   center out. `0` labels none.
#' @param min_angle Numeric `[0, 360]`: Smallest angle, in degrees, of a sector
#'   that carries a label.
#' @param label_rotate Character \{"radial", "tangential", "horizontal"\}:
#'   Label direction.
#' @param radius Character: Outer radius, as a CSS length or percentage.
#' @param inner_radius Character: Radius of the empty center, as a CSS length or
#'   percentage.
#' @param border_width Numeric `[0, Inf)`: Width of the border between sectors,
#'   in pixels.
#' @param palette Optional Character: Colors of the innermost ring, overriding
#'   the theme palette. `NULL` uses the theme's.
#' @inheritParams ChartConfig
#'
#' @return `SunburstConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' setup_SunburstConfig(levels = c("region", "country"), value = "n")@type
SunburstConfig <- new_class(
  name = "SunburstConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("sunburst"),
    # -- data binding --------------------------------------------------------
    levels = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Columns naming each row's node at each depth, outermost first."
    ),
    value = prop_string(
      NULL,
      nullable = TRUE,
      description = "Column holding each row's value. Unset counts each row as 1."
    ),
    # -- appearance ----------------------------------------------------------
    label_depth = prop_integer(
      2L,
      min = 0L,
      description = "Rings that carry labels, from the center out. 0 labels none."
    ),
    min_angle = prop_float(
      0,
      min = 0,
      max = 360,
      description = "Smallest angle, in degrees, of a sector that carries a label."
    ),
    label_rotate = prop_string(
      "radial",
      enum = c("radial", "tangential", "horizontal"),
      description = "Label direction."
    ),
    radius = prop_string(
      "85%",
      description = "Outer radius, as a CSS length or percentage."
    ),
    inner_radius = prop_string(
      "18%",
      description = "Radius of the empty center, as a CSS length or percentage."
    ),
    border_width = prop_float(
      1,
      min = 0,
      description = "Width of the border between sectors, in pixels."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Colors of the innermost ring, overriding the theme palette. Unset uses the theme's."
    )
  )
) # /rtemis.draw::SunburstConfig


# %% SUNBURST_ORIGIN_NAMES ----
SUNBURST_ORIGIN_NAMES <- setdiff(
  names(SunburstConfig@properties),
  c("type", PROVENANCE_PROPS)
)


# %% setup_SunburstConfig ----
#' Set up a Sunburst Configuration
#'
#' The seam between convenient input and a complete, validated object. **Every
#' argument is optional**, which is what lets the published schema require
#' nothing.
#'
#' @inheritParams SunburstConfig
#' @inheritParams setup_TreemapConfig
#'
#' @return [SunburstConfig] object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' places <- data.frame(
#'   region = c("Europe", "Europe", "Asia"),
#'   country = c("France", "Spain", "Japan"),
#'   n = c(3, 2, 5)
#' )
#' draw(setup_SunburstConfig(levels = c("region", "country"), value = "n"), data = places)
setup_SunburstConfig <- function(
  levels = NULL,
  value = NULL,
  label_depth = 2L,
  min_angle = 0,
  label_rotate = "radial",
  radius = "85%",
  inner_radius = "18%",
  border_width = 1,
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL
) {
  origin <- origin %||% chart_origin(match.call(), SUNBURST_ORIGIN_NAMES)
  SunburstConfig(
    levels = levels,
    value = value,
    label_depth = clean_int(label_depth),
    min_angle = min_angle,
    label_rotate = label_rotate,
    radius = radius,
    inner_radius = inner_radius,
    border_width = border_width,
    palette = palette,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
} # /rtemis.draw::setup_SunburstConfig


# %% resolve.SunburstConfig ----
method(resolve, SunburstConfig) <- function(config, data = NULL, ...) {
  config
}


# %% compile.SunburstConfig ----
method(compile, SunburstConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  hierarchy_request(config, data)
}
