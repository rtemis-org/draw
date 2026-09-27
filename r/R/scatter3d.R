# scatter3d.R
# Native ECharts-GL interaction, with a deterministic vector projection for SVG.

#' Three-Dimensional Scatter Configuration
#'
#' Uses ECharts-GL Scatter3DSeries, Grid3DModel, Cartesian3D, and OrbitControl.
#' Coordinates are independently scaled to equal box dimensions. The camera is
#' orthographic: parallel lines remain parallel and exported point positions
#' match the configured browser view. Browser rotation/zoom is interactive;
#' saving the R widget exports its configured initial camera.
#' @param x,y,z Optional Character: Numeric coordinate column bindings.
#' @param group Optional Character: Group column binding.
#' @param alpha Numeric `[-80, 80]`: Camera elevation in degrees.
#' @param beta Numeric `[-180, 180]`: Camera azimuth in degrees.
#' @param view_size Numeric `[175, 400]`: Orthographic viewing extent for a 100-unit cube.
#' @param point_size Numeric `(0, 100]`: Marker diameter in pixels.
#' @param opacity Numeric `[0, 1]`: Point and line opacity.
#' @param mode Character `c("points", "lines", "both")`: Geometry to draw.
#' @param order Character `c("input", "x")`: Order within complete path runs.
#'   Missing coordinates break paths; sorting never joins across those breaks.
#' @param line_width Numeric `(0, 100]`: Path width in pixels.
#' @param xlab,ylab,zlab Optional Character: Axis names.
#' @param palette Optional Character: Group colors.
#' @inheritParams ChartConfig
#' @return Scatter3DConfig: Validated portable settings.
#' @export
#' @examples
#' setup_Scatter3DConfig(x = "Sepal.Length", y = "Sepal.Width", z = "Petal.Length")
Scatter3DConfig <- new_class(
  "Scatter3DConfig",
  parent = ChartConfig,
  package = "rtemis.draw",
  properties = list(
    type = prop_chart_type("scatter3d"),
    x = prop_string(NULL, nullable = TRUE),
    y = prop_string(NULL, nullable = TRUE),
    z = prop_string(NULL, nullable = TRUE),
    group = prop_string(NULL, nullable = TRUE),
    alpha = prop_float(20, min = -80, max = 80),
    beta = prop_float(40, min = -180, max = 180),
    view_size = prop_float(190, min = 175, max = 400),
    point_size = prop_float(8, exclusive_min = 0, max = 100),
    opacity = prop_float(.8, min = 0, max = 1),
    mode = prop_string(
      "points",
      enum = c("points", "lines", "both"),
      description = "Draw points, paths, or both."
    ),
    order = prop_string(
      "input",
      enum = c("input", "x"),
      description = "Order observations within complete path runs."
    ),
    line_width = prop_float(
      2,
      exclusive_min = 0,
      max = 100,
      description = "Path width in pixels."
    ),
    xlab = prop_string(NULL, nullable = TRUE),
    ylab = prop_string(NULL, nullable = TRUE),
    zlab = prop_string(NULL, nullable = TRUE),
    palette = prop_string(NULL, nullable = TRUE, vector = TRUE)
  )
)

#' Set Up a Three-Dimensional Scatter Configuration
#' @inheritParams Scatter3DConfig
#' @return Scatter3DConfig: Validated settings.
#' @export
#' @examples
#' draw(setup_Scatter3DConfig(x = "Sepal.Length", y = "Sepal.Width",
#'   z = "Petal.Length", group = "Species"), data = iris)
setup_Scatter3DConfig <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  group = NULL,
  alpha = 20,
  beta = 40,
  view_size = 190,
  point_size = 8,
  opacity = .8,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  palette = NULL,
  title = NULL,
  dat_path = NULL,
  origin = NULL,
  writer = NULL,
  mode = "points",
  order = "input",
  line_width = 2
) {
  origin <- origin %||%
    chart_origin(
      match.call(),
      setdiff(names(Scatter3DConfig@properties), c("type", PROVENANCE_PROPS))
    )
  Scatter3DConfig(
    x = x,
    y = y,
    z = z,
    group = group,
    alpha = alpha,
    beta = beta,
    view_size = view_size,
    point_size = point_size,
    opacity = opacity,
    mode = mode,
    order = order,
    line_width = line_width,
    xlab = xlab,
    ylab = ylab,
    zlab = zlab,
    palette = palette,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, Scatter3DConfig) <- function(config, data = NULL, ...) {
  config_derive(config, list(xlab = config@x, ylab = config@y, zlab = config@z))
}
method(render_meta, Scatter3DConfig) <- function(config, option) {
  legend_meta("top", "outside")
}
method(compile, Scatter3DConfig) <- function(config, data = NULL, ...) {
  scatter3d_option(
    config_column(data, config@x, "x"),
    config_column(data, config@y, "y"),
    config_column(data, config@z, "z"),
    config_column(data, config@group, "group"),
    config
  )
}

#' Validate and materialize native 3D point records
#' @param x,y,z Numeric: Coordinate vectors.
#' @param group Optional vector: Group identities.
#' @param config Scatter3DConfig: Display and camera settings.
#' @return EChartsOption: Native ECharts-GL option with explicit axis ranges.
#' @keywords internal
#' @noRd
scatter3d_option <- new_generic("scatter3d_option", "x")
method(scatter3d_option, class_any) <- function(x, y, z, group, config) {
  values <- list(x, y, z)
  if (
    length(unique(lengths(values))) != 1L ||
      !length(x) ||
      any(
        !vapply(
          values,
          function(v) {
            is.numeric(v) &&
              !is.complex(v) &&
              is.null(dim(v)) &&
              !any(is.infinite(v))
          },
          logical(1)
        )
      )
  ) {
    abort(
      "Supply equally sized numeric x, y, and z vectors, allowing NA but not infinity.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  group <- group_values(group, length(x)) %||% rep("Observations", length(x))
  keep <- !is.na(x) & !is.na(y) & !is.na(z) & !is.na(group)
  if (!any(keep)) {
    abort(
      "Supply at least one complete 3D observation.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  groups <- unique(as.character(group[keep]))
  series <- list()
  for (g in groups) {
    indices <- which(!is.na(group) & as.character(group) == g)
    complete <- keep[indices]
    runs <- split(indices[complete], cumsum(!complete)[complete])
    runs <- lapply(runs, function(index) {
      if (config@order == "x") {
        index <- index[order(x[index])]
      }
      Map(function(a, b, c) list(a, b, c), x[index], y[index], z[index])
    })
    if (config@mode %in% c("points", "both")) {
      series[[length(series) + 1L]] <- list(
        type = "scatter3D",
        name = g,
        symbolSize = config@point_size,
        itemStyle = list(opacity = config@opacity),
        data = unlist(runs, recursive = FALSE, use.names = FALSE)
      )
    }
    if (config@mode %in% c("lines", "both")) {
      for (run in runs) {
        if (length(run) < 2L) {
          next
        }
        series[[length(series) + 1L]] <- list(
          type = "line3D",
          name = g,
          lineStyle = list(width = config@line_width, opacity = config@opacity),
          data = run
        )
      }
    }
  }
  if (!length(series)) {
    abort(
      "Supply at least two consecutive complete observations in a group to draw a 3D path.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  values <- lapply(values, `[`, keep)
  group <- as.character(group[keep])
  groups <- unique(group)
  labels <- list(config@xlab, config@ylab, config@zlab)
  axes <- lapply(seq_len(3L), function(i) {
    limits <- range(values[[i]])
    if (limits[[1]] == limits[[2]]) {
      radius <- max(1, abs(limits[[1]]) * .05)
      limits <- limits + c(-radius, radius)
    }
    if (
      !all(is.finite(limits)) || !is.finite(diff(limits)) || diff(limits) <= 0
    ) {
      abort(
        "Rescale 3D coordinates to a finite nonzero range.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    list(
      type = "value",
      min = limits[[1]],
      max = limits[[2]],
      interval = diff(limits) / 4,
      name = labels[[i]] %||% c("x", "y", "z")[[i]],
      nameGap = 25,
      axisLine = list(lineStyle = list(color = "#888888")),
      axisLabel = list(color = "#888888"),
      nameTextStyle = list(color = "#888888")
    )
  })
  EChartsOption(
    x_axis_3d = axes[[1]],
    y_axis_3d = axes[[2]],
    z_axis_3d = axes[[3]],
    grid_3d = list(
      boxWidth = 100,
      boxDepth = 100,
      boxHeight = 100,
      top = 60,
      bottom = 20,
      left = 12,
      right = 12,
      viewControl = list(
        projection = "orthographic",
        alpha = config@alpha,
        beta = config@beta,
        orthographicSize = config@view_size - 1,
        minOrthographicSize = 20,
        maxOrthographicSize = 1000,
        animation = FALSE,
        autoRotate = FALSE,
        panMouseButton = "right"
      ),
      rtemisViewSize = config@view_size
    ),
    series = series,
    color = config@palette,
    legend = if (length(groups) > 1L) {
      Legend(data = as.list(groups))
    } else {
      Legend(show = FALSE)
    },
    tooltip = Tooltip(trigger = "item"),
    title = if (!is.null(config@title)) Title(text = config@title) else NULL
  )
}

#' Draw Interactive Three-Dimensional Points and Paths
#'
#' Uses ECharts-GL in the browser and vector circles, paths, and text for SVG.
#' The saved view uses the configured camera, not later browser rotations.
#' Points omit missing coordinate/group rows together. Missing coordinates break
#' paths within each group; groups keep their input order. A data frame or matrix
#' can supply the coordinates in its first three columns, which must be numeric.
#' @param x Numeric, matrix, or data frame: X values, or three coordinate columns.
#' @param y,z Optional Numeric: Y and Z values for vector x.
#' @param group Optional vector: Group identities.
#' @param ... Additional settings passed to [setup_Scatter3DConfig()].
#' @inheritParams draw_line theme width height element_id filename
#' @return htmlwidget: Interactive, exportable 3D drawing.
#' @export
#' @examples
#' draw_scatter3d(iris[1:3], group = iris$Species)
draw_scatter3d <- function(
  x,
  y = NULL,
  z = NULL,
  group = NULL,
  ...,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  config <- setup_Scatter3DConfig(...)
  if (is.data.frame(x) || is.matrix(x)) {
    if (!is.null(y) || !is.null(z) || ncol(x) < 3L) {
      abort(
        "Supply three coordinate columns alone, or separate x/y/z vectors.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    labels <- colnames(x)
    if (!is.null(labels)) {
      config <- config_derive(
        config,
        list(xlab = labels[[1]], ylab = labels[[2]], zlab = labels[[3]])
      )
    }
    z <- x[, 3L]
    y <- x[, 2L]
    x <- x[, 1L]
  }
  draw(
    scatter3d_option(x, y, z, group, config),
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = legend_meta("top", "outside")
  )
}
