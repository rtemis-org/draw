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
#' @param opacity Optional Numeric `[0, 1]`: Point and line opacity. `NULL`
#'   sets it from the number of complete observations, as in [draw_scatter()]'s
#'   `point_alpha`, and draws paths alone (`mode = "lines"`) opaque.
#' @param mode Character `c("points", "lines", "both")`: Geometry to draw.
#' @param order Character `c("input", "x")`: Order within complete path runs.
#'   Missing coordinates break paths; sorting never joins across those breaks.
#' @param line_width Numeric `(0, 100]`: Path width in pixels.
#' @param xlab,ylab,zlab Optional Character: Axis names.
#' @param palette Optional Character: Group colors.
#' @param fit Optional Character: Model of z on x and y drawn as a surface per
#'   group: `"glm"`, `"gam"`, or an rtemis supervised learning algorithm name
#'   such as `"LINAD"`, which requires the rtemis package (>= 1.4.1). See
#'   [draw_scatter()]. `NULL` draws no surface.
#' @param fit_params Optional Named list: Arguments passed to the learner named
#'   in `fit`. See [draw_scatter()].
#' @param n_fit Integer `[2, Inf)`: Grid points per axis for fitted surfaces.
#' @param fit_alpha Numeric `[0, 1]`: Opacity of fitted surfaces.
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
    opacity = prop_float(
      NULL,
      min = 0,
      max = 1,
      nullable = TRUE,
      description = paste(
        "Point and line opacity. Unset sets it from the number of complete",
        "observations, or draws paths alone opaque."
      )
    ),
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
    palette = prop_string(NULL, nullable = TRUE, vector = TRUE),
    fit = prop_string(
      NULL,
      nullable = TRUE,
      description = paste(
        "Surface of z on x and y to overlay: glm, gam, or an rtemis supervised",
        "learning algorithm name. Unset draws no surface."
      )
    ),
    fit_params = prop_bag(
      nullable = TRUE,
      description = paste(
        "Arguments passed to the learner named in fit: to glm() or gam()",
        "(k sets each smooth's basis dimension), or to the rtemis",
        "algorithm's setup function. Unset uses the learner's defaults."
      )
    ),
    n_fit = prop_integer(
      25L,
      min = 2L,
      description = "Grid points per axis for fitted surfaces."
    ),
    fit_alpha = prop_float(
      .35,
      min = 0,
      max = 1,
      description = "Opacity of fitted surfaces."
    )
  ),
  validator = config_validator(list(FIT_PARAMS_RULE))
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
  opacity = NULL,
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
  line_width = 2,
  fit = NULL,
  fit_params = NULL,
  n_fit = 25L,
  fit_alpha = .35
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
    fit = fit,
    fit_params = fit_params,
    n_fit = clean_int(n_fit),
    fit_alpha = fit_alpha,
    title = title,
    dat_path = dat_path,
    origin = origin,
    writer = writer
  )
}

method(resolve, Scatter3DConfig) <- function(config, data = NULL, ...) {
  coords <- list(
    config_column(data, config@x, "x"),
    config_column(data, config@y, "y"),
    config_column(data, config@z, "z"),
    config_column(data, config@group, "group")
  )
  bound <- !any(vapply(coords[1:3], is.null, logical(1)))
  config_derive(
    config,
    list(
      xlab = config@x,
      ylab = config@y,
      zlab = config@z,
      opacity = if (bound) {
        scatter3d_opacity(config@mode, complete_rows(coords))
      }
    )
  )
}
method(render_meta, Scatter3DConfig) <- function(config, option) {
  legend_meta("top", "outside")
}
method(compile, Scatter3DConfig) <- function(
  config,
  data = NULL,
  theme = NULL,
  ...
) {
  scatter3d_option(
    config_column(data, config@x, "x"),
    config_column(data, config@y, "y"),
    config_column(data, config@z, "z"),
    config_column(data, config@group, "group"),
    config
  )
}

#' Default 3D point and line opacity
#'
#' Points use `auto_alpha()` on the number of complete observations. Paths
#' alone are drawn opaque, since the count of vertices says little about how
#' much a path overdraws.
#' @param mode Character: `Scatter3DConfig@mode`.
#' @param n Integer: Number of complete observations.
#' @return Numeric `[0, 1]`: Opacity.
#' @keywords internal
#' @noRd
scatter3d_opacity <- function(mode, n) {
  if (mode == "lines") 1 else auto_alpha(n)
}

#' Count rows complete across the non-NULL vectors in a list
#' @param values List: Equally sized vectors, or `NULL` entries to skip.
#' @return Integer: Number of rows without a missing value.
#' @keywords internal
#' @noRd
complete_rows <- function(values) {
  values <- Filter(Negate(is.null), values)
  sum(Reduce(`&`, lapply(values, function(v) !is.na(v))))
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
  opacity <- config@opacity %||% scatter3d_opacity(config@mode, sum(keep))
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
        itemStyle = list(opacity = opacity),
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
          lineStyle = list(width = config@line_width, opacity = opacity),
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
  # One surface per group, named after it: legend selection and the palette
  # color follow the group. The z axis extends to include the predictions.
  surfaces <- list()
  if (!is.null(config@fit)) {
    surfaces <- lapply(groups, function(g) {
      index <- group == g
      fit_surface(
        values[[1L]][index],
        values[[2L]][index],
        values[[3L]][index],
        g,
        config
      )
    })
    series <- c(series, surfaces)
    # Missing predictions are serialized as NULL grid values.
    values[[3L]] <- c(
      values[[3L]],
      unlist(lapply(surfaces, function(surface) {
        lapply(surface[["data"]], `[[`, 3L)
      }))
    )
  }
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

#' Fit and materialize one group's surface
#'
#' Predicts z on a regular `n_fit` by `n_fit` grid spanning the group's x and y
#' ranges, using the same fitting step as 2D scatter fits.
#' @param x,y,z Numeric: Complete coordinates of one group.
#' @param name Character: Group name, used as the surface name.
#' @param config Scatter3DConfig: Settings naming the fit and grid.
#' @return List: Native ECharts-GL surface series without a color, which the
#'   renderer assigns from the palette by name.
#' @keywords internal
#' @noRd
fit_surface <- function(x, y, z, name, config) {
  if (length(z) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    abort(
      "Each fitted group needs at least 3 complete observations and two ",
      "distinct x and y values; add observations or set `fit = NULL`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  grid_x <- seq(min(x), max(x), length.out = config@n_fit)
  grid_y <- seq(min(y), max(y), length.out = config@n_fit)
  # expand.grid varies x fastest, which fills the matrix one column per y.
  pred <- fit_model_values(
    data.frame(x = x, y = y),
    z,
    expand.grid(x = grid_x, y = grid_y),
    config@fit,
    se = FALSE,
    params = config@fit_params
  )
  surface <- to_list(SurfaceLayer(
    x = grid_x,
    y = grid_y,
    z = matrix(pred[["fitted"]], nrow = length(grid_x)),
    name = name,
    opacity = config@fit_alpha
  ))
  surface[["itemStyle"]][["color"]] <- NULL
  surface
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
