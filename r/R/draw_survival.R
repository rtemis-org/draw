# draw_survival.R
# ::rtemis.draw::

#' Validate and normalize precomputed survival records
#' @param config [SurvivalConfig]: Column bindings and layer settings.
#' @param data Data frame or list: Curves and optional explicit risk records.
#' @return List containing ordered curve frames and risk records.
#' @keywords internal
#' @noRd
survival_data <- new_generic("survival_data", "config")
method(survival_data, SurvivalConfig) <- function(config, data) {
  if (!is.list(data)) {
    abort(
      "Supply a curve data frame or a list with a `curves` data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  curves <- if (is.data.frame(data)) data else data[["curves"]]
  risk <- if (is.data.frame(data)) NULL else data[["risk"]]
  if (!is.data.frame(curves) || !nrow(curves)) {
    abort(
      "Supply a nonempty curve data frame, or a list with `curves` and optional `risk` frames.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  fields <- c(
    "time",
    "survival",
    "group",
    "lower",
    "upper",
    "n_censor",
    "n_risk"
  )
  d <- setNames(
    lapply(fields, function(nm) config_column(curves, prop(config, nm), nm)),
    fields
  )
  d[["group"]] <- d[["group"]] %||% rep("Survival", nrow(curves))
  if (
    !is.atomic(d[["group"]]) ||
      !is.null(dim(d[["group"]])) ||
      anyNA(d[["group"]]) ||
      any(!nzchar(as.character(d[["group"]])))
  ) {
    abort(
      "Supply a nonmissing, nonempty group label for every curve row.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  d[["group"]] <- as.character(d[["group"]])
  for (nm in setdiff(fields, "group")) {
    v <- d[[nm]]
    if (is.null(v)) {
      next
    }
    if (
      !is.numeric(v) ||
        is.complex(v) ||
        !is.null(dim(v)) ||
        length(v) != nrow(curves) ||
        any(!is.finite(v[!is.na(v)])) ||
        (nm %in% c("time", "survival") && anyNA(v))
    ) {
      abort(
        "Supply finite numeric `",
        nm,
        "` values; only optional fields may contain NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      (nm %in%
        c("survival", "lower", "upper") &&
        any(v < 0 | v > 1, na.rm = TRUE)) ||
        (nm %in% c("n_risk", "n_censor") && any(v < 0, na.rm = TRUE))
    ) {
      abort(
        "Keep survival probabilities and bounds in [0, 1], and counts nonnegative.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (
    !is.null(d[["lower"]]) &&
      (any(d[["lower"]] > d[["survival"]], na.rm = TRUE) ||
        any(d[["upper"]] < d[["survival"]], na.rm = TRUE))
  ) {
    abort(
      "Supply confidence bounds that enclose each survival estimate.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  d <- as.data.frame(d[!vapply(d, is.null, logical(1))])
  labels <- unique(d[["group"]])
  groups <- lapply(labels, function(label) {
    g <- d[d[["group"]] == label, , drop = FALSE]
    g <- g[order(g[["time"]]), , drop = FALSE]
    if (anyDuplicated(g[["time"]]) || any(diff(g[["survival"]]) > 1e-12)) {
      abort(
        "Use unique times and nonincreasing survival estimates within each group.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    g
  })
  if (config@risk_table) {
    if (
      !is.data.frame(risk) ||
        !nrow(risk) ||
        !all(c("time", "group", "n_risk") %in% names(risk))
    ) {
      abort(
        "Supply explicit `risk` records with time, group, and n_risk columns to draw a risk table.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      !is.numeric(risk[["time"]]) ||
        is.complex(risk[["time"]]) ||
        any(!is.finite(risk[["time"]])) ||
        !is.numeric(risk[["n_risk"]]) ||
        is.complex(risk[["n_risk"]]) ||
        any(!is.finite(risk[["n_risk"]][!is.na(risk[["n_risk"]])])) ||
        any(risk[["n_risk"]] < 0, na.rm = TRUE) ||
        anyNA(risk[["group"]]) ||
        !setequal(as.character(risk[["group"]]), labels) ||
        anyDuplicated(risk[c("group", "time")])
    ) {
      abort(
        "Supply unique group/time risk rows with finite times and nonnegative counts or NA.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    times <- sort(unique(risk[["time"]]))
    if (
      min(times) < min(d[["time"]]) ||
        max(times) > max(d[["time"]]) ||
        any(vapply(
          labels,
          function(g) sum(risk[["group"]] == g) != length(times),
          logical(1)
        ))
    ) {
      abort(
        "Use the same risk times for every group, within the chart's recorded time range.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(groups = groups, risk = risk)
}

#' Compute the median of a recorded right-continuous survival curve
#' @param x Data frame: Ordered time and survival columns.
#' @return Numeric median time, or NA if the median is not reached.
#' @keywords internal
#' @noRd
survival_median <- new_generic("survival_median", "x")
method(survival_median, class_data.frame) <- function(x) {
  s <- x[["survival"]]
  t <- x[["time"]]
  tol <- sqrt(.Machine$double.eps)
  hit <- which(s <= .5 + tol)
  if (!length(hit)) {
    return(NA_real_)
  }
  i <- hit[[1L]]
  if (abs(s[[i]] - .5) > tol) {
    return(t[[i]])
  }
  below <- which(seq_along(s) > i & s < .5 - tol)
  end <- if (length(below)) t[[below[[1L]]]] else tail(t, 1L)
  (t[[i]] + end) / 2
}

#' Compile native survival curves and optional statistical layers
#' @inheritParams survival_data
#' @return An [EChartsOption] with portable native vector marks.
#' @keywords internal
#' @noRd
survival_option <- new_generic("survival_option", "config")
method(survival_option, SurvivalConfig) <- function(config, data) {
  records <- survival_data(config, data)
  groups <- records[["groups"]]
  labels <- vapply(groups, function(g) g[["group"]][[1L]], character(1))
  domain <- range(unlist(lapply(groups, function(g) g[["time"]])))
  # A one-time-point record still needs a visible coordinate domain.
  if (diff(domain) == 0) {
    domain <- domain + c(-.5, .5)
  }
  fmt <- function(v) {
    ifelse(
      is.na(v),
      "NA",
      formatC(v, digits = config@digits, format = "f", decimal.mark = ".")
    )
  }
  series <- list()
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    name <- labels[[i]]
    color <- if (is.null(config@palette)) {
      NULL
    } else {
      config@palette[[(i - 1L) %% length(config@palette) + 1L]]
    }
    # Unrounded coordinates and ordinal tooltip text are separate dimensions.
    vals <- lapply(seq_len(nrow(g)), function(j) {
      list(
        g[["time"]][[j]],
        g[["survival"]][[j]],
        fmt(g[["survival"]][[j]]),
        if (is.null(g[["lower"]])) "NA" else fmt(g[["lower"]][[j]]),
        if (is.null(g[["upper"]])) "NA" else fmt(g[["upper"]][[j]]),
        if (is.null(g[["n_risk"]])) "NA" else as.character(g[["n_risk"]][[j]]),
        if (is.null(g[["n_censor"]])) {
          "NA"
        } else {
          as.character(g[["n_censor"]][[j]])
        }
      )
    })
    curve <- to_list(LineSeries(
      name = name,
      data = vals,
      step = "end",
      smooth = FALSE,
      show_symbol = TRUE,
      symbol = "circle",
      symbol_size = 6,
      clip = FALSE,
      line_style = LineStyle(color = color, width = config@line_width),
      item_style = ItemStyle(
        color = color,
        opacity = if (nrow(g) == 1L) 1 else 0
      ),
      z = 3
    ))
    curve[["emphasis"]] <- list(itemStyle = list(opacity = 1))
    curve[["dimensions"]] <- c(
      list("time", "survival"),
      lapply(
        c("Survival", "Lower", "Upper", "At risk", "Censored"),
        function(nm) list(name = nm, type = "ordinal")
      )
    )
    curve[["encode"]] <- list(x = 0L, y = 1L, tooltip = as.list(c(0L, 2L:6L)))
    series[[length(series) + 1L]] <- curve
    if (config@show_ci && !is.null(g[["lower"]])) {
      ok <- !is.na(g[["lower"]]) & !is.na(g[["upper"]])
      for (band in c("lower", "width")) {
        values <- if (band == "lower") {
          g[["lower"]]
        } else {
          g[["upper"]] - g[["lower"]]
        }
        points <- list()
        for (j in seq_len(nrow(g))) {
          if (j > 1L && ok[[j - 1L]] && !ok[[j]]) {
            # Retain the preceding known interval up to the missing step.
            # A null row alone would erase that horizontal segment in ECharts.
            points[[length(points) + 1L]] <- list(
              g[["time"]][[j]],
              values[[j - 1L]]
            )
          }
          points[[length(points) + 1L]] <- list(
            g[["time"]][[j]],
            if (ok[[j]]) values[[j]] else NULL
          )
        }
        s <- to_list(LineSeries(
          name = name,
          step = "end",
          stack = paste0("ci-", i),
          data = points,
          show_symbol = FALSE,
          connect_nulls = FALSE,
          silent = TRUE,
          legend_hover_link = FALSE,
          item_style = ItemStyle(color = color),
          line_style = LineStyle(opacity = 0),
          area_style = AreaStyle(
            color = color,
            opacity = if (band == "lower") 0 else config@ci_opacity
          ),
          z = 1
        ))
        s[["stackStrategy"]] <- "all"
        series[[length(series) + 1L]] <- s
      }
    }
    if (config@show_censors && !is.null(g[["n_censor"]])) {
      ids <- which(g[["n_censor"]] > 0)
      if (length(ids)) {
        s <- to_list(ScatterSeries(
          name = name,
          data = vals[ids],
          symbol = "rect",
          symbol_size = c(1.5, config@censor_size),
          clip = FALSE,
          item_style = ItemStyle(color = color),
          z = 4
        ))
        s[["dimensions"]] <- curve[["dimensions"]]
        s[["encode"]] <- curve[["encode"]]
        series[[length(series) + 1L]] <- s
      }
    }
    if (config@show_median) {
      median <- survival_median(g)
      if (is.finite(median)) {
        # Finite native segments avoid callback-based markLine labels and
        # distinguish a median dropline from a full-height reference.
        s <- to_list(LineSeries(
          name = name,
          data = list(c(domain[[1L]], .5), c(median, .5), c(median, 0)),
          show_symbol = FALSE,
          silent = TRUE,
          legend_hover_link = FALSE,
          item_style = ItemStyle(color = color),
          line_style = LineStyle(color = color, type = "dashed", opacity = .7),
          z = 2
        ))
        series[[length(series) + 1L]] <- s
      }
    }
    if (length(config@landmarks)) {
      times <- sort(unique(config@landmarks))
      times <- times[times >= min(g[["time"]]) & times <= max(g[["time"]])]
      if (length(times)) {
        ids <- findInterval(times, g[["time"]])
        s <- to_list(ScatterSeries(
          name = name,
          data = lapply(seq_along(times), function(j) {
            list(
              value = c(times[[j]], g[["survival"]][[ids[[j]]]]),
              name = fmt(g[["survival"]][[ids[[j]]]])
            )
          }),
          symbol_size = 6,
          item_style = ItemStyle(color = color),
          clip = FALSE,
          label = LabelOption(
            show = TRUE,
            position = "top",
            formatter = "{b}",
            text_style = TextStyle(color = "inherit")
          ),
          z = 5
        ))
        series[[length(series) + 1L]] <- s
      }
    }
    if (config@risk_table) {
      risk <- records[["risk"]]
      risk <- risk[risk[["group"]] == name, , drop = FALSE]
      # Native labels on offset scatter symbols stay aligned to the main time
      # axis under resize, legend placement, and SVG export. No second grid or
      # custom rendering callback is necessary.
      for (row in c("heading", "counts")) {
        points <- if (row == "heading") {
          list(list(value = c(mean(domain), 0), name = paste("At risk:", name)))
        } else {
          lapply(seq_len(nrow(risk)), function(j) {
            list(
              value = c(risk[["time"]][[j]], 0),
              name = if (is.na(risk[["n_risk"]][[j]])) {
                "NA"
              } else {
                as.character(risk[["n_risk"]][[j]])
              }
            )
          })
        }
        s <- to_list(ScatterSeries(
          name = name,
          data = points,
          symbol_size = 1,
          symbol_offset = c(
            0,
            80 + (i - 1L) * 40 + if (row == "counts") 17 else 0
          ),
          clip = FALSE,
          silent = TRUE,
          legend_hover_link = FALSE,
          item_style = ItemStyle(color = color, opacity = 0),
          label = LabelOption(
            show = TRUE,
            position = "inside",
            formatter = "{b}",
            text_style = TextStyle(color = "inherit", font_size = 11)
          ),
          z = 5
        ))
        # Item transparency must not hide the text itself.
        s[["label"]][["opacity"]] <- 1
        series[[length(series) + 1L]] <- s
      }
    }
  }
  if (config@risk_table) {
    # Explicit time headings also identify irregular risk-table columns when
    # they do not coincide with the main axis's automatic ticks.
    times <- sort(unique(records[["risk"]][["time"]]))
    header <- to_list(ScatterSeries(
      data = lapply(times, function(t) {
        list(value = c(t, 0), name = paste0("t=", t))
      }),
      symbol_size = 1,
      symbol_offset = c(0, 55),
      clip = FALSE,
      silent = TRUE,
      legend_hover_link = FALSE,
      item_style = ItemStyle(color = "#888888", opacity = 0),
      label = LabelOption(
        show = TRUE,
        position = "inside",
        formatter = "{b}",
        text_style = TextStyle(color = "#888888", font_size = 11)
      ),
      z = 5
    ))
    header[["label"]][["opacity"]] <- 1
    series[[length(series) + 1L]] <- header
  }
  EChartsOption(
    title = Title(text = config@title, left = "center", text_align = "center"),
    grid = Grid(
      left = 70,
      right = 30,
      top = if (is.null(config@title)) 25 else 50,
      bottom = if (config@risk_table) 90 + 40 * length(groups) else 60,
      contain_label = FALSE
    ),
    x_axis = Axis(
      type = "value",
      min = domain[[1L]],
      max = domain[[2L]],
      name = config@xlab,
      name_location = "middle",
      name_gap = 30
    ),
    y_axis = Axis(
      type = "value",
      min = 0,
      max = 1,
      name = config@ylab,
      name_location = "middle",
      name_gap = 45
    ),
    legend = Legend(
      show = config@legend,
      data = as.list(labels),
      padding = 0,
      item_style = ItemStyle(opacity = 1),
      text_style = TextStyle(font_size = 12, line_height = 14)
    ),
    tooltip = Tooltip(trigger = "item", confine = TRUE),
    series = series
  )
}

#' Draw Precomputed Survival Curves
#'
#' Use [draw_survfit()] to draw fitted R survival objects. This interface accepts
#' portable records and does not require the survival package.
#' @inheritSection SurvivalConfig Records and statistical semantics
#' @param data Data frame or list: Curve records, or `curves` and `risk` frames.
#' @param ... Additional named settings for [setup_SurvivalConfig()].
#' @param group,lower,upper,n_censor,n_risk Optional Character: Column bindings.
#'   When omitted, a column with the matching name is used if present.
#'   Explicit NULL disables that binding.
#' @inheritParams draw_roc
#' @return An ECharts htmlwidget with native SVG export.
#' @export
#' @examples
#' draw_survival(data.frame(time = c(0, 1, 3), survival = c(1, .8, .4)))
draw_survival <- function(
  data,
  ...,
  group = NULL,
  lower = NULL,
  upper = NULL,
  n_censor = NULL,
  n_risk = NULL,
  legend_position = "top",
  legend_placement = "outside",
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  if (!is.list(data)) {
    abort(
      "Supply a curve data frame or a list with a `curves` data frame.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  curves <- if (is.data.frame(data)) data else data[["curves"]]
  if (missing(group) && "group" %in% names(curves)) {
    group <- "group"
  }
  if (missing(lower) && "lower" %in% names(curves)) {
    lower <- "lower"
  }
  if (missing(upper) && "upper" %in% names(curves)) {
    upper <- "upper"
  }
  if (missing(n_censor) && "n_censor" %in% names(curves)) {
    n_censor <- "n_censor"
  }
  if (missing(n_risk) && "n_risk" %in% names(curves)) {
    n_risk <- "n_risk"
  }
  config <- setup_SurvivalConfig(
    group = group,
    lower = lower,
    upper = upper,
    n_censor = n_censor,
    n_risk = n_risk,
    legend_position = legend_position,
    legend_placement = legend_placement,
    ...
  )
  draw(
    config,
    data = data,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
}
