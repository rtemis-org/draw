# draw_varimp.R
# ::rtemis.draw::
# 2026- EDG rtemis.org
# spec: draw/first-cran-release#variable-importance

# %% varimp_data ----
#' Normalize variable-importance records
#'
#' Keep named measures and observed fold rows intact. Summaries are computed
#' separately so this boundary can also supply distribution views.
#'
#' @param x Numeric vector, single-column matrix, or data frame: Importance data.
#' @return Data frame with `variable`, named measures, and optionally `fold`.
#' @keywords internal
#' @noRd
varimp_data <- new_generic("varimp_data", "x")

method(varimp_data, class_any) <- function(x) {
  if (
    !is.numeric(x) ||
      is.complex(x) ||
      (!is.null(dim(x)) && (!is.matrix(x) || ncol(x) != 1L))
  ) {
    abort(
      "Supply numeric importance scores, a single-column matrix, or a table.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  variables <- if (is.matrix(x)) rownames(x) else names(x)
  varimp_data(data.frame(
    variable = variables %||% as.character(seq_along(x)),
    importance = as.numeric(x)
  ))
}

method(varimp_data, class_data.frame) <- function(x) {
  x <- as.data.frame(x)
  measures <- setdiff(names(x), c("variable", "fold"))
  if (
    nrow(x) == 0L ||
      !"variable" %in% names(x) ||
      !length(measures) ||
      anyNA(names(x)) ||
      any(!nzchar(names(x))) ||
      anyDuplicated(names(x))
  ) {
    abort(
      "Supply a nonempty table with unique column names, `variable`, ",
      "and at least one named numeric measure.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keys <- intersect(c("variable", "fold"), names(x))
  for (key in keys) {
    value <- x[[key]]
    if (is.factor(value) || (key == "fold" && is.numeric(value))) {
      value <- as.character(value)
    }
    if (
      !is.character(value) ||
        !is.null(dim(value)) ||
        anyNA(value) ||
        any(!nzchar(trimws(value)))
    ) {
      abort(
        "Supply nonmissing, nonempty identifiers in `",
        key,
        "`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    x[[key]] <- value
  }
  if (anyDuplicated(x[keys])) {
    abort(
      "Supply one row per variable, or per variable and fold.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  for (measure in measures) {
    value <- x[[measure]]
    if (is.logical(value) && all(is.na(value))) {
      value <- as.numeric(value)
    }
    if (
      !is.numeric(value) ||
        is.complex(value) ||
        !is.null(dim(value)) ||
        any(is.infinite(value))
    ) {
      abort(
        "Supply finite numeric scores or NA in `",
        measure,
        "`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    x[[measure]] <- as.numeric(value)
  }
  rownames(x) <- NULL
  x
}


# %% varimp_measure ----
#' Select a named importance measure
#' @param data Data frame: Normalized importance records.
#' @param measure Optional Character: Measure column to select.
#' @return Character: Selected column name.
#' @keywords internal
#' @noRd
varimp_measure <- new_generic("varimp_measure", "data")

method(varimp_measure, class_data.frame) <- function(data, measure = NULL) {
  measures <- setdiff(names(data), c("variable", "fold"))
  measure <- measure %||% measures[[1L]]
  check_character_scalar(measure)
  check_enum(measure, measures)
  measure
}


#' Resolve the fold universe and reporting folds for one importance measure
#' @param data Data frame: Normalized importance records.
#' @param measure Character: Selected numeric measure column.
#' @param folds Optional Character: Complete fold IDs, including unavailable folds.
#' @return List: Full fold IDs and IDs reporting at least one score.
#' @keywords internal
#' @noRd
varimp_fold_info <- new_generic("varimp_fold_info", "data")
method(varimp_fold_info, class_data.frame) <- function(
  data,
  measure,
  folds = NULL
) {
  observed_folds <- unique(data[["fold"]])
  if (!is.null(folds)) {
    if (
      is.null(observed_folds) ||
        !is.character(folds) ||
        !is.null(dim(folds)) ||
        !length(folds) ||
        anyNA(folds) ||
        any(!nzchar(trimws(folds))) ||
        anyDuplicated(folds) ||
        !all(observed_folds %in% folds)
    ) {
      abort(
        "Supply `folds` as unique nonempty names containing every observed fold, ",
        "and include a `fold` column in the data.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(
    ids = folds %||% observed_folds,
    scored = unique(data[["fold"]][!is.na(data[[measure]])])
  )
}

#' Materialize selected variable-by-fold scores for portable boxplots
#'
#' Allocate only selected variables, after ranking. Missing rows become zero
#' only under the same reporting-fold contract used by the summary renderer.
#' Explicit missing values and entirely unavailable folds remain missing.
#' @param data Data frame: Normalized importance records with a fold column.
#' @param selected Data frame: Selected summary rows in display order.
#' @param measure Character: Selected numeric measure column.
#' @param absent Character: Validated omitted-row policy.
#' @param folds Optional Character: Complete fold IDs.
#' @return List: Wide data, variable column names, and observation column name.
#' @keywords internal
#' @noRd
varimp_distribution <- new_generic("varimp_distribution", "data")
method(varimp_distribution, class_data.frame) <- function(
  data,
  selected,
  measure,
  absent,
  folds = NULL
) {
  info <- varimp_fold_info(data, measure, folds)
  if (is.null(info[["ids"]])) {
    abort(
      "Supply fold-level importance records with a `fold` column for type = 'boxplot'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  variables <- selected[["variable"]]
  observation <- ".fold"
  while (observation %in% variables) {
    observation <- paste0(observation, "_")
  }
  wide <- setNames(data.frame(info[["ids"]]), observation)
  # Index once instead of rescanning all input rows for each selected variable.
  at <- which(data[["variable"]] %in% variables)
  rows <- split(at, factor(data[["variable"]][at], levels = variables))
  for (variable in variables) {
    at <- rows[[variable]]
    matched <- match(info[["ids"]], data[["fold"]][at])
    values <- data[[measure]][at][matched]
    if (absent == "zero") {
      values[is.na(matched) & info[["ids"]] %in% info[["scored"]]] <- 0
    }
    wide[[variable]] <- values
  }
  list(data = wide, columns = variables, observation = observation)
}


# %% summarize_varimp ----
#' Summarize and select importance records for either view
#'
#' Missing scores are handled before ranking. Ties retain first appearance.
#' Zero padding uses counts rather than a dense variable-by-fold matrix.
#'
#' @param data Data frame: Normalized importance records.
#' @inheritParams draw_varimp
#' @return Data frame: `variable`, `importance`, `n_available`, `n_folds`, and
#'   `n_zero`, in the requested rank order.
#' @keywords internal
#' @noRd
summarize_varimp <- new_generic("summarize_varimp", "data")

method(summarize_varimp, class_data.frame) <- function(
  data,
  measure = NULL,
  top_n = 20L,
  rank_by = "magnitude",
  summary = "mean",
  absent = "missing",
  folds = NULL,
  decreasing = TRUE
) {
  measure <- varimp_measure(data, measure)
  check_character_scalar(rank_by)
  check_enum(rank_by, c("magnitude", "signed"))
  check_logical_scalar(decreasing)
  check_character_scalar(summary)
  check_enum(summary, c("mean", "median"))
  check_character_scalar(absent)
  check_enum(absent, c("missing", "zero"))
  if (!is.null(top_n)) {
    top_n <- clean_int(top_n)
    check_integer_scalar(top_n)
    if (top_n < 1L) {
      abort(
        "Set `top_n` to a positive whole-number count, or NULL for all variables.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  info <- varimp_fold_info(data, measure, folds)
  observed_folds <- info[["ids"]]
  n_folds <- if (is.null(observed_folds)) 1L else length(observed_folds)
  variables <- unique(data[["variable"]])
  rows <- split(seq_len(nrow(data)), match(data[["variable"]], variables))
  rows <- rows[as.character(seq_along(variables))]
  # A wholly unavailable fold/measure has no known sparse result. Only folds
  # reporting at least one score can contribute structural zeros. An explicit
  # missing row is still unknown, even when absent rows mean zero.
  scored_folds <- info[["scored"]]
  in_scored_fold <- data[["fold"]] %in% scored_folds
  n_zero <- vapply(
    rows,
    function(at) {
      if (absent != "zero" || is.null(observed_folds)) {
        return(0L)
      }
      length(scored_folds) - sum(in_scored_fold[at])
    },
    integer(1L),
    USE.NAMES = FALSE
  )
  n_available <- vapply(
    rows,
    function(at) {
      sum(!is.na(data[[measure]][at]))
    },
    integer(1L),
    USE.NAMES = FALSE
  ) +
    n_zero
  scores <- vapply(
    seq_along(rows),
    function(index) {
      at <- rows[[index]]
      values <- data[[measure]][at]
      values <- values[!is.na(values)]
      n <- n_available[[index]]
      if (n == 0L) {
        return(NA_real_)
      }
      if (!length(values)) {
        return(0)
      }
      if (summary == "mean") {
        return(mean(values) * (length(values) / n))
      }
      # Find the middle order statistics with implicit zeros. This also handles
      # negative scores and even fold counts without allocating the missing rows.
      negative <- sort(values[values < 0])
      positive <- sort(values[values > 0])
      last_zero <- n - length(positive)
      middle <- c(floor((n + 1) / 2), ceiling((n + 1) / 2))
      mean(vapply(
        middle,
        function(i) {
          if (i <= length(negative)) {
            negative[[i]]
          } else if (i <= last_zero) {
            0
          } else {
            positive[[i - last_zero]]
          }
        },
        numeric(1L)
      ))
    },
    numeric(1L),
    USE.NAMES = FALSE
  )
  keep <- which(!is.na(scores))
  if (!length(keep)) {
    abort(
      "The selected measure has no available scores; select another measure ",
      "or supply finite importance values.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  rank <- if (rank_by == "magnitude") abs(scores[keep]) else scores[keep]
  keep <- keep[order(if (decreasing) -rank else rank, seq_along(rank))]
  if (!is.null(top_n)) {
    keep <- head(keep, top_n)
  }
  data.frame(
    variable = variables[keep],
    importance = scores[keep],
    n_available = n_available[keep],
    n_folds = n_folds,
    n_zero = n_zero[keep]
  )
}


# %% draw_varimp ----
#' Draw Variable Importance
#'
#' Draw signed importance summaries with [BarConfig], or fold distributions
#' with [BoxplotConfig] using `type = "boxplot"`. Numeric input
#' uses its names as variable labels, or row numbers when unnamed. Tables
#' preserve named measures and optionally identify resamples with `fold`.
#'
#' @details
#' Resampled scores are summarized before selection. Magnitude ranking uses
#' the absolute summary, not the mean absolute fold score. Signed ranking uses
#' the summary itself. `decreasing = FALSE` selects the smallest ranks first.
#' Ties retain first appearance in the input. The first selected variable
#' appears at the top of horizontal bars or the left of
#' vertical bars or boxes. Zero scores are retained. For distributions, the
#' summary controls selection and ordering only; boxes use the fold values.
#' Ranking direction is never inferred from a measure name. For smaller-is-better
#' scores, use `rank_by = "signed", decreasing = FALSE`. Averaging fold p-values
#' does not produce a combined p-value; choose a suitable summary outside this
#' function when the score's interpretation requires it.
#'
#' Set `bar_width` to a pixel thickness for separate zero-to-score segments.
#' This uses native bars and covers the legacy importance `type = "line"`
#' geometry without connecting different variables or adding a new chart type.
#'
#' Explicit NA scores and wholly unavailable folds are excluded from each
#' variable's summary. Variables with no available score are omitted.
#' `absent = "zero"` declares that omitted variable rows mean known zero scores
#' within folds reporting that measure. It never replaces an explicit NA or a
#' wholly unavailable fold/measure with zero. Use it only for sparse importance
#' tables whose producer omits zero entries. Infinite scores are rejected.
#'
#' Means and medians give each contributing fold equal weight. An incomplete
#' summary describes the available folds, not an estimate guaranteed to be
#' unbiased for all folds. Incomplete fold coverage is reported in the console;
#' chart labels retain only the variable and measure names. The input is never
#' modified.
#'
#' Boxplots require fold-level records. Every available score (including known
#' structural zeros) is overlaid by default, with its fold ID in the tooltip.
#' Missing values remain missing in the materialized table and are reported in
#' the console. Boxes describe resample variability, not a confidence interval.
#' See [draw_boxplot()] for quartiles, whiskers, and point placement. Use
#' `whisker = 0` for full-range whiskers, as in the current live importance view.
#'
#' For portable rendering, materialize the selected, summarized `label`
#' and `importance` columns in display order and bind them using
#' `setup_BarConfig(x = "label", y = "importance")`. Preserve variable IDs and
#' contributing/total fold counts alongside these columns. Set `horizontal`,
#' axis labels, and title explicitly. That table and config reproduce the view
#' without a fitted model or R callbacks. The raw-data summary and selection
#' options are not yet part of a shared visualization schema. For distributions,
#' use one numeric column per selected variable and an observation column for
#' the complete fold universe. Bind those columns and the variable
#' labels through [setup_BoxplotConfig()]. Only selected variables are widened;
#' selection does not discard folds needed to identify structural zeros.
#'
#' @param x Numeric vector, single-column matrix, or data frame: Importance
#'   scores. Tables require `variable` and named numeric measures, optionally
#'   `fold`. Variable names must be unique within each fold.
#' @param measure Optional Character: Measure column. NULL selects the first
#'   column other than `variable` or `fold`; vectors use `"importance"`.
#' @param top_n Optional Integer `[1, Inf)`: Maximum number of variables.
#'   NULL includes all variables with a summary; fractions are not accepted.
#' @param rank_by Character \{"magnitude", "signed"\}: Rank by absolute or signed
#'   summary, respectively.
#' @param decreasing Logical: Select and display ranks from largest to smallest.
#'   FALSE selects and displays the smallest ranks first.
#' @param summary Character \{"mean", "median"\}: Summary across folds.
#' @param absent Character \{"missing", "zero"\}: Meaning of omitted variable
#'   rows within a fold reporting the selected measure.
#' @param folds Optional Character: Full set of fold IDs, including folds with
#'   no rows. NULL uses the IDs observed in the `fold` column.
#' @param horizontal Logical: Draw horizontal bars or boxes.
#' @param type Character \{"bar", "boxplot"\}: Summary or fold-distribution view.
#' @param xlab,ylab Optional Character: Physical axis labels. NULL derives the
#'   score label from the measure and summary and labels the variable axis.
#' @param title Optional Character: Chart title.
#' @inheritParams draw_bar
#' @param theme Optional [Theme]: Theme override. Set `palette` in `...` to
#'   override its series colors.
#' @param height Optional Character or Numeric: Widget height. NULL allocates
#'   space per selected variable for horizontal bars.
#' @param ... Additional settings for [setup_BarConfig()] or
#'   [setup_BoxplotConfig()], according to `type`. Boxplots accept `boxpoints`,
#'   `quartiles`, `whisker`, and point styling; unset `boxpoints` shows all scores.
#'   Data bindings, labels, and orientation are set here.
#' @return htmlwidget: ECharts importance bars or fold distributions.
#' @export
#' @examples
#' draw_varimp(c(age = 0.8, weight = -0.4, height = 0.2))
#' scores <- data.frame(
#'   variable = c("age", "weight", "age"),
#'   fold = c("A", "A", "B"),
#'   gain = c(0.8, 0.4, 0.6)
#' )
#' draw_varimp(scores, measure = "gain", absent = "zero")
#' draw_varimp(scores, measure = "gain", type = "boxplot", absent = "zero")
#' @inheritParams draw_line legend_position legend_placement
draw_varimp <- function(
  x,
  measure = NULL,
  top_n = 20L,
  rank_by = "magnitude",
  summary = "mean",
  absent = "missing",
  folds = NULL,
  horizontal = TRUE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  type = "bar",
  decreasing = TRUE,
  ...,
  legend_position = "top",
  legend_placement = "outside"
) {
  check_character_scalar(type)
  check_enum(type, c("bar", "boxplot"))
  data <- varimp_data(x)
  measure <- varimp_measure(data, measure)
  values <- summarize_varimp(
    data,
    measure = measure,
    top_n = top_n,
    rank_by = rank_by,
    decreasing = decreasing,
    summary = summary,
    absent = absent,
    folds = folds
  )
  check_logical_scalar(horizontal)
  score_label <- labelify(
    if (type == "bar" && "fold" %in% names(data)) {
      paste(summary, measure)
    } else {
      measure
    }
  )
  values[["label"]] <- values[["variable"]]
  incomplete <- values[["n_available"]] < values[["n_folds"]]
  if (type == "bar" && any(incomplete)) {
    msg(
      "Importance summaries use available folds:",
      paste0(
        values[["variable"]][incomplete],
        " (",
        values[["n_available"]][incomplete],
        "/",
        values[["n_folds"]][incomplete],
        " folds)",
        collapse = "; "
      )
    )
  }
  if (horizontal) {
    values <- values[rev(seq_len(nrow(values))), , drop = FALSE]
  }
  if (type == "boxplot") {
    records <- varimp_distribution(data, values, measure, absent, folds)
    settings <- list(...)
    reserved <- intersect(names(settings), c("group", "observation", "labels"))
    if (length(reserved)) {
      abort(
        "Omit distribution bindings from `...`: ",
        paste(reserved, collapse = ", "),
        ". draw_varimp() sets them from variable and fold identities.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (!"boxpoints" %in% names(settings)) {
      settings[["boxpoints"]] <- "all"
    }
    config <- do.call(
      setup_BoxplotConfig,
      c(
        list(
          x = records[["columns"]],
          group = NULL,
          observation = records[["observation"]],
          labels = values[["label"]],
          horizontal = horizontal,
          xlab = xlab %||% if (horizontal) score_label else "Variable",
          ylab = ylab %||% if (horizontal) "Variable" else score_label,
          title = title,
          legend_position = legend_position,
          legend_placement = legend_placement
        ),
        settings
      )
    )
    plot_data <- records[["data"]]
  } else {
    config <- setup_BarConfig(
      x = "label",
      y = "importance",
      horizontal = horizontal,
      xlab = xlab %||% if (horizontal) score_label else "Variable",
      ylab = ylab %||% if (horizontal) "Variable" else score_label,
      title = title,
      legend_position = legend_position,
      legend_placement = legend_placement,
      ...
    )
    plot_data <- values
  }
  draw(
    config,
    data = plot_data,
    theme = theme,
    width = width,
    height = height %||%
      if (horizontal) max(400, 160 + 24 * nrow(values)) else 400,
    element_id = element_id,
    filename = filename
  )
}
