# measure.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# What a value axis measures: the rows a host combines before an
# @rtemis/draw builder draws them, and the axis name of the measure.
# spec: draw/shared-builders#bar

# %% aggregate_rows ----
#' Combine the rows that share their key columns
#'
#' One row per distinct combination of the key columns, in order of first
#' appearance, with each value column summarized by `fn`. Missing values are
#' left out of each summary. Factor key columns keep their levels.
#'
#' @param data Data frame: The chart data.
#' @param keys Character: Key columns: a bar's category and group, or a
#'   line's x and group.
#' @param values Character: Value columns.
#' @param fn Optional Character \{"sum", "mean", "median"\}: Summary of the
#'   value columns.
#' @param count Optional Character: Name of a column to hold each
#'   combination's number of rows.
#'
#' @return Data frame: The key and value columns, one row per combination.
#'
#' @author EDG
#' @keywords internal
#' @noRd
aggregate_rows <- function(data, keys, values, fn = NULL, count = NULL) {
  summary <- if (length(values) > 0L) {
    switch(
      fn,
      sum = function(v) sum(v, na.rm = TRUE),
      mean = function(v) {
        if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
      },
      median = function(v) {
        if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE)
      }
    )
  }
  key <- do.call(
    paste,
    c(lapply(keys, function(k) as.character(data[[k]])), sep = "\r")
  )
  first <- !duplicated(key)
  out <- lapply(keys, function(k) data[[k]][first])
  names(out) <- keys
  index <- match(key, key[first])
  for (v in values) {
    out[[v]] <- vapply(
      split(data[[v]], factor(index, levels = seq_len(sum(first)))),
      summary,
      numeric(1)
    )
  }
  if (!is.null(count)) {
    out[[count]] <- tabulate(index, nbins = sum(first))
  }
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
} # /rtemis.draw::aggregate_rows


# %% value_label ----
#' Label for a chart's value axis
#'
#' The measure a chart shows, as `@rtemis/draw`'s `valueLabel()` names it:
#' "Count" with `y` unset; one `y` column gives its name, prefixed by the
#' aggregate ("Mean X", "Median X", "Sum of X"); several give the aggregate
#' alone, or none without one, since the series legend names each.
#'
#' @param config [BarConfig] or [LineConfig]: A config with `y` and
#'   `aggregate`.
#'
#' @return Character, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
value_label <- function(config) {
  if (is.null(config@y)) {
    return("Count")
  }
  only <- if (length(config@y) == 1L) config@y
  switch(
    config@aggregate,
    mean = paste(c("Mean", only), collapse = " "),
    median = paste(c("Median", only), collapse = " "),
    sum = if (is.null(only)) "Sum" else paste("Sum of", only),
    only
  )
} # /rtemis.draw::value_label
