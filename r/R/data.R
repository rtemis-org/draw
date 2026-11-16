# data.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Synthetic example datasets, defined by formulas in data-raw/example_data.R
# so every interface reproduces them exactly.

#' Monthly Electricity Generation by Source
#'
#' Synthetic monthly electricity generation in 2024 from three sources. The
#' sources add up to the total generated, so the data stacks. Defined by
#' formulas in `data-raw/example_data.R`, and written with the same values as
#' the `@rtemis/draw` test fixtures.
#'
#' @format A data frame with 36 rows and 4 columns:
#' \describe{
#'   \item{month}{Date: First day of the month.}
#'   \item{quarter}{Factor \{"Q1", "Q2", "Q3", "Q4"\}: Quarter of the month.}
#'   \item{source}{Factor \{"Solar", "Wind", "Hydro"\}: Generation source.}
#'   \item{gwh}{Numeric: Electricity generated, in gigawatt-hours.}
#' }
#'
#' @source Synthetic.
#' @examples
#' draw_line(
#'   power_generation$month,
#'   power_generation$gwh,
#'   group = power_generation$source,
#'   ylab = "Generation (GWh)"
#' )
"power_generation"

#' Satisfaction Survey Responses
#'
#' Synthetic responses to a satisfaction survey at three sites, one row per
#' respondent, 20 respondents per site. Defined in `data-raw/example_data.R`.
#'
#' @format A data frame with 60 rows and 2 columns:
#' \describe{
#'   \item{site}{Factor \{"A", "B", "C"\}: Survey site.}
#'   \item{satisfaction}{Factor \{"Low", "Medium", "High"\}: Response, in
#'     increasing order.}
#' }
#'
#' @source Synthetic.
#' @examples
#' draw(
#'   setup_BarConfig(x = "site", group = "satisfaction"),
#'   data = satisfaction_survey
#' )
"satisfaction_survey"

#' Weekly Symptom Scores Across Study Phases
#'
#' A synthetic weekly symptom score, lower being better, over a four-week
#' baseline, a five-week treatment, and a three-week washout. Defined in
#' `data-raw/example_data.R`.
#'
#' @format A data frame with 12 rows and 3 columns:
#' \describe{
#'   \item{week}{Integer: Study week.}
#'   \item{phase}{Factor \{"Baseline", "Treatment", "Washout"\}: Study phase.}
#'   \item{score}{Numeric: Symptom score.}
#' }
#'
#' @source Synthetic.
#' @examples
#' draw(
#'   setup_LineConfig(
#'     x = "week",
#'     y = "score",
#'     blocks = "phase",
#'     block_color = c("#9E9E9E", "#2D83D3", "#C55E28"),
#'     points = TRUE
#'   ),
#'   data = symptom_scores
#' )
"symptom_scores"
