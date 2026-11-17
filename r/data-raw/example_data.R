# example_data.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Synthetic example datasets, defined by formulas so every interface can
# reproduce them exactly. Writes the package datasets in data/ and the JSON
# fixtures @rtemis/draw's tests read in typescript/tests/fixtures/.
# Run from r/: Rscript data-raw/example_data.R

# Monthly electricity generation in 2024 by source, in GWh. The three sources
# add up to the total, so the data stacks.
month <- seq(as.Date("2024-01-01"), by = "month", length.out = 12L)
m <- 0:11
generation <- list(
  Solar = 40 + 30 * sin(2 * pi * (m - 2) / 12),
  Wind = 60 + 20 * cos(2 * pi * m / 12),
  Hydro = 50 + 10 * sin(2 * pi * (m - 3) / 12)
)
power_generation <- data.frame(
  month = rep(month, times = length(generation)),
  quarter = factor(
    rep(paste0("Q", (m %/% 3L) + 1L), times = length(generation)),
    levels = paste0("Q", 1:4)
  ),
  source = factor(
    rep(names(generation), each = length(month)),
    levels = names(generation)
  ),
  gwh = round(unlist(generation, use.names = FALSE), 1)
)

# Responses to a satisfaction survey at three sites, one row per respondent.
counts <- rbind(
  A = c(Low = 4L, Medium = 9L, High = 7L),
  B = c(Low = 8L, Medium = 8L, High = 4L),
  C = c(Low = 2L, Medium = 7L, High = 11L)
)
satisfaction_survey <- data.frame(
  site = factor(
    rep(rep(rownames(counts), each = ncol(counts)), times = t(counts)),
    levels = rownames(counts)
  ),
  satisfaction = factor(
    rep(rep(colnames(counts), times = nrow(counts)), times = t(counts)),
    levels = colnames(counts)
  )
)

# A weekly symptom score (lower is better) over a baseline, a treatment, and
# a washout phase.
symptom_scores <- data.frame(
  week = 1:12,
  phase = factor(
    rep(c("Baseline", "Treatment", "Washout"), times = c(4L, 5L, 3L)),
    levels = c("Baseline", "Treatment", "Washout")
  ),
  score = c(7.2, 7.5, 6.9, 7.3, 6.1, 5.2, 4.4, 3.8, 3.5, 4.6, 5.3, 5.9)
)

save(power_generation, file = "data/power_generation.rda", compress = "xz")
save(
  satisfaction_survey,
  file = "data/satisfaction_survey.rda",
  compress = "xz"
)
save(symptom_scores, file = "data/symptom_scores.rda", compress = "xz")

# Fixtures: the tabular data the builders take, as rtemis.draw writes it.
pkgload::load_all(quiet = TRUE)
fixtures <- "../typescript/tests/fixtures"
write_fixture <- function(data, columns, name, frequency = NULL) {
  json <- tabular_json(data, columns, frequency = frequency)
  writeLines(
    jsonlite::prettify(json, indent = 2),
    file.path(fixtures, paste0(name, ".json"))
  )
}
write_fixture(power_generation, c("month", "source", "gwh"), "power-generation")
quarterly <- aggregate_rows(
  power_generation,
  c("quarter", "source"),
  "gwh",
  "sum"
)
write_fixture(
  quarterly,
  c("quarter", "source", "gwh"),
  "power-generation-quarterly"
)
write_fixture(
  satisfaction_survey,
  c("site", "satisfaction"),
  "satisfaction-survey"
)
write_fixture(symptom_scores, c("week", "phase", "score"), "symptom-scores")

# Boxplot summaries, as rtemis.draw computes them for the boxplot builder.
write_summary <- function(request, name) {
  writeLines(
    jsonlite::prettify(request@data, indent = 2),
    file.path(fixtures, paste0(name, ".json"))
  )
}
write_summary(
  boxplot_option(
    iris["Sepal.Length"],
    group = iris[["Species"]],
    boxpoints = "outliers"
  ),
  "iris-sepal-length"
)
write_summary(
  boxplot_option(
    iris[c("Sepal.Length", "Petal.Length")],
    group = iris[["Species"]],
    geometry = "both",
    density_points = 32L
  ),
  "iris-lengths-violin"
)
# Synthetic paired scores of 12 subjects before and after an intervention.
subject <- sprintf("S%02d", 1:12)
before <- 50 + (1:12 * 7) %% 13
after <- before - 5 + (1:12) %% 4
scores <- data.frame(Before = before, After = after)
p <- stats::t.test(after, before, paired = TRUE)[["p.value"]]
write_summary(
  boxplot_option(
    scores,
    observation = subject,
    paired = TRUE,
    boxpoints = "all",
    comparisons = data.frame(
      from = "Before",
      to = "After",
      label = sprintf("paired t-test p = %s", format(signif(p, 2)))
    )
  ),
  "paired-scores"
)

# Scatter data and fits, as rtemis.draw computes them for the scatter builder.
# Bill length and depth of the Palmer penguins (datasets, R >= 4.5.0): within
# each species depth rises with length, and over all three it falls.
penguins <- stats::na.omit(datasets::penguins[c(
  "species",
  "bill_len",
  "bill_dep",
  "flipper_len",
  "body_mass"
)])
bills <- setup_ScatterConfig(
  x = "bill_len",
  y = "bill_dep",
  group = "species",
  fit = "glm",
  n_fit = 20L
)
write_summary(compile(bills, data = penguins), "penguins-bills")
bills@fit_per_group <- FALSE
write_summary(compile(bills, data = penguins), "penguins-bills-pooled")
write_fixture(
  penguins,
  c("bill_len", "bill_dep", "flipper_len", "body_mass"),
  "penguins-measures"
)
# Synthetic predictions of a quantity known exactly, for a true-versus-
# predicted plot.
truth <- seq(1, 20, by = 0.5)
predictions <- data.frame(
  true = truth,
  predicted = round(truth + 1.5 * sin(truth), 3)
)
write_summary(
  compile(
    setup_ScatterConfig(x = "true", y = "predicted", fit = "glm", n_fit = 20L),
    data = predictions
  ),
  "true-predicted"
)
