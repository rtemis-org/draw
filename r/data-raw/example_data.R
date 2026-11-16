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
