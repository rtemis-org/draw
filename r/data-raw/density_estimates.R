# density_estimates.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Writes typescript/tests/fixtures/density-estimates.json: samples, the
# summary a host passes to `kernelDensity()`, and the curves
# `stats::density()` computes for every kernel under the nrd0 and nrd
# bandwidth rules and two `adjust` values, at 64 points. @rtemis/draw's tests
# bin the samples as a host does and compare its estimates with these.
# Run from r/: Rscript data-raw/density_estimates.R

set.seed(2026)
samples <- list(
  normal = rnorm(300, mean = 10, sd = 2),
  bimodal = c(rnorm(150, -2, 0.6), rnorm(100, 3, 1)),
  skewed = rexp(200, rate = 0.5),
  penguins = as.numeric(stats::na.omit(datasets::penguins[["bill_len"]]))
)
samples <- lapply(samples, signif, digits = 5)
kernels <- c(
  "gaussian",
  "epanechnikov",
  "rectangular",
  "triangular",
  "biweight",
  "cosine",
  "optcosine"
)

cases <- list()
for (name in names(samples)) {
  x <- samples[[name]]
  quartiles <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
  summary <- list(
    n = length(x),
    min = min(x),
    max = max(x),
    sd = stats::sd(x),
    q1 = quartiles[[1L]],
    q3 = quartiles[[2L]]
  )
  for (kernel in kernels) {
    for (bw in c("nrd0", "nrd")) {
      for (adjust in c(1, 2)) {
        d <- stats::density(
          x,
          bw = bw,
          adjust = adjust,
          kernel = kernel,
          n = 64
        )
        cases[[length(cases) + 1L]] <- list(
          sample = name,
          summary = summary,
          kernel = kernel,
          bw = bw,
          adjust = adjust,
          bandwidth = d[["bw"]],
          x = d[["x"]],
          y = d[["y"]]
        )
      }
    }
  }
}

out <- file.path(
  "..",
  "typescript",
  "tests",
  "fixtures",
  "density-estimates.json"
)
writeLines(
  jsonlite::toJSON(
    list(samples = samples, cases = cases),
    auto_unbox = TRUE,
    digits = I(17),
    pretty = FALSE
  ),
  out
)
