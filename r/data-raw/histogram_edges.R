# histogram_edges.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Writes typescript/tests/fixtures/histogram-edges.json: for samples of
# several sizes, scales, and shapes, the summary a host passes to
# `histogramEdges()` and the edges `graphics::hist()` computes for each
# `breaks` rule and suggested bin count, and `pretty()` cases with
# `min.n = 1`. @rtemis/draw's tests compare its port of the rules with them.
# Values carry at most five significant digits, so `nclass.FD()` reads the
# same quartiles as the summary.
# Run from r/: Rscript data-raw/histogram_edges.R

set.seed(2026)
samples <- list(
  normal_100 = rnorm(100),
  normal_1e4 = rnorm(1e4, mean = 50, sd = 12),
  uniform_37 = runif(37, -3, 7),
  exponential_500 = rexp(500, rate = 0.2),
  tiny_scale = rnorm(200, mean = 0.002, sd = 3e-4),
  large_scale = rnorm(300, mean = 2e6, sd = 4e5),
  negative = -rgamma(150, shape = 2, scale = 3),
  narrow = 100 + rnorm(80, sd = 0.01),
  integers = as.numeric(sample(1:12, 60, replace = TRUE)),
  two_values = rep(c(3, 4), c(5, 7)),
  constant = rep(2.5, 9),
  penguins = as.numeric(stats::na.omit(datasets::penguins[["bill_len"]]))
)
samples <- lapply(samples, signif, digits = 5)

cases <- lapply(names(samples), function(name) {
  x <- samples[[name]]
  quartiles <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
  edges <- function(breaks) {
    graphics::hist(x, breaks = breaks, plot = FALSE)[["breaks"]]
  }
  list(
    name = name,
    summary = list(
      n = length(x),
      min = min(x),
      max = max(x),
      sd = if (length(x) > 1L) stats::sd(x) else NULL,
      q1 = quartiles[[1L]],
      q3 = quartiles[[2L]]
    ),
    breaks = list(
      Sturges = edges("Sturges"),
      Scott = edges("Scott"),
      FD = edges("FD")
    ),
    bins = lapply(c(1L, 5L, 12L, 30L, 100L), function(b) {
      list(bins = b, edges = edges(b))
    })
  )
})

pretty_cases <- list(
  c(0, 1, 5),
  c(-1.37, 2.81, 7),
  c(1e-9, 3e-9, 4),
  c(123456, 123789, 10),
  c(-50, -2, 3),
  c(0, 0, 5),
  c(7, 7, 4),
  c(0.1, 0.30000000000000004, 2)
)
pretty <- lapply(pretty_cases, function(p) {
  list(
    lo = p[[1L]],
    hi = p[[2L]],
    n = p[[3L]],
    edges = base::pretty(c(p[[1L]], p[[2L]]), n = p[[3L]], min.n = 1L)
  )
})

out <- file.path(
  "..",
  "typescript",
  "tests",
  "fixtures",
  "histogram-edges.json"
)
writeLines(
  jsonlite::toJSON(
    list(cases = cases, pretty = pretty),
    auto_unbox = TRUE,
    digits = I(17),
    null = "null",
    pretty = TRUE
  ),
  out
)
