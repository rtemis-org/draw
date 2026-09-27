test_that("survfit adapters preserve labels, estimates, start times and censor counts", {
  skip_if_not_installed("survival")
  fit <- survival::survfit(
    survival::Surv(time, status) ~ sex,
    data = survival::lung
  )
  d <- survfit_data(fit)[["curves"]]
  expect_identical(unique(d[["group"]]), names(fit[["strata"]]))
  f0 <- survival::survfit0(fit)
  expect_equal(d[["time"]], f0[["time"]])
  expect_equal(d[["survival"]], f0[["surv"]])
  expect_equal(d[["lower"]], f0[["lower"]])
  expect_equal(d[["n_censor"]], f0[["n.censor"]])
  groups <- split(d, d[["group"]])
  med <- vapply(groups, survival_median, numeric(1))
  expect_equal(
    unname(med),
    as.numeric(stats::quantile(fit, probs = .5, conf.int = FALSE))
  )
  conditional <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = survival::lung,
    start.time = 100
  )
  expect_equal(min(survfit_data(conditional)[["curves"]][["time"]]), 100)
  expect_no_error(draw_survfit(conditional))
  expect_error(draw_survfit(list()), "survfit")
  expect_error(draw_survfit(fit, risk_table = NA))
})

test_that("automatic right-censored risk counts agree with actual risk sets", {
  skip_if_not_installed("survival")
  raw <- data.frame(
    time = c(1, 2, 2, 4, 5, 8),
    event = c(1, 0, 1, 1, 0, 0),
    group = c("A", "A", "A", "B", "B", "B")
  )
  fit <- survival::survfit(survival::Surv(time, event) ~ group, raw)
  times <- c(0, 1, 1.5, 2, 3, 5, 8)
  risk <- survfit_data(fit, times)[["risk"]]
  for (i in seq_len(nrow(risk))) {
    group <- sub("group=", "", risk[["group"]][[i]], fixed = TRUE)
    expected <- sum(
      raw[["group"]] == group & raw[["time"]] >= risk[["time"]][[i]]
    )
    expect_equal(risk[["n_risk"]][[i]], expected)
  }
  expect_no_error(draw_survfit(fit, risk_times = times))
  expect_no_error(draw_survfit(fit, risk_table = TRUE))
  expect_error(survfit_data(fit, -1), "range")
})

test_that("delayed-entry fits do not fabricate risk sets between recorded times", {
  skip_if_not_installed("survival")
  raw <- data.frame(start = c(0, 2, 5), stop = c(4, 6, 9), event = c(1, 1, 0))
  fit <- survival::survfit(survival::Surv(start, stop, event) ~ 1, raw)
  records <- survfit_data(fit)
  expect_true(is.na(records[["curves"]][["n_risk"]][[1L]]))
  expect_no_error(draw_survfit(fit))
  expect_error(draw_survfit(fit, risk_times = c(1, 3)), "explicit risk")
  times <- c(1, 3, 5, 7)
  records[["risk"]] <- data.frame(
    time = times,
    group = "Survival",
    n_risk = vapply(
      times,
      function(t) sum(raw[["start"]] < t & raw[["stop"]] >= t),
      integer(1)
    )
  )
  expect_no_error(draw_survival(records, risk_table = TRUE))
})

test_that("unavailable confidence bounds and non-scalar survival fits are explicit", {
  skip_if_not_installed("survival")
  raw <- data.frame(time = c(1, 2), status = c(1, 1))
  fit <- survival::survfit(survival::Surv(time, status) ~ 1, raw)
  expect_no_error(draw_survfit(fit))
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    raw,
    conf.type = "none"
  )
  expect_false("lower" %in% names(survfit_data(fit)[["curves"]]))
  expect_no_error(draw_survfit(fit))
  fit[["surv"]] <- matrix(fit[["surv"]], ncol = 1)
  expect_error(draw_survfit(fit), "select one")
})

test_that("weighted risk sets and events at the starting time are preserved", {
  skip_if_not_installed("survival")
  raw <- data.frame(
    time = c(0, 1, 3),
    status = c(1, 0, 1),
    weight = c(.5, 1.5, 2)
  )
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    raw,
    weights = weight
  )
  out <- survfit_data(fit, c(0, .5, 3))
  expect_equal(out[["risk"]][["n_risk"]], c(4, 3.5, 2))
  expect_equal(out[["curves"]][["survival"]][[1L]], .875)
  expect_false(anyDuplicated(out[["curves"]][["time"]]) > 0)
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    transform(raw, time = time - 2)
  )
  expect_equal(min(survfit_data(fit)[["curves"]][["time"]]), -2)
})
