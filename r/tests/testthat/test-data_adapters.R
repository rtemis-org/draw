# test-data_adapters.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

test_that("model packages can prepare drawing data through public generics", {
  paired <- getExportedValue("rtemis.draw", "true_pred_data")
  importance <- getExportedValue("rtemis.draw", "varimp_data")
  confusion <- getExportedValue("rtemis.draw", "confusion_input")
  probabilities <- getExportedValue("rtemis.draw", "roc_probabilities")
  vertices <- getExportedValue("rtemis.draw", "roc_vertices")

  expect_equal(paired(1:2, c(1.2, 1.8))[["true"]], 1:2)
  expect_error(paired(1:2, 1:3), class = "rtemis_input_error")
  expect_identical(importance(c(age = 2))[["variable"]], "age")
  expect_error(importance("bad"), class = "rtemis_input_error")
  y <- factor(c("no", "yes"), levels = c("no", "yes"))
  expect_equal(sum(confusion(y, y)[["n"]]), 2)
  expect_error(confusion(y, y[1]), class = "rtemis_input_error")
  normalized <- probabilities(y, c(.1, .9))
  expect_equal(unique(vertices(normalized)[["auc"]]), 1)
  expect_error(probabilities(y, c(-1, 2)), class = "rtemis_input_error")
})

test_that("assembled ROC records preserve row and class identities", {
  x <- roc_probabilities(factor(c("no", "yes")), c(.1, .9))
  for (bad in list(
    list(),
    within(x, y <- "no"),
    within(x, classes <- "unknown"),
    within(x, prob <- prob[, 2:1, drop = FALSE]),
    within(x, prob[1, 1] <- Inf)
  )) {
    expect_error(roc_vertices(bad), class = "rtemis_input_error")
  }
  empty <- x
  empty[["y"]] <- character()
  empty[["prob"]] <- x[["prob"]][FALSE, , drop = FALSE]
  expect_true(is.na(roc_vertices(empty)[["auc"]]))
})
