# Extracted from test-draw_roc.R:210

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rtemis.draw", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
roc_test_labels <- function() {
  factor(c("no", "yes", "no", "yes"), levels = c("no", "yes"))
}
roc_test_records <- function() roc_input(roc_test_labels(), c(.1, .8, .5, .5))
roc_test_config <- function(...) {
  setup_ROCConfig(
    auc = "auc",
    class_label = "class",
    split = "split",
    fold = "fold",
    omitted = "omitted",
    ...
  )
}

# test -------------------------------------------------------------------------
skip_if_no_node()
path <- tempfile(fileext = ".svg")
on.exit(unlink(path), add = TRUE)
widget <- draw_roc(roc_test_records(), palette = "#123456", filename = path)
text <- paste(readLines(path, warn = FALSE), collapse = "\n")
expect_match(text, "yes 0.88", fixed = TRUE)
expect_match(text, ">AUC<", fixed = TRUE)
expect_true(widget[["x"]][["legendTitleAbove"]])
expect_match(text, 'stroke="#123456"')
expect_match(text, 'stroke="#888888"')
