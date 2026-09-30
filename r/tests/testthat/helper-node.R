# helper-node.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Tests that run Node.js (SVG export and the JavaScript geometry fixtures) run
# only off CRAN and where `node` meets the version save_drawing() requires.
# CRAN check machines are not required to have a current Node.js, so a failure
# there would reflect the machine rather than the package.

# %% node_ok ----
#' Whether a usable Node.js binary is available
#'
#' @return Logical scalar: TRUE when `find_node()` succeeds.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_ok <- function() {
  tryCatch(
    {
      find_node()
      TRUE
    },
    rtemis_export_error = function(e) FALSE
  )
} # /node_ok


# %% node_tests_enabled ----
#' Whether Node.js tests should run
#'
#' Non-skipping counterpart of `skip_if_no_node()`, for a test whose Node.js
#' step is one part of a larger test.
#'
#' @return Logical scalar: TRUE off CRAN when a usable Node.js is available.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_tests_enabled <- function() {
  (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) && node_ok()
} # /node_tests_enabled


# %% skip_if_no_node ----
#' Skip a test on CRAN or without a usable Node.js
#'
#' @return Invisible TRUE when the test continues; signals a skip otherwise.
#'
#' @author EDG
#' @keywords internal
#' @noRd
skip_if_no_node <- function() {
  skip_on_cran()
  skip_if_not(node_ok(), "Node.js (>= 18) not found")
} # /skip_if_no_node
