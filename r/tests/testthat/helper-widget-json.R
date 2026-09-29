# helper-widget-json.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Serialization tests need the JSON that htmlwidgets actually sends to the
# browser, which R objects cannot show (e.g. a named palette becomes a JSON
# object). htmlwidgets does not export its serializer, and `:::` into another
# package breaks whenever its internals change, so these helpers render the
# widget through the public htmltools API and read the payload back out of the
# `<script type="application/json">` tag the browser reads.

#' Widget payload JSON as sent to the browser
#'
#' @param w htmlwidget: Widget to render.
#'
#' @return Character: JSON text of the htmlwidgets payload,
#'   `{"x": ..., "evals": ..., "jsHooks": ...}`, where `x` is the widget data.
#'
#' @keywords internal
#' @noRd
widget_payload_json <- function(w) {
  html <- as.character(htmltools::renderTags(w)[["html"]])
  m <- regmatches(
    html,
    regexec(
      '<script type="application/json" data-for="[^"]+">(.*?)</script>',
      html,
      perl = TRUE
    )
  )[[1L]]
  if (length(m) < 2L) {
    stop("No htmlwidgets JSON payload found in the rendered widget.")
  }
  m[[2L]]
}

#' Widget data as the browser receives it
#'
#' @param w htmlwidget: Widget to render.
#'
#' @return List: The payload's `x` element parsed back from JSON with
#'   `simplifyVector = FALSE`, so JSON arrays stay lists and scalars stay
#'   scalars exactly as the browser sees them.
#'
#' @keywords internal
#' @noRd
widget_wire <- function(w) {
  jsonlite::fromJSON(widget_payload_json(w), simplifyVector = FALSE)[["x"]]
}
