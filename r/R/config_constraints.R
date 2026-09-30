# config_constraints.R
# Shared declarative constraints used by S7 validation and JSON Schema.

#' Match the supported JSON Schema constraint vocabulary
#'
#' Property types and numeric bounds remain owned by prop_* declarations.
#' This deliberately small interpreter handles supplementary structural rules,
#' not arbitrary JSON Schema or data-dependent statistical validation.
#' @param value Any: Property value or named configuration list.
#' @param rule List: Structural JSON Schema rule.
#' @return Logical: Whether the value satisfies the rule.
#' @keywords internal
#' @noRd
config_matches <- new_generic("config_matches", "value")
method(config_matches, class_any) <- function(value, rule) {
  supported <- c(
    "allOf",
    "anyOf",
    "not",
    "if",
    "then",
    "properties",
    "required",
    "type",
    "const",
    "enum",
    "minItems",
    "maxItems",
    "pattern"
  )
  if (length(setdiff(names(rule), supported))) {
    abort(
      "Use only the documented structural configuration-rule keywords.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(rule[["type"]])) {
    if (!identical(rule[["type"]], "null")) {
      abort(
        "Declare property types with prop_*; constraint type supports null only.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (!is.null(value)) return(FALSE)
  }
  if (
    "const" %in%
      names(rule) &&
      !isTRUE(all.equal(value, rule[["const"]], check.attributes = FALSE))
  ) {
    return(FALSE)
  }
  if (
    !is.null(rule[["enum"]]) &&
      !any(vapply(
        rule[["enum"]],
        function(x) {
          isTRUE(all.equal(value, x, check.attributes = FALSE))
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  if (
    !is.null(rule[["required"]]) && !all(rule[["required"]] %in% names(value))
  ) {
    return(FALSE)
  }
  properties <- rule[["properties"]]
  for (name in intersect(names(properties), names(value))) {
    if (!config_matches(value[[name]], properties[[name]])) return(FALSE)
  }
  if (!is.null(value)) {
    for (key in c("minItems", "maxItems")) {
      limit <- rule[[key]]
      if (
        !is.null(limit) &&
          if (key == "minItems") {
            length(value) < limit
          } else {
            length(value) > limit
          }
      ) {
        return(FALSE)
      }
    }
    if (
      !is.null(rule[["pattern"]]) &&
        !all(grepl(rule[["pattern"]], value, perl = TRUE))
    ) {
      return(FALSE)
    }
  }
  if (!is.null(rule[["not"]]) && config_matches(value, rule[["not"]])) {
    return(FALSE)
  }
  if (
    !is.null(rule[["allOf"]]) &&
      !all(vapply(
        rule[["allOf"]],
        function(x) config_matches(value, x),
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  if (
    !is.null(rule[["anyOf"]]) &&
      !any(vapply(
        rule[["anyOf"]],
        function(x) config_matches(value, x),
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  if (
    !is.null(rule[["if"]]) &&
      config_matches(value, rule[["if"]]) &&
      !is.null(rule[["then"]]) &&
      !config_matches(value, rule[["then"]])
  ) {
    return(FALSE)
  }
  TRUE
}

#' Construct a validator carrying its structural schema rules
#' @param rules List: Entries containing schema and corrective message.
#' @param extra Optional function: Data or relational checks beyond JSON Schema.
#' @return Function: S7 validator with inspectable schema_rules metadata.
#' @keywords internal
#' @noRd
config_validator <- new_generic("config_validator", "rules")
method(config_validator, class_list) <- function(rules, extra = NULL) {
  validator <- function(self) {
    properties <- names(S7_class(self)@properties)
    values <- setNames(
      lapply(properties, function(name) prop(self, name)),
      properties
    )
    errors <- unlist(
      lapply(rules, function(rule) {
        if (!config_matches(values, rule[["schema"]])) rule[["message"]]
      }),
      use.names = FALSE
    )
    if (!is.null(extra)) {
      errors <- c(errors, extra(self))
    }
    if (length(errors)) errors else NULL
  }
  attr(validator, "schema_rules") <- lapply(rules, `[[`, "schema")
  validator
}

# Axis-limit cardinality is structural; ordering remains a runtime comparison.
CONFIG_LIMIT_RULES <- list(list(
  schema = list(
    properties = list(xlim = list(maxItems = 2L), ylim = list(maxItems = 2L))
  ),
  message = "Supply exactly two increasing finite axis limits."
))

# Learner arguments mean nothing without a learner to pass them to.
FIT_PARAMS_RULE <- list(
  schema = list(
    `if` = list(
      required = list("fit_params"),
      properties = list(fit_params = list(not = list(type = "null")))
    ),
    then = list(
      properties = list(fit = list(not = list(type = "null")))
    )
  ),
  message = "Set fit to the learner that fit_params configure."
)

#' Check ordered axis limits after property validation
#' @param config ChartConfig: Configuration containing xlim and ylim.
#' @return Optional Character: Corrective validation messages.
#' @keywords internal
#' @noRd
config_ordered_limits <- new_generic("config_ordered_limits", "config")
method(config_ordered_limits, class_any) <- function(config) {
  errors <- character()
  for (name in c("xlim", "ylim")) {
    value <- prop(config, name)
    if (!is.null(value) && length(value) == 2L && value[[1L]] >= value[[2L]]) {
      errors <- c(
        errors,
        paste0("@", name, " must contain two increasing finite limits")
      )
    }
  }
  if (length(errors)) errors else NULL
}
