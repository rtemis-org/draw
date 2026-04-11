# utils.R
# Foundational utilities for the draw package:
# - snake_case to camelCase conversion for JSON serialization
# - NULL-dropping for clean list output
# - Type validators for S7 property constraints

# -- snake_case to camelCase conversion ------------------------------------------

#' Convert snake_case to camelCase
#'
#' Used internally by `to_list()` methods to convert R-idiomatic snake_case
#' property names to echarts-expected camelCase JSON keys.
#'
#' @param x Character string in snake_case.
#' @return Character string in camelCase.
#' @keywords internal
#' @examples
#' snake_to_camel("border_width")   # "borderWidth"
#' snake_to_camel("font_size")      # "fontSize"
#' snake_to_camel("color")          # "color" (no underscores, unchanged)
snake_to_camel <- function(x) {
  gsub("_(\\w)", "\\U\\1", x, perl = TRUE)
}

# -- List utilities --------------------------------------------------------------

#' Drop NULL values from a list
#'
#' Removes all NULL entries from a named list. Used by `to_list()` methods
#' to produce clean JSON (echarts treats missing keys as defaults).
#'
#' @param x A named list.
#' @return The list with NULL values removed.
#' @keywords internal
drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Convert an S7 object to an echarts-compatible named list
#'
#' Extracts all S7 properties, converts names from snake_case to camelCase,
#' recursively converts nested S7 objects, and drops NULLs.
#'
#' @param obj An S7 object.
#' @param rename Named character vector of exceptions where the R property name
#'   does not follow the standard snake_to_camel mapping. Format:
#'   `c(r_name = "echartsName")`.
#' @return A named list suitable for `jsonlite::toJSON()`.
#' @keywords internal
props_to_list <- function(obj, rename = NULL) {
  pnames <- names(S7::S7_class(obj)@properties)
  vals <- lapply(pnames, function(nm) S7::prop(obj, nm))
  names(vals) <- pnames

  # Drop NULLs
  vals <- drop_nulls(vals)

  if (length(vals) == 0L) return(list())

  # Convert names: apply explicit renames first, then snake_to_camel
  out_names <- names(vals)
  if (!is.null(rename)) {
    idx <- match(out_names, names(rename))
    has_rename <- !is.na(idx)
    out_names[has_rename] <- rename[idx[has_rename]]
  }
  out_names <- snake_to_camel(out_names)
  names(vals) <- out_names

  # Recursively convert S7 objects to lists
  vals <- lapply(vals, function(v) {
    if (S7::S7_inherits(v)) {
      to_list(v)
    } else if (is.list(v) && !is.null(names(v))) {
      # Named list: recurse into values
      lapply(v, function(vv) {
        if (S7::S7_inherits(vv)) to_list(vv) else vv
      })
    } else if (is.list(v)) {
      # Unnamed list (array): recurse into elements
      lapply(v, function(vv) {
        if (S7::S7_inherits(vv)) to_list(vv) else vv
      })
    } else {
      v
    }
  })

  vals
}

# -- Generic to_list method ------------------------------------------------------

#' Convert to echarts-compatible list
#'
#' Generic function that converts an S7 object to a plain named list
#' matching the ECharts JSON option structure.
#'
#' @param x An S7 object.
#' @param ... Additional arguments (unused).
#' @return A named list.
#' @export
to_list <- S7::new_generic("to_list", "x")

# -- Type validators for S7 properties -------------------------------------------
# These return validator functions or S7 class unions.

#' @keywords internal
nullable <- function(type) {
  type | S7::class_missing | NULL
}

#' Validate that a value is one of the allowed choices
#'
#' Returns a custom S7 property object with validation.
#'
#' @param values Character vector of allowed values.
#' @param default Default value (must be one of `values` or NULL).
#' @param nullable If TRUE, NULL is accepted.
#' @return An S7 property definition.
#' @keywords internal
enum_property <- function(values, default = NULL, nullable = TRUE) {
  S7::new_property(
    class = if (nullable) S7::class_any else S7::class_character,
    default = default,
    validator = function(value) {
      if (is.null(value) && nullable) return(NULL)
      if (!is.character(value) || length(value) != 1L || !(value %in% values)) {
        paste0("must be one of: ", paste(dQuote(values), collapse = ", "))
      }
    }
  )
}

#' Property that accepts a number or NULL
#' @keywords internal
numeric_or_null_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (!is.numeric(value) || length(value) != 1L) {
        "must be a single number or NULL"
      }
    }
  )
}

#' Property that accepts a string or NULL
#' @keywords internal
string_or_null_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (!is.character(value) || length(value) != 1L) {
        "must be a single string or NULL"
      }
    }
  )
}

#' Property that accepts a logical or NULL
#' @keywords internal
bool_or_null_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (!is.logical(value) || length(value) != 1L) {
        "must be TRUE, FALSE, or NULL"
      }
    }
  )
}

#' Property that accepts a number, string, or NULL
#'
#' Used for echarts fields that accept both pixel values (number) and
#' percentage strings (e.g. "50%").
#' @keywords internal
numeric_or_string_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (!is.numeric(value) && !is.character(value)) {
        "must be a number, string, or NULL"
      }
      if (length(value) != 1L) {
        "must be a single value"
      }
    }
  )
}

#' Convert a color to an rgba string with specified alpha
#'
#' Parses any R color specification (hex, named color, etc.) and returns
#' an `rgba()` CSS color string with the given alpha.
#'
#' @param color An R color string (e.g. `"#00b2b2"`, `"red"`).
#' @param alpha Alpha value between 0 and 1.
#' @return An `rgba()` color string.
#' @keywords internal
#' Calculate padded axis limits from data
#'
#' Computes `c(min, max)` from `values` with symmetric padding as a fraction
#' of the data range.
#'
#' @param values Numeric vector of data values (NAs ignored).
#' @param pad Fraction of the data range to add on each side.
#' @return A length-2 numeric vector `c(min, max)`.
#' @keywords internal
calc_limits <- function(values, pad = 0.04) {
  rng <- range(values, na.rm = TRUE)
  span <- rng[2] - rng[1]
  if (span == 0) span <- abs(rng[1]) * 0.1  # handle constant data
  c(rng[1] - pad * span, rng[2] + pad * span)
}

color_with_alpha <- function(color, alpha) {
  rgb <- grDevices::col2rgb(color)[, 1]
  sprintf("rgba(%d, %d, %d, %g)", rgb[1], rgb[2], rgb[3], alpha)
}

#' Property that accepts a color string, or NULL
#'
#' Currently accepts any string. Future versions may validate
#' hex, rgb(), rgba(), hsl(), or named CSS colors.
#' @keywords internal
color_property <- function(default = NULL) {
  string_or_null_property(default = default)
}

#' Property that accepts a character vector of colors, or NULL
#'
#' Used for color palettes (e.g. theme and option `color` fields).
#' @keywords internal
color_palette_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (is.character(value)) return(NULL)
      "must be a character vector of colors or NULL"
    }
  )
}

#' Property that accepts an S7 class instance or NULL
#' @keywords internal
class_or_null_property <- function(s7_class) {
  S7::new_property(
    class = S7::class_any,
    default = NULL,
    validator = function(value) {
      if (is.null(value)) return(NULL)
      if (!S7::S7_inherits(value, s7_class)) {
        paste0("must be a ", s7_class@name, " object or NULL")
      }
    }
  )
}
