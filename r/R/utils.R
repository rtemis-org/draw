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
#' @param x Character: String in snake_case.
#' @return Character: String in camelCase.
#' @keywords internal
#' @noRd
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
#' @param x List: Named list.
#' @return List: Input list with `NULL` values removed.
#' @keywords internal
#' @noRd
drop_nulls <- function(x) {
  # Also drop zero-length values: S7 0.2.2 stores empty prototypes (e.g. logical(0))
  # instead of NULL for new_union(type, NULL) properties with default = NULL.
  x[!vapply(x, function(v) is.null(v) || length(v) == 0L, logical(1))]
}

#' Convert an S7 object to an echarts-compatible named list
#'
#' Extracts all S7 properties, converts names from snake_case to camelCase,
#' recursively converts nested S7 objects, and drops NULLs.
#'
#' @param obj S7 object: Object to serialize.
#' @param rename Optional Named character vector: Exceptions where the R property name
#'   does not follow the standard snake_to_camel mapping. Format:
#'   `c(r_name = "echartsName")`.
#' @return List: Named list suitable for `jsonlite::toJSON()`.
#' @keywords internal
#' @noRd
props_to_list <- function(obj, rename = NULL) {
  pnames <- names(S7::S7_class(obj)@properties)
  vals <- lapply(pnames, function(nm) S7::prop(obj, nm))
  names(vals) <- pnames

  # Drop NULLs
  vals <- drop_nulls(vals)

  if (length(vals) == 0L) {
    return(list())
  }

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
#' @param x S7 object: Object to serialize.
#' @param ... Dots: Unused.
#' @return List: Named list.
#' @export
to_list <- S7::new_generic("to_list", "x")

# -- Type validators for S7 properties -------------------------------------------
# These return validator functions or S7 class unions.

#' Property that accepts a non-negative numeric scalar, with a default
#'
#' For required style values (e.g. node size, edge scale) that always carry a
#' sensible default in a render-spec option object.
#' @param default Numeric: Default value.
#' @keywords internal
#' @noRd
nonneg_numeric_default <- function(default) {
  S7::new_property(
    class = S7::class_numeric,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || is.na(value) || value < 0) {
        return("must be a single non-negative number")
      }
      NULL
    }
  )
}

#' Property that accepts a probability scalar in \[0, 1\], with a default
#' @param default Numeric \[0, 1\]: Default value.
#' @keywords internal
#' @noRd
prob_default <- function(default) {
  S7::new_property(
    class = S7::class_numeric,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || is.na(value) || value < 0 || value > 1) {
        return("must be a single number in [0, 1]")
      }
      NULL
    }
  )
}

#' Property that accepts a logical scalar, with a default
#' @param default Logical: Default value.
#' @keywords internal
#' @noRd
logical_default <- function(default) {
  S7::new_property(
    class = S7::class_logical,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || is.na(value)) {
        return("must be a single logical (TRUE or FALSE)")
      }
      NULL
    }
  )
}

#' Property that accepts one of a fixed set of strings, with a default
#'
#' For render-spec enum fields (e.g. classification, color scheme, corner).
#' @param choices Character: Allowed values.
#' @param default Character: Default value (must be in `choices`).
#' @keywords internal
#' @noRd
map_enum_default <- function(choices, default) {
  force(choices)
  S7::new_property(
    class = S7::class_character,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || !value %in% choices) {
        return(paste0(
          "must be one of ",
          paste0("\"", choices, "\"", collapse = ", ")
        ))
      }
      NULL
    }
  )
}

#' Property that accepts a number or NULL
#' @keywords internal
#' @noRd
numeric_or_null_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (!is.numeric(value) || length(value) != 1L) {
        "must be a single number or NULL"
      }
    }
  )
}

#' Property that accepts a number, string, or NULL
#'
#' Used for echarts fields that accept both pixel values (number) and
#' percentage strings (e.g. "50%").
#' @keywords internal
#' @noRd
numeric_or_string_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
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
#' @param color Character: R color string.
#' @param alpha Numeric `[0, 1]`: Alpha value.
#' @return Character: `rgba()` color string.
#' @keywords internal
#' @noRd
color_with_alpha <- function(color, alpha) {
  rgb <- grDevices::col2rgb(color)[, 1]
  sprintf("rgba(%d, %d, %d, %g)", rgb[1], rgb[2], rgb[3], alpha)
}


#' Calculate padded axis limits from data
#'
#' Computes `c(min, max)` from `values` with symmetric padding as a fraction
#' of the data range.
#'
#' @param values Numeric: Data values. `NA` values are ignored.
#' @param pad Numeric `[0, Inf)`: Fraction of the data range added on each side.
#' @return Numeric: Length-2 vector `c(min, max)`.
#' @keywords internal
#' @noRd
calc_limits <- function(values, pad = 0.04) {
  rng <- range(values, na.rm = TRUE)
  span <- rng[2] - rng[1]
  if (span == 0) {
    span <- abs(rng[1]) * 0.1
  } # handle constant data
  c(rng[1] - pad * span, rng[2] + pad * span)
}


#' Parse a user-supplied `margins` argument into a named list of per-side values
#'
#' Validates a named `margins` argument supplied to `draw_*` functions and
#' returns a list with elements `top`, `right`, `bottom`, `left`, each either
#' the user-supplied value (numeric or character) or `NULL` when the user did
#' not specify that side. Accepts a named numeric vector
#' (e.g. `c(left = 80, right = 20)`) or a named list
#' (e.g. `list(left = 80, right = "10%")`). The list form is required when any
#' side is a percentage string. Sides are matched by name; unrecognised names
#' error with a corrective message.
#'
#' @param margins Optional Named numeric vector or named list: User-supplied
#'   margins argument. Valid names are `"top"`, `"right"`, `"bottom"`, `"left"`.
#' @return Named list with elements `top`, `right`, `bottom`, `left`, each
#'   either the user-supplied value or `NULL`.
#' @keywords internal
#' @noRd
parse_margins <- function(margins) {
  empty <- list(top = NULL, right = NULL, bottom = NULL, left = NULL)
  if (is.null(margins)) {
    return(empty)
  }
  # Accept a named atomic vector (numeric / character) or a named list.
  if (!is.list(margins) && !is.atomic(margins)) {
    abort(
      "`margins` must be a named numeric vector or named list.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  nms <- names(margins)
  if (is.null(nms) || any(!nzchar(nms))) {
    abort(
      "`margins` must be named with any of 'top', 'right', 'bottom', 'left'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  valid_sides <- c("top", "right", "bottom", "left")
  bad <- setdiff(nms, valid_sides)
  if (length(bad) > 0L) {
    abort(
      "`margins` has unrecognised names: ",
      paste(bad, collapse = ", "),
      ". Valid names are: ",
      paste(valid_sides, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (any(duplicated(nms))) {
    abort(
      "`margins` has duplicate names: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  for (side in valid_sides) {
    if (!(side %in% nms)) {
      next
    }
    val <- margins[[side]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) {
      next
    }
    if (
      !((is.numeric(val) && length(val) == 1L) ||
        (is.character(val) && length(val) == 1L))
    ) {
      abort(
        "`margins` element '",
        side,
        "' must be a single number or string (e.g. '10%'); got ",
        class(val)[1],
        " of length ",
        length(val),
        ".",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    empty[[side]] <- val
  }
  empty
}

#' Resolve a user-supplied `margins` argument into a [Grid] (or `NULL`)
#'
#' Convenience wrapper around `parse_margins()` that builds a [Grid] with the
#' specified sides set (other sides left `NULL` so echarts' default behaviour
#' still applies to them). Returns `NULL` when `margins` is `NULL` so callers
#' can pass the result straight to `EChartsOption(grid = ...)`.
#'
#' @param margins Optional Named numeric vector or named list: User-supplied
#'   margins argument. Valid names are `"top"`, `"right"`, `"bottom"`, `"left"`.
#' @return Optional [Grid]: `Grid` object with the specified sides set, or
#'   `NULL` when `margins` is `NULL`.
#' @keywords internal
#' @noRd
resolve_margins <- function(margins) {
  if (is.null(margins)) {
    return(NULL)
  }
  m <- parse_margins(margins)
  Grid(
    top = m[["top"]],
    right = m[["right"]],
    bottom = m[["bottom"]],
    left = m[["left"]]
  )
}


#' Validate an axis-limits argument
#'
#' Checks that a user-supplied `xlim`/`ylim`/`zlim` value is either `NULL` or a
#' length-2 numeric vector. Errors with a corrective [rtemis.core::abort()] message
#' otherwise. Returns the value invisibly so callers can chain if desired.
#'
#' @param value Any: User-supplied limits value.
#' @param arg Character: Name of the caller argument (e.g. `"xlim"`) used in
#'   the error message.
#' @return Invisibly, `value`.
#' @keywords internal
#' @noRd
validate_axis_lim <- function(value, arg) {
  if (is.null(value)) {
    return(invisible(value))
  }
  if (!is.numeric(value) || length(value) != 2L || !all(is.finite(value))) {
    abort(
      "`",
      arg,
      "` must be a length-2 finite numeric vector or `NULL`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  invisible(value)
}


#' Property that accepts a character vector of colors, or NULL
#'
#' Used for color palettes (e.g. theme and option `color` fields).
#' @keywords internal
#' @noRd
color_palette_property <- function(default = NULL) {
  S7::new_property(
    class = S7::class_any,
    default = default,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (is.character(value)) {
        return(NULL)
      }
      "must be a character vector of colors or NULL"
    }
  )
}

#' Property that accepts an S7 class instance or NULL
#' @keywords internal
#' @noRd
class_or_null_property <- function(s7_class) {
  S7::new_property(
    class = S7::class_any,
    default = NULL,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (!S7::S7_inherits(value, s7_class)) {
        paste0("must be a ", s7_class@name, " object or NULL")
      }
    }
  )
}
