# 00_init.R
# Local property definitions that mirror upstream rtemis.core exports not yet
# on CRAN. Remove each entry here once the corresponding rtemis.core version
# is available on CRAN and declared in DESCRIPTION.

# nonneg_integer_scalar / optional_nonneg_integer_scalar
# Added to rtemis.core >= 0.1.1; mirrors 00_S7_properties.R.
nonneg_integer_scalar <- S7::new_property(
  S7::class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0L) {
      return("must be a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)

optional_nonneg_integer_scalar <- S7::new_property(
  class = S7::new_union(NULL, S7::class_integer),
  validator = function(value) {
    if (
      !is.null(value) && (length(value) != 1L || is.na(value) || value < 0L)
    ) {
      return("must be NULL or a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)
