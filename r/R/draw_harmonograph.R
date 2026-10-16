# draw_harmonograph.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Harmonograph curves after Fronkonstin (2014),
# https://fronkonstin.com/2014/10/13/beautiful-curves-the-harmonograph/

# %% HARMONOGRAPH_SEGMENTS ----
# Maximum number of separately stroked segments per curve.
HARMONOGRAPH_SEGMENTS <- 500L


# %% harmonograph_params ----
#' Draw and validate harmonograph parameters
#'
#' Draws all 12 parameters in a fixed order: for each of the four pendulums a
#' frequency of 2 or 3 perturbed by [base::jitter()], then four damping rates
#' uniform on `[0, 0.01]`, then four phases uniform on `[0, pi]`. Supplied
#' vectors then replace their drawn counterparts, so a seed fixes the remaining
#' parameters whichever vectors are supplied.
#'
#' @param frequency,damping,phase Optional Numeric: Length-4 parameter vectors.
#' @param seed Optional Integer: Seed for the draw. The caller's RNG state is
#'   restored on exit.
#'
#' @return Named list: `frequency`, `damping` and `phase`, each numeric of
#'   length 4.
#'
#' @author EDG
#' @keywords internal
#' @noRd
harmonograph_params <- function(
  frequency = NULL,
  damping = NULL,
  phase = NULL,
  seed = NULL
) {
  check_harmonograph_param(frequency, "frequency")
  check_harmonograph_param(damping, "damping", min = 0)
  check_harmonograph_param(phase, "phase")
  if (!is.null(seed)) {
    seed <- clean_int(seed)
    # Restore the caller's RNG state, or its absence, on exit.
    genv <- globalenv()
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = genv), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = genv), add = TRUE)
    }
    set.seed(seed)
  }
  drawn_frequency <- vapply(
    seq_len(4L),
    function(i) jitter(sample(c(2, 3), 1L)),
    numeric(1L)
  )
  drawn_damping <- stats::runif(4L, 0, 1e-2)
  drawn_phase <- stats::runif(4L, 0, pi)
  list(
    frequency = as.numeric(frequency %||% drawn_frequency),
    damping = as.numeric(damping %||% drawn_damping),
    phase = as.numeric(phase %||% drawn_phase)
  )
} # /rtemis.draw::harmonograph_params


# %% check_harmonograph_param ----
#' Validate one harmonograph parameter vector
#'
#' @param x Optional Numeric: The supplied vector.
#' @param arg Character: Argument name, for the error message.
#' @param min Numeric: Smallest admissible value.
#'
#' @return Invisibly, `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_harmonograph_param <- function(x, arg, min = -Inf) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (!is.numeric(x) || length(x) != 4L || !all(is.finite(x))) {
    abort(
      "`",
      arg,
      "` must be a finite numeric vector of length 4, one value per ",
      "pendulum, or NULL to draw it at random.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (any(x < min)) {
    abort(
      "`",
      arg,
      "` values must be at least ",
      min,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(x)
} # /rtemis.draw::check_harmonograph_param


# %% harmonograph_curve ----
#' Evaluate a harmonograph curve
#'
#' Each coordinate is the sum of two damped sinusoids:
#' `x(t) = exp(-d1 t) sin(f1 t + p1) + exp(-d2 t) sin(f2 t + p2)`, and `y(t)`
#' is the same expression with parameters 3 and 4.
#'
#' @param t Numeric: Time points.
#' @param params Named list: Output of `harmonograph_params()`.
#'
#' @return Named list: Numeric `x` and `y`, each the length of `t`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
harmonograph_curve <- function(t, params) {
  f <- params[["frequency"]]
  d <- params[["damping"]]
  p <- params[["phase"]]
  pendulum <- function(i) exp(-d[i] * t) * sin(t * f[i] + p[i])
  list(
    x = pendulum(1L) + pendulum(2L),
    y = pendulum(3L) + pendulum(4L)
  )
} # /rtemis.draw::harmonograph_curve


# %% harmonograph_option ----
#' Build the ECharts option for a harmonograph
#'
#' The curve is split into at most `HARMONOGRAPH_SEGMENTS` consecutive
#' segments, each a silent line series with no symbols, sharing its endpoints
#' with its neighbors. Each segment is stroked separately, so the opacity of
#' overlapping segments accumulates and dense regions of the curve appear
#' brighter. All segments share one series name, which ECharts maps to one
#' palette color. Animation is off. Both hidden value axes span the same interval length,
#' centered on each coordinate's range, so the curve keeps its proportions in
#' the square plotting box that `aspect_meta()` requests. Coordinates are
#' rounded to 4 decimal places, below one pixel at any practical size for a
#' curve spanning at most `[-2, 2]`.
#'
#' @inheritParams draw_harmonograph
#'
#' @return [EChartsOption]: The option object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
harmonograph_option <- function(
  frequency = NULL,
  damping = NULL,
  phase = NULL,
  t = seq(1, 500, by = 0.01),
  seed = NULL,
  color = NULL,
  alpha = 0.2,
  line_width = 1,
  pad = DEFAULT_PAD,
  title = NULL,
  margins = DEFAULT_MARGINS
) {
  if (!is.numeric(t) || length(t) < 2L || !all(is.finite(t))) {
    abort(
      "`t` must be a finite numeric vector of at least two time points.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  check_optional_character_scalar(color)
  check_prob_scalar(alpha)
  check_pos_double_scalar(line_width)
  check_nonneg_double_scalar(pad)
  check_optional_character_scalar(title)
  params <- harmonograph_params(
    frequency = frequency,
    damping = damping,
    phase = phase,
    seed = seed
  )
  curve <- lapply(harmonograph_curve(t, params), round, digits = 4L)

  # A common half-span keeps one data unit the same size on both axes.
  x_range <- range(curve[["x"]])
  y_range <- range(curve[["y"]])
  half <- max(diff(x_range), diff(y_range), 1e-8) * (1 + 2 * pad) / 2
  axis <- function(rng) {
    Axis(
      type = "value",
      show = FALSE,
      min = mean(rng) - half,
      max = mean(rng) + half
    )
  }

  # Consecutive segments overlap by one point, so the curve is continuous.
  n <- length(t)
  seg_len <- max(2L, ceiling((n - 1L) / HARMONOGRAPH_SEGMENTS) + 1L)
  starts <- seq(1L, n - 1L, by = seg_len - 1L)
  # A two-column matrix serializes to an array of [x, y] pairs.
  points <- cbind(curve[["x"]], curve[["y"]])
  line_style <- LineStyle(width = line_width, opacity = alpha)
  series <- lapply(starts, function(start) {
    LineSeries(
      name = "harmonograph",
      data = points[start:min(start + seg_len - 1L, n), , drop = FALSE],
      color = color,
      show_symbol = FALSE,
      silent = TRUE,
      line_style = line_style
    )
  })

  EChartsOption(
    title = if (!is.null(title)) Title(text = title, left = "center") else NULL,
    grid = resolve_margins(margins),
    x_axis = axis(x_range),
    y_axis = axis(y_range),
    series = series,
    # The initial animation would sweep each of the segments separately.
    animation = FALSE
  )
} # /rtemis.draw::harmonograph_option


# %% draw_harmonograph ----
#' Draw a Harmonograph
#'
#' Draws the curve traced by a
#' [harmonograph](https://en.wikipedia.org/wiki/Harmonograph): two damped
#' pendulums move the pen along x and two along y.
#'
#' @details
#' The curve is
#' \deqn{x(t) = e^{-d_1 t} \sin(f_1 t + p_1) + e^{-d_2 t} \sin(f_2 t + p_2)}
#' \deqn{y(t) = e^{-d_3 t} \sin(f_3 t + p_3) + e^{-d_4 t} \sin(f_4 t + p_4)}
#' with frequencies \eqn{f}, damping rates \eqn{d} and phases \eqn{p}.
#'
#' Parameters left `NULL` are drawn at random: each frequency is 2 or 3,
#' perturbed by [base::jitter()]; damping rates are uniform on `[0, 0.01]`;
#' phases are uniform on `[0, pi]`. All 12 parameters are drawn in that order
#' and supplied vectors replace their drawn counterparts, so a fixed `seed`
#' keeps the remaining parameters fixed whichever vectors are supplied. A
#' given `seed` yields the same parameters as `mplot3_harmonograph()` in rtemis
#' 0.98. Seeds to try include 9, 17, 26, 202 and 208.
#'
#' The line follows the theme: `color = NULL` uses the first palette color, on
#' the theme background. The curve is drawn as up to 500 consecutive segments,
#' and the opacity of overlapping segments accumulates, so dense regions of the
#' curve appear brighter.
#'
#' The curve formulation follows the Fronkonstin blog post "Beautiful Curves:
#' The Harmonograph" (2014).
#'
#' @param frequency Optional Numeric: Pendulum frequencies, length 4. The first
#'   two move the pen along x, the last two along y.
#' @param damping Optional Numeric `[0, Inf)`: Pendulum damping rates, length 4.
#' @param phase Optional Numeric: Pendulum phases in radians, length 4.
#' @param t Numeric: Time points at which the curve is evaluated.
#' @param seed Optional Integer: Seed for the random parameters. The caller's
#'   random number generator state is restored on exit. `NULL` uses the
#'   current state.
#' @param color Optional Character: Line color. `NULL` uses the theme palette's
#'   first color.
#' @param alpha Numeric `[0, 1]`: Line opacity.
#' @param line_width Numeric `(0, Inf)`: Line width in pixels.
#' @param pad Numeric `[0, Inf)`: Fraction of the curve's larger range added on
#'   each side.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. `NULL` follows the light or
#'   dark appearance of the viewer.
#' @param margins Optional Named numeric vector or named list: Plot margins in
#'   pixels for any of `"top"`, `"right"`, `"bottom"`, `"left"`.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height.
#' @param element_id Optional Character: Explicit DOM element ID for the widget
#'   container. `NULL` lets htmlwidgets generate one.
#' @param filename Optional Character: If provided, save the widget to this
#'   file via [save_drawing()].
#'
#' @return htmlwidget: Widget object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' draw_harmonograph(seed = 9)
#' draw_harmonograph(
#'   frequency = c(2, 3, 3, 2),
#'   damping = rep(0.004, 4),
#'   phase = c(0, pi / 2, pi / 4, 0),
#'   alpha = 0.5
#' )
draw_harmonograph <- function(
  frequency = NULL,
  damping = NULL,
  phase = NULL,
  t = seq(1, 500, by = 0.01),
  seed = NULL,
  color = NULL,
  alpha = 0.2,
  line_width = 1,
  pad = DEFAULT_PAD,
  title = NULL,
  theme = NULL,
  margins = DEFAULT_MARGINS,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  opt <- harmonograph_option(
    frequency = frequency,
    damping = damping,
    phase = phase,
    t = t,
    seed = seed,
    color = color,
    alpha = alpha,
    line_width = line_width,
    pad = pad,
    title = title,
    margins = margins
  )

  draw(
    opt,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename,
    meta = aspect_meta(opt, square = TRUE, equal_axes = TRUE)
  )
} # /rtemis.draw::draw_harmonograph
