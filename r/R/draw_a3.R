# draw_a3.R - A3 amino acid sequence visualization
#
# Renders an `A3` object (rtemis.a3) as an interactive ECharts diagram: the
# amino-acid sequence is wrapped in a meander/serpentine path, with optional
# site, region, PTM, processing, and variant annotations overlaid as separate
# series with a scrollable vertical legend.
#
# Layout and series logic translated from:
#   ~/Code/rtemislive-draw/src/lib/a3/visualization/layout.ts
#   ~/Code/rtemislive-draw/src/lib/a3/visualization/echarts.ts
#   ~/Code/rtemislive-draw/src/lib/a3/visualization/palette.ts

# ── Annotation color palettes ────────────────────────────────────────────────
# Identical to the TypeScript DEFAULT_*_PALETTE constants; shared across
# light and dark themes so legend colors are consistent.

.A3_REGION_PALETTE <- c(
  "#0F766E", "#2563EB", "#D97706", "#7C3AED", "#DC2626", "#0891B2"
)
.A3_SITE_PALETTE <- c(
  "#0EA5E9", "#14B8A6", "#E11D48", "#A855F7", "#F97316", "#16A34A"
)
.A3_PTM_PALETTE <- c(
  "#0284C7", "#F59E0B", "#10B981", "#8B5CF6", "#EF4444", "#14B8A6"
)
.A3_PROC_PALETTE <- c(
  "#D97706", "#DC2626", "#0EA5E9", "#9333EA", "#16A34A", "#DB2777"
)

# ECharts SVG path used for region legend icons (horizontal band)
.A3_LEGEND_REGION_ICON <- "path://M2 5 H22 V11 H2 Z"

# Sentinel names for legend heading entries (invisible placeholder series)
.A3_LEGEND_HEADING_REGIONS    <- "__legend_heading_regions__"
.A3_LEGEND_HEADING_PTMS       <- "__legend_heading_ptms__"
.A3_LEGEND_HEADING_PROCESSING <- "__legend_heading_processing__"

# Amino acid one-letter to full-name lookup
.A3_AA_NAMES <- c(
  A = "Alanine",
  R = "Arginine",
  N = "Asparagine",
  D = "Aspartate",
  C = "Cysteine",
  Q = "Glutamine",
  E = "Glutamate",
  G = "Glycine",
  H = "Histidine",
  I = "Isoleucine",
  L = "Leucine",
  K = "Lysine",
  M = "Methionine",
  F = "Phenylalanine",
  P = "Proline",
  S = "Serine",
  T = "Threonine",
  W = "Tryptophan",
  Y = "Tyrosine",
  V = "Valine",
  B = "Aspartic acid or Asparagine",
  Z = "Glutamine or Glutamic acid",
  X = "(Any)",
  `*` = "Termination codon"
)

# ── Internal layout helpers ──────────────────────────────────────────────────

#' Compute wrapped-sequence (meander) coordinates
#'
#' Translates `createSequenceCoordinates()` from layout.ts into R.
#' Returns integer-indexed raw xs and ys (before x-scaling) for `seq_length`
#' residues, with half-step adjustments at the two wrap points per period.
#'
#' @param seq_length Integer: Number of residues.
#' @param n_per_row Integer `[2, Inf)`: Residues per row.
#' @return Named list with numeric vectors `xs` and `ys`.
#' @keywords internal
#' @noRd
a3_seq_coords <- function(seq_length, n_per_row) {
  # X oscillates: 1..n_per_row, (n_per_row-1)..2; period = 2*n_per_row - 2
  x_pattern <- c(seq_len(n_per_row), seq.int(n_per_row - 1L, 2L))
  period    <- length(x_pattern) # 2 * n_per_row - 2

  # Y: first residue at y=1. Each subsequent band of (n_per_row-1) residues
  # shares the same integer y, incrementing once per band.
  if (seq_length > 1L) {
    n_bands <- ceiling((seq_length - 1L) / (n_per_row - 1L))
    y_tail  <- rep(seq_len(n_bands), each = n_per_row - 1L)
    y_tail  <- y_tail[seq_len(seq_length - 1L)]
    ys <- as.numeric(c(1L, y_tail))
  } else {
    ys <- 1.0
  }
  xs <- as.numeric(x_pattern[((seq_len(seq_length) - 1L) %% period) + 1L])

  border_offset <- 1 - sqrt(3) / 2

  # Right-turn wraps: 0-based indices n_per_row-1, n_per_row-1+period, ...
  # → 1-based: n_per_row, n_per_row+period, ...
  if (n_per_row <= seq_length) {
    turn_r <- seq.int(n_per_row, seq_length, by = period)
    ys[turn_r] <- ys[turn_r] + 0.5
    xs[turn_r] <- xs[turn_r] - border_offset
  }

  # Left-turn wraps: 0-based indices 2*n_per_row-2, ...
  # → 1-based: 2*n_per_row-1, ...
  if ((2L * n_per_row - 1L) <= seq_length) {
    turn_l <- seq.int(2L * n_per_row - 1L, seq_length, by = period)
    ys[turn_l] <- ys[turn_l] + 0.5
    xs[turn_l] <- xs[turn_l] + border_offset
  }

  list(xs = xs, ys = ys)
}

#' Scale residue x-coordinate by residue spacing
#' @keywords internal
#' @noRd
a3_scale_x <- function(x, residue_spacing) {
  if (residue_spacing == 1) x else 1 + (x - 1) * residue_spacing
}

#' Convert a hex color to an rgba() string with specified alpha
#'
#' Non-hex colors (e.g. named CSS colors) are returned unchanged.
#'
#' @param color Character: Hex color string (e.g. `"#0F766E"`).
#' @param alpha Numeric `[0, 1]`: Alpha channel value.
#' @return Character: `rgba()` string.
#' @keywords internal
#' @noRd
a3_with_alpha <- function(color, alpha) {
  hex <- trimws(color)
  if (!startsWith(hex, "#")) {
    return(hex)
  }
  h <- substring(hex, 2L)
  if (nchar(h) == 3L) {
    chars <- strsplit(h, "")[[1L]]
    h <- paste0(rep(chars, each = 2L), collapse = "")
  }
  r <- strtoi(substr(h, 1L, 2L), 16L)
  g <- strtoi(substr(h, 3L, 4L), 16L)
  b <- strtoi(substr(h, 5L, 6L), 16L)
  sprintf("rgba(%d, %d, %d, %g)", r, g, b, alpha)
}


#' Compute a 2-D circular offset in pixels
#'
#' Places annotation symbols at evenly-spaced positions around a clock face.
#' `index` is 1-based; angle 0 maps to the top (positive y on screen = down).
#'
#' @param index Integer `[1, count]`: 1-based position in the clock face.
#' @param count Integer `[1, Inf)`: Total number of positions.
#' @param gap Numeric: Radius in pixels.
#' @return Named list with numeric `x` and `y` pixel offsets.
#' @keywords internal
#' @noRd
a3_circular_offset <- function(index, count, gap) {
  angle <- 2 * pi * index / count
  list(x = sin(angle) * gap, y = cos(angle) * gap)
}

#' Resolve PTM symbol pixel offset for a given placement strategy
#' @keywords internal
#' @noRd
a3_ptm_offset <- function(index, count, placement, marker_size, ptm_symbol_size) {
  residue_radius <- marker_size / 2
  ptm_radius     <- ptm_symbol_size / 2
  radial_distance <- switch(
    placement,
    innerRadial = max(0, residue_radius - ptm_radius),
    outerRadial = residue_radius + ptm_radius,
    residue_radius # "radial": default
  )
  a3_circular_offset(index, count, radial_distance)
}

#' Expand a flex-entry index to a flat integer vector of positions
#'
#' An `A3Position` index (via `[[`) is an integer vector; an `A3Range` index
#' is a 2-column integer matrix.  Both cases are handled here.
#'
#' @param index Integer vector or 2-column integer matrix.
#' @return Integer vector of individual positions.
#' @keywords internal
#' @noRd
a3_flex_positions <- function(index) {
  if (is.matrix(index)) {
    # A3Range: rows are [start, end] pairs
    unlist(lapply(
      seq_len(nrow(index)),
      function(i) seq.int(index[i, 1L], index[i, 2L])
    ))
  } else {
    as.integer(index)
  }
}

# ── draw_a3 ──────────────────────────────────────────────────────────────────

#' Draw an A3 Amino Acid Sequence Visualization
#'
#' Renders an `A3` object (from `rtemis.a3`) as an interactive ECharts
#' amino-acid sequence diagram. The sequence is wrapped into rows in a
#' meander/serpentine path, with optional site, region, PTM, processing, and
#' variant annotations overlaid as distinct series and collected in a
#' scrollable legend.
#'
#' Corresponds to `createA3EChartsOption()` in
#' `src/lib/a3/visualization/echarts.ts`.
#'
#' @param x `A3` object from [rtemis.a3::create_A3()] or
#'   [rtemis.a3::read_A3json()].
#' @param n_per_row Integer `[2, Inf)`: Number of residues per wrapped row.
#' @param residue_spacing Numeric `(0, Inf)`: Horizontal spacing multiplier.
#'   Values below `1` compress the layout; `1` gives natural hex spacing.
#' @param marker_size Numeric `(0, Inf)`: Residue circle diameter in pixels.
#' @param font_size Numeric `(0, Inf)`: Base font size in pixels for residue
#'   and position labels.
#' @param line_width Numeric `(0, Inf)`: Backbone line width in pixels.
#' @param show_markers Logical: Whether to render the backbone line and
#'   residue circles.
#' @param show_labels Logical: Whether to render single-letter residue labels.
#' @param position_every Optional Integer `[1, Inf)`: Show a numeric position
#'   label every N residues. `NULL` disables position labels.
#' @param region_opacity Numeric `[0, 1]`: Opacity of region band overlays.
#' @param ptm_placement Character `{"radial", "innerRadial", "outerRadial"}`:
#'   Placement of PTM symbols relative to the residue circle.
#'   `"radial"` centres on the circle edge; `"innerRadial"` places inside;
#'   `"outerRadial"` places outside.
#' @param residue_fill Character: Residue circle fill color.
#' @param residue_stroke Character: Residue circle stroke and backbone line color.
#' @param label_color Character: Default residue label text color.
#' @param pos_label_color Character: Position label text color.
#' @param variant_color Character: Label color for variant residues.
#' @param disease_variant_color Character: Label color for disease-associated
#'   variant residues (takes precedence over `variant_color`).
#' @param enable_zoom Logical: Whether to enable Shift+scroll zoom.
#' @param title Optional Character: Chart title.
#' @param theme Optional [Theme]: Theme override. `NULL` auto-detects
#'   light/dark mode; `NA` uses raw ECharts defaults.
#' @param width Optional Numeric or Character: Widget width.
#' @param height Optional Numeric or Character: Widget height. Auto-computed
#'   from layout bounds and `marker_size` when `NULL`.
#' @param filename Optional Character: If provided, save the widget to this
#'   file via [save_drawing()].
#' @return htmlwidget: Widget object.
#' @export
#'
#' @examples
#' if (requireNamespace("rtemis.a3", quietly = TRUE)) {
#'   a <- rtemis.a3::create_A3(
#'     "MAEPRQEFEVMEDHAGTYGLGDRK",
#'     site = list(
#'       Active_site = rtemis.a3::annotation_position(c(5L, 17L))
#'     ),
#'     region = list(
#'       Domain = rtemis.a3::annotation_range(
#'         matrix(c(3L, 10L, 15L, 22L), ncol = 2, byrow = TRUE)
#'       )
#'     )
#'   )
#'   draw_a3(a)
#' }
draw_a3 <- function(
  x,
  n_per_row = 21L,
  residue_spacing = 0.3,
  marker_size = 28,
  font_size = 18,
  line_width = 2,
  show_markers = TRUE,
  show_labels = TRUE,
  position_every = 10L,
  region_opacity = 0.35,
  ptm_placement = "radial",
  residue_fill = "#E7E5E4",
  residue_stroke = "#44403C",
  label_color = "#1C1917",
  pos_label_color = "#78716C",
  variant_color = "#FA6E1E",
  disease_variant_color = "#E266AE",
  enable_zoom = TRUE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  filename = NULL
) {
  # ── Input validation ────────────────────────────────────────────────────────
  if (!S7::S7_inherits(x)) {
    cli::cli_abort(
      "{.arg x} must be an {.cls A3} object. \\
      Create one with {.fn rtemis.a3::create_A3}."
    )
  }
  cls_name <- S7::S7_class(x)@name
  if (!grepl("::A3$|^A3$", cls_name)) {
    cli::cli_abort(
      "{.arg x} must be an {.cls A3} object, not {.cls {cls_name}}. \\
      Create one with {.fn rtemis.a3::create_A3}."
    )
  }
  ptm_placement <- match.arg(ptm_placement, c("radial", "innerRadial", "outerRadial"))
  n_per_row <- as.integer(n_per_row)
  if (n_per_row <= 1L) {
    cli::cli_abort("{.arg n_per_row} must be an integer > 1.")
  }
  if (!is.numeric(residue_spacing) || residue_spacing <= 0) {
    cli::cli_abort("{.arg residue_spacing} must be a positive number.")
  }

  # ── Extract data via the A3 [[ method ──────────────────────────────────────
  # x[["sequence"]] calls to_base(prop(x, "sequence")), returning character(1)
  seq_str  <- x[["sequence"]]
  seq_length <- nchar(seq_str)
  seq_chars  <- strsplit(seq_str, "")[[1L]]

  annotations  <- x[["annotations"]]
  site_anns    <- annotations[["site"]]
  region_anns  <- annotations[["region"]]
  ptm_anns     <- annotations[["ptm"]]
  proc_anns    <- annotations[["processing"]]
  variant_anns <- annotations[["variant"]]

  # ── Layout ─────────────────────────────────────────────────────────────────
  coords <- a3_seq_coords(seq_length, n_per_row)
  xs <- a3_scale_x(coords[["xs"]], residue_spacing)
  ys <- coords[["ys"]]

  min_x <- min(xs)
  max_x <- max(xs)
  min_y <- min(ys)
  max_y <- max(ys)

  # ── Residue label color assignment ─────────────────────────────────────────
  # Disease-associated variant positions override generic variant positions.
  disease_pos <- integer(0L)
  for (nm in names(site_anns)) {
    if (tolower(gsub("[-[:space:]]", "_", nm)) == "disease_associated_variant") {
      disease_pos <- c(disease_pos, as.integer(site_anns[[nm]][["index"]]))
    }
  }
  variant_pos <- if (length(variant_anns) > 0L) {
    vapply(variant_anns, function(v) as.integer(v[["position"]]), integer(1L))
  } else {
    integer(0L)
  }

  label_cols <- vapply(seq_len(seq_length), function(i) {
    if (i %in% disease_pos) {
      disease_variant_color
    } else if (i %in% variant_pos) {
      variant_color
    } else {
      label_color
    }
  }, character(1L))

  # ── Auto-compute height ────────────────────────────────────────────────────
  if (is.null(height)) {
    # titleMarginTop: matches resolveA3TitleMarginTop() in layout.ts
    title_margin_top <- if (!is.null(title)) {
      max(64L, 32L + font_size + 14L)
    } else {
      24L
    }
    vertical_span <- max_y - min_y
    height <- ceiling(
      title_margin_top + 24L + marker_size * (2 * vertical_span + 1)
    )
  }

  # ── Series construction ────────────────────────────────────────────────────
  series        <- list()
  site_legend   <- list()
  region_legend <- list()
  ptm_legend    <- list()
  proc_legend   <- list()

  # 1. Backbone line (z = 10): primary structure
  if (show_markers) {
    backbone_data <- lapply(seq_len(seq_length), function(i) {
      res      <- seq_chars[[i]]
      res_name <- .A3_AA_NAMES[[res]] %||% res
      list(
        value   = list(xs[[i]], ys[[i]]),
        tooltip = list(formatter = sprintf("%d: %s", i, res_name))
      )
    })
    series <- c(series, list(list(
      type       = "line",
      name       = "Primary structure",
      z          = 10L,
      data       = backbone_data,
      showSymbol = TRUE,
      symbol     = "circle",
      symbolSize = marker_size,
      lineStyle  = list(color = residue_stroke, width = line_width),
      itemStyle  = list(
        color       = residue_fill,
        borderColor = residue_stroke,
        borderWidth = 1
      )
    )))
  }

  # 2. Regions (z = 15): thick semi-transparent lines, one per named region
  for (i in seq_along(region_anns)) {
    nm    <- names(region_anns)[[i]]
    entry <- region_anns[[i]]
    color <- .A3_REGION_PALETTE[((i - 1L) %% length(.A3_REGION_PALETTE)) + 1L]
    fill  <- a3_with_alpha(color, region_opacity)

    # Expand [start, end] pairs; insert [null, null] breaks between ranges
    idx_mat     <- entry[["index"]] # 2-column integer matrix
    region_data <- list()
    for (j in seq_len(nrow(idx_mat))) {
      for (pos in seq.int(idx_mat[j, 1L], idx_mat[j, 2L])) {
        if (pos >= 1L && pos <= seq_length) {
          region_data <- c(region_data, list(list(xs[[pos]], ys[[pos]])))
        }
      }
      # Null break to disconnect consecutive ranges
      region_data <- c(region_data, list(list(NULL, NULL)))
    }

    series <- c(series, list(list(
      type         = "line",
      name         = nm,
      z            = 15L,
      data         = region_data,
      connectNulls = FALSE,
      showSymbol   = FALSE,
      itemStyle    = list(color = fill),
      lineStyle    = list(
        color = fill,
        width = marker_size,
        cap   = "round",
        join  = "round"
      )
    )))
    region_legend <- c(region_legend, list(
      list(name = nm, icon = .A3_LEGEND_REGION_ICON)
    ))
  }

  # 3. Sites (z = 20): hollow circles at specific positions
  for (i in seq_along(site_anns)) {
    nm    <- names(site_anns)[[i]]
    entry <- site_anns[[i]]
    color <- .A3_SITE_PALETTE[((i - 1L) %% length(.A3_SITE_PALETTE)) + 1L]

    positions <- as.integer(entry[["index"]])
    positions <- positions[positions >= 1L & positions <= seq_length]
    site_data <- lapply(positions, function(pos) list(xs[[pos]], ys[[pos]]))

    series <- c(series, list(list(
      type       = "scatter",
      name       = nm,
      z          = 20L,
      data       = site_data,
      symbol     = "circle",
      symbolSize = marker_size,
      itemStyle  = list(
        color       = "rgba(0,0,0,0)",
        borderColor = color,
        borderWidth = 1.5
      )
    )))
    # Legend icon: built-in circle shape with per-item itemStyle override so the
    # icon is a hollow ring (transparent fill, colored border). CSP-safe.
    site_legend <- c(site_legend, list(list(
      name      = nm,
      icon      = "circle",
      itemStyle = list(
        color       = "rgba(0,0,0,0)",
        borderColor = color,
        borderWidth = 2
      )
    )))
  }

  # 4. PTMs (z = 30): small filled circles, offset radially from residues
  n_ptms         <- length(ptm_anns)
  ptm_symbol_size <- marker_size / 4.5
  for (i in seq_along(ptm_anns)) {
    nm    <- names(ptm_anns)[[i]]
    entry <- ptm_anns[[i]]
    color <- .A3_PTM_PALETTE[((i - 1L) %% length(.A3_PTM_PALETTE)) + 1L]

    positions <- a3_flex_positions(entry[["index"]])
    positions <- positions[positions >= 1L & positions <= seq_length]
    ptm_data  <- lapply(positions, function(pos) list(xs[[pos]], ys[[pos]]))
    offset    <- a3_ptm_offset(i, max(n_ptms, 1L), ptm_placement,
                               marker_size, ptm_symbol_size)

    series <- c(series, list(list(
      type         = "scatter",
      name         = nm,
      z            = 30L,
      data         = ptm_data,
      symbol       = "circle",
      symbolSize   = ptm_symbol_size,
      symbolOffset = list(offset[["x"]], offset[["y"]]),
      itemStyle    = list(color = color, opacity = 1)
    )))
    # No custom icon: ECharts uses the default scatter legend icon (filled circle)
    # with color driven by series-level itemStyle.color.
    ptm_legend <- c(ptm_legend, list(list(name = nm)))
  }

  # 5. Processing (z = 31): triangles, circularly offset from residues
  annotation_gap  <- 16
  n_proc          <- length(proc_anns)
  proc_symbol_size <- marker_size / 4
  for (i in seq_along(proc_anns)) {
    nm    <- names(proc_anns)[[i]]
    entry <- proc_anns[[i]]
    color <- .A3_PROC_PALETTE[((i - 1L) %% length(.A3_PROC_PALETTE)) + 1L]

    positions <- a3_flex_positions(entry[["index"]])
    positions <- positions[positions >= 1L & positions <= seq_length]
    proc_data <- lapply(positions, function(pos) list(xs[[pos]], ys[[pos]]))
    offset    <- a3_circular_offset(i, max(n_proc, 1L), annotation_gap)

    series <- c(series, list(list(
      type         = "scatter",
      name         = nm,
      z            = 31L,
      data         = proc_data,
      symbol       = "triangle",
      symbolRotate = 180,
      symbolSize   = proc_symbol_size,
      symbolOffset = list(offset[["x"]], offset[["y"]]),
      itemStyle    = list(color = color)
    )))
    # No custom icon: ECharts uses the series symbol (triangle) for the legend icon.
    proc_legend <- c(proc_legend, list(list(name = nm)))
  }

  # 6. Legend heading placeholder series (invisible; exist only for legend grouping)
  # Use character(0) for `data` so it serialises as JSON [] rather than {}.
  if (length(region_legend) > 0L) {
    series <- c(series, list(list(
      type       = "scatter",
      name       = .A3_LEGEND_HEADING_REGIONS,
      data       = character(0L),
      symbolSize = 0,
      silent     = TRUE,
      tooltip    = list(show = FALSE)
    )))
  }
  if (length(ptm_legend) > 0L) {
    series <- c(series, list(list(
      type       = "scatter",
      name       = .A3_LEGEND_HEADING_PTMS,
      data       = character(0L),
      symbolSize = 0,
      silent     = TRUE,
      tooltip    = list(show = FALSE)
    )))
  }
  if (length(proc_legend) > 0L) {
    series <- c(series, list(list(
      type       = "scatter",
      name       = .A3_LEGEND_HEADING_PROCESSING,
      data       = character(0L),
      symbolSize = 0,
      silent     = TRUE,
      tooltip    = list(show = FALSE)
    )))
  }

  # 7. Residue labels (z = 40): invisible markers carrying per-residue text
  if (show_labels) {
    label_data <- lapply(seq_len(seq_length), function(i) {
      list(
        value = list(xs[[i]], ys[[i]]),
        label = list(
          show      = TRUE,
          formatter = seq_chars[[i]],
          color     = label_cols[[i]],
          fontSize  = font_size
        )
      )
    })
    series <- c(series, list(list(
      type       = "scatter",
      name       = "Residue labels",
      z          = 40L,
      data       = label_data,
      silent     = TRUE,
      symbolSize = 0,
      tooltip    = list(show = FALSE)
    )))
  }

  # 8. Position labels (z = 41): numeric labels every N residues
  if (!is.null(position_every) && seq_length > position_every) {
    pos_font_size <- max(10L, font_size - 6L)
    pos_label_gap <- 16
    pos_indices   <- which(seq_len(seq_length) %% as.integer(position_every) == 0L)

    pos_data <- lapply(pos_indices, function(i) {
      list(
        value = list(xs[[i]], ys[[i]]),
        label = list(
          show      = TRUE,
          position  = "top",
          distance  = pos_label_gap,
          formatter = as.character(i),
          color     = pos_label_color,
          fontSize  = pos_font_size
        )
      )
    })
    series <- c(series, list(list(
      type       = "scatter",
      name       = "Position labels",
      z          = 41L,
      data       = pos_data,
      symbolSize = 0,
      tooltip    = list(show = FALSE)
    )))
  }

  # ── Legend data ─────────────────────────────────────────────────────────────
  legend_data <- list()
  if (show_markers) {
    legend_data <- c(legend_data, list("Primary structure"))
  }
  legend_data <- c(legend_data, site_legend)

  if (length(region_legend) > 0L) {
    legend_data <- c(
      legend_data,
      list(list(name = .A3_LEGEND_HEADING_REGIONS, icon = "none")),
      region_legend
    )
  }
  if (length(ptm_legend) > 0L) {
    legend_data <- c(
      legend_data,
      list(list(name = .A3_LEGEND_HEADING_PTMS, icon = "none")),
      ptm_legend
    )
  }
  if (length(proc_legend) > 0L) {
    legend_data <- c(
      legend_data,
      list(list(name = .A3_LEGEND_HEADING_PROCESSING, icon = "none")),
      proc_legend
    )
  }

  # JavaScript formatter for legend heading entries (rich text)
  legend_formatter <- htmlwidgets::JS(sprintf(
    "function(name) {
      if (name === '%s') return '{heading|Regions}';
      if (name === '%s') return '{heading|PTMs}';
      if (name === '%s') return '{heading|Processing}';
      return name;
    }",
    .A3_LEGEND_HEADING_REGIONS,
    .A3_LEGEND_HEADING_PTMS,
    .A3_LEGEND_HEADING_PROCESSING
  ))

  # ── ECharts option ──────────────────────────────────────────────────────────
  legend_right_inset <- 16
  legend_rail_width  <- 196
  legend_gap         <- 90
  title_margin_top   <- if (!is.null(title)) max(64L, 32L + font_size + 14L) else 24L

  option <- list(
    animation               = TRUE,
    animationDuration       = 220,
    animationDurationUpdate = 140,
    grid = list(
      left         = 24,
      right        = legend_right_inset + legend_rail_width + legend_gap,
      top          = title_margin_top,
      bottom       = 24,
      containLabel = FALSE
    ),
    xAxis = list(
      type     = "value",
      min      = min_x - 1,
      max      = max_x + 1,
      show     = FALSE,
      axisTick = list(show = FALSE)
    ),
    yAxis = list(
      type     = "value",
      min      = min_y - 1,
      max      = max_y + 1,
      inverse  = TRUE,
      show     = FALSE,
      scale    = TRUE,
      axisTick = list(show = FALSE)
    ),
    legend = list(
      type      = "scroll",
      orient    = "vertical",
      top       = if (!is.null(title)) title_margin_top + 8L else 24L,
      right     = legend_right_inset,
      bottom    = 24,
      width     = legend_rail_width,
      data      = legend_data,
      formatter = legend_formatter,
      textStyle = list(
        fontSize = font_size,
        rich     = list(
          heading = list(
            fontSize   = max(12L, font_size - 1L),
            fontWeight = 600,
            padding    = list(12, 0, 2, 0)
          )
        )
      )
    ),
    tooltip = list(trigger = "item", confine = TRUE),
    series  = series
  )

  if (!is.null(title)) {
    option[["title"]] <- list(
      text      = title,
      left      = "5.5%",
      top       = 32,
      textStyle = list(fontSize = font_size)
    )
  }

  if (enable_zoom) {
    option[["dataZoom"]] <- list(
      list(
        type                    = "inside",
        xAxisIndex              = 0,
        zoomOnMouseWheel        = "shift",
        moveOnMouseMove         = TRUE,
        moveOnMouseWheel        = TRUE,
        preventDefaultMouseMove = FALSE
      ),
      list(
        type             = "inside",
        yAxisIndex       = 0,
        zoomOnMouseWheel = FALSE,
        moveOnMouseWheel = FALSE,
        moveOnMouseMove  = FALSE
      )
    )
  }

  # ── Render ──────────────────────────────────────────────────────────────────
  # Pass the pre-built plain list directly to draw(). The a3 option is
  # self-contained and bypasses the S7 class hierarchy intentionally.
  draw(
    option,
    theme    = theme,
    width    = width,
    height   = height,
    filename = filename
  )
}
