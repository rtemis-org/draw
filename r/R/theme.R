# theme.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# A chart theme is a document of tokens: the constants chosen once for a set of
# charts, independent of the data and the surface. @rtemis/draw projects the
# tokens to each backend's theme in the browser and in Node: `themeObject()` for
# ECharts, `networkTheme()`, `choroplethTheme()`, and `diagramTheme()` for the
# Sigma.js, MapLibre, and Pele widgets. R holds no projection.
# spec: draw/chart-appearance#the-theme

# The arguments `Theme()` accepted before the token document, for one release.
THEME_LEGACY_ARGS <- c(
  color = "palette",
  background_color = "bg_color",
  text_style = "textStyle",
  title = "title",
  legend = "legend",
  tooltip = "tooltip",
  data_zoom = "dataZoom",
  line = "line",
  radar = "radar",
  bar = "bar",
  pie = "pie",
  scatter = "scatter",
  category_axis = "categoryAxis",
  value_axis = "valueAxis",
  log_axis = "logAxis",
  time_axis = "timeAxis"
)

# A color: a nonempty character scalar.
theme_color_property <- function(default = NULL, description = "") {
  prop_string(default, nullable = is.null(default), description = description)
}

# A font size in pixels, unset to follow `base_font_size`.
theme_size_property <- function(description) {
  prop_float(nullable = TRUE, exclusive_min = 0, description = description)
}

#' Theme
#'
#' The tokens of a chart theme, which every backend reads: fonts, text sizes,
#' text weights, surface colors, text colors, the categorical palette, the sign
#' colors, gridline style, and axis spacing. `@rtemis/draw` projects them to the
#' ECharts theme object (`themeOption()`) and to the themes of the network, map,
#' and diagram widgets, in the browser and in SVG export. [theme_light()] and
#' [theme_dark()] build the two built-in themes.
#'
#' Corresponds to `ThemeTokens` and `ThemeDocument` in `@rtemis/draw`
#' `src/theme/tokens.ts`.
#'
#' The arguments `color`, `background_color`, `text_style`, `title`, `legend`,
#' `tooltip`, `data_zoom`, `line`, `radar`, `bar`, `pie`, `scatter`,
#' `category_axis`, `value_axis`, `log_axis`, and `time_axis` are deprecated:
#' `color` sets `palette`, `background_color` sets `bg_color`, and the others
#' set the matching key of `echarts`, with a warning.
#'
#' @param scheme Character \{"light", "dark"\}: Which neutral control colors the
#'   theme uses.
#' @param font_family Optional Character: Font family name or CSS font stack,
#'   e.g. `"Geist"`. `NULL` uses the system UI font stack.
#' @param base_font_size Numeric `(0, Inf)`: Size in pixels of all text.
#' @param title_font_size,subtitle_font_size,axis_label_font_size,legend_font_size,tooltip_font_size
#'   Optional Numeric `(0, Inf)`: Text sizes in pixels. `NULL` sets the title
#'   to 1.2 times `base_font_size` and the others to `base_font_size`.
#' @param color_scale_font_size Optional Numeric `(0, Inf)`: Text size in
#'   pixels of color scale labels. `NULL` uses `base_font_size`.
#' @param font_weight Optional Numeric `[1, 1000]`: Weight of all text; 400 is
#'   normal, 700 bold. `NULL` uses ECharts' default, 400.
#' @param title_font_weight Optional Numeric `[1, 1000]`: Weight of titles.
#'   `NULL` uses ECharts' default, 700.
#' @param palette Character: Categorical palette.
#' @param bg_color Character: Chart background color.
#' @param fg_color Character: Text color.
#' @param title_color,subtitle_color,legend_color,axis_color,grid_color,tooltip_bg,tooltip_border_color,tooltip_color
#'   Optional Character: Colors of titles, subtitles, legend text, axis lines,
#'   gridlines, the tooltip background, its border, and its text. `NULL`
#'   follows `fg_color` for text and ECharts' default otherwise.
#' @param grid_opacity Optional Numeric `[0, 1]`: Opacity of gridlines.
#' @param grid_style Optional Character \{"solid", "dashed", "dotted"\}: Dash
#'   style of gridlines.
#' @param negative_color,neutral_color,positive_color Character: Colors of
#'   negative values, values of neither sign, and positive values, in every
#'   chart that colors by sign. A chart's own color arguments take precedence.
#' @param axis_label_gap Optional Numeric `[0, Inf)`: Distance from the plot to
#'   the tick labels, in em of the axis label font size. `NULL` uses ECharts'
#'   default, 8 pixels.
#' @param axis_name_gap Optional Numeric `[0, Inf)`: Margin around an axis
#'   name, in em of `base_font_size`; a centered name keeps it from the tick
#'   labels. `NULL` uses ECharts' default.
#' @param axis_name_location Optional Character \{"start", "middle", "end"\}:
#'   Where every axis name sits along its axis. `NULL` leaves it to each chart.
#' @param axis_line_show Optional Logical: Whether axis lines show. `NULL` uses
#'   ECharts' default for each axis type.
#' @param echarts Optional Named list: ECharts theme-object patch, merged over
#'   the projection of the tokens: objects merge recursively, any other value
#'   replaces, and `NULL` inside the patch removes a key. Keys are ECharts'
#'   camelCase names. Only ECharts charts read it.
#' @param origin Optional Named character \{"user", "default", "derived"\}:
#'   Where each token came from, one entry per token. [theme_light()],
#'   [theme_dark()], and `Theme()` record it; a resolved theme marks the tokens
#'   it derived.
#' @param writer Optional Named character: Which interface wrote the theme, as
#'   `name` and `version`. Absent on an authored theme.
#' @param color,background_color,text_style,title,legend,tooltip,data_zoom,line,radar,bar,pie,scatter,category_axis,value_axis,log_axis,time_axis
#'   Deprecated; see Details.
#'
#' @return `Theme` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Theme(palette = c("#16a085", "#e67e22"), base_font_size = 14)
Theme <- S7::new_class(
  "Theme",
  properties = list(
    scheme = prop_string(
      "light",
      enum = c("light", "dark"),
      description = "Which neutral control colors the theme uses."
    ),
    font_family = prop_string(
      nullable = TRUE,
      description = "Font family name or CSS font stack."
    ),
    base_font_size = prop_float(
      12,
      exclusive_min = 0,
      description = "Size in pixels of all text."
    ),
    title_font_size = theme_size_property("Title size in pixels."),
    subtitle_font_size = theme_size_property("Subtitle size in pixels."),
    axis_label_font_size = theme_size_property("Axis label size in pixels."),
    legend_font_size = theme_size_property("Legend text size in pixels."),
    tooltip_font_size = theme_size_property("Tooltip text size in pixels."),
    color_scale_font_size = theme_size_property(
      "Color scale label size in pixels."
    ),
    font_weight = prop_float(
      nullable = TRUE,
      min = 1,
      max = 1000,
      description = "Weight of all text."
    ),
    title_font_weight = prop_float(
      nullable = TRUE,
      min = 1,
      max = 1000,
      description = "Weight of titles."
    ),
    palette = prop_string(
      quote(palette_colors(rtemis_colors)),
      vector = TRUE,
      description = "Categorical palette."
    ),
    bg_color = theme_color_property("#ffffff", "Chart background color."),
    fg_color = theme_color_property("rgba(0, 0, 0, 0.7)", "Text color."),
    title_color = theme_color_property(description = "Title color."),
    subtitle_color = theme_color_property(description = "Subtitle color."),
    legend_color = theme_color_property(description = "Legend text color."),
    axis_color = theme_color_property(description = "Axis line color."),
    grid_color = theme_color_property(description = "Gridline color."),
    grid_opacity = prop_float(
      nullable = TRUE,
      min = 0,
      max = 1,
      description = "Opacity of gridlines."
    ),
    grid_style = prop_string(
      nullable = TRUE,
      enum = c("solid", "dashed", "dotted"),
      description = "Dash style of gridlines."
    ),
    tooltip_bg = theme_color_property(description = "Tooltip background."),
    tooltip_border_color = theme_color_property(
      description = "Tooltip border color."
    ),
    tooltip_color = theme_color_property(description = "Tooltip text color."),
    negative_color = theme_color_property(
      SIGN_COLORS[["negative"]],
      "Color of negative values."
    ),
    neutral_color = theme_color_property(
      SIGN_COLORS[["neutral"]],
      "Color of values of neither sign."
    ),
    positive_color = theme_color_property(
      SIGN_COLORS[["positive"]],
      "Color of positive values."
    ),
    axis_label_gap = prop_float(
      nullable = TRUE,
      min = 0,
      description = "Distance from the plot to the tick labels, in em."
    ),
    axis_name_gap = prop_float(
      nullable = TRUE,
      min = 0,
      description = "Margin around an axis name, in em."
    ),
    axis_name_location = prop_string(
      nullable = TRUE,
      enum = c("start", "middle", "end"),
      description = "Where every axis name sits along its axis."
    ),
    axis_line_show = prop_boolean(
      nullable = TRUE,
      description = "Whether axis lines show."
    ),
    echarts = prop_bag(
      nullable = TRUE,
      description = "ECharts theme-object patch merged over the projection."
    ),
    # Provenance, as a chart config's: where each token came from, and which
    # interface wrote a resolved theme.
    origin = prop_string(
      NULL,
      enum = ORIGIN_VALUES,
      nullable = TRUE,
      map = TRUE,
      description = paste(
        "Where each token came from: 'user' if the author set it, 'default'",
        "if the theme's constructor filled it in, 'derived' if resolving the",
        "theme computed it from other tokens."
      )
    ),
    writer = prop_string(
      NULL,
      nullable = TRUE,
      map = TRUE,
      description = paste(
        "Which interface wrote this document, as `name` and `version`.",
        "Absent on an authored theme."
      )
    )
  ),
  validator = function(self) {
    if (!is.null(self@font_family)) {
      # Errors on a family `font_stack()` cannot resolve.
      font_stack(self@font_family)
    }
    if (length(self@palette) == 0L) {
      return("`palette` must hold at least one color.")
    }
    origin_extra <- setdiff(names(self@origin), THEME_TOKENS)
    if (length(origin_extra) > 0L) {
      return(paste0(
        "@origin names tokens a theme does not have: ",
        paste(origin_extra, collapse = ", "),
        "."
      ))
    }
    NULL
  },
  constructor = function(
    scheme = "light",
    font_family = NULL,
    base_font_size = 12,
    title_font_size = NULL,
    subtitle_font_size = NULL,
    axis_label_font_size = NULL,
    legend_font_size = NULL,
    tooltip_font_size = NULL,
    color_scale_font_size = NULL,
    font_weight = NULL,
    title_font_weight = NULL,
    palette = palette_colors(rtemis_colors),
    bg_color = "#ffffff",
    fg_color = "rgba(0, 0, 0, 0.7)",
    title_color = NULL,
    subtitle_color = NULL,
    legend_color = NULL,
    axis_color = NULL,
    grid_color = NULL,
    grid_opacity = NULL,
    grid_style = NULL,
    tooltip_bg = NULL,
    tooltip_border_color = NULL,
    tooltip_color = NULL,
    negative_color = SIGN_COLORS[["negative"]],
    neutral_color = SIGN_COLORS[["neutral"]],
    positive_color = SIGN_COLORS[["positive"]],
    axis_label_gap = NULL,
    axis_name_gap = NULL,
    axis_name_location = NULL,
    axis_line_show = NULL,
    echarts = NULL,
    origin = NULL,
    writer = NULL,
    color = NULL,
    background_color = NULL,
    text_style = NULL,
    title = NULL,
    legend = NULL,
    tooltip = NULL,
    data_zoom = NULL,
    line = NULL,
    radar = NULL,
    bar = NULL,
    pie = NULL,
    scatter = NULL,
    category_axis = NULL,
    value_axis = NULL,
    log_axis = NULL,
    time_axis = NULL
  ) {
    legacy <- list(
      color = color,
      background_color = background_color,
      text_style = text_style,
      title = title,
      legend = legend,
      tooltip = tooltip,
      data_zoom = data_zoom,
      line = line,
      radar = radar,
      bar = bar,
      pie = pie,
      scatter = scatter,
      category_axis = category_axis,
      value_axis = value_axis,
      log_axis = log_axis,
      time_axis = time_axis
    )
    legacy <- legacy[!vapply(legacy, is.null, logical(1L))]
    origin <- origin %||% theme_origin(match.call())
    if (length(legacy) > 0L) {
      warn(
        "Deprecated `Theme()` argument(s): ",
        paste0("`", names(legacy), "`", collapse = ", "),
        ". `color` sets `palette`, `background_color` sets `bg_color`, and ",
        "the others set keys of `echarts`.",
        use_warning = TRUE
      )
      if (!is.null(legacy[["color"]])) {
        palette <- legacy[["color"]]
      }
      if (!is.null(legacy[["background_color"]])) {
        bg_color <- legacy[["background_color"]]
      }
      patch <- legacy[setdiff(names(legacy), c("color", "background_color"))]
      if (
        !is.null(patch[["text_style"]]) &&
          S7::S7_inherits(patch[["text_style"]], TextStyle)
      ) {
        patch[["text_style"]] <- to_list(patch[["text_style"]])
      }
      names(patch) <- unname(THEME_LEGACY_ARGS[names(patch)])
      echarts <- utils::modifyList(patch, echarts %||% list())
      if (length(echarts) == 0L) echarts <- NULL
    }
    S7::new_object(
      S7::S7_object(),
      scheme = scheme,
      font_family = font_family,
      base_font_size = base_font_size,
      title_font_size = title_font_size,
      subtitle_font_size = subtitle_font_size,
      axis_label_font_size = axis_label_font_size,
      legend_font_size = legend_font_size,
      tooltip_font_size = tooltip_font_size,
      color_scale_font_size = color_scale_font_size,
      font_weight = font_weight,
      title_font_weight = title_font_weight,
      palette = palette_colors(palette),
      bg_color = bg_color,
      fg_color = fg_color,
      title_color = title_color,
      subtitle_color = subtitle_color,
      legend_color = legend_color,
      axis_color = axis_color,
      grid_color = grid_color,
      grid_opacity = grid_opacity,
      grid_style = grid_style,
      tooltip_bg = tooltip_bg,
      tooltip_border_color = tooltip_border_color,
      tooltip_color = tooltip_color,
      negative_color = negative_color,
      neutral_color = neutral_color,
      positive_color = positive_color,
      axis_label_gap = axis_label_gap,
      axis_name_gap = axis_name_gap,
      axis_name_location = axis_name_location,
      axis_line_show = axis_line_show,
      echarts = echarts,
      origin = origin,
      writer = writer
    )
  }
)

# The tokens of a theme: every property but its provenance.
THEME_TOKENS <- setdiff(names(Theme@properties), c("origin", "writer"))

#' Where each token of a theme came from
#'
#' The tokens named in `call` are `"user"`, the rest `"default"`. The
#' deprecated `Theme()` arguments map to the tokens they set: `color` to
#' `palette`, `background_color` to `bg_color`, and the others to `echarts`;
#' [theme_light()]'s `color` is its `palette`.
#'
#' @param call Call: The call of `Theme()`, [theme_light()], or [theme_dark()].
#'
#' @return Named character vector, one entry per token.
#' @keywords internal
#' @noRd
theme_origin <- function(call) {
  supplied <- setdiff(names(as.list(call))[-1L], "")
  supplied[supplied == "color"] <- "palette"
  supplied[supplied == "background_color"] <- "bg_color"
  supplied[
    supplied %in%
      setdiff(names(THEME_LEGACY_ARGS), c("color", "background_color"))
  ] <- "echarts"
  stats::setNames(
    ifelse(THEME_TOKENS %in% supplied, "user", "default"),
    THEME_TOKENS
  )
}

#' The token document of a theme
#'
#' Every set property of `x`, under its own name, for the widgets and SVG export
#' to project in JavaScript. The palette is an `AsIs` vector, so a one-color
#' palette serializes as a JSON array.
#'
#' @param x `Theme` object.
#' @param ... Not used.
#' @return Named list: The theme document (`ThemeDocument` in `@rtemis/draw`).
#' @keywords internal
#' @noRd
S7::method(to_list, Theme) <- function(x, ...) {
  out <- lapply(
    stats::setNames(nm = S7::prop_names(x)),
    function(name) S7::prop(x, name)
  )
  out <- out[!vapply(out, is.null, logical(1L))]
  out[["palette"]] <- I(out[["palette"]])
  # Maps serialize as JSON objects.
  for (map in c("origin", "writer")) {
    if (!is.null(out[[map]])) out[[map]] <- as.list(out[[map]])
  }
  out
}

#' A theme argument as a `Theme`
#'
#' `NULL` and `NA` pass through. A `Theme` is returned as it is. A list is
#' deprecated: one whose names are `Theme()` arguments builds a `Theme` from
#' them; any other list is an ECharts theme object and becomes the `echarts`
#' patch of the light theme.
#'
#' @param theme Optional `Theme`, list, or `NA`: The theme, as passed to
#'   [draw()].
#' @return `Theme` object, `NULL`, or `NA`.
#' @keywords internal
#' @noRd
as_theme <- function(theme) {
  if (is.null(theme) || identical(theme, NA) || S7::S7_inherits(theme, Theme)) {
    return(theme)
  }
  if (is.list(theme)) {
    warn(
      "A list `theme` is deprecated; pass a `Theme`, e.g. ",
      "`theme_light(echarts = list(...))`.",
      use_warning = TRUE
    )
    if (length(theme) > 0L && all(names(theme) %in% names(formals(Theme)))) {
      return(do.call(Theme, theme))
    }
    return(theme_light(echarts = theme))
  }
  abort(
    "`theme` must be a `Theme`, `NULL`, or `NA`.",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
}

# -- Font family ----------------------------------------------------------------

# Font stack that follows a user's font family, and the whole font family of a
# theme when neither `font_family` nor the `rtemis.font_family` option is set.
DEFAULT_FONT_FAMILY <- "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"

# CSS generic family names and the vendor keywords for the system UI font.
# These are keywords: quoting one turns it into a family name that no font has.
GENERIC_FONT_FAMILIES <- c(
  "serif",
  "sans-serif",
  "monospace",
  "cursive",
  "fantasy",
  "math",
  "emoji",
  "fangsong",
  "system-ui",
  "ui-serif",
  "ui-sans-serif",
  "ui-monospace",
  "ui-rounded",
  "-apple-system",
  "blinkmacsystemfont"
)

#' Split a CSS font stack into its family names
#'
#' @param x Character scalar: CSS `font-family` value.
#' @return Character vector: Family names without surrounding quotes or
#'   whitespace, in stack order.
#' @keywords internal
#' @noRd
font_stack_entries <- function(x) {
  entries <- trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  entries <- entries[nzchar(entries)]
  sub("^(['\"])(.*)\\1$", "\\2", entries)
}

#' Resolve a font family into a complete CSS font stack
#'
#' Quotes every family name that is not a sequence of CSS identifiers or a CSS
#' variable reference, e.g. `Source Sans 3`, and appends `DEFAULT_FONT_FAMILY`
#' unless the stack ends in a generic family, so that text falls back to the
#' system UI font when the named fonts are missing.
#'
#' @param font_family Optional Character scalar: Font family name or CSS font
#'   stack. `NULL` returns `DEFAULT_FONT_FAMILY`.
#' @return Character scalar: CSS `font-family` value.
#' @keywords internal
#' @noRd
font_stack <- function(font_family) {
  if (is.null(font_family)) {
    return(DEFAULT_FONT_FAMILY)
  }
  entries <- if (
    is.character(font_family) &&
      length(font_family) == 1L &&
      !is.na(font_family)
  ) {
    trimws(strsplit(font_family, ",", fixed = TRUE)[[1L]])
  }
  entries <- entries[nzchar(entries)]
  if (length(entries) == 0L) {
    abort(
      "`font_family` and the `rtemis.font_family` option must be a nonempty ",
      "character scalar, e.g. \"Geist\".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  quoted <- grepl("^(['\"]).*\\1$", entries)
  generic <- tolower(entries) %in% GENERIC_FONT_FAMILIES
  ident <- grepl(
    "^-?[A-Za-z_][A-Za-z0-9_-]*( +-?[A-Za-z_][A-Za-z0-9_-]*)*$",
    entries
  )
  # A CSS variable reference (`var(--font-sans)`) is kept as written.
  variable <- grepl("^var\\(.*\\)$", entries, ignore.case = TRUE)
  bare <- !quoted & !generic & !ident & !variable
  entries[bare] <- paste0("'", gsub("'", "\\\\'", entries[bare]), "'")
  if (!generic[length(generic)]) {
    entries <- c(entries, DEFAULT_FONT_FAMILY)
  }
  paste(entries, collapse = ", ")
}


# -- Resolution ------------------------------------------------------------------

#' Resolve a theme's derived tokens
#'
#' Fills the tokens a theme leaves to rules of the theme itself, and marks each
#' one `"derived"`: the title size, 1.2 times `base_font_size` rounded half to
#' even; the subtitle, axis label, legend, tooltip, and color scale sizes,
#' `base_font_size`; the title, subtitle, legend, and tooltip colors,
#' `fg_color`; and the font family, the system UI font stack. Tokens left to a
#' backend's own default, such as `axis_color`, stay unset. A port of
#' `resolveTheme()` in `@rtemis/draw`, held equal to it by shared fixtures.
#'
#' @param theme `Theme` object.
#' @return `Theme` object with the derived tokens set and `writer` stamped.
#' @keywords internal
#' @noRd
resolve_theme <- function(theme) {
  origin <- theme@origin %||% theme_origin(quote(Theme()))
  derive <- function(name, value) {
    if (is.null(S7::prop(theme, name))) {
      S7::prop(theme, name) <<- value
      origin[[name]] <<- "derived"
    }
  }
  base <- theme@base_font_size
  # R's round() rounds half to even, as the TypeScript port does.
  derive("title_font_size", round(base * 1.2))
  for (name in c(
    "subtitle_font_size",
    "axis_label_font_size",
    "legend_font_size",
    "tooltip_font_size",
    "color_scale_font_size"
  )) {
    derive(name, base)
  }
  for (name in c(
    "title_color",
    "subtitle_color",
    "legend_color",
    "tooltip_color"
  )) {
    derive(name, theme@fg_color)
  }
  derive("font_family", DEFAULT_FONT_FAMILY)
  theme@origin <- origin
  theme@writer <- chart_writer()
  theme
} # /rtemis.draw::resolve_theme

#' The record of a theme
#'
#' The resolved theme as a list for a chart record's `theme` member: every
#' token, unset ones as explicit nulls, with its origin and writer. `NULL`
#' (the automatic light/dark theme) records the light theme with every origin
#' `"default"`, so a reader may resolve it again for its own scheme.
#'
#' @param theme Optional `Theme` object.
#' @return Named list.
#' @keywords internal
#' @noRd
theme_record <- function(theme = NULL) {
  if (is.null(theme)) {
    theme <- theme_light()
    theme@origin <- stats::setNames(
      rep("default", length(THEME_TOKENS)),
      THEME_TOKENS
    )
  }
  resolved <- resolve_theme(theme)
  out <- lapply(stats::setNames(nm = S7::prop_names(resolved)), function(name) {
    S7::prop(resolved, name)
  })
  out[["palette"]] <- I(out[["palette"]])
  out[["origin"]] <- as.list(out[["origin"]])
  out[["writer"]] <- as.list(out[["writer"]])
  out
} # /rtemis.draw::theme_record


# -- Built-in themes -----------------------------------------------------------

#' Light Theme
#'
#' Returns the light theme: dark text on white, the rtemis palette, and the sign
#' colors. Title text defaults to 1.2 times `base_font_size`; all other text to
#' `base_font_size`. Tick labels sit 2/3 em from the plot, axis names 0.625 em
#' clear of the tick labels, and axis names are centered on their axis.
#'
#' @inheritParams Theme
#' @param font_family Optional Character: Font family name or CSS font stack,
#'   e.g. `"Geist"` or `"Geist, Helvetica, sans-serif"`. Unless the stack ends
#'   in a generic family such as `sans-serif`, the system UI font stack follows
#'   it as the fallback. A family name that is not a sequence of CSS
#'   identifiers, e.g. `Source Sans 3`, is quoted. `NULL` uses the system UI
#'   font stack. The default is the `rtemis.font_family` option, which sets the
#'   font of every rtemis plot, e.g. `options(rtemis.font_family = "Geist")`. A
#'   widget uses the font when the viewer's system has it installed or the page
#'   loads it.
#' @param color Character: Categorical palette.
#' @return [Theme]: Theme object.
#' @author EDG
#' @export
#'
#' @examples
#' theme_light(base_font_size = 14)
theme_light <- function(
  base_font_size = 12,
  title_font_size = NULL,
  subtitle_font_size = NULL,
  axis_label_font_size = NULL,
  legend_font_size = NULL,
  tooltip_font_size = NULL,
  font_family = getOption("rtemis.font_family"),
  color = palette_colors(rtemis_colors),
  bg_color = "#ffffff",
  fg_color = "rgba(0, 0, 0, 0.7)",
  title_color = NULL,
  subtitle_color = NULL,
  legend_color = NULL,
  axis_color = NULL,
  grid_color = NULL,
  tooltip_bg = NULL,
  tooltip_border_color = NULL,
  tooltip_color = NULL,
  negative_color = SIGN_COLORS[["negative"]],
  neutral_color = SIGN_COLORS[["neutral"]],
  positive_color = SIGN_COLORS[["positive"]],
  font_weight = NULL,
  title_font_weight = NULL,
  grid_opacity = NULL,
  grid_style = NULL,
  axis_label_gap = 2 / 3,
  axis_name_gap = 0.625,
  axis_name_location = "middle",
  axis_line_show = NULL,
  color_scale_font_size = NULL,
  echarts = NULL
) {
  Theme(
    scheme = "light",
    font_family = font_family,
    base_font_size = base_font_size,
    title_font_size = title_font_size,
    subtitle_font_size = subtitle_font_size,
    axis_label_font_size = axis_label_font_size,
    legend_font_size = legend_font_size,
    tooltip_font_size = tooltip_font_size,
    font_weight = font_weight,
    title_font_weight = title_font_weight,
    palette = color,
    bg_color = bg_color,
    fg_color = fg_color,
    title_color = title_color,
    subtitle_color = subtitle_color,
    legend_color = legend_color,
    axis_color = axis_color,
    grid_color = grid_color,
    grid_opacity = grid_opacity,
    grid_style = grid_style,
    tooltip_bg = tooltip_bg,
    tooltip_border_color = tooltip_border_color,
    tooltip_color = tooltip_color,
    negative_color = negative_color,
    neutral_color = neutral_color,
    positive_color = positive_color,
    axis_label_gap = axis_label_gap,
    axis_name_gap = axis_name_gap,
    axis_name_location = axis_name_location,
    axis_line_show = axis_line_show,
    color_scale_font_size = color_scale_font_size,
    echarts = echarts,
    origin = theme_origin(match.call())
  )
} # /rtemis.draw::theme_light

#' Dark Theme
#'
#' Returns the dark theme: light text on near-black, the rtemis palette, and the
#' sign colors, which keep their contrast on either background. Title text
#' defaults to 1.2 times `base_font_size`; all other text to `base_font_size`.
#' The axis spacing is that of [theme_light()].
#'
#' @inheritParams theme_light
#' @return [Theme]: Theme object.
#' @author EDG
#' @export
#'
#' @examples
#' theme_dark(base_font_size = 14)
theme_dark <- function(
  base_font_size = 12,
  title_font_size = NULL,
  subtitle_font_size = NULL,
  axis_label_font_size = NULL,
  legend_font_size = NULL,
  tooltip_font_size = NULL,
  font_family = getOption("rtemis.font_family"),
  color = palette_colors(rtemis_colors),
  bg_color = "#181818",
  fg_color = "rgba(255, 255, 255, 0.7)",
  title_color = "rgba(255, 255, 255, 0.9)",
  subtitle_color = "rgba(255, 255, 255, 0.5)",
  legend_color = NULL,
  axis_color = "rgba(255, 255, 255, 0.3)",
  grid_color = "rgba(255, 255, 255, 0.1)",
  tooltip_bg = "rgba(20, 20, 20, 0.9)",
  tooltip_border_color = "rgba(255, 255, 255, 0.1)",
  tooltip_color = NULL,
  negative_color = SIGN_COLORS[["negative"]],
  neutral_color = SIGN_COLORS[["neutral"]],
  positive_color = SIGN_COLORS[["positive"]],
  font_weight = NULL,
  title_font_weight = NULL,
  grid_opacity = NULL,
  grid_style = NULL,
  axis_label_gap = 2 / 3,
  axis_name_gap = 0.625,
  axis_name_location = "middle",
  axis_line_show = NULL,
  color_scale_font_size = NULL,
  echarts = NULL
) {
  Theme(
    scheme = "dark",
    font_family = font_family,
    base_font_size = base_font_size,
    title_font_size = title_font_size,
    subtitle_font_size = subtitle_font_size,
    axis_label_font_size = axis_label_font_size,
    legend_font_size = legend_font_size,
    tooltip_font_size = tooltip_font_size,
    font_weight = font_weight,
    title_font_weight = title_font_weight,
    palette = color,
    bg_color = bg_color,
    fg_color = fg_color,
    title_color = title_color,
    subtitle_color = subtitle_color,
    legend_color = legend_color,
    axis_color = axis_color,
    grid_color = grid_color,
    grid_opacity = grid_opacity,
    grid_style = grid_style,
    tooltip_bg = tooltip_bg,
    tooltip_border_color = tooltip_border_color,
    tooltip_color = tooltip_color,
    negative_color = negative_color,
    neutral_color = neutral_color,
    positive_color = positive_color,
    axis_label_gap = axis_label_gap,
    axis_name_gap = axis_name_gap,
    axis_name_location = axis_name_location,
    axis_line_show = axis_line_show,
    color_scale_font_size = color_scale_font_size,
    echarts = echarts,
    origin = theme_origin(match.call())
  )
} # /rtemis.draw::theme_dark
