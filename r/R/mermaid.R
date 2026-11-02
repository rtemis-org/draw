# mermaid.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# Mermaid diagrams rendered with Pele (https://pele.run), the fourth rendering
# backend after ECharts, Sigma.js and MapLibre.
# spec: draw/mermaid-diagrams
#
# Pipeline:
#   draw_mermaid(source, ...)               # Mermaid text -> PeleOption (S7)
#     -> mermaid_option()                   # shared with compile(MermaidConfig)
#     -> draw(PeleOption)                   # dispatch -> htmlwidget ("rtemis-diagram")
#     -> rtemis-diagram.js                  # Pele renders SVG in the browser
#
# The Mermaid source is the bound data; the option carries render settings.
# Colors and fonts come from the theme in the browser
# (lib/draw/diagram_theme.js), which also serves SVG export.

# %% PELE_SOURCE_LIMIT ----
# Pele's default limit on source length, in characters.
PELE_SOURCE_LIMIT <- 50000L


# %% PeleOption ----
#' Pele Render Option
#'
#' The complete, validated render specification for a Mermaid diagram drawn
#' with Pele: the Mermaid source plus its render settings. This is the Pele
#' analog of [EChartsOption], [SigmaOption], and [MapLibreOption], the object
#' [draw()] dispatches on to create a `rtemis-diagram` widget. The theme is an
#' argument to [draw()], not a property.
#'
#' Most users call [draw_mermaid()], which builds this object.
#'
#' @param source Character: Mermaid diagram text, one string.
#' @param font_size Numeric (0, Inf): Label font size in pixels.
#' @param padding Optional Numeric \[0, Inf): Space around the diagram in
#'   pixels. `NULL` uses Pele's default.
#' @param auto_direction Logical: Allow Pele to draw left-to-right flowcharts
#'   top-to-bottom in elements narrower than 640 pixels.
#' @param fit Logical: Shrink the diagram to the widget width, with pinch and
#'   Ctrl/Cmd-wheel zoom. `FALSE` draws it at its natural size in a scrolling
#'   widget.
#' @param palette Optional Character: Colors for categorical entities, such as
#'   pie slices and mindmap branches. Pele uses the first eight. `NULL` uses the
#'   theme palette.
#' @param node_color Optional Named character: Colors for selected nodes,
#'   named by node id. Each named node gets a border in its color and a fill of
#'   the same color at `fill_alpha` opacity.
#' @param fill_alpha Numeric \[0, 1\]: Fill opacity of the nodes in
#'   `node_color`.
#' @param critical_color Character: Color of critical Gantt tasks and
#'   high-priority kanban items.
#'
#' @return `PeleOption` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' PeleOption(source = "flowchart LR\n  A --> B")
PeleOption <- S7::new_class(
  "PeleOption",
  package = "rtemis.draw",
  properties = list(
    source = prop_string(description = "Mermaid diagram text."),
    font_size = prop_float(
      14,
      exclusive_min = 0,
      description = "Label font size in pixels."
    ),
    padding = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      description = "Space around the diagram in pixels."
    ),
    auto_direction = prop_boolean(
      FALSE,
      description = "Allow top-to-bottom flowcharts in narrow elements."
    ),
    fit = prop_boolean(
      FALSE,
      description = "Shrink the diagram to the widget width."
    ),
    palette = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Colors for categorical entities."
    ),
    node_color = prop_string(
      NULL,
      nullable = TRUE,
      map = TRUE,
      description = "Colors for selected nodes, keyed by node id."
    ),
    fill_alpha = prop_float(
      0.1,
      min = 0,
      max = 1,
      description = "Fill opacity of the nodes in node_color."
    ),
    critical_color = prop_string(
      rtemis_colors[["red"]],
      description = "Color of critical Gantt tasks and high-priority items."
    )
  )
) # /rtemis.draw::PeleOption


# %% to_list.PeleOption ----
# The `rtemis-diagram` payload: the source and a `style` object read by
# rtemis-diagram.js and lib/draw/diagram_theme.js.
S7::method(to_list, PeleOption) <- function(x, ...) {
  style <- list(
    fontSize = x@font_size,
    padding = x@padding,
    autoDirection = x@auto_direction,
    fit = x@fit,
    # as.list() keeps a one-color palette a JSON array under auto_unbox.
    palette = if (!is.null(x@palette)) as.list(unname(x@palette)),
    # A named list serializes as a JSON object keyed by node id.
    nodeColor = if (!is.null(x@node_color)) as.list(x@node_color),
    fillAlpha = x@fill_alpha,
    criticalColor = x@critical_color
  )
  list(
    source = x@source,
    style = style[!vapply(style, is.null, logical(1L))]
  )
} # /rtemis.draw::to_list.PeleOption


# %% draw.PeleOption ----
# Pele backend: render the diagram as a `rtemis-diagram` widget. `autoHeight`
# tells the binding that R left the height unset, so a document sizes the
# widget to the diagram.
S7::method(draw, PeleOption) <- function(
  option,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL,
  # Accepted to match the draw() generic; Pele diagrams have no animation.
  animation = NULL,
  ...
) {
  check_dots_empty(...)

  payload <- to_list(option)
  payload[["autoHeight"]] <- is.null(height)

  widget <- render_widget(
    "rtemis-diagram",
    payload,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id
  )
  if (!is.null(filename)) {
    save_drawing(widget, filename, width, height)
  }
  widget
} # /rtemis.draw::draw.PeleOption


# %% mermaid_source ----
#' Validate and join Mermaid source text
#'
#' @param source Character: Mermaid text, one string or one element per line.
#'
#' @return Character scalar: The lines joined with newlines.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mermaid_source <- function(source) {
  if (!is.character(source) || length(source) == 0L || anyNA(source)) {
    abort(
      "Supply the Mermaid source as a character vector with no missing ",
      "values: one string, or one element per line.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  source <- paste(source, collapse = "\n")
  if (!nzchar(trimws(source))) {
    abort(
      "The Mermaid source is empty. Supply diagram text, e.g. ",
      "\"flowchart LR\\n  A --> B\".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (nchar(source, type = "chars") > PELE_SOURCE_LIMIT) {
    abort(
      "The Mermaid source has ",
      nchar(source, type = "chars"),
      " characters; Pele renders at most ",
      PELE_SOURCE_LIMIT,
      ". Split the diagram into smaller diagrams.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  source
} # /rtemis.draw::mermaid_source


# %% mermaid_with_title ----
#' Set the title in Mermaid frontmatter
#'
#' Pele draws the `title` of a diagram's YAML frontmatter, the block between
#' two `---` lines at the start of the source. `title` replaces a title the
#' source states and is added to existing frontmatter, or to new frontmatter
#' when the source has none. The value is written as a double-quoted YAML
#' string.
#'
#' @param source Character scalar: Mermaid source.
#' @param title Optional Character scalar: Title. `NULL` returns `source`
#'   unchanged.
#'
#' @return Character scalar: Mermaid source.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mermaid_with_title <- function(source, title = NULL) {
  if (is.null(title)) {
    return(source)
  }
  value <- gsub("[\r\n]+", " ", title)
  value <- gsub("([\"\\\\])", "\\\\\\1", value)
  title_line <- paste0("title: \"", value, "\"")
  lines <- strsplit(source, "\n", fixed = TRUE)[[1L]]
  if (length(lines) > 1L && trimws(lines[[1L]]) == "---") {
    close <- which(trimws(lines[-1L]) == "---")
    if (length(close) > 0L) {
      end <- close[[1L]] + 1L
      header <- if (end > 2L) lines[2:(end - 1L)] else character()
      header <- header[!grepl("^title\\s*:", header)]
      return(paste(
        c("---", title_line, header, lines[end:length(lines)]),
        collapse = "\n"
      ))
    }
  }
  paste(c("---", title_line, "---", lines), collapse = "\n")
} # /rtemis.draw::mermaid_with_title


# %% check_node_color ----
#' Check node colors against the Mermaid source
#'
#' A node id that does not occur in the source colors nothing, so it is
#' reported as an error. The check is a fixed-string search: it detects a
#' misspelled id, not every id that Pele would fail to match.
#'
#' @param node_color Optional Named character: Colors keyed by node id.
#' @param source Character scalar: Mermaid source.
#'
#' @return `node_color`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_node_color <- function(node_color, source) {
  if (is.null(node_color)) {
    return(invisible(node_color))
  }
  ids <- names(node_color)
  if (!is.character(node_color) || is.null(ids)) {
    abort(
      "`node_color` must be a named character vector of colors, named by ",
      "node id, e.g. c(train = \"#6CA3A0\").",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  missing <- ids[!vapply(ids, grepl, logical(1L), x = source, fixed = TRUE)]
  if (length(missing) > 0L) {
    abort(
      "`node_color` names node ids that do not occur in the Mermaid source: ",
      paste(missing, collapse = ", "),
      ". Name each color by the id of a node in the source.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(node_color)
} # /rtemis.draw::check_node_color


# %% mermaid_option ----
#' Build the render option for a Mermaid diagram
#'
#' The single implementation shared by [draw_mermaid()] and `compile()` on a
#' [MermaidConfig]. The render targets stay with the caller.
#'
#' @inheritParams draw_mermaid
#'
#' @return [PeleOption]: The option object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mermaid_option <- function(
  source,
  palette = NULL,
  node_color = NULL,
  fill_alpha = 0.1,
  font_size = 14,
  padding = NULL,
  auto_direction = FALSE,
  fit = FALSE,
  title = NULL
) {
  source <- mermaid_source(source)
  check_node_color(node_color, source)
  PeleOption(
    source = mermaid_with_title(source, title),
    font_size = font_size,
    padding = padding,
    auto_direction = auto_direction,
    fit = fit,
    palette = if (!is.null(palette)) palette_colors(palette),
    node_color = node_color,
    fill_alpha = fill_alpha
  )
} # /rtemis.draw::mermaid_option


# %% draw_mermaid ----
#' Draw a Mermaid Diagram
#'
#' Renders Mermaid diagram text with Pele (<https://pele.run>) as a static SVG
#' in an htmlwidget, shown in the RStudio and VS Code Viewer panes, Quarto and
#' R Markdown documents, and Shiny. With `filename`, it also writes the diagram
#' to an SVG file.
#'
#' The diagram renders at its natural size, so labels keep `font_size` however
#' long or wide the diagram is, and the widget scrolls. In the Viewer pane the
#' widget fills the pane. In a document with `height` unset, the widget takes
#' the diagram's height and scrolls horizontally when the diagram is wider than
#' the page. `fit = TRUE` shrinks the diagram to the widget width instead.
#'
#' Uncolored elements use neutral grays that follow the light or dark theme.
#' Categorical entities use `palette`, or the theme palette. `node_color`
#' highlights selected nodes: each gets a border in its color and a fill of the
#' same color at `fill_alpha` opacity, so its label keeps the theme's text
#' color. It applies to the nodes of flowchart, state, class, entity
#' relationship, block, architecture, and kanban diagrams, which Pele draws with
#' the neutral fill and border.
#'
#' @section Supported Mermaid syntax:
#' Pele supports every diagram type built into Mermaid 12.1.0, including
#' flowchart, sequence, class, state, entity relationship, Gantt, git graph,
#' pie, mindmap, kanban, timeline, user journey, quadrant chart, XY chart,
#' requirement, Sankey, architecture, C4, block, packet, radar, treemap, and
#' Venn diagrams. The syntax and the per-type differences are listed at
#' <https://pele.run/compatibility>; examples of every type are at
#' <https://pele.run/examples>.
#'
#' Not supported: Mermaid themes and `themeVariables`, ZenUML and other
#' external Mermaid plugins, HTML labels, and JavaScript callbacks in `click`
#' statements. Configuration support varies by diagram type.
#'
#' Pele computes its own layout, so node positions, spacing, and edge paths
#' differ from Mermaid's. A flowchart subgraph keeps its own `direction` when
#' it links to nodes outside the subgraph. Pele renders at most 50,000 source
#' characters and 5,000 edges in a flowchart.
#'
#' Pele uses at most eight categorical colors and repeats them from the ninth
#' category. Colors set in the source with `style` or `classDef` override the
#' theme and stay the same in light and dark mode.
#'
#' @param source Character: Mermaid diagram text, as one string or one element
#'   per line.
#' @param palette Optional Character: Colors for categorical entities, such as
#'   pie slices and mindmap branches. `NULL` uses the theme palette.
#' @param node_color Optional Named character: Colors for selected nodes,
#'   named by node id, e.g. `c(train = rtemis_colors[["teal"]])`. A node
#'   styled in the source with `style` or `classDef` keeps that style.
#' @param fill_alpha Numeric \[0, 1\]: Fill opacity of the nodes in
#'   `node_color`.
#' @param font_size Numeric (0, Inf): Label font size in pixels.
#' @param padding Optional Numeric \[0, Inf): Space around the diagram in
#'   pixels. `NULL` uses Pele's default.
#' @param auto_direction Logical: Allow Pele to draw left-to-right flowcharts
#'   top-to-bottom in widgets narrower than 640 pixels.
#' @param fit Logical: Shrink the diagram to the widget width, with pinch and
#'   Ctrl/Cmd-wheel zoom, instead of drawing it at its natural size.
#' @param title Optional Character: Diagram title, written into the source's
#'   frontmatter. It replaces a title the source states.
#' @param theme Optional [Theme], list, or `NA`: Theme override. `NULL` enables
#'   light/dark auto-detection.
#' @param width Optional Character or Numeric: Widget width.
#' @param height Optional Character or Numeric: Widget height. `NULL` fills the
#'   Viewer pane and takes the diagram's height in documents.
#' @param element_id Optional Character: Explicit element ID.
#' @param filename Optional Character: Path of an SVG file to write with
#'   [save_drawing()]. The SVG has the diagram's natural size.
#'
#' @return htmlwidget.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Train/test split
#' draw_mermaid(c(
#'   "flowchart TB",
#'   "  data[(Data)] --> split[Split]",
#'   "  split --> train[(Training set)]",
#'   "  split --> test[(Test set)]",
#'   "  train --> fit[Train model]",
#'   "  fit -->|trained model| eval[Evaluate on test set]",
#'   "  test --> eval"
#' ), node_color = c(
#'   train = rtemis_colors[["teal"]],
#'   test = rtemis_colors[["magenta"]]
#' ))
#'
#' # Train/test split with hyperparameter tuning by resampling of the
#' # training set
#' draw_mermaid(c(
#'   "flowchart TB",
#'   "  data[(Data)] --> split[Split]",
#'   "  split --> train[(Training set)]",
#'   "  split --> test[(Test set)]",
#'   "  subgraph tuning[Resampling: tuning]",
#'   "    direction TB",
#'   "    inner_split[Split j = 1, ..., m] --> inner_train[(Inner training set j)]",
#'   "    inner_split --> val[(Validation set j)]",
#'   "    inner_train --> inner_fit[Train one model per hyperparameter set]",
#'   "    inner_fit --> inner_eval[Evaluate on validation set j]",
#'   "    val --> inner_eval",
#'   "  end",
#'   "  train --> inner_split",
#'   "  inner_eval -->|validation metrics| select[Select hyperparameters]",
#'   "  select -->|selected hyperparameters| fit[Train on training set]",
#'   "  train --> fit",
#'   "  fit -->|trained model| eval[Evaluate on test set]",
#'   "  test --> eval"
#' ), node_color = c(
#'   train = rtemis_colors[["teal"]],
#'   inner_train = rtemis_colors[["teal"]],
#'   val = rtemis_colors[["purple"]],
#'   test = rtemis_colors[["magenta"]]
#' ))
#'
#' # Nested resampling: the inner loop tunes hyperparameters on each outer
#' # training set, and the outer loop estimates generalization performance
#' draw_mermaid(c(
#'   "flowchart TB",
#'   "  data[(Data)] --> split[Outer split i = 1, ..., k]",
#'   "  subgraph outer[Outer resampling: evaluation]",
#'   "    direction TB",
#'   "    split --> train[(Outer training set i)]",
#'   "    split --> test[(Outer test set i)]",
#'   "    subgraph tuning[Inner resampling: tuning]",
#'   "      direction TB",
#'   "      inner_split[Inner split j = 1, ..., m] --> inner_train[(Inner training set j)]",
#'   "      inner_split --> val[(Validation set j)]",
#'   "      inner_train --> inner_fit[Train one model per hyperparameter set]",
#'   "      inner_fit --> inner_eval[Evaluate on validation set j]",
#'   "      val --> inner_eval",
#'   "    end",
#'   "    train --> inner_split",
#'   "    inner_eval -->|validation metrics| select[Select hyperparameters]",
#'   "    select -->|selected hyperparameters| fit[Train on outer training set i]",
#'   "    train --> fit",
#'   "    fit -->|trained model i| eval[Evaluate on outer test set i]",
#'   "    test --> eval",
#'   "  end",
#'   "  eval -->|test metrics, all i| summary[Mean and SD of test metrics]"
#' ), node_color = c(
#'   train = rtemis_colors[["teal"]],
#'   inner_train = rtemis_colors[["teal"]],
#'   val = rtemis_colors[["purple"]],
#'   test = rtemis_colors[["magenta"]]
#' ))
#'
#' # Pie chart of a data split using a single string for the source
#' draw_mermaid(
#'   "pie\n  \"Train\" : 70\n  \"Validation\" : 15\n  \"Test\" : 15",
#'   title = "Data split",
#'   palette = c(
#'     rtemis_colors[["teal"]],
#'     rtemis_colors[["purple"]],
#'     rtemis_colors[["magenta"]]
#'   )
#' )
draw_mermaid <- function(
  source,
  palette = NULL,
  node_color = NULL,
  fill_alpha = 0.1,
  font_size = 14,
  padding = NULL,
  auto_direction = FALSE,
  fit = FALSE,
  title = NULL,
  theme = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL,
  filename = NULL
) {
  option <- mermaid_option(
    source = source,
    palette = palette,
    node_color = node_color,
    fill_alpha = fill_alpha,
    font_size = font_size,
    padding = padding,
    auto_direction = auto_direction,
    fit = fit,
    title = title
  )
  draw(
    option,
    theme = theme,
    width = width,
    height = height,
    element_id = element_id,
    filename = filename
  )
} # /rtemis.draw::draw_mermaid
