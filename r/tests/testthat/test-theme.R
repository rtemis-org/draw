# test-theme.R
# ::rtemis.draw::
# 2026- EDG rtemis.org

# The theme token document (Theme), the built-in themes, the deprecated
# arguments, and the projection @rtemis/draw makes of R's documents.

# The ECharts theme objects the bundled @rtemis/draw projects from `themes`.
project_themes <- function(themes) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      builders = system.file(
        "htmlwidgets/lib/draw/rtemis-draw-builders.js",
        package = "rtemis.draw"
      ),
      themes = lapply(themes, to_list)
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "theme_projection.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  jsonlite::fromJSON(paste(output, collapse = ""), simplifyVector = FALSE)
}

# -- Theme -----------------------------------------------------------------------

test_that("Theme() holds the light theme's tokens", {
  th <- Theme()
  expect_true(S7::S7_inherits(th, Theme))
  out <- to_list(th)
  expect_identical(out[["scheme"]], "light")
  expect_identical(out[["base_font_size"]], 12)
  expect_identical(out[["bg_color"]], "#ffffff")
  expect_identical(out[["fg_color"]], "rgba(0, 0, 0, 0.7)")
  expect_identical(as.character(out[["palette"]]), unname(rtemis_colors))
  expect_identical(out[["negative_color"]], SIGN_COLORS[["negative"]])
  # Unset tokens are left out of the document.
  expect_false(any(c("font_family", "title_color", "echarts") %in% names(out)))
})

test_that("to_list() writes the token document under the tokens' names", {
  th <- Theme(
    palette = "#123456",
    font_family = "Geist",
    font_weight = 500,
    grid_opacity = 0.5,
    grid_style = "dashed",
    axis_label_gap = 0.5,
    axis_name_gap = 0.625,
    axis_name_location = "middle",
    axis_line_show = FALSE,
    color_scale_font_size = 11,
    echarts = list(title = list(textStyle = list(fontSize = 20)))
  )
  out <- to_list(th)
  expect_false(out[["axis_line_show"]])
  expect_identical(out[["color_scale_font_size"]], 11)
  expect_identical(out[["font_family"]], "Geist")
  expect_identical(out[["font_weight"]], 500)
  expect_identical(out[["grid_style"]], "dashed")
  expect_identical(out[["axis_name_location"]], "middle")
  expect_identical(out[["echarts"]][["title"]][["textStyle"]][["fontSize"]], 20)
  # A one-color palette is still an array in JSON.
  json <- jsonlite::toJSON(out, auto_unbox = TRUE)
  expect_match(json, '"palette":["#123456"]', fixed = TRUE)
})

test_that("Theme validates its tokens", {
  expect_error(Theme(scheme = "sepia"))
  expect_error(Theme(base_font_size = 0))
  expect_error(Theme(font_weight = 2000))
  expect_error(Theme(grid_opacity = 1.5))
  expect_error(Theme(grid_style = "wavy"))
  expect_error(Theme(axis_name_location = "left"))
  expect_error(Theme(axis_line_show = "no"))
  expect_error(Theme(color_scale_font_size = 0))
  expect_error(Theme(palette = character()))
  expect_error(Theme(palette = 123))
  expect_error(Theme(font_family = ""))
})

test_that("deprecated Theme() arguments map to tokens and the ECharts patch", {
  expect_warning(
    th <- Theme(
      color = c("#5470c6", "#91cc75"),
      background_color = "#fafafa",
      text_style = TextStyle(font_size = 14),
      title = list(textStyle = list(fontSize = 20)),
      category_axis = list(splitLine = list(show = TRUE)),
      data_zoom = list(fillerColor = "#777777")
    ),
    "Deprecated `Theme\\(\\)` argument"
  )
  expect_identical(th@palette, c("#5470c6", "#91cc75"))
  expect_identical(th@bg_color, "#fafafa")
  expect_identical(th@echarts[["textStyle"]][["fontSize"]], 14)
  expect_identical(th@echarts[["title"]][["textStyle"]][["fontSize"]], 20)
  expect_identical(th@echarts[["categoryAxis"]][["splitLine"]][["show"]], TRUE)
  expect_identical(th@echarts[["dataZoom"]][["fillerColor"]], "#777777")
  # The old properties are gone.
  expect_error(th@background_color)
})

test_that("a list theme is deprecated and becomes a Theme", {
  expect_warning(th <- as_theme(list(bg_color = "#202830")), "deprecated")
  expect_identical(th@bg_color, "#202830")
  expect_warning(
    th <- as_theme(list(backgroundColor = "#202830")),
    "deprecated"
  )
  expect_identical(th@scheme, "light")
  expect_identical(th@echarts[["backgroundColor"]], "#202830")
  expect_null(as_theme(NULL))
  expect_identical(as_theme(NA), NA)
  expect_error(as_theme("dark"), class = "rtemis_type_error")
})

# -- Built-in themes -----------------------------------------------------------

test_that("theme_light() and theme_dark() state their scheme and colors", {
  old <- options(rtemis.font_family = NULL)
  on.exit(options(old), add = TRUE)
  light <- theme_light()
  expect_identical(light@scheme, "light")
  expect_identical(light@bg_color, "#ffffff")
  expect_null(light@font_family)
  dark <- theme_dark(base_font_size = 14, grid_style = "dotted")
  expect_identical(dark@scheme, "dark")
  expect_identical(dark@bg_color, "#181818")
  expect_identical(dark@fg_color, "rgba(255, 255, 255, 0.7)")
  expect_identical(dark@grid_color, "rgba(255, 255, 255, 0.1)")
  expect_identical(dark@base_font_size, 14)
  expect_identical(dark@grid_style, "dotted")
})

test_that("the rtemis.font_family option sets the theme font", {
  old <- options(rtemis.font_family = "Geist")
  on.exit(options(old), add = TRUE)
  for (th in list(theme_light(), theme_dark())) {
    expect_identical(th@font_family, "Geist")
  }
  # An explicit argument takes precedence over the option.
  expect_identical(
    theme_light(font_family = "Arial, sans-serif")@font_family,
    "Arial, sans-serif"
  )
  options(rtemis.font_family = 12)
  expect_error(theme_light())
})

# -- Projection ------------------------------------------------------------------

test_that("@rtemis/draw projects R's theme documents to ECharts themes", {
  skip_if_no_node()
  old <- options(rtemis.font_family = "Geist")
  on.exit(options(old), add = TRUE)
  out <- project_themes(list(
    theme_light(),
    theme_dark(),
    theme_light(echarts = list(title = list(textStyle = list(fontSize = 30))))
  ))
  light <- out[[1L]]
  dark <- out[[2L]]
  expect_identical(light[["backgroundColor"]], "#ffffff")
  expect_identical(
    light[["textStyle"]][["fontFamily"]],
    paste0("Geist, ", DEFAULT_FONT_FAMILY)
  )
  expect_identical(light[["textStyle"]][["fontSize"]], 12L)
  expect_identical(light[["title"]][["textStyle"]][["fontSize"]], 14L)
  expect_false(light[["categoryAxis"]][["splitLine"]][["show"]])
  # Neutral zoom sliders, by scheme.
  expect_identical(light[["dataZoom"]][["handleStyle"]][["color"]], "#ffffff")
  expect_identical(dark[["dataZoom"]][["handleStyle"]][["color"]], "#303030")
  # The radar coordinate system takes the axis and grid colors.
  expect_identical(dark[["radar"]][["symbol"]], "circle")
  expect_identical(
    dark[["radar"]][["axisLine"]][["lineStyle"]][["color"]],
    "rgba(255, 255, 255, 0.3)"
  )
  expect_null(light[["radar"]][["splitLine"]])
  # The ECharts patch merges over the projection.
  patched <- out[[3L]][["title"]][["textStyle"]]
  expect_identical(patched[["fontSize"]], 30L)
  expect_identical(patched[["color"]], "rgba(0, 0, 0, 0.7)")
})

test_that("neutral slider themes survive native rendering and allow chart overrides", {
  skip_if_no_node()
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      echarts = system.file(
        "htmlwidgets/lib/echarts/echarts.min.js",
        package = "rtemis.draw"
      ),
      builders = system.file(
        "htmlwidgets/lib/draw/rtemis-draw-builders.js",
        package = "rtemis.draw"
      ),
      themes = list(to_list(theme_light()), to_list(theme_dark()))
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )
  output <- system2(
    Sys.which("node"),
    c(shQuote(test_path("fixtures", "slider_theme.js")), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "Neutral slider themes")
})

# -- Font family -------------------------------------------------------------------

test_that("font_stack appends the system UI fallback to a font family", {
  expect_identical(font_stack(NULL), DEFAULT_FONT_FAMILY)
  expect_identical(font_stack("Geist"), paste0("Geist, ", DEFAULT_FONT_FAMILY))
  expect_identical(font_stack("Geist, sans-serif"), "Geist, sans-serif")
  expect_identical(font_stack("Geist, system-ui"), "Geist, system-ui")
  expect_identical(
    font_stack("Source Sans 3"),
    paste0("'Source Sans 3', ", DEFAULT_FONT_FAMILY)
  )
  expect_identical(
    font_stack("var(--font-sans), sans-serif"),
    "var(--font-sans), sans-serif"
  )
  expect_identical(
    font_stack("\"Helvetica Neue\", Arial"),
    paste0("\"Helvetica Neue\", Arial, ", DEFAULT_FONT_FAMILY)
  )
})

test_that("font_stack rejects anything but a nonempty string", {
  for (bad in list(1, NA_character_, "", " , ", c("Geist", "Arial"))) {
    expect_error(font_stack(bad), class = "rtemis_type_error")
  }
})

test_that("font_stack_entries returns unquoted family names", {
  expect_identical(
    font_stack_entries("'Source Sans 3', \"Geist\", sans-serif"),
    c("Source Sans 3", "Geist", "sans-serif")
  )
})
