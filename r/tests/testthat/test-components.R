# test-components.R
# Tests for Grid, Title, Legend, Tooltip S7 classes

# -- Grid -----------------------------------------------------------------------

test_that("Grid creates with defaults (all NULL)", {
  g <- Grid()
  expect_true(S7::S7_inherits(g, Grid))
  expect_equal(to_list(g), list())
})

test_that("Grid to_list() converts names", {
  g <- Grid(
    show = TRUE,
    left = "10%",
    right = "10%",
    top = 60,
    bottom = 60,
    contain_label = TRUE,
    background_color = "#fafafa",
    border_width = 1,
    border_color = "#ccc"
  )
  out <- to_list(g)
  expect_equal(out$show, TRUE)
  expect_equal(out$left, "10%")
  expect_equal(out$right, "10%")
  expect_equal(out$top, 60)
  expect_equal(out$bottom, 60)
  expect_equal(out$containLabel, TRUE)
  expect_equal(out$backgroundColor, "#fafafa")
  expect_equal(out$borderWidth, 1)
  expect_equal(out$borderColor, "#ccc")
})

test_that("Grid shadow fields work", {
  g <- Grid(
    shadow_blur = 10,
    shadow_color = "rgba(0,0,0,0.3)",
    shadow_offset_x = 2,
    shadow_offset_y = 2
  )
  out <- to_list(g)
  expect_equal(out$shadowBlur, 10)
  expect_equal(out$shadowColor, "rgba(0,0,0,0.3)")
  expect_equal(out$shadowOffsetX, 2)
  expect_equal(out$shadowOffsetY, 2)
})

test_that("Grid width/height accept string and number", {
  g1 <- Grid(width = "80%", height = 400)
  expect_equal(g1@width, "80%")
  expect_equal(g1@height, 400)
})

# -- Title ----------------------------------------------------------------------

test_that("Title creates with defaults", {
  t <- Title()
  expect_true(S7::S7_inherits(t, Title))
  expect_equal(to_list(t), list())
})

test_that("Title to_list() converts names", {
  t <- Title(
    show = TRUE,
    text = "Main Title",
    subtext = "Subtitle here",
    text_align = "center",
    text_vertical_align = "top",
    left = "center",
    top = 10,
    item_gap = 10,
    trigger_event = FALSE
  )
  out <- to_list(t)
  expect_equal(out$show, TRUE)
  expect_equal(out$text, "Main Title")
  expect_equal(out$subtext, "Subtitle here")
  expect_equal(out$textAlign, "center")
  expect_equal(out$textVerticalAlign, "top")
  expect_equal(out$left, "center")
  expect_equal(out$top, 10)
  expect_equal(out$itemGap, 10)
  expect_equal(out$triggerEvent, FALSE)
})

test_that("Title link and target", {
  t <- Title(
    text = "Click me",
    link = "https://example.com",
    target = "blank",
    subtext = "More info",
    sublink = "https://example.com/info",
    subtarget = "self"
  )
  out <- to_list(t)
  expect_equal(out$link, "https://example.com")
  expect_equal(out$target, "blank")
  expect_equal(out$sublink, "https://example.com/info")
  expect_equal(out$subtarget, "self")
})

test_that("Title target validates", {
  expect_error(Title(target = "invalid"))
  expect_error(Title(subtarget = "invalid"))
})

test_that("Title text_style and subtext_style nest correctly", {
  t <- Title(
    text = "Bold Title",
    text_style = TextStyle(
      color = "#333",
      font_size = 18,
      font_weight = "bold"
    ),
    subtext_style = TextStyle(
      color = "#999",
      font_size = 12
    )
  )
  out <- to_list(t)
  expect_equal(out$textStyle$color, "#333")
  expect_equal(out$textStyle$fontSize, 18)
  expect_equal(out$textStyle$fontWeight, "bold")
  expect_equal(out$subtextStyle$color, "#999")
  expect_equal(out$subtextStyle$fontSize, 12)
})

test_that("Title padding accepts scalar and vector", {
  t1 <- Title(padding = 5)
  expect_equal(t1@padding, 5)
  t2 <- Title(padding = c(5, 10, 5, 10))
  expect_equal(t2@padding, c(5, 10, 5, 10))
})

test_that("Title padding rejects invalid", {
  expect_error(Title(padding = "bad"))
  expect_error(Title(padding = c(1, 2, 3)))
})

test_that("Title border_radius accepts scalar and vector", {
  t1 <- Title(border_radius = 4)
  expect_equal(t1@border_radius, 4)
  t2 <- Title(border_radius = c(4, 4, 0, 0))
  expect_equal(t2@border_radius, c(4, 4, 0, 0))
})

test_that("Title text_align validates", {
  for (a in c("auto", "left", "center", "right")) {
    expect_equal(Title(text_align = a)@text_align, a)
  }
  expect_error(Title(text_align = "invalid"))
})

test_that("Title position fields accept string and number", {
  t <- Title(left = "center", top = 20, right = "5%", bottom = 10)
  out <- to_list(t)
  expect_equal(out$left, "center")
  expect_equal(out$top, 20)
  expect_equal(out$right, "5%")
  expect_equal(out$bottom, 10)
})

# -- Legend ---------------------------------------------------------------------

test_that("Legend creates with defaults", {
  l <- Legend()
  expect_true(S7::S7_inherits(l, Legend))
  expect_equal(to_list(l), list())
})

test_that("Legend to_list() converts names", {
  l <- Legend(
    show = TRUE,
    orient = "horizontal",
    align = "auto",
    left = "center",
    top = "top",
    item_gap = 10,
    item_width = 25,
    item_height = 14,
    trigger_event = FALSE
  )
  out <- to_list(l)
  expect_equal(out$show, TRUE)
  expect_equal(out$orient, "horizontal")
  expect_equal(out$align, "auto")
  expect_equal(out$left, "center")
  expect_equal(out$top, "top")
  expect_equal(out$itemGap, 10)
  expect_equal(out$itemWidth, 25)
  expect_equal(out$itemHeight, 14)
  expect_equal(out$triggerEvent, FALSE)
})

test_that("Legend orient validates", {
  expect_error(Legend(orient = "diagonal"))
})

test_that("Legend align validates", {
  for (a in c("auto", "left", "right")) {
    expect_equal(Legend(align = a)@align, a)
  }
  expect_error(Legend(align = "center"))
})

test_that("Legend selected_mode accepts valid values", {
  l1 <- Legend(selected_mode = TRUE)
  expect_equal(l1@selected_mode, TRUE)
  l2 <- Legend(selected_mode = "single")
  expect_equal(l2@selected_mode, "single")
  l3 <- Legend(selected_mode = "multiple")
  expect_equal(l3@selected_mode, "multiple")
})

test_that("Legend selected_mode rejects invalid", {
  expect_error(Legend(selected_mode = "all"))
})

test_that("Legend selected accepts named logical vector", {
  l <- Legend(selected = c("Series A" = TRUE, "Series B" = FALSE))
  out <- to_list(l)
  expect_equal(out$selected[["Series A"]], TRUE)
  expect_equal(out$selected[["Series B"]], FALSE)
})

test_that("Legend selected rejects unnamed vector", {
  expect_error(Legend(selected = c(TRUE, FALSE)))
})

test_that("Legend data accepts character vector", {
  l <- Legend(data = c("A", "B", "C"))
  out <- to_list(l)
  expect_equal(out$data, c("A", "B", "C"))
})

test_that("Legend text_style nests correctly", {
  l <- Legend(
    text_style = TextStyle(color = "#333", font_size = 12)
  )
  out <- to_list(l)
  expect_equal(out$textStyle$color, "#333")
  expect_equal(out$textStyle$fontSize, 12)
})

test_that("Legend item_style nests correctly", {
  l <- Legend(
    item_style = ItemStyle(opacity = 0.8, border_width = 1)
  )
  out <- to_list(l)
  expect_equal(out$itemStyle$opacity, 0.8)
  expect_equal(out$itemStyle$borderWidth, 1)
})

test_that("Legend padding and border_radius", {
  l <- Legend(padding = c(5, 10), border_radius = 4)
  expect_equal(l@padding, c(5, 10))
  expect_equal(l@border_radius, 4)
})

test_that("Legend formatter accepts string", {
  l <- Legend(formatter = "Legend {name}")
  expect_equal(l@formatter, "Legend {name}")
})

test_that("Legend inactive_color works", {
  l <- Legend(inactive_color = "#ccc", inactive_border_color = "#ddd")
  out <- to_list(l)
  expect_equal(out$inactiveColor, "#ccc")
  expect_equal(out$inactiveBorderColor, "#ddd")
})

# -- Tooltip --------------------------------------------------------------------

test_that("Tooltip creates with defaults", {
  tt <- Tooltip()
  expect_true(S7::S7_inherits(tt, Tooltip))
  expect_equal(to_list(tt), list())
})

test_that("Tooltip to_list() converts names", {
  tt <- Tooltip(
    show = TRUE,
    trigger = "axis",
    trigger_on = "mousemove",
    show_content = TRUE,
    always_show_content = FALSE,
    confine = TRUE,
    enterable = FALSE,
    show_delay = 0,
    hide_delay = 100,
    transition_duration = 0.4
  )
  out <- to_list(tt)
  expect_equal(out$show, TRUE)
  expect_equal(out$trigger, "axis")
  expect_equal(out$triggerOn, "mousemove")
  expect_equal(out$showContent, TRUE)
  expect_equal(out$alwaysShowContent, FALSE)
  expect_equal(out$confine, TRUE)
  expect_equal(out$enterable, FALSE)
  expect_equal(out$showDelay, 0)
  expect_equal(out$hideDelay, 100)
  expect_equal(out$transitionDuration, 0.4)
})

test_that("Tooltip trigger validates", {
  for (tr in c("item", "axis", "none")) {
    expect_equal(Tooltip(trigger = tr)@trigger, tr)
  }
  expect_error(Tooltip(trigger = "hover"))
})

test_that("Tooltip trigger_on validates", {
  for (to in c("mousemove", "click", "none", "mousemove|click")) {
    expect_equal(Tooltip(trigger_on = to)@trigger_on, to)
  }
  expect_error(Tooltip(trigger_on = "invalid"))
})

test_that("Tooltip styling fields", {
  tt <- Tooltip(
    background_color = "rgba(0,0,0,0.8)",
    border_color = "#333",
    border_width = 1,
    border_radius = 4,
    padding = c(5, 10)
  )
  out <- to_list(tt)
  expect_equal(out$backgroundColor, "rgba(0,0,0,0.8)")
  expect_equal(out$borderColor, "#333")
  expect_equal(out$borderWidth, 1)
  expect_equal(out$borderRadius, 4)
  expect_equal(out$padding, c(5, 10))
})

test_that("Tooltip text_style nests correctly", {
  tt <- Tooltip(
    text_style = TextStyle(color = "#fff", font_size = 14)
  )
  out <- to_list(tt)
  expect_equal(out$textStyle$color, "#fff")
  expect_equal(out$textStyle$fontSize, 14)
})

test_that("Tooltip formatter accepts string", {
  tt <- Tooltip(formatter = "{b}: {c}")
  expect_equal(tt@formatter, "{b}: {c}")
})

test_that("Tooltip formatter accepts function", {
  fn <- function(params) paste(params$name)
  tt <- Tooltip(formatter = fn)
  expect_true(is.function(tt@formatter))
})

test_that("Tooltip order validates", {
  for (o in c("seriesAsc", "seriesDesc", "valueAsc", "valueDesc")) {
    expect_equal(Tooltip(order = o)@order, o)
  }
  expect_error(Tooltip(order = "invalid"))
})

test_that("Tooltip extra_css_text and class_name", {
  tt <- Tooltip(
    extra_css_text = "box-shadow: 0 0 3px rgba(0,0,0,0.3);",
    class_name = "my-tooltip"
  )
  out <- to_list(tt)
  expect_equal(out$extraCssText, "box-shadow: 0 0 3px rgba(0,0,0,0.3);")
  expect_equal(out$className, "my-tooltip")
})

test_that("Tooltip padding rejects invalid", {
  expect_error(Tooltip(padding = "bad"))
})

# -- DataZoom -------------------------------------------------------------------

test_that("DataZoom creates with defaults (type = slider)", {
  dz <- DataZoom()
  expect_true(S7::S7_inherits(dz, DataZoom))
  # Only the default `type` should serialize; everything else is NULL.
  expect_equal(to_list(dz), list(type = "slider"))
})

test_that("DataZoom type validates", {
  for (t in c("slider", "inside")) {
    expect_equal(DataZoom(type = t)@type, t)
  }
  expect_error(DataZoom(type = "continuous"))
})

test_that("DataZoom slider fields convert to camelCase", {
  dz <- DataZoom(
    type = "slider",
    x_axis_index = 0,
    start = 10,
    end = 80,
    show = TRUE,
    background_color = "#eee",
    left = "10%",
    right = "10%",
    top = 20,
    bottom = 10,
    width = "80%",
    height = 30,
    zoom_lock = FALSE,
    filter_mode = "filter",
    throttle = 100
  )
  out <- to_list(dz)
  expect_equal(out$type, "slider")
  expect_equal(out$xAxisIndex, 0)
  expect_equal(out$start, 10)
  expect_equal(out$end, 80)
  expect_equal(out$show, TRUE)
  expect_equal(out$backgroundColor, "#eee")
  expect_equal(out$left, "10%")
  expect_equal(out$right, "10%")
  expect_equal(out$top, 20)
  expect_equal(out$bottom, 10)
  expect_equal(out$width, "80%")
  expect_equal(out$height, 30)
  expect_equal(out$zoomLock, FALSE)
  expect_equal(out$filterMode, "filter")
  expect_equal(out$throttle, 100)
})

test_that("DataZoom inside mouse-modifier fields work", {
  dz <- DataZoom(
    type = "inside",
    x_axis_index = 0,
    zoom_on_mouse_wheel = "shift",
    move_on_mouse_move = TRUE,
    move_on_mouse_wheel = FALSE,
    prevent_default_mouse_move = TRUE
  )
  out <- to_list(dz)
  expect_equal(out$type, "inside")
  expect_equal(out$zoomOnMouseWheel, "shift")
  expect_equal(out$moveOnMouseMove, TRUE)
  expect_equal(out$moveOnMouseWheel, FALSE)
  expect_equal(out$preventDefaultMouseMove, TRUE)
})

test_that("DataZoom axis index accepts number, vector, or 'all'", {
  expect_equal(DataZoom(x_axis_index = 0)@x_axis_index, 0)
  expect_equal(DataZoom(x_axis_index = c(0, 1))@x_axis_index, c(0, 1))
  expect_equal(DataZoom(y_axis_index = "all")@y_axis_index, "all")
  expect_error(DataZoom(x_axis_index = "first"))
})

test_that("DataZoom filter_mode validates", {
  for (f in c("filter", "weakFilter", "empty", "none")) {
    expect_equal(DataZoom(filter_mode = f)@filter_mode, f)
  }
  expect_error(DataZoom(filter_mode = "strict"))
})

test_that("DataZoom mouse modifier rejects invalid strings", {
  expect_error(DataZoom(zoom_on_mouse_wheel = "meta"))
})

test_that("DataZoom range_mode requires length-2 value/percent vector", {
  dz <- DataZoom(range_mode = c("percent", "value"))
  expect_equal(dz@range_mode, c("percent", "value"))
  expect_error(DataZoom(range_mode = "percent"))
  expect_error(DataZoom(range_mode = c("bad", "value")))
})

test_that("DataZoom orient validates", {
  for (o in c("horizontal", "vertical")) {
    expect_equal(DataZoom(orient = o)@orient, o)
  }
  expect_error(DataZoom(orient = "diagonal"))
})

test_that("DataZoom start/end must be single numeric", {
  expect_equal(DataZoom(start = 25)@start, 25)
  expect_error(DataZoom(start = "25%"))
  expect_error(DataZoom(end = c(10, 90)))
})
