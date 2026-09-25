test_that("panel layouts are typed and serialize from their property declarations", {
  layout <- setup_PanelLayout(ncol = 2, gap = 20, padding = 8)
  expect_s7_class(layout, PanelLayout)
  expect_identical(to_list(layout), list(ncol = 2L, gap = 20, padding = 8))
  back <- do.call(
    setup_PanelLayout,
    jsonlite::fromJSON(jsonlite::toJSON(to_list(layout), auto_unbox = TRUE))
  )
  expect_equal(to_list(back), to_list(layout))
  for (args in list(
    list(ncol = 0),
    list(ncol = 1.5),
    list(ncol = NA),
    list(gap = -1),
    list(gap = Inf),
    list(padding = "1")
  )) {
    expect_error(do.call(setup_PanelLayout, args))
  }
  schema <- panel_layout_schema()
  expect_equal(schema[["properties"]][["ncol"]][["minimum"]], 1)
  expect_equal(schema[["properties"]][["gap"]][["minimum"]], 0)
  expect_null(schema[["required"]])
  expect_equal(
    panel_layout_schema(TRUE)[["required"]],
    as.list(names(to_list(layout)))
  )
  expect_false(any(vapply(
    schema[["properties"]],
    function(p) "default" %in% names(p),
    logical(1)
  )))
  expect_error(panel_layout_schema(NA))
})

test_that("composition preserves independent payloads and uses the existing widget binding", {
  a <- draw_bar(c("a", "b"), c(2, 3), theme = theme_light())
  b <- draw_boxplot(1:4, boxpoints = "all", theme = theme_dark())
  w <- draw_panels(list(a, b), ncol = 2, gap = 20, padding = 8)
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "rtemis-panels")
  expect_identical(class(w)[[1]], "rtemis-draw")
  expect_identical(w[["x"]][["panels"]], list(a[["x"]], b[["x"]]))
  expect_equal(w[["height"]], 376)
  expect_identical(w[["x"]][["layout"]], list(ncol = 2L, gap = 20, padding = 8))
  expect_identical(
    draw_panels(list(a, b), layout = setup_PanelLayout(2, 20, 8))[["x"]],
    w[["x"]]
  )
  expect_error(draw_panels(list()), "nonempty")
  expect_error(draw_panels(a), "nonempty")
  expect_error(draw_panels(list(1)), "ECharts")
  expect_error(draw_panels(list(w)), "nested")
  expect_error(draw_panels(list(a), layout = list()), "setup_PanelLayout")
  expect_error(
    draw_panels(list(a), ncol = 2, layout = setup_PanelLayout()),
    "either"
  )
  expect_error(draw_panels(list(a), width = 1, padding = 2), "dimensions")
  expect_error(draw_panels(list(a), height = Inf), "dimensions")
  expect_error(draw_panels(list(a), height = 1i), "dimensions")
  for (bad in list(TRUE, list(900), NA_character_, "", c(900, 800), 1i)) {
    expect_error(draw_panels(list(a), width = bad), "width")
  }
  expect_s3_class(draw_panels(list(a), width = "100%"), "htmlwidget")
  expect_error(
    draw_panels(list(htmlwidgets::onRender(a, "function(){}"))),
    "hooks"
  )
  heat <- draw_heatmap(matrix(1:4, 2), square_cells = TRUE)
  expect_identical(
    draw_panels(list(heat))[["x"]][["panels"]][[1L]],
    heat[["x"]]
  )
})

test_that("complete SVG exports retain panel marks, labels and styles", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  a <- draw_boxplot(
    1:4,
    boxpoints = "all",
    title = "First panel",
    palette = "#123456"
  )
  b <- draw_boxplot(
    2:5,
    boxpoints = "all",
    title = "Second panel",
    palette = "#abcdef"
  )
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  w <- draw_panels(
    list(a, b),
    ncol = 2,
    width = 900,
    height = 400,
    filename = path
  )
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(svg, 'width="900" height="400"')
  expect_match(svg, 'x="0" y="0"')
  expect_match(svg, 'x="456" y="0"')
  expect_equal(
    lengths(regmatches(svg, gregexpr('<circle ', svg, fixed = TRUE))),
    8
  )
  for (label in c("First panel", "Second panel", "#123456", "#abcdef")) {
    expect_match(svg, label, fixed = TRUE)
  }
  expect_false(grepl('<image', svg, fixed = TRUE))
  # Failed child rendering is transactional for the complete destination.
  original <- readBin(path, "raw", n = file.info(path)[["size"]])
  w[["x"]][["panels"]][[2]][["option"]][["series"]][[1]][["type"]] <- "custom"
  expect_error(save_drawing(w, path), "Unsupported custom")
  expect_identical(
    readBin(path, "raw", n = file.info(path)[["size"]]),
    original
  )
  a[["x"]][["option"]][["title"]][["text"]] <- htmlwidgets::JS("function(){}")
  expect_error(save_drawing(draw_panels(list(a)), path), "panels\\[1\\]")
})

test_that("panel geometry and fitted aspect use the same module in both renderers", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  code <- paste0(
    "const assert=require('node:assert/strict'); const m=require(",
    jsonlite::toJSON(
      system.file("htmlwidgets/lib/draw/panels.js", package = "rtemis.draw"),
      auto_unbox = TRUE
    ),
    "); const c=m.cells(3,{ncol:2,gap:20,padding:10},820,620);",
    "assert.deepEqual(c,[{x:10,y:10,width:390,height:290},{x:420,y:10,width:390,height:290},{x:10,y:320,width:390,height:290}]);",
    "const p={option:{grid:{}},aspect:{ratio:2,leftPx:20,rightPx:20,topPx:10,botPx:10}};m.fit(p,390,290);",
    "assert.equal(p.option.grid.width,135);assert.equal(p.option.grid.height,270);",
    "assert.throws(()=>m.cells(1,{ncol:0,gap:0,padding:0},10,10));",
    "assert.throws(()=>m.fit(p,10,10));console.log('passed');"
  )
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script), add = TRUE)
  writeLines(code, script)
  out <- system2(
    Sys.which("node"),
    shQuote(script),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  expect_match(paste(out, collapse = "\n"), "passed")
})

test_that("browser panel sizing uses the measured host instead of the viewport", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  out <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "panel_geometry.js")),
      shQuote(system.file("htmlwidgets/lib/draw", package = "rtemis.draw"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  expect_match(paste(out, collapse = "\n"), "Host geometry passed")
})
