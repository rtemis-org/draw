test_that("pie label options inherit validated common labels and serialize", {
  label <- PieLabelOption(
    align_to = "edge",
    edge_distance = 8,
    text_style = TextStyle(font_size = 13)
  )
  expect_true(S7::S7_inherits(label, LabelOption))
  value <- to_list(label)
  expect_equal(value[["alignTo"]], "edge")
  expect_equal(value[["edgeDistance"]], 8)
  expect_equal(value[["fontSize"]], 13)
  expect_false("bleedMargin" %in% names(value))
  expect_error(PieLabelOption(align_to = "middle"))
  for (invalid in list(TRUE, FALSE, list(8), c(1, 2), character())) {
    expect_error(PieLabelOption(edge_distance = invalid))
  }
  expect_equal(
    to_list(PieLabelOption(edge_distance = "10%"))[["edgeDistance"]],
    "10%"
  )
  expect_equal(to_list(PieSeries(label = label))[["label"]], value)
  widget <- draw_pie(c(3, 5, 7), c("Adelie", "Chinstrap", "Gentoo"))
  expect_equal(
    widget[["x"]][["option"]][["series"]][[1L]][["label"]][["alignTo"]],
    "edge"
  )
})

test_that("outside pie labels retain their complete text at narrow widths", {
  skip_if_not(nzchar(Sys.which("node")), "node not found")
  output <- system2(
    Sys.which("node"),
    c(
      shQuote(test_path("fixtures", "pie_labels.js")),
      shQuote(system.file(package = "rtemis.draw"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "Pie labels fit both widths")
})
