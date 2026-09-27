test_that("protein inputs preserve annotations through the A3 contract", {
  skip_if_not_installed("rtemis.a3")
  a <- protein_data(
    strsplit("MAEPRQEFEV", "")[[1]],
    site = list(Active = c(2L, 4L)),
    region = list(Domain = c(1:3, 6:8)),
    ptm = list(Phosphorylation = 3L),
    cleavage_site = list(Cleavage = 5L),
    variant = list(list(position = 7L, ref = "E", alt = "A")),
    disease_variants = 9L
  )
  annotations <- a[["annotations"]]
  expect_equal(a[["sequence"]], "MAEPRQEFEV")
  expect_equal(
    unname(annotations[["region"]][["Domain"]][["index"]]),
    rbind(c(1L, 3L), c(6L, 8L))
  )
  expect_equal(annotations[["processing"]][["Cleavage"]][["index"]], 5L)
  expect_equal(annotations[["variant"]][[1]][["alt"]], "A")
  expect_equal(
    annotations[["site"]][["disease_associated_variant"]][["index"]],
    9L
  )
  expect_s3_class(draw_protein(a), "htmlwidget")
  expect_error(protein_data("MAEPR", site = list(Bad = 8L)))
  expect_error(protein_data("MAEPR", site = list(Bad = 1.5)))
  expect_error(protein_data(a, site = list(Active = 2L)))
})


test_that("protein JSON preserves modern records and discontinuous regions", {
  skip_if_not_installed("rtemis.a3")
  record <- list(
    sequence = "MAEPRQEFEV",
    uniprot_id = "Example",
    annotations = list(
      site = list(Active = list(index = c(2, 4), type = "active")),
      region = list(
        Domain = list(index = rbind(c(1, 3), c(6, 8)), type = "domain")
      ),
      variant = list(list(position = 7, ref = "E", alt = "A"))
    )
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  jsonlite::write_json(record, path, auto_unbox = TRUE)
  value <- protein_data(path)
  expect_equal(value[["metadata"]][["uniprot_id"]], "Example")
  expect_equal(value[["annotations"]][["variant"]][[1]][["position"]], 7)
  expect_equal(
    value[["annotations"]][["site"]][["Active"]][["index"]],
    c(2L, 4L)
  )
  expect_equal(
    value[["annotations"]][["region"]][["Domain"]][["index"]],
    record[["annotations"]][["region"]][["Domain"]][["index"]],
    ignore_attr = TRUE
  )
  unsorted <- protein_data(
    "MAEPRQEFEV",
    region = list(Domain = c(8, 1, 3, 2, 7, 6))
  )
  expect_equal(
    unsorted[["annotations"]][["region"]][["Domain"]][["index"]],
    rbind(c(1L, 3L), c(6L, 8L)),
    ignore_attr = TRUE
  )
})


test_that("typed protein annotations also respect sequence bounds", {
  skip_if_not_installed("rtemis.a3")
  expect_error(protein_data(
    "MAEPR",
    site = list(Active = rtemis.a3::annotation_position(9L))
  ))
  expect_error(protein_data(
    "MAEPR",
    region = list(Domain = rtemis.a3::annotation_range(c(1L, 9L)))
  ))
  expect_error(protein_data(
    "MAEPR",
    variant = list(rtemis.a3::annotation_variant(9L))
  ))
})
