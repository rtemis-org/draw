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
  expect_error(
    protein_data(
      "MAEPR",
      variant = list(rtemis.a3::annotation_variant(9L))
    ),
    "Keep variants within the sequence",
    class = "rtemis_value_error"
  )
})
test_that("protein annotation groups require distinct nonempty names", {
  skip_if_not_installed("rtemis.a3")
  for (annotations in list(
    list(2L),
    list(a = 2L, a = 3L),
    setNames(list(2L), ""),
    setNames(list(2L), NA_character_)
  )) {
    expect_error(
      protein_data("MAEPR", site = annotations),
      "Name every protein annotation",
      class = "rtemis_value_error"
    )
    expect_error(
      protein_data("MAEPR", region = annotations),
      "Name every protein region",
      class = "rtemis_value_error"
    )
  }
})

test_that("protein records retain nested metadata and isolated region residues", {
  skip_if_not_installed("rtemis.a3")
  value <- protein_data(list(
    sequence = "maeprqefev",
    uniprot_id = "outer",
    metadata = list(uniprot_id = "nested", description = "Example protein"),
    annotations = list(region = list(Domain = c(1L, 3L, 4L, 8L)))
  ))
  expect_identical(value[["sequence"]], "MAEPRQEFEV")
  expect_identical(value[["metadata"]][["uniprot_id"]], "nested")
  expect_identical(value[["metadata"]][["description"]], "Example protein")
  expect_equal(
    value[["annotations"]][["site"]][["Domain (isolated)"]][["index"]],
    c(1L, 8L)
  )
  expect_equal(
    as.vector(value[["annotations"]][["region"]][["Domain"]][["index"]]),
    c(3L, 4L)
  )
})

test_that("protein inputs accept typed annotations and variant tables", {
  skip_if_not_installed("rtemis.a3")
  position <- rtemis.a3::annotation_position(2L)
  variant <- rtemis.a3::annotation_variant(3L)
  value <- protein_data(
    "MAEPR",
    site = list(Active = position),
    variant = list(variant)
  )
  expect_equal(value[["annotations"]][["site"]][["Active"]][["index"]], 2L)
  expect_equal(value[["annotations"]][["variant"]][[1L]][["position"]], 3L)
  expect_identical(protein_data(value), value)
  tabular <- protein_data(
    "MAEPR",
    variant = data.frame(position = c(2L, 3L), alt = c("V", "A"))
  )
  expect_equal(
    vapply(tabular[["annotations"]][["variant"]], `[[`, integer(1), "position"),
    c(2L, 3L)
  )
})

test_that("protein inputs reject conflicting sources and malformed variants", {
  skip_if_not_installed("rtemis.a3")
  expect_error(
    protein_data(list(sequence = "MAEPR"), site = list(Active = 2L)),
    "record alone"
  )
  expect_error(
    protein_data("protein.json", site = list(Active = 2L)),
    "file alone"
  )
  for (bad in list(1:3, character(), NA_character_, c("MA", "E"))) {
    expect_error(
      protein_data(bad),
      "single-letter residues",
      class = "rtemis_type_error"
    )
  }
  for (bad in list(2L, list(alt = "V"))) {
    expect_error(protein_data("MAEPR", variant = list(bad)), "variant records")
  }
  expect_error(
    protein_data("MAEPR", variant = list(list(position = 6L))),
    "within the sequence"
  )
})
