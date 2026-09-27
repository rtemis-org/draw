# draw_protein.R
# Normalize sequence annotations into the shared rtemis.a3 producer contract.

#' Normalize legacy position annotations
#' @param x List: Named position vectors or modern annotation specifications.
#' @param size Integer: Sequence length.
#' @return List: Named rtemis.a3 position specifications.
#' @keywords internal
#' @noRd
protein_positions <- new_generic("protein_positions", "x")
method(protein_positions, class_list) <- function(x, size) {
  if (
    length(x) &&
      (is.null(names(x)) ||
        anyNA(names(x)) ||
        any(!nzchar(names(x))) ||
        anyDuplicated(names(x)))
  ) {
    abort(
      "Name every protein annotation group distinctly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  lapply(x, function(entry) {
    type <- NULL
    if (is.list(entry) && !is.null(entry[["index"]])) {
      if (S7_inherits(entry[["index"]])) {
        positions <- entry[["index"]]@data
        if (any(positions < 1 | positions > size)) {
          abort(
            "Keep annotations within the sequence.",
            class = c("rtemis_value_error", "rtemis_input_error")
          )
        }
        return(entry)
      }
      type <- entry[["type"]]
      entry <- entry[["index"]]
    }
    if (
      !is.numeric(entry) ||
        any(!is.finite(entry)) ||
        any(entry != trunc(entry)) ||
        any(entry < 1 | entry > size)
    ) {
      abort(
        "Supply integer residue positions within the sequence.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    rtemis.a3::annotation_position(as.integer(unique(entry)), type = type)
  })
}

#' Normalize sequence and annotation inputs to A3
#' @inheritParams draw_protein
#' @return A3: Validated shared protein representation.
#' @keywords internal
#' @noRd
protein_data <- new_generic("protein_data", "x")
method(protein_data, class_any) <- function(
  x,
  site = list(),
  region = list(),
  ptm = list(),
  cleavage_site = list(),
  variant = list(),
  disease_variants = NULL
) {
  if (!requireNamespace("rtemis.a3", quietly = TRUE)) {
    abort(
      "Install rtemis.a3 to draw annotated proteins.",
      class = c("rtemis_dependency_error", "rtemis_input_error")
    )
  }
  supplied <- any(
    lengths(list(site, region, ptm, cleavage_site, variant, disease_variants)) >
      0
  )
  if (S7_inherits(x)) {
    if (supplied) {
      abort(
        "Supply an A3 object alone, or a sequence with annotation arguments.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(x)
  }
  if (is.character(x) && length(x) == 1L && grepl("\\.json$", x)) {
    if (supplied) {
      abort(
        "Supply a protein file alone, or a sequence with annotations.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    x <- jsonlite::read_json(x, simplifyVector = TRUE)
  }
  metadata <- list()
  if (is.list(x)) {
    if (supplied) {
      abort(
        "Supply a protein record alone, or a sequence with annotations.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    metadata <- x[intersect(
      names(x),
      c("uniprot_id", "description", "reference", "organism")
    )]
    if (is.list(x[["metadata"]])) {
      metadata <- utils::modifyList(
        metadata,
        x[["metadata"]][intersect(
          names(x[["metadata"]]),
          c("uniprot_id", "description", "reference", "organism")
        )]
      )
    }
    annotations <- x[["annotations"]] %||% list()
    site <- annotations[["site"]] %||% list()
    region <- annotations[["region"]] %||% list()
    ptm <- annotations[["ptm"]] %||% list()
    cleavage_site <- annotations[["cleavage_site"]] %||%
      annotations[["processing"]] %||%
      list()
    variant <- annotations[["variant"]] %||% list()
    x <- x[["sequence"]]
  }
  if (
    !is.character(x) ||
      !length(x) ||
      anyNA(x) ||
      (length(x) > 1L && any(nchar(x) != 1L))
  ) {
    abort(
      "Supply an amino-acid string or a vector of single-letter residues.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  sequence <- toupper(paste0(x, collapse = ""))
  size <- nchar(sequence)
  if (!is.null(disease_variants)) {
    site[["disease_associated_variant"]] <- disease_variants
  }
  site <- protein_positions(site %||% list(), size)
  if (
    length(region) &&
      (is.null(names(region)) ||
        anyNA(names(region)) ||
        any(!nzchar(names(region))) ||
        anyDuplicated(names(region)))
  ) {
    abort(
      "Name every protein region distinctly.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  regions <- list()
  for (name in names(region)) {
    entry <- region[[name]]
    if (is.list(entry) && !is.null(entry[["index"]])) {
      if (!S7_inherits(entry[["index"]])) {
        entry <- rtemis.a3::annotation_range(
          entry[["index"]],
          type = entry[["type"]]
        )
      }
      if (any(entry[["index"]]@data < 1 | entry[["index"]]@data > size)) {
        abort(
          "Keep regions within the sequence.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      regions[[name]] <- entry
      next
    }
    # Legacy regions enumerate residues. Preserve gaps instead of spanning them.
    positions <- protein_positions(setNames(list(entry), name), size)[[1]][[
      "index"
    ]]@data
    positions <- sort(positions)
    runs <- split(positions, cumsum(c(TRUE, diff(positions) != 1L)))
    isolated <- unlist(runs[lengths(runs) == 1L], use.names = FALSE)
    if (length(isolated)) {
      site[[paste0(name, " (isolated)")]] <- rtemis.a3::annotation_position(
        isolated
      )
    }
    runs <- runs[lengths(runs) > 1L]
    if (length(runs)) {
      regions[[name]] <- rtemis.a3::annotation_range(do.call(
        rbind,
        lapply(runs, range)
      ))
    }
  }
  if (is.data.frame(variant)) {
    variant <- lapply(seq_len(nrow(variant)), function(i) {
      as.list(variant[i, , drop = FALSE])
    })
  }
  variants <- lapply(variant, function(v) {
    if (S7_inherits(v)) {
      if (v@position < 1L || v@position > size) {
        abort(
          "Keep variants within the sequence.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      return(v)
    }
    if (!is.list(v) || is.null(v[["position"]])) {
      abort(
        "Supply variant records with a residue position and optional metadata.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    check_integer_scalar(v[["position"]])
    if (v[["position"]] < 1L || v[["position"]] > size) {
      abort(
        "Keep variant positions within the sequence.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    rtemis.a3::annotation_variant(
      v[["position"]],
      v[setdiff(names(v), "position")]
    )
  })
  do.call(
    rtemis.a3::create_A3,
    c(
      list(
        sequence = sequence,
        site = site,
        region = regions,
        ptm = protein_positions(ptm %||% list(), size),
        processing = protein_positions(cleavage_site %||% list(), size),
        variant = variants
      ),
      metadata
    )
  )
}

#' Draw an Annotated Protein from Sequence and Annotation Inputs
#'
#' Adapts sequence strings, residue vectors, protein records, and local JSON to
#' the shared A3 representation used by [draw_a3()]. Named legacy annotation
#' vectors enumerate one-based residues. Region vectors preserve contiguous
#' runs; isolated region residues are drawn as sites. Modern specifications from
#' rtemis.a3 annotation_position/annotation_range/annotation_variant are accepted.
#' The common meander layout, theme, legends, and SVG path match rtemislive.
#' Fetch accession records explicitly with rtemis.a3 before plotting; a sequence
#' string never initiates a network request.
#' @param x Character, list, or A3: Sequence, protein record, local JSON path, or A3.
#' @param site,region,ptm,cleavage_site List: Named annotation vectors or modern specifications.
#' @param variant List: Records with position plus optional variant metadata.
#' @param disease_variants Optional Numeric: Disease-associated residue positions.
#' @param ... Additional display settings passed to [draw_a3()].
#' @return htmlwidget: Annotated protein diagram.
#' @export
#' @examplesIf requireNamespace("rtemis.a3", quietly = TRUE)
#' draw_protein("MAEPRQEFEVMEDHAGTYGLGDRK", site = list(Active = c(5L, 17L)),
#'   region = list(Domain = 3:10), ptm = list(Phosphorylation = 2L))
draw_protein <- function(
  x,
  site = list(),
  region = list(),
  ptm = list(),
  cleavage_site = list(),
  variant = list(),
  disease_variants = NULL,
  ...
) {
  draw_a3(
    protein_data(
      x,
      site,
      region,
      ptm,
      cleavage_site,
      variant,
      disease_variants
    ),
    ...
  )
}
