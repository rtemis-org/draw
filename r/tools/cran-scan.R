#!/usr/bin/env Rscript
# cran-scan.R
# ::cran-first-submission::
# 2026- EDG rtemis.org
#
# Vendored into the repo so `just cran-scan` works for any contributor without
# an external checkout. Source of truth: the cran-first-submission skill,
# ~/Skills/developer/cran-first-submission/scripts/cran_scan.R. Change it
# there and copy it here.
#
# Static scan of an R source package for the issues CRAN's manual review of a
# new submission raises and `R CMD check --as-cran` does not (or only partly)
# catch. Every finding is a lead to review against references/checklist.md,
# not a verdict: the scanner reads code, it does not understand it.
#
# Usage:
#   Rscript cran-scan.R <pkg_dir> [--tarball=FILE] [--offline] [--ignore=FILE] [--fail]
#
#   <pkg_dir>   directory holding DESCRIPTION
#   --tarball   also inspect the built tarball (size, contents)
#   --offline   skip the CRAN/Bioconductor name and dependency lookups
#   --ignore    file of reviewed findings to suppress, one per line:
#                 <ID> <where-glob>  # <reason>
#               e.g. `CODE-SYSTEM R/export.R:*  # node found via Sys.which()`.
#               The reason is required. Unused entries are reported, so the
#               file cannot silently outlive the code it excuses.
#   --fail      exit with status 1 if any non-advisory finding remains
#
# Output: Markdown on stdout, grouped by checklist ID; advisory findings last.

# Arguments ----
args <- commandArgs(trailingOnly = TRUE)
flags <- grep("^--", args, value = TRUE)
pos <- setdiff(args, flags)
if (length(pos) != 1L) {
  stop("usage: Rscript cran-scan.R <pkg_dir> [--tarball=FILE] [--offline] [--ignore=FILE] [--fail]")
}
pkg <- normalizePath(pos, mustWork = TRUE)
offline <- "--offline" %in% flags
tarball <- sub("^--tarball=", "", grep("^--tarball=", flags, value = TRUE))
ignore_file <- sub("^--ignore=", "", grep("^--ignore=", flags, value = TRUE))
fail <- "--fail" %in% flags
if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
  stop("no DESCRIPTION in ", pkg)
}

# Findings ----
.findings <- new.env()
.findings$rows <- list()

#' Record one finding.
#' @param id Checklist ID (see references/checklist.md).
#' @param where File, file:line, or DESCRIPTION field.
#' @param msg What to review.
#' @param advisory If TRUE, report but never fail on it.
add <- function(id, where, msg, advisory = FALSE) {
  .findings$rows[[length(.findings$rows) + 1L]] <-
    c(id = id, where = where, msg = msg, advisory = as.character(advisory))
  invisible(NULL)
}

rel <- function(f) sub(paste0("^", pkg, "/?"), "", f)
oneline <- function(x, n = 100L) {
  x <- gsub("[[:space:]]+", " ", paste(x, collapse = " "))
  if (nchar(x) > n) paste0(substr(x, 1L, n - 3L), "...") else x
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

# DESCRIPTION ----
desc <- read.dcf(file.path(pkg, "DESCRIPTION"), keep.white = c("Description", "Authors@R"))[1L, ]
field <- function(f) if (f %in% names(desc)) trimws(desc[[f]]) else NA_character_
pkg_name <- field("Package")
title <- gsub("\\s+", " ", field("Title"))
descr <- gsub("\\s+", " ", field("Description"))

parse_deps <- function(f) {
  x <- field(f)
  if (is.na(x)) return(character())
  x <- trimws(sub("\\(.*\\)", "", strsplit(x, ",")[[1L]]))
  x[nzchar(x)]
}
deps <- list(
  Depends = setdiff(parse_deps("Depends"), "R"),
  Imports = parse_deps("Imports"),
  LinkingTo = parse_deps("LinkingTo"),
  Suggests = parse_deps("Suggests"),
  Enhances = parse_deps("Enhances")
)
all_deps <- unique(unlist(deps))

## DESC-TITLE ----
if (!is.na(title)) {
  if (nchar(title) > 65L) {
    add("DESC-TITLE", "Title", sprintf("%d characters; listings truncate at 65", nchar(title)))
  }
  if (grepl("\\.$", title) && !grepl("\\.\\.\\.$|etc\\.$", title)) {
    add("DESC-TITLE", "Title", "ends in a period")
  }
  tc <- tools::toTitleCase(title)
  if (!identical(tc, title)) {
    add("DESC-TITLE", "Title", sprintf("not title case; toTitleCase() gives: %s", tc))
  }
  if (grepl(paste0("\\b", gsub(".", "\\.", pkg_name, fixed = TRUE), "\\b"), title, ignore.case = TRUE)) {
    add("DESC-TITLE", "Title", "repeats the package name")
  }
}

## DESC-DESCRIPTION ----
if (!is.na(descr)) {
  bad_start <- c(
    "^(The|This|A|In this|In the) package",
    paste0("^'?", gsub(".", "\\.", pkg_name, fixed = TRUE), "'?\\b")
  )
  if (any(vapply(bad_start, grepl, NA, x = descr, ignore.case = TRUE)) ||
      (!is.na(title) && startsWith(tolower(descr), tolower(title)))) {
    add("DESC-DESCRIPTION", "Description",
        "starts with the package name, 'This package', or the title")
  }
  n_sent <- length(gregexpr("[.!?](\\s|$)", descr)[[1L]])
  if (n_sent < 2L) {
    add("DESC-DESCRIPTION", "Description",
        "fewer than two sentences; CRAN asks for a full paragraph on what the package does and why")
  }
}

## DESC-QUOTES: software and package names in single quotes ----
software <- c(
  "Python", "Java", "JavaScript", "TypeScript", "C\\+\\+", "Rust", "Julia", "Stan",
  "JAGS", "TensorFlow", "PyTorch", "Keras", "Quarto", "RStudio", "Excel", "GitHub",
  "Node\\.js", "D3", "Plotly", "LaTeX", "SQL", "SQLite", "DuckDB", "Arrow", "Spark",
  "ECharts", "Leaflet", "Shiny", "Markdown", "HTML", "CSS", "JSON", "WebGL"
)
check_quotes <- function(text, fld) {
  if (is.na(text)) return(invisible())
  names_re <- c(gsub(".", "\\.", setdiff(all_deps, c("stats", "utils", "tools", "methods",
                                                      "graphics", "grDevices")), fixed = TRUE),
                software)
  for (re in names_re) {
    hit <- gregexpr(paste0("(?<![A-Za-z0-9.'])", re, "(?![A-Za-z0-9'])"), text, perl = TRUE)[[1L]]
    if (hit[1L] > 0L) {
      add("DESC-QUOTES", fld, sprintf("'%s' appears without single quotes",
                                      regmatches(text, list(hit))[[1L]][1L]))
    }
  }
  fq <- regmatches(text, gregexpr("'[A-Za-z0-9._]+\\(\\)'", text))[[1L]]
  for (x in fq) add("DESC-QUOTES", fld, sprintf("function %s: write foo() without quotes", x))
}
check_quotes(title, "Title")
check_quotes(descr, "Description")

## DESC-REFS: URLs, DOIs, arXiv ----
if (!is.na(descr)) {
  bare <- regmatches(descr, gregexpr("(?<!<)https?://[^ >,;)]+", descr, perl = TRUE))[[1L]]
  for (u in bare) add("DESC-REFS", "Description", sprintf("URL not in angle brackets: %s", u))
  if (grepl("doi\\.org/|\\bdoi:\\s|\\bDOI:", descr)) {
    add("DESC-REFS", "Description", "DOI not written as <doi:10.prefix/suffix>")
  }
  if (grepl("arXiv:\\s*[0-9]", descr, ignore.case = TRUE)) {
    add("DESC-REFS", "Description", "arXiv id: write as <doi:10.48550/arXiv.ID>")
  }
  if (grepl("<(doi|https?):\\s", descr)) {
    add("DESC-REFS", "Description", "space after 'doi:'/'https:' inside angle brackets breaks the link")
  }
}

## DESC-ACRONYMS ----
if (!is.na(descr)) {
  unquoted <- gsub("'[^']*'", "", descr)
  unquoted <- gsub("<[^>]*>", "", unquoted)
  acr <- unique(regmatches(unquoted, gregexpr("\\b[A-Z][A-Z0-9]{1,}s?\\b", unquoted))[[1L]])
  acr <- setdiff(acr, c("R", "I", "II", "III", "IV"))
  explained <- vapply(acr, function(a) grepl(paste0("\\(", a, "\\)"), descr), NA)
  if (length(acr[!explained])) {
    add("DESC-ACRONYMS", "Description",
        paste("explain or quote:", paste(acr[!explained], collapse = ", ")))
  }
}

## DESC-AUTHORS ----
authors <- NULL
if (is.na(field("Authors@R"))) {
  add("DESC-AUTHORS", "Authors@R", "missing; CRAN prefers Authors@R with roles")
} else {
  authors <- tryCatch(eval(parse(text = field("Authors@R"))), error = function(e) {
    add("DESC-AUTHORS", "Authors@R", paste("does not parse:", conditionMessage(e)))
    NULL
  })
}
if (!is.null(authors)) {
  roles <- lapply(authors, function(p) p$role %||% character())
  cre <- which(vapply(roles, function(r) "cre" %in% r, NA))
  if (length(cre) != 1L) {
    add("DESC-AUTHORS", "Authors@R", sprintf("%d persons with role 'cre'; need exactly one", length(cre)))
  } else if (is.null(authors[[cre]]$email)) {
    add("DESC-AUTHORS", "Authors@R", "maintainer ('cre') has no email")
  }
  if (!any(vapply(roles, function(r) "aut" %in% r, NA))) {
    add("DESC-AUTHORS", "Authors@R", "no person with role 'aut'")
  }
  for (p in authors) {
    orcid <- p$comment[["ORCID"]] %||% NA
    if (!is.na(orcid) && !grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", orcid)) {
      add("DESC-AUTHORS", "Authors@R", sprintf("ORCID for %s is not a bare id: %s", format(p, include = c("given", "family")), orcid))
    }
  }
  if (!is.na(field("Author")) || !is.na(field("Maintainer"))) {
    add("DESC-AUTHORS", "Author/Maintainer",
        "hand-written alongside Authors@R; they must match what Authors@R generates, or drop them")
  }
}

## DESC-LICENSE ----
lic <- field("License")
if (!is.na(lic)) {
  al <- tools:::analyze_license(lic)
  if (!isTRUE(al$is_verified)) {
    add("DESC-LICENSE", "License", "not a verified license from share/licenses/license.db")
  }
  db <- read.dcf(file.path(R.home("share"), "licenses", "license.db"))
  templates <- db[grepl("template", db[, "Note"]), "Abbrev"]
  base_lic <- trimws(sub("\\+.*$", "", lic))
  has_file <- grepl("file LICEN[CS]E", lic)
  lic_file <- list.files(pkg, "^LICEN[CS]E$", full.names = TRUE)
  if (has_file && !length(lic_file)) {
    add("DESC-LICENSE", "License", "refers to file LICENSE, which does not exist")
  }
  if (has_file && !(base_lic %in% templates) && !grepl("^file", base_lic)) {
    add("DESC-LICENSE", "License",
        sprintf("'%s' is part of R; '+ file LICENSE' is only for added restrictions or attribution", base_lic))
  }
  if (base_lic %in% templates && !has_file) {
    add("DESC-LICENSE", "License", sprintf("%s is a template and needs '+ file LICENSE'", base_lic))
  }
  if (base_lic %in% templates && length(lic_file)) {
    ll <- trimws(readLines(lic_file[1L], warn = FALSE))
    ll <- ll[nzchar(ll)]
    need <- if (base_lic == "MIT") c("YEAR", "COPYRIGHT HOLDER") else c("YEAR", "COPYRIGHT HOLDER", "ORGANIZATION")
    keys <- sub(":.*$", "", ll)
    if (!setequal(keys, need)) {
      add("DESC-LICENSE", rel(lic_file[1L]),
          sprintf("should contain only the %s template fields (%s); full license text belongs in a build-ignored LICENSE.md",
                  base_lic, paste(need, collapse = ", ")))
    }
    for (l in ll) add("DESC-LICENSE-INFO", rel(lic_file[1L]), sprintf("confirm value: %s", l), advisory = TRUE)
  }
}

## DESC-VERSION / DESC-DATE ----
ver <- field("Version")
if (!is.na(ver)) {
  comps <- as.integer(strsplit(ver, "[.-]")[[1L]])
  if (any(comps >= 1000L, na.rm = TRUE)) {
    add("DESC-VERSION", "Version", sprintf("%s has a development component; release versions only", ver))
  }
  if (grepl("(^|[.-])0[0-9]", ver)) {
    add("DESC-VERSION", "Version", sprintf("%s has a component with a leading zero", ver))
  }
}
dt <- field("Date")
if (!is.na(dt)) {
  d <- as.Date(dt, optional = TRUE)
  if (is.na(d)) {
    add("DESC-DATE", "Date", "not yyyy-mm-dd")
  } else if (Sys.Date() - d > 30) {
    add("DESC-DATE", "Date", sprintf("%s is over a month old (incoming check NOTE); update or remove it", dt))
  }
}

## DESC-FIELDS ----
if (!is.na(field("Remotes"))) {
  add("DESC-FIELDS", "Remotes", "non-CRAN field; a CRAN release cannot depend on it -- remove before building")
}
for (f in c("URL", "BugReports")) {
  v <- field(f)
  if (!is.na(v) && grepl("http://", v, fixed = TRUE)) {
    add("DESC-FIELDS", f, "uses http://; prefer https://")
  }
}

# Suggest URL and BugReports from the GitHub remote (git searches upward, so a
# package in a subdirectory of the repository works).
gh_repo <- local({
  r <- tryCatch(
    suppressWarnings(system2("git", c("-C", shQuote(pkg), "remote", "get-url", "origin"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (!length(r) || !nzchar(r[1L])) return(NA_character_)
  m <- regmatches(r[1L], regexec("github\\.com[:/]([^/]+)/([^/]+?)(\\.git)?/?$", r[1L]))[[1L]]
  if (length(m) < 3L) NA_character_ else sprintf("https://github.com/%s/%s", m[2L], m[3L])
})
if (!is.na(gh_repo)) {
  gh_issues <- paste0(gh_repo, "/issues")
  # CRAN's URL check fetches these; a private repository answers 404.
  gh_public <- if (offline) NA else tryCatch(
    any(grepl("^HTTP/[0-9.]+ 200", curlGetHeaders(gh_repo))),
    error = function(e) FALSE
  )
  if (isFALSE(gh_public)) {
    add("DESC-FIELDS", "URL/BugReports", sprintf(
      "%s is not publicly reachable: do not list it until it is, or CRAN's URL check fails", gh_repo))
  }
  urls <- trimws(strsplit(field("URL") %||% "", "[,[:space:]]+")[[1L]])
  bug <- field("BugReports")
  if (isFALSE(gh_public)) {
    # Suggesting an unreachable repository would contradict the finding above.
  } else if (is.na(bug)) {
    add("DESC-FIELDS", "BugReports", sprintf("missing; add `BugReports: %s`", gh_issues), advisory = TRUE)
  } else if (!identical(sub("/$", "", bug), gh_issues)) {
    add("DESC-FIELDS", "BugReports", sprintf("is %s but the git remote gives %s; confirm which is current", bug, gh_issues), advisory = TRUE)
  }
  if (!isFALSE(gh_public) && !sub("/$", "", gh_repo) %in% sub("/$", "", urls)) {
    add("DESC-FIELDS", "URL", sprintf("add the source repository: `URL: %s`",
                                       paste(c(urls[nzchar(urls)], gh_repo), collapse = ", ")), advisory = TRUE)
  }
} else {
  if (is.na(field("URL"))) add("DESC-FIELDS", "URL", "missing (optional; users and reviewers expect a homepage or repository)", advisory = TRUE)
  if (is.na(field("BugReports"))) add("DESC-FIELDS", "BugReports", "missing (optional; where users report bugs)", advisory = TRUE)
}

## DESC-RVERSION: syntax and functions newer than Depends: R ----
r_dep <- regmatches(field("Depends") %||% "", regexpr("R \\(>= *[0-9.]+\\)", field("Depends") %||% ""))
r_min <- if (length(r_dep)) package_version(gsub("[^0-9.]", "", r_dep)) else package_version("0.0")
.rneeds <- new.env()
.rneeds$hits <- list()
need_r <- function(v, what, where) {
  if (r_min < package_version(v)) {
    .rneeds$hits[[length(.rneeds$hits) + 1L]] <- c(v = v, what = what, where = where)
  }
}

# Code scanning ----
std_pkgs <- unlist(tools:::.get_standard_package_names())
base_pkgs <- tools:::.get_standard_package_names()$base
path_arg_re <- "(^|[._])(file|filename|filepath|path|dir|directory|folder|outdir|output|out|dest|destfile|outfile)([._]|$)"
core_arg_re <- "^(cores|n_cores|ncores|n\\.cores|mc\\.cores|workers|n_workers|n_jobs|nthread|nthreads|n_threads|num_threads|num\\.threads|threads)$"
reset_re <- "on\\.exit\\(|withr::(local|with)_|\\blocal_(options|par|envvar|dir|locale)\\(|\\bwith_(options|par|envvar|dir|locale)\\("
print_ok_re <- "^(print|format|summary|show|str|toString|knit_print|repr)([.(]|$)|method\\((print|format|summary|show|str)\\b"

#' Name a top-level expression for reporting.
top_name <- function(e) {
  if (is.call(e) && as.character(e[[1L]])[1L] %in% c("<-", "=", "<<-")) {
    oneline(deparse(e[[2L]]), 60L)
  } else if (is.call(e)) {
    paste0(oneline(deparse(e[[1L]]), 40L), "(...)")
  } else {
    ""
  }
}

#' Return the function definition in a top-level assignment, or NULL.
top_fun <- function(e) {
  if (is.call(e) && as.character(e[[1L]])[1L] %in% c("<-", "=") &&
      is.call(e[[3L]]) && identical(e[[3L]][[1L]], as.name("function"))) {
    e[[3L]]
  } else {
    NULL
  }
}

#' Scan parsed R code for CRAN hazards.
#'
#' @param exprs Result of parse(keep.source = TRUE).
#' @param label File label for reporting.
#' @param context One of "R", "examples", "tests", "vignettes".
scan_exprs <- function(exprs, label, context) {
  pd <- utils::getParseData(exprs, includeText = TRUE)
  if (is.null(pd) || !nrow(pd)) return(invisible())
  pd <- pd[order(pd$line1, pd$col1, -pd$line2), ]
  term <- pd[pd$terminal, ]
  prev_tok <- c("", head(term$token, -1L))
  srefs <- attr(exprs, "srcref")
  tops <- data.frame(
    line1 = vapply(srefs, function(s) s[[1L]], 1L),
    line2 = vapply(srefs, function(s) s[[3L]], 1L),
    name = vapply(as.list(exprs), top_name, ""),
    text = vapply(srefs, function(s) paste(as.character(s), collapse = "\n"), "")
  )
  top_of <- function(line) {
    i <- which(tops$line1 <= line & tops$line2 >= line)
    if (length(i)) i[1L] else NA_integer_
  }
  call_text <- function(tok_id) {
    p1 <- pd$parent[pd$id == tok_id]
    p2 <- pd$parent[pd$id == p1]
    pd$text[pd$id == p2] %||% ""
  }
  where <- function(line) sprintf("%s:%d", label, line)
  in_fun <- function(line) {
    i <- top_of(line)
    if (is.na(i) || !nzchar(tops$name[i])) "" else sprintf(" in `%s`", tops$name[i])
  }
  has_reset <- function(line) {
    i <- top_of(line)
    !is.na(i) && grepl(reset_re, tops$text[i])
  }
  file_text <- paste(tops$text, collapse = "\n")
  top_has <- function(line, re) {
    i <- top_of(line)
    !is.na(i) && grepl(re, tops$text[i])
  }
  print_hits <- list()
  other_internal <- character()

  for (k in seq_len(nrow(term))) {
    tok <- term$token[k]
    txt <- term$text[k]
    ln <- term$line1[k]
    id <- term$id[k]
    pv <- prev_tok[k]

    # CODE-TF
    if (tok == "SYMBOL" && txt %in% c("T", "F") && !pv %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT")) {
      add("CODE-TF", where(ln), sprintf("`%s`: write TRUE/FALSE, and do not use T/F as names", txt))
    }

    # CODE-GLOBALENV
    if (tok %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN") && txt %in% c("<<-", "->>") && context %in% c("R", "examples")) {
      add("CODE-GLOBALENV", where(ln), sprintf("`%s`%s: target must be bound in an enclosing function, never the workspace", txt, in_fun(ln)))
    }
    if (tok == "SYMBOL" && txt %in% c(".GlobalEnv", ".Random.seed") && context == "R") {
      add("CODE-GLOBALENV", where(ln), sprintf("`%s`%s", txt, in_fun(ln)))
    }

    # CODE-INTERNAL
    # Own-package ::: is the accepted way to reach internals from tests and
    # examples; another package's internals from tests is fragile but not
    # flagged by check, so those are summarized per file below.
    if (tok == "NS_GET_INT") {
      p <- term$text[k - 1L]
      obj <- paste0(p, ":::", term$text[k + 1L])
      if (p %in% base_pkgs) {
        add("CODE-INTERNAL", where(ln), sprintf("`%s`: base-package internals are not allowed", obj))
      } else if (identical(p, pkg_name)) {
        if (context == "R") add("CODE-INTERNAL", where(ln), sprintf("`%s`: own namespace needs no :::", obj))
      } else if (context == "R") {
        add("CODE-INTERNAL", where(ln), sprintf("`%s`: ::: to another package draws a check NOTE; ask its maintainer to export it", obj))
      } else {
        other_internal <- c(other_internal, obj)
      }
    }

    # DESC-RVERSION
    if (tok == "PIPE") need_r("4.1.0", "native pipe |>", where(ln))
    if (tok == "'\\\\'") need_r("4.1.0", "lambda \\(x)", where(ln))
    if (tok == "PLACEHOLDER") need_r("4.2.0", "pipe placeholder _", where(ln))

    # CODE-HOME: hard-coded home or absolute paths
    if (tok == "STR_CONST" && grepl("^[\"'](~|/Users/|/home/|[A-Za-z]:[\\\\/])", txt)) {
      add("CODE-HOME", where(ln), sprintf("path literal %s%s", oneline(txt, 60L), in_fun(ln)))
    }
    if (tok == "STR_CONST" && grepl("http://", txt, fixed = TRUE) && context == "R") {
      add("CODE-NET", where(ln), sprintf("http:// URL %s: downloads must use https", oneline(txt, 60L)))
    }

    if (tok != "SYMBOL_FUNCTION_CALL") next
    ct <- call_text(id)

    # CODE-SEED
    if (txt %in% c("set.seed", "RNGkind") && context == "R") {
      i <- top_of(ln)
      guarded <- !is.na(i) && grepl("if\\s*\\(\\s*!\\s*is\\.null\\(\\s*seed", tops$text[i])
      if (!guarded || txt == "RNGkind") {
        add("CODE-SEED", where(ln), sprintf("`%s`%s: only set when the user passes a seed", oneline(ct, 50L), in_fun(ln)))
      }
    }

    # CODE-PRINT
    if (context == "R" && txt %in% c("print", "cat", "writeLines", "print.default")) {
      if (txt == "cat" && grepl("file\\s*=", ct)) next
      if (txt == "writeLines" && grepl(",", ct)) next
      i <- top_of(ln)
      nm <- if (is.na(i)) "" else tops$name[i]
      if (!grepl(print_ok_re, nm)) {
        key <- if (nzchar(nm)) nm else label
        print_hits[[key]] <- c(print_hits[[key]], ln)
      }
    }

    # CODE-RESET and CODE-WARN
    if (txt == "options" && grepl("warn\\s*=\\s*-", ct)) {
      add("CODE-WARN", where(ln), "options(warn = -1): use suppressWarnings() on the expression")
    }
    setter <- (txt %in% c("options", "par") && grepl("=", ct)) ||
      txt %in% c("setwd", "Sys.setenv", "Sys.setlocale", "Sys.setLanguage", "Sys.umask")
    if (setter) {
      if (context == "R" && !has_reset(ln)) {
        add("CODE-RESET", where(ln), sprintf("`%s`%s with no on.exit()/withr reset", oneline(ct, 50L), in_fun(ln)))
      }
      if (context %in% c("examples", "vignettes") &&
          !grepl(paste0("<-\\s*", txt, "\\("), file_text)) {
        add("CODE-RESET", where(ln), sprintf("`%s` in %s: save the old value and restore it afterwards", oneline(ct, 50L), context))
      }
      if (context == "tests" && txt == "setwd") {
        add("CODE-RESET", where(ln), "setwd() in tests: use withr::local_dir()")
      }
    }

    # CODE-INSTALLED / CODE-INSTALL
    if (txt == "installed.packages") {
      add("CODE-INSTALLED", where(ln), "installed.packages() is slow: use requireNamespace() or system.file()")
    }
    if (txt %in% c("install.packages", "install_github", "install_version", "install_cran", "pkg_install") ||
        (txt == "install" && grepl("BiocManager", ct))) {
      add("CODE-INSTALL", where(ln), sprintf("`%s`%s: never install software in functions, examples, tests or vignettes", txt, in_fun(ln)))
    }

    # CODE-QUIT
    if (txt %in% c("q", "quit") && context %in% c("R", "examples")) {
      add("CODE-QUIT", where(ln), "q()/quit() terminates the R process")
    }
    if (txt == ".Internal") {
      add("CODE-INTERNAL", where(ln), ".Internal(): not public API")
    }

    # CODE-CORES
    if (txt %in% c("detectCores", "availableCores", "makeCluster", "makePSOCKcluster",
                   "makeForkCluster", "mclapply", "mcmapply", "registerDoParallel",
                   "setDTthreads", "setThreadOptions", "plan", "set_num_threads")) {
      add("CODE-CORES", where(ln), sprintf("`%s`%s: at most 2 cores in examples, tests, vignettes; default <= 2", oneline(ct, 60L), in_fun(ln)))
    }

    # CODE-BROWSER
    if (txt %in% c("browseURL", "viewer", "shell.exec", "file.show", "vignette", "View")) {
      i <- top_of(ln)
      guarded <- !is.na(i) && grepl("interactive\\(\\)", tops$text[i])
      if (context != "R" || !guarded) {
        add("CODE-BROWSER", where(ln), sprintf("`%s`%s: do not start external software in examples/tests; guard with interactive()", txt, in_fun(ln)))
      }
    }

    # CODE-SYSTEM
    if (txt %in% c("system", "system2", "run") && (txt != "run" || grepl("processx", ct)) &&
        !(context != "R" && top_has(ln, "skip_if|Sys\\.which|nzchar\\(|if \\(interactive"))) {
      add("CODE-SYSTEM", where(ln), sprintf("`%s`%s: external software must be in SystemRequirements; examples/tests must skip when absent", txt, in_fun(ln)))
    }

    # CODE-NET
    if (txt %in% c("download.file", "url", "curl_download", "curl_fetch_memory", "GET", "POST",
                   "req_perform", "fromJSON") && grepl("https?://|url", ct)) {
      if (context == "R" && txt != "fromJSON") {
        add("CODE-NET", where(ln), sprintf("`%s`%s: must fail gracefully with an informative message when offline", txt, in_fun(ln)))
      } else if (context %in% c("examples", "tests", "vignettes")) {
        add("CODE-NET", where(ln), sprintf("`%s` in %s: needs \\donttest / skip_if_offline()", txt, context))
      }
    }

    # CODE-WRITE: writes outside tempdir() in examples, tests, vignettes
    if (context != "R" &&
        txt %in% c("writeLines", "saveRDS", "save", "write.csv", "write.table", "write",
                   "file.create", "dir.create", "sink", "saveWidget", "save_html", "png",
                   "pdf", "svg", "jpeg", "ggsave", "fwrite", "write_json", "writeBin",
                   "file.copy", "download.file", "export", "draw_save", "save_svg") &&
        !top_has(ln, "tempfile|tempdir|local_temp|withr::|\\btmp_|\\btemp_")) {
      add("CODE-WRITE", where(ln), sprintf("`%s` in %s: write only under tempdir() and clean up", oneline(ct, 60L), context))
    }
    if (txt == "getwd" && context == "R") {
      add("CODE-HOME", where(ln), sprintf("getwd()%s: never write to the working directory by default", in_fun(ln)))
    }
    if (txt == "R_user_dir") need_r("4.0.0", "tools::R_user_dir()", where(ln))

    # CODE-RMLS
    if (txt == "rm" && grepl("ls\\(", ct) && context != "R") {
      add("CODE-GLOBALENV", where(ln), sprintf("rm(list = ls()) in %s", context))
    }
  }

  if (length(other_internal)) {
    tab <- table(other_internal)
    add("CODE-INTERNAL", label, sprintf("%s in %s: another package's internals; breaks when it changes",
                                        paste(sprintf("`%s` x%d", names(tab), tab), collapse = ", "), context))
  }

  for (nm in names(print_hits)) {
    i <- which(tops$name == nm)[1L]
    verbose <- !is.na(i) && grepl("\\bverbose\\b", tops$text[i])
    add("CODE-PRINT", sprintf("%s:%s", label, paste(unique(print_hits[[nm]]), collapse = ",")),
        sprintf("print/cat/writeLines in `%s`%s", nm,
                if (verbose) " (has `verbose`: confirm every call is guarded)" else ": use message() or guard with verbose"))
  }

  # CODE-HOME and CODE-CORES: argument defaults of top-level functions
  if (context == "R") {
    for (j in seq_along(exprs)) {
      fn <- top_fun(exprs[[j]])
      if (is.null(fn)) next
      fm <- fn[[2L]]
      for (a in names(fm)) {
        # An argument without a default is the empty symbol, which cannot be
        # bound to a variable and then evaluated.
        if (is.name(fm[[a]]) && !nzchar(as.character(fm[[a]]))) next
        d <- fm[[a]]
        dd <- oneline(deparse(d), 60L)
        if (grepl(path_arg_re, a, ignore.case = TRUE) && !is.null(d) &&
            !grepl("^NULL$|tempfile|tempdir|R_user_dir|^NA|^c\\(\"|^\"(html|svg|png|json)\"$", dd) &&
            ((is.character(d) && grepl("[/~\\\\]|\\.[A-Za-z0-9]{1,5}$", d)) ||
             grepl("getwd|path\\.expand|file\\.path|here::", dd))) {
          add("CODE-HOME", sprintf("%s:%d", label, tops$line1[j]),
              sprintf("`%s(%s = %s)`: no default write location outside tempdir()", tops$name[j], a, dd))
        }
        if (grepl(core_arg_re, a) && (grepl("detectCores|availableCores", dd) ||
                                      (is.numeric(d) && d > 2))) {
          add("CODE-CORES", sprintf("%s:%d", label, tops$line1[j]),
              sprintf("`%s(%s = %s)`: default must be <= 2", tops$name[j], a, dd))
        }
      }
    }
  }
  invisible(NULL)
}

#' Parse a file or text and scan it; report parse failures.
scan_source <- function(file = NULL, text = NULL, label, context) {
  exprs <- tryCatch(
    if (is.null(text)) parse(file, keep.source = TRUE) else parse(text = text, keep.source = TRUE),
    error = function(e) {
      add("PARSE", label, paste("could not parse:", oneline(conditionMessage(e), 120L)))
      NULL
    }
  )
  if (!is.null(exprs)) scan_exprs(exprs, label, context)
}

## R/ ----
r_files <- list.files(file.path(pkg, "R"), "\\.[RrSsq]$", full.names = TRUE)
for (f in r_files) scan_source(f, label = rel(f), context = "R")

# %||% is base only from R 4.4.0: flag use without a package definition or import.
uses_or <- any(vapply(r_files, function(f) any(grepl("%||%", readLines(f, warn = FALSE), fixed = TRUE)), NA))
defines_or <- any(vapply(r_files, function(f) any(grepl("`%\\|\\|%`\\s*(<-|=)", readLines(f, warn = FALSE))), NA))
ns_file <- file.path(pkg, "NAMESPACE")
ns_lines <- if (file.exists(ns_file)) readLines(ns_file, warn = FALSE) else character()
if (uses_or && !defines_or && !any(grepl("%||%", ns_lines, fixed = TRUE))) {
  need_r("4.4.0", "base %||%", "R/")
}

## tests/ ----
t_files <- list.files(file.path(pkg, "tests"), "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
n_skip_cran <- 0L
for (f in t_files) {
  scan_source(f, label = rel(f), context = "tests")
  n_skip_cran <- n_skip_cran + sum(grepl("skip_on_cran\\(", readLines(f, warn = FALSE)))
}
if (n_skip_cran > 0L) {
  add("TEST-SKIP", "tests/", sprintf("%d skip_on_cran() calls: CRAN runs none of these, so the remaining tests must still exercise the package", n_skip_cran), advisory = TRUE)
}

## vignettes/ ----
extract_chunks <- function(f) {
  x <- readLines(f, warn = FALSE)
  if (grepl("\\.Rnw$", f, ignore.case = TRUE)) {
    starts <- grep("^<<.*>>=", x); ends <- grep("^@", x)
  } else {
    starts <- grep("^\\s*```+\\s*\\{r", x); ends <- grep("^\\s*```+\\s*$", x)
  }
  code <- rep("", length(x))
  for (s in starts) {
    if (grepl("eval\\s*=\\s*(FALSE|F)\\b", x[s])) next
    e <- ends[ends > s][1L]
    if (is.na(e)) next
    if (e > s + 1L) code[(s + 1L):(e - 1L)] <- x[(s + 1L):(e - 1L)]
  }
  code[grepl("^\\s*#\\|", code)] <- ""
  code
}
v_files <- list.files(file.path(pkg, "vignettes"), "\\.(Rmd|qmd|Rnw|Rmarkdown)$",
                      full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
for (f in v_files) scan_source(text = extract_chunks(f), label = rel(f), context = "vignettes")

# Rd files ----
exports <- character()
export_patterns <- character()
s3_names <- character()
if (file.exists(ns_file)) {
  ns <- parseNamespaceFile(basename(pkg), dirname(pkg), mustExist = FALSE)
  exports <- ns$exports
  export_patterns <- ns$exportPatterns
  if (length(ns$S3methods)) {
    s3_names <- paste(ns$S3methods[, 1L], ns$S3methods[, 2L], sep = ".")
  }
}
is_exported <- function(a) {
  a %in% exports || a %in% s3_names ||
    any(vapply(export_patterns, function(p) grepl(p, a), NA))
}
rd_tags <- function(x) vapply(x, function(e) attr(e, "Rd_tag") %||% "", "")
rd_text <- function(x) paste(unlist(x), collapse = "")
rd_has_tag <- function(x, tag) {
  if (identical(attr(x, "Rd_tag"), tag)) return(TRUE)
  is.list(x) && any(vapply(x, rd_has_tag, NA, tag = tag))
}

rd_files <- list.files(file.path(pkg, "man"), "\\.[Rr]d$", full.names = TRUE)
for (f in rd_files) {
  rd <- tryCatch(tools::parse_Rd(f, permissive = TRUE), error = function(e) NULL)
  if (is.null(rd)) {
    add("PARSE", rel(f), "Rd does not parse")
    next
  }
  tags <- rd_tags(rd)
  doctype <- if ("\\docType" %in% tags) trimws(rd_text(rd[tags == "\\docType"])) else ""
  if (doctype %in% c("data", "package")) next
  aliases <- trimws(vapply(rd[tags == "\\alias"], rd_text, ""))
  usage <- if ("\\usage" %in% tags) rd_text(rd[tags == "\\usage"]) else ""
  is_fun <- grepl("\\(", usage)
  exported <- any(vapply(aliases, is_exported, NA))
  internal <- any(trimws(vapply(rd[tags == "\\keyword"], rd_text, "")) == "internal")
  has_ex <- "\\examples" %in% tags

  # DOC-VALUE
  if (is_fun && !"\\value" %in% tags) {
    add("DOC-VALUE", rel(f), "no \\value: document the returned class and meaning (or 'No return value, called for side effects')")
  }
  # DOC-EXAMPLES
  if (is_fun && exported && !has_ex && !internal) {
    add("DOC-EXAMPLES", rel(f), "exported function without \\examples")
  }
  if (has_ex && !exported) {
    add("DOC-EXAMPLES", rel(f), sprintf("examples for unexported topic (%s): export it, drop the examples, or use %s:::", paste(head(aliases, 3L), collapse = ", "), pkg_name))
  }
  if (!has_ex) next

  ex <- rd[tags == "\\examples"][[1L]]
  if (rd_has_tag(ex, "\\dontrun")) {
    add("DOC-DONTRUN", rel(f), "\\dontrun{}: only for code that truly cannot run; else unwrap, \\donttest{}, or if (interactive())")
  }
  ex_tags <- rd_tags(ex)
  bare <- ex[ex_tags == "RCODE"]
  bare_code <- vapply(bare, function(x) trimws(sub("#.*$", "", rd_text(x))), "")
  if (!any(nzchar(bare_code)) && any(ex_tags %in% c("\\donttest", "\\dontrun"))) {
    add("DOC-DONTTEST-ALL", rel(f), "every example is wrapped: add a small unwrapped example (< 5 s) so CRAN tests it")
  }

  tmp <- tempfile(fileext = ".R")
  ok <- tryCatch({
    tools::Rd2ex(f, tmp, commentDontrun = TRUE, commentDonttest = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(tmp)) {
    scan_source(tmp, label = paste0(rel(f), " (examples)"), context = "examples")
    unlink(tmp)
  }
}

# Top-level files vs .Rbuildignore ----
standard_top <- c(
  "DESCRIPTION", "NAMESPACE", "R", "man", "inst", "tests", "vignettes", "data", "src",
  "LICENSE", "LICENCE", "LICENSE.note", "LICENCE.note", "NEWS.md", "NEWS", "README.md", "README", "configure",
  "configure.win", "configure.ucrt", "cleanup", "cleanup.win", "cleanup.ucrt",
  "demo", "exec", "po", "tools", "build", "java", "INDEX", "ChangeLog", "cran-comments.md"
)
top <- list.files(pkg, all.files = TRUE, no.. = TRUE)
rbi_file <- file.path(pkg, ".Rbuildignore")
rbi <- if (file.exists(rbi_file)) readLines(rbi_file, warn = FALSE) else character()
rbi <- rbi[nzchar(trimws(rbi)) & !startsWith(trimws(rbi), "#")]
ignored <- function(p) any(vapply(rbi, function(re) grepl(re, p, perl = TRUE, ignore.case = TRUE), NA))
for (x in top) {
  # R CMD build drops VCS files itself.
  if (x %in% c(".Rbuildignore", "cran-comments.md", ".git", ".gitignore", ".gitattributes",
               ".svn", ".hg", ".DS_Store")) next
  if (x %in% standard_top) next
  if (!ignored(x)) {
    add("BUILD-FILES", x, "top-level entry not in .Rbuildignore; it will ship in the tarball")
  }
}
if (!file.exists(file.path(pkg, "cran-comments.md"))) {
  add("SUBMIT-COMMENTS", "cran-comments.md", "missing; draft it from references/cran-comments.md")
} else if (!ignored("cran-comments.md")) {
  add("BUILD-FILES", "cran-comments.md", "must be in .Rbuildignore")
}
# NEWS.md is optional, so its absence is not a finding. When present, R CMD
# check parses it with this same reader and notes what it cannot read.
news_file <- file.path(pkg, "NEWS.md")
if (file.exists(news_file)) {
  if (!requireNamespace("commonmark", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
    add("SUBMIT-NEWS", "NEWS.md", "not parsed: install 'commonmark' and 'xml2' (R CMD check needs them too)", advisory = TRUE)
  } else {
    news_msgs <- character()
    news_db <- withCallingHandlers(
      tryCatch(tools:::.build_news_db_from_package_NEWS_md(news_file), error = function(e) {
        news_msgs <<- c(news_msgs, conditionMessage(e))
        NULL
      }),
      warning = function(w) {
        news_msgs <<- c(news_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    # Mirrors tools:::.check_packages: reader messages are a NOTE; otherwise a
    # result that is not a non-empty news_db is "No news entries found".
    for (m in news_msgs) add("SUBMIT-NEWS", "NEWS.md", paste("check NOTE:", oneline(m, 200L)))
    parsed <- inherits(news_db, "news_db") && nrow(news_db) > 0L
    if (!length(news_msgs) && !parsed) {
      add("SUBMIT-NEWS", "NEWS.md", "check NOTE: no news entries found; each release needs a heading containing its version, e.g. '## <pkg> <version>'")
    }
    if (parsed && !is.na(news_db$Version[1L]) && !identical(news_db$Version[1L], ver)) {
      add("SUBMIT-NEWS", "NEWS.md", sprintf("first heading is version %s; DESCRIPTION has %s", news_db$Version[1L], ver))
    }
  }
}

# Bundled third-party code ----
vendor <- list.files(file.path(pkg, c("inst", "src")), "\\.(js|css|mjs|c|cc|cpp|h|hpp|f|f90|java|py)$",
                     recursive = TRUE, full.names = TRUE)
vendor <- vendor[grepl("\\.min\\.|(^|/)(lib|libs|vendor|third[-_]party|external|deps)/", vendor) |
                   file.size(vendor) > 100 * 1024]
if (length(vendor)) {
  others <- if (is.null(authors)) character() else unlist(lapply(authors, function(p) {
    r <- p$role %||% character()
    if (any(r %in% c("cph", "ctb")) && !"cre" %in% r) format(p, include = c("given", "family")) else NULL
  }))
  # Any of these credits bundled code on CRAN: a Copyright field, inst/COPYRIGHTS
  # (the WRE convention), or a top-level LICENSE.note (known to R CMD check).
  credit_files <- c(file.path("inst", "COPYRIGHTS"), "LICENSE.note", "LICENCE.note")
  credit_found <- credit_files[file.exists(file.path(pkg, credit_files))]
  if (!is.na(field("Copyright"))) credit_found <- c("Copyright field", credit_found)
  cr <- length(credit_found) > 0L
  credit_text <- paste(unlist(lapply(file.path(pkg, setdiff(credit_found, "Copyright field")),
                                     readLines, warn = FALSE)),
                       field("Copyright") %||% "", collapse = "\n")
  add("THIRD-PARTY", "Authors@R", advisory = cr, sprintf(
    "other ctb/cph persons: %s; Copyright field, inst/COPYRIGHTS or LICENSE.note: %s. Every bundled library's copyright holders must be credited in one of these",
    if (length(others)) paste(others, collapse = "; ") else "none",
    if (cr) paste(credit_found, collapse = ", ") else "absent"))
  # One line per directory: the reviewer's unit is the bundled library, not the file.
  for (d in unique(dirname(vendor))) {
    in_d <- vendor[dirname(vendor) == d]
    lic <- list.files(d, "(?i)licen[cs]e|copying|copyright|notice")
    mins <- sum(grepl("\\.min\\.", in_d))
    # A directory the credit file names has been accounted for; keep it visible.
    add("THIRD-PARTY", rel(d), advisory = grepl(rel(d), credit_text, fixed = TRUE), sprintf(
      "%d file(s), %.1f MB%s; license file here: %s. If not your own code, credit it and ship or point to its unminified source",
      length(in_d), sum(file.size(in_d)) / 1024^2,
      if (mins) sprintf(", %d minified", mins) else "",
      if (length(lic)) paste(lic, collapse = ", ") else "none"))
  }
}

# Tarball ----
if (length(tarball)) {
  tb <- normalizePath(tarball, mustWork = TRUE)
  mb <- file.size(tb) / 1024^2
  if (mb > 5) add("BUILD-SIZE", basename(tb), sprintf("%.1f MB tarball: > 5 MB draws a NOTE; > 10 MB needs a case in the submission", mb))
  if (!grepl(paste0("^", gsub(".", "\\.", pkg_name, fixed = TRUE), "_", gsub(".", "\\.", ver, fixed = TRUE), "\\.tar\\.gz$"), basename(tb))) {
    add("BUILD-FILES", basename(tb), sprintf("not named %s_%s.tar.gz", pkg_name, ver))
  }
  ex_dir <- tempfile("tarball")
  utils::untar(tb, exdir = ex_dir)
  ff <- list.files(ex_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  rel_ff <- sub(paste0("^", ex_dir, "/[^/]+/"), "", ff)
  hidden <- rel_ff[grepl("(^|/)\\.", rel_ff)]
  for (h in hidden) add("BUILD-FILES", h, "hidden file in tarball")
  bins <- rel_ff[grepl("\\.(exe|dll|so|dylib|o|a|jar|class|pyc|whl)$", rel_ff)]
  for (b in bins) add("BUILD-BINARY", b, "binary/executable code is not allowed in a source package")
  sz <- file.size(ff)
  big <- order(sz, decreasing = TRUE)[seq_len(min(8L, sum(sz > 512 * 1024)))]
  for (i in big) add("BUILD-SIZE", rel_ff[i], sprintf("%.2f MB", sz[i] / 1024^2), advisory = TRUE)
  unlink(ex_dir, recursive = TRUE)
}

# Remote: name and dependency availability ----
if (!offline) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  cran <- tryCatch(rownames(available.packages()), error = function(e) NULL)
  bioc_urls <- sprintf("https://bioconductor.org/packages/release/%s",
                       c("bioc", "data/annotation", "data/experiment"))
  bioc <- tryCatch(rownames(available.packages(repos = bioc_urls)), error = function(e) NULL)
  arch <- tryCatch(names(tools:::CRAN_archive_db()), error = function(e) NULL)
  if (is.null(cran)) {
    add("NAME", "Package", "could not reach CRAN; rerun online")
  } else {
    lc <- tolower(pkg_name)
    same <- cran[tolower(cran) == lc]
    if (length(same)) {
      add("NAME", "Package", sprintf("'%s' is already on CRAN: this is an update, not an initial submission (or a name clash)", same[1L]))
    }
    past <- arch[tolower(arch) == lc]
    if (length(past) && !length(same)) {
      add("NAME", "Package", sprintf("'%s' is in the CRAN archive: names are never reused unless you are its maintainer", past[1L]))
    }
    bsame <- bioc[tolower(bioc) == lc]
    if (length(bsame)) add("NAME", "Package", sprintf("clashes with Bioconductor package '%s'", bsame[1L]))
    avail <- c(cran, bioc)
    strong <- setdiff(unique(c(deps$Depends, deps$Imports, deps$LinkingTo)), std_pkgs)
    for (d in setdiff(strong, avail)) {
      add("DESC-DEPS", "Imports/Depends/LinkingTo", sprintf("'%s' is not on CRAN or Bioconductor: strong dependencies must be", d))
    }
    weak <- setdiff(unique(c(deps$Suggests, deps$Enhances)), std_pkgs)
    missing_weak <- setdiff(weak, avail)
    if (length(missing_weak) && is.na(field("Additional_repositories"))) {
      add("DESC-DEPS", "Suggests/Enhances", sprintf("%s not on CRAN or Bioconductor: add Additional_repositories and use conditionally",
                                                    paste(sQuote(missing_weak, FALSE), collapse = ", ")))
    }
  }
}

# R version needs ----
if (length(.rneeds$hits)) {
  h <- do.call(rbind, .rneeds$hits)
  for (v in unique(h[, "v"])) {
    sub_h <- h[h[, "v"] == v, , drop = FALSE]
    add("DESC-RVERSION", "Depends", sprintf("uses %s (needs R >= %s; Depends has %s), first at %s",
                                            paste(unique(sub_h[, "what"]), collapse = ", "), v,
                                            if (length(r_dep)) r_dep else "no R version", sub_h[1L, "where"]))
  }
}

# Report ----
rows <- if (length(.findings$rows)) {
  as.data.frame(do.call(rbind, .findings$rows), stringsAsFactors = FALSE)
} else {
  data.frame(id = character(), where = character(), msg = character(), advisory = character())
}
rows$advisory <- rows$advisory == "TRUE"

## Suppress reviewed findings ----
n_ignored <- 0L
unused <- character()
if (length(ignore_file)) {
  il <- trimws(readLines(ignore_file, warn = FALSE))
  il <- il[nzchar(il) & !startsWith(il, "#")]
  no_reason <- il[!grepl("\\s#\\s*\\S", il)]
  if (length(no_reason)) {
    stop("ignore entries need a reason after '#':\n", paste(no_reason, collapse = "\n"))
  }
  spec <- strsplit(trimws(sub("\\s#.*$", "", il)), "\\s+")
  keep <- rep(TRUE, nrow(rows))
  for (j in seq_along(spec)) {
    hit <- rows$id == spec[[j]][1L] & grepl(utils::glob2rx(spec[[j]][2L] %||% "*"), rows$where)
    if (!any(hit)) unused <- c(unused, il[j])
    keep <- keep & !hit
  }
  n_ignored <- sum(!keep)
  rows <- rows[keep, , drop = FALSE]
}

cat(sprintf("# CRAN pre-submission scan: %s %s\n\n", pkg_name, ver))
cat(sprintf(
  "Scanned %d R, %d test, %d vignette and %d Rd files%s. %d findings (%d advisory)%s.\n",
  length(r_files), length(t_files), length(v_files), length(rd_files),
  if (offline) " (offline: name and dependency lookups skipped)" else "",
  nrow(rows), sum(rows$advisory),
  if (length(ignore_file)) sprintf("; %d suppressed by %s", n_ignored, basename(ignore_file)) else ""
))
cat("Each finding is a lead: confirm it against the checklist before changing code.\n")
print_group <- function(r) {
  for (id in sort(unique(r$id))) {
    sub_rows <- r[r$id == id, ]
    cat(sprintf("\n## %s (%d)\n\n", id, nrow(sub_rows)))
    shown <- head(sub_rows, 40L)
    for (i in seq_len(nrow(shown))) cat(sprintf("- `%s`: %s\n", shown$where[i], shown$msg[i]))
    if (nrow(sub_rows) > 40L) cat(sprintf("- ... %d more\n", nrow(sub_rows) - 40L))
  }
}
print_group(rows[!rows$advisory, , drop = FALSE])
if (any(rows$advisory)) {
  cat("\n# Advisory\n")
  print_group(rows[rows$advisory, , drop = FALSE])
}
if (length(unused)) {
  cat("\n# Unused ignore entries\n\nThese match nothing; delete them.\n\n")
  cat(sprintf("- `%s`\n", unused), sep = "")
}
if (fail && (any(!rows$advisory) || length(unused))) quit(status = 1L)
