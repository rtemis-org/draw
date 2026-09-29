# justfile
# ::rtemis::
# 2026- EDG rtemis.org

r_dir := "r"
pkg := `awk '/^Package:/{print $2; exit}' r/DESCRIPTION`
r := env("R", "R")
schema_repo := env("SCHEMA_REPO", "")
rscript := env("RSCRIPT", "Rscript")
tarball_glob := pkg + "_*.tar.gz"

# List available recipes
default:
    @just --list

_msg msg:
    @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$(date '+%Y-%m-%d %H:%M:%S')" "{{msg}}"

# Format R code with air CLI (if available)
format:
    @just _msg "─── Formatting {{pkg}} package... ───"
    @if command -v air >/dev/null 2>&1; then \
        cd {{r_dir}} && air format .; \
    else \
        echo "   Note: 'air' CLI not found — skipping R code formatting."; \
    fi
    @just _msg "Done"

# Generate roxygen2 documentation
document: format
    @just _msg "─── Documenting {{pkg}} package... ───"
    cd {{r_dir}} && {{rscript}} -e "roxygen2::roxygenize()"
    @just _msg "Done"

# Document and install the package locally with pak
install: document
    @just _msg "─── Installing {{pkg}} package... ───"
    cd {{r_dir}} && {{rscript}} -e "pak::local_install(upgrade = TRUE)"
    @just _msg "Done"

# Run testthat::test_local(stop_on_failure = TRUE)
test:
    @just _msg "─── Running testthat tests for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "testthat::test_local(stop_on_failure = TRUE)"
    @just _msg "Done"

# Build the source tarball
build: clean
    @just _msg "─── Building {{pkg}} package... ───"
    cd {{r_dir}} && {{r}} CMD build .
    @just _msg "Done"

# Run R CMD check on the built tarball (pass extra flags, e.g. `just check --as-cran`)
check *flags: build
    @just _msg "─── Running R CMD check {{flags}} on {{pkg}}... ───"
    cd {{r_dir}} && {{r}} CMD check {{tarball_glob}} {{flags}}
    rm -f {{r_dir}}/{{tarball_glob}}
    @just _msg "Done"

# Run R CMD check --as-cran
check-cran: (check "--as-cran")

# Run R CMD check --as-cran --no-tests
check-cran-no-tests: (check "--as-cran" "--no-tests")

# Check URLs in package documentation with urlchecker
urls:
    @just _msg "─── Checking URLs for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "urlchecker::url_check()"
    @just _msg "Done"

# Build package manual (PDF)
manual:
    @just _msg "─── Building manual for {{pkg}}... ───"
    cd {{r_dir}} && {{r}} CMD Rd2pdf . --output={{pkg}}.pdf
    @just _msg "Done"

# Build pkgdown site
site:
    @just _msg "─── Building pkgdown site for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "pkgdown::build_site()"
    @just _msg "Done"

_need var path:
    @if [ -z "{{ path }}" ]; then \
        echo "   Error: {{ var }} is not set. Point it at your local schema checkout."; \
        exit 1; \
    elif [ ! -d "{{ path }}" ]; then \
        echo "   Error: {{ var }} is set to '{{ path }}', which is not a directory."; \
        exit 1; \
    fi

# Generate the chart schemas into a throwaway directory, to check they build
schemas-check:
    @just _msg "─── Checking schema generation for {{pkg}}... ───"
    @dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT; \
        cd {{r_dir}} && {{rscript}} data-raw/generate_schemas.R "$dir"
    @just _msg "Done"

# Write the chart schemas to the schema repo (publishing step; commit there
# separately), then reindex it.
#
# The reindex is folded in rather than left as a step to remember, because
# skipping it fails nothing where it happens: the repo commits clean, the files
# serve 200, and `deployed` compares a stale manifest against an identically
# stale one. It surfaces two repos away, as a sha256 mismatch in a consumer's
# schema sync.
schemas repo=schema_repo:
    @just _need SCHEMA_REPO "{{repo}}"
    @just _msg "─── Generating schemas for {{pkg}} into {{repo}}... ───"
    cd {{r_dir}} && {{rscript}} data-raw/generate_schemas.R {{repo}}
    @just _msg "─── Indexing {{repo}}... ───"
    cd "{{repo}}" && just index && just check
    @just _msg "Done"

# Generate schemas, reindex, and stop before the commit for review
publish-schemas: schemas
    @git -C "{{schema_repo}}" status --short
    @just _msg "Review the diff above, then commit and push - the push is the deploy:"
    @echo "   git -C '{{schema_repo}}' add -A && git -C '{{schema_repo}}' commit -m 'add chart schemas' && git -C '{{schema_repo}}' push"


# Spell-check package; accepted technical terms live in inst/WORDLIST
spell:
    @just _msg "─── Spell-checking {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "r <- spelling::spell_check_package(); print(r); if (nrow(r) > 0L) quit(status = 1L)"
    @just _msg "Done"

# Add all current spell-check terms to inst/WORDLIST (review the diff)
spell-update:
    @just _msg "─── Updating inst/WORDLIST for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "spelling::update_wordlist(confirm = FALSE)"
    @just _msg "Done"

# Lint package source for unused objects (variables/arguments).
# Loads the package first: without it lintr resolves each file on its own and
# reports every cross-file internal object as undefined.
lint:
    @just _msg "─── Linting {{pkg}} source for unused objects... ───"
    cd {{r_dir}} && {{rscript}} -e "suppressMessages(pkgload::load_all('.', quiet = TRUE)); l <- lintr::lint_dir('R', linters = list(lintr::object_usage_linter())); print(l); if (length(l) > 0L) quit(status = 1L)"
    @just _msg "Done"

# Check that each man/*.Rd file has \value and \examples sections (CRAN requires both)
check-rd:
    @just _msg "─── Checking Rd sections for {{pkg}}... ───"
    cd {{r_dir}} && tools/check-rd-sections.sh man
    @just _msg "Done"

# Like check-rd but also enforces \keyword{internal} docs (data/package stay exempt)
check-rd-all:
    @just _msg "─── Checking Rd sections (incl. internal) for {{pkg}}... ───"
    cd {{r_dir}} && tools/check-rd-sections.sh -internal man
    @just _msg "Done"

# Check R code formatting without modifying files (CI-friendly; fails if unformatted)
format-check:
    @just _msg "─── Checking formatting for {{pkg}}... ───"
    @if command -v air >/dev/null 2>&1; then \
        cd {{r_dir}} && air format --check .; \
    else \
        echo "   Error: 'air' CLI not found."; \
        exit 1; \
    fi
    @just _msg "Done"

# Catches options/par/setwd changed without on.exit(), writes outside tempdir(),
# T/F, fixed seeds, unsuppressible output, > 2 cores, DESCRIPTION wording,
# \dontrun and uncredited bundled code. Reviewed exceptions go in
# tools/cran-scan-ignore. Pass --offline to skip the CRAN name/dependency lookup.
# Scan for issues CRAN's manual review rejects and R CMD check misses
cran-scan *flags:
    @just _msg "─── Scanning {{pkg}} for CRAN review issues... ───"
    cd {{r_dir}} && {{rscript}} tools/cran-scan.R . --ignore=tools/cran-scan-ignore --fail {{flags}}
    @just _msg "Done"

# cran-scan plus the built tarball: size, largest files, stray and binary files
cran-scan-tarball: build
    @just _msg "─── Scanning {{pkg}} tarball for CRAN review issues... ───"
    cd {{r_dir}} && status=0; {{rscript}} tools/cran-scan.R . --tarball="$(ls {{tarball_glob}})" --ignore=tools/cran-scan-ignore --fail || status=$?; rm -f {{tarball_glob}}; exit $status
    @just _msg "Done"

# CRAN fixes made only in .Rd files are lost on the next roxygenize(). Compares
# before/after, so uncommitted doc changes that are already current pass.
# Fail if man/ or NAMESPACE is stale relative to the roxygen comments
docs-current:
    #!/usr/bin/env bash
    set -euo pipefail
    snap() { git status --porcelain -- {{r_dir}}/man {{r_dir}}/NAMESPACE; git diff -- {{r_dir}}/man {{r_dir}}/NAMESPACE; }
    before=$(snap | shasum)
    (cd {{r_dir}} && {{rscript}} -e "roxygen2::roxygenize()" >/dev/null)
    if [ "$(snap | shasum)" != "$before" ]; then
        echo "   man/ or NAMESPACE was stale and has been regenerated; review and commit:"
        git status --short -- {{r_dir}}/man {{r_dir}}/NAMESPACE
        exit 1
    fi
    echo "   man/ and NAMESPACE are current."

# Mirrors CRAN's noSuggests check. Output goes to a temp dir so the main
# .Rcheck survives.
# R CMD check --as-cran with Depends/Imports only: finds unconditional Suggests use
check-cran-depends-only: build
    @just _msg "─── R CMD check --as-cran with Depends/Imports only on {{pkg}}... ───"
    cd {{r_dir}} && out="$(mktemp -d)"; status=0; _R_CHECK_DEPENDS_ONLY_=true {{r}} CMD check {{tarball_glob}} --as-cran --no-manual --output="$out" || status=$?; rm -f {{tarball_glob}}; exit $status
    @just _msg "Done"

# Reads the timings of the last `just check-cran`. CPU > 2.5x elapsed means
# more than 2 cores.
# Fail on examples over 5 s elapsed or over 2.5x CPU per elapsed second
check-timings:
    @just _msg "─── Checking example timings for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e 'f <- "{{pkg}}.Rcheck/{{pkg}}-Ex.timings"; if (!file.exists(f)) stop("no ", f, ": run `just check-cran` first"); t <- utils::read.table(f, header = TRUE); cpu <- t$user + t$system; bad <- t[t$elapsed > 5 | (t$elapsed > 0.5 & cpu > 2.5 * t$elapsed), ]; cat(sprintf("   %d examples, %.1f s elapsed in total; slowest: %s (%.1f s)\n", nrow(t), sum(t$elapsed), t$name[which.max(t$elapsed)], max(t$elapsed))); if (nrow(bad)) { print(bad, row.names = FALSE); quit(status = 1L) }'
    @just _msg "Done"

# Checks gated by a missing tool are skipped silently or only noted, so a clean
# local check does not cover them. Reports; does not fail.
# List tools R CMD check --as-cran needs and which are missing
cran-tools:
    @just _msg "─── Checking tools used by R CMD check --as-cran... ───"
    @missing=0; \
    for t in pdflatex qpdf tidy; do \
        if command -v "$t" >/dev/null 2>&1; then echo "   ok       $t"; else echo "   MISSING  $t"; missing=1; fi; \
    done; \
    if command -v aspell >/dev/null 2>&1 || command -v hunspell >/dev/null 2>&1; then \
        echo "   ok       aspell/hunspell"; else echo "   MISSING  aspell/hunspell (DESCRIPTION spell check)"; missing=1; fi; \
    {{r}} --version | head -1 | sed 's/^/   /'; \
    if [ $missing -eq 1 ]; then echo "   Checks needing the missing tools did not run: say so in cran-comments.md or install them."; fi
    @just _msg "Done"

# Nothing leaves this machine; win-devel and rhub-check are separate.
# Run every local CRAN pre-submission gate in order; stops at the first failure
cran-prep: cran-tools docs-current cran-scan-tarball check-rd spell urls check-cran check-timings check-cran-depends-only

# Results are emailed to the maintainer address in DESCRIPTION.
# Upload to win-builder for a Windows R-devel check, as CRAN runs
win-devel:
    @just _msg "─── Submitting {{pkg}} to win-builder R-devel... ───"
    cd {{r_dir}} && {{rscript}} -e "devtools::check_win_devel()"
    @just _msg "Done"

# Run rhub checks across CRAN platforms
rhub-check:
    @just _msg "─── Running rhub checks for {{pkg}}... ───"
    cd {{r_dir}} && {{rscript}} -e "rhub::rhub_check(platforms = c('linux', 'macos-arm64', 'windows'))"
    @just _msg "Done"


# Remove tarballs and .Rcheck output
clean:
    @just _msg "─── Cleaning build artifacts... ───"
    rm -rf {{r_dir}}/{{pkg}}.Rcheck
    rm -f {{r_dir}}/{{tarball_glob}}
    @just _msg "Done"

# Inspect installed-package heatmap/A3 browser and vector output (developer QA).
qa-export output:
    {{rscript}} r/tools/visual-qa/export.R "{{output}}"

# Inspect scatter/line/bar browser interactions and vector output (developer QA).
qa-foundation output:
    {{rscript}} r/tools/visual-qa/foundation.R "{{output}}"

# Inspect Sigma/MapLibre browser interactions and genuine vector SVG output.
qa-backends output:
    {{rscript}} r/tools/visual-qa/backends.R "{{output}}"

# Inspect native 3D camera projection, rotation and genuine SVG marks.
qa-scatter3d output:
    {{rscript}} r/tools/visual-qa/scatter3d.R "{{output}}"

# Native 3D path/surface geometry, legend interaction and vector visibility
qa-surface3d output:
    {{rscript}} r/tools/visual-qa/surface3d.R "{{output}}"

# Repeated rendering, theme/resize changes and dense geometry (developer QA).
qa-lifecycle output:
    {{rscript}} r/tools/visual-qa/lifecycle.R "{{output}}"

# Dense labels, annotated proteins and unusual panel layouts (developer QA).
qa-targeted output:
    {{rscript}} r/tools/visual-qa/targeted.R "{{output}}"
