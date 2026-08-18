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

# Write the chart schemas to the schema repo (publishing step; commit there separately)
schemas repo=schema_repo:
    @just _need SCHEMA_REPO "{{repo}}"
    @just _msg "─── Generating schemas for {{pkg}} into {{repo}}... ───"
    cd {{r_dir}} && {{rscript}} data-raw/generate_schemas.R {{repo}}
    @just _msg "Done"

# Generate schemas and refresh the registry index; stops before the commit
publish-schemas: schemas
    @just _msg "─── Indexing {{schema_repo}}... ───"
    cd "{{schema_repo}}" && just index && just check
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
