# justfile
# ::rtemis::
# 2026- EDG rtemis.org

r_dir := "r"
pkg := `awk '/^Package:/{print $2; exit}' r/DESCRIPTION`
r := env_var_or_default("R", "R")
rscript := env_var_or_default("RSCRIPT", "Rscript")
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

# Remove tarballs and .Rcheck output
clean:
    @just _msg "─── Cleaning build artifacts... ───"
    rm -rf {{r_dir}}/{{pkg}}.Rcheck
    rm -f {{r_dir}}/{{tarball_glob}}
    @just _msg "Done"
