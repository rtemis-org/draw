R_DIR := r
PKG   := $(shell awk '/^Package:/{print $$2; exit}' $(R_DIR)/DESCRIPTION)
R     ?= R
RSCRIPT ?= Rscript

msg = @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$$(date -u '+%Y-%m-%d %H:%M:%SZ')" "$(1)"

.DEFAULT_GOAL := help

.PHONY: help format format-r document document-r install install-r test test-r \
        build build-r check check-r check-cran site site-r clean clean-r

# ── Help ─────────────────────────────────────────────────────────────────────
help:
	$(call msg,Available targets:)
	@printf '%s\n' \
		'  format        Format R code with air CLI (if available)' \
		'  document      Generate roxygen2 documentation' \
		'  install       Document and install the R package with pak' \
		'  test          Run testthat::test_local(stop_on_failure = TRUE)' \
		'  build         Build the R source tarball' \
		'  check         Run R CMD check on the built tarball' \
		'  check-cran    Run R CMD check --as-cran' \
		'  site          Build pkgdown site' \
		'  clean         Remove tarballs and .Rcheck output'

# ── Format ────────────────────────────────────────────────────────────────────
format: format-r

format-r:
	$(call msg,─── Formatting $(PKG) [R]... ───)
	@if command -v air >/dev/null 2>&1; then \
		cd $(R_DIR) && air format .; \
	else \
		echo "   Note: 'air' CLI not found — skipping R code formatting."; \
	fi
	$(call msg,Done)

# ── Document ──────────────────────────────────────────────────────────────────
document: document-r

document-r: format-r
	$(call msg,─── Documenting $(PKG) [R]... ───)
	cd $(R_DIR) && $(RSCRIPT) -e "roxygen2::roxygenize()"
	$(call msg,Done)

# ── Install ───────────────────────────────────────────────────────────────────
install: install-r

install-r: document-r
	$(call msg,─── Installing $(PKG) [R]... ───)
	cd $(R_DIR) && $(RSCRIPT) -e "pak::local_install(upgrade = TRUE)"
	$(call msg,Done)

# ── Test ──────────────────────────────────────────────────────────────────────
test: test-r

test-r:
	$(call msg,─── Testing $(PKG) [R]... ───)
	cd $(R_DIR) && $(RSCRIPT) -e "testthat::test_local(stop_on_failure = TRUE)"
	$(call msg,Done)

# ── Build ─────────────────────────────────────────────────────────────────────
build: build-r

build-r: clean-r
	$(call msg,─── Building $(PKG) [R]... ───)
	cd $(R_DIR) && $(R) CMD build .
	$(call msg,Done)

# ── Check ─────────────────────────────────────────────────────────────────────
check: check-r

check-r: build-r
	$(call msg,─── Running R CMD check on $(PKG) [R]... ───)
	cd $(R_DIR) && $(R) CMD check $(PKG)_*.tar.gz
	rm -f $(R_DIR)/$(PKG)_*.tar.gz
	$(call msg,Done)

check-cran: build-r
	$(call msg,─── Running R CMD check --as-cran on $(PKG) [R]... ───)
	cd $(R_DIR) && $(R) CMD check $(PKG)_*.tar.gz --as-cran
	rm -f $(R_DIR)/$(PKG)_*.tar.gz
	$(call msg,Done)

# ── Site ──────────────────────────────────────────────────────────────────────
site: site-r

site-r:
	$(call msg,─── Building pkgdown site for $(PKG) [R]... ───)
	cd $(R_DIR) && $(RSCRIPT) -e "pkgdown::build_site()"
	$(call msg,Done)

# ── Clean ─────────────────────────────────────────────────────────────────────
clean: clean-r

clean-r:
	$(call msg,─── Cleaning build artifacts [R]... ───)
	rm -rf $(R_DIR)/$(PKG).Rcheck
	rm -f $(R_DIR)/$(PKG)_*.tar.gz
	$(call msg,Done)
