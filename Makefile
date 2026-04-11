.PHONY: install install-r test test-r

# ── Document & Install ───────────────────────────────────────────────────────
install: install-r

install-r:
	@echo "==> R"
	cd r && Rscript -e "devtools::document()" && Rscript -e "devtools::install()"

# ── Test ─────────────────────────────────────────────────────────────────────
test: test-r

test-r:
	@echo "==> R"
	cd r && Rscript -e "devtools::test(stop_on_failure = TRUE)"

# ── Build Site ───────────────────────────────────────────────────────────────
site: site-r

site-r:
	cd r && Rscript -e "pkgdown::build_site()"
