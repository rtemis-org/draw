.PHONY: document document-r install install-r test test-r

# ── Document ─────────────────────────────────────────────────────────────────
document: document-r

document-r:
	@echo "=> R: Documenting rtemis.draw"
	cd r && Rscript -e "devtools::document()"

# ── Document & Install ───────────────────────────────────────────────────────
install: install-r

install-r: document-r
	@echo "==> R: Installing rtemis.draw"
	cd r && Rscript -e "devtools::install()"

# ── Test ─────────────────────────────────────────────────────────────────────
test: test-r

test-r:
	@echo "==> R: Testing rtemis.draw"
	cd r && Rscript -e "devtools::test(stop_on_failure = TRUE)"

# ── Check ────────────────────────────────────────────────────────────────────
check: check-r

check-r:
	@echo "==> R: Checking rtemis.draw"
	cd r && Rscript -e "devtools::check()"

# ── Build Site ───────────────────────────────────────────────────────────────
site: site-r

site-r:
	@echo "==> R: Building pkgdown site for rtemis.draw"
	cd r && Rscript -e "pkgdown::build_site()"
