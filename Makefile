# Development entry points. CI runs `check` and `scan`; the rest are local
# conveniences.

R_PKG := r

.PHONY: help all docs install test check scan hooks clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

all: test check scan ## Everything CI runs

docs: ## Regenerate the help pages and NAMESPACE
	Rscript -e 'devtools::document("$(R_PKG)")'

install: docs ## Install the package
	R CMD INSTALL --no-multiarch $(R_PKG)

test: ## Run the test suite
	Rscript -e 'devtools::test("$(R_PKG)")'

check: docs ## R CMD check --as-cran
	Rscript -e 'devtools::check("$(R_PKG)", args = c("--no-manual", "--as-cran"))'

scan: ## Scan the tracked files for participant identifiers
	@rm -rf .scan_export && mkdir -p .scan_export
	@git archive HEAD | tar -x -C .scan_export
	python3 tools/scan_repo.py .scan_export --plain
	@rm -rf .scan_export

hooks: ## Install the pre-commit identifier guard
	cp tools/pre-commit .git/hooks/pre-commit
	chmod +x .git/hooks/pre-commit
	git config publicscan.tool "$(CURDIR)/tools/scan_repo.py"
	@echo "pre-commit guard installed"

clean:
	rm -rf .scan_export
