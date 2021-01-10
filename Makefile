all:
	dune build

test:
	dune runtest

check: test

clean:
	@dune clean

doc:
	dune build @doc

fmt:
	dune build @fmt --auto-promote

.PHONY: check test all clean doc fmt

.PHONY: release
release: ## Release on Opam
	dune-release distrib --skip-build --skip-lint --skip-tests
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
