JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build @install @DEFAULT

test:
	@$(JBUILDER) runtest

check: test

.PHONY: check test all
