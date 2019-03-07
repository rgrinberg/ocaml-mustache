all:
	dune build

test:
	dune runtest

check: test

clean:
	@dune clean

doc:
	dune build @doc

.PHONY: check test all clean doc
