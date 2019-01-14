all:
	dune build

test:
	dune runtest

check: test

clean:
	@dune clean

.PHONY: check test all clean
