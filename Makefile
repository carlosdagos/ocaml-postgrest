.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest test --profile release

.PHONY: repl
repl:
	dune utop lib
