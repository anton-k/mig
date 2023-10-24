.PHONY: build test run docs

build:
	stack build mig

test:
	stack test

run:
	stack run

docs:
	mdbook serve docs

