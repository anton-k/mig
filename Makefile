.PHONY: build test run docs

build:
	stack build 

test:
	stack test mig

run:
	stack run

docs:
	mdbook serve docs

