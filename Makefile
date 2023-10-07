.PHONY: build test run docs

build:
	stack build mig-client

test:
	stack test

run:
	stack run

docs:
	mdbook serve docs

