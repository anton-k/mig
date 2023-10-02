.PHONY: build test run

build:
	stack build  mig

test:
	stack test

run:
	stack run
