.PHONY: build test run

build:
	stack build mig-rio

test:
	stack test

run:
	stack run
