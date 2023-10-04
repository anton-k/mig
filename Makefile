.PHONY: build test run

build:
	stack build mig-server

test:
	stack test

run:
	stack run
