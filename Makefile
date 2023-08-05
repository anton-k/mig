.PHONY: build test run

build:
	stack build

run:
	stack runhaskell examples/HelloWorld.hs
