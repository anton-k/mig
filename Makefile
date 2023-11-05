.PHONY: build test run docs

build:
	stack build 

test:
	stack test

run:
	stack run

docs:
	mdbook serve docs

loc:
	cloc --exclude-dir=.stack-work,.git,.github,docs,examples . --include-ext=hs
