.PHONY: examples all

all: build examples

build:
	stack build

examples: build
	stack ghc -- -o examples/basic examples/Basic.hs

