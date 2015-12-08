.PHONY: examples all

all: build examples

build:
	stack build

examples: build
	stack ghc -- -O2 -threaded -rtsopts -with-rtsopts=-N -o examples/basic examples/Basic.hs
