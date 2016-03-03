.PHONY: examples all

all: build examples

build:
	stack build

examples: build
	$(MAKE) -C examples
