.PHONY: examples all clean

all: build examples

build:
	stack build

clean:
	stack clean
	$(MAKE) -C examples clean

examples: build
	$(MAKE) -C examples
