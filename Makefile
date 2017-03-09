.PHONY: examples all clean test build

all: build examples

build: $(call rwildcard,src,*.hs)
	stack build

clean:
	stack clean
	$(MAKE) -C examples clean

examples: build
	$(MAKE) -C examples

test:
	stack build --test
	$(MAKE) -C examples clean
	$(MAKE) -C examples test
