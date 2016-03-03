.PHONY: test clean

basic: Basic.hs
	stack ghc -- -O2 -threaded -rtsopts -with-rtsopts=-N -o basic Basic.hs

test: basic
	vagrant up
	./basic

clean:
	rm -f *.o
	rm -f *.hi
	rm -f basic
