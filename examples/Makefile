.PHONY: test clean

basic: $(call rwildcard,../src,*.hs) Basic.hs
	stack ghc -- -O2 -threaded -rtsopts -with-rtsopts=-N -o basic Basic.hs

test: basic
	vagrant destroy -f
	vagrant up
	./basic
	./basic

clean:
	rm -f *.o
	rm -f *.hi
	rm -f basic
