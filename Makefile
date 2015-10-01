all: examples/Main.hs
	stack build
	stack ghc -- -o main $^

