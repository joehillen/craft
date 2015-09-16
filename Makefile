all: examples/Main.hs
	stack ghc -- -o main $^

