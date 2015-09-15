all: examples/Main.hs
	stack ghc -- -XRecordWildCards -XRankNTypes -o main $^

