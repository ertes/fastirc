.PHONY: all init

package = $(patsubst %.cabal,%,$(wildcard *.cabal))

all: dist/setup-config
	cabal build
	cabal haddock
	cabal test

dist/setup-config: $(wildcard *.cabal)
	cabal configure --enable-benchmarks --enable-tests
