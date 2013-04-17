.PHONY: all init

package = $(patsubst %.cabal,%,$(wildcard *.cabal))

all: dist/setup-config Makefile
	cabal build
	cabal haddock
	cabal test

dist/setup-config: $(wildcard *.cabal) Makefile
	cabal configure --enable-benchmarks --enable-tests
