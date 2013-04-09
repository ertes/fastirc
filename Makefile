.PHONY: all init

package = $(patsubst %.cabal,%,$(wildcard *.cabal))

all: dist/setup-config
	cabal build
	cabal test

init:
ifeq ($(package),skeleton)
	@echo Please rename the skeleton.cabal file.
	@false
endif
	sed -i -e 's/_PACKAGE_/$(package)/g' \
		LICENSE \
		Setup.lhs \
		$(package).cabal \
		program/Main.hs \
		test/Bench.hs \
		test/Props.hs
	ln -s dist/build/$(package)/$(package)

dist/setup-config: $(wildcard *.cabal)
	cabal configure --enable-benchmarks --enable-tests
